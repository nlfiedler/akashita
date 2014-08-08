#!/usr/bin/env python2.7
"""Creates archives and uploads them to Amazon Glacier.

Based on a configuration file, takes a ZFS snapshot and creates one or more
compressed archives, splits them into manageable pieces, and uploads them
to Glacier.

Requires that xz is installed for compressing the tarballs (i.e. this
script invokes `tar Jc` in a child process).

Requires Amazon Web Services module boto (https://github.com/boto/boto)

"""

import argparse
import ConfigParser
from contextlib import contextmanager
from datetime import datetime
import logging
import os
import shutil
import subprocess
import sys
import time

import boto.glacier
import boto.glacier.layer2
import boto.glacier.vault

#
#    AT&T U-verse "Max" speeds
# Downstream: 12 Mb/s => 1536 KB/s
# Upstream  :  2 Mb/s =>  212 KB/s
#
# Archives consist of compressed tarballs split into parts of 128 MB each.
# Each part 128 MB will take ~10 minutes to upload.
#
# TODO: set up SMF to restart the process
# TODO: configure LogWatch to email daily report
# TODO: configure log rotation
#
# To test this script, create a disposable ZFS filesystem using the
# mkfile command, like so:
#
# $ mkfile 256m tank
# $ pfexec zpool create tank $PWD/tank
# $ pfexec zfs create tank/shared
#

VAULT_PREFIX = 'vault_'
ARCHIVE_PREFIX = 'archive_'

LOG = logging.getLogger('akashita')
LOG_FORMAT = '[%(process)d] <%(asctime)s> (%(name)s) {%(levelname)s} %(message)s'
LOG_FILE = 'akashita.log'


def _configure_logging(config):
    """Configure the logger for our purposes.

    :param config: ConfigParser instance with akashita configuration.

    """
    LOG.setLevel(logging.DEBUG)
    if config.has_option('logging', 'file'):
        log_file = config.get('logging', 'file')
    else:
        log_file = LOG_FILE
    handler = logging.FileHandler(log_file)
    handler.setLevel(logging.DEBUG)
    if config.has_option('logging', 'format'):
        log_format = config.get('logging', 'format')
    else:
        log_format = LOG_FORMAT
    formatter = logging.Formatter(log_format)
    handler.setFormatter(formatter)
    LOG.addHandler(handler)


def _ensure_snapshot_exists(prefix):
    """Create the ZFS snapshot, if it is missing.

    :param prefix: name prefix of the snapshot to create.

    Returns the full name of the snapshot.

    """
    tag = datetime.utcnow().strftime("%Y-%m-%d")
    snapshot = "{}@glacier:{}".format(prefix, tag)
    proc = subprocess.Popen(['zfs', 'list', '-H', snapshot],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    proc.communicate()
    if proc.returncode != 0:
        LOG.debug('_ensure_snapshot_exists() creating {}'.format(snapshot))
        proc = subprocess.Popen(['zfs', 'snapshot', snapshot],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()
        if proc.returncode != 0:
            LOG.error('zfs snapshot error: {}'.format(err))
            raise RuntimeError('zfs snapshot failed')
        else:
            LOG.info('snapshot {} created'.format(snapshot))
    return snapshot


def _ensure_clone_exists(clone, snapshot):
    """Create the ZFS clone, if it is missing.

    :param clone: name of the ZFS clone to create.
    :param snapshot: name of the ZFS snapshot on which to base the clone.

    """
    proc = subprocess.Popen(['zfs', 'list', '-H', clone],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    proc.communicate()
    if proc.returncode != 0:
        LOG.debug('_ensure_clone_exists() creating {} from {}'.format(clone, snapshot))
        proc = subprocess.Popen(['zfs', 'clone', '-p', snapshot, clone],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()
        if proc.returncode != 0:
            LOG.error('zfs clone error: {}'.format(err))
            raise RuntimeError('zfs clone failed')
        else:
            LOG.info('clone {} created'.format(clone))


def _destroy_zfs_object(fsobj):
    """Destroy the named ZFS data set.

    :param fsobj: name of ZFS data set to destroy.

    """
    LOG.debug('_destroy_zfs_object() destroying {}'.format(fsobj))
    proc = subprocess.Popen(['zfs', 'destroy', fsobj],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    out, err = proc.communicate()
    if proc.returncode != 0:
        LOG.error('zfs destroy error: {}'.format(err))
        raise RuntimeError('zfs destroy failed')
    else:
        LOG.info('zfs data set {} destroyed'.format(fsobj))


@contextmanager
def pop_chdir(path):
    """
    Context manager to temporarily change to the given directory.
    """
    cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(cwd)


def _is_go_time(config):
    """Determine if current time within upload window.

    :param config: ConfigParser instance with akashita configuration.

    Returns True if current time within upload window, False otherwise.

    """
    now = time.localtime()
    windows = config.get('schedule', 'go_time').split(',')
    for window in windows:
        go_time, stop_time = window.split('-')
        go_time = time.strptime(go_time, '%H:%M')
        stop_time = time.strptime(stop_time, '%H:%M')
        past_go = now.tm_hour >= go_time.tm_hour and now.tm_min >= go_time.tm_min
        before_stop = now.tm_hour <= stop_time.tm_hour and now.tm_min <= stop_time.tm_min
        if past_go and before_stop:
            return True
    return False


def _ensure_vault_exists(layer2_obj, prefix):
    """Create a vault with the given base name.

    This operation is idempotent.

    :param layer2_obj: instance of glacier.layer2.Layer2()
    :param prefix: name prefix for the vault to create.

    Returns the name of the vault and the Vault instance.

    """
    tag = datetime.utcnow().strftime("%Y-%m-%d")
    vault_name = '{}-{}'.format(prefix, tag)
    layer2_obj.layer1.create_vault(vault_name)
    response_data = layer2_obj.layer1.describe_vault(vault_name)
    LOG.info('created Glacier vault {}'.format(vault_name))
    vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
    return vault_name, vault_obj


def _create_archive(config, paths, archive_name):
    """Create the set of archive files to be uploaded to a vault.

    :param config: ConfigParser instance with akashita configuration.
    :param paths: list of paths to be archived.
    :param archive_name: name prefix for the archive files.

    This function is a generator of the paths of the archive parts
    that were created.

    """
    # create a temporary working directory for the archive parts
    tmpdir = config.get('paths', 'tmpdir')
    tag = datetime.utcnow().strftime("%Y-%m-%d")
    root = os.path.join(tmpdir, '{}-{}'.format(archive_name, tag))
    if os.path.exists(root) and len(os.listdir(root)) == 0:
        os.rmdir(root)
    if not os.path.exists(root):
        os.makedirs(root)
        devnull = os.open(os.devnull, os.O_RDWR)
        try:
            with pop_chdir(root):
                LOG.debug('_create_archive() compressing {}'.format(archive_name))
                tar = subprocess.Popen(['tar', 'Jc'] + paths,
                                       stdout=subprocess.PIPE,
                                       stderr=devnull)
                split = subprocess.Popen(['split', '-b', '128m', '-', archive_name],
                                         stdin=tar.stdout,
                                         stdout=subprocess.PIPE,
                                         stderr=devnull)
                # allow tar process to receive a SIGPIPE if split exits early
                tar.stdout.close()
                # read the outputs so the process finishes, but ignore them
                split.communicate()
                if tar.returncode != 0 and tar.returncode is not None:
                    raise subprocess.CalledProcessError(tar.returncode, "tar")
                if split.returncode != 0 and split.returncode is not None:
                    raise subprocess.CalledProcessError(split.returncode, "split")
        except:
            # wash it all away and try again next time around
            shutil.rmtree(root)
            raise
        finally:
            os.close(devnull)
    for name in os.listdir(root):
        if name.startswith(archive_name):
            yield os.path.join(root, name)


def _process_vault(config, section, name, layer2_obj):
    """Perform the backup for a particular "vault".

    :param config: ConfigParser instance with akashita configuration.
    :param section: vault section from config.
    :param name: name prefix of the vault to process.
    :param layer2_obj: instance of glacier.layer2.Layer2()

    """
    # ensure the vault exists
    vault_name, vault_obj = _ensure_vault_exists(layer2_obj, name)
    LOG.info('Processing vault {}'.format(vault_name))
    # ensure a zfs snapshot exists
    fs_path = config.get(section, 'fs_path')
    snapshot_name = _ensure_snapshot_exists(fs_path)
    # ensure the zfs clone exists
    clone_base = config.get(section, 'clone_base')
    clone_name = clone_base + '/' + vault_name
    clone_path = '/' + clone_name
    _ensure_clone_exists(clone_name, snapshot_name)
    try:
        # process each of the archives specified in the config file
        is_archive = lambda n: n.startswith(ARCHIVE_PREFIX)
        archives = [n for n in config.options(section) if is_archive(n)]
        for archive in archives:
            paths = []
            for entry in config.get(section, archive).split(','):
                paths.append(os.path.join(clone_path, entry.strip()))
            archive_name = archive[len(ARCHIVE_PREFIX):]
            LOG.debug('_process_vault() processing archive {}'.format(archive_name))
            # TODO: ideally would consult SimpleDB to know if this archive
            #       has already been successfully uploaded to this vault
            parts_dir = None
            part_num = 1
            for part in _create_archive(config, paths, archive_name):
                desc = '{}-{}'.format(archive_name, str(part_num))
                LOG.debug('_process_vault() processing archive part {}'.format(part_num))
                # sleep until we reach the 'go' time
                while not _is_go_time(config):
                    LOG.debug('_process_vault() sleeping...')
                    time.sleep(60 * 60 * 10)
                start_time = time.time()
                archive_id = vault_obj.upload_archive(part, desc)
                elapsed = (time.time() - start_time) / 60
                LOG.info('Uploaded archive {} to vault {} in {} minutes'.format(
                    archive_id, vault_name, elapsed))
                # remove each part as it is uploaded
                os.unlink(part)
                LOG.info('Finished part {} of archive {}'.format(part_num, archive_name))
                if parts_dir is None:
                    parts_dir = os.path.dirname(part)
                part_num += 1
            # remove the temporary working directory
            os.rmdir(parts_dir)
            LOG.info('Finished archive {}'.format(archive_name))
            # TODO: store archive metadata in SimpleDB for fast retrieval
    finally:
        _destroy_zfs_object(clone_name)
        _destroy_zfs_object(snapshot_name)
    LOG.info('Processing vault {} completed'.format(vault_name))


def _load_configuration():
    """Find and load the configuration file."""
    config = ConfigParser.ConfigParser()
    fname = os.path.expanduser('~/.akashita')
    if os.path.exists(fname):
        LOG.debug('_load_configuration() reading {}'.format(fname))
        config.read(fname)
    elif os.path.exists('/etc/akashita'):
        LOG.debug('_load_configuration() reading /etc/akashita')
        config.read('/etc/akashita')
    else:
        LOG.error("Could not find configuration file!")
        sys.stderr.write("Could not find configuration file!\n")
        sys.exit(1)
    return config


def _prune_vaults(layer2_obj):
    """Remove any vaults older than three months."""
    # TODO: implement removal of old vaults
    LOG.info('vault pruning not yet implemented')
    for vault in layer2_obj.list_vaults():
        # old = datetime.utcnow() - timedelta(100)
        # '2013-11-17T07:40:39.798Z'
        # vault_time_format = '%Y-%m-%dT%H:%M:%S.???'
        # TODO: retrieve the vault inventory
        # TODO: remove all of the archives contained therein
        # TODO: retrieve inventory again to satisfy requirements
        # TODO: delete the vault
        pass


def _perform_backup(config):
    """Perform the backup procedure.

    :param config: ConfigParser instance with akashita configuration.

    """
    aws_access_key_id = config.get('aws', 'access_key')
    aws_secret_access_key = config.get('aws', 'secret_key')
    region_name = config.get('aws', 'region_name')
    layer2_obj = boto.glacier.layer2.Layer2(
        aws_access_key_id, aws_secret_access_key, region_name=region_name)

    is_vault = lambda n: n.startswith(VAULT_PREFIX)
    vaults = [n for n in config.sections() if is_vault(n)]
    for section_name in vaults:
        _process_vault(config, section_name, section_name[len(VAULT_PREFIX):], layer2_obj)
    _prune_vaults(layer2_obj)


def main():
    """Do the thing."""
    parser = argparse.ArgumentParser(description="Upload archives to Amazon Glacier.")
    parser.add_argument("-R", "--regions", action='store_true',
                        help="list available Amazon Glacier regions")
    args = parser.parse_args()
    if args.regions:
        for reg_info in boto.glacier.regions():
            print("Region: {}".format(reg_info.name))
    else:
        config = _load_configuration()
        _configure_logging(config)
        LOG.info('akashita process started')
        _perform_backup(config)
        LOG.info('akashita process exiting')


if __name__ == "__main__":
    main()
