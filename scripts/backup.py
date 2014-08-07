#!/usr/bin/env python3
"""Creates archives and uploads them to Amazon Glacier.

Based on a configuration file, takes a ZFS snapshot and creates one or more
compressed archives, splits them into manageable pieces, and uploads them
to Glacier.

Requires that xz is installed for compressing the tarballs (i.e. this
script invokes `tar Jc` in a child process).

Requires Amazon Web Services module boto (https://github.com/boto/boto)

"""

import argparse
import configparser
from contextlib import contextmanager
from datetime import datetime
import logging
import os
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
#

VAULT_PREFIX = 'vault_'
ARCHIVE_PREFIX = 'archive_'

LOG = logging.getLogger('akashita')
LOG_FORMAT = '[%(process)d] <%(asctime)s> (%(name)s) {%(levelname)s} %(message)s'


def _configure_logging():
    """Configure the logger for our purposes."""
    LOG.setLevel(logging.DEBUG)
    handler = logging.FileHandler('akashita.log')
    handler.setLevel(logging.DEBUG)
    formatter = logging.Formatter(LOG_FORMAT)
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
    return snapshot


def _ensure_clone_exists(clone, snapshot):
    """Create the ZFS clone, if it is missing.

    :param clone: name of the ZFS clone to create.
    :param snapshot: name of the ZFS snapshot on which to base the clone.

    """
    proc = subprocess.Popen(['zfs', 'list', '-H', snapshot],
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


def _destroy_zfs_object(fsobj):
    """Destroy the named ZFS fsobj.

    :param fsobj: name of ZFS fsobj to destroy.

    """
    LOG.debug('_destroy_zfs_object() destroying {}'.format(fsobj))
    proc = subprocess.Popen(['zfs', 'destroy', fsobj],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    out, err = proc.communicate()
    if proc.returncode != 0:
        LOG.error('zfs destroy error: {}'.format(err))
        raise RuntimeError('zfs destroy failed')


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
    LOG.debug('_ensure_vault_exists() creating {}'.format(vault_name))
    vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
    return vault_name, vault_obj


def _create_archive(config, path, archive_name):
    """Create the set of archive files to be uploaded to a vault.

    :param config: ConfigParser instance with akashita configuration.
    :param path: path containing files to archive.
    :param archive_name: name prefix for the archive files.

    Returns the paths of the archive parts that were created.

    """
    # create a temporary working directory for the archive parts
    tmpdir = config.get('paths', 'tmpdir')
    tag = datetime.utcnow().strftime("%Y-%m-%d")
    root = os.path.join(tmpdir, '{}-{}'.format(archive_name, tag))
    if not os.path.exists(root):
        os.makedirs(root)
    with pop_chdir(root):
        LOG.debug('_create_archive() compressing {}'.format(path))
        tar = subprocess.Popen(['tar', 'Jc', path], stdout=subprocess.PIPE)
        split = subprocess.Popen(['split', '-b', '128m', '-', archive_name],
                                 stdin=tar.stdout, stdout=subprocess.PIPE)
        # Allow tar process to receive a SIGPIPE if split exits early.
        tar.stdout.close()
        # Read the outputs so the process finishes, but ignore them.
        split.communicate()
        if tar.returncode != 0 and tar.returncode is not None:
            raise subprocess.CalledProcessError(tar.returncode, "tar")
        if split.returncode != 0 and split.returncode is not None:
            raise subprocess.CalledProcessError(split.returncode, "split")
    return [os.path.join(root, name) for name in os.listdir() if name.startswith(archive_name)]


def _process_vault(config, section, name, layer2_obj):
    """Perform the backup for a particular "vault".

    :param config: ConfigParser instance with akashita configuration.
    :param section: vault section from config.
    :param name: name prefix of the vault to process.
    :param layer2_obj: instance of glacier.layer2.Layer2()

    """
    # ensure the vault exists
    vault_name, vault_obj = _ensure_vault_exists(layer2_obj, name)
    # ensure a zfs snapshot exists
    fs_path = config.get(section, 'fs_path')
    snapshot_name = _ensure_snapshot_exists(fs_path)
    # ensure the zfs clone exists
    clone_base = config.get(section, 'clone_base')
    clone_name = os.path.join(clone_base, vault_name)
    _ensure_clone_exists(clone_name, snapshot_name)
    try:
        # process each of the archives specified in the config file
        is_archive = lambda n: n.startswith(ARCHIVE_PREFIX)
        archives = [n for n in config.options(section) if is_archive(n)]
        for archive in archives:
            LOG.debug('_process_vault() processing archive {}'.format(archive))
            archive_name = archive[len(ARCHIVE_PREFIX):]
            # TODO: ideally would consult SimpleDB to know if this archive
            #       has already been successfully uploaded to this vault
            parts = _create_archive(config, clone_name, archive_name)
            part_num = 1
            for part in parts:
                desc = '{}-{}'.format(archive_name, str(part_num))
                LOG.debug('_process_vault() processing archive part {}'.format(part_num))
                # sleep until we reach the 'go' time
                while not _is_go_time(config):
                    LOG.debug('_process_vault() sleeping...')
                    time.sleep(60 * 60 * 10)
                vault_obj.upload_archive(part, desc)
                part_num += 1
                # remove each part as it is uploaded
                os.unlink(part)
                LOG.info('Finished archive {}, part {}'.format(archive, part_num))
            # remove the temporary working directory
            os.rmdir(os.path.dirname(parts[0]))
            # TODO: store archive metadata in SimpleDB for fast retrieval
    finally:
        _destroy_zfs_object(clone_name)
        _destroy_zfs_object(snapshot_name)


def _load_configuration():
    """Find and load the configuration file."""
    config = configparser.ConfigParser()
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
    _prune_vaults()


def main():
    """Do the thing."""
    _configure_logging()
    parser = argparse.ArgumentParser(description="Upload archives to Amazon Glacier.")
    parser.add_argument("-R", "--regions", action='store_true',
                        help="list available Amazon Glacier regions")
    args = parser.parse_args()
    if args.regions:
        for reg_info in boto.glacier.regions():
            print("Region: {}".format(reg_info.name))
    else:
        config = _load_configuration()
        _perform_backup(config)


if __name__ == "__main__":
    main()
