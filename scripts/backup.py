#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
"""Creates archives and uploads them to Amazon Glacier.

Based on a configuration file, takes a ZFS snapshot and creates one or more
compressed archives, splits them into manageable pieces, and uploads them
to a new vault.

To test this script, create a disposable ZFS filesystem using the `mkfile`
command, like so:

$ mkfile 256m tank
$ pfexec zpool create tank $PWD/tank
$ pfexec zfs create tank/shared

Using the `at` command makes it easy to start the backup process in the
background with no controlling terminal.

Requires that xz is installed for compressing the tarballs (i.e. this
script invokes `tar Jc` in a child process).

Requires Amazon Web Services module boto (https://github.com/boto/boto)

"""

import argparse
import ConfigParser
from contextlib import contextmanager
import datetime
import logging
import os
import shutil
import subprocess
import sys
import time

import boto.glacier.layer2
import boto.glacier.vault

try:
    import akashita
except ImportError:
    sys.path.append(os.path.dirname(os.path.abspath(__file__)))
    import akashita


VAULT_PREFIX = 'vault_'
ARCHIVE_PREFIX = 'archive_'
METADATA_FILE = '.akashita-meta'

LOG = logging.getLogger('akashita')


def _ensure_snapshot_exists(tag, prefix):
    """Create the ZFS snapshot, if it is missing.

    :type tag: str
    :param tag: tag for creating snapshots and vaults.

    :type prefix: str
    :param prefix: name prefix of the snapshot to create

    :return: full name of the snapshot

    """
    snapshot = "{}@glacier:{}".format(prefix, tag)
    proc = subprocess.Popen(['zfs', 'list', '-H', snapshot],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    proc.communicate()
    if proc.returncode != 0:
        LOG.debug('_ensure_snapshot_exists() creating {}'.format(snapshot))
        cmd = [
            'zfs', 'snapshot',
            '-o', 'com.sun:auto-snapshot=false',
            snapshot
        ]
        subprocess.check_output(cmd, stderr=subprocess.STDOUT)
        LOG.info('snapshot {} created'.format(snapshot))
    return snapshot


def _ensure_clone_exists(clone, snapshot):
    """Create the ZFS clone, if it is missing.

    :type clone: str
    :param clone: name of the ZFS clone to create

    :type clone: str
    :param snapshot: name of the ZFS snapshot on which to base the clone

    """
    proc = subprocess.Popen(['zfs', 'list', '-H', clone],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    proc.communicate()
    if proc.returncode != 0:
        LOG.debug('_ensure_clone_exists() creating {} from {}'.format(clone, snapshot))
        cmd = [
            'zfs', 'clone',
            '-o', 'com.sun:auto-snapshot=false',
            '-p',
            snapshot,
            clone
        ]
        subprocess.check_output(cmd, stderr=subprocess.STDOUT)
        LOG.info('clone {} created'.format(clone))


def _destroy_zfs_object(fsobj):
    """Destroy the named ZFS data set.

    :type fsobj: str
    :param fsobj: name of ZFS data set to destroy

    """
    LOG.debug('_destroy_zfs_object() destroying {}'.format(fsobj))
    subprocess.check_output(['zfs', 'destroy', fsobj],
                            stderr=subprocess.STDOUT)
    LOG.info('zfs data set {} destroyed'.format(fsobj))


@contextmanager
def pop_chdir(path):
    """Context manager to temporarily change to the given directory."""
    cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(cwd)


def is_go_time(config, now):
    """Determine if current time within upload window.

    :type config: ConfigParser.ConfigParser
    :param config: akashita configuration

    :type now: :class:`time.struct_time`
    :param now: current time to be evaluated.

    :rtype: bool
    :return: True if current time within upload window, False otherwise

    """
    now_time = datetime.time(now.tm_hour, now.tm_min)
    windows = config.get('schedule', 'go_time').split(',')
    for window in windows:
        go_time, stop_time = window.split('-')
        go_time = time.strptime(go_time, '%H:%M')
        go_time = datetime.time(go_time.tm_hour, go_time.tm_min)
        stop_time = time.strptime(stop_time, '%H:%M')
        stop_time = datetime.time(stop_time.tm_hour, stop_time.tm_min)
        if stop_time < go_time:
            if go_time <= now_time or now_time <= stop_time:
                return True
        else:
            if go_time <= now_time and now_time <= stop_time:
                return True
    return False


def _ensure_vault_exists(tag, layer2_obj, prefix):
    """Create a vault with the given base name.

    This operation is idempotent.

    :type tag: str
    :param tag: tag for creating snapshots and vaults.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type prefix: str
    :param prefix: name prefix for the vault to create

    :rtype: str
    :return: the name of the vault and the Vault instance.

    """
    vault_name = '{}-{}'.format(prefix, tag)
    layer2_obj.layer1.create_vault(vault_name)
    response_data = layer2_obj.layer1.describe_vault(vault_name)
    LOG.info('created vault {}'.format(vault_name))
    vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
    return vault_name, vault_obj


def _build_tar_cmd(config, section, archive_name, paths):
    """Build up the list of strings to be used to invoke tar.

    :type config: ConfigParser.ConfigParser
    :param config: akashita configuration

    :type archive_name: str
    :param archive_name: name prefix for the archive files

    :type paths: list
    :param paths: paths to be archived

    """
    tar_cmd = ['tar']
    opt_name = 'options_{}'.format(archive_name)
    if config.has_option(section, opt_name):
        option_val = config.get(section, opt_name)
        options = [opt.strip() for opt in option_val.split(',')]
        if 'compressed' in options:
            tar_cmd.append('-J')
    opt_name = 'excludes_{}'.format(archive_name)
    if config.has_option(section, opt_name):
        option_val = config.get(section, opt_name)
        excludes = [opt.strip() for opt in option_val.split(',')]
        for patt in excludes:
            tar_cmd.append('--exclude={}'.format(patt))
    tar_cmd.append('-c')
    tar_cmd.extend(paths)
    return tar_cmd


def _build_split_cmd(config, archive_name):
    """Build up the list of strings to be used to invoke split.

    :type config: ConfigParser.ConfigParser
    :param config: akashita configuration

    :type archive_name: str
    :param archive_name: name prefix for the archive files

    """
    split_cmd = [
        'split',
        '-d',
        # split fails if it runs out of suffix digits, so give it
        # enough digits to handle our enormous files
        '-a', '4',
        '-b', config.get('split', 'size'),
        '-',
        archive_name
    ]
    return split_cmd


def _create_archive(tag, config, section, paths, archive_name):
    """Create the set of archive files to be uploaded to a vault.

    :type tag: str
    :param tag: tag for creating snapshots and vaults.

    :type config: ConfigParser.ConfigParser
    :param config: akashita configuration

    :type section: str
    :param section: vault section from config

    :type paths: list
    :param paths: paths to be archived

    :type archive_name: str
    :param archive_name: name prefix for the archive files

    This function is a generator of the paths of the archive parts
    that were created.

    """
    # create a temporary working directory for the archive parts
    tmpdir = config.get('paths', 'tmpdir')
    root = os.path.join(tmpdir, '{}-{}'.format(archive_name, tag))
    if os.path.exists(root) and len(os.listdir(root)) == 0:
        os.rmdir(root)
    if not os.path.exists(root):
        os.makedirs(root)
        devnull = os.open(os.devnull, os.O_RDWR)
        try:
            with pop_chdir(root):
                LOG.debug('_create_archive() compressing {}'.format(archive_name))
                tar_cmd = _build_tar_cmd(config, section, archive_name, paths)
                LOG.debug('_create_archive(): {}'.format(tar_cmd))
                tar = subprocess.Popen(tar_cmd,
                                       stdout=subprocess.PIPE,
                                       stderr=devnull)
                split_cmd = _build_split_cmd(config, archive_name)
                LOG.debug('_create_archive(): {}'.format(split_cmd))
                split = subprocess.Popen(split_cmd,
                                         stdin=tar.stdout,
                                         stdout=subprocess.PIPE,
                                         stderr=subprocess.STDOUT)
                # allow tar process to receive a SIGPIPE if split exits early
                tar.stdout.close()
                # read the outputs so the process finishes, but ignore them
                out, _ = split.communicate()
                if tar.returncode != 0 and tar.returncode is not None:
                    raise subprocess.CalledProcessError(tar.returncode, "tar")
                if split.returncode != 0 and split.returncode is not None:
                    LOG.error('split output: {}'.format(out))
                    raise subprocess.CalledProcessError(split.returncode, "split")
        except:
            LOG.exception('archive processing failed')
            # wash it all away and try again next time around
            shutil.rmtree(root)
            raise
        finally:
            os.close(devnull)
    for name in os.listdir(root):
        if name.startswith(archive_name):
            yield os.path.join(root, name)


def _process_vault(tag, config, section, name, layer2_obj):
    """Perform the backup for a particular "vault".

    :type tag: str
    :param tag: tag for creating snapshots and vaults.

    :type config: ConfigParser.ConfigParser
    :param config: akashita configuration

    :type section: str
    :param section: vault section from config

    :type name: str
    :param name: name prefix of the vault to process

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    """
    # ensure the vault exists
    vault_name, vault_obj = _ensure_vault_exists(tag, layer2_obj, name)
    LOG.info('processing vault {}'.format(vault_name))
    # ensure a zfs snapshot exists
    dataset = config.get(section, 'dataset')
    snapshot_name = _ensure_snapshot_exists(tag, dataset)
    # ensure the zfs clone exists
    clone_base = config.get(section, 'clone_base')
    clone_name = clone_base + '/' + vault_name
    clone_path = '/' + clone_name
    _ensure_clone_exists(clone_name, snapshot_name)
    # process each of the archives specified in the config file
    is_archive = lambda n: n.startswith(ARCHIVE_PREFIX)
    archives = [n for n in config.options(section) if is_archive(n)]
    for archive in archives:
        paths = []
        for entry in config.get(section, archive).split(','):
            paths.append(os.path.join(clone_path, entry.strip()))
        archive_name = archive[len(ARCHIVE_PREFIX):]
        LOG.debug('_process_vault() processing archive {}'.format(archive_name))
        parts_dir = None
        for part in _create_archive(tag, config, section, paths, archive_name):
            # sleep until we reach the 'go' time
            while not is_go_time(config, time.localtime()):
                LOG.debug('_process_vault() sleeping...')
                time.sleep(60 * 10)
            LOG.debug('_process_vault() processing archive {}'.format(part))
            start_time = time.time()
            desc = 'archive:{};;file:{}'.format(archive_name, os.path.basename(part))
            LOG.info('uploading {}...'.format(desc))
            archive_id = vault_obj.upload_archive(part, desc)
            elapsed = (time.time() - start_time) / 60
            LOG.info('uploaded archive {} to vault {} in {:.1f} minutes'.format(
                archive_id, vault_name, elapsed))
            # remove each part as it is uploaded
            os.unlink(part)
            LOG.info('Finished archive {}'.format(part))
            if parts_dir is None:
                parts_dir = os.path.dirname(part)
        # remove the temporary working directory
        os.rmdir(parts_dir)
        LOG.info('finished archive {}'.format(archive_name))
    # remove the ZFS datasets only if successfully backed up
    _destroy_zfs_object(clone_name)
    _destroy_zfs_object(snapshot_name)
    LOG.info('processing vault {} completed'.format(vault_name))


def _perform_backup(config):
    """Perform the backup procedure.

    :type config: ConfigParser.ConfigParser
    :param config: akashita configuration

    """
    aws_access_key_id = config.get('aws', 'access_key')
    aws_secret_access_key = config.get('aws', 'secret_key')
    region_name = config.get('aws', 'region_name')
    layer2_obj = boto.glacier.layer2.Layer2(
        aws_access_key_id, aws_secret_access_key, region_name=region_name)
    is_vault = lambda n: n.startswith(VAULT_PREFIX)
    vaults = [n for n in config.sections() if is_vault(n)]
    # fetch or compute the tag for this backup operation
    tag = _load_or_compute_tag()
    for section_name in vaults:
        try:
            vault_name = section_name[len(VAULT_PREFIX):]
            _process_vault(tag, config, section_name, vault_name, layer2_obj)
        except:
            LOG.exception('vault processing failed for {}'.format(section_name))
    # after successful completion, delete the meta data
    os.unlink(METADATA_FILE)
    LOG.debug('_perform_backup() deleted {}'.format(METADATA_FILE))


def _load_or_compute_tag():
    """Determine the tag to be used to create snapshots, vaults, etc."""
    config = ConfigParser.ConfigParser()
    if os.path.exists(METADATA_FILE):
        # restore the last saved tag from before the crash
        LOG.debug('_compute_tag() reading {}'.format(METADATA_FILE))
        config.read(METADATA_FILE)
        tag = config.get('meta', 'tag')
    else:
        # generate a new tag and save immediately
        tag = datetime.datetime.utcnow().strftime("%Y-%m-%d")
        config.add_section('meta')
        config.set('meta', 'tag', tag)
        with open(METADATA_FILE, 'w') as fobj:
            config.write(fobj)
        LOG.debug('_compute_tag() saved {}'.format(METADATA_FILE))
    return tag


def main():
    """Create archives and upload to vaults on Amazon Glacier."""
    parser = argparse.ArgumentParser(description="Backup files to Amazon Glacier.")
    parser.add_argument("-T", "--test", action="store_true",
                        help="split archives into smaller files")
    args = parser.parse_args()
    config = akashita.load_configuration(LOG)
    akashita.configure_logging(LOG, config)
    config.add_section('split')
    if args.test:
        config.set('split', 'size', '2M')
    else:
        config.set('split', 'size', '128M')
    LOG.info('backup process started')
    _perform_backup(config)
    LOG.info('backup process exiting')


if __name__ == "__main__":
    main()
