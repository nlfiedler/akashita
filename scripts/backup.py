#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# -------------------------------------------------------------------
#
# Copyright (c) 2014-2016 Nathan Fiedler
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#
# -------------------------------------------------------------------
"""Creates archives and uploads them to Amazon Glacier.

Based on a configuration file, takes a ZFS snapshot and creates one or more
compressed archives, splits them into manageable pieces, and uploads them
to a new vault.

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
import socket
import ssl
import subprocess
import sys
import time

from boto.glacier.exceptions import UnexpectedHTTPResponseError
import boto.glacier.layer2
import boto.glacier.vault

DEFAULT_LOG_FORMAT = '[%(process)d] <%(asctime)s> (%(name)s) {%(levelname)s} %(message)s'
DEFAULT_LOG_FILE = 'akashita.log'
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
        LOG.debug('_ensure_snapshot_exists() creating %s', snapshot)
        cmd = [
            'zfs', 'snapshot',
            '-o', 'com.sun:auto-snapshot=false',
            snapshot
        ]
        subprocess.check_output(cmd, stderr=subprocess.STDOUT)
        LOG.info('snapshot %s created', snapshot)
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
        LOG.debug('_ensure_clone_exists() creating %s from %s', clone, snapshot)
        cmd = [
            'zfs', 'clone',
            '-o', 'com.sun:auto-snapshot=false',
            '-p',
            snapshot,
            clone
        ]
        subprocess.check_output(cmd, stderr=subprocess.STDOUT)
        LOG.info('clone %s created', clone)


def _destroy_zfs_object(fsobj):
    """Destroy the named ZFS data set.

    :type fsobj: str
    :param fsobj: name of ZFS data set to destroy

    """
    LOG.debug('_destroy_zfs_object() destroying %s', fsobj)
    subprocess.check_output(['zfs', 'destroy', fsobj],
                            stderr=subprocess.STDOUT)
    LOG.info('zfs data set %s destroyed', fsobj)


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
    LOG.info('created vault %s', vault_name)
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
                LOG.debug('_create_archive() compressing %s', archive_name)
                tar_cmd = _build_tar_cmd(config, section, archive_name, paths)
                LOG.debug('_create_archive(): %s', tar_cmd)
                tar = subprocess.Popen(tar_cmd,
                                       stdout=subprocess.PIPE,
                                       stderr=devnull)
                split_cmd = _build_split_cmd(config, archive_name)
                LOG.debug('_create_archive(): %s', split_cmd)
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
                    LOG.error('split output: %s', out)
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


def _upload_archive(vault_obj, part, desc):
    """Upload the given part and return an archive identifier.

    Will retry if certain errors are raised.

    :type vault_obj: :class:`boto.glacier.vault.Vault`
    :param vault_obj: Glacier vault object.

    :param str part: name of file to upload.
    :param str desc: description for the archive.

    :rtype: str
    :return: archive identifier.

    """
    archive_id = None
    while archive_id is None:
        try:
            archive_id = vault_obj.upload_archive(part, desc)
        except UnexpectedHTTPResponseError as err:
            LOG.warning('upload of %s failed due to %s, retrying...', desc, err)
        except ssl.SSLError as err:
            LOG.warning('upload of %s failed due to %s, retrying...', desc, err)
        except socket.error as err:
            LOG.warning('upload of %s failed due to %s, retrying...', desc, err)
        except socket.herror as err:
            LOG.warning('upload of %s failed due to %s, retrying...', desc, err)
        except socket.gaierror as err:
            LOG.warning('upload of %s failed due to %s, retrying...', desc, err)
        except socket.timeout as err:
            LOG.warning('upload of %s failed due to %s, retrying...', desc, err)
    return archive_id


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
    LOG.info('processing vault %s', vault_name)
    # ensure a zfs snapshot exists
    dataset = config.get(section, 'dataset')
    snapshot_name = _ensure_snapshot_exists(tag, dataset)
    # ensure the zfs clone exists
    clone_base = config.get(section, 'clone_base')
    clone_name = clone_base + '/' + vault_name
    clone_path = '/' + clone_name
    _ensure_clone_exists(clone_name, snapshot_name)
    # process each of the archives specified in the config file
    archives = [n for n in config.options(section) if n.startswith(ARCHIVE_PREFIX)]
    completed_archives = _read_completed_archives(name)
    for archive in archives:
        archive_name = archive[len(ARCHIVE_PREFIX):]
        if archive_name in completed_archives:
            continue
        paths = []
        for entry in config.get(section, archive).split(','):
            paths.append(os.path.join(clone_path, entry.strip()))
        LOG.debug('_process_vault() processing archive %s', archive_name)
        parts_dir = None
        for part in _create_archive(tag, config, section, paths, archive_name):
            # sleep until we reach the 'go' time
            while not is_go_time(config, time.localtime()):
                LOG.debug('_process_vault() sleeping...')
                time.sleep(60 * 10)
            LOG.debug('_process_vault() processing archive %s', part)
            start_time = time.time()
            desc = 'archive:{};;file:{}'.format(archive_name, os.path.basename(part))
            LOG.info('uploading %s...', desc)
            archive_id = _upload_archive(vault_obj, part, desc)
            elapsed = (time.time() - start_time) / 60
            LOG.info('uploaded archive {} to vault {} in {:.1f} minutes'.format(
                archive_id, vault_name, elapsed))
            # remove each part as it is uploaded
            os.unlink(part)
            LOG.info('finished part %s', part)
            if parts_dir is None:
                parts_dir = os.path.dirname(part)
        # remove the temporary working directory
        os.rmdir(parts_dir)
        LOG.info('finished archive %s', archive_name)
        completed_archives.append(archive_name)
        _write_completed_archives(name, completed_archives)
    # remove the ZFS datasets only if successfully backed up
    _destroy_zfs_object(clone_name)
    _destroy_zfs_object(snapshot_name)
    LOG.info('processing vault %s completed', vault_name)


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
    vaults = [n for n in config.sections() if n.startswith(VAULT_PREFIX)]
    # fetch or compute the tag for this backup operation
    tag = _load_or_compute_tag()
    success = True
    completed_vaults = _read_completed_vaults()
    for section_name in vaults:
        try:
            vault_name = section_name[len(VAULT_PREFIX):]
            if vault_name not in completed_vaults:
                _process_vault(tag, config, section_name, vault_name, layer2_obj)
                completed_vaults.append(vault_name)
                _write_completed_vaults(completed_vaults)
        except:
            LOG.exception('vault processing failed for %s', section_name)
            success = False
    # after successful completion, delete the meta data
    if success:
        os.unlink(METADATA_FILE)
        LOG.debug('_perform_backup() deleted %s', METADATA_FILE)


def _load_or_compute_tag():
    """Determine the tag to be used to create snapshots, vaults, etc."""
    config = ConfigParser.ConfigParser()
    if os.path.exists(METADATA_FILE):
        # restore the last saved tag from before the crash
        LOG.debug('_load_or_compute_tag() reading %s', METADATA_FILE)
        config.read(METADATA_FILE)
        tag = config.get('meta', 'tag')
    else:
        # generate a new tag and save immediately
        tag = datetime.datetime.utcnow().strftime("%Y-%m-%d")
        config.add_section('meta')
        config.set('meta', 'tag', tag)
        with open(METADATA_FILE, 'w') as fobj:
            config.write(fobj)
        LOG.debug('_load_or_compute_tag() saved %s', METADATA_FILE)
    return tag


def _read_completed_vaults():
    """Return the list of vaults that have already been completed."""
    config = ConfigParser.ConfigParser()
    if os.path.exists(METADATA_FILE):
        LOG.debug('_read_completed_vaults() reading %s', METADATA_FILE)
        config.read(METADATA_FILE)
        if config.has_option('meta', 'vaults'):
            vaults = config.get('meta', 'vaults')
            return vaults.split(',')
    return []


def _write_completed_vaults(vaults):
    """Save the list of completed vaults to the metadata file.

    :param list vaults: list of vault names

    """
    config = ConfigParser.ConfigParser()
    if os.path.exists(METADATA_FILE):
        LOG.debug('_write_completed_vaults() reading %s', METADATA_FILE)
        config.read(METADATA_FILE)
    if not config.has_section('meta'):
        config.add_section('meta')
    config.set('meta', 'vaults', ','.join(vaults))
    with open(METADATA_FILE, 'w') as fobj:
        config.write(fobj)
    LOG.debug('_write_completed_vaults() saved %s', METADATA_FILE)


def _read_completed_archives(vault):
    """Return the list of archives that have already been completed.

    :param str vault: name of vault.
    :return: list of completed archives in named vault.

    """
    config = ConfigParser.ConfigParser()
    if os.path.exists(METADATA_FILE):
        LOG.debug('_read_completed_archives() reading %s', METADATA_FILE)
        config.read(METADATA_FILE)
        section_name = VAULT_PREFIX + vault
        if config.has_option(section_name, 'archives'):
            archives = config.get(section_name, 'archives')
            return archives.split(',')
    return []


def _write_completed_archives(vault, archives):
    """Save the list of completed archives to the metadata file.

    :param str vault: name of vault.
    :param list archives: list of archive names

    """
    config = ConfigParser.ConfigParser()
    if os.path.exists(METADATA_FILE):
        LOG.debug('_write_completed_archives() reading %s', METADATA_FILE)
        config.read(METADATA_FILE)
    section_name = VAULT_PREFIX + vault
    if not config.has_section(section_name):
        config.add_section(section_name)
    config.set(section_name, 'archives', ','.join(archives))
    with open(METADATA_FILE, 'w') as fobj:
        config.write(fobj)
    LOG.debug('_write_completed_archives() saved %s', METADATA_FILE)


def _load_configuration(log):
    """Find and load the configuration file.

    :param log: instance of logging.Logger.

    """
    # no interpolation, since logging record format includes %
    config = ConfigParser.RawConfigParser()
    fname = os.path.expanduser('~/.akashita')
    if os.path.exists(fname):
        log.debug('_load_configuration() reading {}'.format(fname))
        config.read(fname)
    elif os.path.exists('/usr/local/etc/akashita.conf'):
        log.debug('_load_configuration() reading /usr/local/etc/akashita.conf')
        config.read('/usr/local/etc/akashita.conf')
    elif os.path.exists('/etc/akashita.conf'):
        log.debug('_load_configuration() reading /etc/akashita.conf')
        config.read('/etc/akashita.conf')
    else:
        log.error("Could not find configuration file!")
        sys.stderr.write("Could not find configuration file!\n")
        sys.exit(1)
    return config


def _configure_logging(log, config):
    """Configure the logger for our purposes.

    :param log: instance of logging.Logger.
    :param config: ConfigParser instance with akashita configuration.

    """
    log.setLevel(logging.DEBUG)
    if config.has_option('logging', 'file'):
        log_file = config.get('logging', 'file')
    else:
        log_file = DEFAULT_LOG_FILE
    handler = logging.FileHandler(log_file)
    handler.setLevel(logging.DEBUG)
    if config.has_option('logging', 'format'):
        log_format = config.get('logging', 'format')
        log_format = log_format.strip('"').strip("'")
    else:
        log_format = DEFAULT_LOG_FORMAT
    formatter = logging.Formatter(log_format)
    handler.setFormatter(formatter)
    log.addHandler(handler)


def main():
    """Create archives and upload to vaults on Amazon Glacier."""
    parser = argparse.ArgumentParser(description="Backup files to Amazon Glacier.")
    parser.add_argument("-T", "--test", action="store_true",
                        help="split archives into smaller files")
    args = parser.parse_args()
    config = _load_configuration(LOG)
    _configure_logging(LOG, config)
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
