#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# -------------------------------------------------------------------
#
# Copyright (c) 2014 Nathan Fiedler
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
"""Prune old vaults from Amazon Glacier.

Use the query.py script to request the inventory of a vault before using
this script to empty the vault. In fact, Amazon requires that you request
the inventory of the _empty_ vault before you can actually delete the
vault, hence the two separate command-line arguments for this script,
--empty and --delete (the former removes the archives based on a completed
job and the latter removes the empty vault, again based on a completed
inventory retrieval job).

Requires Amazon Web Services module boto (https://github.com/boto/boto)

"""

import argparse
import logging
import os
import sys

from boto.glacier.exceptions import UnexpectedHTTPResponseError
import boto.glacier.layer2

try:
    import akashita
except ImportError:
    sys.path.append(os.path.dirname(os.path.abspath(__file__)))
    import akashita

LOG = logging.getLogger('akashita')


def _empty_vault(layer2_obj, job_id, vault_name):
    """Remove the archives from the named vault.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type job_id: str
    :param job_id: job identifier for the completed inventory job.

    :type vault_name: str
    :param vault_name: name of vault associated with the given job.

    """
    try:
        response_data = layer2_obj.layer1.describe_vault(vault_name)
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such vault!\n')
        return
    vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
    try:
        job_obj = vault_obj.get_job(job_id)
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such job for this vault!\n')
        return
    if job_obj.completed:
        if job_obj.action == 'InventoryRetrieval':
            output_map = job_obj.get_output()
            archive_list = output_map['ArchiveList']
            for archive_map in archive_list:
                archive_id = archive_map['ArchiveId']
                vault_obj.delete_archive(archive_id)
                print("Deleted archive {}".format(archive_id))
        else:
            print("Job is not an inventory retrieval job!")
    else:
        print("Job not yet completed!")


def _delete_vault(layer2_obj, job_id, vault_name):
    """Delete the empty vault.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type job_id: str
    :param job_id: job identifier for the completed inventory job.

    :type vault_name: str
    :param vault_name: name of vault associated with the given job.

    """
    try:
        response_data = layer2_obj.layer1.describe_vault(vault_name)
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such vault!\n')
        return
    vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
    try:
        job_obj = vault_obj.get_job(job_id)
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such job for this vault!\n')
        return
    if job_obj.completed:
        if job_obj.action == 'InventoryRetrieval':
            vault_obj.delete()
            print("Deleted vault {}".format(vault_name))
        else:
            print("Job is not an inventory retrieval job!")
    else:
        print("Job not yet completed!")


def main():
    """Remove any old vaults from Amazon Glacier."""
    # parse command-line arguments
    parser = argparse.ArgumentParser(description="Prune old archives from Amazon Glacier.")
    parser.add_argument("-E", "--empty", metavar='JOBID',
                        help="remove the archives within a vault")
    parser.add_argument("-D", "--delete", metavar='JOBID',
                        help="delete the empty vault")
    parser.add_argument('vault', metavar='V', nargs='?',
                        help='name of vault to query')
    args = parser.parse_args()

    # load configuration and prepare Glacier API
    config = akashita.load_configuration(LOG)
    aws_access_key_id = config.get('aws', 'access_key')
    aws_secret_access_key = config.get('aws', 'secret_key')
    region_name = config.get('aws', 'region_name')
    layer2_obj = boto.glacier.layer2.Layer2(
        aws_access_key_id, aws_secret_access_key, region_name=region_name)

    # perform one operation or another based on arguments
    if args.empty:
        if args.vault is None:
            sys.stderr.write('The vault name is required for emptying a vault.\n')
            sys.exit(1)
        _empty_vault(layer2_obj, args.empty, args.vault)
    elif args.delete:
        if args.vault is None:
            sys.stderr.write('The vault name is required for deleting a vault.\n')
            sys.exit(1)
        _delete_vault(layer2_obj, args.delete, args.vault)


if __name__ == "__main__":
    main()
