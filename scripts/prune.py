#!/usr/bin/env python3
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
"""Prune old vaults from Amazon Glacier.

Use the query.py script to request the inventory of a vault before using
this script to empty the vault. In fact, Amazon requires that you request
the inventory of the _empty_ vault before you can actually delete the
vault, hence the two separate command-line arguments for this script,
--empty and --delete (the former removes the archives based on a completed
job and the latter removes the empty vault, again based on a completed
inventory retrieval job).

Requires Amazon Web Services module boto3 (https://github.com/boto/boto3)

"""

import argparse
import json
import sys

import boto3

ACCOUNT_ID = '-'


def _empty_vault(job_id, vault_name):
    """Remove the archives from the named vault.

    :param str job_id: job identifier for the completed inventory job.
    :param str vault_name: name of vault associated with the given job.

    """
    glacier = boto3.resource('glacier')
    job = glacier.Job(ACCOUNT_ID, vault_name, job_id)
    if job.action == 'InventoryRetrieval':
        if job.completed:
            output_map = json.loads(job.get_output()['body'].read().decode('utf-8'))
            archive_list = output_map['ArchiveList']
            for archive_map in archive_list:
                archive_id = archive_map['ArchiveId']
                archive = glacier.Archive(ACCOUNT_ID, vault_name, archive_id)
                archive.delete()
                print("Deleted archive {}".format(archive_id))
        else:
            print("Job still in progress")
    else:
        print("Job is not an inventory retrieval job")


def _delete_vault(job_id, vault_name):
    """Delete the empty vault.

    :param str job_id: job identifier for the completed inventory job.
    :param str vault_name: name of vault associated with the given job.

    """
    glacier = boto3.resource('glacier')
    vault = glacier.Vault(ACCOUNT_ID, vault_name)
    job = glacier.Job(ACCOUNT_ID, vault_name, job_id)
    if job.action == 'InventoryRetrieval':
        if job.completed:
            vault.delete()
            print("Deleted vault {}".format(vault_name))
        else:
            print("Job still in progress")
    else:
        print("Job is not an inventory retrieval job")


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

    # perform one operation or another based on arguments
    if args.empty:
        if args.vault is None:
            sys.stderr.write('The vault name is required for emptying a vault.\n')
            sys.exit(1)
        _empty_vault(args.empty, args.vault)
    elif args.delete:
        if args.vault is None:
            sys.stderr.write('The vault name is required for deleting a vault.\n')
            sys.exit(1)
        _delete_vault(args.delete, args.vault)


if __name__ == "__main__":
    main()
