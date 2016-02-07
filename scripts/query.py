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
"""Query contents of vaults on Amazon Glacier.

This script is used to query most of the interesting data regarding vaults,
including the list of vaults, inventory of vaults, and jobs related to
vaults.

Requires Amazon Web Services module boto3 (https://github.com/boto/boto3)

"""

import argparse
import sys

import boto3

ACCOUNT_ID = '-'


def _print_vaults():
    """List information about vaults on Amazon Glacier."""
    glacier = boto3.resource('glacier')
    vaults = glacier.vaults.all()
    max_name_len = 0
    max_size_len = 0
    number_of_vaults = 0
    for vault in vaults:
        if len(vault.vault_arn) > max_name_len:
            max_name_len = len(vault.vault_arn)
        size = "{:,}".format(int(vault.size_in_bytes / 1048576))
        if len(size) > max_size_len:
            max_size_len = len(size)
        number_of_vaults += 1
    total_size = 0
    num_archives = 0
    print("{0:{aw}} || {1:24} || {2:5} || {3:>{sw}}".format(
        "Vault ARN (/name)", "Creation Date", "Files", "MB", aw=max_name_len, sw=max_size_len))
    for vault in vaults:
        total_size += vault.size_in_bytes
        num_archives += vault.number_of_archives
        print("{0:{aw}} || {1} || {2:5} || {3:>{sw},}".format(
            vault.vault_arn, vault.creation_date, vault.number_of_archives,
            int(vault.size_in_bytes / 1048576), aw=max_name_len, sw=max_size_len))
    print("Number of vaults: {0}".format(number_of_vaults))
    print("Number of archives: {0}".format(num_archives))
    print("Total size: {0:,} MB".format(int(total_size / 1048576)))


def _request_inventory(vault_name):
    """Initiate a job to get the inventory of the named vault.

    :param str vault_name: name of the Glacier vault to query.

    """
    glacier = boto3.resource('glacier')
    vault = glacier.Vault(ACCOUNT_ID, vault_name)
    job = vault.initiate_inventory_retrieval()
    print("Job ID: {}".format(job.id))


def _print_job(job_obj, print_id=False):
    """Print details for a vault-related job object.

    :type job_obj: :class:`boto.glacier.job.Job`
    :param job_obj: the job to print.

    :type print_id: bool
    :param print_id: if True, print the job identifier (default is False).

    """
    if print_id:
        print("Job ID: {}".format(job_obj.id))
    print("Action: {}".format(job_obj.action))
    print("Creation Date: {}".format(job_obj.creation_date))
    if job_obj.completed:
        print("Completion Date: {}".format(job_obj.completion_date))
    print("Status Code: {}".format(job_obj.status_code))
    if job_obj.status_message:
        print("Status Message: {}".format(job_obj.status_message))


def _query_job_status(job_id, vault_name):
    """Query the status of a job.

    :param str job_id: job identifier.
    :param str vault_name: name of vault for which to list jobs.

    """
    glacier = boto3.resource('glacier')
    job = glacier.Job(ACCOUNT_ID, vault_name, job_id)
    _print_job(job)


def _list_vault_jobs():
    """Retrieve the list of jobs for all vaults."""
    glacier = boto3.resource('glacier')
    vaults = glacier.vaults.all()
    for vault in vaults:
        jobs = vault.jobs.all()
        print('-'*70)
        print('Jobs for vault {}...'.format(vault.vault_name))
        for job_obj in jobs:
            print('')
            _print_job(job_obj, print_id=True)
            print('')


def _print_job_output(job_id, vault_name):
    """Retrieve the output of an inventory retrieval job.

    :param str job_id: job identifier.
    :param str vault_name: name of vault associated with the given job.

    """
    glacier = boto3.resource('glacier')
    vault = glacier.Vault(ACCOUNT_ID, vault_name)
    job = vault.Job(job_id)
    if job.completed:
        if job.action == 'InventoryRetrieval':
            output_map = job.get_output()
            output_keys = output_map.keys()
            output_keys.sort()
            for output_key in output_keys:
                if output_key == 'ArchiveList':
                    if output_map[output_key]:
                        print(output_key)
                        for archive_map in output_map[output_key]:
                            archive_keys = archive_map.keys()
                            archive_keys.sort()
                            for archive_key in archive_keys:
                                print("\t{}: {}".format(archive_key, archive_map[archive_key]))
                    else:
                        print("ArchiveList: (empty)")
                else:
                    print("{}: {}".format(output_key, output_map[output_key]))
        else:
            print("Do not know what to do with this job:")
            print(job.get_output())
    else:
        print("Job still in progress")


def main():
    """Query contents of vaults on Amazon Glacier."""
    # parse command-line arguments
    parser = argparse.ArgumentParser(description="Query contents of vaults on Amazon Glacier.")
    parser.add_argument("-V", "--vaults", action='store_true',
                        help="list vaults on Amazon Glacier")
    parser.add_argument('-I', '--inventory',
                        help='request inventory of named vault')
    parser.add_argument('-L', '--joblist', action='store_true',
                        help='list of jobs for all vaults')
    parser.add_argument('-S', '--status', metavar='JOBID',
                        help='query status of job')
    parser.add_argument('-O', '--output', metavar='JOBID',
                        help='retrieve job output to a file')
    parser.add_argument('vault', metavar='V', nargs='?',
                        help='name of vault to query')
    args = parser.parse_args()

    # perform one operation or another based on arguments
    if args.vaults:
        _print_vaults()
    elif args.inventory:
        _request_inventory(args.inventory)
    elif args.status:
        if args.vault is None:
            sys.stderr.write('The vault name is required for querying job status.\n')
            sys.exit(1)
        _query_job_status(args.status, args.vault)
    elif args.output:
        if args.vault is None:
            sys.stderr.write('The vault name is required for getting job output.\n')
            sys.exit(1)
        _print_job_output(args.output, args.vault)
    elif args.joblist:
        _list_vault_jobs()
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
