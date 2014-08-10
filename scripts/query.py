#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
"""Query contents of vaults on Amazon Glacier.

This script is used to query most of the interesting data regarding vaults,
including the list of vaults, inventory of vaults, and jobs related to
vaults.

Requires Amazon Web Services module boto (https://github.com/boto/boto)

"""

import argparse
import logging
import os
import sys

import boto.glacier
from boto.glacier.exceptions import UnexpectedHTTPResponseError
import boto.glacier.layer2
import boto.glacier.vault

try:
    import akashita
except ImportError:
    sys.path.append(os.path.dirname(os.path.abspath(__file__)))
    import akashita

LOG = logging.getLogger('akashita')


def _print_regions():
    """List the regions available for Amazon Glacier."""
    names = [region.name for region in boto.glacier.regions()]
    names.sort()
    for name in names:
        print(name)


def _print_vaults(layer2_obj):
    """List information about vaults on Amazon Glacier.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    """
    vaults = layer2_obj.list_vaults()
    max_name_len = 0
    max_size_len = 0
    for vault in vaults:
        if len(vault.arn) > max_name_len:
            max_name_len = len(vault.arn)
        size = "{:,}".format(vault.size)
        if len(size) > max_size_len:
            max_size_len = len(size)
    total_size = 0
    num_archives = 0
    for vault in vaults:
        total_size += vault.size
        num_archives += vault.number_of_archives
        print("{0:{aw}} || {1} || {2:4} || {3:>{sw},}".format(
            vault.arn, vault.creation_date, vault.number_of_archives, vault.size,
            aw=max_name_len, sw=max_size_len))
    print("Number of vaults: {0}".format(len(vaults)))
    print("Number of archives: {0}".format(num_archives))
    print("Total size: {0:,} MB".format(total_size / 1048576))


def _request_inventory(layer2_obj, vault_name):
    """Initiate a job to get the inventory of the named vault.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type vault_name: str
    :param vault_name: name of the Glacier vault to query.

    """
    try:
        response_data = layer2_obj.layer1.describe_vault(vault_name)
        vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
        job_id = vault_obj.retrieve_inventory()
        print("Job ID: {}".format(job_id))
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such vault!\n')


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


def _query_job_status(layer2_obj, job_id, vault_name):
    """Query the status of a job.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type job_id: str
    :param job_id: job identifier.

    :type vault_name: str
    :param vault_name: name of vault for which to list jobs.

    """
    try:
        response_data = layer2_obj.layer1.describe_vault(vault_name)
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such vault!\n')
        return
    vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
    try:
        job_obj = vault_obj.get_job(job_id)
        _print_job(job_obj)
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such job for this vault!\n')


def _list_vault_jobs(layer2_obj):
    """Retrieve the list of jobs associated with the named vault.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    """
    vaults = layer2_obj.list_vaults()
    for vault in vaults:
        response_data = layer2_obj.layer1.describe_vault(vault.name)
        vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
        jobs = vault_obj.list_jobs()
        if jobs:
            print("-"*70)
            print('Jobs for vault {}...'.format(vault.name))
            for job_obj in jobs:
                print('')
                _print_job(job_obj, print_id=True)
                print('')


def _print_job_output(layer2_obj, job_id, vault_name):
    """Retrieve the output of an inventory retrieval job.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type job_id: str
    :param job_id: job identifier.

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
            print(job_obj.get_output())
    else:
        print("Job not yet completed!")


def main():
    """Query contents of vaults on Amazon Glacier."""
    # parse command-line arguments
    parser = argparse.ArgumentParser(description="Query contents of vaults on Amazon Glacier.")
    parser.add_argument("-V", "--vaults", action='store_true',
                        help="list vaults on Amazon Glacier")
    parser.add_argument("-R", "--regions", action='store_true',
                        help="list available Amazon Glacier regions")
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

    # load configuration and prepare Glacier API
    config = akashita.load_configuration(LOG)
    akashita.configure_logging(LOG, config)
    aws_access_key_id = config.get('aws', 'access_key')
    aws_secret_access_key = config.get('aws', 'secret_key')
    region_name = config.get('aws', 'region_name')
    layer2_obj = boto.glacier.layer2.Layer2(
        aws_access_key_id, aws_secret_access_key, region_name=region_name)

    # perform one operation or another based on arguments
    if args.regions:
        _print_regions()
    elif args.vaults:
        _print_vaults(layer2_obj)
    elif args.inventory:
        _request_inventory(layer2_obj, args.inventory)
    elif args.status:
        if args.vault is None:
            sys.stderr.write('The vault name is required for querying job status.\n')
            sys.exit(1)
        _query_job_status(layer2_obj, args.status, args.vault)
    elif args.output:
        if args.vault is None:
            sys.stderr.write('The vault name is required for getting job output.\n')
            sys.exit(1)
        _print_job_output(layer2_obj, args.output, args.vault)
    elif args.joblist:
        _list_vault_jobs(layer2_obj)
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
