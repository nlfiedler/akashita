#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
"""Request and retrieve archives stored on Amazon Glacier.

To use this script, first get the archive ID of the archive to be restored
by invoking the query.py script (-I vaultname). Once the inventory
retrieval job is complete, get the output of the job, which will include
the archive IDs. Then use this script to initiate the archive retrieval.
Once _that_ job is complete, then you can fetch the job output using this
script.

Requires Amazon Web Services module boto (https://github.com/boto/boto)

"""

import argparse
import logging
import os
import sys
import tempfile
import time

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


def _request_archive(layer2_obj, archive_id, vault_name):
    """Initiate a job to retrieve an archive of the named vault.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type archive_id: str
    :param archive_id: archive identifier

    :type vault_name: str
    :param vault_name: name of the Glacier vault to query

    """
    try:
        response_data = layer2_obj.layer1.describe_vault(vault_name)
    except UnexpectedHTTPResponseError:
        sys.stderr.write('No such vault!\n')
        return
    vault_obj = boto.glacier.vault.Vault(layer2_obj.layer1, response_data)
    job_obj = vault_obj.retrieve_archive(archive_id)
    print("Job ID: {}".format(job_obj.id))


def _save_job_output(layer2_obj, job_id, vault_name):
    """Retrieve the output of an archive retrieval job.

    :type layer2_obj: :class:`glacier.layer2.Layer2`
    :param layer2_obj: Layer2 API instance

    :type job_id: str
    :param job_id: job identifier

    :type vault_name: str
    :param vault_name: name of vault associated with the given job

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
        if job_obj.action == 'ArchiveRetrieval':
            _, filename = tempfile.mkstemp(suffix='.dat', prefix='job_output_', dir=os.getcwd())
            print("Downloading job output to {}...".format(filename))
            start_time = time.time()
            job_obj.download_to_file(filename)
            elapsed = (time.time() - start_time) / 60
            print('Download completed in {:.1f} minutes'.format(elapsed))
        else:
            print("Do not know what to do with this job:")
            print(job_obj.get_output())
    else:
        print("Job not yet completed!")


def main():
    """Query contents of vaults on Amazon Glacier."""
    # parse command-line arguments
    parser = argparse.ArgumentParser(description="Query contents of vaults on Amazon Glacier.")
    parser.add_argument('-R', '--request', metavar='ARCHIVE_ID',
                        help='request retrieval of given archive')
    parser.add_argument('-F', '--fetch', metavar='JOB_ID',
                        help='retrieve job output to a file')
    parser.add_argument('vault', metavar='V',
                        help='name of vault for job/archive')
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
    if args.request:
        _request_archive(layer2_obj, args.request, args.vault)
    elif args.fetch:
        _save_job_output(layer2_obj, args.fetch, args.vault)
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
