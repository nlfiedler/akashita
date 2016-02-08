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
"""Request and retrieve archives stored on Amazon Glacier.

To use this script, first get the archive ID of the archive to be restored
by invoking the query.py script (-I vaultname). Once the inventory
retrieval job is complete, get the output of the job, which will include
the archive IDs. Then use this script to initiate the archive retrieval.
Once _that_ job is complete, then you can fetch the job output using this
script.

Requires Amazon Web Services module boto3 (https://github.com/boto/boto3)

"""

import argparse
import os
import tempfile
import time

import boto3

ACCOUNT_ID = '-'


def _request_archive(archive_id, vault_name):
    """Initiate a job to retrieve an archive of the named vault.

    :param str archive_id: archive identifier
    :param str vault_name: name of the Glacier vault to query

    """
    glacier = boto3.resource('glacier')
    archive = glacier.Archive(ACCOUNT_ID, vault_name, archive_id)
    job = archive.initiate_archive_retrieval()
    # Allow for easy parsing by other programs.
    print("{}\t{}".format(job.id, vault_name))


def _save_job_output(job_id, vault_name):
    """Retrieve the output of an archive retrieval job.

    :param str job_id: job identifier
    :param str vault_name: name of vault associated with the given job

    """
    glacier = boto3.resource('glacier')
    job = glacier.Job(ACCOUNT_ID, vault_name, job_id)
    if job.action == 'ArchiveRetrieval':
        if job.completed:
            _, filename = tempfile.mkstemp(suffix='.dat', prefix='job_output_', dir=os.getcwd())
            print("Downloading job output to {}...".format(filename))
            start_time = time.time()
            _download_to_file(job, filename)
            elapsed = (time.time() - start_time) / 60
            print('Download completed in {:.1f} minutes'.format(elapsed))
        else:
            print("Job not yet completed!")
    else:
        print("Job is not an archive retrieval job")


def _download_to_file(job, filename):
    """Download the job output to the named file."""
    output = job.get_output()
    body = output['body']
    with open(filename, 'wb') as fobj:
        while True:
            buf = body.read(amt=1024*1024)
            fobj.write(buf)
            if not buf:
                break
    body.close()


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

    # perform one operation or another based on arguments
    if args.request:
        _request_archive(args.request, args.vault)
    elif args.fetch:
        _save_job_output(args.fetch, args.vault)
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
