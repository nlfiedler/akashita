#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# -------------------------------------------------------------------
#
# Copyright (c) 2016 Nathan Fiedler
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
"""Upload a single file to Amazon Glacier."""

import argparse

import boto3
import botocore.utils

ACCOUNT_ID = '-'
CHUNK_SIZE = 1024 * 1024


def _upload_file(filename, vault_name, verbose):
    """Upload the named file to the named vault."""
    checksum = botocore.utils.calculate_tree_hash(open(filename, 'rb'))
    glacier = boto3.resource('glacier')
    vault = glacier.Vault(ACCOUNT_ID, vault_name)
    uploader = vault.initiate_multipart_upload(
        archiveDescription="file:".format(filename), partSize=str(CHUNK_SIZE))
    archive_pos = 0
    with open(filename, 'rb') as fobj:
        while True:
            buf = fobj.read(CHUNK_SIZE)
            if not buf:
                break
            # See RFC 2616 Content-Range for the format of the range argument.
            strange = "bytes {}-{}/*".format(archive_pos, archive_pos + len(buf) - 1)
            uploader.upload_part(range=strange, body=buf)
            archive_pos += len(buf)
            if verbose:
                print('.', end='', flush=True)
    if verbose:
        print('\ndone')
    # by this point, archive_pos equals the size of the uploaded data
    result = uploader.complete(archiveSize=str(archive_pos), checksum=checksum)
    print(result['archiveId'])


def main():
    """Remove any old vaults from Amazon Glacier."""
    # parse command-line arguments
    parser = argparse.ArgumentParser(description="Upload a file to Amazon Glacier.")
    parser.add_argument('-f', '--file', metavar='F', required=True,
                        help='the file to be uploaded')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='print progress during upload')
    parser.add_argument('vault', metavar='V',
                        help='name of vault to receive the file')
    args = parser.parse_args()
    _upload_file(args.file, args.vault, args.verbose)


if __name__ == "__main__":
    main()
