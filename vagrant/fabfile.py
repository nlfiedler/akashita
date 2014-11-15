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
"""Fabric file for installing requirements on Ubuntu Linux."""

from fabric.api import sudo, task

# TODO: may need to patch Python 2.7 to work around httplib and unicode bug
#       (see http://bugs.python.org/issue11898 for details and patch)
#       (file to patch is /usr/lib/python2.7/httplib.py, line ~818)


@task
def all():
    """Install everything needed for akashita."""
    install_boto()


@task
def install_boto():
    """Install Amazon Web Service API (boto)."""
    # Use pip to get the most recent version of boto (Ubuntu tends to lag
    # behind quite a bit).
    sudo('apt-get install -q -y python-pip')
    sudo('pip2 -q install boto')
