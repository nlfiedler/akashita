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
"""Fabric file for installing requirements on Ubuntu Linux."""

import os

from fabric.api import cd, env, path, run, shell_env, sudo, task
from fabric.contrib.files import append

env.hosts = ["default"]
env.use_ssh_config = True
if os.path.exists("user_ssh_config"):
    env.ssh_config_path = "user_ssh_config"
else:
    env.ssh_config_path = "ssh_config"

DIR_FOP = 'fop-1.1'
TAR_FOP = '{}-bin.tar.gz'.format(DIR_FOP)
URL_FOP = 'http://mirrors.sonic.net/apache/xmlgraphics/fop/binaries/{}'.format(TAR_FOP)
DIR_OTP = 'otp_src_18.2.1'
TAR_OTP = '{}.tar.gz'.format(DIR_OTP)
URL_OTP = 'http://erlang.org/download/{}'.format(TAR_OTP)
URL_GOLANG = 'https://storage.googleapis.com/golang/go1.5.3.linux-amd64.tar.gz'


@task
def all():
    """Install everything needed for akashita."""
    install_erlang()
    install_golang()
    install_rebar()


@task
def install_erlang():
    """Install Erlang/OTP."""
    # Install the compilers, JDK, and XML tools
    pre_reqs = [
        'build-essential',
        'default-jdk',
        'libncurses5-dev',
        'libssl-dev',
        'libxml2-utils',
        'xsltproc',
    ]
    sudo('apt-get -q -y install {}'.format(' '.join(pre_reqs)))
    # Fetch and extract Apache FOP binary tarball
    run('wget -q {}'.format(URL_FOP))
    run('tar zxf {}'.format(TAR_FOP))
    # Prepare to build Erlang/OTP from source
    run('wget -q {}'.format(URL_OTP))
    run('tar zxf {}'.format(TAR_OTP))
    path_addend = '~/{}'.format(DIR_FOP)
    with cd(DIR_OTP), path(path_addend):
        run('./configure')
        run('make')
        sudo('make install')
        with shell_env(FOP_OPTS="-Xmx512m"):
            run('make docs')
            sudo('make install-docs')
    run('rm -rf {}* {}*'.format(DIR_FOP, DIR_OTP))


@task
def install_golang():
    """Install the Go programming language."""
    run('wget -q {}'.format(URL_GOLANG))
    sudo('tar -C /usr/local -xzf go1.5.3.linux-amd64.tar.gz')
    run('rm -f go1.5.3.linux-amd64.tar.gz')
    append('.profile', 'export PATH=$PATH:/usr/local/go/bin')
    run('mkdir gocode')
    append('.profile', 'export GOPATH=~/gocode')


@task
def install_git():
    """Build and install Git."""
    if run('which git', quiet=True).return_code != 0:
        sudo('apt-get -q -y install git')


@task
def install_rebar():
    """Build and install the rebar build tool."""
    install_git()
    run('git clone -q https://github.com/rebar/rebar.git')
    with cd('rebar'):
        run('git checkout 2.6.1')
        run('./bootstrap')
        sudo('cp rebar /usr/local/bin')
    run('rm -rf rebar')
