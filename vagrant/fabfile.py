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
"""Fabric file for installing requirements on OpenIndiana.

Run the `install_python` task first, then `install_boto`.

"""

from fabric.api import cd, env, path, run, sudo

env.sudo_prefix = 'pfexec '

DIR_CPY = 'Python-2.7.8'
TAR_CPY = '{}.tar.xz'.format(DIR_CPY)
URL_CPY = 'https://www.python.org/ftp/python/2.7.8/{}'.format(TAR_CPY)
URL_PIP = 'https://bootstrap.pypa.io/get-pip.py'


def install_python():
    """Install the developer tools packages."""
    # development tools
    tools_pkgs = [
        'developer/illumos-gcc',
        'developer/gnu-binutils',
        'system/header',
        'system/library/math/header-math',
        'developer/library/lint',
        'compatibility/ucb',
        'compress/xz'
    ]
    _pkg_install(tools_pkgs)
    sudo('crle -u -l /opt/gcc/4.4.4/lib')
    # Python 2.7.x
    run('wget -q {}'.format(URL_CPY))
    run('tar Jxf {}'.format(TAR_CPY))
    with cd(DIR_CPY), path('/opt/gcc/4.4.4/bin'):
        run('./configure')
        run('make')
        sudo('make install')
    run('rm -rf {}*'.format(DIR_CPY))


def install_boto():
    """Install Amazon Web Service API (boto)."""
    sudo('wget -q --no-check-certificate {}'.format(URL_PIP))
    with path('/usr/local/bin'):
        sudo('python2.7 get-pip.py')
        sudo('pip2.7 install boto')


def _pkg_install(pkg):
    """Install the named package or list of packages.

    :type pkg: str|list
    :param pkg: name(s) of package(s) to install

    """
    if isinstance(pkg, list):
        pkg = ' '.join(pkg)
    sudo("pkg install -q {}".format(pkg))
