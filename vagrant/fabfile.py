# -*- coding: utf-8 -*-
"""Fabric file for installing requirements on OpenIndiana."""

from fabric.api import env, sudo

env.sudo_prefix = 'pfexec '

# TOOD: isntall compress/xz so we can fetch the smallest source tarball
# TODO: install Python 2.7
# TODO: install pip using python2.7
# TODO: install boot using pip2.7


def install_tools():
    """Install the developer tools packages."""
    _pkg_install('developer/illumos-gcc')
    _pkg_install('developer/gnu-binutils')
    _pkg_install('system/header')
    _pkg_install('system/library/math/header-math')
    _pkg_install('developer/library/lint')
    _pkg_install('compatibility/ucb')
    # TODO: how to change the PATH? only really need it to build Python
    # $ export PATH=/opt/gcc/4.4.4/bin:$PATH


def add_gcc_lib():
    """Add the gcc lib to the crle path."""
    sudo('crle -u -l /opt/gcc/4.4.4/lib')


def _pkg_install(pkg):
    """Install the named package.

    :type pkg: str
    :param pkg: name of package to install

    """
    sudo("pkg install -q {}".format(pkg))
