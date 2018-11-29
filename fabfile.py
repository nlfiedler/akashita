# -*- coding: utf-8 -*-
"""Fabric file to provision Ubuntu test environment."""

import os
import subprocess

from fabric.api import env, execute, put, run, sudo, task
from fabric.contrib.files import append, exists

env.hosts = ["default"]
env.use_ssh_config = True
env.ssh_config_path = "ssh_config"

# Generate the ssh_config file if it is missing.
if not os.path.exists('ssh_config'):
    config = subprocess.check_output(['vagrant', 'ssh-config'], universal_newlines=True)
    pwd = os.getcwd() + '/'
    with open('ssh_config', 'w') as fobj:
        fobj.write(config.replace(pwd, ''))


@task
def provision():
    """Provision the system for testing."""
    sudo('locale-gen en_US.UTF-8')
    sudo('apt-get -q update')
    sudo('apt-get -q -y install build-essential')
    sudo('apt-get -q -y install zfsutils-linux')
    sudo('apt-get -q -y install erlang')
    run('wget -q https://github.com/erlang/rebar3/releases/download/3.7.4/rebar3')
    run('chmod +x rebar3')
    sudo('mv rebar3 /usr/local/bin')
