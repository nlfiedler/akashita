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
"""Common functions for Akashita scripts."""

import ConfigParser
import logging
import os
import sys

DEFAULT_LOG_FORMAT = '[%(process)d] <%(asctime)s> (%(name)s) {%(levelname)s} %(message)s'
DEFAULT_LOG_FILE = 'akashita.log'


def load_configuration(log):
    """Find and load the configuration file.

    :param log: instance of logging.Logger.

    """
    # no interpolation, since logging record format includes %
    config = ConfigParser.RawConfigParser()
    fname = os.path.expanduser('~/.akashita')
    if os.path.exists(fname):
        log.debug('_load_configuration() reading {}'.format(fname))
        config.read(fname)
    elif os.path.exists('/usr/local/etc/akashita.conf'):
        log.debug('_load_configuration() reading /usr/local/etc/akashita.conf')
        config.read('/usr/local/etc/akashita.conf')
    elif os.path.exists('/etc/akashita.conf'):
        log.debug('_load_configuration() reading /etc/akashita.conf')
        config.read('/etc/akashita.conf')
    else:
        log.error("Could not find configuration file!")
        sys.stderr.write("Could not find configuration file!\n")
        sys.exit(1)
    return config


def configure_logging(log, config):
    """Configure the logger for our purposes.

    :param log: instance of logging.Logger.
    :param config: ConfigParser instance with akashita configuration.

    """
    log.setLevel(logging.DEBUG)
    if config.has_option('logging', 'file'):
        log_file = config.get('logging', 'file')
    else:
        log_file = DEFAULT_LOG_FILE
    handler = logging.FileHandler(log_file)
    handler.setLevel(logging.DEBUG)
    if config.has_option('logging', 'format'):
        log_format = config.get('logging', 'format')
    else:
        log_format = DEFAULT_LOG_FORMAT
    formatter = logging.Formatter(log_format)
    handler.setFormatter(formatter)
    log.addHandler(handler)
