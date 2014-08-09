# -*- coding: utf-8 -*-
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
    config = ConfigParser.ConfigParser()
    fname = os.path.expanduser('~/.akashita')
    if os.path.exists(fname):
        log.debug('_load_configuration() reading {}'.format(fname))
        config.read(fname)
    elif os.path.exists('/etc/akashita'):
        log.debug('_load_configuration() reading /etc/akashita')
        config.read('/etc/akashita')
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
