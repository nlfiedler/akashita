#!/usr/bin/env python2.7
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
"""Unit tests for backup module."""

import ConfigParser
import time
import unittest

import backup


class TestBackup(unittest.TestCase):

    """Tests the backup module."""

    def test_is_go_time(self):
        """Test the is_go_time function."""
        parser = ConfigParser.ConfigParser()
        parser.add_section('schedule')

        parser.set('schedule', 'go_time', '10:05-12:30')
        ttime = time.localtime(time.mktime((2014, 8, 12, 9, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 9, 59, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 10, 0, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 12, 45, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))

        ttime = time.localtime(time.mktime((2014, 8, 12, 10, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 11, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 12, 0, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 12, 29, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))

        parser.set('schedule', 'go_time', '10:00-12:00,16:00-23:00')
        ttime = time.localtime(time.mktime((2014, 8, 12, 9, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 12, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 23, 01, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 23, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))

        ttime = time.localtime(time.mktime((2014, 8, 12, 10, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 10, 30, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 11, 45, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 16, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))

        parser.set('schedule', 'go_time', '22:00-06:00,10:00-16:00')
        ttime = time.localtime(time.mktime((2014, 8, 12, 9, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 9, 45, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 16, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 21, 45, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))

        ttime = time.localtime(time.mktime((2014, 8, 12, 10, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 22, 00, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 23, 45, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 04, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 12, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))

        parser.set('schedule', 'go_time', '05:00-13:00,17:00-00:00')
        ttime = time.localtime(time.mktime((2014, 8, 12, 4, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 4, 45, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 13, 15, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 16, 45, 36, 1, 224, -1)))
        self.assertFalse(backup.is_go_time(parser, ttime))

        ttime = time.localtime(time.mktime((2014, 8, 12, 5, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 17, 00, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 23, 45, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 11, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))
        ttime = time.localtime(time.mktime((2014, 8, 12, 18, 15, 36, 1, 224, -1)))
        self.assertTrue(backup.is_go_time(parser, ttime))


if __name__ == "__main__":
    unittest.main()
