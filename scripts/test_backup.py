#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
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


if __name__ == "__main__":
    unittest.main()
