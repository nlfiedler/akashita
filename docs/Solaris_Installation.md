# Solaris Installation Guide

Running boto in Python 3.2 on OpenIndiana seems to fail badly, probably because it is not really converted to Python3 semantics properly (for one thing, it seems to incorrectly mix strings and byte buffers). As such, we need to build and install Python 2.7.

## Development Tools

We will need these packages for building Python from source.

```
$ pfexec pkg install developer/illumos-gcc
$ pfexec pkg install developer/gnu-binutils
$ pfexec pkg install system/header
$ pfexec pkg install system/library/math/header-math
$ pfexec pkg install developer/library/lint
$ pfexec pkg install compatibility/ucb
```

Then update the paths so the new bits can be found.

```
$ export PATH=/opt/gcc/4.4.4/bin:$PATH
$ pfexec crle -u -l /opt/gcc/4.4.4/lib
```

## Python 2.7

Get the latest Python 2.7 source tarball and build like so.

```
$ ./configure
$ make
$ pfexec make install
```

Then update your path so the new Python can be found.

```
$ export PATH=/usr/local/bin:$PATH
```

## pip

Using [pip](http://pip-installer.org) makes it easy to install Python packages.

```
$ wget --no-check-certificate https://bootstrap.pypa.io/get-pip.py
$ pfexec python2.7 get-pip.py
```

## boto

And now installing [boto](http://github.com/boto/boto) is easy with pip.

```
$ pfexec pip2.7 install boto
```

Now we can test the installation to ensure it basically works.

```
$ python2.7
>>> import boto.glacier
>>> boto.glacier.regions()
```

## Hack httplib

Seems that Python 2.7 has issues in httplib when sending a mix of Unicode and binary data, which occurs often with web service APIs. See bug [11898](http://bugs.python.org/issue11898) for details. Apply the patch attached to the bug report to hack around the apparently complex issue.
