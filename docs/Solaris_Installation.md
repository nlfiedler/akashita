# Solaris Installation Guide

## Development Tools

We will need these packages for building Git from source.

```
$ pfexec pkg install developer/illumos-gcc
$ pfexec pkg install developer/gnu-binutils
$ pfexec pkg install system/header
$ pfexec pkg install system/library/math/header-math
$ pfexec pkg install developer/library/lint
$ pfexec pkg install compatibility/ucb
$ export PATH=/opt/gcc/4.4.4/bin:$PATH
```

## Git

Get the latest source tarball for Git to avoid the very outdated Git package.

```
$ ./configure
$ make
$ pfexec make install
$ export PATH=/usr/local/bin:$PATH
```

## Python 3.x

```
$ pfexec pkg set-publisher -p http://pkg.openindiana.org/sfe
$ pfexec pkg install runtime/python-32
```

## boto

For boto, we will have to perform the 2-to-3 conversion prior to installation.

```
$ git clone git://github.com/boto/boto.git
$ cd boto
$ git checkout 2.32.1
$ 2to3 -w .
$ python3 setup.py build
$ pfexec python3 setup.py install
```

Now we can test the installation to ensure it basically works.

```
$ python3
>>> import boto
>>> import boto.glacier
>>> boto.glacier.regions()
```
