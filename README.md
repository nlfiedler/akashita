# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is an Erlang/Go hybrid application to facilitate automatically creating ZFS snapshots and uploading them to [Amazon Glacier](https://aws.amazon.com/glacier/). The intent is for the upload to occur during "off-peak" hours to avoid competing with high-demand Internet services.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [Go](https://golang.org) 1.5 or higher
* [rebar](https://github.com/rebar/rebar/) for compiling and testing
* [relx](https://github.com/erlware/relx) for building a release

## Building and Testing

To download the dependencies and build the application, use `rebar` as follows:

```
$ go get github.com/nlfiedler/akashita
$ cd $GOPATH/src/github.com/nlfiedler/akashita
$ rebar compile
$ rebar ct
```

### Vagrant VMs

There are virtual machine definitions, managed using [Vagrant](https://www.vagrantup.com) and provisioned using [Fabric](http://www.fabfile.org), that are available for building and testing on [FreeBSD](https://www.freebsd.org) and [Ubuntu](http://www.ubuntu.com). These are found in the `vagrant` directory. The Linux VM is only good for building and testing manually, as the automated tests make use of `mkfile` to produce temporary ZFS datasets.

## Usage

### Running

The process is typically run as a background process, initiated using the `at` command. The advantage of using `at` is that it will send an email when the process terminates, which can be helpful to know if the process ended prematurely. The advantage to using `sudo` is that the process will have permission to create and destroy ZFS datasets, which is part of the normal backup procedure.

```
$ echo '/usr/local/bin/akashita' > job
$ sudo at -m -f job now
```

### Configuration

The Amazon Web Services credentials and region will need to be configured, as described at [AWS SDK for Go](https://aws.amazon.com/sdk-for-go/), under **Getting Started**. This is typically done by putting the credentials in `~/.aws/credentials` and setting the region in the `AWS_REGION` environment variable.
