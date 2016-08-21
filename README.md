# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is an Erlang/Go hybrid application to facilitate automatically creating ZFS snapshots and uploading them to [Google Cloud Storage](https://cloud.google.com/storage/) as a means of off-site backup. The intent is for the upload to occur during "off-peak" hours to avoid competing with high-demand Internet services. The storage class used is hard-coded to "nearline", the most affordable storage offered by Google.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [Go](https://golang.org) 1.5 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

## Building and Testing

To download the dependencies and build the application, use `rebar3` as follows:

```shell
$ go get github.com/nlfiedler/akashita
$ cd $GOPATH/src/github.com/nlfiedler/akashita
$ rebar3 ct
```

### Tested Systems

* FreeBSD 10.x
* Mac OS X 10.x
* Ubuntu Linux 14.04

### Vagrant VMs

There are virtual machine definitions, managed using [Vagrant](https://www.vagrantup.com) and provisioned using [Fabric](http://www.fabfile.org), that are available for building and testing on [FreeBSD](https://www.freebsd.org) and [Ubuntu](http://www.ubuntu.com). These are found in the `vagrant` directory.

## Usage

### Deploying

1. Write a configuration file, named `user_env.config`, at the base of the source tree.
    * See `example.config` in the `docs` directory.
1. Build the release: `rebar3 release`
1. Copy the contents of `_rel` to the desired installation location (e.g. `/opt`).
1. Start it up, likely using `sudo`.
1. Occasionally check the log files in `/opt/akashita/log`.

For example:

```shell
$ cp ~/akashita.config user_env.config
$ rebar3 release
$ sudo mkdir -p /opt
$ sudo cp -R _rel/akashita /opt
$ sudo /opt/akashita/bin/akashita -detached
```

### BSD daemon

See the `config/akashita.rc` file for an example of managing the akashita application as a daemon via `rc.d` in BSD systems (in particular FreeBSD, and likely NetBSD as well). You will need to build and deploy the application as described above, and then use the `service` command to start it, as illustrated in `akashita.rc`.

### Triggering Backup

The first time the application is started it will begin backing up as soon as it reaches the appropriate `go_time`, as defined in the configuration. Once the backup is completed (which may take several weeks depending on the amount of data being uploaded, and the amount of upload time permitted each day), the application will go to sleep. To kick off another backup, connect to the node and send a message to the server process, like so:

```
$ erl -noshell -sname backup@localhost -eval "rpc:call(akashita@localhost, gen_server, call, [akashita_backup, begin_backup]), init:stop()."
```
