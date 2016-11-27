# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is an Erlang/OTP application to facilitate automatically creating ZFS snapshots and uploading them to [Google Cloud Storage](https://cloud.google.com/storage/) as a means of off-site backup. The intent is for the upload to occur during "off-peak" hours to avoid competing with high-demand Internet services. The storage class used is hard-coded to "nearline", the most affordable storage offered by Google.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

## Building and Testing

To build and test the application, use `rebar3` as follows:

```shell
$ rebar3 compile
$ rebar3 ct
```

Running the live tests, which generate sample data and upload it to Google Cloud Storage:

```
$ AKASHITA_LIVE_TEST=1 GCS_REGION='us-west1' GCP_CREDENTIALS=~/.gcloud/mycreds.json rebar3 ct
```

The `AKASHITA_LIVE_TEST` environment variable signals the test suite to include the live tests in addition to the usual suite, and the other two settings are for connecting to Google Cloud Storage. Set the `GCS_REGION` value to the region that best suits your location, and `GCP_CREDENTIALS` is the path to your service account credentials, as described in the `docs/Google_Cloud.md` file.

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

## Amazon Glacier

At an earlier time, this project was coded to upload files to Amazon Glacier. It was decided that Google Cloud Storage was a better option, primarily due to the responsiveness and affordability of the "nearline" storage class. The last commit in which this project supported Glacier is `4711edb7af7f9f8bd8a065c78abf8f12ded1c29f`.
