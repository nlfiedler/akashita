# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is an Erlang/Go hybrid application to facilitate automatically creating ZFS snapshots and uploading them to [Amazon Glacier](https://aws.amazon.com/glacier/). The intent is for the upload to occur during "off-peak" hours to avoid competing with high-demand Internet services.

## TODO

* Finish converting the Python-based prototype to Erlang/Go hybrid.
* The `klutlan` binary needs to be copied to the `priv` directory during build.
* Set up application configuration, most likely via `sys.config`.
* Make sure lager is eventually configured during startup.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [Go](https://golang.org) 1.5 or higher
* [rebar](https://github.com/rebar/rebar/)
* _Temporarily_: [Python](http://python.org) 2.7
* _Temporarily_: [boto](https://aws.amazon.com/sdk-for-python/)

## Building and Testing

To download the dependencies and build the application, use `rebar` as follows:

```
$ rebar compile
$ rebar ct
```

## Usage

### Python

The current Python 2.7 implementation is typically run as a background process, initiated using the `at` command. The advantage of using `at` is that it will send an email when the process terminates, which can be helpful to know if the process ended prematurely.

```
$ echo '/usr/local/bin/backup.py' > job
$ sudo at -m -f job now
```

### Erlang/Go

The Amazon Web Services credentials and region will need to be configured, as described at [AWS SDK for Go](https://aws.amazon.com/sdk-for-go/), under **Getting Started**. This is typically done by putting the credentials in `~/.aws/credentials` and setting the region in the `AWS_REGION` environment variable.
