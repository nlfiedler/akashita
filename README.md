# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is an Erlang/Go hybrid application to facilitate automatically creating ZFS snapshots and uploading them to [Amazon Glacier](https://aws.amazon.com/glacier/). The intent is for the upload to occur during "off-peak" hours to avoid competing with high-demand Internet services.

## TODO

* Finish converting the Python-based prototype to Erlang/Go hybrid.
* Make sure lager is eventually configured during startup.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [Go](https://golang.org) 1.5 or higher
* [rebar](https://github.com/rebar/rebar/)
* _Temporarily_: [Python](http://python.org) 2.7 and 3
* _Temporarily_: [boto](https://aws.amazon.com/sdk-for-python/) and boto3

## Building and Testing

To download the dependencies and build the application, use `make` as follows:

```
$ make
$ make test
```

## Usage

The current Python 2.7 implementation is typically run as a background process, initiated using the `at` command. The advantage of using `at` is that it will send an email when the process terminates, which can be helpful to know if the process ended prematurely.

```
$ echo '/usr/local/bin/backup.py' > job
$ sudo at -m -f job now
```
