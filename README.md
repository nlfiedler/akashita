# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is a project to facilitate automatically creating ZFS snapshots and uploading them to [Amazon Glacier](https://aws.amazon.com/glacier/). The intent is for the upload to occur during "off-peak" hours to avoid competing with high-demand Internet services.

## TODO

* Convert the Python-based prototype to something else
    - Current Python 2.x solution requires patching Python due to HTTP bug
    - Python is not robust or fault tolerant, unlike Erlang/OTP
    - [Erlang](http://www.erlang.org)/[Go](https://golang.org)
        + https://github.com/aws/aws-sdk-go
        + Go for the actual Glacier operations
        + Erlang for the rest (processing vaults, archives)
    - [Erlang](http://www.erlang.org)/[Python](https://www.python.org) hybrid
        + Use Python 3.x and [boto3](https://github.com/boto/boto3) API
        + Use a short Python script to upload each part
        + The rest (processing vaults, archives) would be done in Erlang
    - [Erlang](http://www.erlang.org)
        + https://github.com/jkakar/aws-erlang (missing Glacier)
        + https://github.com/gleber/erlcloud (missing Glacier)
    - [Go](https://golang.org)
        + https://github.com/aws/aws-sdk-go
        + No more robust than Python
    - [Rust](https://www.rust-lang.org)
        + https://github.com/rusoto/rusoto
        + No more robust than Python
        + Missing Glacier API

## Usage

The current Python implementation is typically run as a background process, initiated using the `at` command. The advantage of using `at` is that it will send an email when the process terminates, which can be helpful to know if the process ended prematurely.

```
$ echo '/usr/local/bin/backup.py' > job
$ sudo at -m -f job now
```

## Development Setup

Currently some of the Python scripts use Python 2.7 and boto, while others use Python 3.x and the boto3 API. The following instructions are for Python 3 and boto3 only.

### Mac OS X

1. Install [Homebrew](http://brew.sh)
1. `brew install python3`
1. `pip3 install boto3`

### FreeBSD

1. `sudo pkg install python3`
1. `wget https://bootstrap.pypa.io/get-pip.py`
1. `sudo python3 get-pip.py`
1. `sudo pip install boto3`
