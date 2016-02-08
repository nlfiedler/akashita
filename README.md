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

## Usage

The current Python implementation is typically run as a background process, initiated using the `at` command. The advantage of using `at` is that it will send an email when the process terminates, which can be helpful to know if the process ended prematurely.

```
$ echo '/usr/local/bin/backup.py' > job
$ sudo at -m -f job now
```

## Development Setup

### Python

Currently some of the Python scripts use Python 2.7 and boto, while others use Python 3.x and the boto3 API. The following instructions are for Python 3 and boto3 only.

#### Mac OS X

1. Install [Homebrew](http://brew.sh)
1. `brew install python3`
1. `pip3 install boto3`

#### FreeBSD

1. `sudo pkg install python3`
1. `wget https://bootstrap.pypa.io/get-pip.py`
1. `sudo python3 get-pip.py`
1. `sudo pip install boto3`

### Go

Some of the Python scripts are being rewritten using Go. Hence you will need Go 1.5 or higher to be installed, along with the AWS for Go SDK. This is easily done using the following `go get` command:

```
$ go get -u github.com/aws/aws-sdk-go/...
```

To build the Go code, use `go install` and then run the generated `klutlan` command, like so:

```
$ go install github.com/nlfiedler/akashita/klutlan
$ $GOPATH/bin/klutlan -vaults
```
