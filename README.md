# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is a project to facilitate automatically creating ZFS snapshots and uploading them to [Amazon Glacier](https://aws.amazon.com/glacier/). The intent is for the upload to occur during "off-peak" hours to avoid competing with high-demand Internet services.

## TODO

* Collect requirements using the wiki
* Convert the Python-based prototype to [Erlang/OTP](http://www.erlang.org)
    - Need an Erlang wrapper around the Amazon Glacier API
* Consider switching to Google Cloud Storage
    - https://cloud.google.com/storage-nearline/
    - https://cloud.google.com/storage/docs/json_api/

## Implementation Notes

* Use [gleber/erlcloud](https://github.com/gleber/erlcloud) to access Amazon Web Services
    - Currently missing Glacier API
    - Currently missing SNS API
* Use 'heart' program to keep Erlang node running

## Usage

The current Python implementation is typically run as a background process, initiated using the `at` command. The advantage of using `at` is that it will send an email when the process terminates, which can be helpful to know if the process ended prematurely.

```
$ echo '/usr/local/bin/backup.py' > job
$ sudo at -m -f job now
```
