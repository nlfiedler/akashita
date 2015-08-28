# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is a project to facilitate automatically creating ZFS snapshots and uploading them to Amazon Glacier. The intent is for the upload to occur during "off" hours to avoid competing with high-demand Internet services.

## TODO

* Collect requirements using the wiki
* Convert the Python-based prototype to Erlang/OTP
* Consider switching to Google Cloud Storage
    - https://cloud.google.com/storage-nearline/
    - https://cloud.google.com/storage/docs/json_api/

## Implementation Notes

* Use [gleber/erlcloud](https://github.com/gleber/erlcloud) to access Amazon Web Services
    * Currently missing Glacier API
    * Currently missing SNS API
* Use 'heart' program to keep Erlang node running
