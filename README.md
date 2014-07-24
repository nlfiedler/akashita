# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is a project to facilitate automatically creating ZFS snapshots and uploading them to Amazon Glacier. The intent is to perform the upload during "off" hours to avoid competing with video streaming services at home.

## TODO

* Collect requirements using the wiki
* Learn Erlang and OTP well enough to get started

## Implementation Notes

* Use [gleber/erlcloud](https://github.com/gleber/erlcloud) library to access Amazon Glacier
    * Probably need to write Glacier support, seems easy enough
* Create new vault, upload everything
* Delete vaults older than three months
* Use 'heart' program to keep Erlang node running
