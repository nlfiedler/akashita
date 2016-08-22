# Google Cloud

## Source Code

* https://code.googlesource.com/gocloud

## Storage Locations

* Multi-region: "us"
    - Better availability geographically
* Regional: "us-west1"
    - Better performance within that region

## Initial Setup

1. Visit https://console.cloud.google.com/ and create a new project.
1. Note the "Project ID" and set that in the application configuration file (`gcp_project`).
    * The application configuration file is described in the `README`.
1. Create an application default credentials file, as described below; save the file somewhere safe, and set the path in the application configuration (`gcp_credentials`).

## Authentication

### Setup

* https://developers.google.com/identity/protocols/application-default-credentials
* https://cloud.google.com/docs/authentication
* https://cloud.google.com/storage/docs/authentication

1. Visit https://console.developers.google.com/project/_/apis/credentials
1. From the project drop-down, select your project.
1. On the Credentials page, select the Create credentials drop-down, then select Service account key.
1. From the Service account drop-down, select an existing service account or create a new one.
1. For Key type, select the JSON key option, then select Create. The file automatically downloads to your computer.
1. Put the *.json file you just downloaded in a directory of your choosing. This directory must be private (you can't let anyone get access to this), but accessible to your web server code.
1. Set the environment variable `GOOGLE_APPLICATION_CREDENTIALS` to the path of the JSON file downloaded.

### Debugging

Files within `.config/gcloud/`, as well as the `.gsutil/credstore` file, inform the client library as to how to connect to the Google Cloud Platform. Move these files out of the way when attempting to use the `GOOGLE_APPLICATION_CREDENTIALS` environment variable.

## Client Library

### Installation for Go

Somehow neither one of the packages below is sufficient, both of them are needed. The `rebar.config`
file will automatically install these as needed.

```
$ go get google.golang.org/cloud
$ go get cloud.google.com/go
```

### Documentation

* `godoc -http=:6060`
* https://godoc.org/cloud.google.com/go
