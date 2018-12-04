# Akashita

[Akashita](http://en.wikipedia.org/wiki/Akashita) is an Erlang/OTP application
to facilitate automatically creating ZFS snapshots and uploading them to [Google
Cloud Storage](https://cloud.google.com/storage/) as a means of off-site backup.
The intent is for the upload to occur during "off-peak" hours to avoid competing
with high-demand Internet services. The storage class used is hard-coded to
"nearline", the most affordable storage offered by Google.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R19 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

## Building and Testing

To build and test the application, use `rebar3` as follows:

```shell
$ rebar3 compile
$ rebar3 ct
```

Running the live tests, which generate sample data and upload it to Google Cloud Storage:

```
$ AKASHITA_LIVE_TEST=1 GCS_REGION='us-west1' GCP_CREDENTIALS=~/.gcloud/credentials.json rebar3 ct
```

The `AKASHITA_LIVE_TEST` environment variable signals the test suite to include
the live tests in addition to the usual suite, and the other two settings are
for connecting to Google Cloud Storage. Set the `GCS_REGION` value to the region
that best suits your location, and `GCP_CREDENTIALS` is the path to your service
account credentials, as described in the **Google Cloud** section below.

### Using Vagrant

Unfortunately, a Docker container does not have sufficient privileges to create
or destroy ZFS pools, but it can do just about everything else. As such, testing
can be done within a VM, but not within a container. The test suite itself is
rather delicate and specific to testing on Ubuntu in a VM. Use
[Vagrant](https://www.vagrantup.com) and [Fabric](http://www.fabfile.org) 1.x to
set up the VM.

```shell
host$ vagrant up
host$ fab provision
host$ cp ~/.gcloud/credentials.json .
host$ vagrant ssh
vagrant$ cd /vagrant
vagrant$ rebar3 clean
vagrant$ rebar3 compile
vagrant$ AKASHITA_LIVE_TEST=1 GCS_REGION='us-west1' GCP_CREDENTIALS=/vagrant/credentials.json rebar3 ct
```

## Deployment

### Docker

The application is easily deployed using [Docker](https://www.docker.com), as
there is a provided `Dockerfile` and `docker-compose.yml` file for building and
running the application in Docker.

1. Put a configuration file, named `user_env.config`, in the `config` directory.
    * See `example.config` in the `config` directory.
1. Craft a "production" Compose file that defines a named volume.
1. Invoke `docker-compose build` to build the container.
1. Deploy to the engine as desired.

## Google Cloud

### Storage Locations

* Multi-region: "us"
    - Better availability geographically
* Regional: "us-west1"
    - Better performance within that region

### Initial Setup

1. Visit https://console.cloud.google.com/ and create a new project.
1. Create an application default credentials file, as described below; save the file somewhere safe, and set the path in the application configuration (`gcp_credentials`).

### Authentication

#### Setup

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

#### Debugging

Files within `.config/gcloud/`, as well as the `.gsutil/credstore` file, inform the client library as to how to connect to the Google Cloud Platform. Move these files out of the way when attempting to use the `GOOGLE_APPLICATION_CREDENTIALS` environment variable.

## Amazon Glacier

At an earlier time, this project was coded to upload files to Amazon Glacier. It was decided that Google Cloud Storage was a better option, primarily due to the responsiveness and affordability of the "nearline" storage class. The last commit in which this project supported Glacier is `4711edb7af7f9f8bd8a065c78abf8f12ded1c29f`.
