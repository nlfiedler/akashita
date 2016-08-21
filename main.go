//
// Copyright (c) 2016 Nathan Fiedler
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License. You may obtain
// a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.
//
package main

import (
	"cloud.google.com/go/storage"
	"flag"
	"fmt"
	"golang.org/x/net/context"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path"
)

const (
	// Why is this not already defined in go/storage?
	Nearline = "NEARLINE"
)

// fatal prints a message to standard error and exits with status code 1.
func fatal(v ...interface{}) {
	_, err := fmt.Fprintln(os.Stderr, v)
	if err != nil {
		log.Fatalln("error while printing to stderr: %v", err)
		fmt.Println(v)
	}
	os.Exit(1)
}

// createBucket creates the named bucket within the given project. If the bucket
// already exists, nothing is done.
func createBucket(location, project, bucket string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		fatal(err)
	}
	// check if the bucket already exists
	iter := client.Buckets(ctx, project)
	for {
		attrs, err := iter.Next()
		if err == storage.Done {
			break
		} else if err != nil {
			fatal(err)
		}
		if attrs.Name == bucket {
			// already exists, nothing to do
			return
		}
	}
	// bucket apparently does not exist, create it
	bucket_handle := client.Bucket(bucket)
	attrs := &storage.BucketAttrs{
		Name:         bucket,
		Location:     location,
		StorageClass: Nearline,
	}
	err = bucket_handle.Create(ctx, project, attrs)
	if err != nil {
		fatal(err)
	}
}

// listBuckets prints the names of the buckets associated with the given project
// identifier.
func listBuckets(project string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		fatal(err)
	}
	iter := client.Buckets(ctx, project)
	for {
		attrs, err := iter.Next()
		if err == storage.Done {
			break
		} else if err != nil {
			fatal(err)
		}
		fmt.Println(attrs.Name)
	}
}

// listObjects prints the names of the objects in the named bucket.
func listObjects(bucket string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		fatal(err)
	}
	iter := client.Bucket(bucket).Objects(ctx, nil)
	for {
		attrs, err := iter.Next()
		if err == storage.Done {
			break
		} else if err != nil {
			fatal(err)
		}
		fmt.Println(attrs.Name)
	}
}

// uploadObject uploads the named file to the given bucket.
func uploadObject(bucket, filename string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		fatal(err)
	}
	object := path.Base(filename)
	wc := client.Bucket(bucket).Object(object).NewWriter(ctx)
	defer wc.Close()
	infile, err := os.Open(filename)
	if err != nil {
		fatal(err.Error())
	}
	_, err = io.Copy(wc, infile)
	if err != nil {
		fatal(err)
	}
}

// fetchObject retrieves the named object from the given bucket. It prints the
// name of the temporary file containing the object data.
func fetchObject(bucket, object string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		fatal(err)
	}
	rc, err := client.Bucket(bucket).Object(object).NewReader(ctx)
	if err != nil {
		fatal(err)
	}
	defer rc.Close()
	tmpfile, err := ioutil.TempFile("", object+"-")
	if err != nil {
		fatal(err)
	}
	_, err = io.Copy(tmpfile, rc)
	if err != nil {
		fatal(err)
	}
	fmt.Println(tmpfile.Name())
}

// deleteBucket removes all of the objects in the named bucket, then removes the
// bucket itself.
func deleteBucket(bucket string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		fatal(err)
	}
	bucket_handle := client.Bucket(bucket)
	iter := bucket_handle.Objects(ctx, nil)
	for {
		attrs, err := iter.Next()
		if err == storage.Done {
			break
		} else if err != nil {
			fatal(err)
		}
		if err := bucket_handle.Object(attrs.Name).Delete(ctx); err != nil {
			fatal(err)
		}
		fmt.Printf("removed object %v", attrs.Name)
	}
	if err := bucket_handle.Delete(ctx); err != nil {
		fatal(err)
	}
	fmt.Printf("removed bucket %v", bucket)
}

// main processes the given command line arguments and operates on buckets and
// objects within Google Cloud Storage. Any errors are printed to standard error
// and the program exits with a non-zero exit code (usually 1 for errors
// resulting from cloud operations, 2 for user errors).
func main() {
	var help = flag.Bool("help", false, "show usage information")
	var buckets = flag.Bool("buckets", false, "list all buckets")
	var objects = flag.Bool("objects", false, "list all objects in a bucket")
	var create = flag.Bool("create", false, "create the named bucket")
	var remove = flag.Bool("delete", false, "delete a bucket and all of its objects")
	var fetch = flag.String("fetch", "", "object: retrieve the named object to a file")
	var upload = flag.String("upload", "", "file: upload the named file to a bucket")
	var bucket = flag.String("bucket", "", "bucket name, required for some commands")
	var project = flag.String("project", "", "project ID, required for some commands")
	var location = flag.String("location", "", "location name, required for some commands")
	flag.Parse()

	if *help {
		flag.PrintDefaults()
	} else if *buckets {
		if *project == "" {
			fatal("missing required -project argument")
		}
		listBuckets(*project)
	} else if *objects {
		if *bucket == "" {
			fatal("missing required -bucket argument")
		}
		listObjects(*bucket)
	} else if *create {
		if *bucket == "" {
			fatal("missing required -bucket argument")
		}
		if *location == "" {
			fatal("missing required -location argument")
		}
		if *project == "" {
			fatal("missing required -project argument")
		}
		createBucket(*location, *project, *bucket)
	} else if *fetch != "" {
		if *bucket == "" {
			fatal("missing required -bucket argument")
		}
		fetchObject(*bucket, *fetch)
	} else if *upload != "" {
		if *bucket == "" {
			fatal("missing required -bucket argument")
		}
		uploadObject(*bucket, *upload)
	} else if *remove {
		if *bucket == "" {
			fatal("missing required -bucket argument")
		}
		deleteBucket(*bucket)
	} else {
		// When all else fails, print the help message.
		flag.PrintDefaults()
	}
}
