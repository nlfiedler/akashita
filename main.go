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

// createBucket creates the named bucket with the given project.
func createBucket(location, project, bucket string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		log.Fatalln(err)
	}
	handle := client.Bucket(bucket)
	attrs := &storage.BucketAttrs{
		Name:         bucket,
		Location:     location,
		StorageClass: "NEARLINE",
	}
	err = handle.Create(ctx, project, attrs)
	if err != nil {
		log.Fatalln(err)
	}
}

// listBuckets prints the names of the buckets associated with the given project
// identifier.
func listBuckets(project string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		log.Fatalln(err)
	}
	iter := client.Buckets(ctx, project)
	for {
		attrs, err := iter.Next()
		if err == storage.Done {
			break
		} else if err != nil {
			log.Fatalln(err)
		}
		fmt.Println(attrs.Name)
	}
}

// listObjects prints the names of the objects in the named bucket.
func listObjects(bucket string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		log.Fatalln(err)
	}
	iter := client.Bucket(bucket).Objects(ctx, nil)
	for {
		attrs, err := iter.Next()
		if err == storage.Done {
			break
		} else if err != nil {
			log.Fatalln(err)
		}
		fmt.Println(attrs.Name)
	}
}

// uploadObject uploads the named file to the given bucket.
func uploadObject(bucket, filename string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		log.Fatalln(err)
	}
	object := path.Base(filename)
	wc := client.Bucket(bucket).Object(object).NewWriter(ctx)
	defer wc.Close()
	infile, err := os.Open(filename)
	if err != nil {
		log.Fatalln(err.Error())
	}
	_, err = io.Copy(wc, infile)
	if err != nil {
		log.Fatalln(err)
	}
}

// fetchObject retrieves the named object from the given bucket. It prints the
// name of the temporary file containing the object data. Exit code 1 if
// anything goes wrong, logging an error message to stderr.
func fetchObject(bucket, object string) {
	ctx := context.Background()
	client, err := storage.NewClient(ctx)
	if err != nil {
		log.Fatalln(err)
	}
	rc, err := client.Bucket(bucket).Object(object).NewReader(ctx)
	if err != nil {
		log.Fatalln(err)
	}
	defer rc.Close()
	tmpfile, err := ioutil.TempFile("", object+"-")
	if err != nil {
		log.Fatalln(err)
	}
	_, err = io.Copy(tmpfile, rc)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println(tmpfile.Name())
}

func main() {
	var help = flag.Bool("help", false, "show usage information")
	var buckets = flag.Bool("buckets", false, "list all buckets")
	var objects = flag.Bool("objects", false, "list all objects in a bucket")
	var create = flag.Bool("create", false, "create the named bucket")
	var fetch = flag.String("fetch", "", "object: retrieve the named object to a file")
	// var empty = flag.String("empty", "", "delete all objects in a bucket")
	// var remove = flag.String("delete", "", "delete the named (empty) bucket")
	var upload = flag.String("upload", "", "file: upload the named file to a bucket")
	var bucket = flag.String("bucket", "", "bucket name, required for some commands")
	var project = flag.String("project", "", "project ID, required for some commands")
	var location = flag.String("location", "", "location name, required for some commands")
	flag.Parse()

	if *help {
		flag.PrintDefaults()
	} else if *buckets {
		if *project == "" {
			log.Fatalln("missing required -project argument")
		}
		listBuckets(*project)
	} else if *objects {
		if *bucket == "" {
			log.Fatalln("missing required -bucket argument")
		}
		listObjects(*bucket)
	} else if *create {
		if *bucket == "" {
			log.Fatalln("missing required -bucket argument")
		}
		if *location == "" {
			log.Fatalln("missing required -location argument")
		}
		if *project == "" {
			log.Fatalln("missing required -project argument")
		}
		createBucket(*location, *project, *bucket)
	} else if *fetch != "" {
		if *bucket == "" {
			log.Fatalln("missing required -bucket argument")
		}
		fetchObject(*bucket, *fetch)
	} else if *upload != "" {
		if *bucket == "" {
			log.Fatalln("missing required -bucket argument")
		}
		uploadObject(*bucket, *upload)
		// } else if *empty != "" {
		// 	if *bucket == "" {
		// 		fmt.Println("missing required -bucket argument")
		// 		os.Exit(2)
		// 	}
		// 	emptybucket(*empty, *bucket)
		// } else if *remove != "" {
		// 	if *bucket == "" {
		// 		fmt.Println("missing required -bucket argument")
		// 		os.Exit(2)
		// 	}
		// 	deletebucket(*remove, *bucket)
	} else {
		// When all else fails, print the help message.
		flag.PrintDefaults()
	}
}
