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
	"encoding/hex"
	"flag"
	"fmt"
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/glacier"
	"io"
	"io/ioutil"
	"os"
)

// getAllVaults retrieves the list of all vaults.
func getAllVaults(svc *glacier.Glacier) *glacier.ListVaultsOutput {
	params := &glacier.ListVaultsInput{
		AccountId: aws.String("-"),
	}
	resp, err := svc.ListVaults(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	return resp
}

// listVaults displays the details of all vaults.
func listVaults() {
	svc := glacier.New(session.New())
	resp := getAllVaults(svc)
	fmt.Println(resp)
}

// listVaultJobs displays the jobs for the named vault.
func listVaultJobs(svc *glacier.Glacier, vault string) {
	params := &glacier.ListJobsInput{
		AccountId: aws.String("-"),
		VaultName: &vault,
	}
	resp, err := svc.ListJobs(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	if len(resp.JobList) > 0 {
		fmt.Println(resp)
	}
}

// listAllJobs displays all jobs for all vaults.
func listAllJobs() {
	svc := glacier.New(session.New())
	resp := getAllVaults(svc)
	for _, vault := range resp.VaultList {
		listVaultJobs(svc, *vault.VaultName)
	}
}

// createVault creates the named vault, printing the location URI to standard
// output.
func createVault(vault string) {
	svc := glacier.New(session.New())
	params := &glacier.CreateVaultInput{
		AccountId: aws.String("-"),
		VaultName: &vault,
	}
	resp, err := svc.CreateVault(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	fmt.Println(*resp.Location)
}

// requestInventory requests the inventory retrieval for a vault and prints the
// job identifer to standard output.
func requestInventory(vault string) {
	svc := glacier.New(session.New())
	params := &glacier.InitiateJobInput{
		AccountId: aws.String("-"),
		VaultName: &vault,
		JobParameters: &glacier.JobParameters{
			Type: aws.String("inventory-retrieval"),
		},
	}
	resp, err := svc.InitiateJob(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	fmt.Println(*resp.JobId)
}

// requestArchive requests the retrieval of an archive from the named vault.
// Prints the job identifer to standard output.
func requestArchive(archiveId, vault string) {
	svc := glacier.New(session.New())
	params := &glacier.InitiateJobInput{
		AccountId: aws.String("-"),
		VaultName: &vault,
		JobParameters: &glacier.JobParameters{
			ArchiveId: &archiveId,
			Type:      aws.String("archive-retrieval"),
		},
	}
	resp, err := svc.InitiateJob(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	fmt.Println(*resp.JobId)
}

// printJobStatus displays the details for a particular job.
func printJobStatus(jobId, vault string) {
	svc := glacier.New(session.New())
	params := &glacier.DescribeJobInput{
		AccountId: aws.String("-"),
		JobId:     &jobId,
		VaultName: &vault,
	}
	resp, err := svc.DescribeJob(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	fmt.Println(resp)
}

// printJobOutput displays the final result of the (inventory retrieval) job.
// Exits with non-zero status if the job is not yet complete.
func printJobOutput(jobId, vault string) {
	svc := glacier.New(session.New())
	params := &glacier.DescribeJobInput{
		AccountId: aws.String("-"),
		JobId:     &jobId,
		VaultName: &vault,
	}
	job, err := svc.DescribeJob(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	if *job.Completed {
		if *job.Action == glacier.ActionCodeInventoryRetrieval {
			job_input := &glacier.GetJobOutputInput{
				AccountId: aws.String("-"),
				JobId:     &jobId,
				VaultName: &vault,
			}
			job_output, err := svc.GetJobOutput(job_input)
			if err != nil {
				fmt.Println(err.Error())
				os.Exit(1)
			}
			io.Copy(os.Stdout, job_output.Body)
		} else {
			fmt.Println("not an inventory retrieval job")
			os.Exit(2)
		}
	} else {
		fmt.Println("job still in progress")
		os.Exit(1)
	}
}

// fetchArchive retrieves the archive from the archive retrieval job and saves
// it to a temporary file.
func fetchArchive(jobId, vault string) {
	svc := glacier.New(session.New())
	params := &glacier.DescribeJobInput{
		AccountId: aws.String("-"),
		JobId:     &jobId,
		VaultName: &vault,
	}
	job, err := svc.DescribeJob(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	if *job.Completed {
		if *job.Action == glacier.ActionCodeArchiveRetrieval {
			job_input := &glacier.GetJobOutputInput{
				AccountId: aws.String("-"),
				JobId:     &jobId,
				VaultName: &vault,
			}
			job_output, err := svc.GetJobOutput(job_input)
			if err != nil {
				fmt.Println(err.Error())
				os.Exit(1)
			}
			tmpfile, err := ioutil.TempFile("", "archive")
			if err != nil {
				fmt.Println(err.Error())
				os.Exit(1)
			}
			fmt.Println(tmpfile.Name())
			io.Copy(tmpfile, job_output.Body)
		} else {
			fmt.Println("not an archive retrieval job")
			os.Exit(2)
		}
	} else {
		fmt.Println("job still in progress")
		os.Exit(1)
	}
}

// uploadFile uploads the given file to the named vault.
func uploadFile(filename, desc, vault string) {
	infile, err := os.Open(filename)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	hash := glacier.ComputeHashes(infile)
	infile.Seek(0, 0)

	svc := glacier.New(session.New())
	params := &glacier.UploadArchiveInput{
		AccountId:          aws.String("-"),
		ArchiveDescription: aws.String(desc),
		VaultName:          &vault,
		Body:               infile,
		Checksum:           aws.String(hex.EncodeToString(hash.TreeHash)),
	}
	resp, err := svc.UploadArchive(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	fmt.Println(*resp.ArchiveId)
}

// main parses the command line arguments and delegates to the appropriate
// function.
func main() {
	var help = flag.Bool("help", false, "show usage information")
	var vaults = flag.Bool("vaults", false, "list all known vaults")
	var jobs = flag.Bool("jobs", false, "list all known jobs")
	var inventory = flag.Bool("inventory", false, "request inventory of a vault")
	var create = flag.String("create", "", "create the named vault")
	var archive = flag.String("archive", "", "request retrieval of an archive")
	var status = flag.String("status", "", "query status of a particular job")
	var output = flag.String("output", "", "retrieve output of a completed job")
	var fetch = flag.String("fetch", "", "save the retrieved archive to a file")
	var upload = flag.String("upload", "", "upload the named file to a vault")
	var desc = flag.String("desc", "", "description of archive being uploaded")
	var vault = flag.String("vault", "", "vault name, required for some commands")
	// TODO: rewrite prune.py (need to wait until May to avoid fees)
	flag.Parse()

	if *help {
		flag.PrintDefaults()
	} else if *vaults {
		listVaults()
	} else if *jobs {
		listAllJobs()
	} else if *inventory {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		requestInventory(*vault)
	} else if *create != "" {
		createVault(*create)
	} else if *archive != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		requestArchive(*archive, *vault)
	} else if *status != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		printJobStatus(*status, *vault)
	} else if *output != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		printJobOutput(*output, *vault)
	} else if *fetch != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		fetchArchive(*fetch, *vault)
	} else if *upload != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		uploadFile(*upload, *desc, *vault)
	} else {
		// When all else fails, print the help message.
		flag.PrintDefaults()
	}
}
