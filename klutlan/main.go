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
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/glacier"
	"io"
	"io/ioutil"
	"os"
)

// InventoryRetrievalEntry represents on entry from an inventory retrieval. It
// is unmarshalled from the JSON response to a job. There is one field in this
// struct for every entry in the JSON output.
type InventoryRetrievalEntry struct {
	ArchiveId          string
	ArchiveDescription string
	CreationDate       string
	Size               int64
	SHA256TreeHash     string
}

// InventoryRetrievalJob represents the result of an inventory retrieval job. It
// is unmarshalled from the JSON response to a job. There is one field in this
// struct for every entry in the JSON output.
type InventoryRetrievalJob struct {
	VaultARN      string
	InventoryDate string
	ArchiveList   []InventoryRetrievalEntry
}

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

// deleteVault deletes the named vault. The vault must be empty, as by the
// emptyVault() function. This requires requesting an inventory, emptying the
// vault, then requesting another inventory, and finally deleting the vault
// (after the second inventory job has completed).
func deleteVault(jobId, vault string) {
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
			// make sure the inventory retrieval indicates that the vault
			// contains no archives at all
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
			// unmarshal the json to inventory retrieval objects
			var output bytes.Buffer
			io.Copy(&output, job_output.Body)
			var job_result InventoryRetrievalJob
			err = json.Unmarshal(output.Bytes(), &job_result)
			if err != nil {
				fmt.Println(err.Error())
				os.Exit(1)
			}
			if len(job_result.ArchiveList) == 0 {
				// we can delete the vault with some degree of certainty
				params := &glacier.DeleteVaultInput{
					AccountId: aws.String("-"),
					VaultName: &vault,
				}
				_, err := svc.DeleteVault(params)
				if err != nil {
					fmt.Println(err.Error())
					os.Exit(1)
				}
				fmt.Println(vault)
			} else {
				fmt.Println("vault is not empty")
				os.Exit(2)
			}
		} else {
			fmt.Println("not an inventory retrieval job")
			os.Exit(2)
		}
	} else {
		fmt.Println("job still in progress")
		os.Exit(2)
	}
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

// printInventory displays the final result of the (inventory retrieval) job.
// Exits with non-zero status if the job is not yet complete.
func printInventory(jobId, vault string) {
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
			// format the json for readability
			var output bytes.Buffer
			io.Copy(&output, job_output.Body)
			var out bytes.Buffer
			json.Indent(&out, output.Bytes(), "", "    ")
			out.WriteTo(os.Stdout)
			fmt.Println()
		} else {
			fmt.Println("not an inventory retrieval job")
			os.Exit(2)
		}
	} else {
		fmt.Println("job still in progress")
		os.Exit(2)
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
	svc := glacier.New(session.New())
	params := &glacier.UploadArchiveInput{
		AccountId:          aws.String("-"),
		ArchiveDescription: aws.String(desc),
		VaultName:          &vault,
		Body:               infile,
	}
	resp, err := svc.UploadArchive(params)
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	fmt.Println(*resp.ArchiveId)
}

// emptyVault reads the inventory of the given job and marks all of the archives
// as deleted. The vault can later be deleted after requesting another
// inventory.
func emptyVault(jobId, vault string) {
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
			// unmarshal the json to inventory retrieval objects
			var output bytes.Buffer
			io.Copy(&output, job_output.Body)
			var job_result InventoryRetrievalJob
			err = json.Unmarshal(output.Bytes(), &job_result)
			if err != nil {
				fmt.Println(err.Error())
				os.Exit(1)
			}
			// request that each of the archives be deleted
			for _, arc := range job_result.ArchiveList {
				delete_input := &glacier.DeleteArchiveInput{
					AccountId: aws.String("-"),
					ArchiveId: aws.String(arc.ArchiveId),
					VaultName: &vault,
				}
				_, err := svc.DeleteArchive(delete_input)
				if err != nil {
					fmt.Println(err.Error())
					os.Exit(1)
				}
				fmt.Println(arc.ArchiveDescription)
			}
		} else {
			fmt.Println("not an inventory retrieval job")
			os.Exit(2)
		}
	} else {
		fmt.Println("job still in progress")
		os.Exit(2)
	}
}

// main parses the command line arguments and delegates to the appropriate
// function.
func main() {
	var help = flag.Bool("help", false, "show usage information")
	var vaults = flag.Bool("vaults", false, "list all known vaults")
	var jobs = flag.Bool("jobs", false, "list all known jobs")
	var inventory = flag.Bool("inventory", false, "request inventory of a vault")
	var create = flag.Bool("create", false, "create the named vault")
	var archive = flag.String("archive", "", "archiveId: request retrieval of an archive")
	var status = flag.String("status", "", "jobId: query status of a particular job")
	var output = flag.String("output", "", "jobId: retrieve the output of an inventory job")
	var fetch = flag.String("fetch", "", "jobId: retrieve the requested archive to a file")
	var empty = flag.String("empty", "", "jobId: delete all archives in a vault")
	var remove = flag.String("delete", "", "jobId: delete the named (empty) vault")
	var upload = flag.String("upload", "", "file: upload the named file to a vault")
	var desc = flag.String("desc", "", "description of archive being uploaded")
	var vault = flag.String("vault", "", "vault name, required for some commands")
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
	} else if *create {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		createVault(*vault)
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
		printInventory(*output, *vault)
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
	} else if *empty != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		emptyVault(*empty, *vault)
	} else if *remove != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		deleteVault(*remove, *vault)
	} else {
		// When all else fails, print the help message.
		flag.PrintDefaults()
	}
}
