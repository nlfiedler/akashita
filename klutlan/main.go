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
	"flag"
	"fmt"
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/glacier"
	"os"
)

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

// printJobStatus displays the details for a particular job.
func printJobStatus(jobId string, vault string) {
	svc := glacier.New(session.New())
	params := &glacier.DescribeJobInput{
		AccountId: aws.String("-"),
		VaultName: &vault,
		JobId:     &jobId,
	}
	req, resp := svc.DescribeJobRequest(params)
	err := req.Send()
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	fmt.Println(resp)
}

// main parses the command line arguments and delegates to the appropriate
// function.
func main() {
	var help = flag.Bool("help", false, "show usage information")
	var vaults = flag.Bool("vaults", false, "list all known vaults")
	var jobs = flag.Bool("jobs", false, "list all known jobs")
	var status = flag.String("status", "", "query status of a particular job")
	var vault = flag.String("vault", "", "vault name, required for some commands")
	// TODO: request inventory of a vault
	// TODO: print the output of a completed job
	// TODO: rewrite restore.py
	// TODO: rewrite upload.py
	// TODO: rewrite prune.py (need to wait until May to avoid fees)
	flag.Parse()

	if *help {
		flag.PrintDefaults()
		os.Exit(0)
	}

	if *vaults {
		listVaults()
		os.Exit(0)
	}

	if *jobs {
		listAllJobs()
		os.Exit(0)
	}

	if *status != "" {
		if *vault == "" {
			fmt.Println("Missing required -vault argument")
			os.Exit(2)
		}
		printJobStatus(*status, *vault)
	}
}
