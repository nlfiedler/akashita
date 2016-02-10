#
# Makefile provided for convenience purposes.
#
.PHONY: all prepare aws clean compile test

all: compile

prepare:
	rebar get-deps
	cd deps/lager && $(MAKE)
	rebar prepare-deps

aws:
	go get -u github.com/aws/aws-sdk-go/...

compile:
	@(test -d deps || $(MAKE) prepare)
	@(test -f $(GOPATH)/bin/api-info || $(MAKE) aws)
	rebar compile escriptize

clean:
	rebar clean

test:
	rebar ct
