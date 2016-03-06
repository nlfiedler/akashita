#
# Makefile provided for convenience purposes.
#
.PHONY: deps aws precompile postclean test release

VERSION = _rel/akashita/Version

deps:
	rebar get-deps
	cd deps/lager && $(MAKE)
	rebar prepare-deps

aws:
	go get -u github.com/aws/aws-sdk-go/...

precompile:
	@(test -f $(GOPATH)/bin/api-info || $(MAKE) aws)
	@(test -d deps || $(MAKE) deps)
	go install github.com/nlfiedler/akashita/klutlan
	@(test -d priv || mkdir priv)
	cp $(GOPATH)/bin/klutlan priv

postclean:
	go clean github.com/nlfiedler/akashita/klutlan

test:
	rebar compile
	rebar ct

release: precompile
	rebar clean
	rebar compile
	relx
	@echo 'Build Date:' `date -R` > $(VERSION)
	@echo 'HEAD Commit:' `git log --max-count=1 --pretty='%h'` >> $(VERSION)
