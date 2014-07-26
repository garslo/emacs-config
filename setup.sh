#!/bin/sh

# Change this to wherever you want your tools installed
GO_TOOLS_PATH=~/go/go-tools-ws

mkdir -p $GO_TOOLS_PATH
cd $GO_TOOLS_PATH
export GOPATH=$GO_TOOLS_PATH

# Get all the tools we need
echo "Fetching gocode..."
go get github.com/nsf/gocode
echo "Fetching godef..."
go get code.google.com/p/rog-go/exp/cmd/godef
echo "Fetching errcheck..."
go get github.com/kisielk/errcheck
echo "Fetching golint..."
go get github.com/golang/golint
echo "Fetching oracle..."
go get code.google.com/p/go.tools/cmd/oracle
echo "Fetching goimports..."
go get github.com/bradfitz/goimports
