# This Makefile is meant to be used by people that do not usually work
# with Go source code. If you know what GOPATH is then you probably
# don't need to bother with make.

.PHONY: gvc android ios gvc-cross swarm evm all test clean
.PHONY: gvc-linux gvc-linux-386 gvc-linux-amd64 gvc-linux-mips64 gvc-linux-mips64le
.PHONY: gvc-linux-arm gvc-linux-arm-5 gvc-linux-arm-6 gvc-linux-arm-7 gvc-linux-arm64
.PHONY: gvc-darwin gvc-darwin-386 gvc-darwin-amd64
.PHONY: gvc-windows gvc-windows-386 gvc-windows-amd64

GOBIN = $(shell pwd)/build/bin
GO ?= latest

gvc:
	build/env.sh go run build/ci.go install ./cmd/gvc

	@echo "Done building."
	@echo "Run \"$(GOBIN)/gvc\" to launch gvc."

swarm:
	build/env.sh go run build/ci.go install ./cmd/swarm
	@echo "Done building."
	@echo "Run \"$(GOBIN)/swarm\" to launch swarm."

all:
	build/env.sh go run build/ci.go install

android:
	build/env.sh go run build/ci.go aar --local
	@echo "Done building."
	@echo "Import \"$(GOBIN)/gvc.aar\" to use the library."

ios:
	build/env.sh go run build/ci.go xcode --local
	@echo "Done building."
	@echo "Import \"$(GOBIN)/gvc.framework\" to use the library."

test: all
	build/env.sh go run build/ci.go test

lint: ## Run linters.
	build/env.sh go run build/ci.go lint

clean:
	./build/clean_go_build_cache.sh
	rm -fr build/_workspace/pkg/ $(GOBIN)/*

# The devtools target installs tools required for 'go generate'.
# You need to put $GOBIN (or $GOPATH/bin) in your PATH to use 'go generate'.

devtools:
	env GOBIN= go get -u golang.org/x/tools/cmd/stringer
	env GOBIN= go get -u github.com/kevinburke/go-bindata/go-bindata
	env GOBIN= go get -u github.com/fjl/gencodec
	env GOBIN= go get -u github.com/golang/protobuf/protoc-gen-go
	env GOBIN= go install ./cmd/abigen
	@type "npm" 2> /dev/null || echo 'Please install node.js and npm'
	@type "solc" 2> /dev/null || echo 'Please install solc'
	@type "protoc" 2> /dev/null || echo 'Please install protoc'

swarm-devtools:
	env GOBIN= go install ./cmd/swarm/mimegen

# Cross Compilation Targets (xgo)

gvc-cross: gvc-linux gvc-darwin gvc-windows gvc-android gvc-ios
	@echo "Full cross compilation done:"
	@ls -ld $(GOBIN)/gvc-*

gvc-linux: gvc-linux-386 gvc-linux-amd64 gvc-linux-arm gvc-linux-mips64 gvc-linux-mips64le
	@echo "Linux cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-*

gvc-linux-386:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/386 -v ./cmd/gvc
	@echo "Linux 386 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep 386

gvc-linux-amd64:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/amd64 -v ./cmd/gvc
	@echo "Linux amd64 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep amd64

gvc-linux-arm: gvc-linux-arm-5 gvc-linux-arm-6 gvc-linux-arm-7 gvc-linux-arm64
	@echo "Linux ARM cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep arm

gvc-linux-arm-5:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/arm-5 -v ./cmd/gvc
	@echo "Linux ARMv5 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep arm-5

gvc-linux-arm-6:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/arm-6 -v ./cmd/gvc
	@echo "Linux ARMv6 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep arm-6

gvc-linux-arm-7:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/arm-7 -v ./cmd/gvc
	@echo "Linux ARMv7 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep arm-7

gvc-linux-arm64:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/arm64 -v ./cmd/gvc
	@echo "Linux ARM64 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep arm64

gvc-linux-mips:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/mips --ldflags '-extldflags "-static"' -v ./cmd/gvc
	@echo "Linux MIPS cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep mips

gvc-linux-mipsle:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/mipsle --ldflags '-extldflags "-static"' -v ./cmd/gvc
	@echo "Linux MIPSle cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep mipsle

gvc-linux-mips64:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/mips64 --ldflags '-extldflags "-static"' -v ./cmd/gvc
	@echo "Linux MIPS64 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep mips64

gvc-linux-mips64le:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=linux/mips64le --ldflags '-extldflags "-static"' -v ./cmd/gvc
	@echo "Linux MIPS64le cross compilation done:"
	@ls -ld $(GOBIN)/gvc-linux-* | grep mips64le

gvc-darwin: gvc-darwin-386 gvc-darwin-amd64
	@echo "Darwin cross compilation done:"
	@ls -ld $(GOBIN)/gvc-darwin-*

gvc-darwin-386:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=darwin/386 -v ./cmd/gvc
	@echo "Darwin 386 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-darwin-* | grep 386

gvc-darwin-amd64:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=darwin/amd64 -v ./cmd/gvc
	@echo "Darwin amd64 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-darwin-* | grep amd64


gvc-windows: gvc-windows-386 gvc-windows-amd64
	@echo "Windows cross compilation done:"
	@ls -ld $(GOBIN)/gvc-windows-*

gvc-windows-new:
	CGO_ENABLED=0 GOOS=windows GOARCH=amd64
	build/env.sh go run build/ci.go install ./cmd/gvc
	@echo "Windows cross compilation done:"

gvc-windows-386:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=windows/386 -v ./cmd/gvc
	@echo "Windows 386 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-windows-* | grep 386

gvc-windows-amd64:
	build/env.sh go run build/ci.go xgo -- --go=$(GO) --targets=windows/amd64 -v ./cmd/gvc
	@echo "Windows amd64 cross compilation done:"
	@ls -ld $(GOBIN)/gvc-windows-* | grep amd64
