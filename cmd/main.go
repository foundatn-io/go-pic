package main

import (
	"fmt"
	"os"

	"github.com/pgmitche/go-pic/cmd/pkg/cli"
)

func main() {
	if err := cli.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
