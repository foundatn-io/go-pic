package main

import (
	"fmt"
	"os"

	"github.com/foundatn-io/go-pic/cmd/pkg/cli"
)

func main() {
	if err := cli.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
