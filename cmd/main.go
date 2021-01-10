package main

import (
	"fmt"
	"os"

	"github.com/pgmitche/go-pic/cmd/pkg/cmd"
)

func main() {
	if err := cmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
