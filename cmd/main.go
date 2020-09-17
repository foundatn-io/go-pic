package main

import (
	"fmt"
	"log"
	"os"

	"github.com/pgmitche/go-pic/cmd/pkg/cmd"
)

func main() {
	if err := cmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	log.Println("Remember... always check your output! go-pic struct generation is not complete, it does not yet understand every possible aspect of copybook definitions")
}
