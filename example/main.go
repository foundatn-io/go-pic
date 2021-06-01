package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/foundatn-io/go-pic"
)

func main() {
	f, err := os.Open("example/data.txt")
	if err != nil {
		log.Fatal(err)
	}

	d := pic.NewDecoder(f)
	c := &example{}
	if err := d.Decode(c); err != nil {
		log.Fatal(err)
	}

	s, _ := json.MarshalIndent(c, "", "\t")
	fmt.Print(string(s))
}
