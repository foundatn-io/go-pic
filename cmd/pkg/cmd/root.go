package cmd

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/pgmitche/go-pic/cmd/pkg/parse"
)

var rootCmd = &cobra.Command{
	Use:   "gopic",
	Short: "Gopic - COBOL copybooks to structs",
	Long:  "Gopic helps you generate struct representations from textual PIC definitions (COBOL copybook)",
}

var dirCmd = &cobra.Command{
	Use:   "dir",
	Short: "Path to a directory with copybooks",
	Long:  `Provide a path to a directory containing only copybook files`,
	RunE:  dirRun,
}

var fileCmd = &cobra.Command{
	Use:   "file",
	Short: "Path to a directory with copybooks",
	Long:  `Provide a path to a directory containing only copybook files`,
	RunE:  fileRun,
}

// Execute executes the root command.
func Execute() error {
	log.Println("Always check your output! go-pic struct generation is not complete, it does not yet understand every possible aspect of copybook definitions")
	return rootCmd.Execute()
}

func init() { // nolint:gochecknoinits
	dirCmd.Flags().StringP("output", "o", "", "path to output directory")
	dirCmd.Flags().StringP("input", "i", "", "path to input directory")
	fileCmd.Flags().StringP("output", "o", "", "path to output file")
	fileCmd.Flags().StringP("input", "i", "", "path to input file")

	dirCmd.MarkFlagRequired("output")  // nolint:errcheck,gosec
	dirCmd.MarkFlagRequired("input")   // nolint:errcheck,gosec
	fileCmd.MarkFlagRequired("output") // nolint:errcheck,gosec
	fileCmd.MarkFlagRequired("input")  // nolint:errcheck,gosec

	rootCmd.AddCommand(dirCmd)
	rootCmd.AddCommand(fileCmd)
}

func dirRun(cmd *cobra.Command, _ []string) error {
	out, err := cmd.Flags().GetString("output")
	if err != nil {
		return err
	}

	in, err := cmd.Flags().GetString("input")
	if err != nil {
		return err
	}

	fs, err := ioutil.ReadDir(in)
	if err != nil {
		return err
	}

	_, err = os.Stat(out)
	if os.IsNotExist(err) {
		errDir := os.MkdirAll(out, 0750)
		if errDir != nil {
			return errDir
		}
	}

	for _, ff := range fs {
		if ff.IsDir() {
			continue
		}

		log.Printf("parsing copybook file %s", ff.Name())
		f, err := os.Open(filepath.Join(in, ff.Name())) // nolint:gosec
		if err != nil {
			return err
		}

		if err := run(f, filepath.Join(out, ff.Name())); err != nil {
			return err
		}
	}

	return nil
}

func fileRun(cmd *cobra.Command, _ []string) error {
	out, err := cmd.Flags().GetString("output")
	if err != nil {
		return err
	}

	in, err := cmd.Flags().GetString("input")
	if err != nil {
		return err
	}

	log.Printf("parsing copybook file %s", in)
	f, err := os.Open(in) // nolint:gosec
	if err != nil {
		return err
	}

	return run(f, out)
}

func run(r io.Reader, output string) error {

	name := strings.TrimSuffix(output, filepath.Ext(output))
	c := &parse.Copybook{Name: name[strings.LastIndex(name, "/")+1:]}
	b, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}

	if err = parse.Unmarshal(b, c); err != nil {
		return err
	}

	newFile, err := os.Create(fmt.Sprintf("%s.go", name))
	if err != nil {
		return err
	}

	return c.ToSruct(newFile)
}
