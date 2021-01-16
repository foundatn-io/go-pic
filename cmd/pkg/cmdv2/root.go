package cmdv2

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/pgmitche/go-pic/cmd/pkg/copybook"
	"github.com/pgmitche/go-pic/cmd/pkg/template"
	"github.com/pgmitche/go-pic/pkg/lex"
)

var rootCmd = &cobra.Command{
	Use:   "gopic",
	Short: "go-pic - COBOL copybooks to structs",
	Long:  "go-pic helps you generate struct representations from textual PIC definitions (COBOL copybook). I should have called this Gopybook",
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
	return rootCmd.Execute()
}

func init() { // nolint:gochecknoinits
	dirCmd.Flags().StringP("output", "o", "", "path to output directory")
	dirCmd.Flags().StringP("input", "i", "", "path to input directory")
	fileCmd.Flags().StringP("output", "o", "", "path to output file")
	fileCmd.Flags().StringP("input", "i", "", "path to input file")

	_ = dirCmd.MarkFlagRequired("output")
	_ = dirCmd.MarkFlagRequired("input")
	_ = fileCmd.MarkFlagRequired("output")
	_ = fileCmd.MarkFlagRequired("input")

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
	n := name[strings.LastIndex(name, "/")+1:]

	c := copybook.New(n, template.Copybook())

	b, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}

	lxr := lex.New("go-pic", string(b))
	tree := lex.NewTree(lxr)
	result := tree.Parse()

	log.Println(result)

	// if err = decoder.Unmarshal(b, c); err != nil {
	// 	return err
	// }

	newFile, err := os.Create(fmt.Sprintf("%s.go", name))
	if err != nil {
		return err
	}

	return c.WriteToStruct(newFile)
}
