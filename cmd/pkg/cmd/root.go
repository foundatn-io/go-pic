package cmd

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

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
	Short: "Path to a copybook file",
	Long:  `Provide a path to a single copybook file`,
	RunE:  fileRun,
}

var (
	previewFlag = "preview"
	outFlag     = "output"
	inFlag      = "input"

	previewHelp = "preview in terminal, the results of parsing (not templated)"
	inputHelp   = "path to input file"
	outputHelp  = "path to output file"
)

// Execute executes the root command.
func Execute() error {
	return rootCmd.Execute()
}

func init() { // nolint:gochecknoinits
	dirCmd.Flags().BoolP(previewFlag, "p", false, previewHelp)
	dirCmd.Flags().StringP(outFlag, "o", "", outputHelp)
	dirCmd.Flags().StringP(inFlag, "i", "", inputHelp)
	fileCmd.Flags().BoolP(previewFlag, "p", false, previewHelp)
	fileCmd.Flags().StringP(outFlag, "o", "", outputHelp)
	fileCmd.Flags().StringP(inFlag, "i", "", inputHelp)

	_ = dirCmd.MarkFlagRequired(outFlag)
	_ = dirCmd.MarkFlagRequired(inFlag)
	_ = fileCmd.MarkFlagRequired(outFlag)
	_ = fileCmd.MarkFlagRequired(inFlag)

	rootCmd.AddCommand(dirCmd)
	rootCmd.AddCommand(fileCmd)
}

func dirRun(cmd *cobra.Command, _ []string) error {
	out, err := cmd.Flags().GetString(outFlag)
	if err != nil {
		return err
	}

	in, err := cmd.Flags().GetString(inFlag)
	if err != nil {
		return err
	}

	p, _ := cmd.Flags().GetBool(previewFlag)

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

		if err := run(f, filepath.Join(out, ff.Name()), p); err != nil {
			return err
		}
	}

	return nil
}

func fileRun(cmd *cobra.Command, _ []string) error {
	out, err := cmd.Flags().GetString(outFlag)
	if err != nil {
		return err
	}

	in, err := cmd.Flags().GetString(inFlag)
	if err != nil {
		return err
	}

	p, _ := cmd.Flags().GetBool(previewFlag)

	log.Printf("parsing copybook file %s", in)
	f, err := os.Open(in) // nolint:gosec
	if err != nil {
		return err
	}

	return run(f, out, p)
}

func run(r io.Reader, output string, preview bool) error {
	name := strings.TrimSuffix(output, filepath.Ext(output))
	n := name[strings.LastIndex(name, "/")+1:]

	c := copybook.New(n, template.Copybook())

	b, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}

	tree := lex.NewTree(lex.New("go-pic", string(b)))
	time.Sleep(time.Millisecond)
	c.Root = tree.Parse()
	if preview {
		c.Preview()
	}

	newFile, err := os.Create(fmt.Sprintf("%s.go", name))
	if err != nil {
		return err
	}

	return c.WriteToStruct(newFile)
}
