package cli

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

	"github.com/foundatn-io/go-pic/cmd/pkg/copybook"
	"github.com/foundatn-io/go-pic/cmd/pkg/template"
	"github.com/foundatn-io/go-pic/pkg/lex"
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
	displayFlag = "display"
	outFlag     = "output"
	inFlag      = "input"
	pkgFlag     = "package"

	displayHelp = "display preview in terminal, the results of parsing (not templated)"
	inputHelp   = "path to input file"
	outputHelp  = "path to output file"
	pkgHelp     = "output file package name"
)

// Execute executes the root command.
func Execute() error {
	return fmt.Errorf("execute: %w", rootCmd.Execute())
}

func init() { //nolint:gochecknoinits
	dirCmd.Flags().BoolP(displayFlag, "d", false, displayHelp)
	dirCmd.Flags().StringP(outFlag, "o", "", outputHelp)
	dirCmd.Flags().StringP(inFlag, "i", "", inputHelp)
	dirCmd.Flags().StringP(pkgFlag, "p", "", pkgHelp)

	fileCmd.Flags().BoolP(displayFlag, "d", false, displayHelp)
	fileCmd.Flags().StringP(outFlag, "o", "", outputHelp)
	fileCmd.Flags().StringP(inFlag, "i", "", inputHelp)
	fileCmd.Flags().StringP(pkgFlag, "p", "", pkgHelp)

	_ = dirCmd.MarkFlagRequired(outFlag)
	_ = dirCmd.MarkFlagRequired(inFlag)
	_ = dirCmd.MarkFlagRequired(pkgFlag)

	_ = fileCmd.MarkFlagRequired(outFlag)
	_ = fileCmd.MarkFlagRequired(inFlag)
	_ = fileCmd.MarkFlagRequired(pkgFlag)

	rootCmd.AddCommand(dirCmd)
	rootCmd.AddCommand(fileCmd)
}

func dirRun(cmd *cobra.Command, _ []string) error { //nolint:gocyclo
	out, err := cmd.Flags().GetString(outFlag)
	if err != nil {
		return fmt.Errorf("failed to extract value for flag %s: %w", outFlag, err)
	}

	in, err := cmd.Flags().GetString(inFlag)
	if err != nil {
		return fmt.Errorf("failed to extract value for flag %s: %w", inFlag, err)
	}

	pkg, err := cmd.Flags().GetString(pkgFlag)
	if err != nil {
		return fmt.Errorf("failed to extract value for flag %s: %w", pkgFlag, err)
	}

	d, _ := cmd.Flags().GetBool(displayFlag)

	fs, err := ioutil.ReadDir(in)
	if err != nil {
		return fmt.Errorf("failed to read files for dir %s: %w", in, err)
	}

	_, err = os.Stat(out)
	if os.IsNotExist(err) {
		errDir := os.MkdirAll(out, os.ModePerm)
		if errDir != nil {
			return fmt.Errorf("failed to create dir %s: %w", out, errDir)
		}
	}

	for _, ff := range fs {
		if ff.IsDir() {
			continue
		}

		log.Printf("parsing copybook file %s", ff.Name())
		f, err := os.Open(filepath.Join(in, ff.Name())) //nolint:gosec
		if err != nil {
			return fmt.Errorf("failed to open file %s: %w", ff.Name(), err)
		}

		if err := run(f, filepath.Join(out, ff.Name()), pkg, d); err != nil {
			return err
		}
	}

	return nil
}

func fileRun(cmd *cobra.Command, _ []string) error {
	out, err := cmd.Flags().GetString(outFlag)
	if err != nil {
		return fmt.Errorf("failed to extract value for flag %s: %w", outFlag, err)
	}

	in, err := cmd.Flags().GetString(inFlag)
	if err != nil {
		return fmt.Errorf("failed to extract value for flag %s: %w", inFlag, err)
	}

	pkg, err := cmd.Flags().GetString(pkgFlag)
	if err != nil {
		return fmt.Errorf("failed to extract value for flag %s: %w", pkgFlag, err)
	}

	d, _ := cmd.Flags().GetBool(displayFlag)

	log.Printf("parsing copybook file %s", in)
	f, err := os.Open(in) //nolint:gosec
	if err != nil {
		return fmt.Errorf("failed to open file %s: %w", in, err)
	}

	return run(f, out, pkg, d)
}

func run(r io.Reader, output, pkg string, preview bool) error {
	name := strings.TrimSuffix(output, filepath.Ext(output))
	n := name[strings.LastIndex(name, "/")+1:]

	c := copybook.New(n, pkg, template.Copybook())

	b, err := ioutil.ReadAll(r)
	if err != nil {
		return fmt.Errorf("failed to read input data: %w", err)
	}

	tree := lex.NewTree(lex.New(n, string(b)))
	time.Sleep(time.Millisecond)
	c.Root, err = tree.Parse()
	if err != nil {
		return fmt.Errorf("failed to parse copybook: %w", err)
	}
	// TODO: (pgmitche) if record in tree is struct but has no children,
	// it should probably be ignored entirely
	if preview {
		c.Preview()
	}

	name = fmt.Sprintf("%s.go", name)
	newFile, err := os.Create(name) //nolint:gosec // intentionally creating file
	if err != nil {
		return fmt.Errorf("failed to create file %s: %w", name, err)
	}
	defer func() {
		if err := newFile.Close(); err != nil {
			log.Fatalln(err)
		}
	}()

	return fmt.Errorf("write to struct: %w", c.WriteToStruct(newFile))
}
