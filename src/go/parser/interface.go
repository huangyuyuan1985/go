// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file contains the exported entry points for invoking the parser.

package parser

import (
	"bytes"
	"errors"
	"go/ast"
	"go/token"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
)

// If src != nil, readSource converts src to a []byte if possible;
// otherwise it returns an error. If src == nil, readSource returns
// the result of reading the file specified by filename.
//
//
func readSource(filename string, src interface{}) ([]byte, error) {
	if src != nil {
		switch s := src.(type) {
		case string:
			return []byte(s), nil
		case []byte:
			return s, nil
		case *bytes.Buffer:
			// is io.Reader, but src is already available in []byte form
			if s != nil {
				return s.Bytes(), nil
			}
		case io.Reader:
			return ioutil.ReadAll(s)
		}
		return nil, errors.New("invalid source")
	}
	return ioutil.ReadFile(filename)
}

// A Mode value is a set of flags (or 0).
// They control the amount of source code parsed and other optional
// parser functionality.
//
type Mode uint

const (
	PackageClauseOnly Mode             = 1 << iota // stop parsing after package clause 1
	ImportsOnly                                    // stop parsing after import declarations 2
	ParseComments                                  // parse comments and add them to AST 4
	Trace                                          // print a trace of parsed productions 8
	DeclarationErrors                              // report declaration errors  16
	SpuriousErrors                                 // same as AllErrors, for backward-compatibility  32
	AllErrors         = SpuriousErrors             // report all errors (not just the first 10 on different lines)
)

// ParseFile parses the source code of a single Go source file and returns
// the corresponding ast.File node. The source code may be provided via
// the filename of the source file, or via the src parameter.
//
// If src != nil, ParseFile parses the source from src and the filename is
// only used when recording position information. The type of the argument
// for the src parameter must be string, []byte, or io.Reader.
// If src == nil, ParseFile parses the file specified by filename.
//
// The mode parameter controls the amount of source text parsed and other
// optional parser functionality. Position information is recorded in the
// file set fset, which must not be nil.
//
// If the source couldn't be read, the returned AST is nil and the error
// indicates the specific failure. If the source was read but syntax
// errors were found, the result is a partial AST (with ast.Bad* nodes
// representing the fragments of erroneous source code). Multiple errors
// are returned via a scanner.ErrorList which is sorted by file position.
//
/*
PARSEFILE解析单个GO源文件的源代码，并返回相应的AST.file节点。源代码可以通过源文件的文件名或通过SRC参数提供。

如果SRC！= NIL，PARSEFILE从SRC解析源，文件名仅用于记录位置信息。SRC参数的参数类型必须是String、[]字节或IO.Reader。

如果SRC=＝NIL，PARSEFILE解析文件名指定的文件。

模式参数控制解析的源文本的数量和其他可选的解析器功能。位置信息记录在文件集fSET中，它不能为 nil。

如果无法读取源代码，返回的AST为零，错误指示特定故障。如果读取源但发现语法错误，则结果是部分AST（AST.BAD*节点表示错误源代码的片段）。
通过文件位置排序的 scanner.ErrorList 返回多个错误。
 */
func ParseFile(fset *token.FileSet, filename string, src interface{}, mode Mode) (f *ast.File, err error) {
	if fset == nil {
		panic("parser.ParseFile: no token.FileSet provided (fset == nil)")
	}

	// get source
	// 读内容
	text, err := readSource(filename, src)
	if err != nil {
		return nil, err
	}

	var p parser
	defer func() {
		if e := recover(); e != nil {
			// resume same panic if it's not a bailout
			if _, ok := e.(bailout); !ok {
				panic(e)
			}
		}

		// set result values
		if f == nil {
			// source is not a valid Go source file - satisfy
			// ParseFile API and return a valid (but) empty
			// *ast.File
			f = &ast.File{
				Name:  new(ast.Ident),
				Scope: ast.NewScope(nil),
			}
		}

		p.errors.Sort()
		err = p.errors.Err()
	}()

	// parse source
	// 初始化内容
	// 传入空的 fset， 文件名，文件内容，

	p.init(fset, filename, text, mode)

	// 生成 ast.file  抽象语法树，并且返回
	f = p.parseFile()

	return
}

// ParseDir calls ParseFile for all files with names ending in ".go" in the
// directory specified by path and returns a map of package name -> package
// AST with all the packages found.
//
// If filter != nil, only the files with os.FileInfo entries passing through
// the filter (and ending in ".go") are considered. The mode bits are passed
// to ParseFile unchanged. Position information is recorded in fset, which
// must not be nil.
//
// If the directory couldn't be read, a nil map and the respective error are
// returned. If a parse error occurred, a non-nil but incomplete map and the
// first error encountered are returned.
//
func ParseDir(fset *token.FileSet, path string, filter func(os.FileInfo) bool, mode Mode) (pkgs map[string]*ast.Package, first error) {
	fd, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer fd.Close()

	list, err := fd.Readdir(-1)
	if err != nil {
		return nil, err
	}

	pkgs = make(map[string]*ast.Package)
	for _, d := range list {
		if strings.HasSuffix(d.Name(), ".go") && (filter == nil || filter(d)) {
			filename := filepath.Join(path, d.Name())
			if src, err := ParseFile(fset, filename, nil, mode); err == nil {
				name := src.Name.Name
				pkg, found := pkgs[name]
				if !found {
					pkg = &ast.Package{
						Name:  name,
						Files: make(map[string]*ast.File),
					}
					pkgs[name] = pkg
				}
				pkg.Files[filename] = src
			} else if first == nil {
				first = err
			}
		}
	}

	return
}

// ParseExprFrom is a convenience function for parsing an expression.
// The arguments have the same meaning as for ParseFile, but the source must
// be a valid Go (type or value) expression. Specifically, fset must not
// be nil.
//
// 从字符串开始编译 mode 0
// 生成 ast 结构体，是一个深度遍历的过程
func ParseExprFrom(fset *token.FileSet, filename string, src interface{}, mode Mode) (ast.Expr, error) {
	if fset == nil {
		panic("parser.ParseExprFrom: no token.FileSet provided (fset == nil)")
	}

	// get source
	text, err := readSource(filename, src)
	if err != nil {
		return nil, err
	}

	var p parser
	defer func() {
		if e := recover(); e != nil {
			// resume same panic if it's not a bailout
			if _, ok := e.(bailout); !ok {
				panic(e)
			}
		}
		p.errors.Sort()
		err = p.errors.Err()
	}()

	// TODO 初始化
	// parse expr
	p.init(fset, filename, text, mode)

	// Set up pkg-level scopes to avoid nil-pointer errors.
	// This is not needed for a correct expression x as the
	// parser will be ok with a nil topScope, but be cautious
	// in case of an erroneous x.
	// 运行层
	p.openScope()
	//
	p.pkgScope = p.topScope

	//
	e := p.parseRhsOrType()

	// 把运行范围返回上一层
	p.closeScope()
	assert(p.topScope == nil, "unbalanced scopes")

	// If a semicolon was inserted, consume it;
	// report an error if there's more tokens.
	if p.tok == token.SEMICOLON && p.lit == "\n" {
		p.next()
	}
	p.expect(token.EOF)

	if p.errors.Len() > 0 {
		p.errors.Sort()
		return nil, p.errors.Err()
	}

	return e, nil
}

// ParseExpr is a convenience function for obtaining the AST of an expression x.
// The position information recorded in the AST is undefined. The filename used
// in error messages is the empty string.
//
func ParseExpr(x string) (ast.Expr, error) {
	return ParseExprFrom(token.NewFileSet(), "", []byte(x), 0)
}
