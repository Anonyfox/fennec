// esbuild for Fennec, exposed as a C archive that is statically linked into the
// OCaml binary — in-process bundling, no node, no subprocess. Warm build
// contexts give incremental rebuilds; one-shot builds give exact prod blobs.
package main

/*
#include <stdlib.h>
*/
import "C"

import (
	"encoding/json"
	"sync"

	"github.com/evanw/esbuild/pkg/api"
)

type opts struct {
	Entry      string   `json:"entry"`
	Format     string   `json:"format"`     // iife | esm | cjs
	GlobalName string   `json:"globalName"`
	External   []string `json:"external"`
	Minify     bool     `json:"minify"`
	Sourcemap  bool     `json:"sourcemap"`
	Banner     string   `json:"banner"`
}

func toBuild(o opts) api.BuildOptions {
	f := api.FormatIIFE
	switch o.Format {
	case "esm":
		f = api.FormatESModule
	case "cjs":
		f = api.FormatCommonJS
	}
	b := api.BuildOptions{
		EntryPoints: []string{o.Entry},
		Bundle:      true,
		Format:      f,
		GlobalName:  o.GlobalName,
		Platform:    api.PlatformBrowser,
		External:    o.External,
		Write:       false,
	}
	if o.Minify {
		b.MinifyWhitespace = true
		b.MinifyIdentifiers = true
		b.MinifySyntax = true
	}
	if o.Sourcemap {
		b.Sourcemap = api.SourceMapInline
	}
	if o.Banner != "" {
		b.Banner = map[string]string{"js": o.Banner}
	}
	return b
}

var (
	mu   sync.Mutex
	ctxs = map[int]api.BuildContext{}
	next = 1
)

//export fennec_esbuild_ctx_create
func fennec_esbuild_ctx_create(optsJSON *C.char) C.int {
	var o opts
	if err := json.Unmarshal([]byte(C.GoString(optsJSON)), &o); err != nil {
		return -1
	}
	c, err := api.Context(toBuild(o))
	if err != nil {
		return -1
	}
	mu.Lock()
	id := next
	next++
	ctxs[id] = c
	mu.Unlock()
	return C.int(id)
}

// On success *outLen = byte length, returns the bundle bytes.
// On build error *outLen = -2, returns the formatted error text.
//
//export fennec_esbuild_ctx_rebuild
func fennec_esbuild_ctx_rebuild(handle C.int, outLen *C.int) *C.char {
	mu.Lock()
	c := ctxs[int(handle)]
	mu.Unlock()
	if c == nil {
		*outLen = -1
		return nil
	}
	r := c.Rebuild()
	if len(r.Errors) > 0 {
		joined := ""
		for _, m := range api.FormatMessages(r.Errors, api.FormatMessagesOptions{Color: false}) {
			joined += m
		}
		*outLen = -2
		return C.CString(joined)
	}
	if len(r.OutputFiles) == 0 {
		*outLen = 0
		return nil
	}
	code := r.OutputFiles[0].Contents
	*outLen = C.int(len(code))
	return (*C.char)(C.CBytes(code))
}

//export fennec_esbuild_ctx_dispose
func fennec_esbuild_ctx_dispose(handle C.int) {
	mu.Lock()
	c := ctxs[int(handle)]
	delete(ctxs, int(handle))
	mu.Unlock()
	if c != nil {
		c.Dispose()
	}
}

func main() {}
