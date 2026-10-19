# Serve the MCP tools

Runs a Model Context Protocol (MCP) server over standard input and
output.

## Usage

``` r
serve(path = ".")
```

## Arguments

- path:

  Existing package directory to inspect by default. Accepts absolute,
  relative, and tilde paths. Defaults to the current working directory.

## Value

Called for its side effect of serving MCP requests over standard input
and output. Blocks the current R process until standard input closes.

## Details

Tools that inspect a local package can use `package_root` to select a
different directory for one request. Supply an absolute path; this does
not change the server's default directory.

## File access and security

**The `path` argument is not a sandbox.** It selects a default directory
but does not restrict access to other directories. An MCP client can
request any directory readable by the R process. Responses may disclose
file names, file contents, and absolute paths to that client. The
inspected package's code is not loaded or executed.

To limit access, run the server with restricted filesystem permissions
or in a container that exposes only the files you intend to share.

## See also

[`vignette("tools", package = "rairai")`](https://tosidata.github.io/rairai/articles/tools.md)
for the tool guide.
