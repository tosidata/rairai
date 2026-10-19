# Create an MCP tool error response

Create an MCP tool error response

## Usage

``` r
tool_error_response(..., auto_unbox = TRUE)
```

## Arguments

- ...:

  Fields to include in the JSON object.

- auto_unbox:

  Passed to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

## Value

Throws a tool-level error whose message is JSON text.
