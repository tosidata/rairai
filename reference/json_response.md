# Create a JSON response

Absent, empty, and `NA` values each have distinct JSON-shaping meanings;
use them consistently.

## Usage

``` r
json_response(..., auto_unbox = TRUE)
```

## Arguments

- ...:

  Fields to include in the JSON object.

- auto_unbox:

  Passed to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

## Value

A JSON object string.
