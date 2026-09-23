# Standardise ICD-10 and ATC codes

Converts ICD-10 or ATC codes to uppercase and removes spaces,
punctuation, and other non-alphanumeric characters.

## Usage

``` r
clean_codes(x)
```

## Arguments

- x:

  A character vector or factor containing ICD-10 or ATC codes.

## Value

A character vector containing standardised codes.

## Examples

``` r
clean_codes(
  c(
    "i21.0",
    " I50 ",
    "a10ba02"
  )
)
#> [1] "I210"    "I50"     "A10BA02"
```
