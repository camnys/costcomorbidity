# Apply cost-based comorbidity weights

Calculates a cost-based comorbidity score from binary Charlson,
Elixhauser, or RxRisk indicators.

## Usage

``` r
apply_cost_weights(
  indicators,
  index = "charlson",
  id = "id",
  keep_indicators = TRUE
)
```

## Arguments

- indicators:

  A data frame containing one row per person and one binary column for
  each category in the selected index.

- index:

  Character string specifying the comorbidity index. Must be one of
  \`"charlson"\`, \`"elixhauser"\`, or \`"rxrisk"\`.

- id:

  Character string giving the person-identifier column in
  \`indicators\`.

- keep_indicators:

  Logical. If \`TRUE\`, the binary indicators are retained. If
  \`FALSE\`, only the identifier and score are returned.

## Value

A data frame containing the identifier, optionally the binary
indicators, and one of \`costbased_charlson\`, \`costbased_elixhauser\`,
or \`costbased_rxrisk\`.

## Details

Each binary indicator is multiplied by its corresponding cost-based
coefficient, and the weighted values are summed for each person.

## Examples

``` r
charlson_indicators <- identify_comorbidities(
  people = example_people,
  codes = example_codes,
  index = "charlson"
)

charlson_scores <- apply_cost_weights(
  indicators = charlson_indicators,
  index = "charlson",
  keep_indicators = FALSE
)

head(charlson_scores)
#> # A tibble: 6 × 2
#>   id    costbased_charlson
#>   <chr>              <dbl>
#> 1 S0001               3.70
#> 2 S0002               0   
#> 3 S0003               0   
#> 4 S0004               1.44
#> 5 S0005               0   
#> 6 S0006               0   
```
