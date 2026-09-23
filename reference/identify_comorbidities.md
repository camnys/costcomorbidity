# Identify comorbidity categories from clinical codes

Converts long-format ICD-10 or ATC records into one row per person with
binary comorbidity indicators.

## Usage

``` r
identify_comorbidities(
  people,
  codes,
  index = "charlson",
  id = "id",
  code = "code",
  code_system = "code_system"
)
```

## Arguments

- people:

  A data frame containing one row per person.

- codes:

  A long-format data frame containing clinical codes. Multiple rows per
  person are allowed.

- index:

  Character string specifying the comorbidity index. Must be one of
  \`"charlson"\`, \`"elixhauser"\`, or \`"rxrisk"\`.

- id:

  Character string giving the person-identifier column in both
  \`people\` and \`codes\`.

- code:

  Character string giving the clinical-code column in \`codes\`.

- code_system:

  Character string giving the code-system column in \`codes\`. Charlson
  and Elixhauser use ICD-10 records, while RxRisk uses ATC records.
  Punctuation and letter case in code-system labels are ignored.

## Value

A data frame containing the person identifier followed by one binary
indicator column for each category in the selected index.

## Details

Codes are standardised using \[clean_codes()\], matched against the
selected internal classification mapping, and collapsed so that each
category is coded as either zero or one per person.

For Charlson and Elixhauser, the comorbidity severity hierarchy is
applied, reproducing \`assign0 = TRUE\` from the comorbidity package.
RxRisk categories are not subject to a hierarchy.

## Examples

``` r
people <- data.frame(
  id = c("A", "B")
)

codes <- data.frame(
  id = c("A", "A"),
  code_system = c("ICD10", "ICD-10"),
  code = c("I21.0", "I50.9")
)

identify_comorbidities(
  people = people,
  codes = codes,
  index = "charlson"
)
#>   id cci_mi cci_chf cci_pvd cci_cevd cci_dementia cci_cpd cci_rheumd cci_pud
#> 1  A      1       1       0        0            0       0          0       0
#> 2  B      0       0       0        0            0       0          0       0
#>   cci_mld cci_diab cci_diabwc cci_hp cci_rend cci_canc cci_msld cci_metacanc
#> 1       0        0          0      0        0        0        0            0
#> 2       0        0          0      0        0        0        0            0
#>   cci_aids
#> 1        0
#> 2        0
```
