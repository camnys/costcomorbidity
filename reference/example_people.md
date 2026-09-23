# Synthetic example population

A synthetic person-level dataset used to demonstrate the costcomorbidity
package. The data do not represent real individuals.

## Usage

``` r
example_people
```

## Format

A data frame with one row per synthetic person and four variables:

- id:

  Synthetic person identifier.

- index_date:

  Start date of the prediction year.

- age:

  Age in years at the index date.

- sex:

  Synthetic sex variable.

## Source

Synthetic data generated for the costcomorbidity package.

## Details

ICD-10 diagnoses and ATC prescription records for these individuals are
available in \[example_codes\]. One individual deliberately has no code
records.
