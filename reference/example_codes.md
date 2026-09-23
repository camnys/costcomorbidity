# Synthetic ICD-10 and ATC records

A synthetic long-format dataset containing ICD-10 diagnoses and ATC
prescription codes for individuals in \[example_people\]. The records
occur during the year preceding the index date.

## Usage

``` r
example_codes
```

## Format

A data frame with multiple rows per synthetic person and four variables:

- id:

  Synthetic person identifier.

- code_system:

  Coding system, either \`"ICD10"\` or \`"ATC"\`.

- code:

  ICD-10 diagnosis code or ATC medicine code.

- code_date:

  Synthetic date on which the code was recorded.

## Source

Synthetic data generated for the costcomorbidity package.

## Details

The data contain no real patients, identifiers, diagnoses,
prescriptions, or dates.
