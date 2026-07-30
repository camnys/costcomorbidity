
# costcomorbidity

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXXX.svg)](https://doi.org/10.5281/zenodo.21703585)

`costcomorbidity` is an R package for calculating the ICD-10 based
Charlson and Elixhauser, as well as the ATC-based RxRisk comorbidity
indices, derived from predicting healthcare costs.

The package is currently under active development. Its interface and
coefficient implementation may change before the first formal release.

## Installation

`costcomorbidity` is not currently available from CRAN. The development
version can be installed from GitHub using the `pak` package:

``` r
# Install pak once, if it is not already installed
install.packages("pak")

# Install costcomorbidity from GitHub
pak::pkg_install("camnys/costcomorbidity")
```

Load the package after installation:

``` r
library(costcomorbidity)
```

## Citation

The cost-based comorbidity weights implemented in `costcomorbidity` were
derived and internally validated in:

> Nystrand, C., Johansson, N., and Blom, J. (2026). Derivation and
> Validation of Charlson, Elixhauser and RxRisk Cost-Based Comorbidity
> Indices: A Register-Based Study of 350,000 Individuals Over 11 Years.
> *PharmacoEconomics*. Accepted for publication.
> <https://doi.org/10.1007/s40273-026-01652-x>

The archived software release is:

> Nystrand, C. (2026). *costcomorbidity: Cost-Based Comorbidity Indices*
> (version 0.1.1). Zenodo. <https://doi.org/10.5281/zenodo.21703585>

If you use these indices, please cite the methodological article and
report the version of `costcomorbidity` used. To identify the precise
software implementation, please also cite the archived software release.

Citation information and the installed package version can be obtained
with:

``` r
citation("costcomorbidity")
packageVersion("costcomorbidity")
```

## Supported indices

The package supports:

- Cost-based Charlson index
- Cost-based Elixhauser index
- Cost-based RxRisk index

The resulting score variables are named:

- `costbased_charlson`
- `costbased_elixhauser`
- `costbased_rxrisk`

## RxRisk classification

RxRisk categories are identified at runtime using the `atc_pratt`
classification provided by the `coder` R package. The full Pratt mapping
is not redistributed by `costcomorbidity`.

The cost-based RxRisk coefficients distributed by `costcomorbidity` were
estimated by the package authors.

## Example

``` r
library(costcomorbidity)

rxrisk_indicators <- identify_comorbidities(
  people = example_people,
  codes = example_codes,
  index = "rxrisk"
)

rxrisk_scores <- apply_cost_weights(
  indicators = rxrisk_indicators,
  index = "rxrisk",
  keep_indicators = FALSE
)

head(rxrisk_scores)
#> # A tibble: 6 × 2
#>   id    costbased_rxrisk
#>   <chr>            <dbl>
#> 1 S0001            4.05 
#> 2 S0002            0.259
#> 3 S0003            0    
#> 4 S0004            0.756
#> 5 S0005            4.05 
#> 6 S0006            0
```
