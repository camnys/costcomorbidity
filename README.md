
# costcomorbidity

`costcomorbidity` is an R package for calculating the ICD-10 based
Charlson and Elixhauser, as well as the ATC-based RxRisk comorbidity
indices, derived from predicting healthcare costs.

More information about the derivation of the cost-based indices can be
found in: Nystrand, C., Johansson, N., Blom, J. Derivation and
Validation of Charlson, Elixhauser and RxRisk Cost-Based Comorbidity
Indices: A Register-Based Study of 350,000 Individuals Over 11 Years.
Accepted in Pharmacoeconomics. DOI: 10.1007/s40273-026-01652-x

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

If you use `costcomorbidity` in research or a publication, please cite
the package. The current citation and corresponding BibTeX entry can be
obtained by running:

``` r
citation("costcomorbidity")
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
