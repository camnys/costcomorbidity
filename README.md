
# costcomorbidity

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21703584.svg)](https://doi.org/10.5281/zenodo.21703584)

`costcomorbidity` is an R package for calculating the ICD-10 based
Charlson and Elixhauser, as well as the ATC-based RxRisk comorbidity
indices, derived from predicting healthcare costs.

The package is under active development, and its interface and
implementation may evolve in future releases.

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
> Zenodo. <https://doi.org/10.5281/zenodo.21703584>

If you use these indices, please cite the methodological article and
report the version of `costcomorbidity` used. To identify the precise
software implementation, please also cite the archived software release.

Citation information and the installed package version can be obtained
with:

``` r
citation("costcomorbidity")
packageVersion("costcomorbidity")
```

## Licensing and third-party materials

The original code in `costcomorbidity` is licensed under GPL-2. The
package imports the separately distributed package `coder`; it does not
copy the full `coder` classification objects into this repository.

When `mapping = "atc_pratt"` is used, the RxRisk V mapping is obtained
from `coder` at runtime. The Pratt et al. article describing that
mapping is distributed under CC BY-NC 4.0. The `costcomorbidity` licence
does not relicense third-party publications, mappings, data, or other
materials.

Users intending to use the Pratt-based mapping commercially should
independently verify whether additional permission is required. See
[`inst/THIRD-PARTY-NOTICES.md`](inst/THIRD-PARTY-NOTICES.md) for further
information.

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
