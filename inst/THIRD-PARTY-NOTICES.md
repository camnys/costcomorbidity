# Third-party software and classification materials

## `coder`

`costcomorbidity` imports and calls the separately distributed R package `coder`.
The `coder` package is licensed under GPL-2. `costcomorbidity` does not relicense
`coder` or materials distributed with it.

## RxRisk V mapping associated with Pratt et al.

When `mapping = "atc_pratt"` is selected, `costcomorbidity` accesses the RxRisk V
classification supplied by the installed `coder` package at runtime. The
ATC-to-comorbidity mapping is associated with:

Pratt NL, Kerr M, Barratt JD, et al. The validity of the Rx-Risk Comorbidity
Index using medicines mapped to the Anatomical Therapeutic Chemical (ATC)
Classification System. *BMJ Open*. 2018;8:e021122.
https://doi.org/10.1136/bmjopen-2017-021122

The article is distributed under CC BY-NC 4.0. The GPL-2 licence for
`costcomorbidity` applies only to material for which the package copyright holders
have authority to grant that licence. It does not grant additional rights in
third-party publications, mappings, data, or other materials.

`costcomorbidity` does not include a copied table of the Pratt mapping in its own
source files. Users, particularly those planning commercial use, should check
the terms that apply to the relevant third-party material and obtain any
permission that may be required. This notice is only informational.
