#' Synthetic example population
#'
#' A synthetic person-level dataset used to demonstrate the costcomorbidity
#' package. The data do not represent real individuals.
#'
#' @format A data frame with one row per synthetic person and four variables:
#' \describe{
#'   \item{id}{Synthetic person identifier.}
#'   \item{index_date}{Start date of the prediction year.}
#'   \item{age}{Age in years at the index date.}
#'   \item{sex}{Synthetic sex variable.}
#' }
#'
#' @details
#' ICD-10 diagnoses and ATC prescription records for these individuals are
#' available in [example_codes]. One individual deliberately has no code
#' records.
#'
#' @source Synthetic data generated for the costcomorbidity package.
"example_people"


#' Synthetic ICD-10 and ATC records
#'
#' A synthetic long-format dataset containing ICD-10 diagnoses and ATC
#' prescription codes for individuals in [example_people]. The records occur
#' during the year preceding the index date.
#'
#' @format A data frame with multiple rows per synthetic person and four
#' variables:
#' \describe{
#'   \item{id}{Synthetic person identifier.}
#'   \item{code_system}{Coding system, either `"ICD10"` or `"ATC"`.}
#'   \item{code}{ICD-10 diagnosis code or ATC medicine code.}
#'   \item{code_date}{Synthetic date on which the code was recorded.}
#' }
#'
#' @details
#' The data contain no real patients, identifiers, diagnoses, prescriptions,
#' or dates.
#'
#' @source Synthetic data generated for the costcomorbidity package.
"example_codes"
