###############################################################################
# CREATE INTERNAL MAPPINGS AND COST-BASED WEIGHTS
#
# Creates:
#   charlson_map
#   elixhauser_map
#   charlson_weights
#   elixhauser_weights
#   rxrisk_weights
#
# These objects are stored together in:
#   R/sysdata.rda
#
# {comorbidity} and {coder} are needed only to generate these internal data.
# They will not be dependencies of costcomorbidity.
###############################################################################


###############################################################################
# 0. PROJECT AND PACKAGE CHECKS
###############################################################################

if (!file.exists("DESCRIPTION")) {
  stop(
    paste(
      "No DESCRIPTION file was found.",
      "Open costcomorbidity.Rproj and run this script from the package root."
    )
  )
}

package_name <- unname(
  read.dcf(
    "DESCRIPTION",
    fields = "Package"
  )[1, 1]
)

if (!identical(package_name, "costcomorbidity")) {
  stop(
    "This script must be run from the costcomorbidity package project."
  )
}


required_packages <- c(
  "comorbidity",
  "coder",
  "usethis"
)

missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0L) {
  stop(
    "Install these packages before continuing: ",
    paste(missing_packages, collapse = ", ")
  )
}


###############################################################################
# 1. READ THE FINAL COST-BASED COEFFICIENTS
###############################################################################

weights_path <- file.path(
  "data-raw",
  "costcomorbidity_weights.csv"
)

if (!file.exists(weights_path)) {
  stop(
    "The coefficient file was not found at: ",
    weights_path
  )
}

weights_all <- read.csv(
  weights_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

names(weights_all)[
  names(weights_all) == "mapping"
] <- "source_mapping"

names(weights_all)[
  names(weights_all) == "package_source"
] <- "source_package"


required_weight_columns <- c(
  "index",
  "indicator",
  "category",
  "coefficient",
  "ci_lower",
  "ci_upper"
)

missing_weight_columns <- setdiff(
  required_weight_columns,
  names(weights_all)
)

if (length(missing_weight_columns) > 0L) {
  stop(
    "The coefficient file is missing these columns: ",
    paste(missing_weight_columns, collapse = ", ")
  )
}


# Ensure key text variables are character.
weights_all$index <- as.character(
  weights_all$index
)

weights_all$indicator <- as.character(
  weights_all$indicator
)

weights_all$category <- as.character(
  weights_all$category
)


# Ensure coefficient variables are numeric.
numeric_weight_columns <- c(
  "coefficient",
  "ci_lower",
  "ci_upper"
)

for (variable in numeric_weight_columns) {
  weights_all[[variable]] <- as.numeric(
    weights_all[[variable]]
  )
}


###############################################################################
# 2. CREATE THE THREE WEIGHT OBJECTS
###############################################################################

charlson_weights <- weights_all[
  weights_all$index == "charlson",
  ,
  drop = FALSE
]

elixhauser_weights <- weights_all[
  weights_all$index == "elixhauser",
  ,
  drop = FALSE
]

rxrisk_weights <- weights_all[
  weights_all$index == "rxrisk",
  ,
  drop = FALSE
]


rownames(charlson_weights) <- NULL
rownames(elixhauser_weights) <- NULL
rownames(rxrisk_weights) <- NULL


validate_weights <- function(
    weights,
    expected_n,
    index_name
) {

  if (nrow(weights) != expected_n) {
    stop(
      index_name,
      ": expected ",
      expected_n,
      " weight rows, but found ",
      nrow(weights),
      "."
    )
  }

  if (anyNA(weights$indicator)) {
    stop(
      index_name,
      ": at least one indicator is missing."
    )
  }

  if (anyNA(weights$category)) {
    stop(
      index_name,
      ": at least one category is missing."
    )
  }

  if (anyDuplicated(weights$indicator)) {
    stop(
      index_name,
      ": duplicated indicator names were found."
    )
  }

  if (anyDuplicated(weights$category)) {
    stop(
      index_name,
      ": duplicated category names were found."
    )
  }

  if (
    anyNA(weights$coefficient) ||
    any(!is.finite(weights$coefficient))
  ) {
    stop(
      index_name,
      ": coefficients must all be finite numeric values."
    )
  }

  coefficient_outside_ci <- (
    weights$coefficient < weights$ci_lower |
      weights$coefficient > weights$ci_upper
  )

  if (any(coefficient_outside_ci)) {
    print(
      weights[
        coefficient_outside_ci,
        c(
          "indicator",
          "coefficient",
          "ci_lower",
          "ci_upper"
        )
      ]
    )

    stop(
      index_name,
      ": at least one coefficient lies outside its confidence interval."
    )
  }

  invisible(TRUE)
}


validate_weights(
  charlson_weights,
  expected_n = 17L,
  index_name = "Charlson"
)

validate_weights(
  elixhauser_weights,
  expected_n = 31L,
  index_name = "Elixhauser"
)

validate_weights(
  rxrisk_weights,
  expected_n = 46L,
  index_name = "RxRisk"
)


###############################################################################
# 3. EXTRACT THE COMPLETE CHARLSON AND ELIXHAUSER ICD-10 MAPPINGS
###############################################################################

# {comorbidity} stores its complete mappings in an internal named list.
#
# Each category contains the ICD code stems used by the original
# charlson_icd10_quan or elixhauser_icd10_quan classification.
comorbidity_maps <- getFromNamespace(
  ".maps",
  "comorbidity"
)

# This is the same helper used internally by {comorbidity} to turn a vector
# of code stems into one start-anchored regular expression.
codes_to_regex <- getFromNamespace(
  ".codes_to_regex",
  "comorbidity"
)


if (
  !"charlson_icd10_quan" %in%
  names(comorbidity_maps)
) {
  stop(
    "charlson_icd10_quan was not found in the installed comorbidity package."
  )
}

if (
  !"elixhauser_icd10_quan" %in%
  names(comorbidity_maps)
) {
  stop(
    "elixhauser_icd10_quan was not found in the installed comorbidity package."
  )
}


charlson_source <- comorbidity_maps[["charlson_icd10_quan"]]

elixhauser_source <- comorbidity_maps[["elixhauser_icd10_quan"]]


###############################################################################
# 4. FUNCTION TO CREATE AN ICD-10 MAPPING TABLE
###############################################################################

make_icd10_map <- function(
    source_map,
    weights,
    index_name,
    source_mapping,
    hierarchy
) {

  if (is.null(names(source_map))) {
    stop(
      source_mapping,
      " does not have named categories."
    )
  }

  category <- names(source_map)

  regex <- vapply(
    source_map,
    codes_to_regex,
    character(1)
  )

  # Match each map category to its exact analysis/weight indicator.
  indicator <- weights$indicator[
    match(
      category,
      weights$category
    )
  ]

  superseded_by_category <- unname(
    hierarchy[
      category
    ]
  )

  result <- data.frame(
    index = index_name,
    indicator = indicator,
    category = category,
    code_system = "ICD10",
    regex = unname(regex),
    superseded_by_category =
      superseded_by_category,
    source_mapping = source_mapping,
    source_package = "comorbidity",
    source_version = as.character(
      utils::packageVersion(
        "comorbidity"
      )
    ),
    stringsAsFactors = FALSE
  )

  # Preserve the original code stems as a list column for transparency.
  result$code_stems <- I(
    unname(source_map)
  )

  result <- result[
    c(
      "index",
      "indicator",
      "category",
      "code_system",
      "regex",
      "code_stems",
      "superseded_by_category",
      "source_mapping",
      "source_package",
      "source_version"
    )
  ]

  result
}


###############################################################################
# 5. DEFINE THE OPTIONAL SEVERITY HIERARCHIES
###############################################################################

# If hierarchy is applied:
#
#   mld      is removed when msld is present
#   diab     is removed when diabwc is present
#   canc     is removed when metacanc is present
charlson_hierarchy <- c(
  mld = "msld",
  diab = "diabwc",
  canc = "metacanc"
)


# If hierarchy is applied:
#
#   hypunc   is removed when hypc is present
#   diabunc  is removed when diabc is present
#   solidtum is removed when metacanc is present
elixhauser_hierarchy <- c(
  hypunc = "hypc",
  diabunc = "diabc",
  solidtum = "metacanc"
)


###############################################################################
# 6. CREATE charlson_map AND elixhauser_map
###############################################################################

charlson_map <- make_icd10_map(
  source_map = charlson_source,
  weights = charlson_weights,
  index_name = "charlson",
  source_mapping = "charlson_icd10_quan",
  hierarchy = charlson_hierarchy
)


elixhauser_map <- make_icd10_map(
  source_map = elixhauser_source,
  weights = elixhauser_weights,
  index_name = "elixhauser",
  source_mapping = "elixhauser_icd10_quan",
  hierarchy = elixhauser_hierarchy
)


###############################################################################
# 7. CREATE THE COMPLETE RXRISK PRATT ATC MAPPING
###############################################################################

rxrisk_source <- coder::rxriskv |>
  as.data.frame(
    stringsAsFactors = FALSE
  )


required_rxrisk_columns <- c(
  "group",
  "atc_pratt"
)

missing_rxrisk_columns <- setdiff(
  required_rxrisk_columns,
  names(rxrisk_source)
)

if (length(missing_rxrisk_columns) > 0L) {
  stop(
    "coder::rxriskv is missing these columns: ",
    paste(missing_rxrisk_columns, collapse = ", ")
  )
}


anchor_regex <- function(x) {

  ifelse(
    grepl(
      pattern = "^\\^",
      x = x
    ),
    x,
    paste0(
      "^(",
      x,
      ")"
    )
  )
}


rxrisk_category <- as.character(
  rxrisk_source$group
)

rxrisk_indicator <- rxrisk_weights$indicator[
  match(
    rxrisk_category,
    rxrisk_weights$category
  )
]


###############################################################################
# 8. VALIDATE MAP-WEIGHT MATCHING
###############################################################################

validate_map <- function(
    map,
    weights,
    expected_n,
    index_name
) {

  if (nrow(map) != expected_n) {
    stop(
      index_name,
      ": expected ",
      expected_n,
      " mapping rows, but found ",
      nrow(map),
      "."
    )
  }

  if (anyNA(map$indicator)) {
    unmatched <- map[
      is.na(map$indicator),
      c(
        "category",
        "source_mapping"
      ),
      drop = FALSE
    ]

    print(unmatched)

    stop(
      index_name,
      ": at least one mapping category did not match the coefficient table."
    )
  }

  if (
    anyNA(map$regex) ||
    any(trimws(map$regex) == "")
  ) {
    stop(
      index_name,
      ": at least one regular expression is missing."
    )
  }

  if (anyDuplicated(map$indicator)) {
    stop(
      index_name,
      ": duplicated indicator names were found in the map."
    )
  }

  if (anyDuplicated(map$category)) {
    stop(
      index_name,
      ": duplicated category names were found in the map."
    )
  }

  if (
    !setequal(
      map$indicator,
      weights$indicator
    )
  ) {
    stop(
      index_name,
      ": map indicators do not match weight indicators."
    )
  }

  if (
    !setequal(
      map$category,
      weights$category
    )
  ) {
    stop(
      index_name,
      ": map categories do not match weight categories."
    )
  }

  invisible(TRUE)
}


validate_map(
  charlson_map,
  charlson_weights,
  expected_n = 17L,
  index_name = "Charlson"
)

validate_map(
  elixhauser_map,
  elixhauser_weights,
  expected_n = 31L,
  index_name = "Elixhauser"
)


###############################################################################
# 9. VALIDATE USING THE REPRESENTATIVE EXAMPLE CODES
###############################################################################

example_lookup_path <- file.path(
  "data-raw",
  "example_code_lookup.csv"
)

if (!file.exists(example_lookup_path)) {
  stop(
    "The example-code lookup was not found at: ",
    example_lookup_path
  )
}


example_lookup <- read.csv(
  example_lookup_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)


required_example_columns <- c(
  "index",
  "indicator",
  "example_code"
)

missing_example_columns <- setdiff(
  required_example_columns,
  names(example_lookup)
)

if (length(missing_example_columns) > 0L) {
  stop(
    "example_code_lookup.csv is missing these columns: ",
    paste(missing_example_columns, collapse = ", ")
  )
}


clean_validation_code <- function(x) {

  x <- toupper(
    as.character(x)
  )

  gsub(
    pattern = "[^A-Z0-9]",
    replacement = "",
    x = x
  )
}


validate_example_codes <- function(
    map,
    index_name
) {

  examples <- example_lookup[
    example_lookup$index == index_name,
    ,
    drop = FALSE
  ]

  map_row <- match(
    examples$indicator,
    map$indicator
  )

  if (anyNA(map_row)) {
    stop(
      index_name,
      ": some example indicators were absent from the map."
    )
  }

  validation_passed <- mapply(
    FUN = function(pattern, code) {

      grepl(
        pattern = pattern,
        x = clean_validation_code(code)
      )
    },
    pattern = map$regex[map_row],
    code = examples$example_code,
    USE.NAMES = FALSE
  )

  if (!all(validation_passed)) {

    failed <- examples[
      !validation_passed,
      c(
        "indicator",
        "example_code"
      ),
      drop = FALSE
    ]

    failed$regex <- map$regex[
      map_row[
        !validation_passed
      ]
    ]

    print(failed)

    stop(
      index_name,
      ": at least one representative code did not match its internal map."
    )
  }

  message(
    index_name,
    ": all ",
    nrow(examples),
    " representative codes passed."
  )

  invisible(TRUE)
}


validate_example_codes(
  charlson_map,
  "charlson"
)

validate_example_codes(
  elixhauser_map,
  "elixhauser"
)


###############################################################################
# 10. DISPLAY A FINAL SUMMARY
###############################################################################

mapping_summary <- data.frame(
  index = c(
    "charlson",
    "elixhauser",
    "rxrisk"
  ),
  mapping_rows = c(
    nrow(charlson_map),
    nrow(elixhauser_map),
    nrow(
      as.data.frame(
        coder::rxriskv
      )
    )
  ),
  weight_rows = c(
    nrow(charlson_weights),
    nrow(elixhauser_weights),
    nrow(rxrisk_weights)
  ),
  mapping_storage = c(
    "internal",
    "internal",
    "runtime dependency"
  ),
  stringsAsFactors = FALSE
)

print(mapping_summary)


###############################################################################
# 11. SAVE ALL SIX OBJECTS AS INTERNAL PACKAGE DATA
###############################################################################

usethis::use_data(
  charlson_map,
  elixhauser_map,
  charlson_weights,
  elixhauser_weights,
  rxrisk_weights,
  internal = TRUE,
  overwrite = TRUE
)

message(
  paste(
    "Finished successfully.",
    "The mappings and weights were saved to R/sysdata.rda."
  )
)
