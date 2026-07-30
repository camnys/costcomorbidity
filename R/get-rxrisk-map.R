# Construct the RxRisk Pratt mapping
#
# Retrieves the Pratt ATC classification from the installed coder package
# and converts it to the internal mapping format used by costcomorbidity.
#
# @return An internal data frame containing the RxRisk mapping.
#
# @keywords internal
get_rxrisk_map <- function() {

  source <- as.data.frame(
    coder::rxriskv,
    stringsAsFactors = FALSE
  )

  required_columns <- c(
    "group",
    "atc_pratt"
  )

  missing_columns <- setdiff(
    required_columns,
    names(source)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "coder::rxriskv is missing these required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  category <- as.character(
    source$group
  )

  indicator <- rxrisk_weights$indicator[
    match(
      category,
      rxrisk_weights$category
    )
  ]

  if (anyNA(indicator)) {

    unmatched_categories <- category[
      is.na(indicator)
    ]

    stop(
      "The following coder RxRisk categories do not match the ",
      "costcomorbidity coefficient table: ",
      paste(
        unmatched_categories,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  raw_pattern <- as.character(
    source$atc_pratt
  )

  regex <- ifelse(
    grepl(
      pattern = "^\\^",
      x = raw_pattern
    ),
    raw_pattern,
    paste0(
      "^(",
      raw_pattern,
      ")"
    )
  )

  result <- data.frame(
    index = "rxrisk",
    indicator = indicator,
    category = category,
    code_system = "ATC",
    regex = regex,
    raw_pattern = raw_pattern,
    superseded_by_category = NA_character_,
    source_mapping = "atc_pratt",
    source_package = "coder",
    source_version = as.character(
      utils::packageVersion(
        "coder"
      )
    ),
    stringsAsFactors = FALSE
  )

  if (nrow(result) != 46L) {
    stop(
      "Expected 46 RxRisk categories from coder::rxriskv, but found ",
      nrow(result),
      ".",
      call. = FALSE
    )
  }

  if (anyDuplicated(result$category)) {
    stop(
      "Duplicated RxRisk categories were found in coder::rxriskv.",
      call. = FALSE
    )
  }

  if (anyDuplicated(result$indicator)) {
    stop(
      "Duplicated RxRisk indicator names were created.",
      call. = FALSE
    )
  }

  result
}
