#' Apply cost-based comorbidity weights
#'
#' Calculates a cost-based comorbidity score from binary Charlson,
#' Elixhauser, or RxRisk indicators.
#'
#' Each binary indicator is multiplied by its corresponding cost-based
#' coefficient, and the weighted values are summed for each person.
#'
#' @param indicators A data frame containing one row per person and one binary
#'   column for each category in the selected index.
#'
#' @param index Character string specifying the comorbidity index. Must be one
#'   of `"charlson"`, `"elixhauser"`, or `"rxrisk"`.
#'
#' @param id Character string giving the person-identifier column in
#'   `indicators`.
#'
#' @param keep_indicators Logical. If `TRUE`, the binary indicators are
#'   retained. If `FALSE`, only the identifier and score are returned.
#'
#' @return A data frame containing the identifier, optionally the binary
#'   indicators, and one of `costbased_charlson`, `costbased_elixhauser`, or
#'   `costbased_rxrisk`.
#'
#' @examples
#' charlson_indicators <- identify_comorbidities(
#'   people = example_people,
#'   codes = example_codes,
#'   index = "charlson"
#' )
#'
#' charlson_scores <- apply_cost_weights(
#'   indicators = charlson_indicators,
#'   index = "charlson",
#'   keep_indicators = FALSE
#' )
#'
#' head(charlson_scores)
#'
#' @export
apply_cost_weights <- function(
    indicators,
    index = "charlson",
    id = "id",
    keep_indicators = TRUE
) {

  ###########################################################################
  # Select and validate the index
  ###########################################################################

  definition <- get_index_definition(
    index
  )

  weights <- definition$weights
  score_name <- definition$score_name


  ###########################################################################
  # Validate arguments
  ###########################################################################

  if (!is.data.frame(indicators)) {
    stop(
      "`indicators` must be a data frame.",
      call. = FALSE
    )
  }

  if (
    !is.character(id) ||
    length(id) != 1L ||
    is.na(id) ||
    !nzchar(id)
  ) {
    stop(
      "`id` must be one non-missing character string.",
      call. = FALSE
    )
  }

  if (
    !is.logical(keep_indicators) ||
    length(keep_indicators) != 1L ||
    is.na(keep_indicators)
  ) {
    stop(
      "`keep_indicators` must be either TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!id %in% names(indicators)) {
    stop(
      "`indicators` does not contain the identifier column `",
      id,
      "`.",
      call. = FALSE
    )
  }

  if (anyNA(indicators[[id]])) {
    stop(
      "`indicators` cannot contain missing person identifiers.",
      call. = FALSE
    )
  }

  if (anyDuplicated(indicators[[id]]) > 0L) {
    stop(
      "`indicators` must contain only one row per person.",
      call. = FALSE
    )
  }


  ###########################################################################
  # Check that all required indicator columns are present
  ###########################################################################

  required_indicators <- weights$indicator

  missing_indicators <- setdiff(
    required_indicators,
    names(indicators)
  )

  if (length(missing_indicators) > 0L) {
    stop(
      "`indicators` is missing these ",
      definition$label,
      " columns: ",
      paste(
        missing_indicators,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }


  ###########################################################################
  # Validate binary indicator columns
  ###########################################################################

  indicator_data <- indicators[
    ,
    required_indicators,
    drop = FALSE
  ]

  for (indicator_name in required_indicators) {

    values <- indicator_data[[indicator_name]]

    if (
      !is.numeric(values) &&
      !is.logical(values)
    ) {
      stop(
        "The indicator `",
        indicator_name,
        "` must be numeric, integer, or logical.",
        call. = FALSE
      )
    }

    if (anyNA(values)) {
      stop(
        "The indicator `",
        indicator_name,
        "` contains missing values.",
        call. = FALSE
      )
    }

    if (!all(values %in% c(0, 1))) {
      stop(
        "The indicator `",
        indicator_name,
        "` must contain only 0 and 1.",
        call. = FALSE
      )
    }
  }


  ###########################################################################
  # Calculate the weighted score
  ###########################################################################

  indicator_matrix <- as.matrix(
    indicator_data
  )

  storage.mode(
    indicator_matrix
  ) <- "double"

  coefficients <- weights$coefficient[
    match(
      colnames(indicator_matrix),
      weights$indicator
    )
  ]

  score <- drop(
    indicator_matrix %*%
      coefficients
  )


  ###########################################################################
  # Construct the result
  ###########################################################################

  if (keep_indicators) {

    result <- indicators[
      ,
      c(
        id,
        required_indicators
      ),
      drop = FALSE
    ]

  } else {

    result <- indicators[
      ,
      id,
      drop = FALSE
    ]
  }

  result[[score_name]] <- score

  result
}
