#' Identify comorbidity categories from clinical codes
#'
#' Converts long-format ICD-10 or ATC records into one row per person with
#' binary comorbidity indicators.
#'
#' Codes are standardised using [clean_codes()], matched against the selected
#' internal classification mapping, and collapsed so that each category is
#' coded as either zero or one per person.
#'
#' For Charlson and Elixhauser, the comorbidity severity hierarchy is applied,
#' reproducing `assign0 = TRUE` from the comorbidity package. RxRisk categories
#' are not subject to a hierarchy.
#'
#' @param people A data frame containing one row per person.
#'
#' @param codes A long-format data frame containing clinical codes. Multiple
#'   rows per person are allowed.
#'
#' @param index Character string specifying the comorbidity index. Must be one
#'   of `"charlson"`, `"elixhauser"`, or `"rxrisk"`.
#'
#' @param id Character string giving the person-identifier column in both
#'   `people` and `codes`.
#'
#' @param code Character string giving the clinical-code column in `codes`.
#'
#' @param code_system Character string giving the code-system column in
#'   `codes`. Charlson and Elixhauser use ICD-10 records, while RxRisk uses
#'   ATC records. Punctuation and letter case in code-system labels are ignored.
#'
#' @return A data frame containing the person identifier followed by one
#'   binary indicator column for each category in the selected index.
#'
#' @examples
#' people <- data.frame(
#'   id = c("A", "B")
#' )
#'
#' codes <- data.frame(
#'   id = c("A", "A"),
#'   code_system = c("ICD10", "ICD-10"),
#'   code = c("I21.0", "I50.9")
#' )
#'
#' identify_comorbidities(
#'   people = people,
#'   codes = codes,
#'   index = "charlson"
#' )
#'
#' @export
identify_comorbidities <- function(
    people,
    codes,
    index = "charlson",
    id = "id",
    code = "code",
    code_system = "code_system"
) {

  ###########################################################################
  # Select and validate the index
  ###########################################################################

  definition <- get_index_definition(
    index
  )

  map <- definition$map


  ###########################################################################
  # Validate the input objects
  ###########################################################################

  if (!is.data.frame(people)) {
    stop(
      "`people` must be a data frame.",
      call. = FALSE
    )
  }

  if (!is.data.frame(codes)) {
    stop(
      "`codes` must be a data frame.",
      call. = FALSE
    )
  }


  if (!id %in% names(people)) {
    stop(
      "`people` is missing the identifier column `",
      id,
      "`.",
      call. = FALSE
    )
  }


  required_code_columns <- c(
    id,
    code,
    code_system
  )

  missing_code_columns <- setdiff(
    required_code_columns,
    names(codes)
  )

  if (length(missing_code_columns) > 0L) {
    stop(
      "`codes` is missing these columns: ",
      paste(
        missing_code_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }


  ###########################################################################
  # Validate person identifiers
  ###########################################################################

  if (anyNA(people[[id]])) {
    stop(
      "`people` cannot contain missing person identifiers.",
      call. = FALSE
    )
  }

  if (anyDuplicated(people[[id]]) > 0L) {
    stop(
      "`people` must contain only one row per person.",
      call. = FALSE
    )
  }


  ###########################################################################
  # Establish the required code system
  ###########################################################################

  expected_code_system <- unique(
    map$code_system
  )

  if (
    length(expected_code_system) != 1L ||
    is.na(expected_code_system)
  ) {
    stop(
      "The internal mapping must contain exactly one code system.",
      call. = FALSE
    )
  }


  ###########################################################################
  # Prepare and clean the code records
  ###########################################################################

  codes_use <- codes[
    ,
    c(
      id,
      code,
      code_system
    ),
    drop = FALSE
  ]

  codes_use$.clean_code <- clean_codes(
    codes_use[[code]]
  )

  codes_use$.clean_system <- toupper(
    gsub(
      pattern = "[^A-Z0-9]",
      replacement = "",
      x = as.character(
        codes_use[[code_system]]
      )
    )
  )

  expected_code_system <- toupper(
    gsub(
      pattern = "[^A-Z0-9]",
      replacement = "",
      x = expected_code_system
    )
  )


  ###########################################################################
  # Keep relevant people and the required code system
  ###########################################################################

  keep_record <- (
    codes_use[[id]] %in% people[[id]] &
      !is.na(codes_use$.clean_code) &
      nzchar(codes_use$.clean_code) &
      !is.na(codes_use$.clean_system) &
      codes_use$.clean_system ==
      expected_code_system
  )

  codes_use <- codes_use[
    keep_record,
    c(
      id,
      ".clean_code"
    ),
    drop = FALSE
  ]


  ###########################################################################
  # Remove duplicate person-code records
  ###########################################################################

  codes_use <- unique(
    codes_use
  )


  ###########################################################################
  # Create the zero-filled result
  ###########################################################################

  result <- people[
    ,
    id,
    drop = FALSE
  ]

  for (indicator_name in map$indicator) {
    result[[indicator_name]] <- 0L
  }


  ###########################################################################
  # Match codes to every applicable category
  ###########################################################################

  if (nrow(codes_use) > 0L) {

    for (i in seq_len(nrow(map))) {

      code_matches_category <- grepl(
        pattern = map$regex[i],
        x = codes_use$.clean_code,
        perl = TRUE
      )

      if (any(code_matches_category)) {

        matched_ids <- unique(
          codes_use[[id]][
            code_matches_category
          ]
        )

        result_rows <- match(
          matched_ids,
          result[[id]]
        )

        indicator_name <- map$indicator[i]

        result[[indicator_name]][
          result_rows
        ] <- 1L
      }
    }
  }


  ###########################################################################
  # Apply the stored hierarchy
  ###########################################################################

  result <- apply_hierarchy(
    indicators = result,
    map = map
  )

  result
}
