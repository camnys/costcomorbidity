#' Standardise ICD-10 and ATC codes
#'
#' Converts ICD-10 or ATC codes to uppercase and removes spaces,
#' punctuation, and other non-alphanumeric characters.
#'
#' @param x A character vector or factor containing ICD-10 or ATC codes.
#'
#' @return A character vector containing standardised codes.
#'
#' @examples
#' clean_codes(
#'   c(
#'     "i21.0",
#'     " I50 ",
#'     "a10ba02"
#'   )
#' )
#'
#' @export
clean_codes <- function(x) {

  if (
    !is.character(x) &&
    !is.factor(x)
  ) {
    stop(
      "`x` must be a character vector or factor.",
      call. = FALSE
    )
  }

  x <- as.character(x)
  x <- toupper(x)

  gsub(
    pattern = "[^A-Z0-9]",
    replacement = "",
    x = x
  )
}
