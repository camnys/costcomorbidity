# Return the internal objects associated with an index
#
# This is an internal package helper. It is not exported.
get_index_definition <- function(index = "charlson") {

  if (
    !is.character(index) ||
    length(index) != 1L ||
    is.na(index) ||
    !nzchar(trimws(index))
  ) {
    stop(
      "`index` must be one non-missing character string.",
      call. = FALSE
    )
  }

  index <- tolower(
    trimws(index)
  )

  supported_indices <- c(
    "charlson",
    "elixhauser",
    "rxrisk"
  )

  if (!index %in% supported_indices) {
    stop(
      "`index` must be one of: ",
      paste(
        supported_indices,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  map <- switch(
    index,
    charlson = charlson_map,
    elixhauser = elixhauser_map,
    rxrisk = get_rxrisk_map()
  )

  weights <- switch(
    index,
    charlson = charlson_weights,
    elixhauser = elixhauser_weights,
    rxrisk = rxrisk_weights
  )

  label <- switch(
    index,
    charlson = "Charlson",
    elixhauser = "Elixhauser",
    rxrisk = "RxRisk"
  )

  list(
    index = index,
    label = label,
    map = map,
    weights = weights,
    score_name = paste0(
      "costbased_",
      index
    )
  )
}
