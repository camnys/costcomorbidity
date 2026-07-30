# Apply comorbidity severity hierarchy
#
# Resets less severe comorbidity indicators to zero when the corresponding
# more severe category is present. This reproduces assign0 = TRUE in the
# comorbidity package.
#
# @param indicators A data frame containing one binary indicator column per
#   comorbidity category.
# @param map An internal mapping table containing the columns `indicator`,
#   `category`, and `superseded_by_category`.
#
# @return The indicator data frame after applying the hierarchy.
#
# @keywords internal
apply_hierarchy <- function(
    indicators,
    map
) {

  required_map_columns <- c(
    "indicator",
    "category",
    "superseded_by_category"
  )

  missing_map_columns <- setdiff(
    required_map_columns,
    names(map)
  )

  if (length(missing_map_columns) > 0L) {
    stop(
      "`map` is missing these columns: ",
      paste(missing_map_columns, collapse = ", "),
      call. = FALSE
    )
  }

  hierarchy_rules <- map[
    !is.na(map$superseded_by_category) &
      nzchar(map$superseded_by_category),
    ,
    drop = FALSE
  ]

  if (nrow(hierarchy_rules) == 0L) {
    return(indicators)
  }

  for (i in seq_len(nrow(hierarchy_rules))) {

    mild_indicator <- hierarchy_rules$indicator[i]

    severe_category <-
      hierarchy_rules$superseded_by_category[i]

    severe_indicator <- map$indicator[
      match(
        severe_category,
        map$category
      )
    ]

    if (is.na(severe_indicator)) {
      stop(
        "The severe category `",
        severe_category,
        "` was not found in the mapping table.",
        call. = FALSE
      )
    }

    if (!mild_indicator %in% names(indicators)) {
      stop(
        "The indicator column `",
        mild_indicator,
        "` was not found.",
        call. = FALSE
      )
    }

    if (!severe_indicator %in% names(indicators)) {
      stop(
        "The indicator column `",
        severe_indicator,
        "` was not found.",
        call. = FALSE
      )
    }

    severe_present <- (
      !is.na(indicators[[severe_indicator]]) &
        indicators[[severe_indicator]] == 1L
    )

    indicators[[mild_indicator]][
      severe_present
    ] <- 0L
  }

  indicators
}
