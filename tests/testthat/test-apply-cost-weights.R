make_zero_charlson_indicators <- function(ids) {

  result <- data.frame(
    id = ids,
    stringsAsFactors = FALSE
  )

  for (indicator_name in charlson_weights$indicator) {
    result[[indicator_name]] <- 0L
  }

  result
}


charlson_indicator_name <- function(category) {

  charlson_weights$indicator[
    match(
      category,
      charlson_weights$category
    )
  ]
}


charlson_coefficient <- function(category) {

  charlson_weights$coefficient[
    match(
      category,
      charlson_weights$category
    )
  ]
}

get_test_weights <- function(index) {

  switch(
    index,
    charlson = charlson_weights,
    elixhauser = elixhauser_weights,
    rxrisk = rxrisk_weights
  )
}


make_zero_index_indicators <- function(
    ids,
    index
) {

  weights <- get_test_weights(
    index
  )

  result <- data.frame(
    id = ids,
    stringsAsFactors = FALSE
  )

  for (indicator_name in weights$indicator) {
    result[[indicator_name]] <- 0L
  }

  result
}




test_that("one Charlson category produces its corresponding coefficient", {

  indicators <- make_zero_charlson_indicators(
    "A"
  )

  mi_indicator <- charlson_indicator_name(
    "mi"
  )

  indicators[[mi_indicator]] <- 1L

  observed <- apply_cost_weights(
    indicators = indicators
  )

  expect_equal(
    observed$costbased_charlson,
    charlson_coefficient("mi"),
    tolerance = 1e-12
  )
})


test_that("multiple Charlson coefficients are added", {

  indicators <- make_zero_charlson_indicators(
    "A"
  )

  mi_indicator <- charlson_indicator_name(
    "mi"
  )

  chf_indicator <- charlson_indicator_name(
    "chf"
  )

  indicators[[mi_indicator]] <- 1L
  indicators[[chf_indicator]] <- 1L

  observed <- apply_cost_weights(
    indicators = indicators
  )

  expected <- (
    charlson_coefficient("mi") +
      charlson_coefficient("chf")
  )

  expect_equal(
    observed$costbased_charlson,
    expected,
    tolerance = 1e-12
  )
})


test_that("a person with no Charlson categories receives zero", {

  indicators <- make_zero_charlson_indicators(
    "A"
  )

  observed <- apply_cost_weights(
    indicators = indicators
  )

  expect_equal(
    observed$costbased_charlson,
    0
  )
})


test_that("scores are calculated independently for each person", {

  indicators <- make_zero_charlson_indicators(
    c(
      "A",
      "B",
      "C"
    )
  )

  mi_indicator <- charlson_indicator_name(
    "mi"
  )

  chf_indicator <- charlson_indicator_name(
    "chf"
  )

  indicators[[mi_indicator]] <- c(
    1L,
    1L,
    0L
  )

  indicators[[chf_indicator]] <- c(
    0L,
    1L,
    0L
  )

  observed <- apply_cost_weights(
    indicators = indicators
  )

  expected <- c(
    charlson_coefficient("mi"),
    charlson_coefficient("mi") +
      charlson_coefficient("chf"),
    0
  )

  expect_equal(
    observed$costbased_charlson,
    expected,
    tolerance = 1e-12
  )
})


test_that("indicator column order does not affect the score", {

  indicators <- make_zero_charlson_indicators(
    "A"
  )

  mi_indicator <- charlson_indicator_name(
    "mi"
  )

  indicators[[mi_indicator]] <- 1L

  reordered_names <- c(
    "id",
    rev(
      charlson_weights$indicator
    )
  )

  indicators <- indicators[
    ,
    reordered_names,
    drop = FALSE
  ]

  observed <- apply_cost_weights(
    indicators = indicators
  )

  expect_equal(
    observed$costbased_charlson,
    charlson_coefficient("mi"),
    tolerance = 1e-12
  )
})


test_that("keep_indicators = FALSE returns only identifier and score", {

  indicators <- make_zero_charlson_indicators(
    "A"
  )

  observed <- apply_cost_weights(
    indicators = indicators,
    keep_indicators = FALSE
  )

  expect_named(
    observed,
    c(
      "id",
      "costbased_charlson"
    )
  )
})


test_that("missing indicator columns produce an error", {

  indicators <- data.frame(
    id = "A",
    cci_mi = 1L
  )

  expect_error(
    apply_cost_weights(
      indicators = indicators
    ),
    "missing these Charlson columns"
  )
})


test_that("non-binary indicator values produce an error", {

  indicators <- make_zero_charlson_indicators(
    "A"
  )

  first_indicator <- charlson_weights$indicator[1]

  indicators[[first_indicator]] <- 2L

  expect_error(
    apply_cost_weights(
      indicators = indicators
    ),
    "must contain only 0 and 1"
  )
})


test_that("missing indicator values produce an error", {

  indicators <- make_zero_charlson_indicators(
    "A"
  )

  first_indicator <- charlson_weights$indicator[1]

  indicators[[first_indicator]] <- NA_integer_

  expect_error(
    apply_cost_weights(
      indicators = indicators
    ),
    "contains missing values"
  )
})

test_that("Elixhauser coefficients are applied correctly", {

  indicators <- make_zero_index_indicators(
    ids = "A",
    index = "elixhauser"
  )

  selected_rows <- c(
    1L,
    2L
  )

  selected_indicators <-
    elixhauser_weights$indicator[
      selected_rows
    ]

  indicators[
    1,
    selected_indicators
  ] <- 1L

  observed <- apply_cost_weights(
    indicators = indicators,
    index = "elixhauser"
  )

  expected <- sum(
    elixhauser_weights$coefficient[
      selected_rows
    ]
  )

  expect_equal(
    observed$costbased_elixhauser,
    expected,
    tolerance = 1e-12
  )
})


test_that("RxRisk coefficients are applied correctly", {

  indicators <- make_zero_index_indicators(
    ids = "A",
    index = "rxrisk"
  )

  selected_rows <- c(
    1L,
    2L
  )

  selected_indicators <-
    rxrisk_weights$indicator[
      selected_rows
    ]

  indicators[
    1,
    selected_indicators
  ] <- 1L

  observed <- apply_cost_weights(
    indicators = indicators,
    index = "rxrisk"
  )

  expected <- sum(
    rxrisk_weights$coefficient[
      selected_rows
    ]
  )

  expect_equal(
    observed$costbased_rxrisk,
    expected,
    tolerance = 1e-12
  )
})


test_that("negative RxRisk coefficients are retained", {

  negative_row <- which(
    rxrisk_weights$coefficient < 0
  )[1]

  expect_false(
    is.na(negative_row)
  )

  indicators <- make_zero_index_indicators(
    ids = "A",
    index = "rxrisk"
  )

  negative_indicator <-
    rxrisk_weights$indicator[
      negative_row
    ]

  indicators[[negative_indicator]] <- 1L

  observed <- apply_cost_weights(
    indicators = indicators,
    index = "rxrisk"
  )

  expect_equal(
    observed$costbased_rxrisk,
    rxrisk_weights$coefficient[
      negative_row
    ],
    tolerance = 1e-12
  )

  expect_lt(
    observed$costbased_rxrisk,
    0
  )
})

test_that("score columns have index-specific names", {

  expected_score_names <- c(
    charlson = "costbased_charlson",
    elixhauser = "costbased_elixhauser",
    rxrisk = "costbased_rxrisk"
  )

  for (index_name in names(expected_score_names)) {

    indicators <- make_zero_index_indicators(
      ids = "A",
      index = index_name
    )

    observed <- apply_cost_weights(
      indicators = indicators,
      index = index_name,
      keep_indicators = FALSE
    )

    expect_named(
      observed,
      c("id",expected_score_names[[index_name]]
      )
    )
  }
})


test_that("all three indices assign zero when no categories are present", {

  for (
    index_name in c(
      "charlson",
      "elixhauser",
      "rxrisk"
    )
  ) {

    indicators <- make_zero_index_indicators(
      ids = c(
        "A",
        "B"
      ),
      index = index_name
    )

    observed <- apply_cost_weights(
      indicators = indicators,
      index = index_name,
      keep_indicators = FALSE
    )

    score_column <- paste0(
      "costbased_",
      index_name
    )

    expect_equal(
      observed[[score_column]],
      c(
        0,
        0
      )
    )
  }
})


test_that("unsupported indices produce a clear scoring error", {

  indicators <- make_zero_index_indicators(
    ids = "A",
    index = "charlson"
  )

  expect_error(
    apply_cost_weights(
      indicators = indicators,
      index = "other"
    ),
    "`index` must be one of"
  )
})
