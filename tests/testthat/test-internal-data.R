test_that("mappings have the expected number of categories", {

  rx_map <- get_rxrisk_map()

  expect_equal(
    nrow(charlson_map),
    17L
  )

  expect_equal(
    nrow(elixhauser_map),
    31L
  )

  expect_equal(
    nrow(rx_map),
    46L
  )
})


test_that("stored internal mappings have the expected categories", {

  expect_equal(
    nrow(charlson_map),
    17L
  )

  expect_equal(
    nrow(elixhauser_map),
    31L
  )
})


test_that("RxRisk mapping is obtained from coder at runtime", {

  rx_map <- get_rxrisk_map()

  expect_equal(
    nrow(rx_map),
    46L
  )
})


test_that("mapping indicators and categories are complete and unique", {

  maps <- list(
    charlson_map,
    elixhauser_map,
    get_rxrisk_map()
  )

  for (map in maps) {

    expect_false(
      anyNA(map$indicator)
    )

    expect_false(
      anyNA(map$category)
    )

    expect_false(
      anyNA(map$regex)
    )

    expect_true(
      all(nzchar(map$indicator))
    )

    expect_true(
      all(nzchar(map$category))
    )

    expect_true(
      all(nzchar(map$regex))
    )

    expect_equal(
      anyDuplicated(map$indicator),
      0L
    )

    expect_equal(
      anyDuplicated(map$category),
      0L
    )
  }
})


test_that("weight indicators and categories are complete and unique", {

  weights <- list(
    charlson_weights,
    elixhauser_weights,
    rxrisk_weights
  )

  for (weight_table in weights) {

    expect_false(
      anyNA(weight_table$indicator)
    )

    expect_false(
      anyNA(weight_table$category)
    )

    expect_equal(
      anyDuplicated(weight_table$indicator),
      0L
    )

    expect_equal(
      anyDuplicated(weight_table$category),
      0L
    )
  }
})


test_that("mapping categories match weight categories", {

  rx_map <- get_rxrisk_map()

  expect_true(
    setequal(
      charlson_map$category,
      charlson_weights$category
    )
  )

  expect_true(
    setequal(
      elixhauser_map$category,
      elixhauser_weights$category
    )
  )

  expect_true(
    setequal(
      rx_map$category,
      rxrisk_weights$category
    )
  )
})


test_that("mapping categories match weight categories", {

  rx_map <- get_rxrisk_map()

  expect_true(
    setequal(
      charlson_map$category,
      charlson_weights$category
    )
  )

  expect_true(
    setequal(
      elixhauser_map$category,
      elixhauser_weights$category
    )
  )

  expect_true(
    setequal(
      rx_map$category,
      rxrisk_weights$category
    )
  )
})


test_that("all cost-based coefficients are finite numeric values", {

  weights <- list(
    charlson_weights,
    elixhauser_weights,
    rxrisk_weights
  )

  for (weight_table in weights) {

    expect_type(
      weight_table$coefficient,
      "double"
    )

    expect_true(
      all(
        is.finite(
          weight_table$coefficient
        )
      )
    )
  }
})


test_that("coefficients are contained within their confidence intervals", {

  weights <- list(
    charlson_weights,
    elixhauser_weights,
    rxrisk_weights
  )

  for (weight_table in weights) {

    expect_true(
      all(
        weight_table$ci_lower <=
          weight_table$coefficient
      )
    )

    expect_true(
      all(
        weight_table$coefficient <=
          weight_table$ci_upper
      )
    )
  }
})


test_that("all stored regular expressions are valid", {

  maps <- list(
    charlson_map,
    elixhauser_map,
    get_rxrisk_map()
  )

  is_valid_regex <- function(pattern) {

    tryCatch(
      {
        suppressWarnings(
          grepl(
            pattern = pattern,
            x = "TESTCODE"
          )
        )

        TRUE
      },
      error = function(e) {
        FALSE
      }
    )
  }

  for (map in maps) {

    valid_regex <- vapply(
      map$regex,
      is_valid_regex,
      logical(1)
    )

    expect_true(
      all(valid_regex)
    )
  }
})
