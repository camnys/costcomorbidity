test_that("RxRisk mapping is obtained from coder", {

  observed <- get_rxrisk_map()

  expect_s3_class(
    observed,
    "data.frame"
  )

  expect_equal(
    nrow(observed),
    46L
  )

  expect_true(
    all(
      c(
        "indicator",
        "category",
        "regex",
        "raw_pattern"
      ) %in%
        names(observed)
    )
  )

  expect_false(
    anyNA(observed$indicator)
  )

  expect_false(
    anyNA(observed$category)
  )

  expect_false(
    anyNA(observed$regex)
  )

  expect_equal(
    anyDuplicated(observed$indicator),
    0L
  )

  expect_equal(
    anyDuplicated(observed$category),
    0L
  )
})


test_that("coder categories match cost-based RxRisk weights", {

  observed <- get_rxrisk_map()

  expect_true(
    setequal(
      observed$category,
      rxrisk_weights$category
    )
  )

  expect_true(
    setequal(
      observed$indicator,
      rxrisk_weights$indicator
    )
  )
})


test_that("RxRisk definition reports coder as its source", {

  observed <- get_index_definition(
    "rxrisk"
  )

  expect_true(
    all(
      observed$map$source_package ==
        "coder"
    )
  )

  expect_equal(
    observed$score_name,
    "costbased_rxrisk"
  )
})
