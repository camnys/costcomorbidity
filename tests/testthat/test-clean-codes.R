test_that("clean_codes standardises ICD-10 and ATC codes", {

  observed <- clean_codes(
    c(
      "i21.0",
      " I50 ",
      "a10ba02",
      "J05AF-08"
    )
  )

  expected <- c(
    "I210",
    "I50",
    "A10BA02",
    "J05AF08"
  )

  expect_equal(
    observed,
    expected
  )
})


test_that("clean_codes preserves missing values", {

  expect_equal(
    clean_codes(
      c(
        "I50",
        NA_character_
      )
    ),
    c(
      "I50",
      NA_character_
    )
  )
})


test_that("clean_codes accepts factors", {

  observed <- clean_codes(
    factor(
      c(
        "i21.0",
        "a10ba02"
      )
    )
  )

  expect_equal(
    observed,
    c(
      "I210",
      "A10BA02"
    )
  )
})


test_that("clean_codes rejects numeric input", {

  expect_error(
    clean_codes(
      c(1, 2, 3)
    ),
    "`x` must be a character vector or factor"
  )
})


test_that("clean_codes handles empty input", {

  expect_equal(
    clean_codes(character(0)),
    character(0)
  )
})
