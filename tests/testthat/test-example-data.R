test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("example_people has the expected structure", {
  expect_s3_class(example_people, "data.frame")

  expect_named(
    example_people,
    c(
      "id",
      "index_date",
      "age",
      "sex"
    )
  )

  expect_false(
    anyDuplicated(example_people$id) > 0L
  )

  expect_s3_class(
    example_people$index_date,
    "Date"
  )
})


test_that("example_codes has the expected structure", {
  expect_s3_class(example_codes, "data.frame")

  expect_named(
    example_codes,
    c(
      "id",
      "code_system",
      "code",
      "code_date"
    )
  )

  expect_true(
    all(
      example_codes$id %in%
        example_people$id
    )
  )

  expect_true(
    all(
      example_codes$code_system %in%
        c("ICD10", "ATC")
    )
  )

  expect_s3_class(
    example_codes$code_date,
    "Date"
  )
})


test_that("all example codes occur before the index date", {
  date_check <- merge(
    example_codes,
    example_people[
      c(
        "id",
        "index_date"
      )
    ],
    by = "id",
    all.x = TRUE
  )

  expect_true(
    all(
      date_check$code_date <
        date_check$index_date
    )
  )
})


test_that("at least one example person has no code records", {
  no_code_ids <- setdiff(
    example_people$id,
    example_codes$id
  )

  expect_gt(
    length(no_code_ids),
    0L
  )
})
