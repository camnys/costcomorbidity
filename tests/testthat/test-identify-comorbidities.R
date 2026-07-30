get_charlson_test_code <- function(category) {

  map_row <- match(
    category,
    charlson_map$category
  )

  charlson_map$code_stems[[map_row]][1]
}


get_charlson_indicator <- function(category) {

  charlson_map$indicator[
    match(
      category,
      charlson_map$category
    )
  ]
}

get_elixhauser_test_code <- function(category) {

  map_row <- match(
    category,
    elixhauser_map$category
  )

  elixhauser_map$code_stems[[map_row]][1]
}


get_elixhauser_indicator <- function(category) {

  elixhauser_map$indicator[
    match(
      category,
      elixhauser_map$category
    )
  ]
}



test_that("Charlson codes are classified correctly", {

  mi_code <- get_charlson_test_code(
    "mi"
  )

  chf_code <- get_charlson_test_code(
    "chf"
  )

  people <- data.frame(
    id = c(
      "A",
      "B",
      "C"
    )
  )

  codes <- data.frame(
    id = c(
      "A",
      "B",
      "B"
    ),
    code_system = c(
      "ICD-10",
      "ICD10",
      "icd10"
    ),
    code = c(
      tolower(
        paste0(
          mi_code,
          "."
        )
      ),
      chf_code,
      chf_code
    )
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes,
    index = "charlson"
  )

  mi_indicator <- get_charlson_indicator(
    "mi"
  )

  chf_indicator <- get_charlson_indicator(
    "chf"
  )

  expect_equal(
    observed[[mi_indicator]],
    c(
      1L,
      0L,
      0L
    )
  )

  expect_equal(
    observed[[chf_indicator]],
    c(
      0L,
      1L,
      0L
    )
  )
})


test_that("duplicate records do not increase binary indicators", {

  chf_code <- get_charlson_test_code(
    "chf"
  )

  people <- data.frame(
    id = "A"
  )

  codes <- data.frame(
    id = rep(
      "A",
      3
    ),
    code_system = rep(
      "ICD10",
      3
    ),
    code = rep(
      chf_code,
      3
    )
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes
  )

  chf_indicator <- get_charlson_indicator(
    "chf"
  )

  expect_equal(
    observed[[chf_indicator]],
    1L
  )
})


test_that("people without matching codes are retained", {

  people <- data.frame(
    id = c(
      "A",
      "B"
    )
  )

  codes <- data.frame(
    id = "A",
    code_system = "ICD10",
    code = "Z000"
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes
  )

  indicator_columns <- charlson_map$indicator

  expect_equal(
    nrow(observed),
    2L
  )

  expect_true(
    all(
      unlist(
        observed[
          ,
          indicator_columns,
          drop = FALSE
        ]
      ) == 0L
    )
  )
})


test_that("records from the wrong code system are ignored", {

  mi_code <- get_charlson_test_code(
    "mi"
  )

  people <- data.frame(
    id = "A"
  )

  codes <- data.frame(
    id = "A",
    code_system = "ATC",
    code = mi_code
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes
  )

  indicator_columns <- charlson_map$indicator

  expect_true(
    all(
      unlist(
        observed[
          ,
          indicator_columns,
          drop = FALSE
        ]
      ) == 0L
    )
  )
})


test_that("Charlson hierarchy is applied after classification", {

  mld_code <- get_charlson_test_code(
    "mld"
  )

  msld_code <- get_charlson_test_code(
    "msld"
  )

  people <- data.frame(
    id = "A"
  )

  codes <- data.frame(
    id = c(
      "A",
      "A"
    ),
    code_system = c(
      "ICD10",
      "ICD10"
    ),
    code = c(
      mld_code,
      msld_code
    )
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes
  )

  mld_indicator <- get_charlson_indicator(
    "mld"
  )

  msld_indicator <- get_charlson_indicator(
    "msld"
  )

  expect_equal(
    observed[[mld_indicator]],
    0L
  )

  expect_equal(
    observed[[msld_indicator]],
    1L
  )
})


test_that("people must have one row per identifier", {

  people <- data.frame(
    id = c(
      "A",
      "A"
    )
  )

  codes <- data.frame(
    id = character(0),
    code_system = character(0),
    code = character(0)
  )

  expect_error(
    identify_comorbidities(
      people = people,
      codes = codes
    ),
    "`people` must contain only one row per person"
  )
})


test_that("required code columns are checked", {

  people <- data.frame(
    id = "A"
  )

  codes <- data.frame(
    id = "A",
    code = "I210"
  )

  expect_error(
    identify_comorbidities(
      people = people,
      codes = codes
    ),
    "`codes` is missing these columns"
  )
})

test_that("Elixhauser ICD-10 codes are classified correctly", {

  chf_code <- get_elixhauser_test_code(
    "chf"
  )

  people <- data.frame(
    id = c(
      "A",
      "B"
    )
  )

  codes <- data.frame(
    id = "A",
    code_system = "ICD-10",
    code = tolower(
      paste0(
        chf_code,
        "."
      )
    )
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes,
    index = "elixhauser"
  )

  chf_indicator <- get_elixhauser_indicator(
    "chf"
  )

  expect_equal(
    observed[[chf_indicator]],
    c(
      1L,
      0L
    )
  )

  expect_equal(
    ncol(observed),
    32L
  )
})


test_that("Elixhauser hierarchy reproduces assign0 = TRUE", {

  hypunc_code <- get_elixhauser_test_code(
    "hypunc"
  )

  hypc_code <- get_elixhauser_test_code(
    "hypc"
  )

  people <- data.frame(
    id = "A"
  )

  codes <- data.frame(
    id = c(
      "A",
      "A"
    ),
    code_system = c(
      "ICD10",
      "ICD10"
    ),
    code = c(
      hypunc_code,
      hypc_code
    )
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes,
    index = "elixhauser"
  )

  hypunc_indicator <- get_elixhauser_indicator(
    "hypunc"
  )

  hypc_indicator <- get_elixhauser_indicator(
    "hypc"
  )

  expect_equal(
    observed[[hypunc_indicator]],
    0L
  )

  expect_equal(
    observed[[hypc_indicator]],
    1L
  )
})


test_that("RxRisk ATC codes are classified correctly", {

  rx_map <- get_rxrisk_map()

  test_atc_code <- "J05AF08"

  expected_match <- vapply(
    rx_map$regex,
    function(pattern) {
      grepl(
        pattern = pattern,
        x = test_atc_code,
        perl = TRUE
      )
    },
    logical(1)
  )

  expected_indicators <- rx_map$indicator[
    expected_match
  ]

  people <- data.frame(
    id = c(
      "A",
      "B"
    )
  )

  codes <- data.frame(
    id = "A",
    code_system = "ATC",
    code = "j05af-08"
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes,
    index = "rxrisk"
  )

  expect_true(
    all(
      observed[
        observed$id == "A",
        expected_indicators,
        drop = FALSE
      ] == 1L
    )
  )

  expect_true(
    all(
      observed[
        observed$id == "B",
        rx_map$indicator,
        drop = FALSE
      ] == 0L
    )
  )
})

test_that("one ATC code can activate multiple RxRisk categories", {

  rx_map <- get_rxrisk_map()

  test_atc_code <- "J05AF08"

  expected_match <- vapply(
    rx_map$regex,
    function(pattern) {
      grepl(
        pattern = pattern,
        x = test_atc_code,
        perl = TRUE
      )
    },
    logical(1)
  )

  expected_indicators <- rx_map$indicator[
    expected_match
  ]

  expect_gt(
    length(expected_indicators),
    1L
  )

  people <- data.frame(
    id = "A"
  )

  codes <- data.frame(
    id = "A",
    code_system = "ATC",
    code = test_atc_code
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes,
    index = "rxrisk"
  )

  expect_true(
    all(
      observed[
        1,
        expected_indicators,
        drop = FALSE
      ] == 1L
    )
  )
})


test_that("RxRisk ignores ICD-10 records", {

  rx_map <- get_rxrisk_map()

  people <- data.frame(
    id = "A"
  )

  codes <- data.frame(
    id = "A",
    code_system = "ICD10",
    code = "J05AF08"
  )

  observed <- identify_comorbidities(
    people = people,
    codes = codes,
    index = "rxrisk"
  )

  expect_true(
    all(
      observed[
        ,
        rx_map$indicator,
        drop = FALSE
      ] == 0L
    )
  )
})


test_that("all three indices retain every person", {

  indices <- c(
    "charlson",
    "elixhauser",
    "rxrisk"
  )

  for (index_name in indices) {

    observed <- identify_comorbidities(
      people = example_people,
      codes = example_codes,
      index = index_name
    )

    expect_equal(
      nrow(observed),
      nrow(example_people)
    )

    expect_equal(
      observed$id,
      example_people$id
    )
  }
})


test_that("unsupported indices produce a clear error", {

  expect_error(
    identify_comorbidities(
      people = example_people,
      codes = example_codes,
      index = "other"
    ),
    "`index` must be one of"
  )
})
