test_that("Charlson hierarchy reproduces assign0 = TRUE", {

  indicators <- data.frame(
    id = c("A", "B", "C"),
    cci_mld = c(1L, 1L, 0L),
    cci_msld = c(0L, 1L, 1L),
    cci_diab = c(1L, 1L, 0L),
    cci_diabwc = c(0L, 1L, 1L),
    cci_canc = c(1L, 1L, 0L),
    cci_metacanc = c(0L, 1L, 1L)
  )

  observed <- apply_hierarchy(
    indicators,
    charlson_map
  )

  expect_equal(
    observed$cci_mld,
    c(1L, 0L, 0L)
  )

  expect_equal(
    observed$cci_diab,
    c(1L, 0L, 0L)
  )

  expect_equal(
    observed$cci_canc,
    c(1L, 0L, 0L)
  )

  expect_equal(
    observed$cci_msld,
    indicators$cci_msld
  )

  expect_equal(
    observed$cci_diabwc,
    indicators$cci_diabwc
  )

  expect_equal(
    observed$cci_metacanc,
    indicators$cci_metacanc
  )
})


test_that("Elixhauser hierarchy reproduces assign0 = TRUE", {

  indicators <- data.frame(
    id = c("A", "B"),
    eci_hypunc = c(1L, 1L),
    eci_hypc = c(0L, 1L),
    eci_diabunc = c(1L, 1L),
    eci_diabc = c(0L, 1L),
    eci_solidtum = c(1L, 1L),
    eci_metacanc = c(0L, 1L)
  )

  observed <- apply_hierarchy(
    indicators,
    elixhauser_map
  )

  expect_equal(
    observed$eci_hypunc,
    c(1L, 0L)
  )

  expect_equal(
    observed$eci_diabunc,
    c(1L, 0L)
  )

  expect_equal(
    observed$eci_solidtum,
    c(1L, 0L)
  )
})


test_that("RxRisk is unchanged because it has no hierarchy", {

  indicators <- data.frame(
    id = c("A", "B"),
    rx_cn__ = c(1L, 0L),
    rx_hpt_ = c(0L, 1L)
  )

  observed <- apply_hierarchy(
    indicators,
    get_rxrisk_map()
  )

  expect_equal(
    observed,
    indicators
  )
})
