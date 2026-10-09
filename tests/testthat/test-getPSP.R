local_edition(3)

test_that("damageCodesToExclude() gives the caller's codes, else the forGMCS defaults", {
  expect_null(damageCodesToExclude("BC"))
  expect_null(damageCodesToExclude("NFI"))
  expect_identical(damageCodesToExclude("BC", forGMCS = TRUE), "IBM")
  expect_identical(damageCodesToExclude("AB", forGMCS = TRUE), 3)
  expect_identical(damageCodesToExclude("NFI", forGMCS = TRUE), "IB")

  codes <- list(BC = c("IBM", "IBS", "IDE"), NFI = c("IB", "ID"))
  expect_identical(damageCodesToExclude("BC", codes), c("IBM", "IBS", "IDE"))
  expect_identical(damageCodesToExclude("NFI", codes, forGMCS = TRUE), c("IB", "ID"))
  expect_identical(damageCodesToExclude("AB", codes, forGMCS = TRUE), 3)
  expect_null(damageCodesToExclude("AB", codes))
})

test_that("getPSP() passes each source its damage codes", {
  capture <- function(..., codesToExclude = NULL) {
    stop(structure(
      list(message = "", call = NULL, codes = codesToExclude),
      class = c("psp_codes", "error", "condition")
    ))
  }
  local_mocked_bindings(
    prepInputsBCPSP = function(...) list(),
    prepInputsAlbertaPSP = function(...) list(),
    prepInputsSaskatchwanPSP = function(...) list(),
    prepInputsNFIPSP = function(...) list(),
    dataPurification_BCPSP = capture,
    dataPurification_ABPSP = capture,
    dataPurification_SKPSP = capture,
    dataPurification_NFIPSP = capture
  )
  codesFor <- function(source, ...) {
    tryCatch(getPSP(source, tempdir(), ...), psp_codes = function(cnd) cnd$codes)
  }

  codes <- list(BC = c("IBM", "IDE"), AB = c(1L, 3L), SK = 3L, NFI = "IB")
  for (source in names(codes)) {
    expect_identical(codesFor(source, codesToExclude = codes), codes[[source]])
  }
  expect_identical(codesFor("BC", forGMCS = TRUE), "IBM")
  expect_identical(codesFor("AB", forGMCS = TRUE), 3)
  expect_null(codesFor("SK", forGMCS = TRUE))
  expect_identical(codesFor("NFI", forGMCS = TRUE), "IB")
  expect_null(codesFor("BC"))
})

test_that("getPSP() rejects codesToExclude it cannot use, before downloading", {
  expect_snapshot(error = TRUE, {
    getPSP("ON", tempdir(), codesToExclude = list(ON = "IB"))
  })
  expect_snapshot(error = TRUE, {
    getPSP("BC", tempdir(), codesToExclude = c(BC = "IBM"))
  })
  expect_snapshot(error = TRUE, {
    getPSP("BC", tempdir(), codesToExclude = list("IBM"))
  })
})
