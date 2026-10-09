test_that("damage codes: the caller's by source, else the forGMCS defaults", {
  expect_null(damageCodesToExclude("BC"))
  expect_null(damageCodesToExclude("NFI"))
  ## BC's mountain pine beetle code; "IMB", used before, matched no BC code
  expect_identical(damageCodesToExclude("BC", forGMCS = TRUE), "IBM")
  expect_identical(damageCodesToExclude("AB", forGMCS = TRUE), 3)
  expect_identical(damageCodesToExclude("NFI", forGMCS = TRUE), "IB")

  codes <- list(BC = c("IBM", "IBS", "IDE"), NFI = c("IB", "ID"))
  expect_identical(damageCodesToExclude("BC", codes), c("IBM", "IBS", "IDE"))
  expect_identical(damageCodesToExclude("NFI", codes, forGMCS = TRUE), c("IB", "ID"))
  ## a source the caller does not name keeps its default
  expect_identical(damageCodesToExclude("AB", codes, forGMCS = TRUE), 3)
  expect_null(damageCodesToExclude("AB", codes))
})

test_that("SK: every observation of a tree that died of an excluded cause is removed", {
  trees <- data.table::data.table(
    PLOT_ID = c(1, 1, 1, 1, 2, 2),
    TREE_NO = c(10, 10, 11, 11, 10, 10),
    YEAR = c(2000, 2010, 2000, 2010, 2000, 2010),
    MORTALITY = c(0, 3, 0, 5, 0, 0) ## plot 1 tree 10 died of insects (3), tree 11 of wind (5)
  )
  out <- removeTreesByMortality(trees, codesToExclude = 3)
  expect_identical(nrow(out), 4L)
  expect_false(any(out$PLOT_ID == 1 & out$TREE_NO == 10)) ## its live 2000 measurement goes too
  expect_true(all(c(11, 10) %in% out$TREE_NO)) ## tree 10 on plot 2 is another tree
  expect_identical(nrow(removeTreesByMortality(trees, codesToExclude = 8)), 6L)
})

test_that("getPSP() passes each source its damage codes", {
  seen <- new.env()
  capture <- function(src) {
    function(..., codesToExclude = NULL) {
      assign(src, list(codesToExclude), envir = seen) ## list(): NULL is a value here
      stop("captured")
    }
  }
  local_mocked_bindings(
    prepInputsBCPSP = function(...) list(),
    prepInputsAlbertaPSP = function(...) list(),
    prepInputsSaskatchwanPSP = function(...) list(),
    prepInputsNFIPSP = function(...) list(),
    dataPurification_BCPSP = capture("BC"),
    dataPurification_ABPSP = capture("AB"),
    dataPurification_SKPSP = capture("SK"),
    dataPurification_NFIPSP = capture("NFI")
  )
  codes <- list(BC = c("IBM", "IDE"), AB = c(1L, 3L), SK = 3L, NFI = "IB")
  for (src in names(codes)) {
    expect_error(getPSP(src, tempdir(), codesToExclude = codes), "captured")
    expect_identical(seen[[src]][[1]], codes[[src]])
  }
  ## the climate-mode defaults: BC's mountain pine beetle as "IBM" (it was passed as "IMB")
  defaults <- list(BC = "IBM", AB = 3, SK = NULL, NFI = "IB")
  for (src in names(defaults)) {
    expect_error(getPSP(src, tempdir(), forGMCS = TRUE), "captured")
    expect_identical(seen[[src]][[1]], defaults[[src]])
  }
  for (src in names(defaults)) {
    expect_error(getPSP(src, tempdir()), "captured")
    expect_null(seen[[src]][[1]])
  }
})

## one Saskatchewan plot measured in 2000 and 2010; tree 10 died of insects (3) by 2010
sk_fixture <- function() {
  list(
    SADataRaw = data.table::data.table(
      PLOT_ID = 1, YEAR = 2000, TOTAL_AGE = c(60, 62), TREE_STATUS = 1, CROWN_CLASS = 1
    ),
    plotHeaderRaw = data.table::data.table(PLOT_ID = 1, Z13nad83_e = 500000, Z13nad83_n = 6000000),
    measureHeaderRaw = data.table::data.table(PLOT_ID = 1, PLOT_SIZE = 0.08),
    treeDataRaw = data.table::data.table(
      PLOT_ID = 1,
      TREE_NO = c(10, 10, 11, 11, 12, 12),
      YEAR = c(2000, 2010, 2000, 2010, 2000, 2010),
      SPECIES = "WS",
      DBH = c(20, 21, 15, 17, 12, 14),
      HEIGHT = c(18, 18, 14, 15, 12, 13),
      TREE_STATUS = c(1, 3, 1, 1, 1, 1),
      CONDITION_CODE1 = 0,
      CONDITION_CODE2 = 0,
      CONDITION_CODE3 = 0,
      MORTALITY = c(0, 3, 0, 0, 0, 0),
      OFFICE_ERROR = ""
    ),
    sppEquiv = data.table::data.table(SK_forestry = "WS", Latin_full = "Picea glauca")
  )
}

test_that("dataPurification_SKPSP() removes every measurement of a tree that died of an excluded cause", {
  sk <- function(...) {
    f <- sk_fixture()
    dataPurification_SKPSP(f$SADataRaw, f$plotHeaderRaw, f$measureHeaderRaw, f$treeDataRaw,
                           sppEquiv = f$sppEquiv, ...)
  }
  ## without exclusions, tree 10 keeps its live 2000 measurement (dead trees are dropped anyway)
  kept <- sk()$treeData
  expect_identical(nrow(kept), 5L)
  expect_true(10 %in% kept$TreeNumber)
  ## with insects excluded it goes entirely (this stopped on the missing `treeData` before)
  out <- sk(codesToExclude = 3)$treeData
  expect_identical(nrow(out), 4L)
  expect_false(10 %in% out$TreeNumber)
  ## a cause no tree died of removes nothing
  expect_identical(nrow(sk(codesToExclude = 5)$treeData), 5L)
})

test_that("getPSP() rejects codesToExclude for sources it cannot filter, before downloading", {
  expect_error(getPSP("ON", tempdir(), codesToExclude = list(ON = "IB")), "not ON")
  expect_error(getPSP("BC", tempdir(), codesToExclude = c(BC = "IBM")), "list named by source")
  expect_error(getPSP("BC", tempdir(), codesToExclude = list("IBM")), "list named by source")
})
