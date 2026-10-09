local_edition(3)

## one Saskatchewan plot measured in 2000 and 2010; tree 10 died of insects (3) by 2010
sk_fixture <- function() {
  list(
    SADataRaw = data.table::data.table(
      PLOT_ID = 1,
      YEAR = 2000,
      TOTAL_AGE = c(60, 62),
      TREE_STATUS = 1,
      CROWN_CLASS = 1
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

test_that("removeTreesByMortality() removes every observation of a tree that died of a listed cause", {
  trees <- data.table::data.table(
    PLOT_ID = c(1, 1, 1, 1, 2, 2),
    TREE_NO = c(10, 10, 11, 11, 10, 10),
    YEAR = c(2000, 2010, 2000, 2010, 2000, 2010),
    MORTALITY = c(0, 3, 0, 5, 0, 0)
  )
  out <- removeTreesByMortality(trees, codesToExclude = 3)
  expect_identical(paste(out$PLOT_ID, out$TREE_NO), c("1 11", "1 11", "2 10", "2 10"))
  expect_identical(nrow(removeTreesByMortality(trees, codesToExclude = 8)), 6L)
})

test_that("dataPurification_SKPSP() removes every measurement of a tree that died of an excluded cause", {
  sk <- function(...) {
    f <- sk_fixture()
    dataPurification_SKPSP(
      f$SADataRaw,
      f$plotHeaderRaw,
      f$measureHeaderRaw,
      f$treeDataRaw,
      sppEquiv = f$sppEquiv,
      ...
    )
  }
  kept <- sk()$treeData
  expect_identical(nrow(kept), 5L)
  expect_identical(sort(unique(kept$TreeNumber)), c(10, 11, 12))

  out <- sk(codesToExclude = 3)$treeData
  expect_identical(nrow(out), 4L)
  expect_identical(sort(unique(out$TreeNumber)), c(11, 12))

  expect_identical(nrow(sk(codesToExclude = 5)$treeData), 5L)
})
