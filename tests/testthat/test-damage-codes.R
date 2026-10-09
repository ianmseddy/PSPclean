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

test_that("getPSP() rejects codesToExclude for sources it cannot filter, before downloading", {
  expect_error(getPSP("ON", tempdir(), codesToExclude = list(ON = "IB")), "not ON")
  expect_error(getPSP("BC", tempdir(), codesToExclude = c(BC = "IBM")), "list named by source")
  expect_error(getPSP("BC", tempdir(), codesToExclude = list("IBM")), "list named by source")
})
