test_that("PSPclean works", {
  mainDir <- tempdir()

  ab <- prepInputsAlbertaPSP(dPath = mainDir)
  bc <- prepInputsBCPSP(dPath = mainDir)
  nfi <- prepInputsNFIPSP(dPath = mainDir)
  sk <- prepInputsSaskatchwanPSP(dPath = mainDir)
  skm <- prepInputsSaskatchwanTSP(dPath = mainDir)

  #don't be NULL, basically
  expect_true(class(bc$plotHeaderDataRaw)[[1]] == "data.table")
  expect_true(class(ab$pspABtreeMeasure)[1] == "data.table")
  expect_true(class(nfi$pspNFIHeaderRaw)[1] == "data.table")
  expect_true(class(sk$measureHeaderRaw)[1] == "data.table")
  expect_true(class(skm$compiledPlotData)[1] == "data.table")


  standardizedColNames <- c("MeasureID", "OrigPlotID1", "MeasureYear","Longitude", "Latitude",
                            "Elevation", "PlotSize", "baseYear", "baseSA")

  abClean <- dataPurification_ABPSP(treeMeasure = ab$pspABtreeMeasure,
                                    plotMeasure = ab$pspABplotMeasure,
                                    tree =  ab$pspABtree,
                                    plot = ab$pspABplot)
  expect_true(all(names(abClean$plotHeaderData) %in% standardizedColNames))

  bcClean <- dataPurification_BCPSP(treeDataRaw = bc$treeDataRaw,
                                    plotHeaderDataRaw = bc$plotHeaderDataRaw,
                                    damageAgentCodes = bc$pspBCdamageAgentCodes)
  expect_true(all(names(bcClean$plotHeaderData) %in% standardizedColNames))

  unlink(mainDir, recursive = TRUE)
})



