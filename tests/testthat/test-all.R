
standardizedPlotNames <- c(
  "MeasureID", "OrigPlotID1", "MeasureYear", "Longitude", "Latitude", "Datum", "source",
  "Zone", "Northing", "Easting", "Elevation", "PlotSize", "baseYear", "baseSA", "minDBH"
)
standardizedTreeNames <- c(
  "MeasureID", "OrigPlotID1", "MeasureYear", "TreeNumber", "Species", "source",
  "DBH", "Height", "PSP"
)

test_that("PSP NFI works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "NFI_")

  nfi <- prepInputsNFIPSP(dPath = dPath)
  nfiClean <- dataPurification_NFIPSP(
    NFIdata = nfi
  )

  expect_true(all(colnames(nfiClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(colnames(nfiClean$treeData) %in% standardizedTreeNames))

  notStandardizedTree <- colnames(nfiClean$treeData)
  notStandardizedTree <- notStandardizedTree[!notStandardizedTree %in% standardizedTreeNames]
  expect_true(length(notStandardizedTree) == 0)

  notStandardizedPlot <- colnames(nfiClean$plotData)
  notStandardizedPlot <- notStandardizedPlot[!notStandardizedPlot %in% standardizedPlotNames]
  expect_true(length(notStandardizedPlot) == 0)

  nfiClean_allDam <- dataPurification_NFIPSP(
    NFIdata = nfi,
    codesToExclude = NULL
  )

  # filtering should always reduce rows
  # exclude trees if damaged, but not if undamaged in prior/subsequent obs
  nfiClean_someDam <- dataPurification_NFIPSP(
    NFIdata = nfi,
    excludeAllObs = FALSE
  )

  somedamN <- nrow(nfiClean_someDam$treeData)
  nodamN <- nrow(nfiClean$treeData)
  alldamN <- nrow(nfiClean_allDam$treeData)
  expect_true(somedamN > nodamN & somedamN < alldamN)
})

test_that("PSP BC works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "BC_")

  bc <- prepInputsBCPSP(dPath = dPath)
  bcClean <- dataPurification_BCPSP(
    treeDataRaw = bc$treeDataRaw,
    plotHeaderDataRaw = bc$plotHeaderDataRaw,
    damageAgentCodes = bc$pspBCdamageAgentCodes
  )
  expect_true(all(names(bcClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(bcClean$treeData) %in% standardizedTreeNames))

  bcClean_allDam <- dataPurification_BCPSP(
    treeDataRaw = bc$treeDataRaw,
    plotHeaderDataRaw = bc$plotHeaderDataRaw,
    damageAgentCodes = bc$pspBCdamageAgentCodes,
    codesToExclude = NULL
  )
  bcClean_someDam <- dataPurification_BCPSP(
    treeDataRaw = bc$treeDataRaw,
    plotHeaderDataRaw = bc$plotHeaderDataRaw,
    damageAgentCodes = bc$pspBCdamageAgentCodes,
    excludeAllObs = FALSE
  )

  somedamN <- nrow(bcClean_someDam$treeData)
  nodamN <- nrow(bcClean$treeData)
  alldamN <- nrow(bcClean_allDam$treeData)
  expect_true(somedamN > nodamN & somedamN < alldamN)

  notStandardizedTree <- colnames(bcClean$treeData)
  notStandardizedTree <- notStandardizedTree[!notStandardizedTree %in% standardizedTreeNames]
  expect_true(length(notStandardizedTree) == 0)

  notStandardizedPlot <- colnames(bcClean$plotData)
  notStandardizedPlot <- notStandardizedPlot[!notStandardizedPlot %in% standardizedPlotNames]
  expect_true(length(notStandardizedPlot) == 0)



})

test_that("PSP AB works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "AB_")

  ab <- prepInputsAlbertaPSP(dPath = dPath)
  abClean <- dataPurification_ABPSP(
    treeMeasure = ab$pspABtreeMeasure,
    plotMeasure = ab$pspABplotMeasure,
    tree = ab$pspABtree, plot = ab$pspABplot
  )

  expect_true(all(names(abClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(abClean$treeData) %in% standardizedTreeNames))

  notStandardizedTree <- colnames(abClean$treeData)
  notStandardizedTree <- notStandardizedTree[!notStandardizedTree %in% standardizedTreeNames]
  expect_true(length(notStandardizedTree) == 0)

  notStandardizedPlot <- colnames(abClean$plotData)
  notStandardizedPlot <- notStandardizedPlot[!notStandardizedPlot %in% standardizedPlotNames]
  expect_true(length(notStandardizedPlot) == 0)


  abClean_allDam <- dataPurification_ABPSP(
    treeMeasure = ab$pspABtreeMeasure,
    plotMeasure = ab$pspABplotMeasure,
    tree = ab$pspABtree, plot = ab$pspABplot,
    codesToExclude = NULL
  )

  abClean_someDam <- dataPurification_ABPSP(
    treeMeasure = ab$pspABtreeMeasure,
    plotMeasure = ab$pspABplotMeasure,
    tree = ab$pspABtree, plot = ab$pspABplot,
    excludeAllObs = FALSE
  )

  somedamN <- nrow(abClean_someDam$treeData)
  nodamN <- nrow(abClean$treeData)
  alldamN <- nrow(abClean_allDam$treeData)
  expect_true(somedamN > nodamN & somedamN < alldamN)
})

test_that("PSP SK works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "SK_")

  sk <- prepInputsSaskatchwanPSP(dPath = dPath)

  skClean <- dataPurification_SKPSP(
    SADataRaw = sk$SADataRaw, plotHeaderRaw = sk$plotHeaderRaw,
    measureHeaderRaw = sk$measureHeaderRaw, treeDataRaw = sk$treeDataRaw
  )
  expect_true(all(names(skClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(skClean$treeData) %in% standardizedTreeNames))

  skm <- prepInputsSaskatchwanTSP(dPath = dPath)
  skmClean <- dataPurification_SKTSP_Mistik(
    compiledPlotData = skm$compiledPlotData,
    compiledTreeData = skm$compiledTreeData
  )
  missingCols <- setdiff(standardizedTreeNames, names(skmClean$treeData))
  if(length(missingCols) > 0) skmClean$treeData[, (missingCols) := NA]

  extraCols <- setdiff(names(skmClean$treeData), standardizedTreeNames)
  if(length(extraCols) > 0) skmClean$treeData[, (extraCols) := NULL]

  skmClean$treeData <- skmClean$treeData[, ..standardizedTreeNames]

  expect_true(all(names(skmClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(skmClean$treeData) %in% standardizedTreeNames))

  notStandardizedTree <- colnames(skClean$treeData)
  notStandardizedTree <- notStandardizedTree[!notStandardizedTree %in% standardizedTreeNames]
  expect_true(length(notStandardizedTree) == 0)

  notStandardizedPlot <- colnames(skClean$plotData)
  notStandardizedPlot <- notStandardizedPlot[!notStandardizedPlot %in% standardizedPlotNames]
  expect_true(length(notStandardizedPlot) == 0)
})


test_that("PSP ON works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "ON_")

  ON <- prepInputsOntarioPSP(dPath = dPath)

  sppEquiv <- LandR::sppEquivalencies_CA
  onClean <- dataPurification_ONPSP(ONPSPlist = ON,
                                    sppEquiv = sppEquiv)

  expect_true(all(names(onClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(onClean$treeData) %in% standardizedTreeNames))

  notStandardizedTree <- colnames(onClean$treeData)
  notStandardizedTree <- notStandardizedTree[!notStandardizedTree %in% standardizedTreeNames]
  expect_true(length(notStandardizedTree) == 0)

  notStandardizedPlot <- colnames(onClean$plotData)
  notStandardizedPlot <- notStandardizedPlot[!notStandardizedPlot %in% standardizedPlotNames]
  expect_true(length(notStandardizedPlot) == 0)

})


test_that("PSP QC works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "QC_")

  QC <- prepInputsQCPSP(dPath = dPath)

  sppEquiv <- LandR::sppEquivalencies_CA
  qcClean <- dataPurification_QCPSP(QuebecPSP = QC,
                                    sppEquiv = sppEquiv)

  expect_true(all(names(qcClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(qcClean$treeData) %in% standardizedTreeNames))

  notStandardizedTree <- colnames(qcClean$treeData)
  notStandardizedTree <- notStandardizedTree[!notStandardizedTree %in% standardizedTreeNames]
  expect_true(length(notStandardizedTree) == 0)

  notStandardizedPlot <- colnames(qcClean$plotData)
  notStandardizedPlot <- notStandardizedPlot[!notStandardizedPlot %in% standardizedPlotNames]
  expect_true(length(notStandardizedPlot) == 0)

})

test_that("PSP NB works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "QC_")

  NB <- prepInputsNBPSP(dPath = dPath)

  sppEquiv <- LandR::sppEquivalencies_CA
  nbClean <- dataPurification_NBPSP(NB_PSP_Data = NB,
                                    sppEquiv = sppEquiv)

  expect_true(all(names(nbClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(nbClean$treeData) %in% standardizedTreeNames))

  notStandardizedTree <- colnames(nbClean$treeData)
  notStandardizedTree <- notStandardizedTree[!notStandardizedTree %in% standardizedTreeNames]
  expect_true(length(notStandardizedTree) == 0)

  notStandardizedPlot <- colnames(nbClean$plotData)
  notStandardizedPlot <- notStandardizedPlot[!notStandardizedPlot %in% standardizedPlotNames]
  expect_true(length(notStandardizedPlot) == 0)

})


test_that("geoCleanPSP works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "geoPSP_")

  #with alberta - all lat lon
  ab <- prepInputsAlbertaPSP(dPath = dPath)
  abClean <- dataPurification_ABPSP(
    treeMeasure = ab$pspABtreeMeasure,
    plotMeasure = ab$pspABplotMeasure,
    tree = ab$pspABtree, plot = ab$pspABplot
  )

  out <- geoCleanPSP(abClean$plotHeaderData)

  #bc - all UTM
  bc <- prepInputsBCPSP(dPath = dPath)
  bcClean <- dataPurification_BCPSP(
    treeDataRaw = bc$treeDataRaw,
    plotHeaderDataRaw = bc$plotHeaderDataRaw,
    damageAgentCodes = bc$pspBCdamageAgentCodes
  )

  out2 <- geoCleanPSP(bcClean$plotHeaderData)

  #both
  out3 <- geoCleanPSP(rbind(bcClean$plotHeaderData, abClean$plotHeaderData, fill = TRUE))

  #ON - has some weird NAD27 plots
  ON <- prepInputsOntarioPSP(dPath = dPath)
  sppEquiv <- LandR::sppEquivalencies_CA
  onClean <- dataPurification_ONPSP(ONPSPlist = ON,
                                    sppEquiv = sppEquiv)
  out4 <- geoCleanPSP(onClean$plotHeaderData)

  #SK
  skm <- prepInputsSaskatchwanTSP(dPath = dPath)
  skmClean <- dataPurification_SKTSP_Mistik(
    compiledPlotData = skm$compiledPlotData,
    compiledTreeData = skm$compiledTreeData
  )
  out5 <- geoCleanPSP(skmClean$plotHeaderData)

  sk <- prepInputsSaskatchwanPSP(dPath = dPath)
  skClean <- dataPurification_SKPSP(
    SADataRaw = sk$SADataRaw, plotHeaderRaw = sk$plotHeaderRaw,
    measureHeaderRaw = sk$measureHeaderRaw, treeDataRaw = sk$treeDataRaw
  )
  out6 <- geoCleanPSP(skClean$plotHeaderData)


  nfi <- prepInputsNFIPSP(dPath = dPath)
  nfiClean <- dataPurification_NFIPSP(NFIdata = nfi)
  out7 <- geoCleanPSP(nfiClean$plotHeaderData)


  # all
  out8 <- geoCleanPSP(rbind(onClean$plotHeaderData,
    bcClean$plotHeaderData,
    abClean$plotHeaderData,
    nfiClean$plotHeaderData,
    skmClean$plotHeaderData,
    skClean$plotHeaderData,
    onClean$plotHeaderData,
    fill = TRUE
  ))
  expect_false(any(sf::st_is_empty(out8)))

  expect_equal(names(out), names(out2))
  expect_equal(names(out3), names(out4))
})


test_that("dummy PSP data works", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "dummy_")
  on.exit({
    unlink(dPath, recursive = TRUE)
  }, add = TRUE)
  dummy <- getPSP(destinationPath = dPath, PSPdataTypes = "dummy")

  expect_true(all(names(dummy$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(dummy$treeData) %in% standardizedTreeNames))

})

