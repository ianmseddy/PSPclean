## googledrive needed in suggests b/c used by reproducible, where it's also suggested and therefore
##   not installed by default with this package
stopifnot(require("googledrive", quietly = TRUE))

standardizedPlotNames <- c(
  "MeasureID", "OrigPlotID1", "MeasureYear", "Longitude", "Latitude",
  "Zone", "Northing", "Easting", "Elevation", "PlotSize", "baseYear", "baseSA"
)
standardizedTreeNames <- c(
  "MeasureID", "OrigPlotID1", "MeasureYear", "TreeNumber", "Species",
  "DBH", "Height", "newSpeciesName"
)

test_that("PSP NFI works", {
  dPath <- reproducible::checkPath(file.path(tempdir(), "NFI"), create = TRUE)
  on.exit({
    unlink(dPath, recursive = TRUE)
  }, add = TRUE)

  nfi <- prepInputsNFIPSP(dPath = dPath)
  nfiClean <- dataPurification_NFIPSP(
    lgptreeRaw = nfi$pspTreeMeasure,
    lgpHeaderRaw = nfi$pspHeader,
    approxLocation = nfi$pspLocation,
    treeDamage = nfi$pspTreeDamage
  )

  expect_true(all(colnames(nfiClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(colnames(nfiClean$treeData) %in% standardizedTreeNames))

  nfiClean_allDam <- dataPurification_NFIPSP(
    lgptreeRaw = nfi$pspTreeMeasure,
    lgpHeaderRaw = nfi$pspHeader,
    approxLocation = nfi$pspLocation,
    treeDamage = nfi$pspTreeDamage,
    codesToExclude = NULL
  )

  # filtering should always reduce rows
  # exclude trees if damaged, but not if undamaged in prior/subsequent obs
  nfiClean_someDam <- dataPurification_NFIPSP(
    lgptreeRaw = nfi$pspTreeMeasure,
    lgpHeaderRaw = nfi$pspHeader,
    approxLocation = nfi$pspLocation,
    treeDamage = nfi$pspTreeDamage,
    excludeAllObs = FALSE
  )

  somedamN <- nrow(nfiClean_someDam$treeData)
  nodamN <- nrow(nfiClean$treeData)
  alldamN <- nrow(nfiClean_allDam$treeData)
  expect_true(somedamN > nodamN & somedamN < alldamN)
})

test_that("PSP BC works", {
  dPath <- reproducible::checkPath(file.path(tempdir(), "BC"), create = TRUE)
  on.exit({
    unlink(dPath, recursive = TRUE)
  }, add = TRUE)

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
})

test_that("PSP AB works", {
  dPath <- reproducible::checkPath(file.path(tempdir(), "AB"), create = TRUE)
  on.exit({
    unlink(dPath, recursive = TRUE)
  }, add = TRUE)

  ab <- prepInputsAlbertaPSP(dPath = dPath)
  abClean <- dataPurification_ABPSP(
    treeMeasure = ab$pspABtreeMeasure,
    plotMeasure = ab$pspABplotMeasure,
    tree = ab$pspABtree, plot = ab$pspABplot
  )

  expect_true(all(names(abClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(abClean$treeData) %in% standardizedTreeNames))

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
  dPath <- reproducible::checkPath(file.path(tempdir(), "SK"), create = TRUE)
  on.exit({
    unlink(dPath, recursive = TRUE)
  }, add = TRUE)
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
  expect_true(all(names(skmClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(skmClean$treeData) %in% standardizedTreeNames))
})


test_that("PSP ON works", {
  dPath <- reproducible::checkPath(file.path(tempdir(), "ON"), create = TRUE)
  on.exit({
    unlink(dPath, recursive = TRUE)
  }, add = TRUE)
  ON <- prepInputsOntarioPSP(dPath = dPath)

  sppEquiv <- LandR::sppEquivalencies_CA
  onClean <- dataPurification_ONPSP(ONPSPlist = ON,
                                    sppEquiv = sppEquiv)

  expect_true(all(names(onClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(onClean$treeData) %in% standardizedTreeNames))

})



test_that("geoCleanPSP works", {

  dPath <- reproducible::checkPath(file.path(tempdir(), "AB"), create = TRUE)
  on.exit({
    unlink(dPath, recursive = TRUE)
  }, add = TRUE)

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
  out3 <- geoCleanPSP(rbind(bcClean$plotHeaderData, abClean$plotHeaderData))

  #ON - has some weird NAD27 plots
  ON <- prepInputsOntarioPSP(dPath = dPath)

  sppEquiv <- LandR::sppEquivalencies_CA
  onClean <- dataPurification_ONPSP(ONPSPlist = ON,
                                    sppEquiv = sppEquiv)
  out4 <- geoCleanPSP(onClean$plotHeaderData)

  #all 3
  out5 <- geoCleanPSP(rbind(onClean$plotHeaderData,
                            bcClean$plotHeaderData,
                            abClean$plotHeaderData))

  expect_equal(names(out5), names(out))
})
