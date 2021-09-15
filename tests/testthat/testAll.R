
mainDir <- tempdir()
standardizedPlotNames <- c("MeasureID", "OrigPlotID1", "MeasureYear","Longitude", "Latitude",
                           "Zone", "Northing", "Easting",
                           "Elevation", "PlotSize", "baseYear", "baseSA")

standardizedTreeNames <- c("MeasureID", "OrigPlotID1", "MeasureYear", "TreeNumber", "Species", "DBH", "Height", "newSpeciesName")

test_that("PSP NFI works", {

  nfi <- prepInputsNFIPSP(dPath = mainDir)
  nfiClean <- dataPurification_NFIPSP(lgptreeRaw = nfi$pspTreeMeasure,
                                      lgpHeaderRaw = nfi$pspHeader,
                                      approxLocation = nfi$pspLocation,
                                      treeDamage = nfi$pspTreeDamage)

  expect_true(all(colnames(nfiClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(colnames(nfiClean$treeData) %in% standardizedTreeNames))



  nfiClean_allDam <- dataPurification_NFIPSP(lgptreeRaw = nfi$pspTreeMeasure,
                                            lgpHeaderRaw = nfi$pspHeader,
                                            approxLocation = nfi$pspLocation,
                                            treeDamage = nfi$pspTreeDamage,
                                            codesToExclude = NULL)

  #filtering should always reduce rows
  #exclude trees if damaged, but not if undamaged in prior/subsequent obs
  nfiClean_someDam <- dataPurification_NFIPSP(lgptreeRaw = nfi$pspTreeMeasure,
                                              lgpHeaderRaw = nfi$pspHeader,
                                              approxLocation = nfi$pspLocation,
                                              treeDamage = nfi$pspTreeDamage,
                                              excludeAllObs = FALSE)

  somedamN <- nrow(nfiClean_someDam$treeData)
  nodamN <- nrow(nfiClean$treeData)
  alldamN <- nrow(nfiClean_allDam$treeData)
  expect_true(somedamN > nodamN & somedamN < alldamN)

  unlink(mainDir, recursive = TRUE)
})

test_that("PSP BC works", {


  bc <- prepInputsBCPSP(dPath = mainDir)
  bcClean <- dataPurification_BCPSP(treeDataRaw = bc$treeDataRaw,
                                    plotHeaderDataRaw = bc$plotHeaderDataRaw,
                                    damageAgentCodes = bc$pspBCdamageAgentCodes)
  expect_true(all(names(bcClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(bcClean$treeData) %in% standardizedTreeNames))

  bcClean_allDam <- dataPurification_BCPSP(treeDataRaw = bc$treeDataRaw,
                                           plotHeaderDataRaw = bc$plotHeaderDataRaw,
                                           damageAgentCodes = bc$pspBCdamageAgentCodes,
                                           codesToExclude = NULL)
  bcClean_someDam <- dataPurification_BCPSP(treeDataRaw = bc$treeDataRaw,
                                            plotHeaderDataRaw = bc$plotHeaderDataRaw,
                                            damageAgentCodes = bc$pspBCdamageAgentCodes,
                                            excludeAllObs = FALSE)

  somedamN <- nrow(bcClean_someDam$treeData)
  nodamN <- nrow(bcClean$treeData)
  alldamN <- nrow(bcClean_allDam$treeData)
  expect_true(somedamN > nodamN & somedamN < alldamN)

})
test_that("PSP alberta works", {

  ab <- prepInputsAlbertaPSP(dPath = mainDir)

  sk <- prepInputsSaskatchwanPSP(dPath = mainDir)
  skm <- prepInputsSaskatchwanTSP(dPath = mainDir)
  abClean <- dataPurification_ABPSP(treeMeasure = ab$pspABtreeMeasure,
                                    plotMeasure = ab$pspABplotMeasure,
                                    tree =  ab$pspABtree, plot = ab$pspABplot)

  expect_true(all(names(abClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(abClean$treeData) %in% standardizedTreeNames))

  abClean_allDam <- dataPurification_ABPSP(treeMeasure = ab$pspABtreeMeasure,
                                    plotMeasure = ab$pspABplotMeasure,
                                    tree =  ab$pspABtree, plot = ab$pspABplot,
                                    codesToExclude = NULL)

  abClean_someDam <- dataPurification_ABPSP(treeMeasure = ab$pspABtreeMeasure,
                                            plotMeasure = ab$pspABplotMeasure,
                                            tree =  ab$pspABtree, plot = ab$pspABplot,
                                            excludeAllObs = FALSE)

  somedamN <- nrow(abClean_someDam$treeData)
  nodamN <- nrow(abClean$treeData)
  alldamN <- nrow(abClean_allDam$treeData)
  expect_true(somedamN > nodamN & somedamN < alldamN)

})

test_that("PSP sk works", {

  skClean <- dataPurification_SKPSP(SADataRaw = sk$SADataRaw, plotHeaderRaw = sk$plotHeaderRaw,
                                    measureHeaderRaw = sk$measureHeaderRaw, treeDataRaw = sk$treeDataRaw)
  expect_true(all(names(skClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(skClean$treeData) %in% standardizedTreeNames))

  skmClean <- dataPurification_SKTSP_Mistik(compiledPlotData = skm$compiledPlotData,
                                            compiledTreeData = skm$compiledTreeData)
  expect_true(all(names(skmClean$plotHeaderData) %in% standardizedPlotNames))
  expect_true(all(names(skmClean$treeData) %in% standardizedTreeNames))
})
