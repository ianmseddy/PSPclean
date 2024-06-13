#' return a merged PSP object from a vector of data sources
#'
#' @param PSPdataTypes character vector of PSP data sources - e.g. `c("BC", "SK)"`
#' Use `"all"` to get all available sources, and `"dummy"` for freely availble data
#' @param destinationPath destination folder for downloaded objects
#' @param forGMCS if `TRUE`, will pre-filter plots with insect mortality to avoid
#' attributing insect mortality with climate
#' @param sppEquiv species equivalencies table.
#' @return a list of standardized plot and tree data.tables
#'
#' @export
#' @importFrom data.table rbindlist
#' @importFrom reproducible prepInputs
getPSP <- function(PSPdataTypes, destinationPath, forGMCS = FALSE,
                   sppEquiv = LandR::sppEquivalencies_CA) {
  if ("dummy" %in% PSPdataTypes) {
    message("generating randomized PSP data")

    PSPmeasure <- prepInputs(targetFile = "randomizedPSPmeasure.rds",
                             archive = "randomized_LandR_speciesParameters_Inputs.zip",
                             url =  "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/view?usp=sharing",
                             destinationPath = destinationPath,
                             fun = "readRDS")

    PSPplot <- prepInputs(targetFile = "randomizedPSPplot.rds",
                          archive = "randomized_LandR_speciesParameters_Inputs.zip",
                          url = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/view?usp=sharing",
                          destinationPath = destinationPath,
                          fun = "readRDS")

    PSPgis <- prepInputs(targetFile = "randomizedPSPgis.rds",
                         archive = "randomized_LandR_speciesParameters_Inputs.zip",
                         url = "https://drive.google.com/file/d/1LmOaEtCZ6EBeIlAm6ttfLqBqQnQu4Ca7/view?usp=sharing",
                         overwrite = TRUE,
                         destinationPath = destinationPath,
                         fun = "readRDS")

  } else if (!any(PSPdataTypes %in% "none")) {
    if (!any(c("BC", "AB", "SK", "NFI", "ON", "all") %in% PSPdataTypes)) {
      stop("Please review dataTypes - incorrect value specified")
    }

    PSPmeasures <- list()
    PSPplots <- list()

    if (any(c("BC", "all") %in% PSPdataTypes)) {
      BCexclude <- if (forGMCS) {"IMB"} else {NULL}
      PSPbc <- prepInputsBCPSP(dPath = destinationPath)
      PSPbc <- dataPurification_BCPSP(treeDataRaw = PSPbc$treeDataRaw,
                                      plotHeaderDataRaw = PSPbc$plotHeaderDataRaw,
                                      damageAgentCodes = PSPbc$pspBCdamageAgentCodes,
                                      codesToExclude = BCexclude)
      PSPmeasures[["BC"]] <- PSPbc$treeData
      PSPplots[["BC"]] <- PSPbc$plotHeaderData
    }

    if (any(c("AB", "all") %in% PSPdataTypes)) {
      ABexclude <- if (forGMCS) {3} else {NULL}
      PSPab <- prepInputsAlbertaPSP(dPath = destinationPath)
      PSPab <- dataPurification_ABPSP(treeMeasure = PSPab$pspABtreeMeasure,
                                      plotMeasure = PSPab$pspABplotMeasure,
                                      tree = PSPab$pspABtree,
                                      plot = PSPab$pspABplot,
                                      codesToExclude = ABexclude)
      ## TODO: confirm if they really didn't record species on 11K trees
      PSPmeasures[["AB"]] <- PSPab$treeData
      PSPplots[["AB"]] <- PSPab$plotHeaderData
    }

    if (any(c("SK", "all") %in% PSPdataTypes)) {
      PSPsk <- prepInputsSaskatchwanPSP(dPath = destinationPath)
      PSPsk <- dataPurification_SKPSP(SADataRaw = PSPsk$SADataRaw,
                                      plotHeaderRaw = PSPsk$plotHeaderRaw,
                                      measureHeaderRaw = PSPsk$measureHeaderRaw,
                                      treeDataRaw = PSPsk$treeDataRaw)
      PSPmeasures[["SK"]] <- PSPsk$treeData
      PSPplots[["SK"]] <- PSPsk$plotHeaderData

      TSPsk <- prepInputsSaskatchwanTSP(dPath = destinationPath)
      TSPsk <- dataPurification_SKTSP_Mistik(compiledPlotData = TSPsk$compiledPlotData,
                                             compiledTreeData = TSPsk$compiledTreeData)
      PSPmeasures[["SKtsp"]] <- TSPsk$treeData
      PSPplots[["SKtsp"]] <- TSPsk$plotHeaderData
    }

    if (any(c("ON", "all") %in% PSPdataTypes)) {
      PSPon <- prepInputsOntarioPSP(dPath = destinationPath)
      ## the latin is used to translate species into common names for the biomass equations
      PSPon <- dataPurification_ONPSP(PSPon, sppEquiv)
      PSPmeasures[["ON"]] <- PSPon$treeData
      PSPplots[["ON"]] <- PSPon$plotHeaderData
    }

    if (any(c("NB", "all") %in% PSPdataTypes)) {
      PSPnb <- prepInputsNBPSP(dPath = destinationPath)
      ## the latin is used to translate species into common names for the biomass equations
      PSPnb <- dataPurification_NBPSP(PSPnb, sppEquiv)
      PSPmeasures[["NB"]] <- PSPnb$treeData
      PSPplots[["NB"]] <- PSPnb$plotHeaderData
    }

    if (any(c("NFI", "all") %in% PSPdataTypes)) {
      NFIexclude <- if (forGMCS) {"IB"} else {NULL}
      PSPnfi <- prepInputsNFIPSP(dPath = destinationPath)
      PSPnfi <- dataPurification_NFIPSP(PSPnfi, codesToExclude = NFIexclude)
      PSPmeasures[["NFI"]] <- PSPnfi$treeData
      PSPplots[["NFI"]] <- PSPnfi$plotHeaderData
    }

    PSPmeasure <- rbindlist(PSPmeasures, fill = TRUE)
    PSPplot <- rbindlist(PSPplots, fill = TRUE)
    PSPgis <- geoCleanPSP(Locations = PSPplot)

    ## clean up
    toRemove <- c("Zone", "Datum", "Easting", "Northing", "Latitude", "Longitude")
    toRemove <- toRemove[toRemove %in% colnames(PSPplot)]
    set(PSPplot, NULL, toRemove, NULL)

    #keep only plots with valid coordinates
    PSPmeasure <- PSPmeasure[OrigPlotID1 %in% PSPgis$OrigPlotID1,]
    PSPplot <- PSPplot[OrigPlotID1 %in% PSPgis$OrigPlotID1,]
  }

  return(list(PSPplot = PSPplot,
              PSPmeasure = PSPmeasure,
              PSPgis = PSPgis))
}
