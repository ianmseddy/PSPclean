#' return a merged PSP object from a vector of data sources
#'
#' @param PSPdataTypes character vector of PSP data sources - e.g. `c("BC", "SK)"`
#' Use `"all"` to get all available sources, and `"dummy"` for freely available data
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
    PSPmeasure[, source := "simulated"]
    PSPplot[, source := "simulated"]

  } else if (!any(PSPdataTypes %in% "none")) {
    if (!any(c("BC", "AB", "SK", "NFI", "ON", "QC", "NB", "all") %in% PSPdataTypes)) {
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

    if ("QC" %in% PSPdataTypes | "all" %in% PSPdataTypes) {
      PSPqc <- prepInputsQCPSP(dPath = destinationPath)
      PSPqc <- dataPurification_QCPSP(PSPqc)
      PSPmeasures[["QC"]] <- PSPqc$treeData
      PSPplots[["QC"]] <- PSPqc$plotHeaderData
    }

    if ("NFI" %in% PSPdataTypes | "all" %in% PSPdataTypes) {

      NFIexclude <- if (forGMCS) {"IB"} else {NULL}
      PSPnfi <- prepInputsNFIPSP(dPath = destinationPath)
      PSPnfi <- dataPurification_NFIPSP(PSPnfi, codesToExclude = NFIexclude)
      PSPmeasures[["NFI"]] <- PSPnfi$treeData
      PSPplots[["NFI"]] <- PSPnfi$plotHeaderData
    }

    PSPmeasure <- rbindlist(PSPmeasures, fill = TRUE)
    PSPplot <- rbindlist(PSPplots, fill = TRUE)

    #add Parvin's cleaning functions here:
    browser()
    #first one : Identifies statistical outliers in key variables (e.g., DBH)
    cleaningData1 <- detect_dbh_outliers(Trees = PSPmeasure)
    PSPmeasure <- cleaningData1$Trees
    outliers <- PSPmeasure[outlier_type != "none"] # List of outliers
    table(PSPmeasure$outlier_type)
    PSPplot <- PSPplot[OrigPlotID1 %in% cleaningData1$OrigPlotID1s,]

    #second one : Identify and resolves all inconsistencies, when a tree number in a Plot is linked to multiple Species Names
    cleaningData2 <- treenum_to_multiplePSP(Trees = PSPmeasure)
    PSPmeasure <- cleaningData2$Trees
    PSPmeasure_incorrect_data <- cleaningData2$incorrect_data
    PSPplot <- PSPplot[OrigPlotID1 %in% cleaningData2$OrigPlotID1s,]

    #third one : Process Implausible DBH Changes Across Measurement Years
    cleaningData3 <- process_dbh_issues(Trees = PSPmeasure)
    PSPmeasure <- cleaningData3$Trees
    PSPplot <- PSPplot[OrigPlotID1 %in% cleaningData3$OrigPlotID1s,]

    #fourth one : Classify Tree Status Based on Measurement History (e.g., Regeneration, Last Measurement, Alive),
    cleaningData4 <- classify_tree_status(Trees = PSPmeasure)
    PSPmeasure <- cleaningData4$Trees
    PSPplot <- PSPplot[OrigPlotID1 %in%cleaningData4$OrigPlotID1s,]

    #whatever is correctred needs ot be called PSPPlot, PSPmeasure still

    #fix GIS GIS
    PSPgis <- geoCleanPSP(Locations = PSPplot)

    ## clean up
    toRemove <- c("Zone", "Datum", "Easting", "Northing", "Latitude", "Longitude")
    toRemove <- toRemove[toRemove %in% colnames(PSPplot)]
    set(PSPplot, NULL, toRemove, NULL)

    #keep only plots with valid coordinates
    PSPmeasure <- PSPmeasure[OrigPlotID1 %in% PSPgis$OrigPlotID1,]
    PSPplot <- PSPplot[OrigPlotID1 %in% PSPgis$OrigPlotID1,]
  }

  #safety catch in case for some reason a user has supplied their own outdated sppEquiv
  PSPmeasure[is.na(newSpeciesName), newSpeciesName := ""] #the convention

  return(list(PSPplot = PSPplot,
              PSPmeasure = PSPmeasure,
              PSPgis = PSPgis))
}
