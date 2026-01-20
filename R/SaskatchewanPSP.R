globalVariables(c(
  ":=", ".", "baseSA", "baseYear", "CONDITION_CODE1", "CONDITION_CODE2",
  "CONDITION_CODE3", "CROWN_CLASS", "dbh", "DBH", "Easting", "height",
  "Height", "HEIGHT", "IsBad", "MeasureID", "MeausreYear", "MORTALITY",
  "NofTrees", "Northing", "OFFICE_ERROR", "OrigPlotID1", "OrigPlotID2",
  "PLOT_ID", "PLOT_SIZE", "PlotSize", "SK_forestry", "species", "Species", "SPECIES",
  "TOTAL_AGE", "TREE_NO", "TREE_STATUS", "treeAge", "TreeNumber",
  "YEAR", "Z13nad83_e", "Z13nad83_n", "Zone"
))

#' Standardize and Treat the Saskatchewan PSP Data
#'
#' This function cleans and standardizes the Saskatchewan PSP data, including tree measurements,
#' plot headers, and measurement headers. Species names can be standardized using a species equivalency table.
#'
#' @param SADataRaw A `data.frame` or `data.table` containing raw tree measurement data.
#' @param plotHeaderRaw A `data.frame` or `data.table` containing raw plot header data.
#' @param measureHeaderRaw A `data.frame` or `data.table` containing raw measurement header data.
#' @param treeDataRaw A `data.frame` or `data.table` containing tree data.
#' @param codesToExclude Character vector of damage agent codes used to filter tree data.
#'                       Codes: Natural or Undetermined = 1, Disease = 2, Insect = 3, Human = 4,
#'                       Wind = 5, Snow = 6, Other Trees = 7, Hail or Ice Storm = 8.
#'                       Measurements with these codes will be removed.
#' @param excludeAllObs Logical. If `TRUE`, all prior and future observations of trees with
#'                       codes in `codesToExclude` are removed.
#' @param sppEquiv A table providing species name equivalencies between the original PSP species names
#'                 and the final standardized naming format. Default is `LandR::sppEquivalencies_CA`.
#' @param sppEquivCol Character string. The column in `sppEquiv` that contains the standardized species names.
#'                    Default is `"Latin_full"`.
#'
#' @return A list containing standardized `plotData` and `treeData` as `data.table`s.
#'
#' @export
#' @importFrom data.table setnames setkey rbindlist

dataPurification_SKPSP <- function(SADataRaw, plotHeaderRaw, measureHeaderRaw,
                                   treeDataRaw, codesToExclude = NULL, excludeAllObs = TRUE
                                   , sppEquiv = LandR::sppEquivalencies_CA,
                                   sppEquivCol = "Latin_full") {

  # get rid of artifical trees - plots where the distribution of trees/DBH/species were modelled
  treeDataRaw[, isArtificial := OFFICE_ERROR == "Artificial Tree", ]
  hasArtificial <- treeDataRaw[isArtificial == TRUE, .N, .(PLOT_ID)]
  treeDataRaw <- treeDataRaw[!PLOT_ID %in% hasArtificial$PLOT_ID]
  SADataRaw <- SADataRaw[!PLOT_ID %in% hasArtificial$PLOT_ID]
  plotHeaderRaw <- plotHeaderRaw[!PLOT_ID %in% hasArtificial$PLOT_ID]
  ## there are no plots with artifical and real trees

  header_SA <- SADataRaw[!is.na(TOTAL_AGE) & TREE_STATUS == 1, ]
  header_SA[, baseYear := min(YEAR), by = PLOT_ID]
  header_SA[, treeAge := TOTAL_AGE - (YEAR - baseYear)]
  header_SA_Dom <- header_SA[CROWN_CLASS == 1, ] # the stand age first determined by dominant trees
  header_SA_Dom[, NofTrees := length(CROWN_CLASS), by = PLOT_ID]
  # unique(SADataRawDomSA$NofTrees) # 1 2 3 4 5
  # stand age must determined by using at least 2 trees
  header_SA_Dom <- header_SA_Dom[NofTrees != 1, ]
  # SADataRawDomSA[, treeAgeDif:=max(treeAge)-min(treeAge), by = PLOT_ID]
  # range(SADataRawDomSA$treeAgeDif) # 0 44
  # mean(SADataRawDomSA$treeAgeDif) # 7.03
  header_SA_Dom[, baseSA := as.integer(mean(treeAge)), by = PLOT_ID]
  header_SA_Dom <- unique(header_SA_Dom[, .(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID")
  # for the other plots determine SA using codominant trees
  header_SA_CoDom <- header_SA[CROWN_CLASS == 2, ]

  header_SA_CoDom <- header_SA_CoDom[!(PLOT_ID %in% unique(header_SA_Dom$PLOT_ID)), ]
  header_SA_CoDom[, NofTrees := length(CROWN_CLASS), by = PLOT_ID]
  # unique(SADataRawCodomSA$NofTrees)
  header_SA_CoDom <- header_SA_CoDom[NofTrees != 1, ]
  header_SA_CoDom[, baseSA := as.integer(mean(treeAge)), by = PLOT_ID]
  header_SA_CoDom <- unique(header_SA_CoDom[, .(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID")
  headData_SA <- rbind(header_SA_Dom, header_SA_CoDom)

  headData_loca <- plotHeaderRaw[PLOT_ID %in% unique(headData_SA$PLOT_ID), ][, .(PLOT_ID, Z13nad83_e, Z13nad83_n, Zone = 13)]
  setnames(headData_loca, 2:3, c("Easting", "Northing"))
  headData_SALoca <- setkey(headData_SA, PLOT_ID)[setkey(headData_loca, PLOT_ID),
                                                  nomatch = 0
  ]
  headData_PS <- measureHeaderRaw[PLOT_ID %in% unique(headData_SALoca$PLOT_ID), ][, .(PLOT_ID, PLOT_SIZE)][!is.na(PLOT_SIZE), ]
  headData_PS <- unique(headData_PS, by = "PLOT_ID")
  setnames(headData_PS, "PLOT_SIZE", "PlotSize")
  headData <- headData_SALoca[setkey(headData_PS, PLOT_ID), nomatch = 0]


  # for tree data
  treeDataRaw <- treeDataRaw[PLOT_ID %in% headData$PLOT_ID, ][, .(
    PLOT_ID, TREE_NO, YEAR, SPECIES, DBH,
    HEIGHT, TREE_STATUS, CONDITION_CODE1,
    CONDITION_CODE2, CONDITION_CODE3, MORTALITY
  )]

  # potentialy users want to remove all insect-disturbed trees
  if (!is.null(codesToExclude)) {
    # all dead trees are excluded by default - there is no class of live but damaged unlike other jurisdictions
    # therefore, we only remove observations if excludeAllObs is TRUE
    # meaning, if a tree died due to e.g. insects, remove it from all obs
    if (excludeAllObs) {
      flaggedTrees <- treeData[MORTALITY %in% codesToExclude, ]
    }
  }

  # check the living trees
  # 1. by tree status codes
  #     1 1 Live
  #     2 2 Declining
  #     3 3 Dead or dying
  #     4 4 Loose bark snag
  #     5 5 Clean snag
  #     6 6 Snag with broken top
  #     7 7 Decomposed snag.
  #     8 8 Down snag
  #     9 9 Stump
  treeData <- treeDataRaw[is.na(TREE_STATUS) | # conservatively
                            TREE_STATUS == 0 |
                            TREE_STATUS == 1 |
                            TREE_STATUS == 2, ]
  # 2. by mortality codes
  #     Null 0
  #     Natural or Undetermined 1
  #     Disease 2
  #     Insect 3
  #     Human 4
  #     Wind 5
  #     Snow 6
  #     Other Trees 7
  #     Hail or Ice Storm 8
  treeData <- treeData[MORTALITY == 0 | is.na(MORTALITY), ]

  treeData <- treeData[!is.na(DBH) & DBH != 0, ]
  treeData <- treeData[, .(PLOT_ID, YEAR, TREE_NO, SPECIES, DBH, HEIGHT)]
  setnames(treeData, new = c("OrigPlotID1", "MeasureYear", "TreeNumber", "Species", "DBH", "Height"))
  setnames(headData, "PLOT_ID", "OrigPlotID1")
  measureidtable <- unique(treeData[, .(OrigPlotID1, MeasureYear)], by = c("OrigPlotID1", "MeasureYear"))
  measureidtable[, MeasureID := paste("SKPSP_", row.names(measureidtable), sep = "")]
  measureidtable <- measureidtable[, .(MeasureID, OrigPlotID1, MeasureYear)]
  treeData <- setkey(measureidtable, OrigPlotID1, MeasureYear)[setkey(treeData, OrigPlotID1, MeasureYear), nomatch = 0]
  treeData <- treeData[, .(MeasureID, OrigPlotID1, MeasureYear, TreeNumber, Species, DBH, Height)]
  headData <- setkey(measureidtable, OrigPlotID1)[setkey(headData, OrigPlotID1), nomatch = 0]
  headData <- headData[, .(MeasureID, OrigPlotID1, MeasureYear,
                           Longitude = NA,
                           Latitude = NA, Zone, Easting, Northing, PlotSize, baseYear, baseSA
  )]

  # Standardize
  # Select only necessary columns and join
  sppEquiv <- sppEquiv[SK_forestry != "", .SD, .SDcols = c("SK_forestry", sppEquivCol)]
  # Keep unique rows only
  sppEquiv <- unique(sppEquiv)

  treeData <- sppEquiv[treeData, on = .(SK_forestry  = Species)]

  setnames(treeData, old = c("SK_forestry", sppEquivCol), new = c("PSP", "Species"))


  # Check
  treeData[, .(PSP, Species)]

  treeData[is.na(Species)| Species == "", Species := "unknown"]
  treeData[is.na(PSP) | PSP == "", PSP := "unknown"]

  treeData[MeasureYear == 2044, MeasureYear := 2014] # correct obvious error
  headData[MeasureYear == 2044, MeasureYear := 2014]

  headData[, OrigPlotID1 := as.character(OrigPlotID1)]
  treeData[, OrigPlotID1 := as.character(OrigPlotID1)]

  # this funtion wil identify measurements where no single tree matches a subsequent (or prior) measurement in same plot
  badTreeNumbers <- lapply(headData$MeasureID, FUN = function(x, td = treeData) {
    thisPlot <- td[MeasureID == x, ]
    theOtherPlot <- td[MeasureID != x & OrigPlotID1 %in% thisPlot$OrigPlotID1, .(TreeNumber)]
    thisPlot <- thisPlot[TreeNumber %in% theOtherPlot$TreeNumber, ]
    IsBad <- nrow(thisPlot) == 0 & nrow(theOtherPlot) != 0
    out <- data.table(MeasureID = x, IsBad = IsBad)
    return(out)
  })

  badTreeNumbers <- rbindlist(badTreeNumbers)
  badTreeNumbers <- badTreeNumbers[IsBad == TRUE, ]
  # first isolate the measurements with no matching trees
  badMeasures <- headData[MeasureID %in% badTreeNumbers$MeasureID]
  badPlots <- headData[OrigPlotID1 %in% badMeasures$OrigPlotID1]
  badTrees <- treeData[MeasureID %in% badTreeNumbers$MeasureID]
  headData <- headData[!MeasureID %in% badTreeNumbers$MeasureID, ]
  treeData <- treeData[!MeasureID %in% badTreeNumbers$MeasureID, ]

  # 20004 is bad in 51 and 61
  # the above approach will not correct plots where the numbers change after 2 measurements,
  # ie split 2-2 or 2-3 with 4/5 measurements respectively - at thsi point I assume stand-replacing disturbance

  treeData[, OrigPlotID1 := paste0("SKPSP", OrigPlotID1)]
  headData[, OrigPlotID1 := paste0("SKPSP", OrigPlotID1)]
  # final clean up
  treeData[Height <= 0, Height := NA]
  treeData <- treeData[!is.na(DBH) & DBH > 0]

  treeData <- treeData[, .(
    MeasureID, OrigPlotID1, MeasureYear, TreeNumber, PSP, Species, DBH, Height
  )]

  headData[, source := "SK"]
  treeData[, source := "SK"]

  return(list(
    "plotHeaderData" = headData,
    "treeData" = treeData
  ))
}

#' source the Saskatchewan PSP data
#' @param dPath passed to prepInputs destinationPath
#'
#' @return a list of Saskatchewan PSP data.tables
#'
#' @export
#' @importFrom reproducible prepInputs
prepInputsSaskatchwanPSP <- function(dPath) {
  pspSKRaw <- prepInputs(
    targetFile = "SKPSP.RData",
    url = "https://drive.google.com/open?id=1yvOzfNqZ28oLYKTfdDb8-Wip7jBPtCz6",
    destinationPath = dPath,
    fun = "load",
    overwrite = TRUE
  )

  SADataRaw <- pspSKRaw$plotheader1
  plotHeaderRaw <- pspSKRaw$plotheader3
  measureHeaderRaw <- pspSKRaw$plotheader2
  treeDataRaw <- pspSKRaw$treedata

  return(list(
    "SADataRaw" = SADataRaw,
    "plotHeaderRaw" = plotHeaderRaw,
    "measureHeaderRaw" = measureHeaderRaw,
    "treeDataRaw" = treeDataRaw
  ))
}
