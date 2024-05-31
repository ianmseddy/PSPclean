globalVariables(c(
  ":=", ".", "CONDCOD1", "CONDCOD2", "CONDCOD3", "CRZ_EAST",
  "CRZ_ZONE", "CRZNORTH", "DBH", "Easting", "HEIGHT", "ID_FOR",
  "MeasureID", "MeasureYear", "Northing", "P_AGECLS", "PLOTNUM",
  "PlotSize", "PSIZE", "SA", "SPECIES", "TREENO", "YEAR", "Zone"
))

#' standardize and treat the Saskatchewan Mistik TSP data
#'
#' @param compiledPlotData the plot header data
#' @param compiledTreeData the tree data
#'
#' @return a list of plot and tree data.tables
#'
#' @export
#' @importFrom data.table setnames setkey
dataPurification_SKTSP_Mistik <- function(compiledPlotData, compiledTreeData) {
  headData <- compiledPlotData[, .(ID_FOR, CRZ_ZONE, CRZNORTH, CRZ_EAST, PLOTNUM, YEAR, PSIZE, P_AGECLS)]

  headData <- unique(headData, by = c("ID_FOR", "PLOTNUM"))
  headData[, PlotSize := sum(PSIZE), by = ID_FOR]
  headData <- unique(headData, by = "ID_FOR")[, ":="(PLOTNUM = NULL, PSIZE = NULL)]
  headData[, MeasureID := paste("SKTSP_Mistik_", row.names(headData), sep = "")]
  setnames(
    headData, c("CRZNORTH", "CRZ_EAST", "CRZ_ZONE", "YEAR", "P_AGECLS"),
    c("Northing", "Easting", "Zone", "MeasureYear", "SA")
  )

  treeData <- compiledTreeData[, .(ID_FOR, TREENO, SPECIES, DBH, HEIGHT, CONDCOD1, CONDCOD2, CONDCOD3)]
  treeData <- treeData[ID_FOR %in% unique(headData$ID_FOR), ]
  # remove dead trees
  treeData <- treeData[CONDCOD1 != "DE", ]
  treeData <- treeData[CONDCOD2 != "DE", ]
  treeData <- treeData[CONDCOD3 != "DE", ]
  set(treeData, NULL, c("CONDCOD1", "CONDCOD2", "CONDCOD3"), NULL)
  treeData <- setkey(
    headData[, .(MeasureID, ID_FOR, MeasureYear)],
    ID_FOR
  )[setkey(treeData, ID_FOR), nomatch = 0]

  treeData <- treeData[, .(MeasureID,
    OrigPlotID1 = ID_FOR, MeasureYear,
    TreeNumber = TREENO, Species = SPECIES, DBH, Height = HEIGHT
  )]

  headData <- headData[, .(MeasureID,
    OrigPlotID1 = ID_FOR, MeasureYear, Longitude = NA,
    Latitude = NA, Zone, Easting = Easting * 10000, Northing = Northing * 10000,
    PlotSize, baseYear = MeasureYear, baseSA = SA
  )]

  treeData <- standardizeSpeciesNames(treeData, forestInventorySource = "SKTSP")
  treeData <- treeData[treeData$TreeNumber != 0, ]

  treeData$OrigPlotID1 <- paste0("SKMistik", treeData$OrigPlotID1)
  headData$OrigPlotID1 <- paste0("SKMistik", headData$OrigPlotID1)

  treeData[Height <= 0, Height := NA]
  treeData <- treeData[!is.na(DBH) & DBH > 0]

  headData[, source := "SK"]
  treeData[, source := "SK"]
  headData <- headData[OrigPlotID1 %in% treeData$OrigPlotID1]

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
prepInputsSaskatchwanTSP <- function(dPath) {
  tspSKMistikRaw <- prepInputs(
    targetFile = "SK_TSP_Mistik.RData",
    url = "https://drive.google.com/open?id=1PCn0DpGwsXBhquW3jOaqks1QC9teo_Xx",
    destinationPath = dPath,
    fun = "load",
    overwrite = TRUE
  )

  return(list(
    "compiledPlotData" = tspSKMistikRaw$plotheader,
    "compiledTreeData" = tspSKMistikRaw$treedata
  ))
}
