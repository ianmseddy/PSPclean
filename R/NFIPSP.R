utils::globalVariables(c(
  ":=", ".", "baseSA", "baseYear", "damage_agent", "dbh", "DBH",
  "Easting", "elevation", "Elevation", "Genus", "height", "Height",
  "isArtificial", "lgtree_genus", "lgtree_species", "lgtree_status",
  "meas_num", "meas_plot_size", "MeasureID", "MeasureYear", "nfi_plot",
  "Northing", "orig_plot_area", "OrigPlotID1", "PlotSize", "site_age",
  "Species", "tree_num", "TreeNumber", "utm_e", "utm_n", "utm_zone",
  "year", "Zone", "meas_date"
))

#' standardize and treat the NFI PSP data
#'
#' @param NFIdata list of NFI tree, plot, and location data
#' @param codesToExclude damage agents to exclude from measurements
#' @param excludeAllObs if removing observations of individual trees due to damage codes,
#' remove all prior and future observations if `TRUE`.
#'
#' @return a list of plot and tree data.tables
#'
#' @export
#' @importFrom data.table copy setkey set
dataPurification_NFIPSP <- function(NFIdata, codesToExclude = "IB", excludeAllObs = TRUE) {

  lgptreeRaw <- copy(NFIdata[["pspTreeMeasure"]])
  lgpHeaderRaw <- copy(NFIdata[["pspHeader"]])
  approxLocation <- NFIdata[["pspLocation"]]
  treeDamage <- NFIdata[["pspTreeDamage"]]

  lgptreeRaw <- lgptreeRaw[orig_plot_area == "Y", ]
  # start from tree data to obtain plot infor
  lgptreeRaw[, year := as.Date(meas_date, format = "%Y-%B-%d")]
  lgpHeaderRaw[, year := as.Date(meas_date, format = "%Y-%B-%d")]
  lgptreeRaw[, year := as.numeric(format(year, "%Y"))]
  lgpHeaderRaw[, year := as.numeric(format(year, "%Y"))]

  lgpHeader <- lgpHeaderRaw[nfi_plot %in% unique(lgptreeRaw$nfi_plot), ][, .(nfi_plot, year, meas_plot_size, site_age)]
  approxLocation <- approxLocation[, .(nfi_plot, utm_n, utm_e, utm_zone, elevation)]
  approxLocation <- unique(approxLocation, by = "nfi_plot")
  lgpHeader <- setkey(lgpHeader, nfi_plot)[setkey(approxLocation, nfi_plot), nomatch = 0]
  # remove the plots without SA and location infor
  lgpHeader <- lgpHeader[!is.na(site_age), ][!is.na(utm_n), ][!is.na(utm_e), ]
  treeData <- lgptreeRaw[, .(
    nfi_plot, year, meas_num, tree_num, lgtree_genus, lgtree_species,
    lgtree_status, dbh, height
  )][nfi_plot %in% unique(lgpHeader$nfi_plot), ]
  # DS = dead standing, M = Missing Data
  treeData <- treeData[lgtree_status != "DS" & lgtree_status != "M", ][, lgtree_status := NULL]

  # remove bad plots

  if (!is.null(codesToExclude)) {
    badTrees <- treeDamage[damage_agent %in% codesToExclude, .(nfi_plot, meas_num, tree_num)]
    message(paste("removing", nrow(badTrees), "trees in NFI due to damage agents"))
    if (excludeAllObs) {
      treeData <- treeData[!badTrees, on = c("nfi_plot", "tree_num")]
    } else {
      treeData <- treeData[!badTrees, on = c("nfi_plot", "meas_num", "tree_num")]
    }
  }
  # meas_num is needed to match damage, but not afterward
  treeData[, meas_num := NULL]

  setnames(
    treeData, c("nfi_plot", "year", "tree_num", "lgtree_genus", "lgtree_species", "dbh", "height"),
    c("OrigPlotID1", "MeasureYear", "TreeNumber", "Genus", "Species", "DBH", "Height")
  )

  # names(lgpHeader) <- c("OrigPlotID1", "baseYear", "PlotSize", "baseSA", "Northing", "Easting", "Zone", "Elevation")
  setnames(lgpHeader,
    old = c("nfi_plot", "year", "meas_plot_size", "site_age", "utm_n", "utm_e", "utm_zone", "elevation"),
    new = c("OrigPlotID1", "baseYear", "PlotSize", "baseSA", "Northing", "Easting", "Zone", "Elevation")
  )

  lgpHeader <- unique(lgpHeader, by = "OrigPlotID1")
  newheader <- unique(treeData[, .(OrigPlotID1, MeasureYear)], by = c("OrigPlotID1", "MeasureYear"))
  newheader[, MeasureID := paste("NFIPSP_", row.names(newheader), sep = "")]

  treeData <- setkey(treeData, OrigPlotID1)
  treeData <- treeData[newheader, on = c("OrigPlotID1", "MeasureYear")]
  lgpHeader <- setkey(lgpHeader, OrigPlotID1)[setkey(newheader, OrigPlotID1), nomatch = 0]
  lgpHeader <- setkey(lgpHeader, OrigPlotID1)
  lgpHeader <- lgpHeader[newheader, on = c("OrigPlotID1", "MeasureID")]

  treeData <- treeData[, .(
    MeasureID, OrigPlotID1, MeasureYear,
    TreeNumber, Genus, Species, DBH, Height
  )]
  lgpHeader <- lgpHeader[, .(MeasureID, OrigPlotID1, MeasureYear,
    Longitude = NA, Latitude = NA, Zone,
    Easting, Northing, Elevation, PlotSize, baseYear, baseSA
  )]

  treeData <- standardizeSpeciesNames(treeData, forestInventorySource = "NFIPSP") # Need to add to pemisc

  treeData[, Species := paste0(Genus, "_", Species)]
  treeData[, Genus := NULL] # This column is not in any of the other PSP datasets

  treeData$OrigPlotID1 <- paste0("NFIPSP", treeData$OrigPlotID1)
  lgpHeader$OrigPlotID1 <- paste0("NFIPSP", lgpHeader$OrigPlotID1)

  treeData[Height <= 0, Height := NA]
  treeData <- treeData[!is.na(DBH) & DBH > 0]

  return(list(
    "plotHeaderData" = lgpHeader,
    "treeData" = treeData
  ))
}

#' source the NFI PSP data
#' @param dPath passed to prepInputs destinationPath
#'
#' @return a list of NFI PSP data.tables
#'
#' @export
#' @importFrom reproducible prepInputs
prepInputsNFIPSP <- function(dPath) {
  pspNFILocationRaw <- prepInputs(
    targetFile = "all_gp_site_info.csv",
    archive = "all_gp_climate.zip",
    url = "https://drive.google.com/file/d/1S-4itShMXtwzGxjKPgsznpdTD2ydE9qn/",
    destinationPath = dPath,
    overwrite = TRUE,
    fun = "data.table::fread"
  )

  pspNFIHeaderRaw <- prepInputs(
    targetFile = "all_gp_ltp_header.csv",
    archive = "all_gp_trees.zip",
    url = "https://drive.google.com/file/d/1i4y1Tfi-kpa5nHnpMbUDomFJOja5uD2g/",
    destinationPath = dPath,
    fun = "data.table::fread",
    overwrite = TRUE
  )

  pspNFITreeRaw <- prepInputs(
    targetFile = "all_gp_ltp_tree.csv",
    archive = "all_gp_trees.zip",
    url = "https://drive.google.com/file/d/1i4y1Tfi-kpa5nHnpMbUDomFJOja5uD2g/",
    destinationPath = dPath,
    fun = "data.table::fread",
    overwrite = TRUE
  )

  pspNFITreeDamage <- prepInputs(
    targetFile = "all_gp_ltp_tree_damage.csv",
    archive = "all_gp_trees.zip",
    url = "https://drive.google.com/file/d/1i4y1Tfi-kpa5nHnpMbUDomFJOja5uD2g/",
    destinationPath = dPath,
    fun = "data.table::fread",
    overwrite = TRUE
  )

  return(list(
    "pspLocation" = pspNFILocationRaw,
    "pspHeader" = pspNFIHeaderRaw,
    "pspTreeMeasure" = pspNFITreeRaw,
    "pspTreeDamage" = pspNFITreeDamage
  ))
}
