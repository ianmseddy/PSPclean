utils::globalVariables(c(
  ":=", ".", "baseSA", "baseYear", "damage_agent", "dbh", "DBH",
  "Easting", "elevation", "Elevation", "Genus", "height", "Height",
  "isArtificial", "lgtree_genus", "lgtree_species", "lgtree_status",
  "meas_num", "meas_plot_size", "MeasureID", "MeasureYear", "nfi_plot",
  "Northing", "orig_plot_area", "OrigPlotID1", "PlotSize", "site_age",
  "Species", "tree_num", "TreeNumber", "utm_e", "utm_n", "utm_zone",
  "year", "Zone", "meas_date"
))

#' #' Standardize and Treat the NFI PSP Data
#'
#' This function cleans and standardizes NFI PSP data, including tree, plot, and location data.
#' Species names can be standardized using a species equivalency table.
#'
#' @param NFIdata A list containing NFI tree, plot, and location data.tables.
#' @param codesToExclude Vector of damage agent codes. Measurements with these codes will be removed.
#' @param excludeAllObs Logical. If `TRUE`, removing observations of individual trees due to damage codes
#'                       will also remove all prior and future observations of that tree.
#' @param sppEquiv A table providing species name equivalencies between the original PSP species names
#'                 and the final standardized naming format. Default is `LandR::sppEquivalencies_CA`.
#' @param sppEquivCol Character string. The column in `sppEquiv` that contains the standardized species names.
#'                    Default is `"Latin_full"`.
#'
#' @return A list containing standardized `plotData` and `treeData` as `data.table`s.
#'
#' @export
#' @importFrom data.table copy setkey set

dataPurification_NFIPSP <- function(NFIdata, codesToExclude = "IB", excludeAllObs = TRUE
                                    , sppEquiv = LandR::sppEquivalencies_CA,
                                      sppEquivCol = "Latin_full") {

  lgptreeRaw <- copy(NFIdata[["pspTreeMeasure"]])
  lgpHeaderRaw <- copy(NFIdata[["pspHeader"]])
  approxLocation <- NFIdata[["pspLocation"]]
  treeDamage <- NFIdata[["pspTreeDamage"]]

  lgptreeRaw <- lgptreeRaw[orig_plot_area == "Y", ]

  # start from tree data to obtain plot infor
  lgptreeRaw[, year := as.numeric(format(as.Date(meas_date, "%Y-%B-%d"), "%Y"))]
  lgpHeaderRaw[, year := as.numeric(format(as.Date(meas_date, "%Y-%B-%d"), "%Y"))]


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

  treeData[, SpeciesCode := paste0(Genus, "_", Species)]

  treeData[, Species := NULL]
  sppEquiv <- sppEquiv[!(sppEquiv$NFI == "" & sppEquiv$Latin_full == ""), ]

  # Only keep the PSP column as the standardized species name
  sppEquiv <- sppEquiv[, .SD, .SDcols = c(sppEquivCol,"NFI")]

  # Keep unique rows only
  sppEquiv <- unique(sppEquiv)

  # Join correctly
  treeData <- sppEquiv[treeData, on = .(NFI = SpeciesCode)]

  setnames(treeData, old = c(sppEquivCol, "NFI"), new = c("Species", "PSP"))

  # Check
  treeData[, .(PSP, Species)]

  treeData[is.na(Species)| Species == "", Species := "unknown"]
  treeData[is.na(PSP) | PSP == "", PSP := "unknown"]

  treeData[, "Genus" := NULL] # This "Genus" column is not in any of the other PSP datasets

  treeData$OrigPlotID1 <- paste0("NFIPSP", treeData$OrigPlotID1)
  lgpHeader$OrigPlotID1 <- paste0("NFIPSP", lgpHeader$OrigPlotID1)

  treeData[Height <= 0, Height := NA]
  treeData <- treeData[!is.na(DBH) & DBH > 0]

  treeData <- treeData[, .(MeasureID, OrigPlotID1, MeasureYear, TreeNumber, PSP, Species, DBH, Height)]

  lgpHeader[, source := "NFI"]
  treeData[, source := "NFI"]

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
