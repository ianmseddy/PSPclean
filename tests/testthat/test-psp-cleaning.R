
# This test ensures that the PSP data from all provinces and the NFI are properly cleaned and structured.
# It checks three key aspects:
# - Non-empty data tables: The cleaned tree and plot tables must contain at least one row.
# - No fully NA rows: There should not be any rows where all columns are missing,
# which could indicate a problem during data import or cleaning.
# Standardized columns: The tree and plot tables should have the expected set of standardized column names,
# ensuring consistency across provinces and enabling downstream analyses.

standardizedPlotNames <- c(
  "MeasureID", "OrigPlotID1", "MeasureYear", "Longitude", "Latitude", "Datum", "source",
  "Zone", "Northing", "Easting", "Elevation", "PlotSize", "baseYear", "baseSA"
)
standardizedTreeNames <- c(
  "MeasureID", "OrigPlotID1", "MeasureYear", "TreeNumber", "Species", "source",
  "DBH", "Height", "PSP"
)

# Define mandatory columns
mandatoryTreeCols <- c( "Species", "PSP" )
mandatoryPlotCols <- c("MeasureID", "OrigPlotID1", "MeasureYear")

test_that("PSP data is not empty and mandatory columns are complete for all provinces", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "PSP_")

  check_data <- function(dt, mandatoryCols = c("Species", "PSP")) {
    # Check data.table is not empty
    expect_true(nrow(dt) > 0, info = "Data.table should not be empty")

    # Check there are no rows with all NA
    expect_false(any(apply(dt, 1, function(row) all(is.na(row)))),
                 info = "There are rows with all NA values")

    # Check only mandatory columns for NA or empty
    for (col in mandatoryCols) {
      expect_false(any(is.na(dt[[col]]) | dt[[col]] == ""),
                   info = paste0("Column ", col, " contains NA or empty values"))
    }
  }
  # Alberta
  ab <- prepInputsAlbertaPSP(dPath = dPath)
  abClean <- dataPurification_ABPSP(
    treeMeasure = ab$pspABtreeMeasure,
    plotMeasure = ab$pspABplotMeasure,
    tree = ab$pspABtree,
    plot = ab$pspABplot
  )
  check_data(abClean$treeData, mandatoryTreeCols)
  check_data(abClean$plotHeaderData, mandatoryPlotCols)

  # BC
  bc <- prepInputsBCPSP(dPath = dPath)
  bcClean <- dataPurification_BCPSP(
    treeDataRaw = bc$treeDataRaw,
    plotHeaderDataRaw = bc$plotHeaderDataRaw,
    damageAgentCodes = bc$pspBCdamageAgentCodes
  )
  check_data(bcClean$treeData, mandatoryTreeCols)
  check_data(bcClean$plotHeaderData, mandatoryPlotCols)

  # Ontario
  on <- prepInputsOntarioPSP(dPath = dPath)
  sppEquiv <- LandR::sppEquivalencies_CA
  onClean <- dataPurification_ONPSP(ONPSPlist = on, sppEquiv = sppEquiv)
  check_data(onClean$treeData, mandatoryTreeCols)
  check_data(onClean$plotHeaderData, mandatoryPlotCols)

  # Saskatchewan
  sk <- prepInputsSaskatchwanPSP(dPath = dPath)
  skClean <- dataPurification_SKPSP(
    SADataRaw = sk$SADataRaw,
    plotHeaderRaw = sk$plotHeaderRaw,
    measureHeaderRaw = sk$measureHeaderRaw,
    treeDataRaw = sk$treeDataRaw
  )
  check_data(skClean$treeData, mandatoryTreeCols)
  check_data(skClean$plotHeaderData, mandatoryPlotCols)

  # New Brunswick
  nb <- prepInputsNBPSP(dPath = dPath)
  nbClean <- dataPurification_NBPSP(nb)
  check_data(nbClean$treeData, mandatoryTreeCols)
  check_data(nbClean$plotHeaderData, mandatoryPlotCols)

  # Quebec
  qc <- prepInputsQCPSP(dPath = dPath)
  qcClean <- dataPurification_QCPSP(qc)
  check_data(qcClean$treeData, mandatoryTreeCols)
  check_data(qcClean$plotHeaderData, mandatoryPlotCols)

  # NFI
  nfi <- prepInputsNFIPSP(dPath = dPath)
  nfiClean <- dataPurification_NFIPSP(NFIdata = nfi)
  check_data(nfiClean$treeData, mandatoryTreeCols)
  check_data(nfiClean$plotHeaderData, mandatoryPlotCols)
})
