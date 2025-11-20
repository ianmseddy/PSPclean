
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
  "DBH", "Height", "newSpeciesName"
)

test_that("PSP data is not empty and has no fully NA rows for all provinces", {
  testthat::skip_if_not_installed("withr")
  dPath <- withr::local_tempdir(pattern = "PSP_")

  # Helper function to check for empty, fully NA rows, and standardized columns
  check_data <- function(dt, expected_cols) {
    expect_true(nrow(dt) > 0, info = "Data.table should not be empty")
    expect_false(any(apply(dt, 1, function(row) all(is.na(row)))),
                 info = "There are rows with all NA values")
    expect_true(all(expected_cols %in% colnames(dt)),
                info = "Missing expected standardized columns")
  }

  # Alberta
  ab <- prepInputsAlbertaPSP(dPath = dPath)
  abClean <- dataPurification_ABPSP(
    treeMeasure = ab$pspABtreeMeasure,
    plotMeasure = ab$pspABplotMeasure,
    tree = ab$pspABtree,
    plot = ab$pspABplot
  )
  check_data(abClean$treeData)
  check_data(abClean$plotHeaderData)

  # BC
  bc <- prepInputsBCPSP(dPath = dPath)
  bcClean <- dataPurification_BCPSP(
    treeDataRaw = bc$treeDataRaw,
    plotHeaderDataRaw = bc$plotHeaderDataRaw,
    damageAgentCodes = bc$pspBCdamageAgentCodes
  )
  check_data(bcClean$treeData)
  check_data(bcClean$plotHeaderData)

  # Ontario
  on <- prepInputsOntarioPSP(dPath = dPath)
  sppEquiv <- LandR::sppEquivalencies_CA
  onClean <- dataPurification_ONPSP(ONPSPlist = on, sppEquiv = sppEquiv)
  check_data(onClean$treeData)
  check_data(onClean$plotHeaderData)

  # Saskatchewan
  sk <- prepInputsSaskatchwanPSP(dPath = dPath)
  skClean <- dataPurification_SKPSP(
    SADataRaw = sk$SADataRaw,
    plotHeaderRaw = sk$plotHeaderRaw,
    measureHeaderRaw = sk$measureHeaderRaw,
    treeDataRaw = sk$treeDataRaw
  )
  check_data(skClean$treeData)
  check_data(skClean$plotHeaderData)

  # New Brunswick
  nb <- prepInputsNBPSP(dPath = dPath)
  nbClean <- dataPurification_NBPSP(nb)
  check_data(nbClean$treeData)
  check_data(nbClean$plotHeaderData)

  # Quebec
  qc <- prepInputsQCPSP(dPath = dPath)
  qcClean <- dataPurification_QCPSP(qc)
  check_data(qcClean$treeData)
  check_data(qcClean$plotHeaderData)

  # NFI
  nfi <- prepInputsNFIPSP(dPath = dPath)
  nfiClean <- dataPurification_NFIPSP(NFIdata = nfi)
  check_data(nfiClean$treeData)
  check_data(nfiClean$plotHeaderData)
})
