local_edition(3)

## one Quebec plot at 350 m, measured in 2000 and 2010; one dominant black spruce aged 60 in 2000
qc_fixture <- function() {
  list(
    PLACETTE = data.table::data.table(ID_PE = 1001, NO_PE = 1, LATITUDE = 48.5, LONGITUDE = -72.5),
    PLACETTE_MES = data.table::data.table(
      ID_PE = 1001,
      NO_MES = c(1, 2),
      ID_PE_MES = c(100101, 100102),
      DATE_SOND = c("07/15/2000", "07/15/2010")
    ),
    STATION_PE = data.table::data.table(
      ID_PE = 1001,
      ID_PE_MES = c(100101, 100102),
      ALTITUDE = 350,
      ORIGINE = "",
      PERTURB = ""
    ),
    DENDRO_ARBRES_ETUDES = data.table::data.table(
      ID_PE = 1001, ID_ARBRE = 1, NO_MES = 1, ID_PE_MES = 100101, NO_ARBRE = 1,
      ID_ARB_MES = 1, ETAT = 10, ESSENCE = "EPN", HAUT_ARBRE = 150, AGE_SANSOP = NA,
      DHP = 125, CL_QUAL = NA, ETAGE_ARB = "D", AGE = 60, SOURCE_AGE = 4
    ),
    DENDRO_ARBRES = data.table::data.table(
      ID_PE = 1001,
      ID_PE_MES = c(100101, 100102),
      NO_ARBRE = 1,
      ETAT = "10",
      ESSENCE = "EPN",
      DHP = c(125, 140)
    )
  )
}

test_that("dataPurification_QCPSP() keeps ALTITUDE as Elevation and sets minDBH to 9 cm", {
  sppEquiv <- data.table::data.table(Latin_full = "Picea mariana", QCPSP = "EPN")
  out <- dataPurification_QCPSP(qc_fixture(), sppEquiv = sppEquiv)

  plots <- out$plotHeaderData
  expect_identical(plots$MeasureID, c("QCPSP_100101", "QCPSP_100102"))
  expect_identical(plots$Elevation, c(350, 350))
  expect_identical(plots$minDBH, c(9, 9))
  expect_identical(plots$baseSA, c(60L, 60L))

  ## DHP is recorded in mm; DBH and minDBH are in cm
  expect_identical(out$treeData$DBH, c(12.5, 14))
})
