globalVariables(c(
  "coordinates", "estimatedElevation", "id2", "geometry", "baseSA"
))

#' standardize and treat the BC PSP data
#'
#' @param dPath directory to download elevation data
#' @param filename2 the full filename of the output file
#' @param PSPplot the standardized PSP plot attribute data
#' @param PSPgis the standardized plot location sf object
#'
#' @return a text file
#'
#' @export
#' @importFrom data.table as.data.table setnames setcolorder fwrite
#' @importFrom terra extract
#' @importFrom sf as_Spatial st_coordinates
#' @importFrom reproducible prepInputs
#'

# run the rmd to get standardized PSP object
# elevation will produce a much better climate prediction, even if it isn't strictly necessary
# previous work showed that when elevation is missing,
# it is better to sample elevation from a DEM than to omit it
prepPSP_climateNA <- function(dPath, filename2, PSPplot, PSPgis) {
  # get an elevation DEM - this is the 7.5 arcsecond DEM used for fireSense, from GTOPO
  CanadaDEM <- prepInputs(
    url = "https://drive.google.com/file/d/121x_CfWy2XP_-1av0cYE7sxUfb4pmsup/",
    destinationPath = dPath
  )

  if (is.null(PSPplot$Elevation)) {
    PSPplot[, Elevation := NA]
  }

  missingIDs <- PSPplot[is.na(Elevation), ]$OrigPlotID1
  missingElevation <- PSPgis[PSPgis$OrigPlotID1 %in% missingIDs, ]
  hasElevation <- PSPgis[!PSPgis$OrigPlotID1 %in% missingIDs, ]

  estimatedElevation <- terra::extract(CanadaDEM, missingElevation)
  missingElevation$Elevation <- estimatedElevation[2]
  if (anyNA(missingElevation$estimatedElevation)) {
    warning("Extracting elevation from DEM has failed for some plots")
  }

  newPSPgis <- rbind(hasElevation, missingElevation)
  PSPcoord <- as.data.table(newPSPgis)
  coords <- as.data.table(st_coordinates(newPSPgis))
  PSPcoord[, geometry := NULL]
  PSPcoord <- cbind(PSPcoord, coords)

  # prep for climateNA
  PSPcoord[, baseSA := NULL]
  PSPcoord <- setcolorder(PSPcoord, neworder = c("OrigPlotID1", "X", "Y", "Elevation"))
  # the columns have to be called "id1", "id2", "lat", "long", "elev"
  # sampleData <- fread("C:/users/ieddy/Downloads/ClimateNA_v640/inputFiles/input_test.csv")
  setnames(
    PSPcoord,
    # c("OrigPlotID1", "X", "Y", "Elevation"),
      new = c("id1", "long", "lat", "elev")
  )
  PSPcoord[, id2 := ""]
  setcolorder(PSPcoord, neworder = c("id1", "id2", "lat", "long", "elev"))
  PSPcoord <- unique(PSPcoord)
  message("writing PSP locations data to csv for climateNA")
  fwrite(PSPcoord, file = filename2)
}
