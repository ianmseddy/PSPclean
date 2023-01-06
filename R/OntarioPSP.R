globalVariables(c(
  ":=", "trees_measurement_comment", "tree_origin", "stand_origin", "dbh_age", "maxN",
  "stump_age", "total_age", "crown_class", "measurement_year", "stand_age", "tree_location_id",
  "condition_code1", "condition_code2", "condition_code3", "cause", "cause1", "cause2",
  "cause3", ".N", "N", "sizes", "tempyear", "totalBad", "PlotSize", "tree_plot_area", "MeasureID", "latitude",
  "longitude", "OrigPlotID1", "Species", "MeasureYear", "company_plot_number", "tree_number",
  "species", "minMeasure", "measurement_number", "dbh", "height", "elevation"
))

#' standardize and treat the Ontario PSP data
#'
#' @param ONPSPlist list of relevant plots
#' @param sppEquiv table of species names - see \code{LandR::sppEquiv}-
#' must have column 'latin' and 'PSP'
#' @param excludeAllObs if removing observations of individual trees due to damage codes,
#'
#' @return a list of plot and tree data.tables
#'
#' @export
#' @importFrom data.table copy data.table set setcolorder setkey
#'
dataPurification_ONPSP <- function(ONPSPlist, sppEquiv, excludeAllObs = TRUE){

#TODO: review excludeAllObs - I dont' think we exclude anything at the moment

##### Location ####
Plot <- ONPSPlist[["tblPlot"]]
LocCoord <- ONPSPlist[["tblLocCoord"]]
#41 plots have no location, of which 39 are PSP (not important) and 2 PGP. Unclear why these are missing
#the same plot has two locations due to coordtypeCode
Plot <- LocCoord[Plot, on = "PlotKey"]
#CoordTypeCode: 1 = road post, 3 = location post, from tlkCoordType
#note that not all plots are unique locations
investigateMissingLoc <- Plot[is.na(CoordTypeCode)]
setkey(Plot)
Plot <- Plot[CoordTypeCode == 3]
Plot <- Plot[, .SD, .SDcols = c("PlotKey", "PlotName", "Easting",
                                "Northing", "Datum", "DatasetCode")]
rm(LocCoord)
####Identifying treated plots ####
standInfoTreatment <- ONPSPlist[["tblStandInfoTreat"]]
treatType <- ONPSPlist[["tlkpTreatType"]]
standInfoTreatment <- treatType[standInfoTreatment, on = c("TreatTypeCode")]
rm(treatType)
treated <- Plot[PlotKey %in% standInfoTreatment$PlotKey]
treated <- treated[standInfoTreatment, on = c("PlotKey")]

Visit <- ONPSPlist[["tblVisit"]]
VisitType <- ONPSPlist[["tlkpVisitType"]]
Visit <- VisitType[Visit, on = c("VisitTypeCode")]
Visit$VisitTypeName <- as.factor(Visit$VisitTypeName)
rm(VisitType)

treeGrowthPlot <- ONPSPlist[["tblTreeGrowthPlot"]]
treeMsr <- ONPSPlist[["tblTreeMsr"]]
treeMsr <- treeMsr[, .(TreeMsrKey, TreeGrowthPlotKey, TreeKey, TreeStatusCode, DBH, CrownClassCode)]
treeMsr <- treeGrowthPlot[treeMsr, on = ("TreeGrowthPlotKey")]

#2- connect treeheader with Visit and Package
treeHeader <- ONPSPlist[["tblTreeHeader"]] #has headerkey, VisitKey, and MsrDate.
treeHeader <- Visit[treeHeader, on = c("VisitKey")] #getting package key from Package
Package <- ONPSPlist[["tblPackage"]]
treeHeader <- Package[, .(PackageKey, PlotKey, StartYear)][treeHeader, on = c("PackageKey")]
treeHeader[, c("ManualCode", "VisitTypeCode") := NULL]

# connect plot location and tree:
#treeGrowthPlotKey in treeMsr -> treeGrowthPlotKey in treeGrowthPlot;
#treeHeader in treeGrowthPlot -> treeHeader in treeHeader;
#PlotKey in treeHeader -> PlotKey in Plot
treeGrowthPlot[Radius == 11.26, Radius := 11.28]
treeGrowthPlot[Radius == 11.33, Radius := 11.28]
treeGrowthPlot[, plotArea := Width * Length]
treeGrowthPlot[is.na(plotArea), plotArea := round(Radius^2*3.142)]
#some plots change from radius 11.33 to 11.28 over time - leading to differing areas.
#IMO this area discrepancy is allowable (< 1%) but others are drastic and should be excluded

smallGrowthPlotKey <- treeGrowthPlot[, .(TreeGrowthPlotKey, TreeHeaderKey, GrowthPlotNum, TreeRenumber, plotArea)]
plotData <- treeHeader[Plot, on = c("PlotKey")]
plotData[, c("DatasetCode", "StartYear") := NULL]
plotData[, c("Easting", "Northing", "Datum") := NULL]

plotData <- smallGrowthPlotKey[plotData, on = c("TreeHeaderKey")]
plotData[, VisitTypeName := NULL]
#TODO confirm that the reason this table is < rows than smallGrowthPlot is the loss of treated plots
# partially confirmed - all but two (PlotKey 7565 and 7566)
rm(smallGrowthPlotKey)

#####height#####
treeMsr <- treeMsr[, .SD, .SDcol = colnames(treeMsr)[!colnames(treeMsr) %in%
                                                       c("LocAzi", "Radius", "LocDist", "Width", "Length")]]
treeHeight <- ONPSPlist[["tblHt"]]
treeMsr <- treeHeight[, .(TreeMsrKey, HtTot)][treeMsr, on = c("TreeMsrKey")]
rm(treeHeight)

####species####
tree <- ONPSPlist[["tblTree"]]
specLk <- ONPSPlist[["tlkpSpec"]]
specLk[, fullGenusSpec := paste(SpecGenus, tolower(SpecSpec))]
tree <- specLk[, .(SpecCommon, fullGenusSpec, SpecCode)][tree, on = c("SpecCode")]
tree <- tree[, c("PostNum", "Dist", "Azi", "MortCauseCode", "TagTypeCode") := NULL]
tree <- tree[treeMsr, on = c("TreeKey")]
rm(specLk, treeMsr)

#clean up
tree[, TreeStatusCode := gsub(pattern = " ", replacement = "", x = TreeStatusCode)]
tree[, CrownClsr := NULL]

#####age####
#connect ageHeader with plots for purpose of stand age
ageHeader <- ONPSPlist[["tblAgeHeader"]] #connects visit key (msr purpose) with other age data
ageHeader <- Visit[ageHeader, on = c("VisitKey")]
ageHeader <- Package[ageHeader, on = c("PackageKey")]
#now we can connect with plot
ageHeader[, c("ResidCmpnt", "ManualCode", "VisitTypeName", "VisitTypeCode",
              "CoOpMethod", "ApproachCode", "StartYear") := NULL]
ageHeader <- Plot[, .(PlotKey, PlotName)][ageHeader, on = c("PlotKey")]
#MsrDate cannot be used for joins even though it is in both tables as it will vary by day, and not a key (unlike VisiKey)
#get measurements of individual trees
ageTree <- ONPSPlist[["tblAgeTree"]]
#crownclass codes C D and E are code for codominant, dominant, emergent
# ageTree[, c("SiteCondCode", "HtLiveBranch", "MortAzi", "MortDist", "HtLiveFoliage", "HtToDBH") := NULL]
ageSample <- ONPSPlist[["tblAgeSample"]]
#join so we get plot ID
temp <- ageHeader[, .(AgeHeaderKey, PlotName, PlotKey, FieldSeasonYear)]
ageTree <- temp[ageTree, on = c("AgeHeaderKey")]
rm(temp)
#there are two plots with missing IDs  - ageHeaderKey 9466 and 9467.
#also note that AgeSample does not have AgeTreeKey 20639, and 165 others,
#though these exist the AgeTree table

treeAges <- ageSample[, .(AgeSampleKey, AgeTreeKey, FieldAge, OfficeAge,
                          AgeSampleStatusCode, AgeSampleTypeCode)]
treeAges <- treeAges[ageTree, on = c("AgeTreeKey")]
ageSampleType <- ONPSPlist[["tlkpAgeSampleType"]]
treeAges <- ageSampleType[treeAges, on = ("AgeSampleTypeCode")]

#1 DBH Core and 4 DBH Cookie need to be adjusted to match Base
treeAges <- treeAges[, .(AgeSampleKey, AgeTreeKey, FieldAge, OfficeAge, AgeSampleTypeCode,
                         AgeSampleStatusCode, GrowthPlotNum, FieldSeasonYear, TreeNum, CrownClassCode, DBH)]
#sometimes the base will be wrong due to rot, etc. So Only use C - Complete
treeAges <- treeAges[AgeSampleStatusCode == "C"]
#remove the non dominant crown class
treeAges[, CrownClassCode := gsub(pattern = " ", replacement = "", x = CrownClassCode)]
treeAges <- treeAges[CrownClassCode %in% c("C", "D")]
#treat DBH and Base measurements as separate columns, calculate difference when both are measured
#use this to standardize measurements (as done by Yong Luo in Alberta) by adding difference to DBH
treeAges[AgeSampleTypeCode %in% c(1, 4), ageMethod := "DBH"] #DBH Core and DBH cookie
treeAges[AgeSampleTypeCode %in% c(2, 3), ageMethod := "Base"] #Base Core and Base cookie
treeAges <- treeAges[!is.na(AgeSampleKey)] #get rid of age trees with no samples
#cast the multiple age measurements - there may be a way to drop the NA columns

treeAges <- dcast(treeAges, AgeTreeKey + GrowthPlotNum + FieldSeasonYear +
                    TreeNum + CrownClassCode + DBH  ~ ageMethod,
                  value.var = c("FieldAge", "OfficeAge"), fun = mean, drop = c(TRUE))
#correct obvious mistakes
treeAges[AgeTreeKey == 80459, OfficeAge_Base := 73] #they are clearly missing the 7 - age is 3 otherwise
treeAges[AgeTreeKey == 43397, FieldAge_Base := 94] #they are clearly missing the 9

#standardize DBH and Base measurements
treeAges[!is.na(FieldAge_DBH) & !is.na(FieldAge_Base), FieldAge_diff := FieldAge_Base - FieldAge_DBH]
treeAges[!is.na(OfficeAge_DBH) & !is.na(OfficeAge_Base), OfficeAge_diff := OfficeAge_Base - OfficeAge_DBH]
meanFieldAgeDiff <- as.integer(mean(treeAges$FieldAge_diff, na.rm = TRUE))
meanOfficeAgeDiff <- as.integer(mean(treeAges$OfficeAge_diff, na.rm = TRUE))
treeAges[, c("FieldAge_diff", "OfficeAge_diff") := NULL]
#apply difference
treeAges[is.na(OfficeAge_Base) & !is.na(OfficeAge_DBH), OfficeAge_Base := OfficeAge_DBH + meanOfficeAgeDiff]
treeAges[is.na(FieldAge_Base) & !is.na(FieldAge_DBH), FieldAge_Base := FieldAge_DBH + meanFieldAgeDiff]
treeAges[is.nan(OfficeAge_Base), OfficeAge_Base := NA]
treeAges[is.nan(FieldAge_Base), FieldAge_Base := NA]
treeAges[, c("OfficeAge_DBH", "FieldAge_DBH") := NULL]
treeAges[, unifiedAge := OfficeAge_Base]
treeAges[is.na(unifiedAge), unifiedAge := FieldAge_Base]
treeAges <- treeAges[!is.na(unifiedAge)]
#view 1328596PIP to see why this is necessary - some trees have sneaky bad data
temp <- ageTree[, .(AgeHeaderKey, AgeTreeKey)]
treeAges <- temp[treeAges, on = c("AgeTreeKey")]
treeAges <- ageHeader[treeAges, on = c("AgeHeaderKey", "FieldSeasonYear")]
rm(temp, meanOfficeAgeDiff, meanFieldAgeDiff)
treeAges[, firstMsrYear := min(FieldSeasonYear), .(PlotName)]

#For jurisdictiosn with no estimated standa age,
#the PSPclean approach derives standAge as mean of N trees with crown class Dominant, where N > 1.
#if none are available, we take the mean of all dominant and codominant trees
treeAges[, nDominant := sum(CrownClassCode == "D "), .(PlotName)]
treeAges[, nCodominant := sum(CrownClassCode == "C "), .(PlotName)]
treeAges[, standardizedAge := unifiedAge - FieldSeasonYear + firstMsrYear] #standardize
standAgesDominant <- treeAges[nDominant > 1 & CrownClassCode == "D",
                              .(meanStandAge = as.integer(mean(standardizedAge))), .(PlotName, firstMsrYear)]
standAgesOther <- treeAges[nDominant < 2,
                           .(meanStandAge = as.integer(mean(standardizedAge, na.rm = TRUE))),
                           .(PlotName, firstMsrYear)]
standAges <- rbind(standAgesDominant, standAgesOther)
rm(ageHeader, ageSample, ageSampleType, ageTree, standAgesOther, standAgesDominant, treeAges, Visit)

####Canopy Origin ####
standInfoHeader <- ONPSPlist[["tblStandInfoHeader"]]
#some stands originated from old fields and from harvest
mainCanopyOrigin <- ONPSPlist[["tlkpMainCanopyOrigin"]]
standInfoHeader <- mainCanopyOrigin[standInfoHeader, on = c("MainCanopyOriginCode")]
standInfoHeader[, c("PlotUnifRationale", "MaturClassRationale") := NULL]
rm(mainCanopyOrigin)


###### prep for modules #####
plotData <- unique(plotData[, .(PlotName, TreeGrowthPlotKey, plotArea,
                                            FieldSeasonYear, MsrDate, GrowthPlotNum)])
#add relevant columns to tree
tree <- plotData[, .(PlotName, FieldSeasonYear, TreeGrowthPlotKey)][tree, on = c("TreeGrowthPlotKey")]
#filter out dead trees - done first so that plots aren't excluded because of unmeasured dead trees
#Cut, Dead, Dead Veteran, Exclude, Gone (X), Live, Live Veteran - from tlkpTreeStatus
tree <- tree[TreeStatusCode %in% c("L", "V")]
tree[, c("TreeStatusCode", "SpecCode") := NULL]
#filter out missing DBH
missingDBH_TGPK <- unique(tree[is.na(DBH),]$TreeGrowthPlotKey)
tree <- tree[!TreeGrowthPlotKey %in% missingDBH_TGPK]
# rm(missingDBH_TGPK)
#identify the plots with re-numbered trees, these can be kept but need new PlotNames
needNewPlotNames <- unique(tree[TreeRenumber == 1,]$TreeGrowthPlotKey)
#remove planted stands (and natural stands with planted trees)

#1584 plots have planted trees - remove individual growth plots.
####drop dead trees and planted trees
treeOrigin <- ONPSPlist[["tlkpTreeOrigin"]]
tree <- treeOrigin[tree, on = c("TreeOriginCode")]
hasPlanted <- unique(tree[TreeOriginCode %in% c("P", "p")]$TreeGrowthPlotKey)
tree <- tree[!TreeGrowthPlotKey %in% hasPlanted]
rm(hasPlanted, treeOrigin)
tree[, TreeOriginCode := NULL]

#filter for only natural stands
#328 plots have some kind of treatment (planted, seeded, thinning, pesticide, herbicide, etc)
#some plots can be kept because measuring happened first - 32 growth plots
treatedBad <- plotData[treated[, .(PlotName, TreatYear, TreatTypeName)], on = c("PlotName")]
treatedBad <- treatedBad[FieldSeasonYear >= TreatYear]
tree <- tree[!TreeGrowthPlotKey %in% treatedBad$TreeGrowthPlotKey,]
rm(treated, treatedBad)
#use treeKey for treeNumber, as it is unique to each plot but not through time.
tree[, c("TreeNum", "TreeRenumber", "PlotMapGrowthPlotKey", "Section") := NULL]
setnames(tree, "TreeKey", "TreeNumber")
#correct growthPlot numbers using factor of growthPlot_treeNumber. Confirm rerenumber, oldNumber, treeNumber

#standardize species names - for biomass estimation
sppEquiv <- sppEquiv[, .(Latin_full, PSP)]
setnames(sppEquiv, new = c("fullGenusSpec", "newSpeciesName"))
sppEquiv <- unique(sppEquiv)

#fix a few codings to match PSP - there is no biomass equation for species-specific willow anyway
tree[fullGenusSpec %in% c("Salix sp (tree)", "Salix alba", "Salix babylonica"),
                          fullGenusSpec := "Salix spp."]
#this seems most likely, among populus spp with biomass equations
tree[fullGenusSpec %in% c("Populus x", "Populus sp"), fullGenusSpec := "Populus balsamifera"]
tree[fullGenusSpec == "Acer saccharum ssp. nigrum", fullGenusSpec := "Acer saccharum"]
tree <- sppEquiv[tree, on = ("fullGenusSpec")]
tree[SpecCommon == "Unknown Hardwood", newSpeciesName := "unknown hardwood"]
tree[is.na(newSpeciesName), newSpeciesName := SpecCommon]
tree[, c("OriginName", "fullGenusSpec", "OrigTreeNum") := NULL]


####final clean up of Plot ####
rm(standInfoTreatment, standInfoHeader, Package)


#filter for those missing locations
plotData <- plotData[!PlotName %in% investigateMissingLoc$PlotName]

##filter for only those with age
setnames(standAges, c("firstMsrYear", "meanStandAge"), new = c("baseYear", "baseSA"))
plotData <- standAges[plotData, on = c("PlotName")]
plotData <- plotData[!is.na(baseSA)]

#standardize area -
#some plots have multiple plot sizes -
#some differences are trivial (11.33m radius vs. 11.28m) but others change by > 100 m2
#page 79 of the manual explains some plots have been expanded over time
#but they should not have been reduced (though FCTEM2001038PGP appears to have been reduced by 100m2)
#either way, reject any where area differs by > 5m2
plotData <- plotData[TreeGrowthPlotKey %in% tree$TreeGrowthPlotKey]
plotData[, plotArea := sum(plotArea), .(PlotName, FieldSeasonYear)]
badAreas <- plotData[, .(numAreas = length(unique(plotArea))), .(PlotName)]
badAreas <- badAreas[numAreas > 1]

plotData <- plotData[!PlotName %in% badAreas$PlotName]
plotData[, GrowthPlotNum := NULL]
#add location info

plotData <- Plot[, .(PlotName, Easting, Northing, Datum)][plotData, on = c("PlotName")]

plotData[, MeasureID := as.factor(paste0(PlotName, FieldSeasonYear))]
plotData[, MeasureID := as.factor(paste0("ONPSP_", as.numeric(MeasureID)))]
setnames(plotData, c("PlotName", "FieldSeasonYear", "plotArea"  ), c("OrigPlotID1", "MeasureYear", "PlotSize"))
plotData[, c("MsrDate", "TreeGrowthPlotKey") := NULL]
plotData <- unique(plotData) #treat different growth plots within a plot as one

####Final clean up of Tree####
#use TreeNumber - MeasureID will come from paste of PlotName_FieldSeasonYear

tree[, c("CrownClassCode", "TreeHeaderKey") := NULL]
setnames(tree, c("HtTot", "FieldSeasonYear", "PlotName", "SpecCommon"), c("Height", "MeasureYear", "OrigPlotID1", "Species"))
tree <- tree[plotData[, .(MeasureYear, OrigPlotID1, MeasureID)], on = c("OrigPlotID1", "MeasureYear")]
#some tree measurements wil be dropped as the plots were filtered out

#TODO: review needNewPlotNames - these are renumbered tree

tree[, c("TreeGrowthPlotKey", "TreeMsrKey", "GrowthPlotNum") := NULL]
plotData[, OrigPlotID1 := as.factor(paste0("ONPSP_", OrigPlotID1))]
tree[, OrigPlotID1 := as.factor(paste0("ONPSP_", OrigPlotID1))]
plotData[, Datum := as.factor(Datum)]

browser()
setkey(tree, MeasureID, OrigPlotID1, MeasureYear, TreeNumber, Species, DBH, Height, newSpeciesName)
setcolorder(tree)

setkey(plotData, OrigPlotID1, MeasureID, MeasureYear)
setcolorder(plotData)

return(list(plotHeaderData = plotData,
            treeHeaderData = tree))
}

#' retrieve preprocessed Ontario PSP and PGP data
#'
#' @param dPath the Access database for PSP and PGP plots
#' @param ... additional args passed to prepInputs
#'
#' @return a list of plot and tree data.tables
#'
#' @export
#' @importFrom data.table fread

prepInputsOntarioPSP <- function(dPath, ...) {
  toget <- c("tblAgeSample.csv", "tblAgeTree.csv", "tblLocCoord.csv",
             "tblPackage.csv", "tblPlot.csv", "tblStandInfoHeader.csv",
             "tblStandInfoTreat.csv", "tblTree.csv", "tblTreeGrowthPlot.csv",
             "tblTreeHeader.csv", "tblTreeMsr.csv", "tblVisit.csv", "tlkpAgeSampleType.csv",
             "tlkpMainCanopyOrigin.csv", "tlkpSpec.csv", "tlkpTreatType.csv",
             "tlkpTreeOrigin.csv", "tlkpVisitType.csv", "tblHt.csv")

  actual_dPath <- file.path(dPath, "OntarioPSP")
  out <- prepInputs(url = "https://drive.google.com/file/d/1ca6TC7952cU4M2dJkT68IOsFmObaDrQn/view?usp=share_link",
                    targetFile = "tblAgeHeader.csv",
                    destinationPath = actual_dPath,
                    alsoExtract = toget,
                    fun = "fread",
                    ...)

  actualOut <- file.path(actual_dPath, c(toget, "tblAgeHeader.csv"))
  actualNames <- sub(basename(actualOut), pattern = ".csv", replacement = "")
  actualOut <- lapply(actualOut, fread)
  names(actualOut) <- actualNames
  return(actualOut)
}
