library(RODBC)
library(data.table)
readRenviron(".Renviron") #set uid and pwd to access MS SQL Server

#To follow the existing convention, there should be two functions, one that "gets" the raw data.tables and nothing else,
#and the purification function which will manipulate these appropriately to form two data.tables

#open database connection
ucon <- odbcConnect("PSPodbc", uid = Sys.getenv("uid"), pwd = Sys.getenv("pwd")) #add uid and pwrd. From environment?

#scratch
# sqlQuery(ucon,"select name from master.sys.sysdatabases where dbid > 4")
# psp <- sqlQuery(ucon,"select name from master.sys.sysdatabases where dbid > 4")

# Create query of previously loaded table data - ensure your schema is set
# query <- paste("SELECT [AgeHeaderKey],[VisitKey],[MsrDate] FROM [gyPSPPGP].[dbo].[tblAgeHeader]")

#to save time, wrap these in functions
getTables <- function(tablename, cols = "*") {
  return(paste0("SELECT ", cols, " FROM [gyPSPPGP].[dbo].[", tablename, "]"))
}
makeDT <- function(x, con = ucon) {
  return(as.data.table(sqlQuery(con, getTables(x))))
}

##### Location ####


#TODO: remove the treated plots
# Plot <- Plot[!PlotKey %in% treated$PlotKey]


#OVERVIEW
# to get plotID of a tree, connect treeMsr to treeGrowthPlot to treeHeader to Visit to Package to Plot -
#use Visit and Plot to create MeasureIDs, and Plot ID can be renamed "OrigPlotID1".
#however, you must renumber trees within growth plots and combine them (there are potentially 3)
#Visit Key is the "MeasureID", but
#to start, we need to get treeMsrKey with treeGrowthPlot key, and then treeheader to Visit.

#Conceptually, this should return up to 4 tables (tree, treeHeader, plot before you joined it, and maybe age?)
#better to include columns than drop them, in case we need them. e.g. damage codes, mortality
#then dataPurification_OntarioPSP will turn these data.tables into two table:
#one with treeHeader/plot/age combined, and one with treeHeader/tree.

#####fix plot id columns ####
Plot <- makeDT("tblPlot")
LocCoord <- makeDT("tblLocCoord")
# LocPlot <- makeDT("tblLocPlot")
#41 plots have no location, of which 39 are PSP (not important) and 2 PGP. Unclear why these are missing
#the same plot has two locations due to coordtypeCode
Plot <- LocCoord[Plot, on = "PlotKey"]
#CoordTypeCode: 1 = road post, 3 = location post, from tlkCoordType
#TODO: investigate the missing locations -
#note that not all plots are unique locations
investigateMissingLoc <- Plot[is.na(CoordTypeCode)]
setkey(Plot)
Plot <- Plot[CoordTypeCode == 3]
Plot <- Plot[, .SD, .SDcols = c("PlotKey", "PlotName", "Easting",
                                "Northing", "Datum", "DatasetCode")]
rm(LocCoord)
####Identifying treated plots ####
standInfoTreatment <- makeDT("tblStandInfoTreat")
treatType <- makeDT("tlkpTreatType")
standInfoTreatment <- treatType[standInfoTreatment, on = c("TreatTypeCode")]
rm(treatType)
treated <- Plot[PlotKey %in% standInfoTreatment$PlotKey]
treated <- treated[standInfoTreatment, on = c("PlotKey")]
#328 plots have some kind of treatment (planted, seeded, thinning, pesticide, herbicide, etc)
#1 - treeMsrkey with treeGrowthPlot
Visit <- makeDT("tblVisit")
VisitType <- makeDT("tlkpVisitType")
Visit <- VisitType[Visit, on = c("VisitTypeCode")]
Visit$VisitTypeName <- as.factor(Visit$VisitTypeName)
rm(VisitType)


#tree Growth Plot has up to 3 growth plots for a single tree header key
treeGrowthPlot <- makeDT("tblTreeGrowthPlot")
treeMsr <- makeDT("tblTreeMsr")
treeMsr <- treeMsr[, .(TreeMsrKey, TreeGrowthPlotKey, TreeKey, TreeStatusCode, DBH, CrownClassCode)]
treeMsr <- treeGrowthPlot[treeMsr, on = ("TreeGrowthPlotKey")]
#calculate area

#2- connect treeheader with Visit and Package
#Package contains PlotKey and package Key, which connects Visit key, which connects to TreeHeader
treeHeader <- makeDT("tblTreeHeader") #has headerkey, VisitKey, and MsrDate.
#there are visits which have nothing to do with trees -
treeHeader <- Visit[treeHeader, on = c("VisitKey")] #getting package key from Package
Package <- makeDT("tblPackage")
treeHeader <- Package[, .(PackageKey, PlotKey, StartYear)][treeHeader, on = c("PackageKey")]
treeHeader[, c("ManualCode", "VisitTypeCode") := NULL]
#to connect plot location and tree:
#treeGrowthPlotKey in treeMsr -> treeGrowthPlotKey in treeGrowthPlot;
#treeHeader in treeGrowthPlot -> treeHeader in treeHeader;
#PlotKey in treeHeader -> PlotKey in Plot
smallGrowthPlotKey <- treeGrowthPlot[, .(TreeGrowthPlotKey, TreeHeaderKey, GrowthPlotNum, TreeRenumber)]
plotHeaderData <- treeHeader[Plot, on = c("PlotKey")]
plotHeaderData[, c("DatasetCode", "StartYear") := NULL]
#TODO: for now I am removing location to make a leaner table
plotHeaderData[, c("Easting", "Northing", "Datum") := NULL]
plotHeaderData <- smallGrowthPlotKey[plotHeaderData, on = c("TreeHeaderKey")]
plotHeaderData[, VisitTypeName := NULL]
#TODO confirm that the reason this table is < rows than smallGrowthPlot is the loss of treated plots
#semi confirmed - all but two (PlotKey 7565 and 7566)
#####height#####
treeMsr[, plotArea := Width * Length]
treeMsr[is.na(plotArea), plotArea := round(Radius^2*3.142)]
treeMsr <- treeMsr[, .SD, .SDcol = colnames(treeMsr)[!colnames(treeMsr) %in%
                                                       c("LocAzi", "Radius", "LocDist", "Width", "Length")]]
treeHeight <- makeDT("tblHt")
treeMsr <- treeHeight[, .(TreeMsrKey, HtTot)][treeMsr, on = c("TreeMsrKey")]
rm(treeHeight)

####species####
tree <- makeDT("tblTree")
specLk <- makeDT("tlkpSpec")
specLk[, fullGenusSpec := paste(SpecGenus, SpecSpec)]
tree <- specLk[, .(SpecCommon, fullGenusSpec, SpecCode)][tree, on = c("SpecCode")]
tree <- tree[, c("PostNum", "Dist", "Azi", "MortCauseCode", "TagTypeCode") := NULL]
tree <- tree[treeMsr, on = c("TreeKey")]
rm(specLk)

#####age####
#connect ageHeader with plots for purpose of stand age
ageHeader <- makeDT("tblAgeHeader") #connects visit key (msr purpose) with other age data
ageHeader <- Visit[ageHeader, on = c("VisitKey")]
ageHeader <- Package[ageHeader, on = c("PackageKey")]
#now we can connect with plot
ageHeader[, c("ResidCmpnt", "ManualCode", "VisitTypeName", "VisitTypeCode",
              "CoOpMethod", "ApproachCode", "StartYear") := NULL]
ageHeader <- Plot[, .(PlotKey, PlotName)][ageHeader, on = c("PlotKey")]
#MsrDate cannot be used for joins even though it is in both tables as it will vary by day, and not a key (unlike VisiKey)
#get measurements of individual trees
ageTree <- makeDT("tblAgeTree")
#crownclass codes C D and E are code for codominant, dominant, emergent
ageTree[, c("SiteCondCode", "HtLiveBranch", "MortAzi", "MortDist", "HtLiveFoliage", "HtToDBH") := NULL]
ageSample <- makeDT("tblAgeSample")
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
ageSampleType <- makeDT("tlkpAgeSampleType")
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
rm(ageHeader, ageSample, ageSampleType, ageTree, standAgesOther, standAgesDominant)

####Canopy Origin ####
#TODO: connect standInfo via MsrKey to the plots, and drop those that were planted - aka 4:6
standInfoHeader <- makeDT("tblStandInfoHeader")
#some stands originated from old fields and from harvest
mainCanopyOrigin <- makeDT("tlkpMainCanopyOrigin")
standInfoHeader <- mainCanopyOrigin[standInfoHeader, on = c("MainCanopyOriginCode")]
standInfoHeader[, c("PlotUnifRationale", "MaturClassRationale") := NULL]
rm(mainCanopyOrigin)


###### prep for modules #####
#a bit of clean up
#1.aggregate the growthplots so that each PlotName has one set of tree numbers
tree[, c("SpecCode", "CrownClassCode", "CrownClsr", PlotMapGrowthPlotKey) := NULL]
#filter out those with no measurement of DBH
tree <- tree[!is.na(DBH)]
#correct growthPlot numbers

#correct Plot area


#filter for only natural stands
plotHeaderData[TreeHeaderKey %in% hasPlanted]
#filter for only those with age

#filter out dead trees
#Cut, Dead, Dead Veteran, Exclude, Gone (X), Live, Live Veteran - from tlkpTreeStatus
tree <- tree[TreeStatusCode %in% c("L", "V"]
tree[, TreeStatusCode := NULL]
#filter for no planted trees anywhere
####drop dead trees and planted trees
treeOrigin <- makeDT("tlkpTreeOrigin")
tree <- treeOrigin[tree, on = c("TreeOriginCode")]
#for later - we do not want plots with planted trees
hasPlanted <- unique(tree[TreeOriginCode %in% c("P", "p")]$TreeHeaderKey)

#filter for those missing locatiosn

#filter for no DBH

#some age trees have no DBH - these plots are dropped from plot measurements as they will give biased density

#standarize species name
