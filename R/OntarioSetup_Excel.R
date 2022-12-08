library(RODBC)
library(data.table)
readRenviron(".Renviron") #set uid and pwd to access MS SQL Server

#To follow the existing convention, there should be two functions, one that "gets" the raw data.tables and nothing else,
#and the purification function which will manipulate these appropriately to form two data.tables

#open database connection
# ucon <- odbcConnect("PSPodbc", uid = Sys.getenv("uid"), pwd = Sys.getenv("pwd")) #add uid and pwrd. From environment?

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
Plot <- makeDT("tblPlot")
LocCoord <- makeDT("tblLocCoord")
#this has CoordTypeCode, 1 = road post, 3 = location post, from tlkCoordType
# LocPlot <- makeDT("tblLocPlot")
#41 plots have no location, of which 39 are PSP (not important) and 2 PGP. Unclear why these are missing
#~2000 have road locations and post locations
#the same plot has two locations due to coordtypeCode
Plot <- LocCoord[Plot, on = "PlotKey"]
#this has CoordTypeCode, 1 = road post, 3 = location post, from tlkCoordType
#TODO: investigate the missing locations -
#not all plots are unique locations...
investigateMissingLoc <- Plot[is.na(CoordTypeCode)]
setkey(Plot)
Plot <- Plot[CoordTypeCode == 3]
Plot[, N := .N, .(PlotKey)]
Plot <- Plot[, .SD, .SDcols = c("PlotKey", "PlotName", "Easting",
                                "Northing", "Datum", "DatasetCode")]
#It is unclear to me whether the PGPs inside a PSP have an associated PGP plot ID
# Plot[grep("PSP", PlotName)]


####Remove the treated plots ####
# standInfoHeader <- makeDT("tblStandInfoHeader") #not useful yet
standInfoTreatment <- makeDT("tblStandInfoTreat")
treatType <- makeDT("tlkpTreatType")
standInfoTreatment <- treatType[standInfoTreatment, on = c("TreatTypeCode")]
rm(treatType)
treated <- Plot[PlotKey %in% standInfoTreatment$PlotKey]
treated <- treated[standInfoTreatment, on = c("PlotKey")]
#328 plots have some kind of treatment (planted, seeded, thinning, pesticide, herbicide, etc)
#mainly planting and site prep and herbicide.
#it is possible that pre-treatment measurements are still useful -

#TODO: remove the treated plots
# Plot <- Plot[!PlotKey %in% treated$PlotKey]


# #visit types include non-measurement visits. Perhaps better to get Visit through age?
Visit <- makeDT("tblVisit")
VisitType <- makeDT("tlkpVisitType")
Visit <- VisitType[Visit, on = c("VisitTypeCode")]
Visit$VisitTypeName <- as.factor(Visit$VisitTypeName)
rm(VisitType)


#IAN OVERVIEW
# to get plotID of a tree, connect treeMsr to treeGrowthPlot to treeHeader to Visit to Package to Plot -
#use Visit and Plot to create MeasureIDs, and Plot ID can be renamed "OrigPlotID1".
#however, you must renumber trees within growth plots and combine them (there are potentially 3)
#Visit Key is the "MeasureID", but
#to start, we need to get treeMsrKey with treeGrowthPlot key, and then treeheader to Visit.

#Conceptually, this should return up to 4 tables (tree, treeHeader, plot before you joined it, and maybe age?)
#better to include columns than drop them, in case we need them. e.g. damage codes, mortality
#then dataPurification_OntarioPSP will turn these data.tables into two table:
#one with treeHeader/plot/age combined, and one with treeHeader/tree.

#1 - treeMsrkey with treeGrowthPlot
#tree Growth Plot has up to 3 growth plots for a single tree header key
treeGrowthPlot <- makeDT("tblTreeGrowthPlot")
treeMsr <- makeDT("tblTreeMsr")
treeMsr <- treeMsr[, .(TreeMsrKey, TreeGrowthPlotKey, TreeKey, TreeStatusCode, DBH, CrownClassCode)]
treeMsr <- treeGrowthPlot[treeMsr, on = ("TreeGrowthPlotKey")]
# treeMsr[,tempArea := round(Radius^2 * 3.1415, digits = 0)]
# treeMsr[is.na(tempArea), tempArea := Width * Length]
#calculate area with treeGrowthPlot (using treeGrowthPlotKey, not treeMsr)
smallGrowthPlotKey <- treeGrowthPlot[, .(TreeGrowthPlotKey, TreeHeaderKey, GrowthPlotNum, TreeRenumber)]

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
plotHeaderData <- treeHeader[Plot, on = c("PlotKey")]
plotHeaderData[, c("DatasetCode", "StartYear") := NULL]
#TODO: for now I am removing location to make a leaner table
plotHeaderData[, c("Easting", "Northing", "Datum") := NULL]
plotHeaderData <- smallGrowthPlotKey[plotHeaderData, on = c("TreeHeaderKey")]
plotHeaderData[, VisitTypeName := NULL]
#TODO confirm that the reason this table is < rows than smallGrowthPlot is the loss of treated plots
#semi confirmed - all but two
# PackageKey PlotKey StartYear VisitTypeName VisitTypeAbbrev VisitKey FieldSeasonYear TreeHeaderKey    MsrDate TreeGrowthPlotKey GrowthPlotNum
# 1:      21551    7565      2001 Establishment            Est     35892            2001         16930 2001-07-17             19781             1
# 2:      21552    7566      2001 Establishment            Est     35893            2001         16931 2001-07-18             19782             1

#it is different from fieldseason year in 162/10000 cases - sometimes by 14 years. Sometimes when it is "establishment"
#Yong previously used row names after setting key on OrigPlotID1, so the measurement ID would be sequential.

#####fix plot id columns ####
#TODO: figure out how to handle nested plots. we could ignore TreeGrowthPlot but only after we adjust the areas, tree numbers,
#and confirm measurement dates are equal (I would imagine they area), as well as other plot level info

# plotHeaderData[, OrigPlotID1 := paste0("ON", PlotKey)]
#Tree Header is not the correct key - we want TreeGrowthPlotKey, as there are multiple growth plots per plot
# plotHeaderData[, MeasureID := paste0("ONPSP", TreeHeaderKey)]
# plotHeaderData[, OrigPlotID2 := paste0("ON", )]

####figure out when####
#####height#####
# treeMsr[, tempArea := NULL]
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


#do this for tree
# plotHeaderData[, OrigPlotID1 := paste0("ON", PlotKey)]
# plotHeaderData[, MeasureID := paste0("ONPSP", TreeHeaderKey)]
# be aware that 1 PlotKey can have up to 3 GrowthPlots, which have repeating tree numbers - must fix this


###prep for LandRCS and Biomass_speciesParameters
####Canopy Origin ####
#TODO: connect standInfo via MsrKey to the plots, and drop those that were planted - aka 4:6
standInfoHeader <- makeDT("tblStandInfoHeader")
#some stands originated from old fields and from harest
mainCanopyOrigin <- makeDT("tlkpMainCanopyOrigin")
standInfoHeader <- mainCanopyOrigin[standInfoHeader, on = c("MainCanopyOriginCode")]
standInfoHeader[, c("PlotUnifRationale", "MaturClassRationale") := NULL]

rm(mainCanopyOrigin)

#####age####
#note the earliest measurement date in standInfo precedes the earliest visit field season year (1958 to 1980).
ageHeader <- makeDT("tblAgeHeader") #connects measurement date and visit key (msr purpose) with other age data
# plotHeaderData <- ageHeader[plotHeaderData, on = c("VisitKey", "MsrDate")]
ageTree <- makeDT("tblAgeTree")
# crownClassCode <- makeDT("tlkpCrownClass") #this just confirms C D and E are code for codominant, dominant, emergent
# ageTree <- crownClassCode[, .(CrownClassCode, CrownClassName)][ageTree, on = c("CrownClassCode")]
ageTree[, c("SiteCondCode", "HtLiveBranch", "MortAzi", "MortDist", "HtLiveFoliage", "HtToDBH") := NULL]
#types of crown class per plot
#there are 1000 plots where they did not record the crownclass
#keep only those that aren't planted and that have locations and no treatments
# subsetForAge <- plotHeaderData[!is.na(AgeHeaderKey), .(PlotKey, VisitKey, MsrDate, TreeGrowthPlotKey, TreeHeaderKey, PlotName)]
ageHeader <- plotHeaderData[ageHeader, on = c("VisitKey", "MsrDate")]
#some plots are NA because we excluded them already - revisit/
ageHeader <- ageHeader[!is.na(PlotName)]
# table(ageTree$CrownClassName)
#most are C, D, or I. A few are emergent but we probably don't want these?
#
####drop dead trees and planted trees
treeOrigin <- makeDT("tlkpTreeOrigin")
tree <- treeOrigin[tree, on = c("TreeOriginCode")]
#for later - we do not want plots with planted trees
hasPlanted <- unique(tree[TreeOriginCode %in% c("P", "p")]$TreeHeaderKey)
ageSample <- makeDT("tblAgeSample")

ageTree <- ageHeader[ageTree, on = c("AgeHeaderKey", "GrowthPlotNum")]
#TODO: confirm that the dropped trees are because of plots removed for planting, etc.
ageTree <- ageTree[!is.na(TreeGrowthPlotKey)]

# ageTreeSample <- ageSample[ageTree, on = ("AgeTreeKey")]
# ageTreeSample <- ageTreeSample[, .(AgeSampleKey, AgeTreeKey, AgeSampleTypeCode, FieldAge, OfficeAge,
#                                    CrownClassCode, AgeHeaderKey, GrowthPlotNum, TreeNum, DBH)]
# ageTreeSample <- ageTreeSample[ageHeader, on = c("AgeHeaderKey")]
ageSampleSmall <- ageSample[, .(AgeSampleKey, AgeTreeKey, FieldAge, OfficeAge)]
ageSampleSmall <- ageSampleSmall[ageTree, on = c("AgeTreeKey")]
ageSampleSmall <- ageSampleSmall[, .(AgeSampleKey, AgeTreeKey, FieldAge, OfficeAge, TreeGrowthPlotKey, GrowthPlotNum,
                           VisitTypeAbbrev, FieldSeasonYear, TreeNum, CrownClassCode)]
#remove the non dominant crown class
ageSampleSmall <- ageSampleSmall[CrownClassCode %in% c("C ", "D ", "E ")]
temp <- unique(ageTree[, .(TreeGrowthPlotKey, PlotKey)])
ageSampleSmall <- temp[ageSampleSmall, on = c("TreeGrowthPlotKey")]

#cleanup
rm(ageSample)

# ageTreeSample[, c("SiteCondCode", "HtToDBH", "HtLiveFoliage", "HtLiveBranch", "MortAzi", "MortDist") := NULL]
# ageTreeSample[, c("BarkThickness", "Incre5", "Incre10", "Incre15", "AgeSampleStatusCode") := NULL]

#figure out which, if any, have multiple measurements
# repeats <- ageTreeSample[!is.na(AgeSampleKey), .(NMeasures = length(unique(MsrDate))), .(PlotName)]
#what needs to happen
#first, use estimates if field age is present - second,
# testAge <- ageTreeSample[CrownClassName %in% c("Codominant", "Dominant", "Emergent")]
# testAge[!is.na(OfficeAge), FirstYearOfAgeMeasurement := min(year(MsrDate)), .(PlotName)]
# testAge[, firstMeasurementYear := min(as.numeric(year(MsrDate))), .(TreeHeaderKey)]
# testAge[, timeAfterFirstYear := year(MsrDate) - FirstYearOfAgeMeasurement]
# testAge[!is.na(OfficeAge), EstimatedAge := mean(OfficeAge), .()]

#filter for no planted trees anywhere
plotHeaderData[TreeHeaderKey %in% hasPlanted]
#filter for only those with age

#treeStatus (for whether it is alive), tllkTreeStatus (to interpret status code)




###the big renaming table
#from treeKey to treeNo
#from treeMsrKey to MeasureID
#from PlotID to OrigPlotID1?
