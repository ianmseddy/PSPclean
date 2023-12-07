
# library(data.table)
# library(rio)
###with packages - we specify which functions we need


PSP_TREE_YIMO <- import ("E:/yimo.NB/send to ian/PSP_TREE_YIMO.xlsx")
PSP_PLOTS <- import ("E:/yimo.NB/send to ian/PSP_PLOTS.xlsx")
PSP_TREE_YIMO <- as.data.table(PSP_TREE_YIMO)
PSP_PLOTS <- as.data.table(PSP_PLOTS)

globalVariables(c(
  ":=", "sys_Comments", "stand_age","cr","PlotType","Treatment","Plot","GPS_Lat","dbh",
   "GPS_Long","Status","LSTYR","plot_size","species-code","species","TreeNumber","SpeciesCode","Species","DBH",
   "OrigPlotID1","MeasureYear","PlotSize","baseSA","LATITUDE","LONGITUDE","treenum","MeasNum","RemeasID"
))


dataPurification_NBPSP <- function(PSP_PLOTS, PSP_TREE_YIMO,
                                    codesToExclude = NULL, excludeAllObs = TRUE) {
  PSP_TREE_YIMO<- copy(PSP_TREE_YIMO)
  PSP_PLOTS<-copy(PSP_PLOTS)

  ########################################################################################about plots
  #remove comment colomn
  PSP_PLOTS[, sys_Comments :=NULL]
  ##filter base on stand age
  PSP_PLOTS <-   PSP_PLOTS[stand_age != -999,]
  #remove plots with 0 code
  PSP_TREE_YIMO <- PSP_TREE_YIMO[cr != 0,]
  ## remain just plot type=M
  PSP_PLOTS <-   PSP_PLOTS[PlotType != "M",]
  #selects unmanaged psp
  PSP_PLOTS<-PSP_PLOTS[!Treatment=="Untreated"]
  ####### # location
  PlotLocation <- plot[, .(Plot, GPS_Lat, GPS_Long)]

  PSP_PLOTS <- PSP_PLOTS[, .(Plot, Status, PlotType, GPS_Long, GPS_Lat, stand_age,
                             PlotSize, Treatment,LSTYR, )]

  ####change names
  setnames(PSP_PLOTS, old = c("Plot", "LSTYR", "plot_size", "Stand_age", "GPS_Long", "GPS_Lat"),
           new = c("OrigPlotID1", "MeasureYear", "PlotSize", "baseSA", "LATITUDE", "LONGITUDE"))
  PSP_PLOTS <- unique(PSP_PLOTS, by = c("OrigPlotID1"))

  ##########################################################################################about tree Data
  ##remove Na and 0 trees in treenum coloumn
  PSP_TREE_YIMO <- PSP_TREE_YIMO[!treenum == 0 & !is.na(dbh)]

  ###filter trees with amount of CR ( we need cr:1,2,3,4,5,6,7,8,9)

  PSP_TREE_YIMO<- PSP_TREE_YIMO[cr == 1 | cr == 2| cr == 3| cr == 4| cr == 5| cr == 6| cr == 7| cr == 8| cr == 9,
                                .(Plot, treenum,MeasNum,RemeasID, species-code, treenum, species, dbh)]


  ##change names
  setnames(PSP_TREE_YIMO, c("Plot", "treenum","species-code", "species", "dbh"),
           c("OrigPlotID1", "TreeNumber", "SpeciesCode", "Species", "DBH"))

  #PSP_TREE_YIMO<- PSP_TREE_YIMO [ OrigPlotID1 %in% unique (PSP_PLOTS$OrigPlotID1]
  PSP_TREE_YIMO <- PSP_TREE_YIMO[PSP_TREE_YIMO$OrigPlotID1 %in% unique(PSP_PLOTS$OrigPlotID1), ]

  ###############################################################################################
  # unique(treeData$ld)
  #MAKE the value unique
  PSP_TREE_YIMO$OrigPlotID1 <- paste0("NB",  PSP_TREE_YIMO$OrigPlotID1)
  PSP_PLOTS$OrigPlotID1 <- paste0("NB", PSP_PLOTS$OrigPlotID1)

  return(list(
    "PSP_PLOTS" = PSP_PLOTS,
    "PSP_TREE_YIMO" = PSP_TREE_YIMO))
}


######################################################################################secound fucntion

prepInputsNBPSP <- function(dPath) {

  pspNBtree <- prepInputs(targetFile = file.path(dPath, "PSP_TREE_YIMO.csv"),
                          url = paste0("https://docs.google.com/spreadsheets/d/1_E-GS44I_nBmBspkX5Q0PLwP1fFTI_Yt/".
                                       "edit?usp=drive_link&ouid=105925340091005951003&rtpof=true&sd=true"),
                          fun = 'fread')

  pspNBplot <- prepInputs(targetFile = file.path(dPath, "PSP_PLOTS.csv"),
                          url = paste0("https://docs.google.com/spreadsheets/d/18SzMKYE3Q_n4_RJHf8PzN-bX85Al0rDs/",
                                       "edit?usp=drive_link&ouid=105925340091005951003&rtpof=true&sd=true"),
                          destinationPath = dPath,
                          fun = 'fread')
  return(list(
    "PSP_PLOTS" = pspNBplot,
    "PSP_TREE_YIMO" = pspNBtree
  ))
}
