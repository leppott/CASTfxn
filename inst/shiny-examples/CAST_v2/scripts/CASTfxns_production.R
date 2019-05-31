# Code to generate output for the shiny app
# Load all items for "all sessions" in server.R outside of call to "shinyServer()"
# #
# # load dependencies
# #library(shiny)
# library("readxl")
# library(reshape)
# library(maps)
# library(RColorBrewer)
# library(rgdal)
# library(maptools)
# library(RgoogleMaps)
# library(rgeos)
# library(raster)
# library(dplyr)
# library(ggplot2)
# 
# # clear the workspace
# rm(list=ls())

################################

getSiteInfo <- function(TargetSiteID,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster) {
    mySiteInfo <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                ,c("FinalLatitude","FinalLongitude","WaterbodyName"
                                ,"GIS_County","CARefSite_2017","COMID_NHD2")]
    data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
                            select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
    # # generate map -- I DON'T KNOW HOW TO MAKE THIS A SINGLE OBJECT TO ADD TO THE RETURNED LIST
    # # HOW DO WE MAP JUST THE ECOREGION? IS THAT POSSIBLE?
    # png(file = paste0("Results/map.example.",TargetSiteID, ".png"),
    #     width = 550, height = 480, pointsize = 13)
    # df.plotSite <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID,]
    #     myState = "California"
    #     map('state',region=myState)
    #     myLatitude <- "FinalLatitude"
    #     myLongitude <- "FinalLongitude"
    #     points(data.refSites[,"FinalLongitude"], data.refSites[,"FinalLatitude"], col="blue", pch=19, cex=1.25)
    #     points(df.plotSite[,myLongitude], df.plotSite[,myLatitude], col="red", pch=17, cex=1.25)
    #     # get sampling info (dates of samples)
    # dev.off()
    mySamps <- data.SampSummary[data.SampSummary[,"StationID_Master"]==TargetSiteID
                            ,c("CollDate","nChemSamp","nPhabSamp","nBMIMetSamp","nAlgMetSamp")]
    # Number of Samples by Category
    mySamps.N <- cbind(nTotal=nrow(mySamps),as.matrix(t(colSums(mySamps[2:5]))))
    # get response information (CSCI, H20, etc)
    myBMImetrics <- data.bmi.metrics[data.bmi.metrics[,"StationID_Master"]==TargetSiteID
                            ,c("CollDate","CSCI","O_E","MMI_Score")]
    myAlgaeMetrics <- data.algae.metrics[data.algae.metrics[,"StationCode"]==TargetSiteID
                            ,c("SampleDate","H20","D18","S2")]
    # get COMID 
    myCOMID <- mySiteInfo$COMID_NHD2
    myReachInfo <- data.cluster[data.cluster[,"COMID"]==myCOMID,c("H6_noland","H6_land"
                            ,"ElevWs","WsAreaSqKm","PrecipWs", "TmeanWs"
                            ,"W___AGRIC","W___URBAN","W___FOREST")]
    myClustIDs <- myReachInfo[,c("H6_noland","H6_land")]
    # get listing status -- need a fixed data file for this.
    rm(data.refSites)
    mySiteSummary <- list(SiteInfo = mySiteInfo, Samps.N = mySamps.N, Samps = mySamps, MetricsBMI = myBMImetrics
                            , MetricsAlg = myAlgaeMetrics, ReachInfo = myReachInfo
                            , COMID = myCOMID, ClustIDs = myClustIDs)
    return(mySiteSummary)
}

# getChemDataSubsets <- function(TargetSiteID, comid, cluster, clustertype, useLU) {
#     #Create subsets for target sites, ref sites in cluster, all sites in cluster
#     site.COMID <- comid
#     site.Clusters <- cluster
#     nolu.cluster <- paste(clustertype, "_noland", sep="")
#     lu.cluster <- paste(clustertype, "_land", sep="")
# 
#     #Create a vector of Reference Site IDs
#     data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
#                             select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
#     refSiteIDs <- as.vector(unique(data.refSites[,"StationID_Master"]))
#     refSiteCOMIDs <- as.vector(unique(data.refSites[,"COMID_NHD2"]))
# 
#     data.clusterIDs <- data.cluster[,c("COMID",nolu.cluster,lu.cluster)]
#     data.Stations.Clusters <- merge(data.Stations.Info, data.clusterIDs, by.x="COMID_NHD2",by.y="COMID")
#     data.Stations.ClustIDs <- data.Stations.Clusters[,c("StationID_Master",nolu.cluster,lu.cluster)]
#     data.chem.raw <- merge(data.chem.raw, data.Stations.ClustIDs, by.x="StationID_Master", by.y="StationID_Master")
# 
#     #Create stressor data cross-tabs
#     site.chem <- subset(data.chem.raw, StationID_Master %in% TargetSiteID)
# 
#     # site.chem2 contains the chemicals detected at the target site
#     site.chem2 <- subset(site.chem, !is.na(site.chem["ResultValue"]))
#     chems <- unique(site.chem2["ConvertTo"])
#     chems.groups <- merge(chems, data.chem.info, by.x="ConvertTo", by.y="Analyte")
# 
#     # chems.groups.sort is the list of chems detected at the target site, in group sort order
#     chems.groups.sort <- chems.groups[order(chems.groups$GroupNum, chems.groups$ConvertTo),]
#     numgps <- length(unique(chems.groups$GroupName))
#     groupnames <- unique(chems.groups.sort$GroupName)
#     site.lu <- site.Clusters[lu.cluster]
#     site.nolu <- site.Clusters[nolu.cluster]
# 
#     # all.chems is the list of target site chems across all sites in dataset (all clusters)
#     all.chems <- subset(data.chem.raw, ConvertTo %in% chems$ConvertTo)
#     all.chems2 <- all.chems[,c("ChemSampleID","ConvertTo","ResultValue")]
#     all.chems3 = cast(all.chems2, ChemSampleID ~ ConvertTo, mean)
# 
# 
#     # chem.tab2 is the list of target site chems at sites in the target site cluster
#     if (useLU == TRUE) {
#         cluster.chem.tab2 <- subset(all.chems, all.chems[,lu.cluster]==site.lu[,1])
#     } else {
#         cluster.chem.tab2 <- subset(all.chems, all.chems[,nolu.cluster]==site.nolu[,1])
#     }
# 
#     cluster.chem.tab3 <- cluster.chem.tab2[,c("ChemSampleID","ConvertTo","ResultValue")]
#     cluster.chem.samps <- unique(cluster.chem.tab2[,c("StationID_Master","ChemSampleID")])
#     cluster.chem.tab4 = cast(cluster.chem.tab3, ChemSampleID ~ ConvertTo, mean)
#     cluster.chem.tab5 = merge(cluster.chem.samps, cluster.chem.tab4, by.x = "ChemSampleID", by.y = "ChemSampleID")
#     site.chem3 <- site.chem2[,c("ChemSampleID", "ConvertTo", "ResultValue")]
#     site.chem4 = cast(site.chem3, ChemSampleID ~ ConvertTo, mean)
# 
#     mySubsets <- list(ref.sites = refSiteIDs, ref.reaches = refSiteCOMIDs, cluster.samps = cluster.chem.samps
#                       , chem.info = chems.groups.sort, all.chems = all.chems3
#                       , cluster.chem = cluster.chem.tab5, site.chem = site.chem4)
# 
#     return(mySubsets)
# }
# 
# getClusterInfo <- function(site.COMID, clustertype, siteClusters, refSiteCOMIDs, useLU = FALSE) {
#     nolu.cluster <- paste(clustertype, "_noland", sep="")
#     lu.cluster <- paste(clustertype, "_land", sep="")
#     if (length(siteClusters)==0) {
#         # do not proceed
#         # no cluster assignment
#         stop(paste("No cluster assignment for", TargetSiteID, sep = " "))
#     }
#     data.cluster.mySites <- data.cluster[data.cluster$COMID %in% site.COMID,]
#     df.plot.3 <- data.cluster[data.cluster$COMID %in% refSiteCOMIDs,]
#     png(file = paste0("Results/cluster.example.",TargetSiteID, ".png"),
#         width = 1200, height = 800, pointsize = 13)##PNG.START
#       if (useLU == FALSE) {
#           selvar <- c("WsAreaSqKm","PrecipWs","TmeanWs","SLOPE","MAXELEVSMO")
#           varnames <- c("WS Area", "WS Precipitation","Mean Temp", "Slope", "Max Elevation")
#           par(mfrow = c(2,3), mar = c(2,4,1,1))
#           df.plot <- data.cluster
#           df.plot.2 <- data.cluster.mySites
#           for(ii in 1:length(selvar)) {
#               myY <- df.plot[,selvar[ii]]
#               myX <- df.plot[,nolu.cluster]
#               boxplot(myY~myX, main = "Clusters w/o Land Use", xlab ="Cluster", ylab = varnames[ii], boxwex = 0.5, col ="lightgray")
#               ############
#               # add points to plots for reference sites
#               myY <- df.plot.3[,selvar[ii]]
#               myX <- df.plot.3[,nolu.cluster]
#               points(myX,myY,col="blue",cex=1.5,pch=19)
#              ############
#               # add points to plots for selected sites
#               myY <- df.plot.2[,selvar[ii]]
#               myX <- df.plot.2[,nolu.cluster]
#               points(myX,myY,col="red",cex=1.5,pch=19)
#               ############
#           }
#       } else {
#           data.cluster.mySites <- data.cluster[data.cluster$COMID %in% site.COMID,]
#           selvar <- c("WsAreaSqKm","PrecipWs","TmeanWs","SLOPE","MAXELEVSMO")
#           varnames <- c("W_Area", "WS Precipitation","Mean Temp", "Slope", "Max Elevation")
#           par(mfrow = c(2,3), mar = c(2,4,1,1))
#           df.plot <- data.cluster
#           df.plot.2 <- data.cluster.mySites
#           for(ii in 1:length(selvar)) {
#               myY <- df.plot[,selvar[ii]]
#               myX <- df.plot[,lu.cluster]
#               boxplot(myY~myX, main = "Clusters w/Land Use", xlab ="Cluster", ylab = varnames[ii], boxwex = 0.5, col ="lightgray")
#               ############
#               # add points to plots for reference sites
#               myY <- df.plot.3[,selvar[ii]]
#               myX <- df.plot.3[,lu.cluster]
#               points(myX,myY,col="blue",cex=1.5,pch=19)
#               ############
#               # add points to plots for selected sites
#               myY <- df.plot.2[,selvar[ii]]
#               myX <- df.plot.2[,lu.cluster]
#               points(myX,myY,col="red",cex=1.5,pch=19)
#               ############
#           }
#       }
#     dev.off()##PNG.END
# }
# 
# getStressorList <- function(TargetSiteID, site.Clusters, chem.info, cluster.chem
#                             , cluster.samps, ref.sites, site.chem
#                             , probsHigh, probsLow) {
# 
#     stations <- TargetSiteID
#     nolu.cluster <- paste(clustertype, "_noland", sep="")
#     lu.cluster <- paste(clustertype, "_land", sep="")
# 
#     cluster.chem.data <- cluster.chem[3:ncol(cluster.chem)]
#     cluster.ref.chem <- subset(cluster.chem, cluster.chem$StationID_Master %in% ref.sites)
#     cluster.ref.chem.data <- cluster.ref.chem[3:ncol(cluster.ref.chem)]
#     chemnames <- names(cluster.chem[,3:ncol(cluster.chem.data)])
#     allcount <- apply(cluster.chem.data, 2, function(x) sum(!is.na(x)))
#     alltype <- unlist(lapply(1:ncol(cluster.chem.data), function(x) is.numeric(cluster.chem[,x])))
#     coolvar <- names(allcount)[allcount>2 & alltype]
# 
#     groupnames <- unique(subset(chem.info, chem.info$ConvertTo %in% chemnames, select = "GroupName"))
#     numgps <- length(groupnames[,1])
# 
#     for (g in 1:numgps) {    # Generate 1 box plot for each group, ref sites in blue, target site in red
#         gpchems <- subset(chem.info, GroupName == groupnames[g,], select = "ConvertTo")
#         gpcoolvar <- subset(coolvar, coolvar %in% gpchems$ConvertTo)
#         n <- length(gpcoolvar)
#         if(n>0) { ##FOR.n.START
#             png(file = paste0("Results/boxes.example.",TargetSiteID
#                             , ".", groupnames[g,], ".png"), width = 1200
#                             , height = 800, pointsize = 13)
#               maintitle <- paste(groupnames[g,], "Standardized values, All sites in cluster", sep=", ")
#               par(mfrow = c(1,1), mar = c(4,8,1,1))
#               if (useLU == TRUE) {
#                   labmain = paste(stations, ": Cluster", site.Clusters[1,lu.cluster])
#               } else {
#                   labmain = paste(stations, ": Cluster", site.Clusters[1,nolu.cluster])
#               }
#               labx = paste(maintitle, labmain, sep = "\n")
#               plot(y= 1:n, x= runif(n,0,1), axes = F, type="n", xlab = labx, ylab ="",
#                    xlim = c(0,1))
#               axis(1, at = seq(0,1, 0.2),labels = seq(0,1, 0.2))
#               axis(2, at = 1:n, labels = gpcoolvar[1:n], las =1, cex.axis = .6)
#               for(i in 1:n) {
#                   xvar <- cluster.chem[,gpcoolvar[i]]; dif <- diff(range(xvar, na.rm =T))
#                   newvar <- (xvar-min(xvar, na.rm=T))/dif
#                   boxplot(newvar, at = i,boxwex=.5, horizontal =T, add =T,axes = F)
#                   good.ref.data <- cluster.ref.chem.data[,gpcoolvar[i]][!is.na(cluster.ref.chem[,gpcoolvar[i]])]
#                   if (length(good.ref.data) != 0) {
#                       point2 <- (cluster.ref.chem.data[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif
#                       points(point2, rep(i,length(point2)), col = "blue", pch = 15,cex=1.2, bg = 2)
#                   }
#                   point1 <- (site.chem[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif
#                   points(point1, rep(i,length(point1)), col = "red", pch = 18,cex=0.8, bg = 2)
#               }
#               box(bty="l")
#             dev.off()
#         }
# 
#     }
#     chem.pctrank <- apply(cluster.chem[,3:ncol(cluster.chem)], 2, function(x) percent_rank(x))
#     data.chem.pctrank <- as.data.frame(chem.pctrank)
#     data.chem.pctrank <- cbind(cluster.chem$StationID_Master,cluster.chem$ChemSampleID,data.chem.pctrank)
#     colnames(data.chem.pctrank)[1] <- "StationID_Master"
#     colnames(data.chem.pctrank)[2] <- "ChemSampleID"
#     row.names(data.chem.pctrank) <- NULL
#     write.table(data.chem.pctrank, file = paste("Results/chem.pctrank.",TargetSiteID,".txt", sep="")
#                 ,sep="\t", col.names=TRUE)
#     site.pctrank <- subset(data.chem.pctrank, StationID_Master==TargetSiteID)
#     stressor <- c("none")
#     for (c in 3:ncol(site.pctrank)) {
#         chemname <- colnames(site.pctrank)[c]
#         bad <- is.na(site.pctrank[,c])
#         check <- site.pctrank[,c]
#         good <- check[!bad]
#         maxSiteVal <- max(good)
#         minSiteVal <- min(good)
#         if ((chemname == "DO_uf_mg_L") || (chemname == "pH")) {
#             if (minSiteVal <= probsLow) {
#                 stressor <- c(stressor, chemname)
#             }
#         }
#         if ((chemname != "DO_uf_mg_L") && (maxSiteVal >= probsHigh)) {
#             stressor <- c(stressor, chemname)
#         }
#     }
#     stressorlist <- stressor
#     myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank)
# 
#     return(myStressors)
# } # FUN end
# 
# 
# getBMIMatches <- function(stressors, list.data) {
# 
#     all.chems <- list.data[["all.chems"]]
#     cl.chems <- list.data[["cluster.chem"]]
#     site.chem <- list.data[["site.chem"]]
# 
#     # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
#     # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
#     # These aren't in all.chems, because they don't have data corresponding to the site data
#     useChemSamps <- all.chems$ChemSampleID
#     mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
# 
#     mbmi.Samps <- na.omit(data.SampSummary[,c("ChemSampleID","BMI.Metrics.SampID")])
#     mbmi.use.samps <- subset(mbmi.Samps, mbmi.Samps$ChemSampleID %in% mUseSamps)
# 
#     # bmi stressor data to use: all.mbmi.stress, cl.mbmi.stress, and site.stress
#     all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
#     all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
#                         , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
#     all.mbmi.stress <- subset(all.stress, ChemSampleID %in% mbmi.use.samps$ChemSampleID)
#     all.mbmi.stress <- merge(mbmi.use.samps, all.mbmi.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
#     cl.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% cl.chems$ChemSampleID)
#     site.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% site.chem$ChemSampleID)
# 
#     # bmi response data to use: all.mbmi.resp, cl.mbmi.resp, and site.mbmi.resp
#     all.resp <- subset(data.bmi.metrics, BMISampleID %in% mbmi.use.samps$BMI.Metrics.SampID)
#     all.mbmi.resp <- merge(mbmi.use.samps, all.resp, by.x = "BMI.Metrics.SampID", by.y = "BMISampleID")
#     cl.mbmi.resp <- subset(all.mbmi.resp, ChemSampleID %in% cl.chems$ChemSampleID)
#     site.mbmi.resp <- subset(all.mbmi.resp, ChemSampleID %in% site.chem$ChemSampleID)
# 
#     myMatchData <- list(all.b.str = all.mbmi.stress
#                             , cl.b.str = cl.mbmi.stress
#                             , site.b.str = site.mbmi.stress
#                             , all.b.rsp = all.mbmi.resp
#                             , cl.b.rsp = cl.mbmi.resp
#                             , site.b.rsp = site.mbmi.resp)
# 
#     return(myMatchData)
# }
# 
# getAlgMatches <- function(stressors, list.data) {
# 
#     all.chems <- list.data[["all.chems"]]
#     cl.chems <- list.data[["cluster.chem"]]
#     site.chem <- list.data[["site.chem"]]
#     ref.sites <- list.data[["ref.sites"]]
# 
#     # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
#     # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
#     # These aren't in all.chems, because they don't have data corresponding to the site data
#     useChemSamps <- all.chems$ChemSampleID
#     mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
# 
#     malg.Samps <- na.omit(data.SampSummary[,c("ChemSampleID","Algae.Metrics.SampID")])
#     malg.use.samps <- subset(malg.Samps, malg.Samps$ChemSampleID %in% mUseSamps)
# 
#     # bmi stressor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
#     all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
#     all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
#                         , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
# 
#     # alg stresor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
#     all.malg.stress <- subset(all.stress, ChemSampleID %in% malg.use.samps$ChemSampleID)
#     all.malg.stress <- merge(malg.use.samps, all.malg.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
#     cl.malg.stress <- subset(all.malg.stress, ChemSampleID %in% cl.chems$ChemSampleID)
#     site.malg.stress <- subset(all.malg.stress, ChemSampleID %in% site.chem$ChemSampleID)
# 
#     # alg response data to use: all.malg.resp, cl.malg.resp, and site.malg.resp
#     all.malg.resp <- subset(data.algae.metrics, StationDateRep %in% malg.use.samps$Algae.Metrics.SampID)
#     cl.malg.resp <- subset(all.malg.resp, StationDateRep %in% cl.chems$ChemSampleID)
#     site.malg.resp <- subset(all.malg.resp, StationDateRep %in% site.chem$ChemSampleID)
# 
#     myMatchData <- list(all.a.str = all.malg.stress
#                         , cl.a.str = cl.malg.stress
#                         , site.a.str = site.malg.stress
#                         , all.a.rsp = all.malg.resp
#                         , cl.a.rsp = cl.malg.resp
#                         , site.a.rsp = site.malg.resp )
#     return(myMatchData)
# }
# 
# getBMIStressorResponses <- function(stressors,list.MatchBMIData) {
# 
#     for (p in 1:length(stressors)) {
#         stressName <- stressors[p]
#         varFlag <- 1
#         if (stressName %in% c("DO_uf_mg_L", "pH", "Temp_degC")) {
#             log.yn <- FALSE
#         } else {
#             log.yn <- TRUE
#         }
#         for (r in 1: length(BMIresp)) {
#             respName <- BMIresp[r]
# 
#             #get all data to plot
#             all.xvar<- list.MatchBMIData[["all.b.str"]][,c("StationID_Master","BMI.Metrics.SampID", stressName)]
#             all.yvar<- list.MatchBMIData[["all.b.rsp"]][,c("StationID_Master","BMI.Metrics.SampID", respName)]
#             df.plot1 <- merge(all.xvar[,2:3],all.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
#             all.df.plot <- df.plot1[complete.cases(df.plot1),2:3]
# 
#             #get all ref   data to plot
#             all.ref.xvar <- subset(all.xvar, all.xvar$StationID_Master %in% ref.sites)
#             all.ref.yvar <- subset(all.yvar, all.yvar$StationID_Master %in% ref.sites)
#             df.plot2 <- merge(all.ref.xvar[,2:3],all.ref.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
#             all.ref.df.plot <- df.plot2[complete.cases(df.plot2),2:3]
# 
#             #get all cluster data to plot
#             cl.xvar<- list.MatchBMIData[["cl.b.str"]][,c("StationID_Master","BMI.Metrics.SampID", stressName)]
#             cl.yvar<- list.MatchBMIData[["cl.b.rsp"]][,c("StationID_Master","BMI.Metrics.SampID", respName)]
#             df.plot3 <- merge(cl.xvar[,2:3],cl.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
#             cl.df.plot <- df.plot3[complete.cases(df.plot3),2:3]
# 
#             #get all cluster ref data to plot
#             cl.ref.xvar <- subset(cl.xvar, cl.xvar$StationID_Master %in% ref.sites)
#             cl.ref.yvar <- subset(cl.yvar, cl.yvar$StationID_Master %in% ref.sites)
#             df.plot4 <- merge(cl.ref.xvar[,2:3],cl.ref.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
#             cl.ref.df.plot <- df.plot4[complete.cases(df.plot4),2:3]
# 
#             #get target site data to plot
#             site.xvar<- list.MatchBMIData[["site.b.str"]][,c("BMI.Metrics.SampID", stressName)]
#             site.yvar<- list.MatchBMIData[["site.b.rsp"]][,c("BMI.Metrics.SampID", respName)]
#             df.plot5 <- merge(site.xvar,site.yvar, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
#             site.df.plot <- df.plot5[complete.cases(df.plot5),2:3]
# 
#             #jpeg(filename = paste(varFileOut,varXName,"_", varYName,".jpg", sep = ""), width = 1028, height = 768,quality=100,pointsize=14)
#             par(cex.main=1.0,cex.lab=1.0,font.main=2, font.lab=2)
#             if (log.yn == TRUE) {
#                 all.df.plot <- cbind(log10(all.df.plot[,1]),all.df.plot[,2])
#                 all.ref.df.plot <- cbind(log10(all.ref.df.plot[,1]),all.ref.df.plot[,2])
#                 cl.df.plot <- cbind(log10(cl.df.plot[,1]),cl.df.plot[,2])
#                 cl.ref.df.plot <- cbind(log10(cl.ref.df.plot[,1]),cl.ref.df.plot[,2])
#                 site.df.plot <- cbind(log10(site.df.plot[,1]),site.df.plot[,2])
#             }
# 
#             # There should never be a case where either x or y are always NA for all data
#             if (length(all.ref.df.plot) > 0) {
#                 plot(all.df.plot[,2]~all.df.plot[,1],xlab=stressName,ylab=respName, col="grey", pch=1)
#             } else {
#                 next
#             }
#             if (length(all.ref.df.plot) > 0) {
#                 points(all.ref.df.plot[,2]~all.ref.df.plot[,1], col="blue", pch=16) # blue solid dots
#             }
#             if (length(cl.df.plot) > 0) {
#                 points(cl.df.plot[,2]~cl.df.plot[,1], col="red", pch=2) # Red open triangles
#             }
#             if (length(cl.ref.df.plot) > 0) {
#                 points(cl.ref.df.plot[,2]~cl.ref.df.plot[,1], col="blue", pch=17) # Solid blue triangles
#             }
#             if (length(site.df.plot) > 0) {
#                 points(site.df.plot[,2]~site.df.plot[,1], col="black", pch=19, cex = 1.2) # black solid dots
#             }
# 
#             cl.x.sd <- sd(cl.df.plot[,1])
#             cl.y.sd <- sd(cl.df.plot[,2])
#             #Check for vertical line
#             if (!is.na(cl.x.sd)) {
#                 if (cl.x.sd == 0) {
#                     print(paste("Vertical line for", stressName, respName, sep=" "))
#                     flush.console()
#                     next     #It's okay to plot the points, but not the regression line
#                 }
#             }
#             #Check for horizontal line
#             if (!is.na(cl.y.sd)) {
#                 if (cl.y.sd == 0) {
#                     print(paste("Horizontal line for", stressName, respName, sep=" "))
#                     flush.console()
#                     next     #It's okay to plot the points, but not the regression line
#                 }
#             }
# 
#             #Linear Regression (uses cluster data -- all sites in the cluster)
#             varY <- cl.df.plot[,2]
#             varX <- cl.df.plot[,1]
#             fit = lm(varY~varX)
#             pred.int = predict(fit,interval="prediction",level=predint)
#             fitted.values = pred.int[,1]
#             pred.lower = pred.int[,2]
#             pred.upper = pred.int[,3]
# 
#             abline(lm(varY~varX), col="red", lwd=2)
#             abline(lm(pred.lower~varX), col="blue", lwd=1.5)
#             abline(lm(pred.upper~varX), col="blue", lwd=1.5)
#             #
#             slope <- summary(lm(varY~varX))[[4]][[2]]
#             intercept <- summary(lm(varY~varX))[[4]][[1]]
#             pval_intercept <- summary(lm(varY~varX))[[4]][[7]]
#             pval_slope <- summary(lm(varY~varX))[[4]][[8]]
#             slope = signif(slope, 3)
#             intercept = signif(intercept, 3)
#             pval_intercept = signif(pval_intercept, 3)
#             pval = signif(pval_slope, 3)
#             # # r� text and legend
#             r = cor(varX, varY, method="pearson",use="pairwise.complete.obs")
#             r2 = formatC(r^2,format="f",digits=3)
#             #
#             c1S <- (cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
#             df.corr = data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
#                                        , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
#             # # Create results data frame
#             if (varFlag==1) {  #First time through loop
#                 df.CorrTable <- c(df.corr)
#             } # IF, END
#             df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
#             pval.corr = signif(c1S$p.value,2)
# 
#             #Print equation, r2, and p-value
#             if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
#                 eqn <- paste("Cluster regression\n"
#                              , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
#                              ,"p-value = ",pval.corr,"\n","n = ",length(varX),"\n")
#                 symbshape <- c(1, 16, 2, 17, 19)
#                 symbcol <- c("grey", "blue", "red", "blue", "black")
#                 symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
#                 legend(varLegLoc, inset = varInset, (paste("Cluster regression\n"
#                                                            , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
#                                                            ,"p-value = ",pval.corr,"\n","n = ",length(varX))), bty="n"
#                        , col = c("black"), cex=0.8)
#                 legend(varLegOpp,inset=varInset, symbname, pch=symbshape, col=symbcol, cex=0.6)
#             }
#             varFlag <- 0
#         }
#     }
#     write.table(df.CorrTable,file="StressRespCorrs.BMI.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)
# }
# 
# getAlgStressorResponses <- function(stressors,list.MatchBMIData) {
# 
#     for (p in 1:length(stressors)) {
#         stressName <- stressors[p]
#         if (stressName %in% c("DO_uf_mg_L", "pH", "Temp_degC")) {
#             log.yn <- FALSE
#         } else {
#             log.yn <- TRUE
#         }
#         varFlag <- 1
#         for (r in 4:length(AlgResp)) {
#             respName <- AlgResp[r]
# 
#             #get all data to plot
#             all.xvar<- list.MatchAlgData[["all.a.str"]][,c("StationID_Master","Algae.Metrics.SampID", stressName)]
#             all.yvar<- list.MatchAlgData[["all.a.rsp"]][,c("StationCode","StationDateRep", respName)]
#             df.plot1 <- merge(all.xvar[,2:3],all.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
#             all.df.plot <- df.plot1[complete.cases(df.plot1),2:3]
# 
#             #get all ref   data to plot
#             all.ref.xvar <- subset(all.xvar, all.xvar$StationID_Master %in% ref.sites)
#             all.ref.yvar <- subset(all.yvar, all.yvar$StationCode %in% ref.sites)
#             df.plot2 <- merge(all.ref.xvar[,2:3],all.ref.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
#             all.ref.df.plot <- df.plot2[complete.cases(df.plot2),2:3]
# 
#             #get all cluster data to plot
#             cl.xvar<- list.MatchAlgData[["cl.a.str"]][,c("StationID_Master","Algae.Metrics.SampID", stressName)]
#             cl.yvar<- list.MatchAlgData[["cl.a.rsp"]][,c("StationCode","StationDateRep", respName)]
#             df.plot3 <- merge(cl.xvar[,2:3],cl.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
#             cl.df.plot <- df.plot3[complete.cases(df.plot3),2:3]
# 
#             #get all cluster ref data to plot
#             cl.ref.xvar <- subset(cl.xvar, cl.xvar$StationCode %in% ref.sites)
#             cl.ref.yvar <- subset(cl.yvar, cl.yvar$StationCode %in% ref.sites)
#             df.plot4 <- merge(cl.ref.xvar[,2:3],cl.ref.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
#             cl.ref.df.plot <- df.plot4[complete.cases(df.plot4),2:3]
# 
#             #get target site data to plot
#             site.xvar<- list.MatchAlgData[["site.a.str"]][,c("Algae.Metrics.SampID", stressName)]
#             site.yvar<- list.MatchAlgData[["site.a.rsp"]][,c("StationDateRep", respName)]
#             df.plot5 <- merge(site.xvar,site.yvar, by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
#             site.df.plot <- df.plot5[complete.cases(df.plot5),2:3]
# 
#             #jpeg(filename = paste(varFileOut,varXName,"_", varYName,".jpg", sep = ""), width = 1028, height = 768,quality=100,pointsize=14)
#             par(cex.main=1.0,cex.lab=1.0,font.main=2, font.lab=2)
#             if (log.yn == TRUE) {
#                 all.df.plot <- cbind(log10(all.df.plot[,1]),all.df.plot[,2])
#                 all.ref.df.plot <- cbind(log10(all.ref.df.plot[,1]),all.ref.df.plot[,2])
#                 cl.df.plot <- cbind(log10(cl.df.plot[,1]),cl.df.plot[,2])
#                 cl.ref.df.plot <- cbind(log10(cl.ref.df.plot[,1]),cl.ref.df.plot[,2])
#                 site.df.plot <- cbind(log10(site.df.plot[,1]),site.df.plot[,2])
#             }
# 
#             # There should never be a case where either x or y are always NA for all data
#             if (length(all.ref.df.plot) > 0) {
#                 plot(all.df.plot[,2]~all.df.plot[,1],xlab=stressName,ylab=respName, col="grey", pch=1)
#             } else {
#                 next
#             }
#             if (length(all.ref.df.plot) > 0) {
#                 points(all.ref.df.plot[,2]~all.ref.df.plot[,1], col="blue", pch=16) # blue solid dots
#             }
#             if (length(cl.df.plot) > 0) {
#                 points(cl.df.plot[,2]~cl.df.plot[,1], col="red", pch=2) # Red open triangles
#             }
#             if (length(cl.ref.df.plot) > 0) {
#                 points(cl.ref.df.plot[,2]~cl.ref.df.plot[,1], col="blue", pch=17) # Solid blue triangles
#             }
#             if (length(site.df.plot) > 0) {
#                 points(site.df.plot[,2]~site.df.plot[,1], col="black", pch=19, cex = 1.2) # black solid dots
#             }
# 
#             cl.x.sd <- sd(cl.df.plot[,1])
#             cl.y.sd <- sd(cl.df.plot[,2])
#             #Check for vertical line
#             if (!is.na(cl.x.sd)) {
#                 if (cl.x.sd == 0) {
#                     print(paste("Vertical line for", stressName, sep=" "))
#                     flush.console()
#                     next     #It's okay to plot the points, but not the regression line
#                 }
#             }
#             #Check for horizontal line
#             if (!is.na(cl.y.sd)) {
#                 if (cl.y.sd == 0) {
#                     print(paste("Horizontal line for", respName, sep=" "))
#                     flush.console()
#                     next     #It's okay to plot the points, but not the regression line
#                 }
#             }
# 
#             #Linear Regression (uses cluster data -- all sites in the cluster)
#             varY <- cl.df.plot[,2]
#             varX <- cl.df.plot[,1]
#             fit = lm(varY~varX)
#             pred.int = predict(fit,interval="prediction",level=predint)
#             fitted.values = pred.int[,1]
#             pred.lower = pred.int[,2]
#             pred.upper = pred.int[,3]
# 
#             abline(lm(varY~varX), col="red", lwd=2)
#             abline(lm(pred.lower~varX), col="red", lwd=1.5)
#             abline(lm(pred.upper~varX), col="red", lwd=1.5)
#             #
#             slope <- summary(lm(varY~varX))[[4]][[2]]
#             intercept <- summary(lm(varY~varX))[[4]][[1]]
#             pval_intercept <- summary(lm(varY~varX))[[4]][[7]]
#             pval_slope <- summary(lm(varY~varX))[[4]][[8]]
#             slope = signif(slope, 3)
#             intercept = signif(intercept, 3)
#             pval_intercept = signif(pval_intercept, 3)
#             pval = signif(pval_slope, 3)
#             # # r� text and legend
#             r = cor(varX, varY, method="pearson",use="pairwise.complete.obs")
#             r2 = formatC(r^2,format="f",digits=3)
#             #
#             c1S <- (cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
#             df.corr = data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
#                                        , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
#             # # Create results data frame
#             if (varFlag==1) {  #First time through loop
#                 df.CorrTable <- c(df.corr)
#             } # IF, END
#             df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
#             pval.corr = signif(c1S$p.value,2)
# 
#             #Print equation, r2, and p-value
#             if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
#                 eqn <- paste("Cluster regression\n"
#                              , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
#                              ,"p-value = ",pval.corr,"\n","n = ",length(varX),"\n")
#                 symbshape <- c(1, 16, 2, 17, 19)
#                 symbcol <- c("grey", "blue", "red", "blue", "black")
#                 symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
#                 legend(varLegLoc, inset = varInset, (paste("Cluster regression\n"
#                                                            , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
#                                                            ,"p-value = ",pval.corr,"\n","n = ",length(varX))), bty="n"
#                        , col = c("black"), cex=0.8)
#                 legend(varLegOpp,inset=varInset, symbname, pch=symbshape, col=symbcol, cex=0.6)
#             }
#             varFlag <- 0
#         }
#     }
#     write.table(df.CorrTable,file="StressRespCorrs.Algae.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)
# 
# }
# #     
# #     
# # ##############
# # ##   PREP   ##
# # ##############
# # 
# # #Read all data files
# # 
# # CurrentDir<-getwd()
# # myDir.Data <- paste(CurrentDir,"data/",sep="/")
# # 
# # # data need for operation
# # ## Stations - PickList     ## Change to include full list of stations w/bio data
# # data.Stations <- read.delim(paste(myDir.Data,"data.Stations.LookUp.tab",sep=""))
# # LU.Stations <- data.Stations[,"StationID"]
# # ## Stations - Location Information     ##Updated to most recent (trimmed data)
# # ## For ref stations, use CARefSite2017 column (T=1/F=0)
# # data.Stations.Info <- read.delim(paste(myDir.Data,"data.Stations.Info.tab",sep=""))
# # ## Listing status, etc. From Erik. Should be good. What is the PK column?
# # ## This file doesn't seem right. There are only 3 COMIDs? 
# # #data.303d.ComID <- read.csv(paste0(myDir.Data,"data.303dcomid.csv"))#,colClasses="character")
# # ## Data summary by site
# # data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep=""),na.strings = c(""," "))
# # ## all cluster data (COMID, cluster assignments, and predictors)
# # data.cluster <- read.delim(paste(myDir.Data,"data.all.clust.tab",sep=""))
# # ## Stressor data
# # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
# # #data.chem.raw <- readRDS(paste0(myDir.Data,"data.chem.raw.RDS"))
# # #data.chem.raw <- read.table(file=(paste(myDir.Data,"data.chem.raw.tab",sep="")), header=TRUE, sep="\t", nrows=109266)
# # dim(data.chem.raw)
# # data.phab.raw <- read.delim(paste(myDir.Data,"data.phab.raw.tab",sep=""))
# # ## Stressor group data (lookup values)
# # #data.taxa.response <- read_excel(paste0(myDir.Data,"data.taxa.xlsx"),sheet=2)
# # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
# # data.phab.info <- read.delim(paste(myDir.Data,"data.phab.info.tab",sep=""))
# # ## Response data (BMI/Algae Indices/Metrics)
# # data.algae.metrics <- read.delim(paste(myDir.Data,"data.algae.metrics.tab",sep=""))
# # data.bmi.metrics <- read.delim(paste(myDir.Data,"data.bmi.metrics.tab",sep=""))
# # data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep=""), na.strings = c(""," "))
# # ## Raw taxa data (abundance in each sample)
# # data.bmi.taxa.raw <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
# # ## Taxonomic information (BMI) (Master taxa table)
# # data.bmi.info <- read.delim(paste(myDir.Data,"data.bmi.info.tab",sep=""))
# # ## Modified Status     ##This is site-based mod/flow status. Need reach based, too
# # #data.mod <- read.delim(paste(myDir.Data,"data.SanDiego.BioIndex.ModPerStatus.tab",sep=""))
# # ## SSD data
# # 
# # ## Criteria
# # 
# # #Set variables to get site, cluster information
# # TargetSiteID <- "907CCCR02"
# # clustertype <- "H6"
# # useLU <- FALSE
# # # set cutoff for possible stressor identification
# # probsLow <- 0.10
# # probsHigh <- 0.90
# # # For stressor-response analysis
# # varLegLoc <- "topright"
# # BMIresp <- c("CSCI", "O_E", "MMI_Score", "ClingerTaxaPct", "ColeopteraTaxaPct"
# #              , "EPTTaxaPct", "ShredderTaxa", "TaxaRichness")
# # AlgResp <- colnames(data.algae.metrics[4:ncol(data.algae.metrics)-3])
# # 
# # # For regression graphs
# # predint <- 0.75
# # if (varLegLoc == "topleft") {
# #     varInset = 0.01       #top inset = 0.05
# #     varSpacer = "\n\n\n"
# #     varLegOpp = "bottomright"
# # }
# # if (varLegLoc == "topright") {
# #     varInset = 0.01
# #     varSpacer = "\n\n\n\n"
# #     varLegOpp = "bottomleft"
# # }
# # if (varLegLoc == "bottomleft") {
# #     varInset = 0.01
# #     varSpacer = ""
# #     varLegOpp = "topright"
# # }
# # if (varLegLoc == "bottomright") {
# #     varInset = 0.01
# #     varSpacer = ""
# #     varLegOpp = "topleft"
# # }
# # 
# # 
# # 
# # 
# # ##############
# # ##   MAIN   ##
# # ##############
# # 
# # 
# # list.SiteSummary <- getSiteInfo(TargetSiteID)
# #     # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo, Samps = mySamps, BMImetrics = myBMImetrics
# #     #                       , AlgMetrics = myAlgaeMetrics, ReachInfo = myReachInfo
# #     #                       , COMID = myCOMID, ClustIDs = myClustIDs)
# #     site.COMID <- list.SiteSummary[["COMID"]]
# #     site.Clusters <- list.SiteSummary[["ClustIDs"]]
# # 
# # list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
# #     # Returns: mySubsets <- list(ref.sites = refSiteIDs, ref.reaches = refSiteCOMIDs, cluster.samps = cluster.chem.samps
# #     #                   , chem.info = chems.groups.sort, all.chems = all.chems3
# #     #                   , cluster.chem = cluster.chem.tab5, site.chem = site.chem4)
# #     ref.sites <- list.data[["ref.sites"]]
# #     ref.reaches <- list.data[["ref.reaches"]]
# #     cluster.samps <- list.data[["cluster.samps"]]
# #     cluster.chem <- list.data[["cluster.chem"]]
# #     # cluster.ref.chem <- subset(cluster.chem, cluster.chem$StationID_Master %in% ref.sites)
# #     site.chem <- list.data[["site.chem"]]
# #     chem.info <- list.data[["chem.info"]]
# # getClusterInfo(site.COMID, clustertype, site.Clusters, ref.reaches, useLU)
# #     # Should pass the plots as an object, but I have no idea how to do that, exactly
# #     # Need to capture the error condition of no cluster assignment!
# # list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
# #                                   , cluster.samps, ref.sites, site.chem
# #                                   , probsHigh, probsLow)
# #     # Returns: myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank)
# #     # Should pass the plots as an object, but I have no idea how to do that.
# # stressors <- list.stressors[["stressors"]]
# # if ((length(stressors) == 1) && stressors[1] == "none") {
# #     # No stressors returned
# #     stop(paste("No stressors identified for site", TargetSiteID, sep = " "))
# # } else {
# #     stressors <- c(stressors[2:length(stressors)])
# # }
# # if (length(list.SiteSummary[["BMImetrics"]]) > 0) {
# #     # BMI Responses Found
# #     list.MatchBMIData <- getBMIMatches(stressors, list.data)
# #     # myBMIMatchData <- list(all.b.str = all.mbmi.stress
# #     #                        , cl.b.str = cl.mbmi.stress
# #     #                        , site.b.str = site.mbmi.stress
# #     #                        , all.b.rsp = all.mbmi.resp
# #     #                        , cl.b.rsp = cl.mbmi.resp
# #     #                        , site.b.rsp = site.mbmi.resp)
# #     getBMIStressorResponses(stressors, list.MatchBMIData)
# #     # Should return graphics as objects, but I don't know how to do this
# # }
# # 
# # if (length(list.SiteSummary[["AlgMetrics"]]) > 0) {
# #     # Algae Responses Found
# #     list.MatchAlgData <- getAlgMatches(stressors, list.data)
# #     # myAlgMatchData <- list(all.a.str = all.malg.stress
# #     #                        , cl.a.str = cl.malg.stress
# #     #                        , site.a.str = site.malg.stress
# #     #                        , all.a.rsp = all.malg.resp
# #     #                        , cl.a.rsp = cl.malg.resp
# #     #                        , site.a.rsp = site.malg.resp )
# #     getAlgStressorResponses(stressors, list.MatchBMIData)
# #     # Should return graphics as objects, but I don't know how to do this
# # }
# # 
# # # getStressorSpecificData
# # 
# # SSTV <- subset(data.chem.info, SSTV != 0, c("Analyte", "SSTV", "SensMin"
# #                                             , "SensMax", "TolMin", "TolMax"))
# # if (length(SSTV) != 0) {
# #     stressor.SSTV <- subset(SSTV, Analyte %in% stressors)
# #     if (length(stressor.SSTV) != 0) {
# #         SSTV.names <- as.vector(SSTV$Analyte)
# #         all.match.b <- list.MatchBMIData[["all.b.str"]]
# #         all.SSTV.str <- all.match.b[c("StationID_Master", "ChemSampleID"
# #                                       , "BMI.Metrics.SampID", SSTV.names)]
# #         SSTV.bmi.samps <- merge(data.SampSummary[,c("BMI.Metrics.SampID", "BMISampID")]
# #                 , all.SSTV.str, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
# #         SSTV.bmi.taxa <- merge(SSTV.bmi.samps["BMISampID"], data.bmi.taxa.raw
# #                                , by.x="BMISampID", by.y = "BMISampleID")
# #     }
# # }
# # # get sites with SS data
# # stress.name <- eval.SSTV$Analyte
# # 
# # # identify matching BMI sites
# # # get BMI abundance data, for all BMI
# # 
# # 
# # 
# # 
# # 
# # # if stressor-specific tolerance values are available,
# # # calculate the total abundance of Sensitive & Tolerant BMI
# # # also calculate the total richness of Sensitive & Tolerant BMI
# # # perform regressions for stressor-specific metrics
# # 
# # # getSSDs
# # 
# # # if SSD data are available for a given stressor, generate the SSD
# # (ERIK is working on this)
