# Code to generate output for the shiny app
# Load all items for "all sessions" in server.R outside of call to "shinyServer()"
#
# load dependencies
#library(shiny)
library(reshape)
library(maps)
library(RColorBrewer)
library(rgdal)
library(maptools)
library(RgoogleMaps)
library(rgeos)
library(raster)
library(dplyr)
library(ggplot2)

# clear the workspace
rm(list=ls())

################################

getSiteInfo <- function(TargetSiteID, clustertype, useLU = FALSE) {
    mySiteInfo <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                ,c("FinalLatitude","FinalLongitude","WaterbodyName"
                                ,"GIS_County","CARefSite_2017","COMID_NHD2")]
    data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
                            select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
    nolu.cluster <- paste(clustertype, "_noland", sep="")
    lu.cluster <- paste(clustertype, "_land", sep="")
    
    # get sampling info (dates of samples)
    mySamps <- data.SampSummary[data.SampSummary[,"StationID_Master"]==TargetSiteID
                                ,c("CollDate","ChemSampleID","PhabSampID"
                                   ,"BMI.Metrics.SampID","Algae.Metrics.SampID")]
    # get response information (CSCI, H20, etc)
    myBMImetrics <- data.bmi.metrics[data.bmi.metrics[,"StationID_Master"]==TargetSiteID
                                     ,c("CollDate","CSCI","O_E","MMI_Score")]
    myAlgaeMetrics <- data.algae.metrics[data.algae.metrics[,"StationCode"]==TargetSiteID
                                         ,c("SampleDate","H20","D18","S2")]
    # get COMID 
    myCOMID <- mySiteInfo$COMID_NHD2
    myWBName <- mySiteInfo$WaterbodyName
    myReachInfo <- data.cluster[data.cluster[,"COMID"]==myCOMID,c("H6_noland","H6_land"
                                                                  ,"ElevWs","WsAreaSqKm","PrecipWs", "TmeanWs"
                                                                  ,"W___AGRIC","W___URBAN","W___FOREST")]
    myClustIDs <- myReachInfo[,c("H6_noland","H6_land")]
    
    myReachMods <- data.mod[data.mod[,"COMID"]==myCOMID,c("ReachModStatus", "ModReason")]
    
    my303d.COMID <- subset(data.303d.ComID, data.303d.ComID$ComID == myCOMID)
    my303d.COMID.WBName <- subset(my303d.COMID, my303d.COMID$WATER.BODY.NAME %in% myWBName)
    myCurrent303d <- subset(my303d.COMID.WBName, my303d.COMID.WBName$Year == 2012)
    myImpairments <- myCurrent303d[,c("ComID", "WATER.BODY.NAME", "POLLUTANT",
                                      "FINAL.LISTING.DECISION")]


    all.map.sites <- merge(data.Stations.Info, data.cluster, by.x = "COMID_NHD2", by.y = "COMID")
    if (useLU == TRUE) {
        df.plot.cl <- all.map.sites[all.map.sites[,lu.cluster]==myClustIDs[,2]
                    , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
    } else {
        df.plot.cl <- all.map.sites[all.map.sites[,lu.cluster]==myClustIDs[,1]
                    , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
    }
    
    
    # generate map -- I DON'T KNOW HOW TO MAKE THIS A SINGLE OBJECT TO ADD TO THE RETURNED LIST
    # HOW DO WE MAP JUST THE ECOREGION? IS THAT POSSIBLE?
    ppi <- 300
    png(file = paste0("Results/map.",TargetSiteID, ".png"),
        width = 4*ppi, height = 4*ppi, pointsize = 13)
        df.plotSite <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID,]
        #outline <- readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
        flowline <- readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85")
        #plot(outline, border=TRUE)
        plot(flowline, col="light blue")
        
        myLatitude <- "FinalLatitude"
        myLongitude <- "FinalLongitude"
        points(data.Stations.Info[,"FinalLongitude"], data.Stations.Info[,"FinalLatitude"]
               , col="gray", pch=19, cex=1.0)
        points(df.plot.cl[,"FinalLongitude"], df.plot.cl[,"FinalLatitude"], col="cyan3", pch=19, cex=1.0)
#        points(df.plot.cl[,"FinalLongitude"], df.plot.cl[,"FinalLatitude"], col="lightgoldenrod4", pch=19, cex=1.0)
        points(data.refSites[,"FinalLongitude"], data.refSites[,"FinalLatitude"], col="blue", pch=19, cex=1.2)
        points(df.plotSite[,myLongitude], df.plotSite[,myLatitude], col="red", pch=17, cex=1.8)
    dev.off()


    mySiteSummary <- list(SiteInfo = mySiteInfo, Samps = mySamps, BMImetrics = myBMImetrics
                            , AlgMetrics = myAlgaeMetrics, ReachInfo = myReachInfo
                            , COMID = myCOMID, ClustIDs = myClustIDs, impair = myImpairments
                            , mods = myReachMods)
    return(mySiteSummary)
}

getChemDataSubsets <- function(TargetSiteID, comid, cluster, clustertype, useLU) {
    #Create subsets for target sites, ref sites in cluster, all sites in cluster
    site.COMID <- comid
    site.Clusters <- cluster
    nolu.cluster <- paste(clustertype, "_noland", sep="")
    lu.cluster <- paste(clustertype, "_land", sep="")
    
    #Create a vector of Reference Site IDs
    data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
                            select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
    refSiteIDs <- as.vector(unique(data.refSites[,"StationID_Master"]))
    refSiteCOMIDs <- as.vector(unique(data.refSites[,"COMID_NHD2"]))

    data.clusterIDs <- data.cluster[,c("COMID",nolu.cluster,lu.cluster)]
    data.Stations.Clusters <- merge(data.Stations.Info, data.clusterIDs, by.x="COMID_NHD2",by.y="COMID")
    data.Stations.ClustIDs <- data.Stations.Clusters[,c("StationID_Master",nolu.cluster,lu.cluster)]
    data.chem.raw <- merge(data.chem.raw, data.Stations.ClustIDs, by.x="StationID_Master", by.y="StationID_Master")
    
    #Create stressor data cross-tabs
    site.chem <- subset(data.chem.raw, StationID_Master %in% TargetSiteID)
    
    # site.chem2 contains the chemicals detected at the target site
    site.chem2 <- subset(site.chem, !is.na(site.chem["ResultValue"]))
    chems <- unique(site.chem2["ConvertTo"])
    chems.groups <- merge(chems, data.chem.info, by.x="ConvertTo", by.y="Analyte")
    
    # chems.groups.sort is the list of chems detected at the target site, in group sort order
    chems.groups.sort <- chems.groups[order(chems.groups$GroupNum, chems.groups$ConvertTo),]
    numgps <- length(unique(chems.groups$GroupName))
    groupnames <- unique(chems.groups.sort$GroupName)
    site.lu <- site.Clusters[lu.cluster]
    site.nolu <- site.Clusters[nolu.cluster]
    
    # all.chems is the list of target site chems across all sites in dataset (all clusters)
    all.chems <- subset(data.chem.raw, ConvertTo %in% chems$ConvertTo)
    all.chems2 <- all.chems[,c("ChemSampleID","ConvertTo","ResultValue")]
    all.chems3 = cast(all.chems2, ChemSampleID ~ ConvertTo, mean)
    

    # chem.tab2 is the list of target site chems at sites in the target site cluster
    if (useLU == TRUE) {
        cluster.chem.tab2 <- subset(all.chems, all.chems[,lu.cluster]==site.lu[,1])
    } else {
        cluster.chem.tab2 <- subset(all.chems, all.chems[,nolu.cluster]==site.nolu[,1])
    }
    
    cluster.chem.tab3 <- cluster.chem.tab2[,c("ChemSampleID","ConvertTo","ResultValue")]
    cluster.chem.samps <- unique(cluster.chem.tab2[,c("StationID_Master","ChemSampleID")])
    cluster.chem.tab4 = cast(cluster.chem.tab3, ChemSampleID ~ ConvertTo, mean)
    cluster.chem.tab5 = merge(cluster.chem.samps, cluster.chem.tab4, by.x = "ChemSampleID", by.y = "ChemSampleID")
    site.chem3 <- site.chem2[,c("ChemSampleID", "ConvertTo", "ResultValue")]
    site.chem4 = cast(site.chem3, ChemSampleID ~ ConvertTo, mean)
    
    mySubsets <- list(ref.sites = refSiteIDs, ref.reaches = refSiteCOMIDs, cluster.samps = cluster.chem.samps
                      , chem.info = chems.groups.sort, all.chems = all.chems3
                      , cluster.chem = cluster.chem.tab5, site.chem = site.chem4)
    return(mySubsets)
}

getClusterInfo <- function(site.COMID, clustertype, siteClusters, refSiteCOMIDs, useLU = FALSE) {
    nolu.cluster <- paste(clustertype, "_noland", sep="")
    lu.cluster <- paste(clustertype, "_land", sep="")
    if (length(siteClusters)==0) {
        # do not proceed
        # no cluster assignment
        stop(paste("No cluster assignment for", TargetSiteID, sep = " "))
    }
    
    data.cluster.mySites <- data.cluster[data.cluster$COMID %in% site.COMID,]
    df.plot.3 <- data.cluster[data.cluster$COMID %in% refSiteCOMIDs,]
    png(file = paste0("Results/cluster.example.",TargetSiteID, ".png"),
        width = 1200, height = 800, pointsize = 13)
    if (useLU == FALSE) {
        selvar <- c("WsAreaSqKm","PrecipWs","TmeanWs","SLOPE","MAXELEVSMO")
        varnames <- c("WS Area", "WS Precipitation","Mean Temp", "Slope", "Max Elevation")
        par(mfrow = c(2,3), mar = c(2,4,1,1))
        df.plot <- data.cluster
        df.plot.2 <- data.cluster.mySites
        for(ii in 1:length(selvar)) {
            myY <- df.plot[,selvar[ii]]
            myX <- df.plot[,nolu.cluster]
            boxplot(myY~myX, main = "Clusters w/o Land Use", xlab ="Cluster"
                    , ylab = varnames[ii], boxwex = 0.5, col ="lightgray")
            ############
            # add points to plots for reference sites
            myY <- df.plot.3[,selvar[ii]]
            myX <- df.plot.3[,nolu.cluster]
            points(myX,myY,col="blue",cex=1.8,pch=19)
           ############
            # add points to plots for selected sites
            myY <- df.plot.2[,selvar[ii]]
            myX <- df.plot.2[,nolu.cluster]
            points(myX,myY,col="red",cex=1.8,pch=19)
            ############
        }    
    } else {
        data.cluster.mySites <- data.cluster[data.cluster$COMID %in% site.COMID,]
        selvar <- c("WsAreaSqKm","PrecipWs","TmeanWs","SLOPE","MAXELEVSMO")
        varnames <- c("W_Area", "WS Precipitation","Mean Temp", "Slope", "Max Elevation")
        par(mfrow = c(2,3), mar = c(2,4,1,1))
        df.plot <- data.cluster
        df.plot.2 <- data.cluster.mySites
        for(ii in 1:length(selvar)) {
            myY <- df.plot[,selvar[ii]]
            myX <- df.plot[,lu.cluster]
            boxplot(myY~myX, main = "Clusters w/Land Use", xlab ="Cluster"
                    , ylab = varnames[ii], boxwex = 0.5, col ="lightgray")
            ############
            # add points to plots for reference sites
            myY <- df.plot.3[,selvar[ii]]
            myX <- df.plot.3[,lu.cluster]
            points(myX,myY,col="blue",cex=1.8,pch=19)
            ############
            # add points to plots for selected sites
            myY <- df.plot.2[,selvar[ii]]
            myX <- df.plot.2[,lu.cluster]
            points(myX,myY,col="red",cex=1.8,pch=19)
            ############
        }
    }
    dev.off()
}
    
getStressorList <- function(TargetSiteID, site.Clusters, chem.info, cluster.chem
                            , cluster.samps, ref.sites, site.chem
                            , probsHigh, probsLow) {

    stations <- TargetSiteID
    nolu.cluster <- paste(clustertype, "_noland", sep="")
    lu.cluster <- paste(clustertype, "_land", sep="")
    
    cluster.chem.data <- cluster.chem[3:ncol(cluster.chem)]
    cluster.ref.chem <- subset(cluster.chem, cluster.chem$StationID_Master %in% ref.sites)
    cluster.ref.chem.data <- cluster.ref.chem[3:ncol(cluster.ref.chem)]
    chemnames <- names(cluster.chem[,3:ncol(cluster.chem.data)])
    allcount <- apply(cluster.chem.data, 2, function(x) sum(!is.na(x)))
    alltype <- unlist(lapply(1:ncol(cluster.chem.data), function(x) is.numeric(cluster.chem[,x])))
    coolvar <- names(allcount)[allcount>2 & alltype]

    groupnames <- unique(subset(chem.info, chem.info$ConvertTo %in% chemnames, select = "GroupName"))
    numgps <- length(groupnames[,1])
    
    ppi <- 300
    
    for (g in 1:numgps) {    # Generate 1 box plot for each group, ref sites in blue, target site in red
        gpchems <- subset(chem.info, GroupName == groupnames[g,], select = "ConvertTo")
        gpcoolvar <- subset(coolvar, coolvar %in% gpchems$ConvertTo)
        n <- length(gpcoolvar)
        if(n>0) { ##FOR.n.START
            png(file = paste0("Results/boxes.example.",TargetSiteID
                            , ".", groupnames[g,], ".png"), width = 4*ppi
                            , height = 3*ppi, pointsize = 13)
            maintitle <- paste(groupnames[g,], "Standardized values, All sites in cluster", sep=", ")
            par(mfrow = c(1,1), mar = c(4,8,1,1))
            if (useLU == TRUE) {
                labmain = paste(stations, ": Cluster", site.Clusters[1,lu.cluster])
            } else {
                labmain = paste(stations, ": Cluster", site.Clusters[1,nolu.cluster])
            }
            labx = paste(maintitle, labmain, sep = "\n")
            plot(y= 1:n, x= runif(n,0,1), axes = F, type="n", xlab = labx, ylab ="",
                 xlim = c(0,1))
            axis(1, at = seq(0,1, 0.2),labels = seq(0,1, 0.2))
            axis(2, at = 1:n, labels = gpcoolvar[1:n], las =1, cex.axis = .6)
            for(i in 1:n) {
                xvar <- cluster.chem[,gpcoolvar[i]]; dif <- diff(range(xvar, na.rm =T))
                newvar <- (xvar-min(xvar, na.rm=T))/dif
                boxplot(newvar, at = i,boxwex=.5, horizontal =T, add =T,axes = F)
                good.ref.data <- cluster.ref.chem.data[,gpcoolvar[i]][!is.na(cluster.ref.chem[,gpcoolvar[i]])]
                if (length(good.ref.data) != 0) {
                    point2 <- (cluster.ref.chem.data[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
                    points(point2, rep(i,length(point2)), col = "blue", pch = 15,cex=1.2, bg = 2)
                }
                point1 <- (site.chem[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
                points(point1, rep(i,length(point1)), col = "red", pch = 19,cex=1.2, bg = 2)
            }
            box(bty="l")
            dev.off()
        }
        
    }
    chem.pctrank <- apply(cluster.chem[,3:ncol(cluster.chem)], 2, function(x) percent_rank(x))
    data.chem.pctrank <- as.data.frame(chem.pctrank)
    data.chem.pctrank <- cbind(cluster.chem$StationID_Master,cluster.chem$ChemSampleID,data.chem.pctrank)
    colnames(data.chem.pctrank)[1] <- "StationID_Master"
    colnames(data.chem.pctrank)[2] <- "ChemSampleID"
    row.names(data.chem.pctrank) <- NULL
    write.table(data.chem.pctrank, file = paste("Results/chem.pctrank.",TargetSiteID,".txt", sep="")
                ,sep="\t", col.names=TRUE)
    site.pctrank <- subset(data.chem.pctrank, StationID_Master==TargetSiteID)
    stressor <- c("none")
    for (c in 3:ncol(site.pctrank)) {
        chemname <- colnames(site.pctrank)[c]
        bad <- is.na(site.pctrank[,c])
        check <- site.pctrank[,c]
        good <- check[!bad]
        maxSiteVal <- max(good)
        minSiteVal <- min(good)
        if ((chemname == "DO_uf_mg_L") || (chemname == "pH")) {
            if (minSiteVal <= probsLow) {
                stressor <- c(stressor, chemname)
            }
        }
        if ((chemname != "DO_uf_mg_L") && (maxSiteVal >= probsHigh)) {
            stressor <- c(stressor, chemname)
        }
    }
    stressorlist <- stressor
    myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank)
    
} # FUN end


getBMIMatches <- function(stressors, list.data) {

    all.chems <- list.data[["all.chems"]]
    cl.chems <- list.data[["cluster.chem"]]
    site.chem <- list.data[["site.chem"]]

    # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
    # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
    # These aren't in all.chems, because they don't have data corresponding to the site data
    useChemSamps <- all.chems$ChemSampleID
    mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
    
    mbmi.Samps <- na.omit(data.SampSummary[,c("ChemSampleID","BMI.Metrics.SampID")])
    mbmi.use.samps <- subset(mbmi.Samps, mbmi.Samps$ChemSampleID %in% mUseSamps)
    
    # bmi stressor data to use: all.mbmi.stress, cl.mbmi.stress, and site.stress
    all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
    all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
                        , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
    all.mbmi.stress <- subset(all.stress, ChemSampleID %in% mbmi.use.samps$ChemSampleID)
    all.mbmi.stress <- merge(mbmi.use.samps, all.mbmi.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
    cl.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% cl.chems$ChemSampleID)
    site.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% site.chem$ChemSampleID)
    
    # bmi response data to use: all.mbmi.resp, cl.mbmi.resp, and site.mbmi.resp
    all.resp <- subset(data.bmi.metrics, BMISampleID %in% mbmi.use.samps$BMI.Metrics.SampID)
    all.mbmi.resp <- merge(mbmi.use.samps, all.resp, by.x = "BMI.Metrics.SampID", by.y = "BMISampleID")
    cl.mbmi.resp <- subset(all.mbmi.resp, ChemSampleID %in% cl.chems$ChemSampleID)
    site.mbmi.resp <- subset(all.mbmi.resp, ChemSampleID %in% site.chem$ChemSampleID)
        
    myMatchData <- list(all.b.str = all.mbmi.stress
                            , cl.b.str = cl.mbmi.stress
                            , site.b.str = site.mbmi.stress
                            , all.b.rsp = all.mbmi.resp
                            , cl.b.rsp = cl.mbmi.resp
                            , site.b.rsp = site.mbmi.resp)
    return(myMatchData)
}

getAlgMatches <- function(stressors, list.data) {
    
    all.chems <- list.data[["all.chems"]]
    cl.chems <- list.data[["cluster.chem"]]
    site.chem <- list.data[["site.chem"]]
    ref.sites <- list.data[["ref.sites"]]
    
    # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
    # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
    # These aren't in all.chems, because they don't have data corresponding to the site data
    useChemSamps <- all.chems$ChemSampleID
    mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
    
    malg.Samps <- na.omit(data.SampSummary[,c("ChemSampleID","Algae.Metrics.SampID")])
    malg.use.samps <- subset(malg.Samps, malg.Samps$ChemSampleID %in% mUseSamps)
    
    # bmi stressor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
    all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
    all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
                        , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
    
    # alg stresor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
    all.malg.stress <- subset(all.stress, ChemSampleID %in% malg.use.samps$ChemSampleID)
    all.malg.stress <- merge(malg.use.samps, all.malg.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
    cl.malg.stress <- subset(all.malg.stress, ChemSampleID %in% cl.chems$ChemSampleID)
    site.malg.stress <- subset(all.malg.stress, ChemSampleID %in% site.chem$ChemSampleID)
    
    # alg response data to use: all.malg.resp, cl.malg.resp, and site.malg.resp
    all.malg.resp <- subset(data.algae.metrics, StationDateRep %in% malg.use.samps$Algae.Metrics.SampID)
    cl.malg.resp <- subset(all.malg.resp, StationDateRep %in% cl.chems$ChemSampleID)
    site.malg.resp <- subset(all.malg.resp, StationDateRep %in% site.chem$ChemSampleID)

    myMatchData <- list(all.a.str = all.malg.stress
                        , cl.a.str = cl.malg.stress
                        , site.a.str = site.malg.stress
                        , all.a.rsp = all.malg.resp
                        , cl.a.rsp = cl.malg.resp
                        , site.a.rsp = site.malg.resp )
}

getBMIStressorResponses <- function(stressors,list.MatchBMIData) {
    
    for (p in 1:length(stressors)) {
        stressName <- stressors[p]
        varFlag <- 1
        if (stressName %in% c("DO_uf_mg_L", "pH", "Temp_degC")) {
            log.yn <- FALSE
        } else {
            log.yn <- TRUE
        }
        for (r in 1: length(BMIresp)) {
            respName <- BMIresp[r]
            
            #get all data to plot
            all.xvar<- list.MatchBMIData[["all.b.str"]][,c("StationID_Master","BMI.Metrics.SampID", stressName)]
            all.yvar<- list.MatchBMIData[["all.b.rsp"]][,c("StationID_Master","BMI.Metrics.SampID", respName)]
            df.plot1 <- merge(all.xvar[,2:3],all.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
            all.df.plot <- df.plot1[complete.cases(df.plot1),2:3]
            
            #get all ref   data to plot
            all.ref.xvar <- subset(all.xvar, all.xvar$StationID_Master %in% ref.sites)
            all.ref.yvar <- subset(all.yvar, all.yvar$StationID_Master %in% ref.sites)
            df.plot2 <- merge(all.ref.xvar[,2:3],all.ref.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
            all.ref.df.plot <- df.plot2[complete.cases(df.plot2),2:3]
            
            #get all cluster data to plot
            cl.xvar<- list.MatchBMIData[["cl.b.str"]][,c("StationID_Master","BMI.Metrics.SampID", stressName)]
            cl.yvar<- list.MatchBMIData[["cl.b.rsp"]][,c("StationID_Master","BMI.Metrics.SampID", respName)]
            df.plot3 <- merge(cl.xvar[,2:3],cl.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
            cl.df.plot <- df.plot3[complete.cases(df.plot3),2:3]
            
            #get all cluster ref data to plot
            cl.ref.xvar <- subset(cl.xvar, cl.xvar$StationID_Master %in% ref.sites)
            cl.ref.yvar <- subset(cl.yvar, cl.yvar$StationID_Master %in% ref.sites)
            df.plot4 <- merge(cl.ref.xvar[,2:3],cl.ref.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
            cl.ref.df.plot <- df.plot4[complete.cases(df.plot4),2:3]
            
            #get target site data to plot
            site.xvar<- list.MatchBMIData[["site.b.str"]][,c("BMI.Metrics.SampID", stressName)]
            site.yvar<- list.MatchBMIData[["site.b.rsp"]][,c("BMI.Metrics.SampID", respName)]
            df.plot5 <- merge(site.xvar,site.yvar, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
            site.df.plot <- df.plot5[complete.cases(df.plot5),2:3]
            
            #jpeg(filename = paste(varFileOut,varXName,"_", varYName,".jpg", sep = ""), width = 1028, height = 768,quality=100,pointsize=14)
            par(cex.main=1.0,cex.lab=1.0,font.main=2, font.lab=2)
            if (log.yn == TRUE) {
                all.df.plot <- cbind(log10(all.df.plot[,1]),all.df.plot[,2])
                all.ref.df.plot <- cbind(log10(all.ref.df.plot[,1]),all.ref.df.plot[,2])
                cl.df.plot <- cbind(log10(cl.df.plot[,1]),cl.df.plot[,2])
                cl.ref.df.plot <- cbind(log10(cl.ref.df.plot[,1]),cl.ref.df.plot[,2])
                site.df.plot <- cbind(log10(site.df.plot[,1]),site.df.plot[,2])
            }
            
            varMain <- paste("Linear regression of", stressName, "on", respName
                             , "for", TargetSiteID, "\n","with", paste(predint*100, "th", sep= "")
                             , "percentile prediction interval", sep = " ")
            if (log.yn == TRUE) {
                varxlab <- paste("Log10", stressName)
            } else {
                varxlab <- stressName
            }
            # There should never be a case where either x or y are always NA for all data
            if (length(all.ref.df.plot) > 0) {
                plot(all.df.plot[,2]~all.df.plot[,1],main=varMain,xlab=varxlab,ylab=respName, col="grey", pch=1)
            } else {
                next
            }
            if (length(all.ref.df.plot) > 0) {
                points(all.ref.df.plot[,2]~all.ref.df.plot[,1], col="blue", pch=16) # blue solid dots
            }
            if (length(cl.df.plot) > 0) {
                points(cl.df.plot[,2]~cl.df.plot[,1], col="darkorange3", pch=2) # Black open triangles
            }
            if (length(cl.ref.df.plot) > 0) {
                points(cl.ref.df.plot[,2]~cl.ref.df.plot[,1], col="blue", pch=17) # Solid blue triangles
            }
            if (length(site.df.plot) > 0) {
                points(site.df.plot[,2]~site.df.plot[,1], col="red", pch=19, cex = 1.2) # black solid dots
            }
            
            cl.x.sd <- sd(cl.df.plot[,1])
            cl.y.sd <- sd(cl.df.plot[,2])
            #Check for vertical line
            if (!is.na(cl.x.sd)) {
                if (cl.x.sd == 0) {
                    print(paste("Vertical line for", stressName, respName, sep=" "))
                    flush.console()
                    next     #It's okay to plot the points, but not the regression line
                }
            }
            #Check for horizontal line
            if (!is.na(cl.y.sd)) {
                if (cl.y.sd == 0) {
                    print(paste("Horizontal line for", stressName, respName, sep=" "))
                    flush.console()
                    next     #It's okay to plot the points, but not the regression line
                }
            }    
            
            #Linear Regression (uses cluster data -- all sites in the cluster)
            varY <- cl.df.plot[,2]
            varX <- cl.df.plot[,1]
            fit = lm(varY~varX)
            pred.int = predict(fit,interval="prediction",level=predint)
            fitted.values = pred.int[,1]
            pred.lower = pred.int[,2]
            pred.upper = pred.int[,3]
            
            abline(lm(varY~varX), col="darkorange3", lwd=2)
            abline(lm(pred.lower~varX), col="darkorange3", lwd=1.5)
            abline(lm(pred.upper~varX), col="darkorange3", lwd=1.5)
            # 
            slope <- summary(lm(varY~varX))[[4]][[2]]
            intercept <- summary(lm(varY~varX))[[4]][[1]]
            pval_intercept <- summary(lm(varY~varX))[[4]][[7]]
            pval_slope <- summary(lm(varY~varX))[[4]][[8]]
            slope = signif(slope, 3)
            intercept = signif(intercept, 3)
            pval_intercept = signif(pval_intercept, 3)
            pval = signif(pval_slope, 3)
            # # r� text and legend
            r = cor(varX, varY, method="pearson",use="pairwise.complete.obs")
            r2 = formatC(r^2,format="f",digits=3)
            # 
            c1S <- (cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
            df.corr = data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
                                       , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
            # # Create results data frame
            if (varFlag==1) {  #First time through loop
                df.CorrTable <- c(df.corr)
            } # IF, END
            df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
            pval.corr = signif(c1S$p.value,2)
            
            #Print equation, r2, and p-value
            if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
                eqn <- paste("Cluster regression\n"
                             , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
                             ,"p-value = ",pval.corr,"\n","n = ",length(varX),"\n")
                symbshape <- c(1, 16, 2, 17, 19)
                symbcol <- c("grey", "blue", "darkorange3", "blue", "red")
                symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
                legend(varLegLoc, inset = varInset, (paste("Cluster regression\n"
                                                           , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
                                                           ,"p-value = ",pval.corr,"\n","n = ",length(varX))), bty="n"
                       , col = c("black"), cex=0.8)
                legend(varLegOpp,inset=varInset, symbname, pch=symbshape, col=symbcol, cex=0.6)
            }
            varFlag <- 0
        }
    }
    write.table(df.CorrTable,file="StressRespCorrs.BMI.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)  
}

getAlgStressorResponses <- function(stressors,list.MatchAlgData) {
    
    for (p in 1:length(stressors)) {
        stressName <- stressors[p]
        if (stressName %in% c("DO_uf_mg_L", "pH", "Temp_degC")) {
            log.yn <- FALSE
        } else {
            log.yn <- TRUE
        }
        varFlag <- 1
        for (r in 4:length(AlgResp)) {
            respName <- AlgResp[r]
            
            #get all data to plot
            all.xvar<- list.MatchAlgData[["all.a.str"]][,c("StationID_Master","Algae.Metrics.SampID", stressName)]
            all.yvar<- list.MatchAlgData[["all.a.rsp"]][,c("StationCode","StationDateRep", respName)]
            df.plot1 <- merge(all.xvar[,2:3],all.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
            all.df.plot <- df.plot1[complete.cases(df.plot1),2:3]
            
            #get all ref   data to plot
            all.ref.xvar <- subset(all.xvar, all.xvar$StationID_Master %in% ref.sites)
            all.ref.yvar <- subset(all.yvar, all.yvar$StationCode %in% ref.sites)
            df.plot2 <- merge(all.ref.xvar[,2:3],all.ref.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
            all.ref.df.plot <- df.plot2[complete.cases(df.plot2),2:3]
            
            #get all cluster data to plot
            cl.xvar<- list.MatchAlgData[["cl.a.str"]][,c("StationID_Master","Algae.Metrics.SampID", stressName)]
            cl.yvar<- list.MatchAlgData[["cl.a.rsp"]][,c("StationCode","StationDateRep", respName)]
            df.plot3 <- merge(cl.xvar[,2:3],cl.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
            cl.df.plot <- df.plot3[complete.cases(df.plot3),2:3]
            
            #get all cluster ref data to plot
            cl.ref.xvar <- subset(cl.xvar, cl.xvar$StationCode %in% ref.sites)
            cl.ref.yvar <- subset(cl.yvar, cl.yvar$StationCode %in% ref.sites)
            df.plot4 <- merge(cl.ref.xvar[,2:3],cl.ref.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
            cl.ref.df.plot <- df.plot4[complete.cases(df.plot4),2:3]
            
            #get target site data to plot
            site.xvar<- list.MatchAlgData[["site.a.str"]][,c("Algae.Metrics.SampID", stressName)]
            site.yvar<- list.MatchAlgData[["site.a.rsp"]][,c("StationDateRep", respName)]
            df.plot5 <- merge(site.xvar,site.yvar, by.x = "Algae.Metrics.SampID", by.y = "StationDateRep")
            site.df.plot <- df.plot5[complete.cases(df.plot5),2:3]
            
            #jpeg(filename = paste(varFileOut,varXName,"_", varYName,".jpg", sep = ""), width = 1028, height = 768,quality=100,pointsize=14)
            par(cex.main=1.0,cex.lab=1.0,font.main=2, font.lab=2)
            if (log.yn == TRUE) {
                all.df.plot <- cbind(log10(all.df.plot[,1]),all.df.plot[,2])
                all.ref.df.plot <- cbind(log10(all.ref.df.plot[,1]),all.ref.df.plot[,2])
                cl.df.plot <- cbind(log10(cl.df.plot[,1]),cl.df.plot[,2])
                cl.ref.df.plot <- cbind(log10(cl.ref.df.plot[,1]),cl.ref.df.plot[,2])
                site.df.plot <- cbind(log10(site.df.plot[,1]),site.df.plot[,2])
            }
            
            varMain <- paste("Linear regression of", stressName, "on", respName
                             , "for", TargetSiteID, "\n","with", paste(predint*100, "th", sep= "")
                             , "percentile prediction interval", sep = " ")
            if (log.yn == TRUE) {
                varxlab <- paste("Log10", stressName)
            } else {
                varxlab <- stressName
            }
            # There should never be a case where either x or y are always NA for all data
            if (length(all.ref.df.plot) > 0) {
                plot(all.df.plot[,2]~all.df.plot[,1],main=varMain, xlab=varxlab,ylab=respName, col="grey", pch=1)
            } else {
                next
            }
            if (length(all.ref.df.plot) > 0) {
                points(all.ref.df.plot[,2]~all.ref.df.plot[,1], col="blue", pch=16) # blue solid dots
            }
            if (length(cl.df.plot) > 0) {
                points(cl.df.plot[,2]~cl.df.plot[,1], col="darkorange3", pch=2) # Red open triangles
            }
            if (length(cl.ref.df.plot) > 0) {
                points(cl.ref.df.plot[,2]~cl.ref.df.plot[,1], col="blue", pch=17) # Solid blue triangles
            }
            if (length(site.df.plot) > 0) {
                points(site.df.plot[,2]~site.df.plot[,1], col="red", pch=19, cex = 1.2) # black solid dots
            }
            
            cl.x.sd <- sd(cl.df.plot[,1])
            cl.y.sd <- sd(cl.df.plot[,2])
            #Check for vertical line
            if (!is.na(cl.x.sd)) {
                if (cl.x.sd == 0) {
                    print(paste("Vertical line for", stressName, respName, sep=" "))
                    flush.console()
                    next     #It's okay to plot the points, but not the regression line
                }
            }
            #Check for horizontal line
            if (!is.na(cl.y.sd)) {
                if (cl.y.sd == 0) {
                    print(paste("Horizontal line for", stressName, respName, sep=" "))
                    flush.console()
                    next     #It's okay to plot the points, but not the regression line
                }
            }    

            #Linear Regression (uses cluster data -- all sites in the cluster)
            varY <- cl.df.plot[,2]
            varX <- cl.df.plot[,1]
            fit = lm(varY~varX)
            pred.int = predict(fit,interval="prediction",level=predint)
            fitted.values = pred.int[,1]
            pred.lower = pred.int[,2]
            pred.upper = pred.int[,3]
            
            abline(lm(varY~varX), col="darkorange3", lwd=2)
            abline(lm(pred.lower~varX), col="darkorange3", lwd=1.5)
            abline(lm(pred.upper~varX), col="darkorange3", lwd=1.5)
            # 
            slope <- summary(lm(varY~varX))[[4]][[2]]
            intercept <- summary(lm(varY~varX))[[4]][[1]]
            pval_intercept <- summary(lm(varY~varX))[[4]][[7]]
            pval_slope <- summary(lm(varY~varX))[[4]][[8]]
            slope = signif(slope, 3)
            intercept = signif(intercept, 3)
            pval_intercept = signif(pval_intercept, 3)
            pval = signif(pval_slope, 3)
            # # r� text and legend
            r = cor(varX, varY, method="pearson",use="pairwise.complete.obs")
            r2 = formatC(r^2,format="f",digits=3)
            # 
            c1S <- (cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
            df.corr = data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
                                       , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
            # # Create results data frame
            if (varFlag==1) {  #First time through loop
                df.CorrTable <- c(df.corr)
            } # IF, END
            df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
            pval.corr = signif(c1S$p.value,2)
            
            #Print equation, r2, and p-value
            if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
                eqn <- paste("Cluster regression\n"
                             , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
                             ,"p-value = ",pval.corr,"\n","n = ",length(varX),"\n")
                symbshape <- c(1, 16, 2, 17, 19)
                symbcol <- c("grey", "blue", "darkorange3", "blue", "red")
                symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
                legend(varLegLoc, inset = varInset, (paste("Cluster regression\n"
                                                           , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
                                                           ,"p-value = ",pval.corr,"\n","n = ",length(varX))), bty="n"
                       , col = c("black"), cex=0.8)
                legend(varLegOpp,inset=varInset, symbname, pch=symbshape, col=symbcol, cex=0.6)
            }
            varFlag <- 0
        }
    }
    write.table(df.CorrTable,file="StressRespCorrs.Algae.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)  
    
}

getStressorSpecificRegressions <- function(matchedData) {
    
    SSTV <- subset(data.chem.info, SSTV != 0, c("Analyte", "SSTV", "SensMin"
                                                , "SensMax", "TolMin", "TolMax"))
    
    if (nrow(SSTV) != 0) {
        stressor.SSTV <- subset(SSTV, Analyte %in% stressors)
        if (nrow(stressor.SSTV) != 0) {
            
            for (tv in 1:nrow(stressor.SSTV)) {        # Currently only valid for SpecCond
                SSTV.analyte <- as.vector(SSTV$Analyte)
                SSTV.name <- as.vector(SSTV$SSTV)
                if (SSTV.analyte %in% c("DO_uf_mg_L", "pH", "Temp_degC")) {
                    log.yn <- FALSE
                } else {
                    log.yn <- TRUE
                }
                # get all the matched sample data for this stressor
                all.match.b <- matchedData$all.b.str[,c("StationID_Master"
                            , "ChemSampleID", "BMI.Metrics.SampID", SSTV.analyte)]
                cl.match.b <- matchedData$cl.b.str[,c("StationID_Master"
                            , "ChemSampleID", "BMI.Metrics.SampID")]
                all.SSTV.str <- all.match.b[c("StationID_Master", "ChemSampleID"
                            , "BMI.Metrics.SampID", SSTV.analyte)]
                # get all the matched taxonomic data
                SSTV.bmi.samps <- merge(data.SampSummary[,c("BMI.Metrics.SampID", "BMISampID")]
                            , all.SSTV.str, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
                SSTV.bmi.taxa <- merge(SSTV.bmi.samps["BMISampID"], data.bmi.taxa.raw
                            , by.x="BMISampID", by.y = "BMISampleID")
                
                totabund.bySamp <- tapply(SSTV.bmi.taxa$SumOfResult_Value
                                          , SSTV.bmi.taxa$BMISampID, sum)
                totabund.bySamp <- cbind(row.names(totabund.bySamp), totabund.bySamp)
                row.names(totabund.bySamp) <- NULL
                colnames(totabund.bySamp)[1] <- "BMISampID"
                colnames(totabund.bySamp)[2] <- "SampleAbundance"
                totabund.bySamp[is.na(totabund.bySamp)] <- 0  # if sum = NA, then sum = zero  (OKAY)
                totabund.bySampTV <- with(SSTV.bmi.taxa, tapply(SSTV.bmi.taxa$SumOfResult_Value
                            , list(SSTV.bmi.taxa$BMISampID, SSTV.bmi.taxa$SpecCondTolVal), sum))
                totabund.bySampTV <- cbind(row.names(totabund.bySampTV), totabund.bySampTV)
                totabund.bySampTV[is.na(totabund.bySampTV)] <- 0  # if sum = NA, then sum = zero
                colnames(totabund.bySampTV)[1] <- "BMISampID"
                colnames(totabund.bySampTV)[2:7] <- c("TV1", "TV2", "TV3", "TV4"
                                                      , "TV5", "TV6")
                totabund.cat.bySamp <- cbind(totabund.bySampTV, (as.numeric(totabund.bySampTV[,"TV1"])
                            + as.numeric(totabund.bySampTV[,"TV2"])), (as.numeric(totabund.bySampTV[,"TV5"])
                            + as.numeric(totabund.bySampTV[,"TV6"])))
                colnames(totabund.cat.bySamp)[8:9] <- c("SensTaxa", "TolTaxa")
                totabund.bySample <- merge(totabund.cat.bySamp, totabund.bySamp
                            , by.x = "BMISampID", by.y = "BMISampID")
                totabund.bySample <- subset(totabund.bySample, totabund.bySample[,"SampleAbundance"] != "0")
                write.table(totabund.bySample, file="data/data.totabund.bySample.tab"
                            , quote = FALSE, sep="\t", row.names = FALSE, col.names = TRUE)
                data.SSTV.totabund <- read.delim(paste(myDir.Data,"data.totabund.bySample.tab",sep=""))
                all.SSTV.totabund <- cbind(data.SSTV.totabund
                            , data.SSTV.totabund[,"SensTaxa"]/data.SSTV.totabund[,"SampleAbundance"]
                            , data.SSTV.totabund[,"TolTaxa"]/data.SSTV.totabund[,"SampleAbundance"])
                colnames(all.SSTV.totabund)[11:12] <- c("SensRelAbund", "TolRelAbund")
                all.SSTV.abund <- merge(SSTV.bmi.samps, all.SSTV.totabund, by.x = "BMISampID", by.y = "BMISampID")
                all.SSTV.abund <- all.SSTV.abund[, c("StationID_Master"
                                                     , "ChemSampleID", SSTV.analyte
                                                     , "SensRelAbund", "TolRelAbund")]
                good.SSTV.abund <- all.SSTV.abund[complete.cases(all.SSTV.abund),]
                all.ref.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% ref.sites)
                cl.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$ChemSampleID %in% cl.match.b$ChemSampleID)
                cl.ref.SSTV.abund <- subset(cl.SSTV.abund, cl.SSTV.abund$StationID_Master %in% ref.sites)
                site.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% TargetSiteID)
                SSTV.Resp <- c("SensRelAbund", "TolRelAbund")
                
                # good.SSTV.abund: StationID_Master, ChemSampleID, SSTV.analyte
                #, SensRelAbund, TotRelAbund; all sites
                # all.ref.SSTV.abund: StationID_Master, ChemSampleID, SSTV.analyte
                #, SensRelAbund, TotRelAbund; all referencesites
                # cl.SSTV.abund: StationID_Master, ChemSampleID, SSTV.analyte
                #, SensRelAbund, TotRelAbund; cluster sites
                # cl.ref.SSTV.abund: StationID_Master, ChemSampleID, SSTV.analyte
                #, SensRelAbund, TotRelAbund; reference sites in cluster
                # site.SSTV.abund: StationID_Master, ChemSampleID, SSTV.analyte
                #, SensRelAbund, TotRelAbund; target site
                
                varFlag <- 1
                for (r in 1:length(SSTV.Resp)) {
                    respName <- SSTV.Resp[r]
                    df.plot1 <- good.SSTV.abund[,c(SSTV.analyte,respName)]
                    df.plot2 <- all.ref.SSTV.abund[,c(SSTV.analyte,respName)]
                    df.plot3 <- cl.SSTV.abund[,c(SSTV.analyte,respName)]
                    df.plot4 <- cl.ref.SSTV.abund[,c(SSTV.analyte,respName)]
                    df.plot5 <- site.SSTV.abund[,c(SSTV.analyte,respName)]
                    
                    #jpeg(filename = paste(varFileOut,varXName,"_", varYName,".jpg", sep = ""), width = 1028, height = 768,quality=100,pointsize=14)
                    par(cex.main=1.0,cex.lab=1.0,font.main=2, font.lab=2)
                    if (log.yn == TRUE) {
                        df.plot1 <- cbind(log10(df.plot1[,1]),df.plot1[,2])
                        df.plot2 <- cbind(log10(df.plot2[,1]),df.plot2[,2])
                        df.plot3 <- cbind(log10(df.plot3[,1]),df.plot3[,2])
                        df.plot4 <- cbind(log10(df.plot4[,1]),df.plot4[,2])
                        df.plot5 <- cbind(log10(df.plot5[,1]),df.plot5[,2])
                    }
                    
                    if (respName == "SensRelAbund") {
                        respText <- "Sensitive Taxa Relative Abundance"
                    } else if (respName == "SensTotAbund") {
                        respText <- "Sensitive Taxa Abundance"
                    } else if (respName == "TolRelAbund") {
                        respText <- "Tolerant Taxa Relative Abundance"
                    } else {
                        respText <- "Tolerant Taxa Abundance"
                    }
                    
                    varMain <- paste("Linear regression of", SSTV.analyte, "on", respText
                                     , "for", TargetSiteID, "\n","with", paste(predint*100, "th", sep= "")
                                     , "percentile prediction interval", sep = " ")
                    if (log.yn == TRUE) {
                        varxlab <- paste("Log10", SSTV.analyte)
                    } else {
                        varxlab <- SSTV.analyte
                    }
                    # There should never be a case where either x or y are always NA for all data
                    if (length(df.plot1) > 0) {
                        plot(df.plot1[,2]~df.plot1[,1],main=varMain, xlab=varxlab,ylab=respText, col="grey", pch=1)
                    } else {
                        next
                    }
                    if (length(df.plot2) > 0) {
                        points(df.plot2[,2]~df.plot2[,1], col="blue", pch=16) # blue solid dots
                    }
                    if (length(df.plot3) > 0) {
                        points(df.plot3[,2]~df.plot3[,1], col="cyan4", pch=2) # Red open triangles
                    }
                    if (length(df.plot4) > 0) {
                        points(df.plot4[,2]~df.plot4[,1], col="blue", pch=17) # Solid blue triangles
                    }
                    if (length(df.plot5) > 0) {
                        points(df.plot5[,2]~df.plot5[,1], col="red", pch=19, cex = 1.2) # black solid dots
                    }
                    
                    cl.x.sd <- sd(df.plot3[,1])
                    cl.y.sd <- sd(df.plot3[,2])
                    #Check for vertical line
                    if (!is.na(df.plot3)) {
                        if (df.plot3 == 0) {
                            print(paste("Vertical line for", SSTV.analyte, respName, sep=" "))
                            flush.console()
                            next     #It's okay to plot the points, but not the regression line
                        }
                    }
                    #Check for horizontal line
                    if (!is.na(df.plot3)) {
                        if (df.plot3 == 0) {
                            print(paste("Horizontal line for", SSTV.analyte, respName, sep=" "))
                            flush.console()
                            next     #It's okay to plot the points, but not the regression line
                        }
                    }    
                    
                    #Linear Regression (uses cluster data -- all sites in the cluster)
                    varY <- df.plot3[,2]
                    varX <- df.plot3[,1]
                    fit = lm(varY~varX)
                    pred.int = predict(fit,interval="prediction",level=predint)
                    fitted.values = pred.int[,1]
                    pred.lower = pred.int[,2]
                    pred.upper = pred.int[,3]
                    
                    abline(lm(varY~varX), col="cyan4", lwd=2)
                    abline(lm(pred.lower~varX), col="cyan4", lwd=1.5)
                    abline(lm(pred.upper~varX), col="cyan4", lwd=1.5)
                    # 
                    slope <- summary(lm(varY~varX))[[4]][[2]]
                    intercept <- summary(lm(varY~varX))[[4]][[1]]
                    pval_intercept <- summary(lm(varY~varX))[[4]][[7]]
                    pval_slope <- summary(lm(varY~varX))[[4]][[8]]
                    slope = signif(slope, 3)
                    intercept = signif(intercept, 3)
                    pval_intercept = signif(pval_intercept, 3)
                    pval = signif(pval_slope, 3)
                    # # r� text and legend
                    r = cor(varX, varY, method="pearson",use="pairwise.complete.obs")
                    r2 = formatC(r^2,format="f",digits=3)
                    # 
                    c1S <- (cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
                    df.corr = data.frame(cbind(SSTV.analyte, respName, signif(c1S$statistic,2)
                                               , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
                    # # Create results data frame
                    if (varFlag==1) {  #First time through loop
                        df.CorrTable <- c(df.corr)
                    } # IF, END
                    df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
                    pval.corr = signif(c1S$p.value,2)
                    
                    #Print equation, r2, and p-value
                    if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
                        eqn <- paste("Cluster regression\n"
                                     , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
                                     ,"p-value = ",pval.corr,"\n","n = ",length(varX),"\n")
                        symbshape <- c(1, 16, 2, 17, 19)
                        symbcol <- c("grey", "blue", "cyan4", "blue", "red")
                        symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
                        legend(varLegLoc, inset = varInset, (paste("Cluster regression\n"
                                                                   , "y = ", slope, "x + ", intercept, "\n", "r� = ",r2,"\n"
                                                                   ,"p-value = ",pval.corr,"\n","n = ",length(varX))), bty="n"
                               , col = c("black"), cex=0.8)
                        legend(varLegOpp,inset=varInset, symbname, pch=symbshape, col=symbcol, cex=0.6)
                    }
                    
                    print(paste(SSTV.analyte, respName, sep="\t"))
                    flush.console()
                    
                    varFlag <- 0
                    
                }  # End For loop over responses
                
            }  # End For loop over stressors
            SSTVfile <- paste("Results/", TargetSiteID, ".SSTVCorrs.txt", sep="")
            write.table(df.CorrTable, file=SSTVfile, sep= "\t",quote=FALSE,row.names=FALSE,col.names=TRUE)
        }
    }    
}
    
    
##############
##   PREP   ##
##############

#Read all data files

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# data need for operation
## Stations - PickList     ## Change to include full list of stations w/bio data
# data.Stations <- read.delim(paste(myDir.Data,"data.Stations.LookUp.tab",sep=""))
# LU.Stations <- data.Stations[,"StationID"]
## Stations - Location Information     ##Updated to most recent (trimmed data)
## For ref stations, use CARefSite2017 column (T=1/F=0)
data.Stations.Info <- read.delim(paste(myDir.Data,"data.Stations.Info.tab",sep=""))
## Listing status, etc.
data.303d.ComID <- readRDS(paste0(myDir.Data,"data.303dcomid.RDS"))
## Data summary by site
data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep=""),na.strings = c(""," "))
## all cluster data (COMID, cluster assignments, and predictors)
data.cluster <- read.delim(paste(myDir.Data,"data.all.clust.tab",sep=""))
## Stressor data
data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
#data.phab.raw <- read.delim(paste(myDir.Data,"data.phab.raw.tab",sep=""))
data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
#data.phab.info <- read.delim(paste(myDir.Data,"data.phab.info.tab",sep=""))
## Response data (BMI/Algae Indices/Metrics)
data.algae.metrics <- read.delim(paste(myDir.Data,"data.algae.metrics.tab",sep=""))
data.bmi.metrics <- read.delim(paste(myDir.Data,"data.bmi.metrics.tab",sep=""))
## Sample matches (same day)
data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep=""), na.strings = c(""," "))
## Raw taxa data (abundance in each sample)
data.bmi.taxa.raw <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
## Taxonomic information (BMI) (Master taxa table)
data.bmi.info <- read.delim(paste(myDir.Data,"data.bmi.info.tab",sep=""))
## Modified Status     ##This is site-based mod/flow status. Need reach based, too
data.mod <- read.delim(paste(myDir.Data,"data.ModPerStatus.tab",sep=""))
## SSD data

## Criteria

#Set variables to get site, cluster information
TargetSiteID <- "404S16516"
clustertype <- "H6"
useLU <- FALSE
# set cutoff for possible stressor identification
probsLow <- 0.10
probsHigh <- 0.90
# For stressor-response analysis
varLegLoc <- "topright"
BMIresp <- c("CSCI", "O_E", "MMI_Score", "ClingerTaxaPct", "ColeopteraTaxaPct"
             , "EPTTaxaPct", "ShredderTaxa", "TaxaRichness")
AlgResp <- colnames(data.algae.metrics[4:ncol(data.algae.metrics)-3])

# For regression graphs
predint <- 0.75
if (varLegLoc == "topleft") {
    varInset = 0.01       #top inset = 0.05
    varSpacer = "\n\n\n"
    varLegOpp = "bottomright"
}
if (varLegLoc == "topright") {
    varInset = 0.01
    varSpacer = "\n\n\n\n"
    varLegOpp = "bottomleft"
}
if (varLegLoc == "bottomleft") {
    varInset = 0.01
    varSpacer = ""
    varLegOpp = "topright"
}
if (varLegLoc == "bottomright") {
    varInset = 0.01
    varSpacer = ""
    varLegOpp = "topleft"
}


##############
##   MAIN   ##
##############

list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
    # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo, Samps = mySamps, BMImetrics = myBMImetrics
    #                       , AlgMetrics = myAlgaeMetrics, ReachInfo = myReachInfo
    #                       , COMID = myCOMID, ClustIDs = myClustIDs)
    site.COMID <- list.SiteSummary$COMID
    site.Clusters <- list.SiteSummary$ClustIDs
    SiteInfo <- list.SiteSummary$SiteInfo

list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
    # Returns: mySubsets <- list(ref.sites = refSiteIDs, ref.reaches = refSiteCOMIDs, cluster.samps = cluster.chem.samps
    #                   , chem.info = chems.groups.sort, all.chems = all.chems3
    #                   , cluster.chem = cluster.chem.tab5, site.chem = site.chem4)
    ref.sites <- list.data$ref.sites
    ref.reaches <- list.data$ref.reaches
    cluster.samps <- list.data$cluster.samps
    cluster.chem <- list.data$cluster.chem
    # cluster.ref.chem <- subset(cluster.chem, cluster.chem$StationID_Master %in% ref.sites)
    site.chem <- list.data$site.chem
    chem.info <- list.data$chem.info
getClusterInfo(site.COMID, clustertype, site.Clusters, ref.reaches, useLU)
    # Should pass the plots as an object, but I have no idea how to do that, exactly
    # Need to capture the error condition of no cluster assignment!
list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
                                  , cluster.samps, ref.sites, site.chem
                                  , probsHigh, probsLow)
    # Returns: myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank)
    # Should pass the plots as an object, but I have no idea how to do that.
stressors <- list.stressors$stressors
if ((length(stressors) == 1) && stressors[1] == "none") {
    # No stressors returned
    print(paste("No stressors identified for site", TargetSiteID, sep = " "))
    flush.console()
} else {
    stressors <- c(stressors[2:length(stressors)])
    if (nrow(list.SiteSummary$BMImetrics) == 0) {
        # BMI Responses Found
        print(paste("No BMI response data available for ", TargetSiteID
                    , ". Regression data illustrate cluster relationships only."
                    , sep = ""))
        flush.console()
    }
    list.MatchBMIData <- getBMIMatches(stressors, list.data)
    # myBMIMatchData <- list(all.b.str = all.mbmi.stress
    #                        , cl.b.str = cl.mbmi.stress
    #                        , site.b.str = site.mbmi.stress
    #                        , all.b.rsp = all.mbmi.resp
    #                        , cl.b.rsp = cl.mbmi.resp
    #                        , site.b.rsp = site.mbmi.resp)
    getBMIStressorResponses(stressors, list.MatchBMIData)
    # Should return graphics as objects, but I don't know how to do this
    
    if (nrow(list.SiteSummary$AlgMetrics) > 0) {
        # Algae Responses Found
        print(paste("No algae response data available for ", TargetSiteID
                    , ". Regression data illustrate cluster relationships only."
                    , sep = ""))
        flush.console()
    }
    list.MatchAlgData <- getAlgMatches(stressors, list.data)
    # myAlgMatchData <- list(all.a.str = all.malg.stress
    #                        , cl.a.str = cl.malg.stress
    #                        , site.a.str = site.malg.stress
    #                        , all.a.rsp = all.malg.resp
    #                        , cl.a.rsp = cl.malg.resp
    #                        , site.a.rsp = site.malg.resp )
    # getAlgStressorResponses(stressors, list.MatchAlgData)
    # Should return graphics as objects, but I don't know how to do this
    
    getStressorSpecificRegressions(list.MatchBMIData)
    
    # getSSDs

}
