#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents 
#  is expressly prohibited without prior written permission of TetraTech.
#
#  You can contact the author at:
#  - RPPTool R package source repository : https://github.com/ALincolnTt/RPPTool


# Ann.RoseberryLincoln@tetratech.com
# Erik.Leppo@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.0.2
# 
# library(devtools)
# install_github("ALincolnTt/RPPTool")
#
# Add Shiny code for use in Shiny App
# 2020-09-10, Erik
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getConnectivity <- function(TargetCOMID
                            , cxndist_km = 5
                            , dfNetwork
                            , results_dir) { # FUNCTION.START
    #
    # rm(list=ls())
    boo_DEBUG <- FALSE
    start.time <- Sys.time()
    #
    if(boo_DEBUG==TRUE){
        TargetCOMID <- reach
        cxndist_km <- 10
        dfNetwork <- dfNetwork
        # fn_network <- file.path(getwd(),"Data","ConnectivityData.xlsx")
        # dfNetwork <- readxl::read_excel(fn_network, sheet=1, na="NA"
        #                                 , trim_ws = TRUE)
        
    }##IF~boo_DEBUG~END
    fn.output <- file.path(results_dir, paste0(TargetCOMID,"_Connections.tab"))
    
    `%>%` <- dplyr::`%>%`
    # not_all_na <- function(x) {!all(is.na(x))}

    getDSrecursive <- function(dfGraph,to.node,TargetCOMID) {
        comid <- TargetCOMID
        aggReachLen <- dfGraph$AggLengthKM[dfGraph$COMID==comid]
        dfGraph$UpDown[dfGraph$COMID==comid] <- "Down"
        dfGraph$ToNodeDone[dfGraph$COMID==comid] <- TRUE
        temp <- dfGraph[dfGraph$COMID==comid,]
        write.table(temp, fn.output, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        dfDSnode <- dplyr::filter(dfGraph, FromNode==to.node)

        # Explore edges coming from to.node (now from.node)
        if (aggReachLen<=(2.5*cxndist_km)) {
            if (nrow(dfDSnode)>0) {
                for (e in 1:nrow(dfDSnode)) {
                    comid <- dfDSnode$COMID[e]
                    if (dfGraph$EdgeDone[dfGraph$COMID==comid]==FALSE) {
                        to.node <- dfGraph$ToNode[dfGraph$COMID==comid]
                        if (dfGraph$ToNodeDone[dfGraph$COMID==comid]==FALSE) {
                            dfGraph$EdgeDone[dfGraph$COMID==comid] <- TRUE
                            lenReach2 <- dfGraph$LENGTHKM[dfGraph$COMID==comid]
                            dfGraph$AggLengthKM[dfGraph$COMID==comid] <- 
                                aggReachLen + lenReach2
                            dfGraph$FromNodeDone[dfGraph$COMID==comid] <- TRUE
                            # return(dfGraph)
                            getDSrecursive(dfGraph, to.node, comid)
                        } # End check if to node is done already
                    } # End check if comid (edge) is done already
                } # End for loop over multiple edges from same node
            } # No more edges, need to stop
        } # Aggregate length within 20x connectivity distance
    } # End function

    getUSrecursive <- function(dfGraph,from.node,TargetCOMID,up.down="Up",startAggLen=0) {
        comid <- TargetCOMID
        aggReachLen <- dfGraph$AggLengthKM[dfGraph$COMID==comid] + startAggLen
        dfGraph$UpDown[dfGraph$COMID==comid] <- up.down
        dfGraph$FromNodeDone[dfGraph$COMID==comid] <- TRUE
        temp <- dfGraph[dfGraph$COMID==comid,]
        write.table(temp, fn.output, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        dfUSnode <- dplyr::filter(dfGraph, ToNode==from.node)
        
        # Explore edges coming from from.node (now to.node)
        if (aggReachLen<=(2.5*cxndist_km)) {
            if (nrow(dfUSnode)>0) {
                for (e in 1:nrow(dfUSnode)) {
                    # e=1
                    comid <- dfUSnode$COMID[e]
                    if (dfGraph$EdgeDone[dfGraph$COMID==comid]==FALSE) {
                        from.node <- dfGraph$FromNode[dfGraph$COMID==comid]
                        if (dfGraph$FromNodeDone[dfGraph$COMID==comid]==FALSE) {
                            dfGraph$EdgeDone[dfGraph$COMID==comid] <- TRUE
                            lenReach2 <- dfGraph$LENGTHKM[dfGraph$COMID==comid]
                            dfGraph$AggLengthKM[dfGraph$COMID==comid] <- 
                                aggReachLen + lenReach2
                            currentAggLen <- dfGraph$AggLengthKM[dfGraph$COMID==comid]
                            dfGraph$ToNodeDone[dfGraph$COMID==comid] <- TRUE
                            getUSrecursive(dfGraph, from.node, comid
                                           , up.down=up.down, startAggLen=0)
                        } else if (up.down == "Branch to downstream mainstem") {
                            dfGraph$EdgeDone[dfGraph$COMID==comid] <- TRUE
                            lenReach2 <- dfGraph$LENGTHKM[dfGraph$COMID==comid]
                            dfGraph$AggLengthKM[dfGraph$COMID==comid] <- 
                                aggReachLen + lenReach2
                            currentAggLen <- dfGraph$AggLengthKM[dfGraph$COMID==comid]
                            dfGraph$ToNodeDone[dfGraph$COMID==comid] <- TRUE
                            getUSrecursive(dfGraph, from.node, comid
                                           , up.down=up.down, startAggLen=0)
                        } # End check if to node is done already or needs to be done again (new edge from downstream)
                    } # End check if comid (edge) is done already
                } # End for loop over multiple edges from same node
            } # No more edges, need to stop
        } # Aggregate length within 20x connectivity distance
    } # End function
    
    # Main -- get downstream connections ####
    dfReaches <- dfNetwork[,c("COMID", "FTYPE", "FromNode", "ToNode", "LENGTHKM"
                              , "StartFlag")]
    dfReaches$FromNodeDone <- FALSE
    dfReaches$EdgeDone <- FALSE
    dfReaches$ToNodeDone <- FALSE
    dfReaches$AggLengthKM <- 0
    dfReaches$UpDown <- NA
    to.node <- as.numeric(dfReaches$ToNode[dfReaches$COMID==TargetCOMID])
    dfWrite <- dfReaches[dfReaches$COMID==TargetCOMID,]
    dfWrite <- dfWrite[-1,]
    write.table(dfWrite, fn.output, append = FALSE, col.names = TRUE
                , row.names = FALSE, sep = "\t")
    getDSrecursive(dfReaches, to.node, TargetCOMID)

    # Main -- get upstream connections ####
    from.node <- as.numeric(dfReaches$FromNode[dfReaches$COMID==TargetCOMID])
    getUSrecursive(dfReaches,from.node,TargetCOMID,up.down="Up",startAggLen=0)
    # getUSrecursive(dfReaches, from.node, TargetCOMID, startAggLen=0)
    
    # This represents everything upstream of the target COMID and everything
    # along the main stem downstream. Still need branches feeding the main stem
    # downstream of the target COMID
    dfFirstPass <- read.delim(fn.output, header = TRUE, sep = "\t")
    dfFirstPass$FromNodeDone <- ifelse(dfFirstPass$COMID==TargetCOMID
                                  , TRUE, dfFirstPass$FromNodeDone)
    dfFirstPass$ToNodeDone <- ifelse(dfFirstPass$COMID==TargetCOMID
                                , TRUE, dfFirstPass$ToNodeDone)
    dfFirstPass$EdgeDone <- ifelse(dfFirstPass$COMID==TargetCOMID
                              , TRUE, dfFirstPass$EdgeDone)
    dfFirstPass$UpDown <- ifelse(dfFirstPass$COMID==TargetCOMID
                            , "Origin", as.character(dfFirstPass$UpDown))
    dfFirstPass <- unique(dfFirstPass)
    dfFirstPass <- dfFirstPass[dfFirstPass$FTYPE != "Coastline",]
    dfFirstPass <- dfFirstPass[floor(dfFirstPass$AggLengthKM) <= cxndist_km,]
    dfFirstPass <- dfFirstPass[dfFirstPass$AggLengthKM <= (1.02*cxndist_km),]
    file.remove(fn.output)
    
    # Main -- get feeds to downstream branches ####
    # Scroll through dfCxns and for all down, check to node to see if any from nodes
    # exist that are not marked done. If true, run getUSrecursive() on them.
    dfDSMainStem <- dplyr::filter(dfFirstPass, UpDown!="Up")
    doneCOMIDs <- unique(as.numeric(dfDSMainStem$COMID))
    dfReachesNotDone <- dfReaches[!(dfReaches$COMID %in% doneCOMIDs),]
    
    write.table(dfFirstPass, fn.output, append = FALSE, col.names = TRUE
                , row.names = FALSE, sep = "\t")
    
    # Traverse downstream mainstem to look for feeder branches
    for (r in 1:nrow(dfDSMainStem)) {
        # Get to.node, COMID that was done, and AggLen to that point
        to.node <- as.numeric(dfDSMainStem$ToNode[r])
        doneCOMID <- as.numeric(dfDSMainStem$COMID[r])
        mainAggLen <- as.numeric(dfDSMainStem$AggLengthKM[r])
        numToNode <- nrow(dfReaches[dfReaches$ToNode %in% to.node,])
        
        # Determine if there's a branch coming into the to node
        if (numToNode>1) { # If only one, no branch; if >1, branches exist
            
            # Iterate over the branches upstream to get lengths
            dfUSBranches <- dfReaches[dfReaches$ToNode %in% to.node,]
            dfUSBranches <- dplyr::filter(dfUSBranches, !(COMID %in% doneCOMIDs))
            
            if (nrow(dfUSBranches)>0) {
                
                for (branch in 1:nrow(dfUSBranches)) {
                    # Get node that wasn't done and send to getUSrecursive()
                    comid <- as.numeric(dfUSBranches$COMID[branch])
                    ftype <- as.character(dfUSBranches$FTYPE[branch])
                    
                    if (ftype=="Coastline") { next }
                    # getUSrecursive(dfReaches, from.node=to.node, TargetCOMID=comid
                    #                , startAggLen=mainAggLen)
                    getUSrecursive(dfReachesNotDone
                                   ,from.node=to.node
                                   ,TargetCOMID=comid
                                   ,up.down="Branch to downstream mainstem"
                                   ,startAggLen=mainAggLen)
                } # End this node's upstream branches
            
            } else { 
                
                # no upstream branches from this ToNode
            }

        } # End this comid
        
    } # End all branches to downstream mainstem
    # rm(dfWrite)
    
    dfCxns <- read.delim(fn.output, header = TRUE, sep = "\t"
                         , stringsAsFactors = FALSE)
    dfCxns <- dfCxns[dfCxns$EdgeDone==TRUE,]
    dfCxns <- dfCxns[dfCxns$FTYPE != "Coastline",]
    dfCxns <- dfCxns[floor(dfCxns$AggLengthKM) <= cxndist_km,]
    dfCxns <- dfCxns[dfCxns$AggLengthKM <= (1.02*cxndist_km),]
    dfCxns <- dplyr::select(dfCxns, -FromNodeDone, -ToNodeDone, -EdgeDone)
    file.remove(fn.output)
    
    totalUp <- dfCxns %>%
        dplyr::select(UpDown, AggLengthKM) %>%
        dplyr::filter(UpDown=="Up") %>%
        dplyr::summarize(TotalLength = sum(AggLengthKM), .groups="drop_last")
    totalUp <- as.numeric(totalUp)
    
    totalDown <- dfCxns %>%
        dplyr::select(UpDown, AggLengthKM) %>%
        dplyr::filter(UpDown!="Up") %>%
        dplyr::summarize(TotalLength = sum(AggLengthKM), .groups="drop_last")
    totalDown <- as.numeric(totalDown)
    
    dfCxns <- dplyr::mutate(dfCxns
                            , TotalLength = ifelse(UpDown=="Up",totalUp,totalDown)
                            , FractionLength = (LENGTHKM) / TotalLength)
    
    # fn.out <- paste0(TargetCOMID,"_Cxns_",format.Date(Sys.Date(),"%Y%m%d"),".tab")
    # write.table(dfCxns, file.path(results_dir,fn.out), append = FALSE
    #             , col.names = TRUE, row.names = FALSE, sep = "\t")
    
    end.time <- Sys.time()
    elapsed.time <- end.time - start.time
    message(paste0("Elapsed time = ", format.difftime(elapsed.time)))
    
    return(dfCxns)

    # rm(list = ls())
    
} # FUNCTION END