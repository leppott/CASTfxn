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

drawAllScoresPlot <- function(TargetReach
                              , allScores
                              , dfSiteInfo
                              , dfStressors
                              , dfStressorInfo
                              , results_dir) { # FUNCTION.START
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`
    
    if (boo_DEBUG==TRUE) {
        TargetReach <- TargetReach
        allScores <- dfAllScoresSummary
        dfSiteInfo <- dfAllSites
        dfStressors <- dfWoE
        dfStressorInfo <- dfStressInfo
        results_dir <- results_dir
    }
    
    drawPlot <- FALSE
    colorpal <- viridis::viridis(8)
    colorpal2 <- viridis::plasma(8)

    # Create results dir, if it doesn't exist
    ifelse(!dir.exists(file.path(results_dir))==TRUE
           , dir.create(file.path(results_dir))
           , FALSE)
    ifelse(!dir.exists(file.path(results_dir, TargetReach))==TRUE
           , dir.create(file.path(results_dir, TargetReach))
           , FALSE)

    # Get filename for saving
    myTime <- lubridate::now()
    myDate <- stringr::str_replace_all(stringr::str_extract(myTime,"\\d{4}-\\d{2}-\\d{2}")
                                       , "-", "")
    myTime <- stringr::str_replace_all(stringr::str_extract(myTime,"\\d{2}:\\d{2}:\\d{2}")
                                       , ":", "")
    fn_TargetScoreGraph <- file.path(results_dir, TargetReach
                                     ,paste0(TargetReach,"_AllScores_", myDate
                                             ,"_",myTime,".png"))
    rm(myDate, myTime)
    
    if (is.null(dfStressors)){
        dfReachInfo <- dfSiteInfo %>%
            dplyr::filter(COMID==TargetReach) %>%
            dplyr::select(COMID, StationID_Master, FinalLatitude, FinalLongitude) %>%
            dplyr::mutate(Stressor = "None", Label = "None")
    } else if (nrow(dfStressors[dfStressors$COMID==TargetReach,])<1) {
        dfReachInfo <- dfSiteInfo %>%
            dplyr::filter(COMID==TargetReach) %>%
            dplyr::select(COMID, StationID_Master, FinalLatitude, FinalLongitude) %>%
            dplyr::mutate(Stressor = "None", Label = "None")
    } else {
        # Get stressor and site info for target reach
        dfStressors <- merge(dfStressors, dfSiteInfo[dfSiteInfo$COMID==TargetReach,]
                             , by.x=c("StationID_Master", "FinalLatitude", "FinalLongitude")
                             , by.y=c("StationID_Master", "FinalLatitude", "FinalLongitude")
                             , all.y=TRUE)
        dfStressors <- merge(dfStressors, dfStressorInfo[,c("Stressor", "Label")]
                             , by.x="Stressor", by.y="Stressor", all.x=TRUE)
        dfReachInfo <- dplyr::filter(dfStressors, COMID==TargetReach) %>%
            dplyr::select(COMID, StationID_Master, FinalLatitude
                                     , FinalLongitude, Stressor, Label)
    }
    
    # Get scores for specified COMID
    dfReachScores <- allScores[allScores$COMID==TargetReach,]
    if (nrow(dfReachScores)>1) {
        msgScoresMany <- paste0("More than one score is available for ", TargetReach, ".")
    } else if (nrow(dfReachScores)==0) {
        msgScoresNone <- paste0("No scores are available for ", TargetReach, ".")
    } else {
        drawPlot <- TRUE
    }
    
    # Get bioassessment site info from Sites file
    dfReachSites <- unique(dplyr::select(dfReachInfo, COMID, StationID_Master
                                         , FinalLatitude, FinalLongitude))
    if (nrow(dfReachSites)==0) {
        siteList <- "No bioassessment sites"
    } else { 
        dfReachSites <- dfReachSites %>%
            dplyr::mutate(SiteInfo = paste0(StationID_Master, " ("
                                           , format(FinalLatitude,digits=5,nsmall=5)
                                           , ", "
                                           , format(FinalLongitude,digits=5,nsmall=5)
                                           , ")"))
        siteList <- as.character(dfReachSites$SiteInfo[!is.na(dfReachSites$SiteInfo)])
    }
    
    # Get any observed stressors
    dfReachStressors <- unique(dplyr::select(dfReachInfo, Label))
    if (all(dfReachStressors$Label=="None") | (nrow(dfReachStressors)==0)) {
        strlist <- "No stressors available"
    } else { 
        strlist <- as.character(dfReachStressors$Label[!is.na(dfReachStressors$Label)])
    }
    
    if (drawPlot) { # Draw the scores plot with whatever data are available
        
        rank <- unique(as.character(dfReachScores$RankByIndexType))
        indexType <- unique(as.character(dfReachScores$IndexType))
        indexTypeLabel <- ifelse(indexType == "Protect"
                                 , "Protection Index"
                                 , "Restoration Index")

        # Ensure all data required are in dataframe
        dfScores2Plot <- dfReachScores %>%
            dplyr::select(-CSCI, -BCGTier, -BioType, -IndexType, -RankByIndexType) %>%
            tidyr::gather(key="ScoreType", value="value", -COMID) %>%
            dplyr::mutate(Type=ifelse(grepl("Index$",ScoreType)
                                      ,"index"
                                      , ifelse(grepl("Subindex$",ScoreType)
                                               , "subindex"
                                               , "indicator"))
                          , Category=ifelse(Type=="index"
                                            , "index"
                                            , ifelse(grepl("^(Pot)|(Bio)|(Stress)",ScoreType)
                                                     ,"Potential"
                                                     , ifelse(grepl("^(Fire)|(PlannedDev)|(Threat)"
                                                         , ScoreType)
                                                         ,"Threat"
                                                         , "Opportunity")))
                          , GoodBad=ifelse(Category=="Threat"
                                           , "bad"
                                           ,ifelse(Category=="Opportunity"
                                                   , "good"
                                                   , ifelse(grepl("Bio", ScoreType)
                                                            , "good"
                                                            , ifelse(grepl("Stress", ScoreType)
                                                                   , "bad"
                                                                   , "good")))))
        
        # Create df of axis labels to merge
        df_axislabels <- data.frame("ScoreType" = dfScores2Plot$ScoreType,
                                   "axisLabels" = c("RPP Index", "Potential"
                                                    , "Bio Cond", "Bio Cxn", "Stressor"
                                                    , "Stressor Cxn", "Threat"
                                                    , "Fire Hazard", "Planned Dev"
                                                    , "Opportunity", "Recreation"
                                                    , "MSCP", "NASVI", "User spec"))
        dfScores2Plot <- merge(dfScores2Plot, df_axislabels, by.x="ScoreType"
                               , by.y="ScoreType")
        
        goodCol=colorpal[5]
        # badCol=colorpal[2]
        badCol=colorpal2[5]
        
        if (indexType=="Protect") {
            indexCol=colorpal[1]
        } else {
            indexCol=colorpal[3]
        }
        dfScores2Plot$ColorBar=ifelse(dfScores2Plot$Category=="index"
                                      ,indexCol
                                      ,ifelse(dfScores2Plot$GoodBad=="good"
                                              , goodCol
                                              , badCol))
        dfScores2Plot$Val2Plot=ifelse(is.na(dfScores2Plot$value)
                                            , 0, dfScores2Plot$value)
        # Change score type to an ordered factor
        dfScores2Plot$axisLabels <- factor(dfScores2Plot$axisLabels
                            , levels = rev(c("RPP Index", "Potential"
                                         , "Threat", "Opportunity"
                                         , "Bio Cond", "Bio Cxn"
                                         , "Stressor", "Stressor Cxn"
                                         , "Fire Hazard", "Planned Dev"
                                         , "Recreation", "MSCP"
                                         , "NASVI", "User spec")))

        # Create overall index bar plot (fix y-ticks to not show)
        indexScore <- dfScores2Plot[dfScores2Plot$Type=="index",]
        p_RPPIndex <- ggplot2::ggplot(indexScore, aes(x=ScoreType, y=Val2Plot)) +
            ggplot2::geom_bar(stat="identity", fill=indexCol, width=0.75
                              , alpha=0.8) + 
            ggplot2::geom_hline(yintercept=0, color="dark gray") +
            ggplot2::geom_text(aes(label=ifelse(is.na(value), "NA"
                                , format(Val2Plot, digits=2, nsmall=2)), y=0)
                               , hjust=1, nudge_x=-0.05, vjust=0, nudge_y=-0.01
                               , color="black", size=3) +
            ggplot2::coord_flip() +
            ggplot2::ylim(-0.15,1) + ggplot2::theme_minimal() +
            ggplot2::labs(title = indexTypeLabel) +
            ggplot2::theme(title=element_text(size=12)
                           , axis.text.x=element_text(size=8)
                           , axis.text.y=element_blank()
                           , axis.title=element_blank()
                           , axis.ticks.y=element_blank())
        
        # Create subindices bar plot
        subindexScores <- dfScores2Plot[dfScores2Plot$Type=="subindex",]
        p_subindex <- ggplot2::ggplot(subindexScores, aes(x=axisLabels, y=Val2Plot)) +
            ggplot2::geom_bar(stat="identity", fill=subindexScores$ColorBar
                              , width=0.4) + 
            ggplot2::geom_hline(yintercept=0, color="dark gray") +
            ggplot2::geom_text(aes(label=ifelse(is.na(value), "NA"
                                , format(Val2Plot, digits=2, nsmall=2)), y=0)
                               , hjust=1, nudge_x=-0.05, vjust=0, nudge_y=-0.01
                               , color="black", size=3) +
            ggplot2::annotate(geom="text", x=3.3, y=0.05, label="Potential"
                              , size=4, hjust=0) +
            ggplot2::annotate(geom="text", x=2.3, y=0.05, label="Threat"
                              , size=4, hjust=0) +
            ggplot2::annotate(geom="text", x=1.3, y=0.05, label="Opportunity"
                              , size=4, hjust=0) +
            ggplot2::coord_flip() +
            ggplot2::ylim(-0.15,1) + 
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.y=ggplot2::element_blank()
                           , axis.text.x=element_text(size=8)
                           , axis.title=ggplot2::element_blank())
        
        # Create potential indicator bar plot
        potIndScores <- dfScores2Plot[dfScores2Plot$Type=="indicator" & 
                                          dfScores2Plot$Category=="Potential",]
        p_potInds <- ggplot2::ggplot(potIndScores, aes(x=axisLabels, y=Val2Plot)) +
            ggplot2::geom_bar(stat="identity", fill=potIndScores$ColorBar
                              , width=0.5) + 
            ggplot2::geom_hline(yintercept=0, color="dark gray") +
            ggplot2::geom_text(aes(label=ifelse(is.na(value), "NA"
                                , format(Val2Plot, digits=2, nsmall=2)), y=0)
                               , hjust=1, nudge_x=-0.05, vjust=0, nudge_y=-0.01
                               , color="black", size=3) +
            ggplot2::annotate(geom="text", x=4.5, y=0.05, label="Biological Condition"
                              , size=3, hjust=0) +
            ggplot2::annotate(geom="text", x=3.5, y=0.05, label="Biological Connectivity"
                              , size=3, hjust=0) +
            ggplot2::annotate(geom="text", x=2.5, y=0.05, label="Stressor"
                              , size=3, hjust=0) +
            ggplot2::annotate(geom="text", x=1.5, y=0.05, label="Stressor Connectivity"
                              , size=3, hjust=0) +
            ggplot2::coord_flip() +
            ggplot2::ylim(-0.15,1) + 
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x=ggplot2::element_blank()
                           , axis.text.y=ggplot2::element_blank()
                           , axis.title=ggplot2::element_blank())

        # Create threat indicator bar plot
        thrIndScores <- dfScores2Plot[dfScores2Plot$Type=="indicator" & 
                                          dfScores2Plot$Category=="Threat",]
        p_thrInds <- ggplot2::ggplot(thrIndScores, aes(x=axisLabels, y=Val2Plot)) +
            ggplot2::geom_bar(stat="identity", fill=thrIndScores$ColorBar
                              , width=0.5) + 
            ggplot2::geom_hline(yintercept=0, color="dark gray") +
            ggplot2::geom_text(aes(label=ifelse(is.na(value), "NA"
                                , format(Val2Plot, digits=2, nsmall=2)), y=0)
                               , hjust=1, nudge_x=-0.05, vjust=0, nudge_y=-0.01
                               , color="black", size=3) +
            ggplot2::annotate(geom="text", x=2.5, y=0.05, label="Fire Hazard"
                              , size=3, hjust=0) +
            ggplot2::annotate(geom="text", x=1.5, y=0.05, label="Planned Dev"
                              , size=3, hjust=0) +
            ggplot2::coord_flip() +
            ggplot2::ylim(-0.15,1) + 
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x=ggplot2::element_blank()
                           , axis.text.y=ggplot2::element_blank()
                           , axis.title=ggplot2::element_blank())
        
        # Create potential indicator bar plot
        oppIndScores <- dfScores2Plot[dfScores2Plot$Type=="indicator" & 
                                          dfScores2Plot$Category=="Opportunity",]
        p_oppInds <- ggplot2::ggplot(oppIndScores, aes(x=axisLabels, y=Val2Plot)) +
            ggplot2::geom_bar(stat="identity", fill=oppIndScores$ColorBar
                              , width=0.5) + 
            ggplot2::geom_hline(yintercept=0, color="dark gray") +
            ggplot2::geom_text(aes(label=ifelse(is.na(value), "NA"
                                , format(Val2Plot, digits=2, nsmall=2)), y=0)
                               , hjust=1, nudge_x=-0.05, vjust=0, nudge_y=-0.01
                               , color="black", size=3) +
            ggplot2::annotate(geom="text", x=4.5, y=0.05, label="Recreation"
                              , size=3, hjust=0) +
            ggplot2::annotate(geom="text", x=3.5, y=0.05, label="MSCP"
                              , size=3, hjust=0) +
            ggplot2::annotate(geom="text", x=2.5, y=0.05, label="NASVI"
                              , size=3, hjust=0) +
            ggplot2::annotate(geom="text", x=1.5, y=0.05, label="User-applied"
                              , size=3, hjust=0) +
            ggplot2::coord_flip() +
            ggplot2::ylim(-0.15,1) + 
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x=ggplot2::element_text(size=8)
                           , axis.text.y=ggplot2::element_blank()
                           , axis.title=ggplot2::element_blank())
        
        # Create text grobs
        reachGrob <- grid::textGrob(paste0("Reach: ", TargetReach), x=0.05, just="left"
                                    , gp=grid::gpar(fontsize=14, fontface="bold"))
        siteGrob <- grid::textGrob(paste("Site(s):", stringr::str_c(siteList, collapse="\n")
                                         , sep="\n"), x=0.05, just="left"
                                   , gp=grid::gpar(fontsize=8))
        stressorGrob <- grid::textGrob(paste("Stressor(s):"
                                            , stringr::str_wrap(stringr::str_c(strlist
                                            , collapse="; "), width = 60),sep="\n")
                                       , x=0.05, just="left", gp=grid::gpar(fontsize=7))
        rankGrob <- grid::textGrob(paste("Rank:", rank), x=0.05, just="left"
                                   , gp=grid::gpar(fontsize=10, fontface="bold"))
        captionGrob <- grid::textGrob(paste("Green bars represent direct relationships (more is better)."
                                            , "Red bars represent inverse relationships (less is better)."
                                            , sep= " "), x=0.01, just="left"
                                      , gp=grid::gpar(fontsize=8, fontface="italic"))

        p_blank <- ggplot2::ggplot() +
            ggplot2::geom_blank(ggplot2::aes(1,1)) +
            ggplot2::theme_void()
        
        p_text <- gridExtra::grid.arrange(reachGrob, siteGrob, stressorGrob
                                          , rankGrob, ncol=1, nrow=4
                                          , heights = c(0.2, 0.8, 3.8, 0.2))
        
        p_final <- gridExtra::grid.arrange(p_text, p_RPPIndex, p_subindex
                                           , p_potInds, p_thrInds, p_oppInds
                                           , captionGrob, ncol=4, nrow=4
                                           , layout_matrix = rbind(c(1,1,3,4)
                                                                   , c(1,1,3,5)
                                                                   , c(2,2,3,6)
                                                                   , c(7,7,7,7))
                                           , widths = c(1.5,1.5,3,3)
                                           , heights = c(2.25,2.25,2.25,0.25))
        
        ggplot2::ggsave(fn_TargetScoreGraph, p_final, width=9, height=7
                        , units="in", dpi=600)

    }

}

