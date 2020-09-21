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

addMunicipality <- function(dfSites, dfAllScores, fn_reachMuni, fn_resultsMuni) {
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`
    
    if (boo_DEBUG==TRUE) {
        dfSites <- dfSites
        dfAllScores <- dfAllScores
        fn_reachMuni <- fn_reachMuni
        fn_resultsMuni <- fn_resultsMuni
    }
    
    base_path <- file.path(data_dir,"SMCReaches")
    
    df_reachMuni <- read.delim(fn_reachMuni, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    
    df_reach <- merge(df_reachMuni, dfAllScores, by.x = "COMID", by.y = "COMID", all.x = TRUE)
    df_reach <- unique(df_reach)
    
    df_reach <- merge(dfSites[,c("COMID","StationID_Master","FinalLatitude","FinalLongitude")]
                      , df_reach, by.x = "COMID", by.y = "COMID", all.y = TRUE)
    df_reach <- df_reach %>%
        dplyr::filter(!is.na(CSCIpred_qt50) | !is.na(CSCIobs))
    
    write.table(df_reach,fn_resultsMuni, append = FALSE, sep = "\t"
                , col.names = TRUE, row.names = FALSE)
    
    return(df_reach)
    
}

