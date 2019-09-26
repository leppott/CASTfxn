#' @title Get site background graphics
#' 
#' @description Graph disturbance-related variables for the target site obtained
#' from StreamCat. Copies site photos from a general photo library to target site
#' results folder.
#' 
#' @details Generates faceted barplots showing disturbance variables for the 
#' target site, including land cover data, pollution data, and physical 
#' modifications (e.g., development, roads, dams, mines) of the catchment or 
#' watershed. Also, copies site photos (name must contain the target site ID)
#' to the target site results folder.
#' 
#' Note that this function does not actually check the file extensions, so any 
#' files containing the target site ID will be copied to the results folder.
#' 
#' Uses the libraries dplyr, tidyr, ggplot2, and ggthemes.
#' 
#' @param TargetSiteID Site ID
#' @param TargetCOMID NHDPlus version 2 COMID for the reach on which the site lies.
#' @param fn_bkgdata Filename for the file containing the StreamCat disturbance data.
#' @param fn_bkginfo Filename for the file containing the metadata for the data.
#' @param dir_photo Directory containing the site photos, if available. 
#' Default = "file.path(getwd(),"Data","Photos")"
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")"
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo".
#' 
#' @return One or more jpgs in SiteID/TemporalSequence/Biocomm subfolder of the 
#'        "Results" folder of working directory. No scores are currently generated.
#' 
#' @keywords internal
#' 
#' @export
getSiteBackground <- function(TargetSiteID
                              , TargetCOMID
                              , fn_bkgdata
                              , fn_bkginfo
                              , dir_photo = file.path(getwd(),"Data","Photos")
                              , dir_results = file.path(getwd(), "Results")
                              , dir_sub = "SiteInfo") {
    
    # QC
    # TargetSiteID = TargetSiteID
    # targetCOMID = list.SiteSummary$COMID
    # fn_bkgdata = fn_bkgdata
    # fn_bkginfo = fn_bkginfo
    # dir_photo = file.path(wd,dataDir,"Photos")
    # dir_results = file.path(wd, "Results")
    # dir_sub = "SiteInfo"
    
    plot_w = 6
    plot_h = 4
    ppi = 300
    
    # define pipe
    `%>%` <- dplyr::`%>%`
    
    # Check for presence of SiteInfo directory. If not present, create
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID))
           , FALSE)
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID, dir_sub))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID, dir_sub))
           , FALSE)
    
    path <- file.path(dir_results, TargetSiteID, dir_sub)
    fn.bkg <- paste0(TargetSiteID,".bkgdinfo.tab")
    
    # Check for presence of Photos in data directory. If not present, skip.
    if (dir.exists(dir_photo)==TRUE) {
        photofiles <- list.files(dir_photo)
        have.photos <- FALSE
        for (l in 1:length(photofiles)) {
            photoname <- photofiles[l]
            if (str_detect(photoname, eval(TargetSiteID))==TRUE) {
                file.copy(file.path(dir_photo,photoname)
                          , file.path(dir_results,TargetSiteID,dir_sub,photoname))
                print(paste0(photoname, " copied."))
                flush.console()
                have.photos <- TRUE
            }
            if (!have.photos) {
                print(paste0("No site photos are available for ", TargetSiteID))
                flush.console()
            }
        }
    } else { 
        print("No photos are available.")
        flush.console()
    }

    # Get background data from fn_bkgdata; use COMID to select single row
    df.bkgdata <- read.table(fn_bkgdata, header = TRUE, sep = "\t"
                             , na.strings = c("","NA"))
    df.bkgdata <- dplyr::filter(df.bkgdata, COMID == TargetCOMID)
    df.bkgdata2 <- tidyr::gather(df.bkgdata, -COMID, key = "ColName"
                                 , value = "val")
    df.bkgdata2 <- dplyr::select(df.bkgdata2, -COMID)
    
    write.table(df.bkgdata, file.path(path,fn.bkg), append = FALSE
                , sep = "\t", col.names = TRUE, row.names = FALSE)
    
    # Get metadata from fn_bkginfo
    df.bkginfo <- read.table(fn_bkginfo, header = TRUE, sep = "\t"
                             , na.strings = c("", "NA")
                             , stringsAsFactors = FALSE)
    df.bkg2plot <- dplyr::left_join(df.bkginfo, df.bkgdata2)
    
    rm(df.bkgdata, df.bkgdata2, df.bkginfo)

    # Determine appropriate graphics
    # Bar charts, faceted with catchment on left, watershed on right
    cat.sub <- unique(df.bkg2plot[,c("Category","Subcategory","Units","AbbrFN")])
    
    plot.path <- file.path(dir_results,TargetSiteID,dir_sub)
    
    for (i in 1:nrow(cat.sub)) {
        # pull out temp data set to plot
        df.temp <- df.bkg2plot %>%
            dplyr::filter(Category == cat.sub$Category[i]
                   , Subcategory == cat.sub$Subcategory[i])
        
        xlab <- paste0(cat.sub$Category[i],": ",cat.sub$Subcategory[i]
                       ,", ",cat.sub$Units[i])
        fn.plot <- file.path(plot.path,paste0(cat.sub[i,4],".png"))
        p.title <- paste("Potential anthropogenic alterations")
        p.subtitle <- TargetSiteID
        numcols <- length(unique(df.temp$Scale))/2
        
        print(xlab)
        flush.console()
        
        if (is.na(df.temp$StudyYear)) {
            p.bkg <- ggplot2::ggplot(df.temp, ggplot2::aes(x = ShortName
                                            , y = signif(val, digits = 2))) +
                ggplot2::geom_bar(stat = "identity", width = 0.5, fill = "firebrick4") +
                ggplot2::geom_text(ggplot2::aes(label = signif(val, digits = 2)
                              , vjust=-0.2), color = "black", size=3) +
                ggplot2::ylim(0, max(df.temp$val)*1.1) +
                ggplot2::facet_wrap(Scale~.)
            p.bkg <- p.bkg + ggthemes::theme_stata() + 
                ggplot2::theme(legend.position = "none") +
                ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)) +
                ggplot2::labs(title = p.title, subtitle = p.subtitle
                     , x = xlab, y = "Value")
            p.bkg <- p.bkg + 
                ggplot2::theme(axis.text.x = ggplot2::element_text(size=7
                                                    ,angle=45,hjust=1)
                      , axis.text.y = ggplot2::element_text(size=8)
                      , axis.title.x = ggplot2::element_text(size=9, face="bold")
                      , axis.title.y = ggplot2::element_text(size=9, face="bold")
                      , plot.title = ggplot2::element_text(size=10, face="bold")
                      , plot.subtitle = ggplot2::element_text(size=9, face="bold"))
            ggplot2::ggsave(fn.plot, p.bkg, dpi=ppi)
            
        } else {
            
            p.bkg <- ggplot2::ggplot(df.temp, ggplot2::aes(x = ShortName
                                                , y = signif(val, digits = 2)
                                                , group = StudyYear)) +
                ggplot2::geom_bar(position="dodge", stat = "identity", width = 0.5
                         , fill = "firebrick4") +
                ggplot2::geom_text(ggplot2::aes(label = signif(val, digits=2)
                              , vjust=-0.2), color = "black", size=3) +
                ggplot2::ylim(0, max(df.temp$val)*1.1) +
                ggplot2::facet_grid(Scale~StudyYear, margins = FALSE)
            p.bkg <- p.bkg + ggthemes::theme_stata() + 
                ggplot2::theme(legend.position = "none") +
                ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)
                      , strip.text.y = ggplot2::element_text(size = 8)) +
                ggplot2::labs(title = p.title, subtitle = p.subtitle
                     , x = xlab, y = "Value")
            p.bkg <- p.bkg +
                ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7
                                                , angle = 45, hjust = 1)
                    , axis.text.y = ggplot2::element_text(size=8)
                    , axis.title.x = ggplot2::element_text(size=9, face="bold")
                    , axis.title.y = ggplot2::element_text(size=9, face="bold")
                    , plot.title = ggplot2::element_text(size=10, face="bold")
                    , plot.subtitle = ggplot2::element_text(size=9, face="bold"))
            ggplot2::ggsave(fn.plot, p.bkg, dpi=ppi)

        }

    }

}
    