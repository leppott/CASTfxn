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

# Create individual box plots using one grouping column and an option x-axis title
drawBarPlot <- function(df.data, fn.plotpath, plotType = "bar", groupCol, valCol
                        , plot_W=4, plot_H=4, ppi=300, str_title, str_subtitle
                        , str_ylab, str_xlab, str_caption, title_size=10
                        , subtitle_size=8, axistextx_size=8, axistexty_size=8
                        , caption_size=8) {
    
    boo_DEBUG = FALSE
    `%>%` <- dplyr::`%>%`
    
    if(boo_DEBUG==TRUE) {
        df.data = dfAllScores2Plot
        fn.plotpath = fn_scoreplots
        numReaches = length(unique(as.vector(dfAllScores2Plot$COMID)))
        plotType = "histogram"
        groupCol="ScoreType"
        valCol="Score"
        plot_W=4
        plot_H=4
        ppi=300
        str_title = "Histogram of subindex scores"
        str_subtitle = "SMC Region"
        str_xlab = "Normalized Score"
        str_ylab = paste0("Number of reaches (total scored: "
                            , numReaches, ")")
        str_caption = NULL
        title_size=10
        subtitle_size=8
        axistextx_size=8
        axistexty_size=8
        caption_size=8
    }
    
    if (plotType=="bar") {
        if (grepl("Date",groupCol)==TRUE) {
            
            data <- unique(df.data)
            data$Year <- lubridate::year(data[,groupCol])
            dfplot <- data %>%
                dplyr::select(Year,eval(valCol)) %>%
                dplyr::group_by(Year) %>%
                dplyr::summarise(NumSamps = dplyr::n(), .groups="drop_last")
            
            NAcount <- dfplot$NumSamps[is.na(dfplot$Year)]
            
            dfplot <- data[!is.na(data$Year),]
            dfplot <- dfplot[dfplot$Year>=1990,] # 2000 should be start of entire dataset
            
            p <- ggplot2::ggplot(dfplot, ggplot2::aes(x=Year)) +
                ggplot2::geom_bar() +
                ggplot2::labs(title = str_title, subtitle=str_subtitle
                              , y=str_ylab, x=str_xlab, caption=str_caption) +
                ggplot2::theme_bw() + 
                ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5,size=title_size)
                               , plot.subtitle=ggplot2::element_text(hjust=0.5,size=subtitle_size)
                               , axis.text.x = ggplot2::element_text(size=axistextx_size)
                               , axis.text.y = ggplot2::element_text(size=axistexty_size)
                               , plot.caption = ggplot2::element_text(size=caption_size))
        } else {
            
            data <- unique(df.data)
            data <- data[!is.na(data[,valCol]),]
            # data[,groupCol] <- as.factor(data[,groupCol])
            dfplot <- data %>%
                dplyr::select(eval(groupCol),eval(valCol)) #%>%
                dplyr::group_by(groupCol) %>%
                dplyr::summarise(NumSamps = dplyr::n(), .groups="drop_last")
            
            
            p <- ggplot2::ggplot(dfplot, ggplot2::aes(x=valCol, group_by(groupCol))) +
                ggplot2::geom_bar() +
                ggplot2::labs(title = str_title, subtitle=str_subtitle
                              , y=str_ylab, x=str_xlab, caption=str_caption) +
                ggplot2::theme_bw() + 
                ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5,size=title_size)
                               , plot.subtitle=ggplot2::element_text(hjust=0.5,size=subtitle_size)
                               , axis.text.x = ggplot2::element_text(size=axistextx_size)
                               , axis.text.y = ggplot2::element_text(size=axistexty_size)
                               , plot.caption = ggplot2::element_text(size=caption_size))
            
        }
    
    } else if (plotType=="histogram") {
        
        data <- unique(df.data)
        dfplot <- data[!is.na(data[,valCol]),]
        dfplot[,groupCol] <- as.factor(dfplot[,groupCol])
        
        # valCol and groupCol are hard-coded. Need to change.
        p <- ggplot2::ggplot(data=dfplot, ggplot2::aes(x=Score)) +
            ggplot2::geom_histogram(position="identity", binwidth=0.015) +
            ggplot2::facet_grid(ScoreType ~.) +
            ggplot2::xlim(0,1) +
            ggplot2::labs(title = str_title, subtitle=str_subtitle
                          , y=str_ylab, x=str_xlab, caption=str_caption) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position="None") +
            ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5,size=title_size)
                           , plot.subtitle=ggplot2::element_text(hjust=0.5,size=subtitle_size)
                           , axis.text.x = ggplot2::element_text(size=axistextx_size)
                           , axis.text.y = ggplot2::element_text(size=axistexty_size))
        
    } else {
        message("Plot type not recognized.")
    }

    ggplot2::ggsave(fn.plotpath, p, width=plot_W, height=plot_H, units="in")
    
    return(p)
    
}
