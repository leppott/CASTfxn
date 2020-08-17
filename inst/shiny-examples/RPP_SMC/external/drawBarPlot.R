# drawBarPlot (Specific for SMC)
# Ann.RoseberryLincoln@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 


# Create individual box plots using one grouping column and an option x-axis title
drawBarPlot <- function(df.data, fn.plotpath, plotType = "bar", groupCol, valCol
                        , plot_W=4, plot_H=4, ppi=300, str_title, str_subtitle
                        , str_ylab, str_xlab, str_caption, title_size=10
                        , subtitle_size=8, axistextx_size=8, axistexty_size=8
                        , caption_size=8) {
    
    boo_DEBUG = FALSE
    `%>%` <- dplyr::`%>%`
    
    if(boo_DEBUG==TRUE) {
        df.data = dfBCGscorePlot
        fn.plotpath = fn_BCGhistograms
        plotType = "histogram"
        groupCol="Type"
        valCol="Score"
        plot_W=4
        plot_H=4
        ppi=300
        str_title = "Histogram of BCG scores for protection or restoration"
        str_subtitle = "SMC Region"
        str_xlab = "Normalized BCG Score"
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
                dplyr::summarise(NumSamps = dplyr::n())
            
            NAcount <- dfplot$NumSamps[is.na(dfplot$Year)]
            
            dfplot <- data[!is.na(data$Year),]
            dfplot <- dfplot[dfplot$Year>=2000,] # 2000 should be start of entire dataset
            
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
                dplyr::summarise(NumSamps = dplyr::n())
            
            
            p <- ggplot2::ggplot(dfplot, ggplot2::aes(x=valCol, group_by(groupCol))) +
                ggplot2::geom_bar() +
                # ggplot2::facet_grid(groupCol ~ .) +
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
        
        p <- ggplot2::ggplot(data=dfplot, ggplot2::aes(x=Score)) +
            ggplot2::geom_histogram(position="identity", binwidth=0.02) +
            ggplot2::facet_grid(Type ~.) +
            # ggplot2::geom_text(aes(label=..count..), vjust=-1, size=3) +
            ggplot2::labs(title = str_title, subtitle=str_subtitle
                          , y=str_ylab, x=str_xlab, caption=str_caption) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position="None") +
            ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5,size=title_size)
                           , plot.subtitle=ggplot2::element_text(hjust=0.5,size=subtitle_size)
                           , axis.text.x = ggplot2::element_text(size=axistextx_size)
                           , axis.text.y = ggplot2::element_text(size=axistexty_size))
        
    } else {
        print("Plot type not recognized.")
        flush.console()
    }

    ggplot2::ggsave(fn.plotpath, p, width=plot_W, height=plot_H, units="in")
    
    return(p)
    
}
