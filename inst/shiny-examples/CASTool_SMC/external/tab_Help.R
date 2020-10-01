function(){
  tabPanel("HELP",
           tabsetPanel(id = "tsp_HELP"
                       , tabPanel("General", value = "pan_help"
                                  , h2("Help") #, style  = "text-align:center")
                                  , fluidRow(htmlOutput("help_html"))
                                  )##tabPanel ~ Help
                       , tabPanel("Plots", value = "pan_legends"
                                  , h3("Legend Key")
                                  , fluidRow(htmlOutput("LegKey_html"))
                                  )##tabPanel~Plot Key~END
                       , tabPanel("Scoring", value = "pan_legends"
                                  , h3("Scoring")
                                  #, fluidRow(htmlOutput("LegKey_html"))
                                  , p("Scoring included in decriptions of plots.")
                       )##tabPanel~Plot Key~END
           )##tabsetPanel
  )##tabPanel~END
}##FUNCTION~END
