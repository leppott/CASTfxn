function(){
  tabPanel("HELP"
            , fluidPage(
                 fluidRow(h2("Help"
                             , style  = "text-align:center"))##fluidRow~END
                , htmlOutput("help_html")


            
             )##fluidPage~END   
           
  )##tabPanel~END
}##FUNCTION~END
