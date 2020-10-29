function(){
  tabPanel("Disclaimer"
            , fluidPage(
                 fluidRow(h2("Causal Assessment Screening Tool (CASTool) Disclaimer"
                             , style  = "text-align:center"))##fluidRow~END
        #       # , htmlOutput("help_html")
             , fluidRow(column(width = 6, offset = 3, p("The CASTool is a Causal Assessment Screening Tool.  
             Its intended to assist the City of San Diego and its agents to rapidly screen and identify 
             the causes of biological impairment in a given waterbody, and thereby assist with 
             prioritizing restoration and protection actions.  Due the CASTool's reliance 
             on currently available and sometimes incomplete data, however, it is 
             not intended to be a final arbiter on causal assessments analyses, 
             nor does it represent any commitments by the City to perform any specific
             projects, studies, or other actions.")))

        #    
             )##fluidPage~END   
           
  )##tabPanel~END
}##FUNCTION~END
