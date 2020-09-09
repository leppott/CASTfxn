# Shiny, UI
# RPP - SMC
#

# Packages
#library(shiny)

# Source Pages ####
# Load files for individual screens
tab_Disclaimer  <- source("external/tab_Disclaimer.R", local=TRUE)$value
tab_Map_Station <- source("external/tab_Map_Station.R", local=TRUE)$value
tab_Map_Reach   <- source("external/tab_Map_Reach.R", local=TRUE)$value
tab_RPP_Calc    <- source("external/tab_RPP_Calc.R", local=TRUE)$value
tab_Help        <- source("external/tab_Help.R", local=TRUE)$value

# Need for console messages to Shiny
shinyjs::useShinyjs()

# Define UI for application that draws a histogram
shinyUI(
  # fluidPage(
	  navbarPage(title = div(img(src="RPPTool.png", height = 35), "Restoration and Protection Potential Tool (RPPTool), SMC")
	  #navbarPage("Restoration and Protection Potential Tool (RPPTool), SMC 2020-09-01"
				 , theme = "bootstrap.css"
				 , inverse = TRUE
				 , tab_Disclaimer()
				 , tab_Map_Station()
				 , tab_Map_Reach()
				 , tab_RPP_Calc()
				 , tab_Help()
		)## navbarPage~ END
  # )##fluidPage~END
)##shinyUI~END
  
