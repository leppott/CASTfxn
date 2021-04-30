# Prepare results summary error page
#
# Erik.Leppo@tetratech.com
# 2021-04-30
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Shiny App  HTML and save to Shiny app www directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
#library(rmarkdown)

# 1. Convert to HTML (render) #####
# 2. Save to Shiny App www directory ####
fn_rmd <- file.path(".", "data-raw", "Shiny_CAST_NoResult.RMD")
dn_output <- file.path(".", "inst", "shiny-examples", "CASTool_SMC", "www")
rmarkdown::render(fn_rmd
                  , output_file = "ShinyNoResult.html"
                  , output_dir = dn_output)




