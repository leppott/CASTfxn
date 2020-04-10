# Prepare RMD output for Disclaimer_Key
#
# Erik.Leppo@tetratech.com
# 20191107
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
library(rmarkdown)

# 1. Define RMD #####
dn_input <- file.path(".", "data-raw", "Legends")
fn_rmd   <- "Legend_Key.rmd"
dn_output <- file.path(".", "inst", "shiny-examples", "CAST_SMC", "www")

# 2. render/knit RMD
rmarkdown::render(file.path(dn_input, fn_rmd), output_format = "html_document", output_dir = dn_output)
