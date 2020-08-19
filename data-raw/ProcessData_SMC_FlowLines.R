# Process Data
# GIS - SMC Reaches (flowline)
# Erik.Leppo@tetratech.com
# 2020-08-12
#
# Already saved SHP as RDA in Shiny App.
# No "ProcessData" script.  Recreate it here using parts from global.R.
# test simplify

# 0. Prep####
# Packages
library(rgdal)
library(rgeos)
library(usethis)

# 1. Get data and process#####
# 1.1. Import Data
# Prepare SMC flowlines
sp_flowline <- rgdal::readOGR(dsn = "inst/extdata/SMCReaches", layer = "SMCReaches_aea")
sp_flowline_wgs <- spTransform(sp_flowline, CRS("+proj=longlat +datum=WGS84 +no_def"))
flowlines <- list(flowline_aea = sp_flowline, flowline_wgs = sp_flowline_wgs)

# Test = FAIL
# library(leaflet)
# leaflet() %>%
#   # Groups, Base
#   addTiles(group="OSM (default)") %>%  #default tile too cluttered
#   addProviderTiles("CartoDB.Positron", group="Positron") %>%
#   addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
#   addPolylines(data=lines.flowline.proj
#                , color="blue"
#                , popup=~COMID
#                , highlightOptions=highlightOptions(bringToFront=TRUE
#                                                    , color="red" )
#                , group="Streams")

# * DOESN'T WORK ***

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
lines.flowline.proj <- flowlines
usethis::use_data(lines.flowlines.proj, overwrite = TRUE)
