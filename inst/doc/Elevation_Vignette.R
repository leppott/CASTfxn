## ----Setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo=TRUE)
library(knitr)
library(CASTfxn)

## ----AZ_Elev-------------------------------------------------------------
# Packages
library(CASTfxn)
#
# Data
df_AZ <- data_Sites
#
# Summarize
cnt.NA <- sum(is.na(df_AZ$Elevation))
cnt.Total <- nrow(df_AZ)
pct.NA  <- round(cnt.NA / cnt.Total, 3)
#
summary(df_AZ$Elevation)
knitr::kable(cbind(cnt.NA, cnt.Total, pct.NA), caption = "Missing Data for AZ Stations Elevation")

## ----GetElev, eval=FALSE-------------------------------------------------
#  # Packages
#  library(CASTfxn)
#  # Function Arguments
#  df_loc <- data_Sites
#  Lat <- "FinalLatitude"
#  Long <- "FinalLongitude"
#  elev_units <- "feet"
#  proj_loc <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#  # Run Function
#  df_elev <- getElev(df_loc, Lat, Long, elev_units, proj_loc)
#  # Save Results
#  dir_out <- getwd()
#  fn_out <- file.path(dir_out, "AZ_Elev.tsv")
#  write.table(df_elev, fn_out, sep="\t", row.names=FALSE, col.names = TRUE)

## ----AppElev, fig.width=7------------------------------------------------
# Packages
library(CASTfxn)
library(ggplot2)

# Data
dir_data <- file.path(path.package(package="CASTfxn"), "extdata")
fn_elev <- file.path(dir_data, "AZ_Elev.tsv")
df_elev <- read.delim(fn_elev)
# new class elevation category
df_elev$ElevCat2 <- ifelse(df_elev$elevation>=5000,"hi","lo")

# Table
knitr::kable(table(df_elev$ElevCategory, df_elev$ElevCat2), caption="Elevation Categories (Left=AZ, Top=getElev)")

# Plot 1, Elevation Category
p1 <- ggplot(df_elev, aes(Elevation, elevation, color=ElevCategory)) + 
  geom_point() + 
  geom_hline(yintercept=5000, color="blue") + 
  geom_vline(xintercept=5000, color="blue") + 
  labs(x="Elevation (ft) [AZ data]", y="Elevation (ft) [elevatr package]")
p1

# Plot 2, State Map
p3 <- ggplot(data = subset(map_data("state"), region =="arizona")) + 
  geom_polygon(aes(x=long, y=lat, group=group), fill=NA, color="black") +
  coord_fixed(1.3) +
  geom_point(data=df_elev, aes(FinalLongitude, FinalLatitude, color=elevation, shape=ElevCategory)) + 
  scale_shape_manual(name="Elevation Category", values=c(17,16)) + 
    scale_color_gradientn(colours = terrain.colors(5))
p3


