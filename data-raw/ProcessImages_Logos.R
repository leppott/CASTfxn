# Create Logos using hexSticker
# Erik.Leppo@tetratech.com
# 2020-09-08
#~~~~~~~~~~~~~~
# Clint emailed logos in Powerpoint on 2020-09-04.
# Convert to R hex logos using `hexSticker` package.
# https://github.com/GuangchuangYu/hexSticker
#~~~~~~~~~~~~~~


# 0. Prep####
# Packages
library(hexSticker)
# dir
wd <- getwd() # assume is package directory


# 1. Data#####
# 1.1. Data
fn_img_CAST <- file.path(".", "inst", "extdata", "Logo_CAST_small.png")
fn_img_RPP<- file.path(".", "inst", "extdata", "Logo_RPP_small.png")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save Logo####
#
hex_CAST <- sticker(fn_img_CAST
                    , package = "CASTool"
                    , s_x = 1, s_y=1.3, s_width = 0.5
                    , p_size = 20, p_y = 0.6, p_color = "1881C2"
                    , filename = file.path(".", "inst", "extdata", "hex_CAST.png")
                    , h_color = "#1881C2" # blue
                    , h_fill = "#FFFFFF" # white
                    )
hex_CAST

hex_RPP <- sticker(fn_img_RPP
                    , package = "RPPtool"
                    , s_x = 1, s_y=1.3, s_width = 0.6
                    , p_size = 9, p_y = 0.6, p_color = "1881C2"
                    , filename = file.path(".", "inst", "extdata", "hex_RPP.png")
                    , h_color = "#1881C2" # blue
                    , h_fill = "#FFFFFF" # white
)
hex_RPP