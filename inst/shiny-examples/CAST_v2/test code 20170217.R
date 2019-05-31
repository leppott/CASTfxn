


myDir.Data <- "data/"


data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""))
dim(data.chem.raw)

saveRDS(data.chem.raw,"data.chem.raw.RDS")

x <- readRDS("data.chem.raw.RDS")


# P:\Current\OtherGov\City of San Diego\FY2017\CAST\CAST


#C:\Users\Erik.Leppo\OneDrive - Tetra Tech, Inc\Test

# https://tetratechinc-my.sharepoint.com/personal/erik_leppo_tetratech_com/_layouts/15/guestaccess.aspx?guestaccesstoken=PCnfPN9v5NNTjG5el5U8QfbLTXO9Gl0DmWO8bh0XOxY%3d&folderid=2_1d4eea2d38ccd4d8ea408c5c7a471465e&rev=1