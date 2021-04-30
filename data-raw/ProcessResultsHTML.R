# Prepare data for use in the package
#
# Erik.Leppo@tetratech.com
# 2021-04-07
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy results summary HTML
# 15+ GB so takes  ~ 5 min
# 1k+ files so about 2 GB for just HTML files
# Zips to about 1 GB
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# all files at root of each subdirectory
# takes too long to recursive all dirs

# Prep -----
# assume is package directory
dn_shiny <- file.path("inst", "shiny-examples", "CASTool_SMC_v2", "Results_pre")
dn_data <- "P:\\Current\\OtherGov\\City of San Diego\\SEP\\2021UpdatedCASTResults_FINAL"

# Files to copy ----
# ~ 5 min over VPN
fn_results <- list.files(path = list.dirs(dn_data, recursive = FALSE)[1:10]
                         , pattern = "Results_Summary"
                         , full.names = TRUE)

length(fn_results) # 1,010

# Copy files ----
file.copy(from = fn_results, to = dn_shiny)

# Zip files ----
z_files <- list.files(path = dn_shiny, pattern = "html", full.names = TRUE)
for (i in z_files) {
  z_ID <- strsplit(basename(i), "_")[[1]][1]
  zip(zipfile = file.path(dn_shiny, paste0(z_ID, ".zip"))
      , files = i)
}## FOR ~ i


# Remove HTML ----
file.remove(z_files)


#~~~~~~~~~~~~~~
# too much space, not using it
