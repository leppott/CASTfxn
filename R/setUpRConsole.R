setUpRConsole <- function(
    in_dir,
    out_dir,
    region,
    useBC = FALSE,
    install_missing = TRUE,
    load_local = TRUE
) {
  # Check/install pak
  if (!requireNamespace("pak", quietly = TRUE)) {
    if (install_missing) {
      install.packages("pak")
    } else {
      stop("Package 'pak' is required but not installed.")
    }
  }

  # Check/install CASToolHelperPckg
  if (!requireNamespace("CASToolHelperPckg", quietly = TRUE)) {
    if (install_missing) {
      message("Installing CASToolHelperPckg")
      pak::pak("laura-naslund/CASToolHelperPckg")
    } else {
      stop("Package 'CASToolHelperPckg' is required but not installed.")
    }
  }

  # Load helper package
  library(CASToolHelperPckg)

  # Load local package/functions if requested
  if (load_local) {
    devtools::load_all()
  }

  # Return setup values
  return(list(
    in_dir = in_dir,
    out_dir = out_dir,
    region = region,
    useBC = useBC
  ))
}
