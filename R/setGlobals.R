setGlobals <- function(boo_shiny = FALSE,
                       boo_debug = FALSE,
                       dn_checked_sk = "_CheckedInputs",
                       boo_plot = TRUE,
                       pt_type = c("target", "insideND", "insideD", "outsideND", "outsideD"),
                       pt_fill = c("#CC79A7", "#56B4E9", "#E69F00", "#0072B2", "#D55E00"),
                       pt_shape = c(24, 21, 25, 21, 25),
                       pt_size = c(1.75, 0.8, 1, 0.8, 1),
                       pt_alpha = c(1, 0.5, 0.7, 0.5, 0.7),
                       refOutline_col = "#000000",
                       plot_dpi = 600,
                       plot_units = "in",
                       plot_H = 6,
                       plot_W = 8) {
  data_plotvars <- data.frame(
    "Type" = pt_type,
    "Fill" = pt_fill,
    "Shape" = pt_shape,
    "Size" = pt_size,
    "Alpha" = pt_alpha
  )

  return(list(
    boo_shiny = boo_shiny,
    boo_debug = boo_debug,
    dn_checked_sk = dn_checked_sk,
    boo_plot = boo_plot,
    data_plotvars = data_plotvars,
    refOutline_col = refOutline_col,
    plot_dpi = plot_dpi,
    plot_units = plot_units,
    plot_H = plot_H,
    plot_W = plot_W
  ))

}
