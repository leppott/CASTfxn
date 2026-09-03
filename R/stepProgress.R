#' Step progress
#'
#' @param config
#' @param msg
#' @param prog_cnt
#' @param prog_n
#' @param prog_sleep
#'
#' @return
#' @export
#'
#' @examples
stepProgress <- function(config, msg, prog_cnt, prog_n, prog_sleep){
  if(isTRUE(config$boo_shiny)){
    prog_cnt <- prog_cnt + 1
    incProgress(1/prog_n, message = paste0("Step ", prog_cnt), detail = msg)
    Sys.sleep(prog_sleep)
    message("Step: ", prog_cnt, ";", msg)
  }
  return(prog_cnt)
}
