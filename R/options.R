#' Options
#'
#' Options for parkR package.
#'
#' Currently only \code{debug} are used.
#'
#' @param ... a list of options.
#'
#' @return The function prints all set options if called with no arguments.
#' When setting options, nothing is returned.
#' @export
#'
#' @examples
#' parkr_options()
#'
#' parkr_options(debug = TRUE)
parkr_options <- function(...){
  x <- list(...)
  opts <- .parkr_env$opts
  if(length(x)){
    opts[names(x)] <- x
    .parkr_env$opts <- opts
    invisible()
  } else {
    opts
  }
}

.parkr_env <- new.env()
parkr_options(debug = F)
parkr_options(phrase_breaks = 0:15)
