#' Output a series of characters with newlines
#'
#' This is a small helper function for documetation
#'
#' @param ... parameters passed on to \code{paste(..., sep ="\n")}
#' @return The return value from \code{paste()}
#' @export
#' @examples
#'   doc("## Documentation",
#'     "Documentation withtout the need to",
#'     "include new lines is wonderful."
#'   )

doc <- function(...) {
  paste(..., sep = "\n")
}
