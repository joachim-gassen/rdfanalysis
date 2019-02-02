#' Source the R files that contain the design steps
#'
#' This sources all code files constructed by \code{define_design()}
#'
#' @param d A character vector of the research design steps function names
#' @param rel_dir The relative path to your code directory, deafults to "code"
#' @param one_file If \code{TRUE}, then all step code is expected to be in a file
#'   with the name \code{one_file_name}. If {FALSE} (the default), each step code
#'   is expected to be in a separate file with the step name as file name.
#' @param one_file_name The name of the code file conating the code if
#'   all step code is in one file. Defaults to "design_steps.R".
#' @return The result of the \code{source()} command.
#' @details See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

source_design <- function(d, rel_dir = "code",
                         one_file = FALSE, one_file_name = "design_steps.R") {
  code_dir <- file.path(getwd(), rel_dir)
  if (!dir.exists(code_dir))
    stop(sprintf("Specified %s directory does not exist", code_dir))
  if (one_file) {
    code_file <- file.path(code_dir, one_file_name)
    if (!file.exists(code_file))
      stop(sprintf("Specified file %s does not exist", code_file))
    source(code_file)
  } else {
    code_files <- file.path(code_dir, paste0(d, ".R"))
    if (!all(file.exists(code_files)))
      stop(sprintf("The following code files do not exist: %s",
           paste(code_files[which(!file.exists(code_files))],
                 collapse = ", ")))
    source(code_files)
  }
}

