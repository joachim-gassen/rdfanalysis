#' Test Design for Code Consistency
#'
#' Test whether design steps can be processed and produce valid output for each possible choice.
#'
#' @param d A character vector of the research design steps function names
#' @param input input parameter of the first step. Can be either data (passed to first step as is)
#'   or a function. If it is a function, the function is called to generate the input data.
#'   If you do not provide \code{input} then only \code{step_description}, \code{choice_description}
#'   and \code{choice_type} of each step are tested for consisitency. If \code{input} is provided,
#'   you also need an R code file to test \code{input} for validity.
#' @param input_test_code A path to an R file containg test code to verifiy the integrity of \code{input}
#'   data.
#' @param output_test_code A character vector of a length equal to the length of \code{d} containing
#'   paths to test code files for each step's output.
#' @param ... Are passed to \code{testthat::test_file()}
#' @return The return value \code{testthat::test_file()}
#' @details If called without parameters for \code{input}, only the \code{choice_type} list structure is
#'   tested for consistency. If \code{input} is provided, data steps are tested across all choices to verify
#'   that they produce structually valid output without errors or warnings.
#'   If \code{input_test_code} is provided this test code is used to verify \code{input}.
#'   If \code{output_test_code} is provided each step's output is tested.
#'   See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

test_design <- function(d, input = NULL,
                        input_test_code = NULL,
                        output_test_code = NULL, ...) {
  e <- testthat::test_env()
  e$d <- d
  if(!is.null(input)) {
    e$input <- input
    e$input_test_code <- input_test_code
    e$wd <- getwd()
    if(!is.null(output_test_code) &&
       (length(output_test_code) != length(d) ||
        is.character(output_test_code)))
      stop("Invalid format for output_test_code. Needs to be a character vector with a test code path for each design step")
    e$output_test_code <- output_test_code
  }
  pkg_app_dir <- system.file("test_code", package = "rdfanalysis")
  test_code <- paste0(pkg_app_dir, "/test_design.R")
  testthat::test_file(test_code, env = e, ...)
}
