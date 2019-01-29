#' Test Design for Code Consistency
#'
#' Test whether design steps can be processed and produce valid output for each possible choice.
#'
#' @param d The design vector containing the research design steps.
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
#' @details See the vignette of the package for further details.
#' @export
#' @examples
#'   demo_step <- function(input = NULL, choice = NULL) {
#'     step_description <- doc(
#'       "## demo_step",
#'       "### Content",
#'       "",
#'       "This is a demo step. It simply returns two times the value of the choice variable"
#'     )
#'     choice_description <- doc(
#'       "### Choice",
#'       "",
#'       "`base_value`: A numerical value that needs to be within the range [0, 10]"
#'     )
#'     choice_type <- list(
#'       list(name = "base_value",
#'       type = "double",
#'       valid_min = 0,
#'       valid_max = 10)
#'     )
#'     if (is.null(choice)) return(list(
#'       step_description = step_description,
#'       choice_description = choice_description,
#'       choice_type = choice_type
#'     )) else check_choice(choice, choice_type)
#'
#'     return(list(
#'       data = choice[[1]] * 2,
#'       protocol = choice
#'     ))
#'   }
#'
#'   design <- "demo_step"
#'  test_design(design)

test_design <- function(d, input = NULL,
                        input_test_code = NULL,
                        output_test_code = NULL, ...) {
  e <- testthat::test_env()
  e$d <- d
  if(!is.null(input)) {
    if (is.function(input)) input <- input()
    e$input <- input
    if(is.null(input_test_code))
      e$input_test_code <- paste0("user_code/", d[1], "_test_input.R")
    else e$input_test_code <- input_test_code
    e$wd <- getwd()
    if(!is.null(output_test_code) &&
       (length(output_test_code) != length(d) ||
        is.character(output_test_code)))
      stop("Invalid format for output_test_code. Needs to be a character vector with a test code path for each design step")
    e$output_test_code <- output_test_code
  }
  pkg_app_dir <- system.file("test_code", package = "rdfanalysis")
  test_code <- paste0(pkg_app_dir, "/test_design.R")
  testthat::test_file(test_code, env = e, encoding = "UTF-8", ...)
}
