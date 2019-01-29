#' Document your research design in a PDF file
#'
#' Uses the documentation block of each step to generate a rmarkdown based
#' documentation of the research design. Output can be saved to a PDF file
#' or directly viewed.
#'
#' @param d The design vector containing the research design steps.
#' @param output_file The file name to store the PDF file to.
#' @param title A title string.
#' @param code A logical value indicating whether the R code of the design
#'   steps should be included in the documentation (defaults to TRUE).
#' @param rmd_header The character value containing the header of the rmarkdown
#'   file that the function generates. Defaults to a PDF output header
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
#'   prepare_design_documentation(design, "my_design.pdf")

prepare_design_documentation <-
  function(d, output_file,
           title = "Research Design",
           code = TRUE,
           rmd_header = paste0("--- \n",
                               sprintf("title: \"%s\"\n", title),
                               sprintf("date: \"%s\"\n", Sys.Date()),
                               "output: pdf_document\n",
                               "---\n\n")) {
    file <- tempfile(fileext = ".Rmd")
    write(rmd_header, file)
    for (step in d) {
      res <- get(step)()
      write(sprintf("# Step: %s\n\n", step), file, append = TRUE)
      write("\n", file, append = TRUE)
      write(res$step_description, file, append = TRUE)
      write("\n", file, append = TRUE)
      write(res$choice_description, file, append = TRUE)
      write("\n\n", file, append = TRUE)
      if (code) {
        write("### Code\n\n", file, append = TRUE)
        code_text <- utils::capture.output(get(step))
        write(sprintf("``` {r %s, eval = FALSE}\n", step), file, append = TRUE)
        write(code_text[1], file, append = TRUE)
        code_line = FALSE
        for (l in 2:(length(code_text) - 1)) {
          if(code_line) write(code_text[l], file, append = TRUE)
          else if (trimws(code_text[l]) == "") code_line <- TRUE
        }
        write(sprintf("```\n\n\n", step), file, append = TRUE)
      }
    }
    dir <- paste0(getwd(), "/", dirname(output_file))
    rmarkdown::render(file, output_dir = dir,
                      output_file = basename(output_file))
  }
