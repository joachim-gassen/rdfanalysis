#' Document your research design in a PDF file
#'
#' Uses the documentation block of each step to generate a rmarkdown based
#' documentation of the research design. Output can be saved to a PDF file
#' or directly viewed.
#'
#' @param d A character vector of the research design steps function names
#' @param output_file The file name to store the PDF file to.
#' @param title A title string.
#' @param code A logical value indicating whether the R code of the design
#'   steps should be included in the documentation (defaults to TRUE).
#' @param rmd_header The character value containing the header of the rmarkdown
#'   file that the function generates. Defaults to a PDF output header
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

prepare_design_documentation <-
  function(d, output_file,
           title = "Research Design",
           code = TRUE,
           rmd_header = c("--- ",
                          sprintf("title: \"%s\"", title),
                          sprintf("date: \"%s\"", Sys.Date()),
                          "output: pdf_document",
                          "---",
                          "")) {
    file <- tempfile(fileext = ".Rmd")
    file.create(file)
    con <- file(file, "w")
    writeLines(rmd_header, con)
    for (step in d) {
      res <- get(step)()
      writeLines(sprintf("# Step: %s", step), con)
      writeLines(c("", ""), con)
      writeLines(res$step_description, con)
      writeLines("", con)
      writeLines(res$choice_description, con)
      writeLines(c("", ""), con)
      if (code) {
        write("### Code", con)
        code_text <- utils::capture.output(get(step))
        writeLines(sprintf("``` {r %s, eval = FALSE}\n", step), con)
        writeLines(code_text[1], con)
        start_code <- grep("___ Analysis code starts below ___", code_text)
        writeLines(code_text[(start_code + 1):length(code_text)], con)
        writeLines(c("```", "", ""), con)
      }
    }
    close(con)
    dir <- paste0(getwd(), "/", dirname(output_file))
    rmarkdown::render(file, output_dir = dir,
                      output_file = basename(output_file))
  }
