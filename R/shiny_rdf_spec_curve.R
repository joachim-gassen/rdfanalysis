#' @title An Interactive Specification Curve
#'
#' @description A shiny based web app that allows you to explore your
#'   researcher degrees of freedom's specification curve interactively.
#'
#' @param ests The data frame provided by \code{\link[rdfanalysis:exhaust_design]{exhaust_design()}}.
#' @param spec_curve_parms A list containing additional paramters that will be
#'   passed on to \code{\link[rdfanalysis:plot_rdf_spec_curve]{plot_rdf_spec_curve()}}.
#' @param design if not \code{NULL} it takes the design that was used to generate
#'   the estimates. In this case, you also need to specify the \code{rel_dir}
#'   and \code{start_input} parameter below. The shiny app will then display
#'   full regresssion results when you select choices that generate less than
#'   \code{regression_cutoff} estimates.
#' @param rel_dir The path to the code directory. See above.
#' @param start_input The parameters that you pass to the first design step.
#'   See above.
#' @param libs A vector containing additional packages that need to be attached
#'   to run the design. NOTE: While this works fine when you host the shiny app
#'   yourself, shinyapps.io fails to include the listed packages when deploying
#'   the app. So, if you plan to host your app on shinyapps.io, you are better
#'   served including \code{library()} calls in your design code files or to use
#'   the \code{::} operator in your code.
#' @param add_files A character vector containing relative paths to files and
#'   dreictories that you want to bundle with the shiny app. The files will be
#'   copied to the temporary directory that hosts the shiny app and directories
#'   will be copied recursively.
#' @param regression_cutoff If your choices generate less or equal estimates,
#'   the display will switch to normal regression output (needs parameters above
#'   to be not \code{NULL}).
#' @param default_choices A list containing choices that you want
#'   the app to start with. If \code{NULL}, it will start with all choices
#'   included.
#' @param title The title of the shiny app.
#' @param abstract Text that will be displayed by the app. Wrapped
#'   into \code{HTML()} so that you can use HTML code.
#' @param choice_labels Character vector containing the labels that will be used
#'   to label the select list input controls in the shiny app. If \code{NULL},
#'   the select list input controls are labeled based on the choice column names
#'   from the \code{ests} data frame.
#'
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export
shiny_rdf_spec_curve <- function(ests, spec_curve_parms,
                                 design = NULL, rel_dir = NULL,
                                 start_input = NULL, libs = NULL,
                                 add_files = NULL,
                                 regression_cutoff = 5,
                                 default_choices = NULL,
                                 title = "A Shiny Specification Curve",
                                 abstract = NULL,
                                 choice_labels = NULL) {
  if (!is.data.frame(ests)) stop("ests is not a dataframe")
  if (!is.list(spec_curve_parms) || length(spec_curve_parms) < 1)
    stop("spec_curve_parms needs to be a non-empty list")
  if (!all(is.null(c(design, rel_dir, start_input))) &&
           any(is.null(c(design, rel_dir, start_input))))
    stop(paste("When you set one of design, rel_dir, start_input, you need",
               "to set all of them"))

  if (!is.null(choice_labels)) {
    if (length(choice_labels) != length(attr(ests, "choices")))
      stop("choice_labels does not have labels for each choice column in ests")
    if (!is.character(choice_labels))
      stop("choice_labels is not a character vector")
  }

  pkg_app_dir <- system.file("application", package = "rdfanalysis")
  file.copy(pkg_app_dir, tempdir(), recursive=TRUE)
  app_dir <- file.path(tempdir(), "application")
  if (!is.null(rel_dir)) {
    code_dir <- file.path(app_dir, "code")
    dir.create(code_dir)
    file.copy(file.path(rel_dir, paste0(design, ".R")), code_dir)
  }

  if(!is.null(add_files)) {
    if (!is.character(add_files))
      stop("add_files is not a character vector")
    if (any(!file.exists(add_files)))
      stop("add_files points to files that do not exist")
    file.copy(add_files, app_dir, recursive = TRUE)
  }

  save(ests, spec_curve_parms, design, rel_dir, libs, start_input,
       regression_cutoff, default_choices, title, abstract, choice_labels,
       file = paste0(app_dir, "/shiny.Rda"))
  on.exit(unlink(app_dir, recursive = TRUE))
  try(shiny::runApp(appDir = app_dir))
}
