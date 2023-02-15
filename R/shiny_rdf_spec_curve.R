#' @title An Interactive Specification Curve
#'
#' @description A shiny based web app that allows you to explore your
#'   researcher degrees of freedom's specification curve interactively.
#'
#' @param ests The data frame provided by \code{\link{exhaust_design}}.
#' @param spec_curve_parms A list containing additional parameters that will be
#'   passed on to \code{\link{plot_rdf_spec_curve}}.
#'   You can also provide a named list of lists with each list containing
#'   parameters for a specification curve. In this case, the shiny app will
#'   include an additional Select Input element where the users can select
#'   which specification curve to display.
#' @param spec_curve_selected If you provide a named list of specification
#'   curves in \code{spec_curve_parms}, you can provide here the name of
#'   the default curve to plot first.
#' @param design if not \code{NULL} it takes the design that was used to generate
#'   the estimates. In this case, you either need to have all required design
#'   elements in your current environment or you need to specify the
#'   \code{rel_dir} parameter pointing to the code files below. In addition, you
#'   need to set \code{start_input}. The shiny app will then display
#'   regression results when you select choices that generate less than
#'   \code{regression_cutoff} estimates.
#' @param rel_dir The path to the code directory where the design functions are
#'   located. Only needed when the functions are loaded to your current
#'   environment See above.
#' @param start_input The parameters that you pass to the first design step.
#'   See above.
#' @param libs A vector containing additional packages that need to be attached
#'   to run the design. NOTE: This will modify the shiny app code to include
#'   literal \code{library()} calls so that shinyapps.io includes the libraries
#'   on deployment.
#' @param add_files A character vector containing relative paths to files and
#'   directories that you want to bundle with the shiny app. The files will be
#'   copied to the temporary directory that hosts the shiny app and directories
#'   will be copied recursively.
#' @param regression_cutoff If your choices generate less or equal estimates,
#'   the display will switch to normal regression table output (needs parameters
#'   above to be not \code{NULL}).
#' @param model_render_func A function to create the regression table,
#'   taking a list of the models as parameter. The function is evaluated within
#'   the shiny app environment. By default (\code{NULL}),
#'   the regressions are rendered by calling the internal function
#'   \code{renderModels()} that then calls
#'   \code{\link[modelsummary]{modelsummary}}
#'   to create the HTML output.
#'   If you need to prep the model data for preparation, you can provide
#'   a function here that calls \code{renderModels()} after prepping the data.
#'   Alternatively, you can provide a function
#'   that generates the HTML output directly.
#' @param default_choices A list containing choices that you want
#'   the app to start with. If \code{NULL}, it will start with all choices
#'   included.
#' @param restore_button Set to \code{TRUE} when you want to have a restore
#'   button in the app (defaults to \code{FALSE}).
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
shiny_rdf_spec_curve <- function(
    ests, spec_curve_parms, spec_curve_selected = NULL,
    design = NULL, rel_dir = NULL,
    start_input = NULL, libs = NULL, add_files = NULL,
    regression_cutoff = 5,
    model_render_func = NULL,
    default_choices = NULL, restore_button = FALSE,
    title = "A Shiny Specification Curve", abstract = NULL,
    choice_labels = NULL
) {
  if (!is.data.frame(ests)) stop("ests is not a dataframe")
  if (!is.list(spec_curve_parms) || length(spec_curve_parms) < 1)
    stop("spec_curve_parms needs to be a non-empty list")

  if(!is.null(spec_curve_selected)) {
    if (!is.character(spec_curve_selected))
      stop(paste(
        "spec_curve_selected needs to be a character string containing the name",
        "of the default specification curve to plot"
      ))
    if(! spec_curve_selected %in% names(spec_curve_parms))
      stop(paste(
        "spec_curve_selected does not match a name of ",
        "a specification curve provided in spec_curve_parms"
      ))
  }

  if (!is.null(design)) {
    if (!all(unlist(lapply(design, exists))) & is.null(rel_dir))
      stop(paste(
        "You set a design but the functions are not included in the",
        "environment and you also do not provide a path in rel_dir"
      ))
    if (is.null(start_input)) stop(paste(
      "You set a design but did not provide the start parameter for the",
      "first step in start_input."
    ))
  }

  if (!is.null(choice_labels)) {
    if (length(choice_labels) != length(attr(ests, "choices")))
      stop("choice_labels does not have labels for each choice column in ests")
    if (!is.character(choice_labels))
      stop("choice_labels is not a character vector")
  }

  pkg_app_dir <- system.file("application", package = "rdfanalysis")
  tdir <- tempdir()
  app_dir <- file.path(tdir, "application")
  if (is.null(libs)) {
    file.copy(pkg_app_dir, tdir, recursive=TRUE)
  } else {
    libs_code <- sprintf("library(%s)", libs)
    app_code <- readLines(file.path(pkg_app_dir, "app.R"), encoding = "UTF-8")
    dir.create(app_dir)
    writeLines(c(libs_code, app_code), file.path(app_dir, "app.R"))
  }


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

  if (!is.null(model_render_func) & !is.function(model_render_func)) {
    stop(paste(
      "model_render_func either needs to be NULL or a function",
      "taking the model list as a parameter and returning HTML code."
    ))
  }


  objects <- c(
    "ests", "spec_curve_parms", "spec_curve_selected",
    "design", "rel_dir", "libs", "start_input",
    "regression_cutoff", "model_render_func", "default_choices",
    "title", "abstract", "choice_labels", "restore_button"
  )

  if (!is.null(design) & is.null(rel_dir)) {
    objects <- unique(c(objects, ls(envir = .GlobalEnv)))
  }

  save(list = objects, file = paste0(app_dir, "/shiny.Rda"))
  on.exit(unlink(app_dir, recursive = TRUE))
  try(shiny::runApp(appDir = app_dir))
}
