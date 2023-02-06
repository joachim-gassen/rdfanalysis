#' Exhaust all Researcher Degrees of Freedom in Parallel
#'
#' Parse through a research design, iterating over all possible choices, thereby
#' exhausting all documented researcher degrees of freedom. Uses the parallel
#' package to enable parallel computing. Returns a data frame containing the
#' results for all choice combinations.
#'
#' @param d A character vector of the research design steps function names
#' @param start_input The input data for the first step.
#' @param cl Either the return value of \code{parallel::make_cluster()} or
#'   the number of cores that you want the function to start. If \code{cl} is
#'   \code{NULL} then no clusters are used.
#' @param libs The libraries that the design steps rely on.
#' @param export The members of the environment that you want to export to the
#' parallel cores. Defaults to all members of the global environment.
#' @param weight Whether each step's choices should be weighted by their user
#'   assigned weights as included in the \code{choice_type}. Protocols with zero
#'   weight are excluded from the analysis. Defaults to \code{FALSE}.
#' @param est_by_cchoice Each continuous choice will be evaluated by
#'   \code{est_by_choice} equally spaced steps, staring at \code{valid_min} and
#'   ending at \code{valid_max}.
#' @param verbose Set to \code{TRUE} for some additional diagnostic output.
#'   Useful for large designs that take a while to process.
#' @return A data frame containing results for all feasible choice permutations.
#' @details See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

exhaust_design_parallel <- function(
    d, start_input, cl = NULL, libs = NULL,
    export = ls(globalenv()),
    weight = FALSE, est_by_cchoice = 10,
    verbose = FALSE
) {
  choice_df <- generate_choice_df(d, weight, est_by_cchoice, verbose)

  i <- NULL # to make devtools:check() happy

  if (!is.null(cl)) {
    if (is.numeric(cl)) {
      if (verbose) message(
        sprintf("%s: Setting up %d clusters...", Sys.time(), cl), appendLF = FALSE
      )
      cl <- parallel::makeCluster(pc)
      if (verbose) message(" done!")
      started_clusters <- TRUE
    } else started_clusters <- FALSE

    if (verbose) message(
      sprintf(
        "%s: Preparing environment for %d clusters...", Sys.time(), length(cl)
      ),
      appendLF = FALSE
    )
    invisible({
      parallel::clusterExport(cl = cl, varlist = unique(c(export, d)))
      parallel::clusterExport(cl = cl, varlist = c("libs"), envir = environment())
      parallel::clusterEvalQ(cl = cl, {
        lapply(libs, library, character.only = TRUE)
      })
    })
    if (verbose) message(" done!")
  } else started_clusters <- FALSE

  cl_job <- function(i) {
    for (step in d) {
      vars <- unlist(get(step)()$choice_type)[names(unlist(get(step)()$choice_type)) %in% "name"]
      params <- choice_df[i, vars]
      if (match(c(step), d) == 1)
        input <- do.call(get(step), list(start_input, params))
      else input <- do.call(get(step), list(input, params))
    }
    unlist(input$data)
  }

  if (verbose) message(
    sprintf(
      "%s: Running %s options...", Sys.time(),
      format(nrow(choice_df), big.mark = ",")
    )
  )

  results <- dplyr::bind_rows(
    pbapply::pblapply(1:nrow(choice_df), cl_job, cl = cl)
  )
  if (started_clusters) parallel::stopCluster(cl)

  rownames(results) <- NULL
  if (!weight) choices <- 1:ncol(choice_df)
  else choices <- 1:(ncol(choice_df) - 1)
  choice_df <- cbind(choice_df, results)
  attr(choice_df, "choices") <- choices
  rownames(choice_df) <- NULL
  choice_df
}
