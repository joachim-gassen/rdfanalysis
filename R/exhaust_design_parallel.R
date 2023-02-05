#' Exhaust all Researcher Degrees of Freedom in Parallel
#'
#' Parse through a research design, iterating over all possible choices, thereby
#' exhausting all documented researcher degrees of freedom. Uses the DoParallel
#' package to enable parallel computing. Returns a data frame containing the
#' results for all choice combinations.
#'
#' @param d A character vector of the research design steps function names
#' @param start_input The input data for the first step.
#' @param pc The number of cores you want \code{doParallel::makeCluster()} to use.
#' @param libs The libraries that the design steps rely on.
#' @param exports The members of the environment that you want to export to the
#' parallel cores. Defaults to all members of the parent environment.
#' @param weight Whether each step's choices should be weighted by their user
#'   assigned weights as included in the \code{choice_type}. Protocols with zero
#'   weight are excluded from the analysis. Defaults to \code{FALSE}.
#' @param est_by_cchoice Each continuous choice will be evaluated by
#'   \code{est_by_choice} equally spaced steps, staring at \code{valid_min} and
#'   ending at \code{valid_max}.
#' @return A data frame containing results for all feasible choice permutations.
#' @details See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

exhaust_design_parallel <- function(d, start_input, pc, libs,
                                    export = ls(parent.env(environment())),
                                    weight = FALSE, est_by_cchoice = 10,
                                    ) {
  choice_df <- generate_choice_df(d, weight, est_by_cchoice)

  doParallel::registerDoParallel(cores = pc)
  pb <- utils::txtProgressBar(max = nrow(choice_df), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  results <-
    foreach::foreach (i = 1:nrow(choice_df), .combine = rbind,  .options.multicore = opts,
             .packages = libs,
             .export = c(export, d)) %dopar% {
               for (step in d) {
                 vars <- unlist(get(step)()$choice_type)[names(unlist(get(step)()$choice_type)) %in% "name"]
                 params <- choice_df[i, vars]
                 if (match(c(step), d) == 1)
                   input <- do.call(get(step), list(start_input, params))
                 else input <- do.call(get(step), list(input, params))
               }
               unlist(input$data)
             }
  close(pb)

  choice_df <- cbind(choice_df, results)
  rownames(choice_df) <- NULL
  choice_df
}
