#' Exhaust all Researcher Degrees of Freedom
#'
#' Parse through a research design, iterating over all possible choices, thereby exhausting all
#' documented researcher degrees of freedom. Returns a data frame containing the results for all
#' choice combinations.
#'
#' @param d A character vector of the research design steps function names.
#' @param start_input The input data for the first step.
#' @param weight Whether each step's choices should be weighted by their user assigned weights as
#'   included in the \code{choice_type}. Protocols with zero weight are excluded from the analysis.
#'   Defaults to FALSE.
#' @param est_by_cchoice Each continous choice will be evaluated by \code{est_by_choice} equally
#'   spaced steps, staring at \code{valid_min} and ending at \code{valid_max}.
#' @param verbose Set to \code{TRUE} for some additional diagnostic output.
#'   Useful for large designs that take a while to process.
#' @return A data frame containing results for all feasible choice permutations.
#'
#' @details See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

exhaust_design <- function(
    d, start_input, weight = FALSE, est_by_cchoice = 10, verbose = FALSE
) {
  choice_df <- generate_choice_df(d, weight, est_by_cchoice, verbose)

  pb <- utils::txtProgressBar(max = nrow(choice_df), style = 3)

  i <- NULL # to make devtools:check() happy

  results <- foreach::foreach (i = 1:nrow(choice_df), .combine = rbind) %do% {
    for (step in d) {
      vars <- unlist(get(step)()$choice_type)[names(unlist(get(step)()$choice_type)) %in% "name"]
      params <- choice_df[i, vars]
      if (match(c(step), d) == 1)
        input <- do.call(get(step), list(start_input, params))
      else input <- do.call(get(step), list(input, params))
    }
    utils::setTxtProgressBar(pb, i)
    unlist(input$data)
  }
  close(pb)
  rownames(results) <- NULL
  if (!weight) choices <- 1:ncol(choice_df)
  else choices <- 1:(ncol(choice_df) - 1)
  choice_df <- cbind(choice_df, results)
  attr(choice_df, "choices") <- choices
  rownames(choice_df) <- NULL
  choice_df
}
