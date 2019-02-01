#' Test Design for Code Consistency
#'
#' Test whether design steps can be processed and produce valid output for each possible choice.
#'
#' @param d The design vector containing the research design steps.
#' @param protocol A list of \code{choice}s for the \code{step}s of the \code{design}
#' @param input_sim_func A function that, when called, will return a simulated input to the first step
#'   for the function. See Details section for further information.
#' @param range_n A vector containing the differnt sample sizes for which you wan to estimate the
#'   power of your design.
#' @param effect_size The effect size that you want your simulated data to reflect.
#' @param runs How many runs do you want to simulate to estimate you power?
#' @param input_sim_params If your \code{input_sim_func} needs additional parameters
#'   besides \code{n} and \code{effect_size} you can provide a list of them here.
#'
#' @return A data frame containing the \code{output} of the last \code{step} of the \code{design}
#'   along with the simulation parameters,
#'
#' @details
#' The function provided via \code{input_sim_func} needs to take a sample size parameter and an
#' effect size parameter as the first two input parameters.
#' See the vignette of the package for additional information on how to implement the RDF analysis
#' workfow.
#'
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

simulate_design_power <- function(d, protocol,
                              input_sim_func, range_n, effect_size,
                              runs = 100, input_sim_params = NULL) {
  r <- i <- NULL # for devtools::check()

  df <- foreach::foreach (i = range_n, .combine = rbind) %do% {
    foreach::foreach (r = 1:runs, .combine = rbind) %do% {
      l <- 1
      if (!is.null(input_sim_params))
        input <- do.call(input_sim_func, c(i, effect_size, input_sim_params))
      else input <- do.call(input_sim_func, list(i, effect_size))
      for (step in d) {
        input <- do.call(get(step), list(input, protocol[[l]]))
        l <- l + 1
      }
      cbind(n = i, run = r, data.frame(input$data))
    }
  }
  as.data.frame(df)
}

