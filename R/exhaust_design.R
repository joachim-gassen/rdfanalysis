generate_choice_df <- function(d, weighted = FALSE, est_by_cchoice = 10) {
  nobs <- NULL # to make ddevtools:check() happy

  choice_list <- vector("list", length(d))
  if(weighted) weight_list <- vector("list", length(d))
  choice_df_cnames <- c()
  for (step in d) {
    res <- get(step)()
    step_cl <- vector("list", length(res$choice_type))
    if (weighted) step_wl <- vector("list", length(res$choice_type))
    i <- 1
    for (c in res$choice_type) {
      if(c$type == "character") {
        cvals <- c$valid_values
        if (weighted) cweights <- c$weights
      } else {
        if (weighted) {
          df <- dplyr::tibble(cvals = c$weight_sample) %>%
            dplyr::group_by(cvals) %>%
            dplyr::summarise(nobs = dplyr::n()) %>%
            dplyr::mutate(weights = nobs / sum(nobs))
          cvals <- df$cvals
          cweights <- df$weights
        } else cvals <- seq(from = c$valid_min, to = c$valid_max,
                            by = (c$valid_max - c$valid_min)/est_by_cchoice)
      }
      step_cl[[i]] <- cvals
      if (weighted) step_wl[[i]] <- cweights
      choice_df_cnames <- c(choice_df_cnames, c$name)
      i <- i + 1
    }
    choice_list[[match(c(step), d)]] <- step_cl
    if(weighted) weight_list[[match(c(step), d)]] <- step_wl
  }
  cl <- unlist(choice_list, recursive = FALSE)
  choice_df <- expand.grid(cl, stringsAsFactors = FALSE)
  colnames(choice_df) <- choice_df_cnames
  if(weighted) {
    wl <- unlist(weight_list, recursive = FALSE)
    choice_df$weight <- NA
    for (i in 1:nrow(choice_df)) {
      weight <- 1
      for (j in 1:(ncol(choice_df) - 1))
        weight <- weight * wl[[j]][match(choice_df[i,j], cl[[j]])]
      choice_df$weight[i] <- weight
    }
    choice_df <- choice_df%>%
      dplyr::filter(weight > 0)
  }
  choice_df
}


#' Exhaust all Researcher Degrees of Freedom
#'
#' Parse through a reserch design, iterating over all possible choices, thereby exhausting all
#' documented researcher degrees of freedom. Returns a data frame containing the results for all
#' choice combinations.
#'
#' @param d The design vector containing the research design steps.
#' @param start_input The input data for the first step.
#' @param weight Whether each step's choices should be weighted by their user assigned weights as
#'   included in the \code{choice_type}. Protocols with zero weight are excluded from the analysis.
#'   Defaults to FALSE.
#' @param est_by_cchoice Each continous choice will be evaluated by \code{est_by_choice} equally
#'   spaced steps, staring at \code{valid_min} and ending at \code{valid_max}.
#' @return A data frame containing results for all feasible choice permutations.
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
#'  exhaust_design(design, NULL)

exhaust_design <- function(d, start_input, weight = FALSE, est_by_cchoice = 10) {
  choice_df <- generate_choice_df(d, weight, est_by_cchoice)

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
  choice_df <- cbind(choice_df, results)
  rownames(choice_df) <- NULL
  choice_df
}