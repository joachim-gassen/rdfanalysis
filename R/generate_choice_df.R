#' Creates the choice dataframe for the exhaust_design functions
#'
#' @param d A character vector of the research design steps function names
#' @param weighted Whether the options should be lim
#' @param weight Whether each step's choices should be weighted by their user
#'   assigned weights as included in the \code{choice_type}. Protocols with zero
#'   weight are excluded from the analysis. Defaults to \code{FALSE}.
#' @param est_by_cchoice Each continuous choice will be evaluated by
#'   \code{est_by_choice} equally spaced steps, staring at \code{valid_min} and
#'   ending at \code{valid_max}.
#' @param verbose Set to \code{TRUE} for some additional diagnostic output.
#'   Useful for large designs that take a while to process.
#' @noRd

generate_choice_df <- function(
    d, weighted = FALSE, est_by_cchoice = 10, verbose = FALSE
) {
  nobs <- NULL # to make devtools:check() happy

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
  if (verbose) message(
    sprintf(
      "%s: Choice space determined: %d choices, %s options",
      Sys.time(), length(cl), format(nrow(choice_df), big.mark = ",")
    )
  )
  colnames(choice_df) <- choice_df_cnames
  if(weighted) {
    if (verbose) message(
      sprintf(
        "%s: weight = TRUE, idenitifying options with positive weights",
        Sys.time()
      )
    )
    wl <- unlist(weight_list, recursive = FALSE)
    choice_df$weight <- NA
    for (i in 1:nrow(choice_df)) { #
      weight <- 1
      for (j in 1:(ncol(choice_df) - 1))
        weight <- weight * wl[[j]][match(choice_df[i,j], cl[[j]])]
      choice_df$weight[i] <- weight
      if (verbose) {
        if (i %% 10000 == 0) message(
          sprintf(
            "%s: Parsed %s options", Sys.time(), format(i, big.mark = ",")
          )
        )
      }
    }
    choice_df <- choice_df%>%
      dplyr::filter(weight > 0)
    if (verbose) message(
      sprintf(
        "%s: Done parsing weights. %s feasible options remain",
        Sys.time(), format(nrow(choice_df), big.mark = ",")
      )
    )
  }
  choice_df
}
