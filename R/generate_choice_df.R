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
  nobs <- id <- weight <- NULL # to make devtools:check() happy

  choice_list <- vector("list", length(d))
  if(weighted) weight_list <- vector("list", length(d))
  choice_df_cnames <- c()
  for (step in d) {
    res <- get(step)()
    step_cl <- vector("list", length(res$choice_type))
    if (weighted) step_wl <- vector("list", length(res$choice_type))
    i <- 1
    for (cs in res$choice_type) {
      if(cs$type == "character") {
        cvals <- cs$valid_values
        if (weighted) cweights <- cs$weights
      } else {
        if (weighted) {
          df <- dplyr::tibble(cvals = cs$weight_sample) %>%
            dplyr::group_by(cvals) %>%
            dplyr::summarise(nobs = dplyr::n()) %>%
            dplyr::mutate(weights = nobs / sum(nobs))
          cvals <- df$cvals
          cweights <- df$weights
        } else cvals <- seq(from = cs$valid_min, to = cs$valid_max,
                            by = (cs$valid_max - cs$valid_min)/est_by_cchoice)
      }
      step_cl[[i]] <- cvals
      if (weighted) step_wl[[i]] <- cweights
      choice_df_cnames <- c(choice_df_cnames, cs$name)
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
    wlup <- dplyr::bind_cols(
      choice = rep(choice_df_cnames, lapply(cl, length)),
      option = unlist(cl),
      weight = unlist(wl)
    )

    pos_weights <- choice_df %>%
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), as.character)
      ) %>%
      dplyr::mutate(id = 1:(dplyr::n())) %>%
      tidyr::pivot_longer(
        cols = -id, names_to = "choice", values_to = "option",
      ) %>%
      dplyr::left_join(wlup, by = c("choice", "option")) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(weight = prod(weight)) %>%
      dplyr::filter(weight > 0)

    choice_df <- choice_df %>%
      dplyr::mutate(id = 1:(dplyr::n())) %>%
      dplyr::inner_join(pos_weights, by = "id") %>%
      dplyr::select(-id)

    if (verbose) message(
      sprintf(
        "%s: Done parsing weights. %s feasible options remain",
        Sys.time(), format(nrow(choice_df), big.mark = ",")
      )
    )
  }
  choice_df
}
