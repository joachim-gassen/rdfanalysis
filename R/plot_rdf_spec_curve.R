#' Plot the Specification Curve of your Reseracher Degrees of Freedom
#'
#' Plots a visulization of the specification curve as introduced by
#' Simonsohn, Simmons and Nelson.
#'
#' @param ests The data frame provided by \code{exhaust_design()}.
#' @param est A character string indicating the estimate that you want to plot.
#' @param lb A character string indicating the lower bound of the estimate.
#' @param ub A character string indicating the upper bound of the estimate.
#' If both \code{lb} und \code{ub} are present the confindence interval around
#' the estimate is plotted and the significane of each estimate is assessed.
#' @param sample_frac The percentage of the protocols that should be plottted.
#' If your design produces many degrees of freedom, sampling speeds up plotting
#' time and makes the plot less clutered.
#' @param sig_color Color of significant estimates (only used when \code{lb} and
#' \code{ub} are present).
#' @param ribbon_color Color of the confidence interval (only used when \code{lb} and
#' \code{ub} are present).
#' @return A plot.
#' @details See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export
plot_rdf_spec_curve <- function(ests, est, lb = "", ub = "",
                                     sample_frac = 0.1, sig_color = "red",
                                     ribbon_color = "lightblue") {
  if (xor(lb == "", ub == "")) stop("lb and ub need to be provided both or not at all")
  choices <- names(ests)[attr(ests, "choices")]
  for(c in choices) {
    if (c == choices[1]) {
      item_levels <- sprintf("%s: %s", c, unique(ests[,c]))
    } else {
      item_levels <- c(item_levels, sprintf("%s: %s", c, unique(ests[,c])))
    }
  }

  if (lb != "") {
    es <- ests %>%
      dplyr::select(!!! rlang::syms(choices),
             !! rlang::sym(est), !! rlang::sym(lb), !! rlang::sym(ub)) %>%
      dplyr::arrange(!! rlang::sym(est), !! rlang::sym(lb), !! rlang::sym(ub)) %>%
      dplyr::mutate(n = row_number()) %>%
      dplyr::sample_frac(sample_frac) %>%
      dplyr::gather(key = "choice", value = "option", -n,
             -!!rlang::sym(est), -!!rlang::sym(lb), -!!rlang::sym(ub)) %>%
      dplyr::mutate(item = sprintf("%s: %s", choice, option),
             sig = !!rlang::sym(lb) > 0) %>%
      dplyr::select(n, choice, item, sig,
             !!rlang::sym(est), !!rlang::sym(lb), !!rlang::sym(ub)) %>%
      dplyr::arrange(n)
  } else {
    es <- ests %>%
      dplyr::select(!!!rlang::syms(choices), !!rlang::sym(est)) %>%
      dplyr::arrange(!!rlang::sym(est)) %>%
      dplyr::mutate(n = row_number()) %>%
      dplyr::sample_frac(sample_frac) %>%
      dplyr::gather(key = "choice", value = "option", -n,
             -!!rlang::sym(est)) %>%
      dplyr::mutate(item = sprintf("%s: %s", choice, option)) %>%
      dplyr::select(n, choice, item, !!rlang::sym(est)) %>%
      dplyr::arrange(n)
  }

  es$item <- factor(es$item, levels = rev(item_levels))
  es$choice <- factor(es$choice, levels = choices)

  if (lb != "") {
    sc <- es %>%
      dplyr::select(n, sig,
             !!rlang::sym(est), !!rlang::sym(lb), !!rlang::sym(ub)) %>%
      dplyr::distinct() %>%
      ggplot2::ggplot(aes(x = n, y = !!rlang::sym(est))) +
      ggplot2::geom_ribbon(aes(ymin = !!rlang::sym(lb), ymax = !!rlang::sym(ub)),
                  fill = ribbon_color, color = ribbon_color) +
      ggplot2::geom_point(aes(color = sig), size = 0.01) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      ggplot2::guides(color = FALSE) +
      ggplot2::scale_color_manual(values=c("black", sig_color))
  } else {
    sc <- es %>%
      dplyr::select(n, !!rlang::sym(est)) %>%
      dplyr::distinct() %>%
      ggplot2::ggplot(aes(x = n, y = !!rlang::sym(est))) +
      ggplot2::geom_point(size = 0.01) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }

  dc <- ggplot2::ggplot(data = es, aes(x = n, y = item, color = choice)) +
    ggplot2::geom_point(size = 0.01) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Protocol ID") +
    ggplot2::ylab("") +
    ggplot2::guides(color = FALSE)

  gsc <- ggplot2::ggplot_build(sc)
  gdc <- ggplot2::ggplot_build(dc)
  gA <- ggplot2::ggplot_gtable(gsc)
  gB <- ggplot2::ggplot_gtable(gdc)
  g <- gtable:::rbind_gtable(gA, gB, "last")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels[2]] <- unit(3,"null")
  grid::grid.newpage()
  grid::grid.draw(g)
}
