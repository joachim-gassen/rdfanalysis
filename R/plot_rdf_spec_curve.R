#' Plot the Specification Curve of your Researcher Degrees of Freedom
#'
#' Visualizes a result estimate across your researcher degrees of
#' freedom by plotting its specification curve as introduced by
#' \href{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2694998}{Simonsohn, Simmons and Nelson}.
#'
#' @param ests The data frame provided by \code{\link[rdfanalysis:exhaust_design]{exhaust_design()}}.
#' @param est A character string indicating the estimate that you want to plot.
#' @param lb A character string indicating the lower bound of the estimate.
#' @param ub A character string indicating the upper bound of the estimate.
#' If both \code{lb} und \code{ub} are present the plot includes a confindence
#' interval around the estimate and colors the estimate points according to
#' their significance (see below).
#' @param sample_frac The percentage of the protocols that should be plottted.
#' If your design produces many degrees of freedom, sampling speeds up plotting
#' time and makes the plot less clutered.
#' @param est_color Color of estimates points for specification curve
#' @param est_color_signeg Color of significantly negative estimates.
#' @param est_color_sigpos Color of significantly positive estimates.
#' @param ribbon_color Color of the confidence interval (only used when \code{lb} and
#' \code{ub} are present).
#' @param lower_to_upper The size of the choice part of the plot, relative to
#' the specification curve itself.
#' @return A ggplot object.
#' @details
#' Significance of estimates is only dislayed when \code{lb} and
#' \code{ub} are present. It is assessed by testing whether
#' \code{lb} and \code{ub} are of equal sign.
#'
#' See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export
plot_rdf_spec_curve <- function(ests, est, lb = "", ub = "",
                                sample_frac = 1,
                                est_color = "black",
                                est_color_signeg = "yellow",
                                est_color_sigpos = "blue",
                                ribbon_color = "lightblue",
                                lower_to_upper = 3) {
  choice <- item <- n <- option <- sig <- NULL
  # To make devtools::check() happy

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
      dplyr::mutate(n = dplyr::row_number()) %>%
      dplyr::sample_frac(sample_frac) %>%
      tidyr::gather(key = "choice", value = "option", -n,
             -!!rlang::sym(est), -!!rlang::sym(lb), -!!rlang::sym(ub)) %>%
      dplyr::mutate(item = sprintf("%s: %s", choice, option),
             sig = factor(sign(!!rlang::sym(lb)) + sign(!!rlang::sym(ub)))) %>%
      dplyr::select(n, choice, item, sig,
             !!rlang::sym(est), !!rlang::sym(lb), !!rlang::sym(ub)) %>%
      dplyr::arrange(n)
  } else {
    es <- ests %>%
      dplyr::select(!!!rlang::syms(choices), !!rlang::sym(est)) %>%
      dplyr::arrange(!!rlang::sym(est)) %>%
      dplyr::mutate(n = dplyr::row_number()) %>%
      dplyr::sample_frac(sample_frac) %>%
      tidyr::gather(key = "choice", value = "option", -n,
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
      ggplot2::ggplot(ggplot2::aes(x = n, y = !!rlang::sym(est))) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = !!rlang::sym(lb),
                                        ymax = !!rlang::sym(ub)),
                           fill = ribbon_color, color = ribbon_color) +
      ggplot2::geom_point(ggplot2::aes(color = sig), size = 0.01) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::guides(color = FALSE) +
      ggplot2::scale_color_manual(values = c("-2" = est_color_signeg,
                                             "0" = est_color,
                                             "2" = est_color_sigpos))
  } else {
    sc <- es %>%
      dplyr::select(n, !!rlang::sym(est)) %>%
      dplyr::distinct() %>%
      ggplot2::ggplot(ggplot2::aes(x = n, y = !!rlang::sym(est))) +
      ggplot2::geom_point(size = 0.01, color = est_color) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank())
  }

  dc <- ggplot2::ggplot(data = es, ggplot2::aes(x = n, y = item, color = choice)) +
    ggplot2::geom_point(size = 0.01) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Protocol") +
    ggplot2::ylab("") +
    ggplot2::guides(color = FALSE)

  gsc <- ggplot2::ggplot_build(sc)
  gdc <- ggplot2::ggplot_build(dc)
  gA <- ggplot2::ggplot_gtable(gsc)
  gB <- ggplot2::ggplot_gtable(gdc)

  # The following is more or less c&p from
  # g <- gtable:::rbind_gtable(gA, gB, "last")
  # to avoid the devtools::check note
  # complaining about the tripple `:::`
  gB$layout$t <- gB$layout$t + nrow(gA)
  gB$layout$b <- gB$layout$b + nrow(gA)
  gA$layout <- rbind(gA$layout, gB$layout)
  gA$heights <- grid::unit.c(gA$heights, gB$heights)
  gA$rownames <- c(gA$rownames, gB$rownames)
  gA$widths <- gB$widths
  gA$grobs <- append(gA$grobs, gB$grobs)
  g <- gA

  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels[2]] <- ggplot2::unit(lower_to_upper,"null")
  grid::grid.newpage()
  grid::grid.draw(g)
}
