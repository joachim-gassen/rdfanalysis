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
#' @param est_label Y-Axis label for the specification curve. Defaults to
#' \code{est}.
#' @param title A title for the plot
#' @param est_color Color of estimates points for specification curve
#' @param est_color_signeg Color of significantly negative estimates.
#' @param est_color_sigpos Color of significantly positive estimates.
#' @param line_color Color of confidence interval lines (only used when
#' \code{lb} and \code{ub} are present).
#' @param ribbon If TRUE a ribbon displaying the confidence interval is being
#' plotted. If FALSE, lines are printed instead (looks nicer if you have only
#' few degrees of freedom to plot) (only used when \code{lb} and
#' \code{ub} are present).
#' @param ribbon_color Color of the confidence interval (only used when
#' \code{lb} and \code{ub} are present).
#' @param pt_size Point plot size for estimates and choice indicators that are
#' not highlighted (see below).
#' @param choice_ind_point Whether you want your choice indicators to be
#' points (TRUE) or vertical lines (FALSE).
#' @param lower_to_upper The size of the choice part of the plot, relative to
#' the specification curve itself.
#' @param highlight \code{NULL} or, if you want to highlight certain protocols
#' by their point size, a data frame containing the choices that identify the
#' protocols.
#' @param pt_size_highlight Point plot size for highlighted estimates and
#' choice indicators.
#' @param addon_sc ggplot objects to add to the the specification curve panel.
#' @param addon_dc ggplot objects to add to the the design choice panel.
#' @param file file name to save the plot to. Uses ggsave and defaults to NULL,
#'  meaning no save
#' @param ... Additional parameter that are forwarded to ggsave
#' @return Nothing. Instead the assembled grob is directly drawn.
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
plot_rdf_spec_curve <- function(
    ests, est, lb = "", ub = "", sample_frac = 1,
    est_label = est, title = "", est_color = "black",
    est_color_signeg = "#E41A1C", est_color_sigpos = "#377EB8",
    line_color = "black",
    ribbon = nrow(ests) > 30,
    ribbon_color = "gray90",
    choice_ind_point = TRUE, pt_size = 0.1,
    lower_to_upper = 3, highlight = NULL,
    pt_size_highlight = 3,
    addon_sc = NULL, addon_dc = NULL,
    file = NULL, ...
) {
  choice <- item <- n <- option <- sig <- NULL
  # To make devtools::check() happy

  choices <- names(ests)[attr(ests, "choices")]

  if(!is.null(highlight)) {
    if (!is.data.frame(highlight) | !identical(names(highlight), choices))
      stop("invalid highlight parameter. Needs to be a data frame with all choices")
    if (sample_frac != 1)
      warning(paste("Highlightening protocols is not advised with sampling",
                    "as protocols might be omitted from the plot"))
  }
  if (xor(lb == "", ub == "")) stop("lb and ub need to be provided both or not at all")

  if(!is.null(addon_sc) & !is.object(addon_sc) & !is.list(addon_sc)) stop(
    "addon_sc needs to be an object or a list of objects"
  )
  if(!is.null(addon_dc) & !is.object(addon_dc) & !is.list(addon_dc)) stop(
    "addon_dc needs to be an object or a list of objects"
  )

  for(c in choices) {
    if (c == choices[1]) {
      item_levels <- sprintf("%s: %s", c, unique(ests[,c]))
    } else {
      item_levels <- c(item_levels, sprintf("%s: %s", c, unique(ests[,c])))
    }
  }

  if (!is.null(highlight)) {
    highlight$highlight <- TRUE
    ests <- ests %>%
      dplyr::left_join(highlight, by = choices)
    ests$highlight[is.na(ests$highlight)] <- FALSE
  } else ests$highlight <- FALSE

  if (lb != "") {
    es <- ests %>%
      dplyr::select(highlight, !!! rlang::syms(choices),
             !! rlang::sym(est), !! rlang::sym(lb), !! rlang::sym(ub)) %>%
      dplyr::arrange(!! rlang::sym(est), !! rlang::sym(lb), !! rlang::sym(ub)) %>%
      dplyr::mutate(n = dplyr::row_number()) %>%
      dplyr::sample_frac(sample_frac) %>%
      tidyr::gather(key = "choice", value = "option", -n, -highlight,
             -!!rlang::sym(est), -!!rlang::sym(lb), -!!rlang::sym(ub)) %>%
      dplyr::mutate(item = sprintf("%s: %s", choice, option),
             sig = factor(sign(!!rlang::sym(lb)) + sign(!!rlang::sym(ub)))) %>%
      dplyr::select(n, choice, item, highlight, sig,
             !!rlang::sym(est), !!rlang::sym(lb), !!rlang::sym(ub)) %>%
      dplyr::arrange(n)
  } else {
    es <- ests %>%
      dplyr::select(highlight, !!!rlang::syms(choices), !!rlang::sym(est)) %>%
      dplyr::arrange(!!rlang::sym(est)) %>%
      dplyr::mutate(n = dplyr::row_number()) %>%
      dplyr::sample_frac(sample_frac) %>%
      tidyr::gather(key = "choice", value = "option", -n, -highlight,
             -!!rlang::sym(est)) %>%
      dplyr::mutate(item = sprintf("%s: %s", choice, option)) %>%
      dplyr::select(n, choice, item, highlight, !!rlang::sym(est)) %>%
      dplyr::arrange(n)
  }

  es$item <- factor(es$item, levels = rev(item_levels))
  es$choice <- factor(es$choice, levels = choices)

  if (lb != "") {
    sc <- es %>%
      dplyr::select(n, sig, highlight,
             !!rlang::sym(est), !!rlang::sym(lb), !!rlang::sym(ub)) %>%
      dplyr::distinct() %>%
      ggplot2::ggplot(ggplot2::aes(x = n, y = !!rlang::sym(est)))
    if(ribbon) sc <- sc +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = !!rlang::sym(lb),
                                          ymax = !!rlang::sym(ub)),
                             fill = ribbon_color, color = ribbon_color)
    else sc <- sc +
        ggplot2::geom_segment(ggplot2::aes(xend = n, y = !!rlang::sym(lb),
                                           yend = !!rlang::sym(ub)),
                              color = line_color)
    sc <- sc +
      ggplot2::geom_point(ggplot2::aes(color = sig, size = highlight)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(y = est_label, title = title) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::guides(color = "none", size = "none") +
      ggplot2::scale_color_manual(values = c("-2" = est_color_signeg,
                                             "0" = est_color,
                                             "2" = est_color_sigpos)) +
      ggplot2::scale_size_manual(values = c("FALSE" = pt_size,
                                             "TRUE" = pt_size_highlight))
  } else {
    sc <- es %>%
      dplyr::select(n, !!rlang::sym(est)) %>%
      dplyr::distinct() %>%
      ggplot2::ggplot(ggplot2::aes(x = n, y = !!rlang::sym(est))) +
      ggplot2::geom_point(ggplot2::aes(size = highlight), color = est_color) +
      ggplot2::theme_minimal() +
      ggplot2::labs(y = est_label, title = title) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::guides(size = "none") +
      ggplot2::scale_size_manual(values = c("FALSE" = pt_size,
                                            "TRUE" = pt_size_highlight))
  }

  if (!is.null(addon_sc)) sc <- sc + addon_sc

  dc <- ggplot2::ggplot(data = es, ggplot2::aes(x = n, y = item, color = choice)) +
    ggplot2::geom_blank()

  if (choice_ind_point)
    dc <- dc +  ggplot2::geom_point(ggplot2::aes(size = highlight))
  else
    dc <- dc + ggplot2::geom_segment(ggplot2::aes(
      xend = n,
      y = as.numeric(item) - 0.2,
      yend = as.numeric(item) + 0.2,
      size = highlight
    ))

  dc <- dc +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Protocol") +
    ggplot2::ylab("") +
    ggplot2::guides(color = "none", size = "none") +
    ggplot2::scale_size_manual(values = c("FALSE" = pt_size,
                                          "TRUE" = pt_size_highlight))

  if (!is.null(addon_dc)) dc <- dc + addon_dc

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
  if (!is.null(file)) ggplot2::ggsave(file, plot = g, ...)
  grid::grid.draw(g)
}
