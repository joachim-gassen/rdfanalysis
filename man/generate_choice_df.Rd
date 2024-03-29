% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_choice_df.R
\name{generate_choice_df}
\alias{generate_choice_df}
\title{Creates the choice dataframe for the exhaust_design functions}
\usage{
generate_choice_df(d, weighted = FALSE, est_by_cchoice = 10, verbose = FALSE)
}
\arguments{
\item{d}{A character vector of the research design steps function names}

\item{weighted}{Whether each step's choices should be weighted by their user
assigned weights as included in the \code{choice_type}. Protocols with zero
weight are excluded from the analysis. Defaults to \code{FALSE}.}

\item{est_by_cchoice}{Each continuous choice will be evaluated by
\code{est_by_choice} equally spaced steps, staring at \code{valid_min} and
ending at \code{valid_max}.}

\item{verbose}{Set to \code{TRUE} for some additional diagnostic output.
Useful for large designs that take a while to process.}
}
\value{
A dataframe containing all feasible research design choices
}
\description{
Creates the choice dataframe for the exhaust_design functions
}
\details{
See the vignette of the package for further details.
}
\examples{
\dontrun{
  print("Sorry. No examples yet.")
}
}
