define_vars <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Define the variables to be used in the analysis",
    "### Content",
    "",
    "Based on the raw data, define the measures to be used in the analysis"
  )
  choice_description <- c(
    "### Choice",
    "",
    "`log_gdp_capita`: A character value containing one of the",
    "following values:",
    "",
    "- `yes`: Take the natural logarithm",
    "- `no`: Use as in (USD value)"
  )
  choice_type <- list(
    list(name = "log_gdp_capita",
         type = "character",
         valid_values = c("yes", "no"),
         # weights are not required but they need to add ip to one if
         # they are provided. Adjust the example below
         weights = c(1, 0))
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)
  # ___ Analysis code starts below ___

  df <- input

  if (choice[[1]] == "yes")
    df$gdp_capita <- log(df$gdp_capita)

  return(list(
    data = df,
    protocol = list(choice)
  ))
}
