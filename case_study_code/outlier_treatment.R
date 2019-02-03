outlier_treatment <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Select the type of outlier treatment",
    "### Content",
    "",
    "Select how you want to treat extreme observations. They can be winsorized or truncated",
    "with percentiles calculated by fiscal year or over the full sample period."
  )
  choice_description <- c(
    "### Choice",
    "",
    "A list containing a character value `outlier_tment_style` and a numerical value `outlier_cutoff`.",
    "`outlier_tment_style` may take one of the following values:",
    "",
    "- `win`: Winsorization",
    "- `win_by_yr`: Winsorization by fiscal year",
    "- `trunc`: Truncation",
    "- `trunc_by_yr`: Truncation by fiscal year",
    "",
    "`outlier_cutoff` sets the cut-off percentile for the outlier treatment",
    "and may take any value within [0, 0.10]"
  )
  choice_type <- list(
    list(name = "outlier_tment_style",
         type = "character",
         valid_values = c("win", "win_by_yr", "trunc", "trunc_by_yr"),
         weights = c(0.5, 0.5, 0, 0)),
    list(name = "outlier_cutoff",
         type = "double",
         valid_min = 0, valid_max = 0.1,
         weight_sample = c(0, 0, rep(0.01, 4), rep(0.05, 4)))
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)
  # ___ Analysis code starts below ___

  if (choice[[2]] == 0) return(
    list(
      data = input$data,
      protocol = input$protocol[[length(input$protocol) + 1]] <- choice
    ))
  switch(choice[[1]],
         "win" = input$data %>%
           treat_outliers(percentile = choice[[2]]) -> df,
         "win_by_yr" = input$data %>%
           treat_outliers(percentile = choice[[2]], by = "year") -> df,
         "trunc" = input$data %>%
           treat_outliers(percentile = choice[[2]],
                          truncate = TRUE) -> df,
         "trunc_by_yr" = input$data %>%
           treat_outliers(percentile = choice[[2]],
                          truncate = TRUE, by = "year")) -> df

  protocol <- input$protocol
  protocol[[length(protocol) + 1]] <-  choice
  return(list(
    data = df,
    protocol = protocol
  ))
}



