treat_extreme_obs <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Select the type of outlier treatment",
    "### Content",
    "",
    "Select how you want to treat extreme observations. They can be winsorized or truncated",
    "to a given percentile of the data."
  )
  choice_description <- c(
    "### Choice",
    "",
    "A list containing a character value `outlier_tment_style` and a numerical value `outlier_cutoff`.",
    "`outlier_tment_style` may take one of the following values:",
    "",
    "- `win`: Winsorization",
    "- `trunc`: Truncation",
    "",
    "`outlier_cutoff` sets the cut-off percentile for the outlier treatment",
    "and may take any value within [0, 0.10]"
  )
  choice_type <- list(
    list(name = "outlier_tment_style",
         type = "character",
         valid_values = c("win", "trunc"),
         weights = c(0.5, 0.5)),
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

  if (choice[[2]] == 0) {
    protocol <- input$protocol
    protocol[[length(protocol) + 1]] <-  choice
    return(list(
      data = input$data,
      protocol = protocol
    ))
  }

  switch(choice[[1]],
         "win" = {
           df <- input$data
           df[, 3:ncol(df)] <-
             ExPanDaR::treat_outliers(df[, 3:ncol(df)], percentile = choice[[2]])
           df
         },
         "trunc" = {
           df <- input$data
           df[, 3:ncol(df)] <-
             ExPanDaR::treat_outliers(df[, 3:ncol(df)],
                            percentile = choice[[2]],
                            truncate = TRUE)
           df
         })

  protocol <- input$protocol
  protocol[[length(protocol) + 1]] <-  choice
  return(list(
    data = df,
    protocol = protocol
  ))
}
