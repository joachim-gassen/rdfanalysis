select_idvs <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Identify the variables for analysis",
    "### Content",
    "",
    "Select the variables that you want to use in the regression analysis."
  )
  choice_description <- c(
    "### Choice",
    "",
    "`idvs`: A character value containing one of the",
    "following values:",
    "",
    "- `gdp_only`: GDP per capita only",
    "- `gdp_school`: GDP per capita and mean years schooling",
    "- `gdp_ue`: GDP per capita and unemployment",
    "- `full`: Full model including all three variables"
  )
  choice_type <- list(
    list(name = "idvs",
         type = "character",
         valid_values = c("gdp_only", "gdp_school", "gdp_ue", "full"),
         weights = c(0, 0, 0, 1))
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)
  # ___ Analysis code starts below ___

  df <- switch (choice[[1]],
                "gdp_only" = input$data %>% select(-mn_yrs_school, -unemployment),
                "gdp_school" = input$data %>% select(-unemployment),
                "gdp_ue" = input$data %>% select(-mn_yrs_school),
                "full" = input$data
  )

  protocol <- input$protocol
  protocol[[length(protocol) + 1]] <-  choice
  return(list(
    data = df,
    protocol = protocol
  ))
}
