est_model <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Estimates the model",
    "### Content",
    "",
    "Uses a multiple regression setup to generate an estimate and",
    "a confidence interval for the effect of GDP per capita on life",
    "expectancy"
  )
  choice_description <- c(
    "### Choice",
    "",
    "A list containing two character values: `cluster` and `feffect`.",
    "`cluster` defines the clsutering of standard errors and",
    "`feffect` defines the fixed effect structure of the model.",
    "Each value may take one of the following values:",
    "",
    "- `none`, `ctry`, `year` or `ctryyear`"
  )
  choice_type <- list(
    list(name = "feffect",
         type = "character",
         valid_values = c("none", "country", "year", "ctryyear"),
         weights = c(0, 0, 0, 1)),
    list(name = "cluster",
         type = "character",
         valid_values = c("none", "country", "year", "ctryyear"),
         weights = c(0, 0, 0, 1))
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)

  # ___ Analysis code starts below ___

  f <- paste0("lifeexpectancy ~ ",
              paste(colnames(input$data)[3:(length(input$data) - 1)], collapse = " + "))

  if (choice[[1]] == "country" || choice[[1]] == "year") f <- paste0(f, "| ", choice[[1]])
  else if (choice[[1]] == "ctryyear") f <- paste0(f, " | country + year")

  if (choice[[2]] == "none") form <- as.formula(f)
  else if (choice[[1]] == "none") f <- paste0(f, " | 0 ")

  if (choice[[2]] == "country" || choice[[2]] == "year") f <- paste0(f, "| 0 | ", choice[[2]])
  else f <- paste0(f, "| 0 | country + year ")

  if (choice[[2]] != "none") form <- as.formula(f)

  mod <- lfe::felm(form, input$data)

  l <- list(
    est = mod$coefficients[row.names(mod$coefficients) == 'gdp_capita'] - 1,
    lb = confint(mod)[row.names(mod$coefficients) == 'gdp_capita', 1] - 1,
    ub = confint(mod)[row.names(mod$coefficients) == 'gdp_capita', 2] - 1
  )

  protocol <- input$protocol
  protocol[[length(protocol) + 1]] <-  choice
  return(list(
    data = l,
    protocol = protocol
  ))
}
