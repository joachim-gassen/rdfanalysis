read_data <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Read data",
    "### Content",
    "",
    "Reads country year world bank data CSV file and generates raw samples"
  )
  choice_description <- c(
    "### Choice",
    "",
    "`na.omit`: A character value containing one of the",
    "following values:",
    "",
    "- `yes`: All observations with missing data are excluded",
    "- `no`: All observations with missing data are included"
  )

  # Specify your valid choices below. Format will be checked by test_design()
  # for consictency

  choice_type <- list(
    list(name = "na.omit",
         type = "character",
         valid_values = c("yes", "no"),
         weights = c(1, 0))
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)
  # ___ Analysis code starts below ___

  df <- read_csv(input, col_types = cols()) %>%
    select(country, year,
           lifeexpectancy, gdp_capita,
           resdevelop_gdp, unemployment)

  if(choice == "yes") df <- df %>%
    na.omit()

  return(list(
    data = df,
    protocol = list(choice)
  ))
}
