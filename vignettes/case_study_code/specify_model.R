specify_model <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Specify the model type to be estimated",
    "### Content",
    "",
    "Specify whether you want to estimate a level model or whether you",
    "want to use log tansformed dependent and/or independent variables."
  )
  choice_description <- c(
    "### Choice",
    "",
    "`model_type`: A character value containing one of the",
    "following values:",
    "",
    "- `level-level`: Use untransformed dependent and independent variables",
    "- `log-level`: Use log-transformed dependent and",
    "untransformed independent variables",
    "- `level-log`: Use untransformed dependent and",
    "log-transformed independent variables",
    "- `log-log`: Use log-transformed dependent and log-independent variables"
  )
  choice_type <- list(
    list(name = "model_type",
         type = "character",
         valid_values = c("level-level", "log-level",
                          "level-log", "log-log"),
         weights = c(0, 0, 1, 0))
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)
  # ___ Analysis code starts below ___

  df <- input$data

  if (choice == "log-level" | choice == "log-log")
    df$lifeexpectancy <- log(df$lifeexpectancy)
  if (choice == "level-log" | choice == "log-log")
    df[,4:ncol(df)] <- log(df[,4:ncol(df)])

  protocol <- input$protocol
  protocol[[length(protocol) + 1]] <- choice

  return(list(
    data = df,
    protocol = protocol
  ))
}
