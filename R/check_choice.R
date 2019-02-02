#' Check Design Step Choice
#'
#' Checks whether a certain choice complies with a provided choice_type.
#' This helper function is called by the step template after the documentation block
#' when choice is not \code{NULL}.
#'
#' @param choice a list containing choices
#' @param choice_type a list of lists characterizing valid choices. Needs to be specified
#'   by the user for each design step.
#' @export
#' @examples
#'   check_choice(list("my_choice"),
#'     list(list(name = "achoice",
#'               type = "character",
#'               valid_values = c("my_choice", "another_choice"))))

check_choice <- function(choice, choice_type) {
  if (length(choice) != length(choice_type))
    stop(sprintf("Invalid number of choices provided. Should be %d but is %d.",
                 length(choice_type), length(choice)))
  for (i in 1:length(choice_type)) {
    if (choice_type[[i]]$type == "character") {
      if (! is.character(choice[[i]]))
        stop("Choice type is discrete but no character value provided (%s)", choice[[i]])
      if (! (choice[[i]] %in% choice_type[[i]]$valid_values))
        stop(sprintf("Invalid choice for choice \"%s\". Should be one of %s",
                     choice_type[[i]]$name,
                     paste(choice_type[[i]]$valid_values, collapse = ",")))

    } else {
      if (! is.numeric(choice[[i]]))
        stop("Choice type is continous but no number provided (%s)", choice[[i]])
      if (choice[[i]] < choice_type[[i]]$valid_min)
        stop("Choice value %d is below valid_min %d", choice[[i]],
             choice_type[[i]]$valid_min)
      if (choice[[i]] > choice_type[[i]]$valid_max)
        stop("Choice value %d is above valid_max %d", choice[[i]],
             choice_type[[i]]$valid_max)
    }
  }
  return (NULL)
}
