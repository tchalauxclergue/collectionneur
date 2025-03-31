#' Interactive Question Prompt with Validation
#'
#' The `questioner` function prompts the user with a question and validates the response against a set of allowed answers.
#' It ensures that the user provides a valid response before proceeding.
#'
#' @param question Character. The question to be displayed to the user.
#' @param answers Character vector. A list of acceptable answers.
#'
#' @return The validated user input as a character string.
#' @export
#'
#' @examples
#' # Example usage:
#' user.choice <- questioner("Choose an option: (yes) or (no) ", c("yes", "no"))
#' print(user.choice)
#'
#' @author Thomas Chalaux-Clergue
#'
questioneur <- function(question, answers){

  deplay.answers <- answers

  if("" %in% answers){
    deplay.answers[which(answers == "")] <- "Enter/Exit"
  }else{
    deplay.answers <- answers
  }

  choice <- readline(base::cat(question))

  while(!(choice %in% answers)){

    base::cat(paste0("\nInput error. Please choose from the available options: ", base::paste0(base::paste0("(", deplay.answers[1:(length(deplay.answers)-1)], ")", collapse = ", "), " or (", deplay.answers[length(deplay.answers)]), ") \n"))

    choice <- base::readline(question)

  }

  return(choice)
}
