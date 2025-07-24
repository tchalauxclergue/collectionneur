#' Le receptioniste: Interactive CSV Loader
#'
#' The `receptioniste` function is an interactive wrapper around `read.csv()` that helps users load a CSV file correctly.
#' If the file does not load properly (e.g., only one column is detected due to an incorrect separator),
#' the function prompts the user to modify parameters or add new arguments dynamically (see 'read.table \{utils\}' for more details).
#'
#' @param file 	The name of the file which the data are to be read from.
#' @param ... Additional arguments to pass to `read.csv()`, such as `header`, `sep`, `dec`, `na.strings`, `fileEncoding`,
#'
#' @return A data frame if the file loads correctly; otherwise, the function continues prompting the user for corrections.
#'
#' @details
#' This function helps users debug and adjust CSV import parameters interactively.
#' If the file loads incorrectly (only one column detected), users are prompted to:
#' - Modify existing `utils::read.csv()` arguments.
#' - Add new arguments dynamically.
#' - Change the file path if needed.
#'
#' @examples
#' \dontrun{
#' # Load a CSV file with default settings
#' database <- receptioniste("database.csv", sep = ";", fileEncoding = "latin1", na.strings = "NA")
#'
#' # If the file doesn't load correctly, the user is prompted to modify parameters
#' }
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
receptioniste <- function(file, ..., db.type){

  require(utils)

  args <- list(...)

  while (TRUE) {
    # Try to load the database with current arguments
    database <- tryCatch(
      do.call(utils::read.csv, c(list(file = file), args)),
      error = function(e) return(NULL)
    )

    # Check if only one column is detected
    if (!is.null(database) && ncol(database) > 1) {
      message(paste0(db.type, " loaded successfully!"))
      return(database)

    }else{
      cat("\nThe database may have loaded incorrectly. Only one column has been identified.\n")
      cat("Please verify the following parameters and provide corrections.\n")

      # Display current settings
      cat("Current settings:\n")
      cat(sprintf("  Path: %s\n", file))
      for (arg.name in names(args)) {
        cat(sprintf("  %s: %s\n", arg.name, args[[arg.name]]))
      }

      # Allow user to modify parameters
      new.file <- readline("Enter new value for 'file' path (or press Enter to keep current): ")
      if (new.file != "") {
        file <- new.file
      }

      for (arg.name in names(args)) {
        new_value <- readline(sprintf("Enter new value for '%s' (or press Enter to keep current): ", arg.name))
        if (new_value != "") {
          args[[arg.name]] <- type.convert(new_value, as.is = TRUE)
        }
      }

      # Allow user to add a new argument(s)
      new.arg <- readline("Would you like to add a new argument? (yes or press Enter to keep currents): ")
      while (tolower(new.arg) == "yes" | tolower(new.arg) == "y") {
        new.arg.name <- readline("Enter the argument name for read.csv (see 'base::read.csv' help for more details): ")
        new.arg.value <- readline(sprintf("Enter value for '%s': ", new.arg.name))

        if (new.arg.name != "" && new.arg.value != "") {
          args[[new.arg.name]] <- type.convert(new.arg.value, as.is = TRUE)
          cat(sprintf("Added argument: %s = %s\n", new.arg.name, new.arg.value))
        }

        new.arg <- readline("Would you like to add another argument? (yes or press Enter to keep currents): ")
      }
    }
  }
}

