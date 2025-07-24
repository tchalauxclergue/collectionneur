#' La Bibliothecaire: Update a Database with New Column Headers and Generate a Report
#'
#' This function updates a database by integrating new column headers from an 'additions' dataset.
#' It provides an interactive way to match, rename, or add new columns, and generates a detailed
#' report on the changes made.
#'
#' @param database A data frame representing the existing database.
#' @param additions A data frame containing new data to be integrated into the database.
#' @param method A string indicating the string distance method used for column name matching. Defaults to "jw" (Jaro-Winkler).
#' @param save.report Logical. If `TRUE`, a report summarizing the updates will be saved. Default is `TRUE`.
#' @param return.report Logical. If `TRUE`, the report is included in the function output. Default is `TRUE`.
#' @param save.dir Character. Optional directory path for saving the report. Required if `save.report = TRUE`.
#' @param database.label Character. Label for the database, used for naming output files.
#' @param note Character. Optional additional note to append to the file and report file name.
#' @param sep A character specifying the field separator for CSV files (default: `","`).
#' @param dec A character specifying the decimal separator for CSV files (default: `"."`).
#' @param na Character string, used for missing values in the data.= (default - "").
#' @param fileEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written, "latin1" (default).
#'
#' @details
#' The function proceeds as follows:
#' - Identifies new column headers in `additions` that are not present in `database`.
#' - Prompts the user to match new headers with existing ones in `database` (optional).
#' - Updates column names accordingly or adds them as new columns filled with `NA`.
#' - Allows the user to choose whether to maintain the column order from `additions`.
#' - Generates a detailed report documenting all modifications.
#'
#' The function supports both automatic string similarity matching (via `stringdist`) and manual selection of column names.
#'
#' @return A list containing:
#' - `database`: The updated database with integrated columns.
#' - `additions`: The input additions dataset (unchanged).
#' - `report`: A character vector containing the report (if `return.report = TRUE`).
#'
#' @examples
#' \dontrun{
#' db <- data.frame(A = 1:5, B = letters[1:5])
#' new.data <- data.frame(B = letters[1:5], C = 6:10)
#'
#' result <- bibliothecaire(database = db, additions = new.data, save.report = FALSE)
#'
#'
#' }
#'
#' @author Thomas Chalaux-Clergue
#' @export
#'list
bibliothecaire <- function(database, additions, method = "jw", save.updates = TRUE, save.report = TRUE, return.report = FALSE, save.dir, database.label, note, sep = ",", dec = ".", na = "", fileEncoding = "latin1") {

  require(stringdist)

  # settings
  choice.2.A2 <- "no"
  choice.2.A4 <- ""

  # Identify newcolumns
  new.columns <- base::setdiff(colnames(additions), colnames(database))
  new.columns.corr <- new.columns

  # Rpint the new columns
  cat(sprintf("\nNew column header(s) were found in the additions: %s.\n", paste(paste0("[", seq_along(new.columns), "] '", new.columns, "'"), collapse = ", ")))

  # Report front head
  col.widths <- c(15, 10, 40, 40, 40) # Define col width

  report.header <- c(sprintf("%-*s------ Database Update Report ------%-*s\n\n", 58, "", 57, ""),
                     sprintf("%-*s------------------------------------%-*s\n", 58, "", 57, ""),
                     sprintf(" Start %s%-*s collectionneur::bibliothecaire %-*s", paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Sys.timezone()), 21, "", 23, ""),
                     sprintf("%-*s------------------------------------%-*s\n\n", 58, "", 57, ""),
                     sprintf(" New column headers found in 'additions':\n\t%s\n", collectionneur::renvoi.a.la.ligne(new.columns, max.line.length = sum(col.widths))),
                     sprintf(" %-*s %-*s %-*s %-*s %-*s\n",
                             col.widths[1], "Replacement",
                             col.widths[2], "Resolve",
                             col.widths[3], "Database header",
                             col.widths[4], "Additions header",
                             col.widths[5], "Final header"))

  column.headers.sep <- gsub("\\w", "-", report.header[length(report.header)])

  report.header <- c(report.header, column.headers.sep)

  report.lines <- c()


  # Does the user identifiyed column that should exist
  choice.1 <- collectionneur::questioneur("Should some of these 'new' column headers from 'additions' correspond to existing 'database' columns? (yes/no)  ", c("yes", "no"))


  ## DUPLICATED NAMES
  if (choice.1 == "yes") {

    choice.management <- collectionneur::questioneur("Review (all) new column headers or (select) a column header?  ", c("all", "select"))

    # settings
    index <- 1
    continue.editing <- "yes"

    repeat{
      if (continue.editing == "no") { break }
      if (index > length(new.columns)) { break }

      resolve.type <- "/-x-/" # Report

      # 1 - UPDATE OF THE INDEX
      ### Review mode to all headers
      if (choice.management == "all") {
        new.name <- new.columns[index]
      }

      ### Review mode to user selected headers
      if (choice.management == "select") {

        # Display available new column header (s) with indices
        pluriel <- "" # manage plural
        if (length(new.columns) > 1) { pluriel <- "s" } # manage plural
        cat(sprintf("\nNew column header%s: %s.\n", pluriel, paste(paste0("[", seq_along(new.columns), "] '", new.columns, "'"), collapse = ", ")))


        index <- -999  # Initialize invalid choice

        # Ask for the index of the new column to edit
        repeat{
          index <- as.numeric(collectionneur::questioneur( "Enter the index of the column header to edit: ",  c(as.character(seq_along(new.columns)), "") ))

          if (!is.na(index) && index %in% seq_along(new.columns)) {

            repeat{
              # Ask for confirmation
              validate.index <- collectionneur::questioneur( sprintf("You selected: '%s'. Do you confirm? (yes/no)  ", new.columns[index]), c("yes", "no") )

              if (validate.index == "yes") { break } # Exit the inner loop if confirmed

              # If the user does not confirm, ask for a new index or quit
              index <- collectionneur::questioneur( "Enter another column header index or 'Enter' to quit?  ", c(as.character(seq_along(new.columns)), "") )

              if (index == "") { break } # Exit both loops if the user chooses to quit

              index <- as.numeric(index)
              if (!is.na(index) && index %in% seq_along(new.columns)) { next } # Restart the confirmation loop with the new index

              base::cat("\nInvalid input. Please enter a valid index.\n")
            }
            break  # Exit the outer loop when a valid and confirmed index is chosen
          }
          base::cat("\nInvalid input. Please enter a valid index.\n")
        }

        new.name <- new.columns[index]  # Get the selected column name
      }

      # 2 - EDIT HEADERs
      # Print the current column name
      cat(sprintf("\n\n   *****  '%s' [%s/%s]  *****\n", new.name, which(new.columns == new.name), length(new.columns)))

      if (index != "") {
        if (choice.2.A2 == "no") {
          choice.2.A1 <- collectionneur::questioneur("\nProceed to an automatic search for column header correspondence? (yes/no)  ", c("yes", "no"))
          if (choice.2.A1 == "yes" & choice.management == "all") {
            choice.2.A2 <- collectionneur::questioneur("Keep 'yes' and do the same for the other new headers? (yes/no)  ", c("yes", "no"))
          }
        }

        choice.2.A3 = ""

        # Automatic search of corresponding column in database -------
        if (choice.2.A1 == "yes") {

          # Compute string distances
          distances <- stringdist::stringdist(a = new.name, b = colnames(database), method = method)
          # Get indices of the three closest names
          closest.indices <- order(distances)[1:3]
          # Return the three most similar names
          closest.names <- colnames(database)[closest.indices]

          cat(sprintf("\nMost similar column headers to '%s':   %s\n", new.name, paste(paste0("[", seq(closest.names), "] '", closest.names, "'"), collapse = ", ")))
          choice.2.A3 <- collectionneur::questioneur("Enter the index of the relevant column header or (Enter) if none matches:  ", c(seq(closest.names), ""))

          # Correct the names with the correspondences
          if (as.numeric(choice.2.A3) %in% seq(closest.names)) {

            choice.2.A4 <- "initial"
            while(!(choice.2.A4 %in% c("old", "new",  ""))) { # loop until the user chose old, new or skip
              old.name <- closest.names[as.numeric(choice.2.A3)]
              choice.2.A4 <- collectionneur::questioneur(sprintf("Keep (old) '%s' or use (new) '%s' column header - (return) to reselect or (Enter) to quit:  ", old.name, new.name), c("old", "new", "return", ""))
              if (choice.2.A4 == "") { # skip the merging
                cat(sprintf(">>> The 'additions' header '%s' will be considered as a new columns and be added to the 'database'.\n", new.name))
              }else if (choice.2.A4 == "return") { # allows to set new answer
                cat(sprintf("\nMost similar column headers to '%s':   %s\n", new.name, paste(paste0("[", seq(closest.names), "] '", closest.names, "'"), collapse = ", ")))
                choice.2.A3 <- collectionneur::questioneur("Enter the index of the relevant column header or (Enter) if none matches:  ", c(seq(closest.names), ""))
              }
            }

          }
        }

        # Manually provide a column header from the database ------
        if (choice.2.A1 == "no" | choice.2.A3 == "") {
          choice.2.A5 <- collectionneur::questioneur("Do you want to provide a 'database' column header or skip and keep both columns (Enter)?  ", c(colnames(database), ""))

          if (choice.2.A5 %in% colnames(database)) { # if index is provided

            choice.2.A4 <- "initial"
            while(!(choice.2.A4 %in% c("old", "new",  ""))) { # loop until the user chose old, new or skip
              old.name <- choice.2.A5
              choice.2.A4 <- collectionneur::questioneur(sprintf("Keep (old) '%s' or use (new) '%s' column header - (return) to reselect or (Enter) to quit:  ", old.name, new.name), c("old", "new", "return", ""))
              if (choice.2.A4 == "") { # skip the merging
                cat(sprintf(">>> The 'additions' header '%s' will be considered as a new columns and be added to the 'database'.\n", new.name))
              }else if (choice.2.A4 == "return") { # allows to set new answer
                choice.2.A5 <- collectionneur::questioneur("Do you want to provide a 'database' column header or skip and keep both columns (Enter)?  ", c(colnames(database), ""))
              }
            }
          }else if (choice.2.A5 == "") {
            cat(sprintf(">>> The 'additions' header '%s' will be considered as a new columns and be added to the 'database'.\n", new.name))
          }
        }


        # CORRECT COLUMN HEADERS
        ## Keep the old name
        if (choice.2.A4 == "old") {
          colnames(additions)[which(colnames(additions) == new.name)] <- old.name
          final.name <- old.name
          resolve.type <- "old"
          new.columns.corr <- new.columns.corr[!new.columns.corr %in% new.name] # Remove the name from the list of new columns

          cat(sprintf("\n>>> 'Database' header retained -> '%s' was replaced by '%s' in the 'additions'.\n", new.name, old.name))  # message

        }else if (choice.2.A4 == "new") {  ## Use the new name
          colnames(database)[which(colnames(database) == old.name)] <- new.name
          final.name <- new.name
          resolve.type <- "new"
          new.columns.corr <- new.columns.corr[!new.columns.corr %in% new.name] # Remove the name from the list of new columns

          cat(sprintf("\n>>> 'Additions' header retained -> '%s' was replaced by '%s' in the 'database'.\n", old.name, new.name)) # message
        }

        # REPORT
        if (choice.2.A4 != "") {
          # Add entry to report
          report.lines <- c(report.lines, sprintf(" %-*s %-*s %-*s %-*s %-*s \n",
                                                  col.widths[1], choice.management,
                                                  col.widths[2], resolve.type,
                                                  col.widths[3], old.name,
                                                  col.widths[4], new.name,
                                                  col.widths[5], final.name))
        }
      }

      # UPDATE INDEX
      if (choice.management == "all") {
        index <- index + 1
        if (index > length(new.columns)) {
          cat("\n\n>>> All new column headers were reviewed.\n")
        }
      }else if (choice.management == "select") {
        continue.editing <- collectionneur::questioneur("\nDo you want to modify another new header? (yes/no) ", c("yes", "no"))
      }
    }
  }

  # s'il reste encore des nouvelles colonnes ou que l'utilisateur a choisi de les considerer toutes comme des nouvelles colones
  if (choice.1 == "no" | length(new.columns.corr) > 0) {
    # petit message pour indiquer les collones de new.columns.corr seront ajouter comme des nouvelles colonnes dans la base de données.
    if (length(new.columns.corr) != length(new.columns)) {
      base::cat(base::sprintf("\nAfter the corrections, %s new columns remain, and will be added to the 'batabase': %s. \n", length(new.columns.corr), paste(new.columns.corr, collapse=", ")))
    }else if (choice.1 == "no") {
      base::cat("\nThese columns will be added to the 'batabase'.\n")
    }else{
      base::cat(base::sprintf("\nThe following columns will be added to the 'batabase': %s.\n", paste(new.columns.corr, collapse=", ")))
    }

    choice.3 <- collectionneur::questioneur("Where possible, keep their position in 'additions'? (yes/no)  ", c("yes", "no"))

    # Add the new columns filled with NAe
    database[,new.columns.corr] <- NA

    # No re-order of the columns
    if (tolower(choice.3) == "no") {
      cat(sprintf("\nInput: %s  ---   The new columns were added at the end the batabase.\n", choice.3))


    }else if (tolower(choice.3) == "yes") {
      # Create the final column order
      final.order <- unique(c(colnames(additions), colnames(database)))
      # Reorder database columns to match additions' structure
      database <- database[, final.order, drop = FALSE]
      cat("\n>>> The new column header(s) originating from 'additions' were added to 'database'.\n")
    }

    # Add entry to report
    for(new.name in new.columns.corr) {
      report.lines <- c(report.lines, sprintf(" %-*s %-*s %-*s %-*s %-*s \n",
                                              col.widths[1], "addition",
                                              col.widths[2], "new",
                                              col.widths[3], "/-x-/",
                                              col.widths[4], new.name,
                                              col.widths[5], new.name))
    }

  }

  # write the report
  report.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  report.header[3] <- c(sprintf("%s%-*s%s \n", report.header[3], 15, "", paste("End", report.time)))

  # final report
  report <- c(report.header,
              report.lines,
              column.headers.sep,
              sprintf("\n Final 'database' column names:\n\t%s\n", collectionneur::renvoi.a.la.ligne(colnames(database), max.line.length = sum(col.widths))))



  # Save
  report.time <- gsub(" ", "_", gsub(":", "-", report.time))

  if (!missing(database.label)) {
    file.name <- database.label
    file.name.2 <- addition.label
  }else{
    file.name <- "Database_header_update"
    file.name.2 <- "Additions_header_update"
  }

  # save report
  if (isTRUE(save.report)) {
    if (!missing(note)) {
      report.name <- paste(file.name, note, sep = "_")
    }
    report.name <- paste0(report.name, "_report")
    report.name <- paste(report.name, report.time, sep = "_")

    base::writeLines(text = report, con = paste0(save.dir, "/", report.name, ".txt"), sep = "")  # Save report
  }


  # save results
  if (isTRUE(save.updates) && !missing(save.dir)) {
    if (!missing(note)) {
      file.name <- paste(file.name, note, sep = "_")
      file.name.2 <- paste(file.name.2, note, sep = "_")
    }

    file.name <- paste(file.name, report.time, sep = "_")
    file.name.2 <- paste(file.name.2, report.time, sep = "_")

    utils::write.csv(x = database, file = paste0(save.dir, "/", file.name, ".csv"), row.names = FALSE, fileEncoding = fileEncoding, na = na)
    utils::write.csv(x = additions, file = paste0(save.dir, "/", file.name.2, ".csv"), row.names = FALSE, fileEncoding = fileEncoding, na = na)
  }

  if (isTRUE(return.report)) {
    to.return <- list(database, additions, report)
    names(to.return) <- c("database", "additions", "report")
  }else{
    to.return <- list(database, additions)
    names(to.return) <- c("database", "additions")
  }

  return(to.return)
}
