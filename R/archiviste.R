#' L'Archiviste : A user-friendly database management.
#'
#' This function updates an existing database with new additions, ensuring consistency in sample identifiers and columns.
#' It also generates a report detailing the updates made.
#'
#' @param database A data frame or a file path (character) to the existing database.
#' @param additions A data frame or a file path (character) to the additions.
#' @param sample.ids A character vector specifying the column names that uniquely identify samples.
#' @param method A character string specifying the matching method (default: `"jw"` for Jaro-Winkler).
#' @param save.updates Logical; if `TRUE`, saves the updated database (default: `TRUE`).
#' @param save.step Logical; if `TRUE`, saves intermediate update steps (default: `FALSE`).
#' @param save.report Logical; if `TRUE`, saves a report of updates made (default: `TRUE`).
#' @param return.report Logical; if `TRUE`, returns both the updated database and the report (default: `FALSE`).
#' @param save.dir Character. Optional directory path for saving the report. Required if `save.report = TRUE`.
#' @param database.label Character. Label for the database, used for naming output files.
#' @param note Character. Optional additional note to append to the file and report file name.
#' @param read.sep A character specifying the field separator to read CSV files (default: `","`).
#' @param read.dec A character specifying the decimal separator to read CSV files (default: `"."`).
#' @param read.sep A character specifying the field separator to save database CSV files (default: `","`).
#' @param read.dec A character specifying the decimal separator to save database CSV files (default: `"."`).
#' @param na.strings A character vector specifying strings to be treated as `NA` (default: `""`).
#' @param na Character string, used for missing values in the data.= (default - "").
#' @param fileEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written, "latin1" (default).
#'
#' @return If `return.report = TRUE`, returns a list with the updated database and a report. Otherwise, returns only the updated database.
#'
#'
#' @examples
#' \dontrun{
#' updated.db <- archiviste(
#'   database = "path.to/database.csv",
#'   additions = "path.to/additions.csv",
#'   sample.ids = c("Sample_name", "IGSN"),
#'   save.updates = TRUE,
#'   save.report = TRUE,
#'   save.dir = "output/directory"
#' )
#' }
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
archiviste <- function(database, additions, sample.ids, method = "jw", save.updates = TRUE, save.step = FALSE, save.report = TRUE, return.report = FALSE, save.dir, database.label, note, read.sep = ",", read.dec = ".", na.strings = "", save.sep = ",", save.dec = ".", na = "", fileEncoding = "latin1"){

  require(utils)
  require(dplyr)

  # Databases loading
  ## if a path is given for the database - load it
  if (class(database) == "character") {
    path.database <- database
    database <- collectionneur::receptioniste(file = path.database, sep = read.sep, dec = read.dec, fileEncoding = fileEncoding, na.strings = na.strings, db.type = "Database")
  }
  ## if a path is given for the additions - load it
  if (class(additions) == "character") {
    path.additions <- additions
    additions <- collectionneur::receptioniste(file = path.additions, sep = read.sep, dec = read.dec, fileEncoding = fileEncoding, na.strings = na.strings, db.type = "Additions")
  }

  # Ensure the ID column exists in both data frames
  if ( !(all(sample.ids %in% colnames(database))) || !(all(sample.ids %in% colnames(additions))) ) {
    stop(cat(sprintf("Not all 'sample.ids' (%s) not found in both dataframes.", paste(sample.ids, collapse = ", ") )))
  }

  # Report front head
  col.widths <- c(15, 10, 40, 40, 40) # Define col width

  report.header <- c(sprintf("%-*s------ Database Update Report ------%-*s\n\n", 58, "", 57, ""),
                     sprintf("%-*s------------------------------------%-*s\n", 58, "", 57, ""),
                     sprintf(" Start %s%-*s collectionneur::archiviste %-*s", paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Sys.timezone()), 23, "", 23, ""),
                     sprintf("%-*s------------------------------------%-*s\n\n\n\n\n", 58, "", 57, "")
                     )

  report.lines <- c()

  # Identify new columns
  if (length(setdiff(colnames(additions), colnames(database))) != 0) {
    # identify new columns in the additions that do not exist in the database
    results.bibliothecaire <- collectionneur::bibliothecaire(database = database, additions = additions, method = method, save.updates = save.step, save.report = FALSE, return.report = TRUE)

    database <- results.bibliothecaire$database
    additions <- results.bibliothecaire$additions
    report.lines <- c(report.lines, results.bibliothecaire$report[2:length(results.bibliothecaire$report)], "\n\n\n") # REPORT
  }


  # Identify common samples
  keys.database <- do.call(paste, c(database[,sample.ids], sep = " - "))
  keys.additions <- do.call(paste, c(additions[,sample.ids], sep = " - "))
  # Find common identifiers
  common.samples <- intersect(keys.database, keys.additions)

  if (!is.null(common.samples)) {

    results.conservateur <- collectionneur::conservateur(database = database, additions = additions, sample.ids = sample.ids, save.updates = save.step, save.report = FALSE, return.report = TRUE, fileEncoding = fileEncoding, na = na)

    database <- results.conservateur$database # update the database
    report.lines <- c(report.lines,  results.conservateur$report[2:length(results.conservateur$report)]) # REPORT

    additions <- additions[!(keys.additions %in% common.samples), ] # remove the common samples
  }

  # add all new samples to the database
  database <- base::rbind(database, additions)


  # write the report
  report.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  report.header[3] <- c(sprintf("%s%-*s%s \n", report.header[3], 15, "", paste("End", report.time)))

  # final report
  report <- c(report.header,
              report.lines)


  cat("\n\n\n>>> The 'database' was uploaded sucessfully from 'additions' data.\n")

  # Save
  report.time <- gsub(" ", "_", gsub(":", "-", report.time))


  if (!missing(database.label)) {
    file.name <- database.label
  }else{
    file.name <- "Database_update"
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
    }

    file.name <- paste(file.name, report.time, sep = "_")

    utils::write.table(x = database, file = paste0(save.dir, "/", file.name, ".csv"), row.names = FALSE, sep = save.sep, dec = save.dec, fileEncoding = fileEncoding, na = na)
  }

  if (isTRUE(return.report)) {
    to.return <- list(database, report)
    names(to.return) <- c("database", "report")
  }else{
    to.return <- database
  }

  return(to.return)
}
