#' Le Conversateur: Ensures that sample information is revised when necessary
#'
#' This function updates a database by resolving conflicts between existing samples and amended samples.
#' It allows users to decide whether to keep old values, replace them with new values, or apply decisions globally.
#'
#' @param database Data frame. The original database containing sample information.
#' @param additions Data frame. New samples or updates of the existing database
#' @param sample.ids List or Character. The name(s) of the column(s) containing sample identifiers.
#' @param save.report Logical. If `TRUE`, a report summarizing the updates will be saved. Default is `TRUE`.
#' @param return.report Logical. If `TRUE`, the report is included in the function output. Default is `TRUE`.
#' @param save.dir Character. Optional directory path for saving the report. Required if `save.report = TRUE`.
#' @param database.label Character. Label for the database, used for naming output files.
#' @param note Character. Optional additional note to append to the file and report file name.
#' @param na Character string, used for missing values in the data.= (default - "").
#' @param fileEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written, "latin1" (default).
#'
#' @return A dataframe containing the updated database, and if the report is not save - a character vector of the report contents.
#'
#' @details
#' The function iterates through samples that exist in both `database` and `amended.samples` and compares column values.
#' If discrepancies are found, the user is prompted to choose whether to keep the old value, replace it with the new value,
#' or apply the decision to all similar cases.
#'
#' A detailed report is generated, documenting the changes made.
#'
#' @examples
#' \dontrun{
#' updated.database <- conservateur(
#'   database = original.db,
#'   amended.samples = updated.samples,
#'   common.samples = intersect(original.db$Sample_name, updated.samples$Sample_name),
#'   sample.id = "Sample_name",
#'   save.report = TRUE,
#'   return.report = TRUE,
#'   save.dir = "./reports",
#'   database.label = "study_database",
#'   note = "revision1")
#' }
#'
#' @author Thomas Chalaux-Clergue
#' @export
#'
conservateur <- function(database, additions, sample.ids, save.updates = TRUE, save.report = TRUE, return.report = FALSE, save.dir, database.label, note, na = "", fileEncoding = "latin1"){

  require(utils)
  require(base)

  # Create unique row identifiers based on sample.ids
  keys.database <- do.call(paste, c(database[,sample.ids], sep = " - "))
  keys.additions <- do.call(paste, c(additions[,sample.ids], sep = " - "))
  # Find common identifiers
  common.samples <- intersect(keys.database, keys.additions)

  if(length(sample.ids) > 1){
    cat(sprintf("\nAccording to the identifiers (%s), sample(s) pre-existing in the database were found in the additions:\n\t%s\n", paste(sample.ids, collapse=", "), paste("'", common.samples, "'", collapse=", ", sep=""))) # Inform the user
  }else if(length(sample.ids == 1)){
    cat(sprintf("\nAccording to the identifier (%s), sample(s) pre-existing in the database were found in the additions:\n\t%s\n", paste(sample.ids, collapse=", "), paste("'", common.samples, "'", collapse=", ", sep=""))) # Inform the user
  }
  cat("\nIf differences are observed between the two data sets, the user will be asked to take action.\n\n")

  # Ask user whether to resolve all conflicts the same way
  resolve.all <- NULL  # NULL means ask per case
  resolve.each <- NULL  # NULL means ask per case
  amended.entries <- list()
  replacement.status <- "Each"


  # Report front head
  col.widths <- c(15, 10, 15*length(sample.ids)+4, 30, 25, 25, 25) # Define col width

  report.header <- c(sprintf("%-*s------ Database Update Report ------%-*s\n\n", 58, "", 57, ""),
                     sprintf("%-*s------------------------------------%-*s\n", 58, "", 57, ""),
                     sprintf(" Start %s%-*s collectionneur::conservateur %-*s", paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Sys.timezone()), 23, "", 23, ""),
                     sprintf("%-*s------------------------------------%-*s\n\n", 58, "", 57, ""),
                     sprintf(" Sample identifier(s): %s \n\n", paste(sample.ids, collapse = ", ")),
                     sprintf(" %-*s %-*s %-*s %-*s %-*s %-*s %-*s\n",
                             col.widths[1], "Replacement",
                             col.widths[2], "Resolve",
                             col.widths[3], "Sample",
                             col.widths[4], "Column",
                             col.widths[5], "Old value",
                             col.widths[6], "New value",
                             col.widths[7], "Final value"))

  column.headers.sep <- gsub("\\w", "-", report.header[length(report.header)])

  report.header <- c(report.header, column.headers.sep)

  report.lines <- c()

  # Filter additions samples that already exists in the database
  amended.samples <- additions[keys.additions %in% common.samples, ]

  # For each sample in common
  # sample <- "FNL_0037 - 10.58052/IETGC000X"
  for(sample in common.samples){

    # Reset if it was chose to apply the same choice for one sample
    if(!is.null(resolve.all) && resolve.all == "no"){
      resolve.each <- NULL  # NULL means ask per case
      replacement.status <- "Each"
    }

    amended.colums <- c() # list of conflicting columns for a sample

    # For each column in the database
    column <- "Country"
    for(column in colnames(database)){
      if(column %in% sample.ids) next  # Skip the 'sample.ids' columns
      if(!(column %in% colnames(amended.samples))) next # Skip columns that do not exist in the addition

      ## Get values and compare them
      old.value <- database[Reduce(`&`, Map(function(col, value) database[[col]] == value, sample.ids, unlist(strsplit(sample, split = " - ")))), column]
      new.value <- amended.samples[Reduce(`&`, Map(function(col, value) amended.samples[[col]] == value, sample.ids, unlist(strsplit(sample, split = " - ")))), column]


      if(identical(old.value, new.value)) next # Skip if the values are identical

      amended.colums <- append(amended.colums, column) # list conflicting columns

      # Display differences if each sample is analysed
      if(is.null(resolve.each) || is.null(resolve.all)){
        cat(sprintf("\nConflict detected for sample: '%s' - Column: '%s'", sample, column))
        cat(sprintf("\n\tCurrent value: %s", old.value))
        cat(sprintf("\n\tNew value: %s\n", new.value))
      }

      # Choose data management strategy
      if(is.null(resolve.each)){

        choice <- collectionneur::questioneur("\nKeep (old) value, replace with (new) value, or apply an unique choice to (all) the columns of this sample? ", c("old", "new", "all"))

      }else{
        choice <- resolve.each
      }

      # Apply management strategy for ALL columns of THIS sample
      if(tolower(choice) == "all"){

        resolve.each <- collectionneur::questioneur("Apply to keep (old) or (new) for all conflict columns of this sample? ", c("old", "new"))

        choice <- resolve.each

        # Apply management strategy for ALL samples
        resolve.all <- collectionneur::questioneur("Apply this choice to all samples? (yes/no) ", c("yes", "no"))

        if(tolower(resolve.all) == "yes"){replacement.status <- "All"} # REPORT
        if(tolower(resolve.all) == "no"){replacement.status <- "Sample"} # REPORT

      }

      resolve.type <- choice

      if(tolower(choice) == "old"){

        final.value <- old.value

        if(is.null(resolve.each)){ # keep it silent if the choice is applied to all columns
          cat(sprintf("\nThe 'database' value has been kept:   %s - %s: %s\n", sample, column, final.value)) # notification for single sample
        }

        # Replace with the newer value from addition
      }else if(tolower(choice) == "new"){

        database[Reduce(`&`, Map(function(col, value) database[[col]] == value, sample.ids, unlist(strsplit(sample, split = " - ")))), column] <- new.value

        final.value <- new.value # REPORT

        if(is.null(resolve.each)){ # Keep it silent if the choice is applied to all columns
          cat(sprintf("\nThe value has been replaced:   %s - '%s': '%s'\n", sample, column, final.value)) # notification for single sample
        }
      }

      # Add entry to report
      if(grepl("[^a-zA-Z0-9_.-]", column)){
        report.lines <- c(report.lines, sprintf(" %-*s %-*s %-*s %-*s %-*s %-*s %-*s\n",
                                              col.widths[1], replacement.status,
                                              col.widths[2], resolve.type,
                                              col.widths[3], sample,
                                              col.widths[4], column,
                                              col.widths[5], paste0(" ", old.value),
                                              col.widths[6], paste0(" ", new.value),
                                              col.widths[7], paste0(" ", final.value)))
      }else{
        report.lines <- c(report.lines, sprintf(" %-*s %-*s %-*s %-*s %-*s %-*s %-*s\n",
                                                col.widths[1], replacement.status,
                                                col.widths[2], resolve.type,
                                                col.widths[3], sample,
                                                col.widths[4], column,
                                                col.widths[5], old.value,
                                                col.widths[6], new.value,
                                                col.widths[7], final.value))
      }
    }
    amended.entries[[sample]] <- list(amended.colums)
    report.lines <- c(report.lines, "\n")
  }

  if(!is.null(resolve.each) && choice == "new"){
    return.sample.cols <- unname(sapply(names(amended.entries), function(sample) {paste0(sample, " (", paste(unlist(amended.entries[[sample]]), collapse = ", "), ")")}))
    cat(sprintf("\n\n\nAll conflicting sample values have been modified accordingly:\n %s \n", paste("   -", return.sample.cols, collapse=";\n ")))
  }

  # write the report
  report.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  report.header[3] <- c(sprintf("%s%-*s%s \n", report.header[3], 15, "", paste("End", report.time)))


  report <- c(report.header, report.lines, column.headers.sep)

  # Save
  report.time <- gsub(" ", "_", gsub(":", "-", report.time))


  if(!missing(database.label)){
    file.name <- database.label
  }else{
    file.name <- "Database_content_update"
  }

  # save report
  if(isTRUE(save.report)){
    if(!missing(note)){
      report.name <- paste(file.name, note, sep = "_")
    }
    report.name <- paste0(report.name, "_report")
    report.name <- paste(report.name, report.time, sep = "_")

    base::writeLines(text = report, con = paste0(save.dir, "/", report.name, ".txt"), sep = "")  # Save report
  }


  # save results
  if(isTRUE(save.updates) && !missing(save.dir)){
    if(!missing(note)){
      file.name <- paste(file.name, note, sep = "_")
    }

    file.name <- paste(file.name, report.time, sep="_")

    utils::write.csv(x = database, file = paste0(save.dir, "/", file.name, ".csv"), row.names = FALSE, fileEncoding = fileEncoding, na = na)
  }

  if(isTRUE(return.report)){
    to.return <- list(database, report)
    names(to.return) <- c("database", "report")
  }else{
    to.return <- database
  }

  return(to.return)
}


