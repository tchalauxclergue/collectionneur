#' Renvoi à la ligne: Format Column Headers for Readability
#'
#' This function takes a vector of column headers and formats them into a string that automatically wraps onto a new line
#' when it reaches a specified character length. This ensures that column names are not split and each is followed by a comma,
#' making the output suitable for direct use in R scripts.
#'
#' @param columns A character vector of column headers.
#' @param max.line.length An integer specifying the maximum number of characters per line (default: 145).
#'
#' @return A formatted string containing column headers, wrapped appropriately.
#'
#' @examples
#' column.names <- c("Layer_thickness_cm", "Depth_min_cm", "Depth_max_cm", "Current_archive",
#'                   "Current_archive_quantity_g", "Current_archive_quantity_last_update", "Note", "Ca_mg.kg")
#' cat(renvoi.a.la.ligne(column.names))
#'
#' @author Thomas Chalaux-Clergue
#' @export
#'
renvoi.a.la.ligne <- function(columns, max.line.length = 145) {

  formatted.lines <- c()
  current.line <- "c("

  for(col in columns){
    if(col != columns[length(columns)]){
      col.with.comma <- paste0("\"", col, "\", ")
    }else{
      col.with.comma <- paste0("\"", col, "\"")
    }

    # Check if adding the next column exceeds max.line.length
    if(nchar(current.line) + nchar(col.with.comma) > max.line.length){
      # Store the current line and start a new one
      formatted.lines <- c(formatted.lines, current.line)
      current.line <- paste0(" ", col.with.comma)
    } else {
      # Append to the current line
      current.line <- paste0(current.line, col.with.comma)
    }
  }

  # Add the last line if it's not empty
  if(nchar(current.line) > 0){
    formatted.lines <- c(formatted.lines, current.line)
  }

  formatted.lines[length(formatted.lines)] <- paste0(formatted.lines[length(formatted.lines)], ")")

  final.line <- paste(paste(formatted.lines, "\n "), collapse = "")

  # cat(final.line)

  return(final.line)
}
