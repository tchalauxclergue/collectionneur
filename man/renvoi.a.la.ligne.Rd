% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renvoi_a_la_ligne.R
\name{renvoi.a.la.ligne}
\alias{renvoi.a.la.ligne}
\title{Renvoi à la ligne: Format Column Headers for Readability}
\usage{
renvoi.a.la.ligne(columns, max.line.length = 145)
}
\arguments{
\item{columns}{A character vector of column headers.}

\item{max.line.length}{An integer specifying the maximum number of characters per line (default: 145).}
}
\value{
A formatted string containing column headers, wrapped appropriately.
}
\description{
This function takes a vector of column headers and formats them into a string that automatically wraps onto a new line
when it reaches a specified character length. This ensures that column names are not split and each is followed by a comma,
making the output suitable for direct use in R scripts.
}
\examples{
column.names <- c("Layer_thickness_cm", "Depth_min_cm", "Depth_max_cm", "Current_archive",
                  "Current_archive_quantity_g", "Current_archive_quantity_last_update", "Note", "Ca_mg.kg")
cat(renvoi.a.la.ligne(column.names))

}
\author{
Thomas Chalaux-Clergue
}
