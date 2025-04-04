
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collectionneur <a href="https://doi.org/10.5281/zenodo.15146959"><img src="man/figures/collectionneur_logo_ver1_small_300dpi.png" align="right" height="138" /></a>

<!-- badges: start -->

![GitHub
version](https://img.shields.io/github/r-package/v/tchalauxclergue/collectionneur?logo=github)
![GitHub Release
Date](https://img.shields.io/github/release-date/tchalauxclergue/collectionneur?color=blue)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.151469598.svg)](https://doi.org/10.5281/zenodo.15146959)
[![GitHub Downloads (all assets, all
releases)](https://img.shields.io/github/downloads/tchalauxclergue/collectionneur/total?style=flat)
![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
<!-- badges: end -->

## Overview

`collectionneur` package is designed to help researchers and data
managers maintain an up-to-date and well-organized database. It
facilitates the comparison and integration of new data entries into an
existing database while keeping a detailed report of all modifications.
All database formats are allowed, and we also propose a unified template
for databases, initially designed for sediment source fingerprinting
databases, on [Zenodo](https://doi.org/10.5281/zenodo.10725788)
(Chalaux-Clergue et al., 2024,
<https://doi.org/10.5281/zenodo.10725788>).

Features:

- Identifies and adds new samples to a database.
- Identifies and updates existing samples based on unique sample
  identifiers.
- Identifies and automatically compares the new columns with the
  existing database.
- Provides a structured and dated report of all modifications made to
  the database.
- Supports any database format and customizable settings for handling
  missing values.

The `collectionneur` package is available in this
[Github](https://github.com/tchalauxclergue/collectionneur) repository
and is also archived on
[Zenodo](https://doi.org/10.5281/zenodo.15146959).

### Table of content

<!-- toc -->

- [Installation](#installation)
- [Usage](#usage)
  - [Updating a database](#updating-a-database)
  - [Report generation](#report-generation)
- [Getting help](#getting-help)
- [Citation](#citation)

<!-- tocstop -->

## Installation

``` r
#install.packages(devtools)
library(devtools)

# Install the latest version from GitHub
devtools::install_github("https://github.com/tchalauxclergue/collectionneur/releases/tag/1.0.0", ref = "master", force = T)

# Alternatively, from the downloaded .tar.gz file
devtools::install_local("path_to_file/collectionneur_1.0.0.tar.gz", repos = NULL) # 'path_to_file' should be modified accordingly to your working environment
```

## Usage

A database (`database.csv`) and a set of new samples (
new.samples.csv\`) are provided to users to test the package

``` r
library(collectionneur)

# Get the dir to data and metadata files within the R package
database.dr <- system.file("extdata", "database.csv", package = "collectionneur")
additions.dr <- system.file("extdata", "new.samples.csv", package = "collectionneur")
```

### Updating a database

The main function of the package, `archiviste`, interacts with the user
to handle updates to a database by:

1.  Virtual loading of the existing database and the new data. No
    necessity to pre-load the database in the working environment.
2.  Identifying new columns and updating the database structure if
    necessary.
3.  Comparing samples based on user-defined identifiers.
4.  Updating existing samples and appending new ones.
5.  Saving an updated version on the database and generating a report
    documenting all modifications to keep track of the database updates.

Once the function is running, the user is required to take decisions via
the console.

``` r
library(collectionneur)

collectionneur::archiviste(database = "path_to_file/database.csv",     # Path to database CSV file
                           additions = "path_to_file/new.samples.csv", # Path to database CSV file
                           sample.ids = c("IGSN", "Sample_name"),      # The column names that uniquely identify samples
                           save.dir = "save_dir/example",              # Path to database update folder
                           database.label = "example",                 # Your database label
                           note = "v2",                                # The update index of the database
                           method = "jw",                              # Matching method (default: "jw" for Jaro-Winkler).
                           sep = ";",                                  # Field separator for CSV files
                           dec = ".",                                  # Decimal separator for CSV files
                           na.strings = "",                            # Strings to be treated as 'NA'
                           fileEncoding = "latin1",                    # The encoding to be used on a file
                           na = ""                                     # Missing values in the data
                           )
```

### Report generation

After running `archiviste`, a detailed report is saved in the specified
directory (`save.dir`). This report includes:

- A summary of the update process.
- Identified new columns and modified columns.
- List of modified and newly added samples.
- A time-stamped log of all changes.

The updated version of the database and the report are time-stamped to
ensure that database versions do not conflict. This ensures full
traceability of database modifications.

## Getting help

If you encounter a clear bug, please file and issue or send an email to
[Thomas Chalaux-Clergue](mailto:thomaschalaux@icloud.com).

## Citation

To cite this packages:

``` r
utils::citation(package = "collectionneur")
#> To cite the 'collectionneur' package in publications please use:
#> 
#>   Chalaux-Clergue, T. (2025). collectionneur: A user-friendly package
#>   designed to manage internal databases efficently. , Zenodo [Package]:
#>   https://doi.org/10.5281/zenodo.15146959, Github [Package]:
#>   https://github.com/tchalauxclergue/collectionneur, Version = 1.0.0.
#> 
#> Une entrée BibTeX pour les utilisateurs LaTeX est
#> 
#>   @Manual{,
#>     title = {collectionneur: A user-friendly package designed to manage internal databases efficently.  },
#>     author = {{Chalaux-Clergue} and {Thomas}},
#>     year = {2025},
#>     month = {4},
#>     note = {R package version 1.0.0},
#>     doi = {https://doi.org/10.5281/zenodo.15146959},
#>     url = {https://github.com/tchalauxclergue/collectionneur},
#>   }
```
