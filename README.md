
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Projet Collectionneur <a href="https://doi.org/10.5281/zenodo.15146959"><img src="man/figures/collectionneur_logo_ver1_small_300dpi.png" align="right" height="138" /></a>

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
([Chalaux-Clergue et al., 2024 -
https://doi.org/10.5281/zenodo.10725788](https://doi.org/10.5281/zenodo.10725788)).

Features:

- Identifies and adds new samples to a database.
- Identifies and updates existing samples based on unique sample
  identifiers.
- Identifies and automatically compares the new columns with the
  existing database.
- Provides a structured and dated report of all modifications made to
  the database.
- Supports any database format and customisation settings for handling
  missing values.

The `collectionneur` package is available in this
[Github](https://github.com/tchalauxclergue/collectionneur) repository
and is also archived on
[Zenodo](https://doi.org/10.5281/zenodo.15146959).

<details>

<summary>

<strong>Table of Contents</strong>
</summary>

<!-- toc -->

- [Installation](#installation)
- [Usage](#usage)
  - [Updating a database](#updating-a-database)
  - [Report generation](#report-generation)
- [Getting help](#getting-help)
- [Citation](#citation)

<!-- tocstop -->

</details>

## Installation

``` r
#install.packages(devtools)
library(devtools)

# Install the latest version from GitHub
devtools::install_github("https://github.com/tchalauxclergue/collectionneur/releases/tag/1.2.0", ref = "master", force = TRUE)

# Alternatively, from the downloaded .tar.gz file
devtools::install_local("path_to_file/collectionneur_1.2.0.tar.gz", repos = NULL) # 'path_to_file' should be modified accordingly to your working environment
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

collectionneur::archiviste(database = "path_to_file/database.csv",     # Path to database CSV file - use 'database.dr' to test
                           additions = "path_to_file/new.samples.csv", # Path to database CSV file - use 'additions.dr' to test
                           sample.ids = c("IGSN", "Sample_name"),      # The column names that uniquely identify samples
                           save.dir = "save_dir/example",              # Path to database update folder
                           database.label = "example",                 # Your database label
                           note = "v2",                                # The update index of the database
                           method = "jw",                              # Matching method (default: "jw" for Jaro-Winkler).
                           read.sep = ";",                             # Field separator for CSV files reading
                           read.dec = ".",                             # Decimal separator for CSV files reading
                           na.strings = "",                            # Strings to be treated as 'NA'
                           fileEncoding = "latin1",                    # The encoding to be used on a file
                           save.sep = ";",                             # Field separator for CSV files saving
                           save.dec = ".",                             # Decimal separator for CSV files saving
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

                                                              ------ Database Update Report ------                                                         

                                                              ------------------------------------                                                         
     Start 2025-03-31 15:41:23 Europe/Paris                        collectionneur::archiviste                                       End 2025-03-31 15:45:47 
                                                              ------------------------------------                                                         




                                                              ------------------------------------                                                         
     Start 2025-03-31 15:41:23 Europe/Paris                      collectionneur::bibliothecaire                                       End 2025-03-31 15:44:11 
                                                              ------------------------------------                                                         

     New column headers found in 'additions':
        c("EDRXF_Al_mg.kg.1", "EDXRF_Ca_mg.kg.1", "EDXRF_Si_mg.kg.1", "Layer_thickness_cm", "Depth_min_cm", "Depth_max_cm", "Current_archive",  
      "Current_archive_quantity_g", "Current_archive_quantity_last_update", "Note") 
     
     Replacement     Resolve    Database header                          Additions header                         Final header                            
     -----------     -------    -------- ------                          --------- ------                         ----- ------                            
     select          new        Al_mg.kg                                 EDRXF_Al_mg.kg.1                         EDRXF_Al_mg.kg.1                         
     select          new        Ca_mg.kg                                 EDXRF_Ca_mg.kg.1                         EDXRF_Ca_mg.kg.1                         
     addition        new        /-x-/                                    EDXRF_Si_mg.kg.1                         EDXRF_Si_mg.kg.1                         
     addition        new        /-x-/                                    Layer_thickness_cm                       Layer_thickness_cm                       
     addition        new        /-x-/                                    Depth_min_cm                             Depth_min_cm                             
     addition        new        /-x-/                                    Depth_max_cm                             Depth_max_cm                             
     addition        new        /-x-/                                    Current_archive                          Current_archive                          
     addition        new        /-x-/                                    Current_archive_quantity_g               Current_archive_quantity_g               
     addition        new        /-x-/                                    Current_archive_quantity_last_update     Current_archive_quantity_last_update     
     addition        new        /-x-/                                    Note                                     Note                                     
     -----------     -------    -------- ------                          --------- ------                         ----- ------                            

     Final 'database' column names:
        c("IGSN", "Parent_IGSN", "Sample_name", "Sample_name_old", "Waypoint", "Waypoint_old", "Geoloc_name", "Material", "Nature", "Class",  
      "Class_decontamination", "Year_of_remediation", "X_WGS84", "Y_WGS84", "Z_elevation_m", "Country", "Region", "State.Province.Prefecture",  
      "City.Township", "Catchment", "Catchment_old", "River", "Lithology", "Lithology_simp", "Pedology_SoilGroup_code", "Pedology_SG_CSCS_2011",  
      "Pedology_SG_WRG_2006", "Dose_sed_µSv.h", "Dose_soil_µSv.h", "Dose_ratio_SedSoil", "EDRXF_Al_mg.kg.1", "EDXRF_Ca_mg.kg.1", "EDXRF_Si_mg.kg.1",  
      "Campaign", "Field_program", "Collection_method", "Sampling_depth_cm", "Layer_thickness_cm", "Depth_min_cm", "Depth_max_cm", "Sampling_date",  
      "Sampling_time", "Collector", "Collector_ORCID", "Collector_address", "Current_archive", "Current_archive_quantity_g",  
      "Current_archive_quantity_last_update", "Note") 
     



                                                              ------------------------------------                                                         
     Start 2025-03-31 15:44:11 Europe/Paris                        collectionneur::conservateur                                       End 2025-03-31 15:45:47 
                                                              ------------------------------------                                                         

     Sample identifier(s): IGSN, Sample_name 

     Replacement     Resolve    Sample                             Column                         Old value                 New value                 Final value              
     -----------     -------    ------                             ------                         --- -----                 --- -----                 ----- -----              
     Each            new        10.58052/IETGC0004 - FAL_0012      Parent_IGSN                    NA                        NA                        NA                       
     Sample          new        10.58052/IETGC0004 - FAL_0012      Dose_soil_µSv.h                NA                        NA                        NA                      
     Sample          new        10.58052/IETGC0004 - FAL_0012      EDXRF_Ca_mg.kg.1               NA                        NA                        NA                       
     Sample          new        10.58052/IETGC0004 - FAL_0012      EDXRF_Si_mg.kg.1               NA                        NA                        NA                       
     Sample          new        10.58052/IETGC0004 - FAL_0012      Layer_thickness_cm             NA                        NA                        NA                       
     Sample          new        10.58052/IETGC0004 - FAL_0012      Depth_min_cm                   NA                        NA                        NA                       
     Sample          new        10.58052/IETGC0004 - FAL_0012      Depth_max_cm                   NA                        NA                        NA                       
     Sample          new        10.58052/IETGC0004 - FAL_0012      Current_archive                NA                        NA                        NA                       

     Sample          new        10.58052/IETGC000K - FAS_0025_A    Dose_soil_µSv.h                NA                        NA                        NA                      
     Sample          new        10.58052/IETGC000K - FAS_0025_A    EDRXF_Al_mg.kg.1               26.95                     8.2                       8.2                      

     All             new        10.58052/IETGC000X - FNL_0037      Region                         Miyagi                    Tohoku                    Tohoku                   
     All             new        10.58052/IETGC000X - FNL_0037      State.Province.Prefecture      NA                        Fukushima                 Fukushima                
     All             new        10.58052/IETGC000X - FNL_0037      City.Township                  NA                        Iitate mura               Iitate mura              
     All             new        10.58052/IETGC000X - FNL_0037      Catchment                      NA                        Niida                     Niida                    
     All             new        10.58052/IETGC000X - FNL_0037      Catchment_old                  NA                        Abukuma                   Abukuma                  
     All             new        10.58052/IETGC000X - FNL_0037      River                          NA                        Niida                     Niida                    
     All             new        10.58052/IETGC000X - FNL_0037      Dose_soil_µSv.h                NA                        NA                        NA                      

     All             new        10.58052/IETGC000Y - FNS_0038      Dose_soil_µSv.h                NA                        NA                        NA                      
     All             new        10.58052/IETGC000Y - FNS_0038      EDRXF_Al_mg.kg.1               46.9                      8.2                       8.2                      

     -----------     -------    ------                             ------                         --- -----                 --- -----                 ----- -----              

## Getting help

If you encounter a clear bug, have a question or suggestion, please
either open an
[Issues](https://github.com/tchalauxclergue/collectionneur/issues) or
send an email to [Thomas Chalaux-Clergue and Amaury Bardelle
(thomaschalaux@icloud.com,
amaury.bardelle@icloud.com)](mailto:thomaschalaux@icloud.com,%20amaury.bardelle@icloud.com).

## Citation

To cite this packages:

``` r
utils::citation(package = "collectionneur")
#> To cite the 'collectionneur' package in publications please use:
#> 
#>   Chalaux-Clergue & Bardelle (2025). collectionneur: A user-friendly
#>   package designed to manage internal databases efficently. , Zenodo
#>   [Package]: https://doi.org/10.5281/zenodo.15146959, Github [Package]:
#>   https://github.com/tchalauxclergue/collectionneur, Version = 1.2.0.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {collectionneur: A user-friendly package designed to manage internal databases efficently.},
#>     author = {{Chalaux-Clergue} and {Thomas} and {Amaury} and {Bardelle}},
#>     year = {2025},
#>     month = {7},
#>     note = {R package version 1.2.0},
#>     doi = {https://doi.org/10.5281/zenodo.15146959},
#>     url = {https://github.com/tchalauxclergue/collectionneur},
#>   }
```
