# leha: Legume consumption and hepatobiliary disease

This project investigates the association between substituting legumes
for meats, fish, or poultry and the risk of developing NAFLD and
gallbladder diseases.

# Installing and setting up the project

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `.Rproj` file and
running this command in the console:

``` r
pak::pak()
```

# Steps to select and download the data

The `data-raw/` folder contains the scripts to select, process, and
prepare the data on the RAP to eventually be downloaded.

The steps to take to select the variables you want, create the CSV file
on the RAP, convert it to Parquet format (for faster loading), and
download to your project on RAP. The order is:

1.  Select the variables you want in `data-raw/project-variables.csv`.
2.  Follow the instructions in the `data-raw/create-data.R` script and
    run it to create the CSV file on the RAP server.
3.  Open the `_targets.R` and change the `download_project_data` target
    in `tar_target()` line from `"parquet"` to `"csv"`. Then run
    `targets::tar_make()` to download the CSV file to `data/`. **RESTART
    SESSION**
4.  Open and run the `data-raw/convert-to-parquet.R` script to convert
    the CSV file to the Parquet format.
5.  Go back into `_targets.R` and change the `"csv"` to `"parquet"` (the
    opposite of what was done in item 3). Run `targets::tar_make()` in
    the Console to download the Parquet file and store in the `data/`
    folder.

# Processing and saving progress

It is very timely to rerun all code every time you have made changes in
the data, so you can save and reload your work along the way by
following the layout in "data-raw/processing.r" script. This is also
helpful to do, when you have completed all data management tasks and
want to save your changes to have a "ready to go" data frame to run your
analysis on.

After having completed the data management and running the processing.R
script, you can start doing descriptive and comparative analyses of your
data.

# Brief description of folder and file contents

The following folders contain:

-   `data/`: Will contain the UK Biobank data (not saved to Git) as well
    as the intermediate results output files.

-   `data-raw/`: Contains the R script to download the data, as well as
    the CSV files that contain the project variables and the variable
    list as named in the RAP.

-   `doc/`: This file contains the R Markdown, Word, or other types of
    documents with written content, like the manuscript and protocol.

-   `R/`: Contains the R scripts and functions to create the figures,
    tables, and results for the project.
