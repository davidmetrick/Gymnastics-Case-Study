# UCSAS 2024 USOPC Data Challenge

This is the USOPC data challenge submission by David Metrick, Siddharth Chandrappa, and Abby Spears, all of Yale University.

# Usage

The data analysis files are meant to be run line-by-line in RStudio. Make sure you set the directory to "Gymnastics-Case-Study" after cloning this repository.

To run our final user interface, go to team_selection_shiny --> app.R


# Files

get.data.r: Reads in raw data from data_2022_2023.csv

prep.data.r: Modifies raw data so names are properly formatted, makes new dataframes for each gender and apparatus

teampick.R: Contains the main algorithm that selects a team for a country for a single simulation

simulationsfast.R: Runs all simulations, assigns value to all top-tier USA combinations for 57 different preference vectors

totsims: The simulation results from when we ran the above files in up-to-down order (these will change if the simulations are run again)

team_selection_shiny: Folder that contains the app interface

app.R: App interface using "shiny" R package. To open, click "Run App" in the right-hand corner of the code window (where the "Run" button usually is) in RStudio.

report.Rmd: Final report of our results

report.pdf: Final report, in PDF format

The remaining files can be readily ignored.

Note that the above files can be run again to create different results; however, it took multiple days for our simulations to run on an M2 Max chip, so we advise using the pre-run results in app.R (which can be run as described above).

To run our simulations again, run the files in the following order (line-by-line may be necessary):

get.data.r --> prep.data.r --> teampick.R --> simulationsfast.R --> app.R