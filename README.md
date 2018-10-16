# MSDS6306-CS1
SMU Data Science - MSDS6306 Doing Data Science - Case Study 1 Subgroup

# Beers and Breweries!

### Introduction
In the course of being adults many, if not all of us have discovered Beer and the positives and negatives associated with said beverage.
We have obtained data of all the beers and all the breweries in America and wanted to find out more regarding each beer's Alcohol by 
Volume (ABV) and International Bittering Units (IBU) as well as much much more.

### Data Files
Located in the /Data/ directory are 2 data files related to beer.  Breweries.csv contains details about each of the individual breweries included in this analysis. The Beers.csv file contains details about indivual beers from the breweries located in the first file.  The 2 files can be joined based on a shared identifier that is an ID for each of the breweries, though they are named differently in each file ("brewery_id" in the Beers.csv file and "brew_id" in the Breweries.csv file.)

### Prerequisites
You are going to want to make sure you have downloaded the datafiles and the libraries we used in the course of our research which I am leaving below.  Of course you will have to use the install.packages() function if you do not have the packages listed below already installed.

```
#Libraries
library(data.table)
library(knitr)
libary(ggplot2)
library(reshape2)
```
# The Report
With the libraries above installed and the two csv files available in the /Data folder, the combined-casestudy.* files contain the details of the report.  combined-casestudy.rmd contains the Rmarkdown code to generate the report, combined-casestudy.md contains the markdown code generated by the previous file, and combined-casestudy.html is the final report in HTML format.

Additionally, a Powerpoint presentation named Beer_CaseStudy.pptx contains a summary presentation of the analysis.

# The Results
The results we have are presented in an Rmarkdown file that when ran, outputs the answers to 6 questions we were given as well as 
the conclusion we have regarding our research into beers around the country.  We hope you enjoy.


### Credit
John Rodgers  
Adam Scheerer  
Sara Zaheri  
