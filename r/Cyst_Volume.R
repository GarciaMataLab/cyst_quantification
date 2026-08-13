# Script name: Cyst_Volume.R
# Filters and compiles quantification data from "NucleiTiff_CystVolume.ijm" Fiji/ImageJ macro
# Author: Dr. Madeline Lovejoy
# Date: 2026-08-13
# R version 4.1.2 (2021-11-01)

# dplyr version 1.1.2
# tidyr version 1.3.0
# stringr version 1.5.0

# load packages
# if you have never used these packages in your R console, you will need to install them first with the code: install packages("package_name")
library(dplyr)
library(tidyr)
library(stringr)

# ***BEFORE RUNNING THIS LINE, MANUALLY SET THE WORKING DIRECTORY TO YOUR QUANTIFICATION FOLDER BY GOING TO SESSION -> SET WORKING DIRECTORY -> CHOOSE DIRECTORY***
# defines working directory as "dir" data frame
dir<-getwd()

# Places Excel sheets ending in "CystVol.csv" in the "FilesC" data frame
FilesC <- list.files(pattern = "CystVol.csv", recursive = TRUE)
CSVlistC <- lapply(FilesC, read.csv)
names(CSVlistC) <- FilesC
# connects the data between columns to each other
dataCyst <-  bind_rows(CSVlistC, .id = "filename")

# separates the file name into different variables based on the placement of underscores ( _ )
dataCyst <- dataCyst %>% separate(filename, c(NA, NA, "Condition", NA, NA, NA, NA, NA, NA, "IMS", "ROI"), sep="_") %>%
  # renames the CSV label for the volume column "Volume..micron.3." to "CystVolume", which is easier to call on later in the code
  rename(CystVolume = Volume..micron.3.) %>%
  # only displays the following columns
  select(Condition, IMS, ROI, CystVolume) %>%
  # gets rid of "CystVol.csv" attached to the ROI value so multiple quantification files can be grouped together based on the ROI value
  mutate(ROI =  str_replace(ROI, "CystVol.csv", ""))%>%
  # discards cyst volume values that are smaller than 8000 microns^3 to eliminate artifacts counted in the Fiji/ImageJ code
  filter(CystVolume > 8000)%>%
  # groups the data by Condition, IMS, and ROI
  arrange(Condition, IMS, ROI, .by_group = TRUE)
# Creates a CSV file from the dataCyst data frame called "Cyst_Volume_Data.csv"
write.csv(dataCyst, file = "Cyst_Volume_Data.csv")
