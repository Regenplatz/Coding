
##### General Infos 
################################################################################

## Data source: see readMe.txt or AircraftRegistration.Rmd

## General abbreviations in column and variable names:
##    ACFT: Aircraft, HP: Horsepower, MFR: Manufacturer, NO: Number
##    df_: dataframe, v_: vector

## Plots are outsourced to AircraftRegistration_plots.R



##### Set up prerequisites
################################################################################

## import libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(usmap)
library(rmarkdown)
library(tinytex)
library(NCmisc)

## set working directory
setwd("")



##### Check which libraries were used in this script 
################################################################################

## list all loaded packages (note that some were not manually but automatically loaded as well)
# (.packages())



##### Read files, use different functions (read.csv, read.table, read.delim) to do that
################################################################################

## read master dataset about aircraft (first 1000 rows): Approach 1 (read.csv())
master_selection <- c("MFR.MDL.CODE", "ENG.MFR.MDL", "YEAR.MFR", "TYPE.REGISTRANT", 
                      "NAME", "CITY", "STATE", "CERTIFICATION", "EXPIRATION.DATE",
                      "ZIP.CODE", "COUNTRY", "TYPE.AIRCRAFT", "TYPE.ENGINE", "LAST.ACTION.DATE")
df_master <- read.csv("MASTER.txt")[, master_selection]

## read dataset about aircraft engines: Approach 2 (read.table())
engine_selection <- c("CODE", "MFR", "MODEL", "TYPE", "HORSEPOWER", "THRUST")
df_engine <- read.table("ENGINE.txt", sep=",", header=TRUE)[, engine_selection]

## read dataset about aircraft registration: Approach 3 (read.delim())
registration_selection <- c("CODE", "MFR", "MODEL", "TYPE.ACFT",
                            "NO.ENG", "NO.SEATS", "SPEED")
df_registration <- read.delim("ACFTREF.txt", sep=",", header=TRUE)[, registration_selection]



##### Join dataframes to a single one, use different functions (merge, left_join) to do that
################################################################################

## left join df_master and df_registration: Approach 1 (using merge function)
df_aircraft <- merge(df_master, df_registration, by.x="MFR.MDL.CODE", by.y="CODE", all.x=TRUE)

## left join newly created df_aircraft and df_engine: Approach 2 (using left_join function)
df_aircraft <- left_join(df_aircraft, df_engine, by=c("ENG.MFR.MDL"="CODE")) 

## show head of data frame
head(df_aircraft)

# remove unnecessary data frames
rm(list = c("df_engine", "df_master", "df_registration"))



##### Rearrange dataframe (convert data types, rename columns)
################################################################################

## set date columns from int to date type
dateCols <- c("LAST.ACTION.DATE","EXPIRATION.DATE")
for (some_date in dateCols) {
  df_aircraft[[some_date]] <- as.Date(paste(df_aircraft[[some_date]]), format="%Y%m%d")
}

## Rename columns: Approach 1 (without function)
names(df_aircraft)[names(df_aircraft) == "MFR.x"] <- "MFR_ACFT"

## Rename columns: Approach 2 (using a function)
renameCols <- function(df, oldName, newName){
  names(df)[names(df) == oldName] <- newName
  return (df)
}
df_aircraft <- renameCols(df_aircraft, "MFR.y", "MFR_engine")
df_aircraft <- renameCols(df_aircraft, "MODEL.x", "MODEL_ACFT")
df_aircraft <- renameCols(df_aircraft, "MODEL.y", "MODEL_engine")

## change classes of variables where necessary
df_aircraft$TYPE.REGISTRANT <- as.character(df_aircraft$TYPE.REGISTRANT)

## check if it is the right class
class(df_aircraft$TYPE.REGISTRANT)



##### Create random sample or subset of dataframe
################################################################################

# ## random sample
# set.seed(12389)
# sampleSize <- 100
# df_sample <- df_aircraft[sample(1:nrow(df_aircraft), sampleSize, replace=FALSE),]
# 
# ## subset: select YEAR.MFR and CITY of observations having more than 2 seats (NO.SEATS)
# df_subset <- subset(df_aircraft, NO.SEATS > 2, select=c(YEAR.MFR, CITY))
# unique(df_aircraft$NO.SEATS)



##### Explore dataset (data types, column names, NAs, dimension, summary)
################################################################################

## check if data was read as dataframe
is.data.frame(df_aircraft)

## check data type of each column
sapply(df_aircraft, class)

## show names of columns
colnames(df_aircraft)

## show number of NAs per column
colSums(is.na(df_aircraft))

## Show names of column containing NAs
colnames(df_aircraft)[colSums(is.na(df_aircraft)) > 0]

## display missing-data patterns
# install.packages("mice")
# library(mice)
# md.pattern(df_aircraft)
# md.pairs(df_aircraft)

## show dimesion of dataframe
dim(df_aircraft)

## show summary of dataframe
summary(df_aircraft)



##### Create vectors for decoding categorical features
################################################################################

v_type_Registrant <- c("Individual", "Partnership", "Corporation", "Co-Owned", "Government",
                       "LLC", "Non Citizen Corporation", "Non Citizen Co-Owned")

v_Certification <- c("Standart", "Limited", "Restricted", "Experimental", "Provisional",
                     "Multiple", "Primary", "Special Flight Permit", "Light Sport")

v_type_ACFT <- c("Glider", "Balloon", "Blimp/Dirigible", "Fixed wing single engine",
                 "Fixed wing multi engine", "Rotorcraft", "Weight-shift-control",
                 "Powered Parachute", "Gyroplane", "Hybrid Lift", "Other")

v_type_engine <- c("None", "Reciprocating", "Turbo-prop", "Turbo-shaft", "Turbo-jet",
                   "Turbo-fan", "Ramjet", "2 Cycle", "4 Cycle", "Unknown",
                   "Electric", "Rotary")


# #####
# ##### Create vectors for various kinds of data types
# ################################################################################
# 
# ## create separate lists for columns of each data type: Approach 1 (without function)
# int_cols <- names(df_aircraft)[sapply(sapply(df_aircraft, class), function(x) {"integer" %in% x} )]
# 
# ## create separate lists for columns of each data type: Approach 2 (using a function)
# dataTypes <- function(datType){
#   someList <- names(df_aircraft)[sapply(sapply(df_aircraft, class), function(x) {datType %in% x} )]
#   return (someList)
# }
# fac_cols <- dataTypes("factor")
# num_cols <- dataTypes("numeric")



##### Pivot table 
################################################################################

## lower code incl. lines 180-182 makes a pivot table with dyplr package
df_pivot <- df_aircraft %>%
  select(STATE, MFR_ACFT, MODEL_ACFT, TYPE.ACFT) # %>%
# group_by(STATE, MFR_ACFT, MODEL_ACFT, ) %>%
# summarize(count_model = n())

df_pivot <- pivot_longer(df_pivot, 
                         cols = c("STATE"),
                         names_to = "category", 
                         values_to = "state")
head(df_pivot, n = 10)



##### Create tables: Top n's of Engine Models & MFT, ACFT Models & MFT, Registrants name
################################################################################

## variable for the "top ten"
nTop <- 10

## Aircraft Manufacturer
df_ACFT_TopList_MFR <- df_aircraft %>%
  group_by(MFR_ACFT) %>%
  filter(!is.na(MFR_ACFT)) %>%
  summarise(count_n=n()) %>%
  slice_max(count_n, n=nTop, with_ties=FALSE)

## Aircraft Model
df_ACFT_TopList_MDL <- df_aircraft %>%
  group_by(MODEL_ACFT) %>%
  filter(!is.na(MODEL_ACFT)) %>%
  summarise(count_n=n()) %>%
  slice_max(count_n, n=nTop, with_ties=FALSE)

## Engine Manufacturer
## NOTE: excludes NA, NONE is also an 'Engine Type' (the easiest to maintain ;D )
df_ENG_TopList_MFR <- df_aircraft %>%
  group_by(MFR_engine) %>%
  filter(!is.na(MFR_engine)) %>%
  summarise(count_n=n()) %>%
  slice_max(count_n, n=nTop, with_ties =FALSE)

## Engine Model
## NOTE: excludes 'Unknown Engine' over the type, conclusion: if the Eng-Mdl is unknown, the Eng-Type is NA
##       NONE is not removed, because it's also a 'type' of engine
df_ENG_TopList_MDL <- df_aircraft %>%
  filter(!is.na(TYPE)) %>%
  group_by(MODEL_engine) %>%
  summarise(count_n = n()) %>%
  slice_max(count_n, n = nTop, with_ties = FALSE)

## Top Owners by Name
## NOTE: there is no NA-Filter bec. there shouldn't be any NAs in Owners name, otherwise the
##       agency did not their job), but 'registration pending' & 'sale reported' are filtered out
df_Ownr_TopList <- df_aircraft %>%
  filter(!grepl(c("REGISTRATION PENDING|SALE REPORTED"), NAME)) %>%
  group_by(NAME) %>%
  summarise(count_n = n()) %>%
  slice_max(count_n, n = nTop, with_ties = FALSE)

## Same list like above but airlines are filtered out, too.
## NOTE: The code looks only for patterns with 'Line' because
##       some airlines write their name like this 'Air Line'
##       there are also about 370 entries without a name, which we also filter NOT!!!
df_Ownr_TopList2 <- df_aircraft %>%
  filter(!grepl(c("REGISTRATION PENDING|SALE REPORTED|LINE"), NAME)) %>%
  group_by(NAME) %>%
  summarise(count_n = n()) %>%
  slice_max(count_n, n = nTop, with_ties = FALSE)



##### Session information
################################################################################
sessionInfo()
names(sessionInfo())

