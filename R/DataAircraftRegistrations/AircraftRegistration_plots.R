##### Info about Data
################################################################################

## Data source: see readMe.txt or AircraftRegistration.Rmd

## General abbreviations in column and variable names:
##  ACFT: Aircraft, HP: Horsepower, MFR: Manufacturer, NO: Number
##  df_: dataframe, v_: vector


#####
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

## set working directory and load data
setwd("")
source("AircraftRegistration_data.R")



##### Explore dataset visually
################################################################################

## plot no. of engines over MFR-Year, incl. filter all NAs and wrong inputs (<4 digits)
## NOTE: one ACFT has 12 engines, it's an experimental
df_yr_filt <- filter(df_aircraft, YEAR.MFR > 1900 & !is.na(YEAR.MFR)) 
plot(x = df_yr_filt$YEAR.MFR, y = df_yr_filt$NO.ENG, pch = 16,
     main = "No. of engines over MFR-Year", xlab = "MFR-Year", ylab = "No. of engines")

## plot no. of seats over MFR-year
plot(x = df_yr_filt$YEAR.MFR, y = df_yr_filt$NO.SEATS, pch = 16)

## plot expiring certificates
plot(df_aircraft$EXPIRATION.DATE)



##### Create different charts
################################################################################

## for the next 3 plots, MFR-Year is grouped to 5 years,
## incl. filter all NAs and wrong inputs (<4 digits)
df_aircraft_MFR5Years <- df_aircraft %>%
  filter(YEAR.MFR > 1900 & !is.na(YEAR.MFR)) %>%
  group_by(as.integer(YEAR.MFR/5)*5)

## renames the last column
names(df_aircraft_MFR5Years)[ncol(df_aircraft_MFR5Years)] <- "YEAR.MFR5"

## group the distribution of No. of ACFT over 5year-periods
df_ACFT_No_MFRYear <- df_aircraft_MFR5Years %>%
  group_by(YEAR.MFR5) %>%
  summarise(count_ac = n())

## simple bar chart
ggplot(df_ACFT_No_MFRYear, aes(x=YEAR.MFR5, y=count_ac)) +
  geom_bar(stat="identity") +
  xlab("MFR 5 year-period")  +
  ylab("No. of ACFT registered") +
  ggtitle("No. of ACFT per 5 MFR-Years")

####

## overview aircraft-certification per 5 years
## pick the first character of Certification, bind the columns and rename it
v_ACFT_cert_cleanded <- substring(df_aircraft_MFR5Years$CERTIFICATION, first = 1, last = 1)
df_aircraft_MFR5Years <- cbind(df_aircraft_MFR5Years, v_ACFT_cert_cleanded)
df_aircraft_MFR5Years <- renameCols(df_aircraft_MFR5Years, "...27", "ACFT Cert. cleaned")
rm(v_ACFT_cert_cleanded)

## create a new dataframe with ""-certification filtered
df_ACFT_cert_5years <- df_aircraft_MFR5Years %>%
  filter(`ACFT Cert. cleaned`>=1) %>%
  group_by(`ACFT Cert. cleaned`, YEAR.MFR5) %>%
  summarise(count_ac = n())

## which type of certification for a certain MRF-period
ggplot(df_ACFT_cert_5years, aes(x=YEAR.MFR5, y=count_ac)) +
  geom_bar(stat = "identity", aes(fill=`ACFT Cert. cleaned`)) +
  xlab("MFR year-period") +
  ylab("No. of ACFT-Certifications") +
  ggtitle("ACFT-Certifications per 5 MFR-years period") +
  scale_fill_discrete(name = "Certification Type",
                      labels = v_Certification)

####

## overview owner-type per 5 years, only for the US-Residentials
## first, its important the type of the column 'Type.Registrant' has changed to character,
## otherwise the function grepl doesn't work -> already done at prepreparation

df_owner_ACFT <- df_aircraft_MFR5Years %>%
  filter(COUNTRY == "US", !is.na(TYPE.REGISTRANT)) %>%
  group_by(YEAR.MFR5, TYPE.REGISTRANT) %>%
  summarise(count_ac = n())

ggplot(df_owner_ACFT, aes(x=YEAR.MFR5, y=count_ac)) + 
  geom_bar(stat="identity", aes(fill=TYPE.REGISTRANT)) +
  xlab("MFR year-period")  +
  ylab("No. of ACFT") +
  ggtitle("Distribution of owner-types per 5 MFR-years period") +
  scale_fill_discrete(name = "Type Registrant",
                      labels = v_type_Registrant) +
  theme(axis.text = element_text(size = 5))

####

## excludes registrants outside of the US, count aircrafts and group by type of ACFT for each state
df_ACFTNo_State <- df_aircraft %>%
  filter(COUNTRY == "US") %>%
  group_by(STATE, TYPE.ACFT) %>%
  summarise(count_ac = n())

ggplot(df_ACFTNo_State, aes(x=STATE, y=count_ac)) + 
  geom_bar(stat="identity", aes(fill=TYPE.ACFT)) +
  xlab("State")  +
  ylab("No. of ACFT registered") +
  ggtitle("No. of Aircrafts registered in each state") +
  scale_fill_discrete(name = "Type Aircraft",
                      labels = v_type_ACFT) +
  theme(axis.text = element_text(size = 5))

####

## a deeper insight to the experimental planes

df_exp_ACFT <- df_aircraft %>%
  filter(grepl("^4", CERTIFICATION), COUNTRY == "US", !is.na(TYPE.REGISTRANT)) %>%
  group_by(STATE, TYPE.REGISTRANT) %>%
  summarise(count_ac = n())

ggplot(df_exp_ACFT, aes(x=STATE, y=count_ac)) + 
  geom_bar(stat="identity", aes(fill=TYPE.REGISTRANT)) +
  xlab("State")  +
  ylab("No. of exp. ACFT") +
  ggtitle("Distribution of experimental aircrafts per states & owner") +
  scale_fill_discrete(name = "Type Registrant",
                      labels = v_type_Registrant) +
  theme(axis.text = element_text(size = 5))



##### Create boxplots
################################################################################

## for the next plots, MFR-Year is grouped to 10 years,
## incl. filter all NAs and wrong inputs (<4 digits)

df_aircraft_MFR10Years <- df_aircraft %>%
  filter(YEAR.MFR > 1900 & !is.na(YEAR.MFR)) %>%
  group_by(as.integer(YEAR.MFR/10)*10)

## renames the last column
names(df_aircraft_MFR10Years)[ncol(df_aircraft_MFR10Years)] <- "YEAR.MFR10"

## change class of 10year-column to factor
df_aircraft_MFR10Years$YEAR.MFR10 <- as.factor(df_aircraft_MFR10Years$YEAR.MFR10)
class(df_aircraft_MFR10Years$YEAR.MFR10)

## boxplot of no. of seats each decade, its necessary to have a logarithmic y-axis,
## because most planes have a lower number of seats
ggplot(df_aircraft_MFR10Years, aes(x=YEAR.MFR10, y=NO.SEATS)) +
  geom_boxplot() +
  scale_y_log10(breaks = c(1, 2, 5, 10, 100, 1000)) +
  xlab("MFR 10 year-period") +
  ylab("No. of seats (log)") +
  ggtitle("Boxplots of ACFT-Seats with log. y-axis")

## boxplot of HP for each decade (generally for piston engines)
ggplot(df_aircraft_MFR10Years, aes(x=YEAR.MFR10, y=HORSEPOWER)) +
  geom_boxplot() +
  xlab("MFR 10 year-period") +
  ylab("Horsepower") +
  ggtitle("Boxplots of Horsepower per decade") +
  scale_y_continuous(limits = c(0,1000))

## boxplot of thrust for each decade
## we have to filter ACFT with MFR-Year before 1950, because there is such a small no of planes
df_aircraft_MFR10Years_thrust <- df_aircraft_MFR10Years %>%
  filter(YEAR.MFR >= 1950)

## removed the x-axis, colored the boxes
ggplot(df_aircraft_MFR10Years_thrust, aes(x=YEAR.MFR10, y=THRUST, fill = YEAR.MFR10)) +
  geom_boxplot() +
  xlab("MFR decade") +
  ylab("Thrust [lbf]") +
  ggtitle("Boxplots of Thrust per decade") +
  scale_y_log10() +
  annotation_logticks(sides = c("l")) +
  scale_fill_discrete(name = "Decade") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



##### View registrations per state in US map
################################################################################

## US states
us_states <- "'AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL',
                  'IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
                  'NE','NV','NH','NJ','NM','NY','NC','ND', 'OH','OK','OR','PA','RI',
                  'SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY'"

df_cntPerState <- sqldf(paste("SELECT STATE as state, COUNT(*) as cnt
                         FROM df_aircraft
                         WHERE STATE in (",us_states,")
                         GROUP BY STATE
                        "))

## plot in US map (the higher the count the deeper the color)
plot_usmap(data=df_cntPerState, values="cnt", color="blue") +
  scale_fill_continuous(
    low="white", high="blue", name="count", label=scales::comma) + 
  theme(legend.position="right")



##### Statistical Analysis: summary, standard deviation, linear regression
################################################################################
summary(df_aircraft$HORSEPOWER)
sd(na.omit(df_aircraft$HORSEPOWER))
fit <- lm(df_aircraft$HORSEPOWER ~ df_aircraft$SPEED)
summary(fit)



##### Session information
################################################################################
sessionInfo()
names(sessionInfo())

