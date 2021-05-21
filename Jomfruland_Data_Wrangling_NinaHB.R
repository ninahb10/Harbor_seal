
#############################################
############## DATA WRANGLING ##############
######## 2020-21 JOMFRULAND (Skagerrak) ####
### Nina Hille Bringsdal ###################
############################################

# Load the relevant data from Access data base (rSRDL-package), starting with 2020 (pv74) and work backwards in time (2019, 2017, 2010, 2009, 2008 in different scripts named appropriately)

# Split the summary-df from the lists and clean it (remove pre-tagging dates, remove NAs, etc.).

# Add solar elevation variables, and add day/night-factor to the df.

# Add moon phase "for fun" (collinearity, ahoy!)

# Access tide levels (Kartverket), windspeeds, air temperature and precipitation (seKlima) for the deployment period, average means and synchronize observation points - and finally add all environmental variables to the main data frame.

# Add sex, weight, and length to the individuals in the summaries of all the summaries in the .R-scripts.



#################### IMPORTANT QUESTIONS ########################
## NB!! Note to self: xts and zoo might be necessary to read in environmental variables - but have to practice queries etc on time series first. ## NB!!

## NB!! Note to self: Need to understand and create Shapefiles (key words: maptools, sp, CRS, proj4, datum) for latitude and longitude covariates. ## NB!!

## NB!! How to merge (cbind) uneven vector and matrix data to fit a time series, and how do I merge uneven coordinates from one df to another? ## NB!!
#####################################################################


# Install package
devtools::install_github("embiuw/rSRDL")

# Some of the packages needed - load up into workspace
library("rSRDL")
library("tidyverse")
library("lubridate")
library("lattice")


#############################################
####### Data base 2020-21 SKAGERRAK ########
############################################

# Load up the data!
#------------------
pv74 <- get.all.SRDLdb("pv74", "C:/Users/ninab/Documents/Masteroppgave_Haulout/Harbor_seal/Data_seal/Harbor seal GSM 2020")



# Explore the data frames
#-------------------------
# Names of the data frames
names(pv74)

### HAULOUT ###
#--------------
# Names of columns in haulout-data frame
names(pv74$haulout)
summary(pv74$haulout)
str(pv74$haulout) 

# NB: Latitude and longitude columns, I need.


# Produces google map of seal movements - neat!
#map.SRDL(pv74, type = "leaflet")


### DEPLOYMENT ###
#------------------
# Read deployment data frame and the unique individual IDs/refs
names(pv74$deployments)
pv74$deployments$ref

# Gives dive profiles of the individuals, with possibility to zoom in for excellent visualization.
#strip.SRDL(pv74, theRef = "pv74-M86_Bjorn-20", type = "dygraph")
#strip.SRDL(pv74, theRef = "pv74-M88_Diego-20", type = "dygraph")


### DIVE ###
#--------------
# Taking a look at the dive-data
names(pv74$dive)
head(pv74$dive$LAT)
summary(pv74$dive$LAT)

# DE.DATE. Dive end: the time at which the depth crossed the threshold on the return to the surface

# SURF.DUR. Surface duration (seconds): the length of time following DE.DATE before the next dive or other event began
summary(pv74$dive$SURF.DUR)

# DIVE.DUR. Dive duration (seconds): the length of time between the crossing of the dive-start threshold on the descent and DE.DATE
summary(pv74$dive$DIVE.DUR)

# D1-D9: Intermediate depth points (meters) in the dive. For most deployments one of these is guaranteed to be the maximum depth.
summary(pv74$dive$D1)
summary(pv74$dive$D2)
summary(pv74$dive$D3)
summary(pv74$dive$D4)
summary(pv74$dive$D5)
summary(pv74$dive$D6)
summary(pv74$dive$D7)
summary(pv74$dive$D8)
summary(pv74$dive$D9)


### SUMMARY DF ###
#------------------
# Extract data frame. Includes probable response variable for model. 

# Looking at the summary data frame
names(pv74$summary)
pv74$summary$HAUL.TM

# Separate summary-df from list
summary_pv74 <- pv74$summary
names(summary_pv74)
summary(summary_pv74)



# Calculate Center Time between S.DATE and E.DATE
#---------------------------------------------------

# Calculate center time between S.DATE and E.DATE
head(summary_pv74$S.DATE)
head(summary_pv74$E.DATE) # (6/2)h = (3600s * 6h)/2 = 10800s


# Create center time variable
summary_pv74$CEN.DATE <- summary_pv74$S.DATE + 10800

# Check if I got the variable right
head(summary_pv74$S.DATE)
head(summary_pv74$CEN.DATE)
head(summary_pv74$E.DATE) # Yes

str(summary_pv74)



# Remove the NA columns
#--------------------------------------------------
# Remove NA columns
summary2_pv74 <- summary_pv74 %>% 
  select(1, 2, 4, 5, 7:12, 17, 19, 29, 31, 49, 52)

summary(summary2_pv74) # 16 variables left, no all-NA columns

# Structure looking good?
str(summary2_pv74) # Yes

# Remove NA observations
#summary3_pv74 <- na.omit(summary2_pv74)



# Clean the factorized REF variable
#---------------------------------------------------------------------
# Do all the levels in REF make sense..?
levels(summary2_pv74$REF)

nlevels(summary2_pv74$REF) # I do not know what the 7 single factor levels with no obs are.

class(summary2_pv74$REF) # Factor

# Tibble<3
fct_count(summary2_pv74$REF) # No observatipns on the 7 unidentified levels

# I'll remove the strange levels at the bottom of the df, they don't have observations

h_REF <- c("pv74-F46_Olivia-20", "pv74-M62_Gamle-Erik-20", "pv74-M70_Osito-20", "pv74-M86_Bjorn-20", "pv74-M88_Diego-20")
h_summ <- summary2_pv74 %>%
  filter(REF %in% h_REF)
nlevels(h_summ$REF) # Still 12 levels

# Use droplevels() to drop the non-informative levels:
summary3_pv74 <- h_summ %>% 
  droplevels()
nlevels(summary3_pv74$REF) # 5 levels

str(summary3_pv74) # Success

# A very rudimentary plot
ggplot(summary3_pv74, aes(DIVE.TM, HAUL.TM)) +
  geom_point(aes(color = REF))


# Remove Data Recorded Pre- and Post Tagging dates
#---------------------------------------------------------------
# Need to remove the data recorded before the tagging date. Should not be used.

# First, have a look at the separate individuals:
levels(summary3_pv74$REF)

## Bjørn
summary3_pv74 %>% 
  filter(REF == "pv74-M86_Bjorn-20") 

# Bjørn was released 14/11/2020 09:09 (Jomfruland), and I see that the first line of his data is not relevant.


## Gamle Erik
summary3_pv74 %>% 
  filter(REF == "pv74-M62_Gamle-Erik-20") 

# Gamle Erik was released 19/10/2020 15:15 (Hvaler). 
# No data recorded before 20/10/2020 at noon (12:05), so in theory I can remove the first 4 lines of the data.


## Olivia
summary3_pv74 %>% 
  filter(REF == "pv74-F46_Olivia-20") 

# Olivia was released 14/11/2020 09:18. 
# The first couple of lines can be removed.


## Osito
summary3_pv74 %>% 
  filter(REF == "pv74-M70_Osito-20") 

# Osito was released 14/11/2020 09:56. 
# I can remove the 2 first lines


## Diego
summary3_pv74 %>% 
  filter(REF == "pv74-M88_Diego-20") 

# Diego was released 14/11/2020 10:03. 
# I can remove the first line.

## Dplyr/play around with intervals
# Remove rows pre-tagging of Gamle Erik and rename df
summary3_pv74 %>% 
  filter(S.DATE > ymd_hms("2020-10-19 12:05:27"))
head(summary5_pv74$S.DATE)

# I just want to remove irrelevant dates from all individuals except Gamle Erik, who probably gave data these dates.

summary3_pv74 %>% 
  filter(REF != "pv74-M62_Gamle-Erik-20") %>% 
  filter(S.DATE > ymd_hms("2020-11-14 12:00:00"))

# I am not sure how to remove specific intervals within differing levels in a factor by date-time. I will remove "safely" and tediously:

# Deleting the lines and creating a new df
summary4_pv74 <- summary3_pv74[-c(1:3, 427, 428, 790, 791, 1279, 1280, 1784, 1785), ]

# Check if it looks good:
summary4_pv74 %>% 
  filter(REF == "pv74-M86_Bjorn-20") # I think all is good.
summary4_pv74 %>% 
  filter(REF == "pv74-F46_Olivia-20") # Yep


# Rename REF-levels
summary4_pv74$REF <- revalue(summary4_pv74$REF, c("pv74-F46_Olivia-20" = "Olivia", "pv74-M62_Gamle-Erik-20" = "Gamle-erik", "pv74-M70_Osito-20" = "Osito", "pv74-M86_Bjorn-20" = "Bjorn", "pv74-M88_Diego-20" = "Diego"))


levels(summary4_pv74$REF)


# Add Solar Elevation as Environmental Variable
#--------------------------------------------------------------
# Add Solar Elevation

library(maptools)
gpclibPermit()

lat <- 58.859459  # Latitude Jomfruland
lon <- 9.587091  # Longitude Jomfruland

# Matrix of the coordinates for Jomfruland
coo <- matrix(c(lon, lat), nrow = 1)
coo


coo <- SpatialPoints(coo, proj4string = CRS("+proj=longlat+datum=WGS84"))
coo

?solarpos # Functions for calculating sunset, and times of dawn and dusk, with flexibility for the various formal definitions. They use algorithms provided by the National Oceanic & Atmospheric Administration (NOAA).

# Making variable of solar elevation that fits the center date
solar <- solarpos(coo, summary4_pv74$CEN.DATE)[,2]
solar[1:10] # Smooth!


# Merging the data with solar-variable
summary4_pv74 <- cbind(summary4_pv74, solar)

head(summary4_pv74)
summary(summary4_pv74)

# Creating day and night-columns
summary4_pv74$nday <- "1_Day"
summary4_pv74$nday[summary4_pv74$solar < 0] <- "2_Night"

# How many hours day vs night all together in the sampled period
table(summary4_pv74$nday)

# 674 solar hours and 1655 dark hours

# A simple plot of haulout time along dates, day/night, for each ref.
ggplot(summary4_pv74, aes(S.DATE, HAUL.TM)) +
geom_point(aes(color = nday)) +
  facet_grid(~ REF)

# A simple plot of dive time along dates, day/night, for each ref.
ggplot(summary4_pv74, aes(S.DATE, DIVE.TM)) +
  geom_point(aes(color = nday)) +
  facet_grid(~ REF)



## Reorder columns for clarity by index:
summary4_pv74 <- summary4_pv74[c(1, 2, 3, 16, 4, 5:15, 17, 18)]
names(summary4_pv74)




# Playing with Biuw's getTide-function
#---------------------------------------------------------------------
# Sourcing Biuw's getTide-function
source("getTide_function.R") # path gotta be in same folder of work dir.

# 24 hours with 10 min intervals
tides_test <- getTide(lat = 58.9, lon = 5.7, date = "2020-09-01", type = "OBS", interval = 10)

# 24 hours with 60 min intervals
tides_test2 <- getTide(lat = 58.859459, lon = 9.587091, date = "2021-03-18", type = "OBS", interval = 60);head(tides_test2)

# No other intervals exist in Kartverket's data base, than 10 and 60 minute intervals. 



# Add Mean Tide-values as Environmental Variable
#------------------------------------------------------------
# Tide data Jomfruland

library("anytime")

#-------------------------NB! Need to unload and reload tidyverse (and lubridate?) before working with tides. The rename-function does not work unless. I have tried to detach packages, but it does not work.---------------------------------------------


# Read in the text file (10 min interval)
tides <- read.table("tide-5.txt")
str(tides)
head(tides)

# Read in second text file (1 h interval)
tides2 <- read.table("tides_helgeroa_2020.txt")
str(tides2)
head(tides2)

##########################################
#Abstract:        The water level is observed at Helgeroa, multiplied by a factor of 0.98 and time adjusted by 0 minutes.
#Provider:        Statens kartverk sjø (Norwegian Hydrographic Service)
#Latitude:        58.85945900
#Longitude:       9.58709100
#Datum:           EUREF89
#Time interval:   3600 seconds
#Reference level: CD (Chart Datum)
#Z0: 49.4 cm
#Start time:      2020-09-19T00:00:00+01:00
#End time:        2021-04-03T00:00:00+01:00
#Unit:            cm
##############################################################

#1: Change dates from character strings to POSIXct with package anytime()
#2: Rename columns

# Change dates from character vector to POSIXct
tides$V1 <- anytime(tides$V1)
head(tides)

tides2$V1 <- anytime(tides2$V1)
head(tides2)
str(tides2)

# Rename columns
df_tides <- tides %>%
  rename(date_time = V1,
         tide_level = V2); head(df_tides)

df_tides2 <- tides2 %>% 
  rename(date_time = V1,
         tide_level = V2); names(df_tides2)


str(df_tides)
str(summary4_pv74$S.DATE) 
head(df_tides)


# Very rudimentary plot of water levels in the sampled period (10th of October 2020 to 3rd of April 2021)
ggplot(df_tides, aes(date_time, tide_level)) +
  geom_point(aes(color = date_time))

ggplot(df_tides2, aes(date_time, tide_level)) +
  geom_point(aes(color = date_time)) # The plots look the same, so stay with the hourly df_tides2


# Have a look at tibbles
head(as_tibble(summary4_pv74$S.DATE)) # 6 hour intervals
head(as_tibble(df_tides$date_time)) # 10 minute intervals
head(as_tibble(df_tides2$date_time)) # 1 hour intervals

   # A bit ugly to do this #

# Remove 18 first rows, so position of time starts at approx same time as first deployment (Gamle Erik)
df_tides2 <- df_tides2[-c(1:18),]
# Remove bottom rows, so time series ends at ca. last end date of haulout-data
df_tides3 <- subset(df_tides2, date_time < "2021-04-02 16:00:00")
tail(df_tides3)




# Calculate the mean tide every 6 hours
df_tidesmean <- df_tides3 %>% 
  group_by(date_time = ceiling_date(date_time, "6 hours")) %>% 
  summarize(mean(tide_level));head(df_tidesmean)


# Rename column
names(df_tidesmean)[names(df_tidesmean) == "mean(tide_level)"] <- "mean_tides"; names(df_tidesmean)

# Plot of mean tides every 6 hours
ggplot(df_tides4, aes(date_time, mean_tides)) +
  geom_point(aes(color = date_time)) # Follows the same trend...

head(df_tidesmean)



## I need to fit the first 423 mean values to (factorized) Gamle-Erik 2020-09-19 to 2021-01-03. I need to fit the next 1906 mean values, from 2020-11-14 to 2021-04-02.
dim(summary4_pv74) 
summary(summary4_pv74)

# Number of obs for each factor level
table(summary4_pv74$REF)


# Df of tidal means between start date and end date of GAMLE ERIK
gamle_erik <- df_tidesmean %>% 
  filter(date_time <= as.POSIXct("2021-01-03"))

dim(gamle_erik) # 423 obs, same as haulout df



# Df of tidal means between start date and end date of BJORN
bjorn <- df_tidesmean %>% 
  filter(date_time >= as.POSIXct("2020-11-14") & date_time <= as.POSIXct("2021-02-12"))

dim(bjorn) # 361 obs, same as haulout df


# Df of tidal means between start date and end date of OLIVIA
olivia <- df_tidesmean %>% 
  filter(date_time >= as.POSIXct("2020-11-14 15:00") & date_time < as.POSIXct("2021-03-16 09:00"))

dim(olivia) # 487 obs OK



# Df of tidal means between start and end date of OSITO
osito <- df_tidesmean %>% 
  filter(date_time >= as.POSIXct("2020-11-14 14:51:51
") & date_time <= as.POSIXct("2021-03-20 08:51:50"))

dim(osito) #  503 obs OK



# Df of tidal means between start and end date of DIEGO
diego <- df_tidesmean %>% 
  filter(date_time >= as.POSIXct("2020-11-14 15:00") & date_time <= as.POSIXct("2021-04-02 04:00"))

dim(diego) # 555 obs OK


# Extract mean tides in vectors
vec_tides_ge <- gamle_erik$mean_tides
vec_tides_bj <- bjorn$mean_tides
vec_tides_ol <- olivia$mean_tides
vec_tides_os <- osito$mean_tides
vec_tides_di <- diego$mean_tides

# Combine vectors
vec_tides <- c(vec_tides_ge, vec_tides_bj, vec_tides_ol, vec_tides_os, vec_tides_di)

# Add tide variable to haulout df
summary4_pv74$Mean_Tide <- vec_tides
head(summary4_pv74)
str(summary4_pv74)
summary(summary4_pv74)
plot(summary4_pv74$Mean_Tide)
summary(df_tidesmean)
summary(df_tides3)  # It doesn't look horrible, and this is the best I could do. Tedious, but got the job done..?



# Convert all dates to time zone GMT
#----------------------------------------------------------
t0 <- as.character(df_tides3$date_time);head(t0)
df_tides3$date_time <- as.POSIXct(t0, tz = "GMT");head(df_tides3$date_time)
t <- as.character(df_tides$date_time);head(t)
df_tides$date_time <- as.POSIXct(t, tz = "GMT") ;head(df_tides$date_time)
t2 <- as.character(summary4_pv74$S.DATE);head(t2)
summary4_pv74$S.DATE <- as.POSIXct(t2, tz = "GMT");head(summary4_pv74$S.DATE)
t3 <- as.character(summary4_pv74$CEN.DATE);head(t3)
summary4_pv74$CEN.DATE <- as.POSIXct(t3, tz = "GMT");head(summary4_pv74$CEN.DATE)
t4 <- as.character(summary4_pv74$E.DATE);head(t4)
summary4_pv74$E.DATE <- as.POSIXct(t4, tz = "GMT");head(summary4_pv74$E.DATE)



# Add Moon Phase as Environmental Variable
#------------------------------------------------------------------------
library(HelpersMG)

## How to interpret the values:
#------------------------------
# 0 and 1.6931595 or 98.3068405 and 100, it is full moon,
# 23.3068405 and 26.6931595, last quarter,
# 48.3068405 and 51.6931595 new moon,
# 73.3068405 and 76.6931595, first quarter


# Create character vector of CEN.DATE in summary4
seal_dates <- as.character(summary4_pv74$CEN.DATE)
# Create moon phase vector length of seal dates
Moon_Phase <- moon.info(date = as.Date(seal_dates), phase = FALSE)
head(Moon_Phase)

# Combine to and rename main df
summary5_pv74 <- cbind(summary4_pv74, Moon_Phase)
str(summary5_pv74)


## NB: Should definitely see relationship between moon phase and mean tides:)





# Add PRECIPITATION, TEMPERATURE & WIND as environmental variables
#-----------------------------------------------------------------
##### Environmental Variables ##### 

## INFO: Weather statistics retrieved from the Norwegian Centre for Climate Services (https://seklima.met.no/).

# The data comes from two stations
# a: Svenner SN29950 (wind and temp) (North of Jomfruland)
# b: Torungen Fyr	SN36200 (precipitation) (South of Jomfruland)

# NCCS's description of variables:
# Precipitation: Amount of precipitation per hour
# Air temperature: Air temperature (default 2 m above ground), present value.
# Mean wind: Mean wind speed is registered as a mean value of the wind speed over the last ten mintues before the observation time. (default: 10 meters above ground, some stations have measurements at 2 meters).

# NB: The data from Jomfruland Station was not continuous/no data before late october 2020. I have collected data from the two nearest stations


# Needful Things
#---------------
source("getTide_function.R")
library(openxlsx)


# Read .xlsx from Svenner
#------------------------
envir_df <- read.xlsx("envir_svenner.xlsx")
head(envir_df)
names(envir_df)
str(envir_df)
summary(envir_df)
dim(envir_df)

# Read .xlsx from Torungen
#-------------------------
rain_df <- read.xlsx("precip_torungen.xlsx")
head(rain_df)
names(rain_df)
str(rain_df)
summary(rain_df)
dim(rain_df)



# Change character dates to Dates or POSIXct.
#-------------------------------------------
# Svenner
names(envir_df)
envir_df$`Time(norwegian.mean.time)` <- dmy_hm(envir_df$`Time(norwegian.mean.time)`)
str(envir_df$`Time(norwegian.mean.time)`) # Looks good

# Torungen
names(rain_df)
rain_df$`Time(norwegian.mean.time)` <- dmy_hm(rain_df$`Time(norwegian.mean.time)`)
str(rain_df$`Time(norwegian.mean.time)`) # Looks good

# Remove last row - it's all NA, all the way.
#------------------------------------------
envir_df <- envir_df[-4705, ]
rain_df <- rain_df[-4705, ]
dim(envir_df)
dim(rain_df)# All good


# Remove rows before 2020-09-19 18:00 and after 2021-04-02 09:00
#--------------------------------------------------------------
# Svenner
names(envir_df)
envir_df2 <- envir_df %>% 
  filter(`Time(norwegian.mean.time)` >= as.POSIXct("2020-09-19 20:00") & `Time(norwegian.mean.time)` <= as.POSIXct("2021-04-02 11:00"))
summary(envir_df2$`Time(norwegian.mean.time)`)

# Torungen
names(rain_df)
rain_df2 <- rain_df %>% 
  filter(`Time(norwegian.mean.time)` >= as.POSIXct("2020-09-19 20:00") & `Time(norwegian.mean.time)` <= as.POSIXct("2021-04-02 11:00"))
summary(rain_df2$`Time(norwegian.mean.time)`)




# Create separate dfs for each variable
#-------------------------------------
# Precipitation
#--------------
precip_df <- rain_df2[, c(3, 4)]
head(precip_df)
tail(precip_df)

# Rename irritating column names
names(precip_df)[names(precip_df) == "Time(norwegian.mean.time)"] <- "Date_Time"
names(precip_df)[names(precip_df) == "Precipitation.(1.h)"] <- "Precipitation"
head(precip_df)


# Air temperature
#----------------
temp_df <- envir_df2[, c(3, 4)]
head(temp_df)
tail(temp_df)

# Rename irritating column names
names(temp_df)
names(temp_df)[names(temp_df) == "Time(norwegian.mean.time)"] <- "Date_Time"
names(temp_df)[names(temp_df) == "Air.temperature"] <- "Air_Temp"
head(temp_df)


# Mean wind speed
#----------------
wind_df <- envir_df2[, c(3, 7)]
head(wind_df)
tail(wind_df) # Looks good

# Rename irritating column names
names(wind_df)
names(wind_df)[names(wind_df) == "Time(norwegian.mean.time)"] <- "Date_Time"
names(wind_df)[names(wind_df) == "Mean.wind.speed"] <- "Mean_windspeed"
head(wind_df)



# Calculate the mean every 6 hours for each variable
#--------------------------------------
# Precipitation mean
#-------------------
names(precip_df)
mean_precip_df <- precip_df %>% 
  filter(!is.na(Precipitation)) %>% 
  group_by(Date_Time = ceiling_date(Date_Time, "6 hours")) %>% 
  summarize(mean_precipitation = mean(Precipitation, na.rm = TRUE))
head(mean_precip_df)
summary(mean_precip_df)
plot(mean_precip_df) # Looks ok, but many zeros will give a low arithmetic mean


# Air temperature mean
#--------------------
names(temp_df)
mean_temp_df <- temp_df %>% 
  filter(!is.na(Air_Temp)) %>% 
  group_by(Date_Time = ceiling_date(Date_Time, "6 hours")) %>% 
  summarize(mean_airtemperature = mean(Air_Temp, na.rm = TRUE))
head(mean_temp_df)
summary(mean_temp_df)
plot(mean_temp_df) # Looks ok


# Wind speed mean of mean
names(wind_df)
meanofmean_wind_df <- wind_df %>% 
  filter(!is.na(Mean_windspeed)) %>% 
  group_by(Date_Time = ceiling_date(Date_Time, "6 hours")) %>% 
  summarize(mean_of_mean_windspeed = mean(Mean_windspeed, na.rm = TRUE))
head(meanofmean_wind_df)
summary(meanofmean_wind_df)
plot(meanofmean_wind_df) # Looks ok



# Fit the environmental means to observations in haulout data
#------------------------------------------------------------

# Number of observations for each seal
table(summary5_pv74$REF) # Gamle_Erik = 423, Bjorn = 361, Olivia = 487, Osito = 503, Diego = 555

# GAMLE ERIK environmental observations
#------------------------
# Df of precipitation means between start date and end date of GAMLE ERIK
gml_er_p <- mean_precip_df %>% 
  filter(Date_Time <= as.POSIXct("2021-01-03 12:00"))
head(gml_er_p)
tail(gml_er_p)
dim(gml_er_p) # 432 obs - OK

# Df of temperature means between start date and end date of GAMLE ERIK
gml_er_t <- mean_temp_df %>% 
  filter(Date_Time <= as.POSIXct("2021-01-03 12:00"))
head(gml_er_t)
tail(gml_er_t)
dim(gml_er_t) # 423 obs - OK

# Df of wind means between start date and end date of GAMLE ERIK
gml_er_w <- meanofmean_wind_df %>% 
  filter(Date_Time <= as.POSIXct("2021-01-03 12:00"))
head(gml_er_w)
tail(gml_er_w)
dim(gml_er_w) # 423 obs - OK

# BJØRN environmental observations
#---------------------------------
# Df of precipitation means between start date and end date of BJØRN
bjorn_p <- mean_precip_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time <= as.POSIXct("2021-02-12 21:00"))
dim(bjorn_p) # 361 obs - OK

# Df of temperature means between start date and end date of BJØRN
bjorn_t <- mean_temp_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time <= as.POSIXct("2021-02-12 21:00"))
dim(bjorn_t) # 361 obs - OK

# Df of wind means between start date and end date of BJØRN
bjorn_w <- meanofmean_wind_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time <= as.POSIXct("2021-02-12 21:00"))
dim(bjorn_w) # 361 obs - OK

# OLIVIA environmental observations
#----------------------------------
# Df of precipitation means between start date and end date of OLIVIA
olivia_p <- mean_precip_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time < as.POSIXct("2021-03-16 09:00"))
dim(olivia_p) # 487 obs OK

# Df of temperature means between start date and end date of OLIVIA
olivia_t <- mean_temp_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time < as.POSIXct("2021-03-16 09:00"))
dim(olivia_t) # 487 obs OK

# Df of wind means between start date and end date of OLIVIA
olivia_w <- meanofmean_wind_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time < as.POSIXct("2021-03-16 09:00"))
dim(olivia_w) # 487 obs OK

# OSITO environmental observations
#--------------------------------
# Df of precipitation means between start date and end date of OSITO
osito_p <- mean_precip_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 14:51:51
") & Date_Time <= as.POSIXct("2021-03-20 08:51:50"))
dim(osito_p) #  503 obs OK

# Df of temperature means between start date and end date of OSITO
osito_t <- mean_temp_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 14:51:51
") & Date_Time <= as.POSIXct("2021-03-20 08:51:50"))
dim(osito_t) #  503 obs OK

# Df of wind means between start date and end date of OSITO
osito_w <- meanofmean_wind_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 14:51:51
") & Date_Time <= as.POSIXct("2021-03-20 08:51:50"))
dim(osito_w) #  503 obs OK

# DIEGO environmental observations
#---------------------------------
# Df of precipitation means between start date and end date of DIEGO
diego_p <- mean_precip_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time <= as.POSIXct("2021-04-02 09:00"))
dim(diego_p) # 555 obs OK

# Df of temperature means between start date and end date of DIEGO
diego_t <- mean_temp_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time <= as.POSIXct("2021-04-02 09:00"))
dim(diego_t) # 555 obs OK

# Df of wind means between start date and end date of DIEGO
diego_w <- meanofmean_wind_df %>% 
  filter(Date_Time >= as.POSIXct("2020-11-14 15:00") & Date_Time <= as.POSIXct("2021-04-02 09:00"))
dim(diego_w) # 555 obs OK


# Create vectors of correct lengths and timely observations
#---------------------------------------------------------
# Gamle Erik
vec_precip_ge <- gml_er_p$mean_precipitation
vec_temp_ge <- gml_er_t$mean_airtemperature
vec_wind_ge <- gml_er_w$mean_of_mean_windspeed

# Bjørn
vec_precip_bj <- bjorn_p$mean_precipitation
vec_temp_bj <- bjorn_t$mean_airtemperature
vec_wind_bj <- bjorn_w$mean_of_mean_windspeed

# Olivia
vec_precip_ol <- olivia_p$mean_precipitation
vec_temp_ol <- olivia_t$mean_airtemperature
vec_wind_ol <- olivia_w$mean_of_mean_windspeed

# Osito
vec_precip_os <- osito_p$mean_precipitation
vec_temp_os <- osito_t$mean_airtemperature
vec_wind_os <- osito_w$mean_of_mean_windspeed

# Diego
vec_precip_di <- diego_p$mean_precipitation
vec_temp_di <- diego_t$mean_airtemperature
vec_wind_di <- diego_w$mean_of_mean_windspeed


# Recombine vectors in correct order of time positions
#----------------------------------------------------
# Precipitation
vec_precip <- c(vec_precip_ge, vec_precip_bj, vec_precip_ol, vec_precip_os, vec_precip_di)
# Temperature
vec_temp <- c(vec_temp_ge, vec_temp_bj, vec_temp_ol, vec_temp_os, vec_temp_di)
# Wind
vec_wind <- c(vec_wind_ge, vec_wind_bj, vec_wind_ol, vec_wind_os, vec_wind_di)


# Add environmental variables to haulout data
summary5_pv74$Mean_Precipitation <- vec_precip
summary5_pv74$Mean_Temperature <- vec_temp
summary5_pv74$Mean_Windspeed <- vec_wind
head(summary5_pv74)
str(summary5_pv74) # Looks good






# How about adding latitude and longitude for each observation? Wouldn't that be something?

# The haulout data has the longs and lats.
haulout_df <- pv74$haulout_orig
dim(haulout_df) # 3405 obs

# Explore the df
summary(haulout_df)
table(haulout_df$REF)
head(haulout_df)
summary(haulout_df)



# Minimize df to 5 columns: Ref, s.date, e.date, wet.n, lat and long
latlong_df <- haulout_df[, c(1, 3, 4, 8, 16, 17)]
head(latlong_df)
dim(latlong_df)
str(latlong_df)

# Add center date between start and end dates
latlong_df$CEN.DATE <- latlong_df$S.DATE + 21600; head(latlong_df)



#------------------- FUN WITH PLOTS------------------------
library(sf)
library(mapview)
library(ggmap)
library(gridExtra)
library(scales)

# Simple plot positions
ggplot(latlong_df, aes(lon, lat)) + 
  geom_point(size = .25, show.legend = FALSE) +
  coord_quickmap()

# No NAs allowed
latlong_nona <- na.omit(latlong_df)

# Create positions object
haulout_locations <- st_as_sf(latlong_nona, coords = c("lon", "lat"), crs = 4326)

# Mapview
mapview(haulout_locations)# Worked like a charm

# Plot of Olivia's haulout profile on centered timeline
summary5_pv74 %>% 
  filter(REF == "pv74-F46_Olivia-20") %>% 
  ggplot(aes(CEN.DATE, HAUL.TM)) +
  geom_point()+
  geom_line()

# Plot of Olivia's WET.N vs CEN.DATE
latlong_df %>% 
  filter(REF == "pv74-F46_Olivia-20") %>% 
  ggplot(aes(CEN.DATE, WET.N)) +
  geom_point() +
  geom_line()



# Add MONTH and JULIAN DAY to summary-df
#--------------------------
# Create a month column in summary-df
summary5_pv74$Month <- month(summary5_pv74$CEN.DATE)
names(summary5_pv74)
str(summary5_pv74$Month)



# Convert with yday into a new column "julian" for julian days
summary5_pv74$julian <- yday(summary5_pv74$CEN.DATE)
head(summary5_pv74$julian)
tail(summary5_pv74$julian)



# Add Julian days column
latlong_nona$julian <- yday(latlong_nona$CEN.DATE)

# Clean the latlong-df a little more
positions_df <- latlong_nona[, -c(1:4)]
head(positions_df)

#-----------------DO NOT RUN - MESSY ----------------------------------

# Merge by Julian days
#summary_merged <- merge(positions_df, summary6_pv74, by = "julian", all.x = TRUE)
#head(summary_merged)


# Remove duplicates
#summary_merged[!duplicated(summary_merged$lat), ]

# Order by datetime and REF
#summary_merged <- summary_merged[order(summary_merged$CEN.DATE.y), ]
#summary_merged <- summary_merged[order(summary_merged$REF), ]
#head(summary_merged) # Nothing to write home about

# This data gives me the time period of a haulout, and the position for the haulout. I have tried several methods to synchronize and merge the positional data with the summary data, with little success. 
#----------------------------------------------------------------------


# Add biological variables
#---------------------------------------------------------------------
# Adding weight, sex, and length to the haulout data

library(openxlsx)

seals_df <- read.xlsx("Data_seal/seals_jomfruland.xlsx")
head(seals_df)
str(seals_df)

# Create Sex-column 
summary6_pv74 <- summary5_pv74 %>% 
  group_by(REF) %>% 
  mutate(Sex = ifelse(REF == "pv74-F46_Olivia-20", "F",
                      ifelse(REF == "pv74-M62_Gamle-Erik-20", "M",
                             ifelse(REF  == "pv74-M70_Osito-20", "M",
                                    ifelse(REF == "pv74-M86_Bjorn-20", "M",
                                           ifelse(REF == "pv74-M88_Diego-20", "M")))))) %>% 
  ungroup(REF)

# Factorize Sex variable
class(summary6_pv74)
str(summary6_pv74$Sex)
summary6_pv74$Sex <- as.factor(summary6_pv74$Sex)
levels(summary6_pv74$Sex)

# Create Weight_kg column
summary7_pv74 <- summary6_pv74 %>% 
  group_by(REF) %>% 
  mutate(Weight_kg = ifelse(REF == "pv74-F46_Olivia-20", 46,
                            ifelse(REF == "pv74-M62_Gamle-Erik-20", 62,
                                   ifelse(REF == "pv74-M70_Osito-20", 70,
                                          ifelse(REF == "pv74-M86_Bjorn-20", 86,
                                                 ifelse(REF == "pv74-M88_Diego-20", 88)))))) %>% 
  ungroup(REF)

# Check if ok
head(summary7_pv74$Weight_kg)
str(summary7_pv74)

# Create Length_cm column
summary8_pv74 <- summary7_pv74 %>% 
  group_by(REF) %>% 
  mutate(Length_cm = ifelse(REF == "pv74-F46_Olivia-20", 112,
                            ifelse(REF == "pv74-M62_Gamle-Erik-20", 128,
                                   ifelse(REF == "pv74-M70_Osito-20", 137,
                                          ifelse(REF == "pv74-M86_Bjorn-20", 156,
                                                 ifelse(REF == "pv74-M88_Diego-20", 135)))))) %>% 
  ungroup(REF)

# Check if ok
str(summary8_pv74)



# Last cleanup
#-------------
# Remove tag-column
summary8_pv74 <- summary8_pv74[, -c(9:16)]
summary8_pv74 <- summary8_pv74[, -2]
str(summary8_pv74)
# Factorize nday column
summary8_pv74$nday <- as.factor(summary8_pv74$nday); levels(summary8_pv74$nday)
table(summary8_pv74$nday)

#---------------------------------------------------------------------
# Plot of haulout profiles
plot_a <- ggplot(summary8_pv74, aes(x = CEN.DATE, y = HAUL.TM, color = REF)) +
  geom_point(na.rm = TRUE, size = .5) +
  facet_grid(. ~ Month) + facet_wrap(~REF) +
  scale_x_datetime(date_breaks = "2 months", date_labels = '%Y-%m', limits = as.POSIXct(c("2020-09-19", "2021-04-02"))) +
  ggtitle("Haulout-time (%) October 2020 - April 2021") +
  xlab("Date") +
  ylab("Haulout time (%)")
plot_a

# Plot of mean windspeed
plot_b <- ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Windspeed, color = Month)) +
  geom_point(na.rm = TRUE, size = .9) +
  scale_x_datetime(date_breaks = "1 month", date_labels = '%Y-%m-%d', limits = as.POSIXct(c("2020-09-19", "2021-04-02"))) +
  ggtitle("Mean Windspeed October 2020 - April 2021") +
  xlab("Date") +
  ylab("Mean Windspeed")
plot_b

# Plot of mean tides
plot_c <- ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Tide, color = Month)) +
  geom_point(na.rm = TRUE, size = .9) +
  scale_x_datetime(date_breaks = "1 month", date_labels = '%Y-%m-%d', limits = as.POSIXct(c("2020-09-19", "2021-04-02"))) +
  ggtitle("Mean Tidelevels, Oct. 2020 - Apr. 2021") +
  xlab("Date") +
  ylab("Mean Tidelevel")
plot_c

# Plot of mean temperature
plot_d <- ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Temperature, color = Month)) +
  geom_point(na.rm = TRUE, size = .9) +
  scale_x_datetime(date_breaks = "1 month", date_labels = '%Y-%m-%d', limits = as.POSIXct(c("2020-09-19", "2021-04-02"))) +
  ggtitle("Mean Airtemperatures, Oct. 2020 - Apr. 2021") +
  xlab("Date") +
  ylab("Mean Temperatures, Oct. 2020 - Apr. 2021")
plot_d

# Plot of mean precipitation
plot_e <- ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Precipitation, color = Month)) +
  geom_point(na.rm = TRUE, size = .9) +
  scale_x_datetime(date_breaks = "1 month", date_labels = '%Y-%m-%d', limits = as.POSIXct(c("2020-09-19", "2021-04-02"))) +
  ggtitle("Mean Precipitation, Oct. 2020 - Apr. 2021") +
  xlab("Date") +
  ylab("Mean Precipitation, Oct. 2020 - Apr. 2021")
plot_e

# Plot haulout profiles on solar radiation (night vs. day)
plot_f <- ggplot(summary8_pv74, aes(x = CEN.DATE, y = HAUL.TM, color = nday)) +
  geom_point(na.rm = TRUE, size = .9) +
  facet_grid(. ~ Month) + facet_wrap(~REF) +
  scale_x_datetime(date_breaks = "1 month", date_labels = '%Y-%m-%d', limits = as.POSIXct(c("2020-09-19", "2021-04-02"))) +
  ggtitle("Solar elevation, Oct. 2020 - Apr. 2021") +
  xlab("Date") +
  ylab("Solar Elevation, Oct. 2020 - Apr. 2021")
plot_f


#---------------------------------------------------------------------




#-----------------------------------------------------------------
#### Individual haulout percents by month during sampling period ####
levels(summary8_pv74$REF)


#  Gamle Erik's haulout percent by month
#---------------------------------------
summary8_pv74 %>% 
  group_by(Month) %>% 
  filter(REF == "pv74-M62_Gamle-Erik-20") %>% 
  filter(S.DATE >= as.POSIXct("2020-09-19 18:05:26") & S.DATE <= as.POSIXct("2020-11-14 20:48:54")) %>% 
  summarize(mean(HAUL.TM, na.rm = TRUE)) %>% 
  ungroup()

# Bjorn's haulout percent by month
#--------------------------------
summary8_pv74 %>% 
  group_by(Month) %>% 
  filter(REF == "pv74-M86_Bjorn-20") %>% 
  filter(S.DATE >= as.POSIXct("2020-11-14 14:48:54") & E.DATE <= as.POSIXct("2021-02-12 20:48:53")) %>% 
  summarize(mean(HAUL.TM, na.rm = TRUE)) %>% 
  ungroup()

# Olivia's haulout percent per month
#-----------------------------------
summary8_pv74 %>% 
  group_by(Month) %>% 
  filter(REF == "pv74-F46_Olivia-20") %>% 
  filter(S.DATE >= as.POSIXct("2020-11-14 14:46:18") & E.DATE <= as.POSIXct("2021-03-16 08:46:20")) %>% 
  summarize(mean(HAUL.TM, na.rm = TRUE)) %>% 
  ungroup()

# Osito's haulout percent per month
#---------------------------------
summary8_pv74 %>% 
  group_by(Month) %>% 
  filter(REF == "pv74-M70_Osito-20") %>% 
  filter(S.DATE >= as.POSIXct("2020-11-14 14:51:51") & E.DATE <= as.POSIXct("2021-03-20 08:51:50")) %>% 
  summarize(mean(HAUL.TM, na.rm = TRUE)) %>% 
  ungroup()

# Diego's haulout percent per month
#----------------------------------
summary8_pv74 %>% 
  group_by(Month) %>% 
  filter(REF == "pv74-M88_Diego-20") %>% 
  filter(S.DATE >= as.POSIXct("2020-11-14 14:49:53") & E.DATE <= as.POSIXct("2021-04-02 08:49:53")) %>% 
  summarize(mean(HAUL.TM, na.rm = TRUE)) %>% 
  ungroup()


# Save final data frame for further use
save(summary8_pv74, file = "Haulout_2020.Rdata")
save(latlong_nona, file = "Positions_2020.Rdata")
save(df_tides3, file = "Tides_2020_RAW.Rdata")
save(precip_df, file = "Precip_2020_RAW.Rdata")
save(temp_df, file = "Temp_2020_RAW.Rdata")
save(wind_df, file = "Wind_2020_RAW.Rdata")





