

#########################################################################
########################## DATA EXPLORATION ############################
###################### 2020-21 SKAGERRAK ################################ #########################################################################

# Needful Things
#---------------

# Files
source("Carla_Examples/HighstatLibV13.R") # Functions from Highland Stats
source("getTide_function.R")              # Martin's tide-function
load("Haulout_2020.Rdata")                # Haulout data complete
load("Positions_2020.Rdata")              # Latitude and Longitude data 
load(df_tides3, file = "Tides_2020_RAW.Rdata")  # Raw tides-data
load(precip_df, file = "Precip_2020_RAW.Rdata") # Raw rain-data
load(temp_df, file = "Temp_2020_RAW.Rdata")     # Raw temp-data
load(wind_df, file = "Wind_2020_RAW.Rdata")     # Raw wind-data

# Packages
library(tidyverse) 
library(rSRDL)
library(lubridate)
library(lattice)
library(maptools)
library(openxlsx)
library(anytime)
library(sf)
library(mapview)
library(ggmap)
library(gridExtra)
library(scales)
library(ggridges)



# I will follow the data exploration protocol from Zuur (2010) and the data exploration for beginners-book (Ieno & Zuur, 2015)

#---------------------------------------------------------------------
# Check list
#-----------
# 1: Explore the variables in the SUMMARY DF (summary5_pv74)
  # a: HAUL.TM (percentage/proportion variable)
  # b: DIVE.TM (percentage/proportion variable)
  # c: SURF.TM (percentage/proportion variable)
  # d: REF (categorical (nominal) with 5 levels)

# 2: Explore the solar elevation and moon phase added to the SUMMARY DF
  # a: solar (continuous)
  # b: Moon_Phase (continuous)

# 3: Explore the raw data of environmental variables
  # a: Tide (Kartverket) (continuous)
  # b: Wind, Temperature, Precipitation (seKlima) (continuous)

# 4: Explore the environmental variables adjusted by calculating the mean of each haul-out period to fit the SUMMARY DF
  # a: Mean_Tide (continuous)
  # b: Mean_Windspeed (continuous)
  # c: Mean_Temperature (continuous)
  # d: Mean_Precipitation (continuous)

#------------------------------------------------------------------------
# Protocol
#----------
#1: Outliers in Y and X (boxplots and Cleveland dotplots)
#2: Collinearity in X (VIFs and Scatterplots)
#3: Relationships Y and X (multipanel scatterplots, conditional boxplots)
#4: Homogeneity in Y (conditional boxplot)
#5: Normality in Y (histograms or QQ-plots)
#6: Zero Trouble in Y (frequency plots or corrgrams)
#7: Interactions (coplots)
#8: Independence in Y (ACF and variograms, plot Y vs. time/space)

#------------------------------------------------------------------------

## 1: OUTLIERS in the response (HAUL.TM)?
#----------------------------------------

# Cleveland Dotplot 1 of response variable HAUL.TM
dotchart(summary8_pv74$HAUL.TM,
         ylab = "Order of observations",
         xlab = "Haul-out time", main = "Cleveland Dotplot")

# Dotplot 2 of response variable HAUL.TM
Mydotplot(summary8_pv74[, "HAUL.TM"]) # Many observations of 100% haulout, and many observations of 0%. 

# Cleveland Dotplot 1 of DIVE.TM and SURF.TM
par(mfrow = c(2, 1))
dotchart(summary8_pv74$DIVE.TM,
         yland = "Order of observations",
         xlab = "Dive time", main = "Cleveland Dotplot")
dotchart(summary8_pv74$SURF.TM,
         ylab = "Order of observations",
         xlab = "Surf time", main = "Cleveland Dotplot") # Skewed towards zero observations during periods.



# Boxplot of the response 
boxplot(summary8_pv74$HAUL.TM) # compact, a little more spread down towards the zeros.

# Boxplots of DIVE.TM and SURF.TM
boxplot(summary8_pv74$DIVE.TM, main = "Dive Time")
boxplot(summary8_pv74$SURF.TM, main = "Surf Time") # one possible high value outlier. 


# Boxplots of response on each factor level (the five seals)
boxplot(HAUL.TM ~ factor(REF),
        varwidth = TRUE, xlab = "Individuals",
        ylab = "Haulout time (percent)", main = "Haulout time individual", data = summary8_pv74) # Gamle Erik is making a bit of a mess:) The other seals look ok. Several observations are considerably smaller than the majority of observations.

# Boxplot and Cleveland dotplot side-by-side
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
dotchart(summary8_pv74$HAUL.TM,
         ylab = "Order of data",
         xlab = "Range of data")
boxplot(summary8_pv74$HAUL.TM)

# Conditional boxplot haultime and Month
boxplot(HAUL.TM ~ Month,
        ylab = "Haulout time (percent)",
        xlab = "Month", data = summary8_pv74,
        main = expression(italic("Haulout time")))

# Conditional boxplot haultime and nday
boxplot(HAUL.TM ~ nday,
        ylab = "Haulout time (percent)",
        xlab = "Day & Night", data = summary8_pv74,
        main = expression(italic("Haulout time")))

# Boxplot haultime incl all obs colored by night & day
ggplot(summary8_pv74, aes(x = factor(1), y = HAUL.TM)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = nday, shape = nday), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)

# Conditional boxplot haultime and Month
boxplot(HAUL.TM ~ Month,
        ylab = "Haulout time (percent)",
        xlab = "Month", data = summary8_pv74,
        main = expression(italic("Haulout time")))

# Conditional boxplot haultime and REF
boxplot(HAUL.TM ~ REF,
        ylab = "Haulout time (percent)",
        xlab = "Seals", data = summary8_pv74,
        main = expression(italic("Haulout time")))

# As expected: Gamle Erik was the only one sending in Sept and Oct, so few observations in these months. April is also scarce, obviously because the last sender stopped a couple of days in.

# November - March are the months with the most observations

# Observations by months
table(summary8_pv74$Month) # Dec., Jan., Nov., Feb., Mar., Oct., Sept., Apr. (only 5 obs.).'


# I cannot identify any apparent outliers, except one large value in SURF.TM - a variable I might not use.



## 2. RELATIONSHIPS between the response and the covariates
#----------------------------------------------------------

# First thoughts: This step is for me to inspect data to visualize if there are associations between the variables. If relationships, I need to decide if it's linear or nonlinear, etc. NB! Would be wise to describe the associations mathematically, too.

# Boxplots of haultime during night and day
boxplot(summary8_pv74$HAUL.TM ~ summary8_pv74$nday) # Higher percent during nights than days
table(summary8_pv74$nday) # Winter months = more nights than days

# Boxplot of divetime during night and day
boxplot(summary8_pv74$DIVE.TM ~ summary8_pv74$nday) # It corresponds with haultime


# Pairplots #
  # HAUL.TM and all covariates
pairs(summary8_pv74[c(7, 8, 10:14)],
cex = .6,
lower.panel = NULL) # I think I see some weak relationships between HAUL.TM and some of the environmental variables

  # HAUL.TM and some covariates
pairs(summary8_pv74[c(7, 10, 12:14)],
      cex = .7,
      lower.panel = NULL) # It is a little difficult to infer relationships. Alittle more haulouts during warm weather? Alittle more haulouts with increase in precipitation? A little less haulout with high windspeeds?


# Not to be used further! Add logit transformed HAUL.TM to see if it will make it easier to visualize:
#--------------------------
# Vector of HAUL.TM
haul_time <- summary8_pv74$HAUL.TM

# From percentage to proportions
prop_haul_time <- prop.table(haul_time)

# Logit transform haul time
logit_haul_time <- logit(prop_haul_time)

# Plot logit
plot(logit_haul_time)

# Add to data frame
summary8_pv74$logit.HAUL.TM <- logit_haul_time

names(summary8_pv74)

# Pairplots with logit transformet haul time
pairs(summary8_pv74[c(20, 10, 12:14)],
      cex = .8,
      lower.panel = NULL) # Less observations at large values of wind, precipitation and high temperatures, but if I squint there might be a weak relationship here. Weak relationship between haultime and tide (less haulouts at high tide).
#-----------------------------------------------------------------------

# Multipanel Scatterplot
MyVar <- c("HAUL.TM", "Mean_Tide", "Mean_Precipitation", "Mean_Windspeed", "Mean_Temperature")
pairs(summary8_pv74[, MyVar], upper.panel = panel.smooth)
# Weak linear relationship between Haul.TM and Tide, and Haul.TM and Temperature.



#----------------------------------------------------------------

## 3. COLLINEARITY and confounding?
#-----------------------------------

# First thoughts: I expect to find correlation between some of the explanatory variables, and multicollinearity.

# Pairplot of selected explanatory variables (linear relationships?) using Pearson's Correlation Coefficient and p-value
#--------------------------------------------------
MyVar <- c("Mean_Tide", "Mean_Precipitation", "Mean_Windspeed", "Mean_Temperature")
pairs(summary8_pv74[, MyVar], upper.panel = panel.smooth, lower.panel = panel.cor, method = "pearson", cex.cor = 1, col = "blue", pch = 21, bg = "gray80")

# Strongest linear relationship between Tide + Temperature and Wind + Temperature. Close to none between Tide and Precipitation.

# Pairplots with Pearson statistics on all proportional data and some environmental variables
#----------------------
MyVar2 <- c("HAUL.TM", "DIVE.TM", "SURF.TM", "Mean_Tide", "Mean_Temperature")
pairs(summary8_pv74[, MyVar2], upper.panel = panel.smooth, lower.panel = panel.cor, method = "pearson", cex.cor = 1, col = "blue", pch = 21, bg = "gray80")

# Obviously, strong correlation between the proportional data. 

#### CORRELATION FUNCTION (https://www.gardenersown.co.uk) ####
#------------------------------------------------------------------
# Function for the correlation statistics (all methods can be used)
panel.cor <- function(x, y, cex.cor = 0.8, method = "pearson", ...) {
  options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
  usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
  par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
  r <- cor(x, y, method = method, use = "pair")               # correlation coef
  p <- cor.test(x, y, method = method)$p.val                  # p-value
  n <- sum(complete.cases(x, y))                              # How many data pairs
  txt <- format(r, digits = 3)                                # Format r-value
  txt1 <- format(p, digits = 3)                                 # Format p-value
  txt2 <- paste0("r= ", txt, '\n', "p= ", txt1, '\n', 'n= ', n) # Make panel text
  text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
  options(warn = 0)                                             # Reset warning
}
# End
########################################################################


# Correlation due to interactions?
# Interaction between temperature and wind
summary8_pv74$TW <- summary8_pv74$Mean_Temperature * summary8_pv74$Mean_Windspeed
MyVar4 <- c("Mean_Tide", "Mean_Precipitation", "Mean_Windspeed", "Mean_Temperature", "TW")
pairs(summary8_pv74[, MyVar4],
      upper.panel = panel.smooth, lower.panel = panel.cor, method = "pearson", cex.cor = 1, col = "blue", pch = 21, bg = "gray80")

# The interaction term actually gives a pretty high correlation coefficient with tide.

# Correlation between HAUL.TM, Mean_Tide and TW?
MyVar5 <- c("HAUL.TM", "Mean_Tide", "TW")
pairs(summary8_pv74[, MyVar5],
      upper.panel = panel.smooth, lower.panel = panel.cor, method = "pearson", cex.cor = 1, col = "blue", pch = 21, bg = "gray80")

# Not so much

# Multipanel Boxplots showing relationship between continuous explanatory variables and the categorical REF, nday and Month.

# Factorize Month
summary8_pv74$fMonth <- factor(summary8_pv74$Month)



#---------------------------------------------------------------------


## 4. Zeros for the response variable
#----------------------------------------------------
# Numb
numZero <- colSums(summary8_pv74 == 0, na.rm = T)
numZero # There are 251 zeros or 10.78% zeros in the response variable. 


#------------------------------------------------------------------


## 5. Time series nature of the data
#----------------------------------------

## Tide through the whole period
ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Tide)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 10))

## Tide through each month
ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Tide)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 10))+
  facet_wrap(~Month, scale = "free", ncol = 2)


## Wind through the whole period
ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Windspeed)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 13))


## Temperature through the whole period
ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Temperature)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 13))

## Neat plot of temperature profiles per month
class(summary8_pv74$Month)
summary8_pv74$fMonth <- factor(summary8_pv74$Month)
ggplot(
  summary8_pv74, 
  aes(x = Mean_Temperature, y = fMonth)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Temp. [C]"
  )+
  labs(title = 'Temperatures in Skagerrak 2020-21') 


## Precipitation through the whole period
ggplot(summary8_pv74, aes(x = CEN.DATE, y = Mean_Precipitation)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 13))

## Solar elevation through the whole period
ggplot(summary8_pv74, aes(x = CEN.DATE, y = solar)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 13)) # cool plot

## Moon phases through the whole period
ggplot(summary8_pv74, aes(x = CEN.DATE, y = Moon_Phase)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 13)) # S'nice and perfectly reasonable

###

## RESPONSE haulout through the whole period
ggplot(summary8_pv74, aes(x = CEN.DATE, y = HAUL.TM)) +
  geom_line(aes(color = Month)) +
  theme(text = element_text(size = 13))

## Haulout through each month - quite like this plot
ggplot(summary8_pv74, aes(x = CEN.DATE, y = HAUL.TM)) +
  geom_line(aes(color = nday)) +
  theme(text = element_text(size = 10))+
  facet_wrap(~Month, scale = "free", ncol = 2)



# 6. Spatial locations of the sampling locations / Spatial dependence?
#--------------------------------------------------------------------

## NB!! NOT DONE, NEED TO PRACTICE GEOMAPPING in R


# First thoughts: Have to convert txt.file (Positions_df) to ShapeFile. Are the variables randomly distributed in space, or are they correlated with the spatial location?




# Fumble
#--------

library(OpenStreetMap)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)

# Load shapefile for Norway
#Norway <- read_sf("C:/Users/ninab/Desktop/SHAPEfiles/no_1km.shp")

#head(Norway)

# Create positions object
#positions_sf <- st_as_sf(positions_df, coords = c("lon", "lat"), crs = #4326)

# Check the sf file
#str(positions_sf)
#class(positions_sf)

# Plot the spatial object
#ggplot() +
#  geom_sf(data = positions_sf) +
#  ggtitle("Map of Plot Locations")

#ggplot() +
#  geom_sf(data = positions_sf) +
 # geom_point(data = summary8_pv74, aes(x = HAUL.TM), color = "blue")


# Extract map
#SkagerrakMap <- openmap(c(59.6, 8.8),
                        c(58.1, 13.9))
# plot map
#plot(SkagerrakMap) # Sucks

# New map
#world <- ne_countries(scale = "small", returnclass = "sf")
class(world)

# Positions only
#positions_only <- positions_sf$geometry
#pos_latlong <- positions_df[, 1:2]
#head(pos_latlong)

# Map of countries
#ggplot(data = world) +
 # geom_sf() +
 # labs( x = "Longitude", y = "Latitude") +
 # ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)"))

# Map of Skagerrak
ggplot(data = Norway) +
  geom_sf() +
  coord_sf(xlim = c(57.247, 60.384), ylim = c(6.790, 17.007))

#------- END NOT DONE ------------------------------------




##### Explore the environmental variables (raw data) #######

### Outliers and Compare with Processed Variable Data
#----------
head(df_tides3)
dim(df_tides3)
head(precip_df)
dim(precip_df)
head(temp_df)
head(wind_df)

# Dotplot and boxplot of tide values raw vs mean tide processed
#--------------------------------------------------------------
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
dotchart(df_tides3$tide_level,
         ylab = "Order of data",
         xlab = "Range of data",
         main = "Tide values")
boxplot(df_tides3$tide_level,
        main = "Tide values") # Some high and low values in boxplot, but the dotplot shows them as part of the tide cycle
dotchart(summary8_pv74$Mean_Tide,
         ylab = "Order of data",
         xlab = "Range of data",
         main = "Mean Tide")
boxplot(summary8_pv74$Mean_Tide, 
        main = "Mean Tide")

# Dotplot and boxplot of temperature values
#------------------------------------------
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
dotchart(temp_df$Air_Temp,
         ylab = "Order of data",
         xlab = "Range of data")
boxplot(temp_df$Air_Temp) # Many zeros
dotchart(summary8_pv74$Mean_Temperature,
         ylab = "Order of data",
         xlab = "Range of data",
         main = "Mean Temperature")
boxplot(summary8_pv74$Mean_Temperature, 
        main = "Mean Temperature") # NB! The Dotplot of Mean Temperature has different ranges. Might have to do something about that...

# Dotplot and boxplot of precipitation values
#--------------------------------------------
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
dotchart(precip_df$Precipitation,
         ylab = "Order of data",
         xlab = "Range of data")
boxplot(precip_df$Precipitation) # Nothing out of the ordinary. Boxplot point not strange
dotchart(summary8_pv74$Mean_Precipitation,
         ylab = "Order of data",
         xlab = "Range of data",
         main = "Mean Precipitation")
boxplot(summary8_pv74$Mean_Precipitation, 
        main = "Mean Precipitation") # The range of the raw data has larger (few) values between 3 and 8. The range in arithmetic mean precip is compressed between 0 and 2, as expected. The boxplots show same trend. Maybe calculate the average differently. Could make two mean - one as is with zeros, and one without zeros, and report the difference. The median is very low, so it would not change the range from mean much. 
colSums(precip_df == 0, na.rm = T) # 3773 zeros
dim(precip_df) # of 4672 data points
3773/4672 * 100 # Wow - 80.8% zeros in precipitation

# Dotplot and boxplot of wind values
#-----------------------------------
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
dotchart(wind_df$Mean_windspeed,
         ylab = "Order of data",
         xlab = "Range of data")
boxplot(wind_df$Mean_windspeed) # Few points on larger values, most between 4 and 9
dotchart(summary8_pv74$Mean_Windspeed,
         ylab = "Order of data",
         xlab = "Range of data",
         main = "Mean of Mean Windspeed")
boxplot(summary8_pv74$Mean_Windspeed, 
        main = "Mean of Mean Windspeed") # Same range, more or less same pattern. Few points in larger orders of data (15-20), so outside the box and whiskers.








