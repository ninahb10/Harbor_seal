
### Position data all haulout and dive locations for QGIS ###

library("rSRDL")

# Load up the data!
#------------------
# 2020
pv74 <- get.all.SRDLdb("pv74", "C:/Users/ninab/Documents/Masteroppgave_Haulout/Harbor_seal/Data_seal/Harbor seal GSM 2020")
# Positions-df 2020
haulout_2020 <- pv74$haulout_orig
# Quick look
dim(haulout_2020)
summary(haulout_2020)
str(haulout_2020)
levels(haulout_2020)
haulout_2020$REF <- as.factor(haulout_2020$REF)
levels(haulout_2020)
str(haulout_2020)
class(haulout_2020$REF)
table(haulout_2020$REF)
# Delete NA rows
haulout_2020 <- haulout_2020[-c(1:38, 3515:3521), ]
# Remove obsolete levels
h_REF <- c("pv74-F46_Olivia-20", "pv74-M62_Gamle-Erik-20", "pv74-M70_Osito-20", "pv74-M86_Bjorn-20", "pv74-M88_Diego-20")
h_summ <- haulout_2020 %>%
  filter(REF %in% h_REF)
nlevels(h_summ$REF) # Still 12 levels
# Use droplevels() to drop the non-informative levels:
haulout_2020 <- h_summ %>% 
  droplevels()
nlevels(haulout_2020$REF) # 5 levels
dim(haulout_2020)

# 2019
pv68 <- get.all.SRDLdb("pv68", "C:/Users/ninab/Documents/Masteroppgave_Haulout/Harbor_seal/Data_seal/Harbour seal GSM 2019")
# Positions-df 2019
haulout_2019 <- pv68$haulout_orig
# Quick look
dim(haulout_2019)
summary(haulout_2019)
str(haulout_2019)
# Order by REF
haulout_2019 <- haulout_2019[order(haulout_2019$REF), ]
levels(haulout_2019$REF)
class(haulout_2019$REF)
# Filter: Remove rows with data pre-tagging.
haulout_2019 <- haulout_2019 %>% 
  filter(S.DATE > ymd_hms("2019-11-14 14:00:00"))
head(haulout_2019) # Looks good

haulout2_2019 <- pv68$haulout
dim(haulout2_2019)
haulout3_2019 <- pv68$dive
dim(haulout3_2019)
names(haulout3_2019)
summary(haulout3_2019$LAT)
gps <- pv68$gps
dim(gps)
names(gps)
summary(gps)




# 2017
pv35b <- get.all.SRDLdb("pv35b", "C:/Users/ninab/Documents/Masteroppgave_Haulout/Harbor_seal/Data_seal/Harbour seal GSM 2017")
# Positions-df 2017
haulout_2017 <- pv35b$haulout_orig
# Quick look
dim(haulout_2017)
summary(haulout_2017)
str(haulout_2017)
class(haulout_2017$REF)
nlevels(haulout_2017$REF) # 7
# Order by REF
haulout_2017 <- haulout_2017[order(haulout_2017$REF), ]
head(haulout_2017)
table(haulout_2017$REF) # Should have processed the data before doing this, but ok. Just keep the refs with observations
# Remove obsolete levels
h_REF <- c("pv35b-04-11", "pv35b-05-11", "pv35b-06-11", "pv35b-08-11", "pv35b-09-11", "pv35b-10-11")
h_summ <- haulout_2017 %>%
  filter(REF %in% h_REF)
nlevels(h_summ$REF) # Still 12 levels
# Use droplevels() to drop the non-informative levels:
haulout_2017 <- h_summ %>% 
  droplevels()
nlevels(haulout_2017$REF)
# Remove NA rows
haulout_2017 <- haulout_2017[-c(1:10, 138, 163), ]



# 2010
pv35 <- get.all.SRDLdb("pv35", "C:/Users/ninab/Documents/Masteroppgave_Haulout/Harbor_seal/Data_seal/Harbour seal GSM 2010")
# Positions-df 2010
haulout_2010 <- pv35$haulout_orig
# Quick look
dim(haulout_2010) # 6 NAs in long and lat
summary(haulout_2010)
str(haulout_2010) # Ref factor with 3 levels
class(haulout_2010$REF)
nlevels(haulout_2010$REF) # 3 levels
table(haulout_2010$REF) # All three have lots of observations
# Order factor level
haulout_2010 <- haulout_2010[order(haulout_2010$REF), ]
# Remove NA rows
haulout_2010 <- haulout_2010 %>% 
  drop_na("lat")
summary(haulout_2010) # worked like a charm


# 2009
pv30 <- get.all.SRDLdb("pv30", "C:/Users/ninab/Documents/Masteroppgave_Haulout/Harbor_seal/Data_seal/Harbour seal GSM 2009")
# Positions-df 2009
haulout_2009 <- pv30$haulout_orig
# Quick look
dim(haulout_2009) #75 NAs in long and lat
summary(haulout_2009)
str(haulout_2009) # Ref factor with 13 levels
class(haulout_2009$ref)
nlevels(haulout_2009$ref) # 13 levels
table(haulout_2009$ref) # Observations in all levels
# Order factor level
haulout_2009 <- haulout_2009[order(haulout_2009$ref), ]
# Remove NA rows
haulout_2009 <- haulout_2009 %>% 
  drop_na("lon")
summary(haulout_2009) # looks good

# 2008
gp10 <- get.all.SRDLdb("gp10", "C:/Users/ninab/Documents/Masteroppgave_Haulout/Harbor_seal/Data_seal/Harbour seal GSM 2008")
# Positions-df 2008
haulout_2008 <- gp10$haulout_orig
# Quick look
dim(haulout_2008)
# Remove SEQ-column
names(haulout_2008)
haulout_2008 <- haulout_2008[, -c(10:11)]
head(haulout_2008)
summary(haulout_2008) # 33 NAs in LAT and LON
str(haulout_2008) # Ref factor with 5 levels
class(haulout_2008$ref)
nlevels(haulout_2008$ref) # 5 levels
table(haulout_2008$ref) # Observations in all levels
# Order factor level
haulout_2009 <- haulout_2008[order(haulout_2008$ref), ]
# Remove NA rows
haulout_2008 <- haulout_2008 %>% 
  drop_na("LON")
summary(haulout_2008) # looks good



# Save the data as .csv files destined to the QGIS-folder
write.csv(haulout_2020, "C:/Users/ninab/Documents/QGIS/Haulout_2020.csv", row.names = F)
write.csv(haulout_2019, "C:/Users/ninab/Documents/QGIS/Haulout_2019.csv", row.names = F)
write.csv(haulout_2017, "C:/Users/ninab/Documents/QGIS/Haulout_2017.csv", row.names = F)
write.csv(haulout_2010, "C:/Users/ninab/Documents/QGIS/Haulout_2010.csv", row.names = F)
write.csv(haulout_2009, "C:/Users/ninab/Documents/QGIS/Haulout_2009.csv", row.names = F)
write.csv(haulout_2008, "C:/Users/ninab/Documents/QGIS/Haulout_2008.csv", row.names = F)


## Alternative positions: load up the DIVE data
#---------------------------------------------
# 2020
dive_2020 <- pv74$dive
names(dive_2020)
str(dive_2020)
# Filter relevant columns
dive_2020 <- dive_2020 %>% 
  select(REF, DE.DATE, LAT, LON)
names(dive_2020)
dim(dive_2020)
summary(dive_2020)
# Remove NAs
dive_2020 <- dive_2020 %>% 
  drop_na("LAT")
dim(dive_2020)
summary(dive_2020)

# 2019
dive_2019 <- pv68$dive
names(dive_2019)
# Filter relevant columns
dive_2019 <- dive_2019 %>% 
  select(REF, DE.DATE, LAT, LON)
names(dive_2019)
dim(dive_2019)
summary(dive_2019)
# No NAs, so ok

# 2017
dive_2017 <- pv35b$dive
names(dive_2017)
# Select relevant columns
dive_2017 <- dive_2017 %>% 
  select(REF, DE.DATE, LAT, LON)
names(dive_2017)
summary(dive_2017)
# Remove NAs
dive_2017 <- dive_2017 %>% 
  drop_na("LAT")
summary(dive_2017)

# 2010
dive_2010 <- pv35$dive
names(dive_2010)
# Select relevant columns
dive_2010 <- dive_2010 %>% 
  select(REF, DE.DATE, LAT, LON)
names(dive_2010)
summary(dive_2010)
dim(dive_2010)
# Remove NAs
dive_2010 <- dive_2010 %>% 
  drop_na("LAT")
summary(dive_2010)

# 2009
dive_2009 <- pv30$dive
names(dive_2009)
# Select relevant columns
dive_2009 <- dive_2009 %>% 
  select(ref, DE.DATE, LAT, LON)
names(dive_2009)
summary(dive_2009)
#Remove NAs
dive_2009 <- dive_2009 %>% 
  drop_na("LAT")
summary(dive_2009)

# 2008
dive_2008 <- gp10$dive
summary(dive$LAT)
# Filter only relevant latitudes
dive_2008 <- dive_2008[dive_2008$LAT < 70,]
dim(dive_2008)
summary(dive_2008$LAT)
names(dive_2008)
# Select relevant columns
dive_2008 <- dive_2008 %>% 
  select(ref, DE.DATE, LAT, LON)
names(dive_2008)
summary(dive_2008)
# Remove NAs
dive_2008 <- dive_2008 %>% 
  drop_na("LAT")
summary(dive_2008)



# Save the data as .csv files destined to the QGIS-folder
write.csv(dive_2020, "C:/Users/ninab/Documents/QGIS/Dive_2020.csv", row.names = F)
write.csv(dive_2019, "C:/Users/ninab/Documents/QGIS/Dive_2019.csv", row.names = F)
write.csv(dive_2010, "C:/Users/ninab/Documents/QGIS/Dive_2010.csv", row.names = F)
write.csv(dive_2017, "C:/Users/ninab/Documents/QGIS/Dive_2017.csv", row.names = F)
write.csv(dive_2009, "C:/Users/ninab/Documents/QGIS/Dive_2009.csv", row.names = F)
write.csv(dive_2008, "C:/Users/ninab/Documents/QGIS/Dive_2008.csv", row.names = F)
