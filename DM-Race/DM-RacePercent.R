########################################################################################
# File: DM-RacePercent.R
# Date: 12/31/16 
# Author: Vincent Palacios
########################################################################################



########################################################################################
# 0. BRIEF DESCRIPTION
########################################################################################
# This 


########################################################################################
# 1. CLEAR ENVIRONMENT, REQUIRE PACKAGES NEEDED
########################################################################################
#remove all objects
rm(list=ls())

#Load packages for use. Note they each must be installed already using: install.packages("PACK_NAME")
x <- c("ggplot2", "rgdal", "maptools", "rgeos", "raster", "acs", "grid", "gridExtra") #, "mapproj")
lapply(x, library, character.only = TRUE)

sessionInfo()

# Move to the 'datadir' folder 
datadir <- "/Users/vincent/VP_GitLibrary/R-ACS-Maps" 
setwd(datadir)

# Check to see if the folder "cb_2015_us_state_20m" already exists. If not, create it.
if (!file.exists("cb_2015_us_state_20m")) {
  dir.create("cb_2015_us_state_20m")
}

# Check to see if the folder "cb_2015_us_county_20m" already exists. If not, create it.
if (!file.exists("cb_2015_us_county_20m")) {
  dir.create("cb_2015_us_county_20m")
}

# Check to see if the folder "cb_2015_us_county_20m" already exists. If not, create it.
if (!file.exists("DM-Race")) {
  dir.create("DM-Race")
}



########################################################################################
# 2. ACCESS ACS API AND STORE RACEDATA_EXC
########################################################################################
# Use your ACS API key here with parentheses
# api.key.install("YOUR KEY HERE", file = "key.rda")

# Create complex geo call so that all counties for all states are querried
for (statenum in fips.state[1:51,1]) {
  if (statenum == 1) allcounties <- geo.make(state = statenum, county = "*") else
    allcounties <- allcounties + geo.make(state = statenum, county = "*")
}


# GET EXCLUSIVE RACE CATEGORIES
#Look at variables in table B03002_001E
acs.lookup(2015, table.name = "B03002")

# List of variables that will be called in API
Table_B03002 <- acs.lookup(2015, table.name = "B03002")
selected_vars <- Table_B03002[c(1, 3, 4, 5, 6, 7, 8, 9, 12)]

# Query API based on variable names and specified geographies
ACSdata_exc <- acs.fetch(2015, span = 5, geography = allcounties,
                     variable = selected_vars, dataset = "acs", col.names = "auto")

#Store results in a data.frame for mapping
racedata_exc <- data.frame(cbind(geography(ACSdata_exc),estimate(ACSdata_exc)))

#Rename variables to more familiar terms
racedata_exc <- rename(racedata_exc, c("B03002_001" = "TOTPOP", "B03002_012" = "HISPANICS", "B03002_003" = "WHITE_NH", 
                                       "B03002_004" = "BLACK_NH", "B03002_005" = "AIAN_NH", "B03002_006" = "ASIAN_NH", 
                                       "B03002_007" = "NHOPI_NH", "B03002_008" = "OTHER_NH", "B03002_009" = "MULTI_NH"))

#Add in Non-White category
racedata_exc <- mutate(racedata_exc,
                       NonWhite = TOTPOP - WHITE_NH)

#Check that categories all combine to total population
racedata_exc$check <- apply(racedata_exc[,c("HISPANICS", "WHITE_NH", "BLACK_NH", "AIAN_NH", 
                                            "ASIAN_NH", "NHOPI_NH", "OTHER_NH", "MULTI_NH")], 1, sum)
sum(racedata_exc$check == racedata_exc$TOTPOP) == 3142 

# Add geographic identifiers for joining table with shapefile
racedata_exc$state  <- formatC(racedata_exc$state , width = 2, flag = "0")
racedata_exc$GEOID  <- paste0(racedata_exc$state,racedata_exc$county)



########################################################################################
# 3. ACCESS ACS API AND STORE RACEDATA_INC
########################################################################################
# GET INCLUSIVE RACE CATEGORIES
selected_vars <- c("B01001_001", "B01001H_001", "B01001I_001", "B02009_001", "B02010_001", "B02011_001", "B02012_001", "B02013_001")

# Query API based on variable names and specified geographies
ACSdata_inc <- acs.fetch(2015, span = 5, geography = allcounties,
                     variable = selected_vars, dataset = "acs", col.names = "auto")

#Store results in a data.frame for mapping
racedata_inc <- data.frame(cbind(geography(ACSdata_inc),estimate(ACSdata_inc)))

#Rename variables to more familiar terms
racedata_inc <- rename(racedata_inc, c("B01001_001" = "TOTPOP", "B01001I_001" = "HISPANICS", "B01001H_001" = "WHITE_NH", 
                                       "B02009_001" = "BLACK_AOIC", "B02010_001" = "AIAN_AOIC", "B02011_001" = "ASIAN_AOIC", 
                                       "B02012_001" = "NHOPI_AOIC", "B02013_001" = "OTHER_AOIC"))

# Add geographic identifiers for joining table with shapefile
racedata_inc$state  <- formatC(racedata_inc$state , width = 2, flag = "0")
racedata_inc$GEOID  <- paste0(racedata_inc$state,racedata_inc$county)



########################################################################################
# 4. CREATE ANALYTIC VARAIBLES TO BE MAPPED, TURN INTO FACTOR VARIABLES
########################################################################################
# FOR EXCLUSIVE RACE CATEGORIES
for (demo in colnames(racedata_exc)[5:13]) {
  racedata_exc[[toString(paste0(demo, "_pct"))]] <- 
    racedata_exc[[toString(demo)]] / racedata_exc$TOTPOP
  #racedata_exc[[toString(paste0(demo, "_pct"))]][is.na(racedata_exc[[toString(paste0(demo, "_pct"))]])] <- -1
}

for (level in colnames(racedata_exc)[5:13]) {
  racedata_exc[[toString(paste0(level, "_pct_lvl"))]] <- 
    cut(racedata_exc[[toString(paste0(level, "_pct"))]],
        breaks = c(0, 0.05, 0.1, 0.25, 0.33, 0.5, 0.75, 0.9, 0.95, 1),
        include.lowest = T)
}

  
# FOR INCLUSIVE RACE CATEGORIES
for (demo in colnames(racedata_inc)[5:11]) {
  racedata_inc[[toString(paste(demo, "_pct", sep=""))]] <- 
    racedata_inc[[toString(demo)]]/ racedata_inc$TOTPOP
}

for (level in colnames(racedata_inc)[5:11]) {
  racedata_inc[[toString(paste0(level, "_pct_lvl"))]] <- 
    cut(racedata_inc[[toString(paste0(level, "_pct"))]], 
        breaks = c(0, 0.05, 0.1, 0.25, 0.33, 0.5, 0.75, 0.9, 0.95, 1),
        include.lowest = T)
}



########################################################################################
# 5. RETRIEVE SHAPEFILES FROM CENSUS
########################################################################################
# Move to the subfolder "cb_2015_us_state_20m"
setwd("./cb_2015_us_state_20m")

# Check to see if the file "cb_2015_us_state_20m.shp" already exists. If not, dowload and unzip 
# the Census 2015 State Cartographic Boundary shapefiles.
if (!file.exists("cb_2015_us_state_20m.shp")) {
  fileUrl <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_20m.zip"
  download.file(fileUrl, destfile = "cb_2015_us_state_20m.zip")
  unzip("cb_2015_us_state_20m.zip")
}

# Move to the subfolder "cb_2015_us_county_20m"
setwd("../cb_2015_us_county_20m")

# Check to see if the file "cb_2015_us_county_20m.shp" already exists. If not, dowload and unzip
# the Census 2015 County Cartographic Boundary shapefiles.
if (!file.exists("cb_2015_us_county_20m.shp")) {
  fileUrl <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_20m.zip"
  download.file(fileUrl, destfile = "cb_2015_us_county_20m.zip")
  unzip("cb_2015_us_county_20m.zip")
}



########################################################################################
# 6. READ, PROJECT, AND TRANSFORM COUNTY SHAPEFILE
########################################################################################
# Read in 2015 Census county shapfile
county <- readOGR(toString(paste0(datadir,"/cb_2015_us_county_20m")), 
                  "cb_2015_us_county_20m", stringsAsFactors = FALSE)
summary(county)

# Remove PR from county shapefile
county <- county[!(county$STATEFP %in% "72"), ]

# Project shapefile using albers equal area
US_Proj <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 
               +b=6370997 +units=m +no_defs")
county <- spTransform(county, US_Proj)

#Rotate and scale Alaska
alaska <- county[county$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(county)

#Rotate Hawaii
hawaii <- county[county$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(county)

#Remove Alaska and Hawaii from US shapefile and substitute transformed versions
county <- county[!county$STATEFP %in% c("02", "15"),]
county <- rbind(county, alaska, hawaii)
plot(county)



########################################################################################
# 6. READ, PROJECT, AND TRANSFORM COUNTY SHAPEFILE
########################################################################################
# Repeat above but for state shapefile
# Read in 2015 Census state shapfile
state <- readOGR(toString(paste0(datadir,"/cb_2015_us_state_20m")), 
                  "cb_2015_us_state_20m", stringsAsFactors = FALSE)
summary(state)

# Remove PR from state shapefile
state <- state[!(state$STATEFP %in% "72"), ]

# Project shapefile using albers equal area
US_Proj <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 
               +b=6370997 +units=m +no_defs")
state <- spTransform(state, US_Proj)

#Rotate and scale Alaska
alaska <- state[state$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(state)

#Rotate Hawaii
hawaii <- state[state$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(state)

#Remove Alaska and Hawaii from US shapefile and substitute transformed versions
state <- state[!state$STATEFP %in% c("02", "15"),]
state <- rbind(state, alaska, hawaii)
plot(state)



########################################################################################
# 7. PREP OBJECTS FOR GGPLOT
########################################################################################
# Disolve state layer into outline of continental US
country <- gUnaryUnion(state)
plot(country)

# Fortify shapefiles so they can be used with ggplot()
county_f <- fortify(county, region = "GEOID")
state_f <- fortify(state, region = "GEOID")
country_f <- fortify(country)

# Merge in race data (exclusive categories) with fortified county shapefile
colnames(county_f)[which(colnames(county_f) == "id")] = "GEOID"
county_f <- join(county_f, racedata_exc[, c(15,25:33)], "GEOID")
head(county_f)
str(county_f)



########################################################################################
# 8. MAP DATA
########################################################################################
library(RColorBrewer)
colors <- brewer.pal(9, "oranges")  #c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b")
sp_minimal <- theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
raceGroups <- colnames(county_f)[c(8:12,14:16)]
raceTitles <- c("White", "Black", "Native American", 
                "Asian", "Pacific Islander", #"Other",
                "Multiracial", "Hispanic", "Non-White")
label.values <- c("  0-5%", " 5-10%", "10-25%", "25-33%", "33-50%", "50-75%", "75-90%", "90-95%", "  +95%")

for (demo in raceGroups) {
                  
  i <- which(raceGroups %in% demo)
  scale_fill_vpcustom <- scale_fill_manual(values=colors, 
    name = paste0("Percentage of Total\nCounty Population\nthat is ",raceTitles[i]),
    labels=label.values)
    
  tempplot <- ggplot() + 
    geom_polygon(data = county_f, colour = NA,
                 aes_string(
                   x = "long", y = "lat", group = "group",
                   fill = toString(demo))) +
    scale_fill_vpcustom +
    geom_polygon(data = state_f,
                 colour = "grey75", size = 0.25, fill = NA,
                 aes(x = long, y = lat, group = group)) + 
    geom_polygon(data = country_f,
                 colour = "black", size = 0.5, fill = NA,
                 aes(x = long, y = lat, group = group)) + 
    coord_equal() + sp_minimal + 
    labs(title = paste0("Race and Ethnicity in the US by County:\nPercentage of the Population that is ", raceTitles[i], ", 2011-2015*"))

  grid.newpage()
  Notes   <- "*Census classifies race and ethnicity as different concepts. All race groups are single race alone and non-Hispanic, except Hispanics which can be of any race.\n"
  Sources <- "Sources: Shapefiles are Census 2015 Cartographic Boundary files. Race/Ethnic data come from Census 2011-2015 ACS 5-year data. For further information on\nmethods and source code see: https://github.com/vincentpalacios/R-ACS-Maps/tree/master/DM-Race"
  g <- arrangeGrob(tempplot, bottom = textGrob(paste(Notes,Sources, sep ="\n"), x = 0.01, y = 0.8, just="left", 
                                               gp = gpar(fontface = "italic", fontsize = 8)))
  ggsave(plot = g,
         filename = toString(paste(datadir, "/DM-Race/", demo,".png",sep="")),
         width = 8, height = 6)
}



########################################################################################
# SELECTED REFERENCES USED IN CREATION OF THIS FILE
########################################################################################
#http://rforpublichealth.blogspot.com/2015/10/mapping-with-ggplot-create-nice.html
#http://eglenn.scripts.mit.edu/citystate/category/uncategorized/
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
#http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
#http://statmodeling.com/best-way-to-add-a-footnote-to-a-plot-created-with-ggplot2.html
#http://gitref.org/basic/



########################################################################################
# ISSUES
########################################################################################



########################################################################################
# END OF FILE
########################################################################################