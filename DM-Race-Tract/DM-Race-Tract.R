########################################################################################
# File: DEMOGRAPHIC MAPS.R
# Date: 3/16/17 
# Author: Vincent Palacios
########################################################################################
# Equalize colors in two-tone 10 class scale
# Combine shapefiles; see bottom note
# Remove colors
# use state 500K shapefiles


########################################################################################
# 0. BRIEF DESCRIPTION
########################################################################################



########################################################################################
# 1. CLEAR ENVIRONMENT, REQUIRE PACKAGES NEEDED
########################################################################################
#remove all objects
rm(list=ls())

#Load packages for use. Note they each must be installed already using: install.packages("PACK_NAME")
x = c("ggplot2", "rgdal", "maptools", "rgeos", "raster", "acs", "grid", "gridExtra") #, "mapproj")
lapply(x, library, character.only = TRUE)

sessionInfo()

#set working directory
#mapdir <-"C:/Users/palacios/SharePoint/Data - Stata/Shapefiles"
#datadir <-"C:/Users/palacios/OneDrive for Business/S/Research/RacialEquity/tractsMaps/"

if (Sys.getenv("USERNAME") == "palacios") {
  mapdir <-"C:/Users/palacios/Center on Budget and Policy Priorities/Data - Stata/Shapefiles"
  datadir <-"C:/Users/palacios/OneDrive - Center on Budget and Policy Priorities/S/Research/RacialEquity/maps/tracts/"
} else if (Sys.getenv("USER") == "vincent") {
  mapdir <-"/Users/vincent/SharePoint/Data - Stata/Shapefiles"
  datadir <-"/Users/vincent/OneDriveBusiness/S/Research/RacialEquity/maps/tracts/"
}



########################################################################################
# 2. READ, PROJECT, AND TRANSFORM tracts SHAPEFILE
########################################################################################
# Search state name strings here: fips.state[,3]
yourstatename <- "District of Columbia"
yourstatefips <- fips.state[grep(yourstatename,fips.state[ , 3]), 1]
yourstatefips00 <- formatC(yourstatefips , width = 2, flag = "0")

########################################################################################
# 2. READ, PROJECT, AND TRANSFORM tracts SHAPEFILE
########################################################################################
# Read in 2015 Census tracts shapfile
tractfilename <- paste0("cb_2015_", yourstatefips00, "_tract_500k")
tracts <- readOGR("/Users/vincent/SharePoint/Data - Stata/Shapefiles/cb_2015_us_tract_500k", 
                  tractfilename, stringsAsFactors = FALSE)
summary(tracts)

# Project shapefile using albers equal area
US_Proj <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 
               +b=6370997 +units=m +no_defs")
tracts <- spTransform(tracts, US_Proj)
plot(tracts)




########################################################################################
# 3. READ, PROJECT, AND TRANSFORM STATE SHAPEFILE
########################################################################################
# Repeat above but for state shapefile
# Read in 2015 Census state shapfile
state <- readOGR(toString(paste0(mapdir,"/cb_2015_us_state_20m")), 
                 "cb_2015_us_state_20m", stringsAsFactors = FALSE)
summary(state)

# Keep just your state
state <- state[(state$STATEFP %in% yourstatefips00), ]

# Project shapefile using albers equal area
US_Proj <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 
               +b=6370997 +units=m +no_defs")
state <- spTransform(state, US_Proj)



########################################################################################
# 4. PREP OBJECTS FOR GGPLOT
########################################################################################

# Fortify shapefiles so they can be used with ggplot()
tracts_f = fortify(tracts, region = "GEOID")
state_f = fortify(state, region = "GEOID")



########################################################################################
# 5. ACCESS ACS API AND STORE RACEDATA_EXC
########################################################################################
# Use your ACS API key here with parentheses
# api.key.install("8922460b24974374d00dc495678726e9f19e7dfd", file = "key.rda")

# Create complex geo call so that all counties for all states are querried
alltracts <- geo.make(state = yourstatefips, county = "*", tract = "*")

# GET EXCLUSIVE RACE CATEGORIES
#Look at variables in table B03002_001E
acs.lookup(2015, table.name = "B03002")

# List of variables that will be called in API
Table_B03002 = acs.lookup(2015, table.name = "B03002")
selected_vars <- Table_B03002[c(1, 3, 4, 5, 6, 7, 8, 9, 12)]

# Query API based on variable names and specified geographies
ACSdata_tract_exc <- acs.fetch(2015, span = 5, geography = alltracts,
                     variable = selected_vars, dataset = "acs", col.names = "auto")

#all counties has 3142 observations
#all tracts has 73056 observations


########################################################################################
# 6. ACCESS ACS API AND STORE RACEDATA_EXC
########################################################################################

#Store results in a data.frame for mapping
racedata_exc <- data.frame(cbind(geography(ACSdata_tract_exc),estimate(ACSdata_tract_exc)))

#Rename variables to more familiar terms
racedata_exc <- rename(racedata_exc, c("B03002_001" = "TOTPOP", "B03002_012" = "HISPANICS", "B03002_003" = "WHITE_NH", 
                                       "B03002_004" = "BLACK_NH", "B03002_005" = "AIAN_NH", "B03002_006" = "ASIAN_NH", 
                                       "B03002_007" = "NHOPI_NH", "B03002_008" = "OTHER_NH", "B03002_009" = "MULTI_NH"))

#Check that categories all combine to total population
racedata_exc$check <- apply(racedata_exc[,c("HISPANICS", "WHITE_NH", "BLACK_NH", "AIAN_NH", 
                                            "ASIAN_NH", "NHOPI_NH", "OTHER_NH", "MULTI_NH")], 1, sum)
sum(racedata_exc$check == racedata_exc$TOTPOP) == 3142 

# Add geographic identifiers for joining table with shapefile
racedata_exc$state  = formatC(racedata_exc$state , width = 2, flag = "0")
racedata_exc$county  = formatC(racedata_exc$county , width = 3, flag = "00")
racedata_exc$GEOID  = paste0(racedata_exc$state,racedata_exc$county,racedata_exc$tract)

racedata_exc$NON_WHITE_NH <- racedata_exc$TOTPOP - racedata_exc$WHITE_NH



########################################################################################
# 7. CREATE ANALYTIC VARAIBLES TO BE MAPPED, TURN INTO FACTOR VARIABLES
########################################################################################
#breaks <- c(0, 0.01, 0.02, 0.05, 0.10, 0.20, 0.35, 0.50, 0.65, 0.95, 1)
breaks <- c(0, 0.01, 0.02, 0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.95, 1)
#breaks <- c(0, 0.02, 0.05, 0.15, 0.33, 0.5, 1)
#breaks <- c(0, 0.05, 0.10, 0.25, 0.33, 0.50, 0.75, 0.90, 0.95, 1)

# FOR EXCLUSIVE RACE CATEGORIES
# Create continuous percentage category
for (demo in colnames(racedata_exc)[c(6:13,16)]) {
  racedata_exc[[toString(paste0(demo, "_pct"))]] <- 
    racedata_exc[[toString(demo)]] / racedata_exc$TOTPOP
  #racedata_exc[[toString(paste0(demo, "_pct"))]][is.na(racedata_exc[[toString(paste0(demo, "_pct"))]])] <- -1
}

# Create factor percentage category
for (level in colnames(racedata_exc)[c(6:13,16)]) {
  racedata_exc[[toString(paste0(level, "_pct_lvl"))]] <- 
    cut(racedata_exc[[toString(paste0(level, "_pct"))]],
        breaks = breaks,
        include.lowest = T)
}


# Merge in race data (exclusive categories) with fortified tracts shapefile
# Change id to GEOID in tracts_f
colnames(tracts_f)[which(colnames(tracts_f) == "id")] = "GEOID"
tracts_f2 = join(tracts_f, racedata_exc[, c(15,26:34)], "GEOID")
head(tracts_f2)
str(tracts_f2)



########################################################################################
# 8. MAP DATA
########################################################################################
#colors <- c("#edf8fb", "#bfd3e6", "#9ebcda", "#8c96c6", "#8856a7", "#810f7c")
#labels <- c("0-2.5%", "2.5-5%", "5-15%", "15-33%", "33-50%", "50-100%")

#colors <- c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b") #, "#bdbdbd")
#labels <- c("0-5%", "5-10%", "10-25%", "25-33%", "33-50%", "50-75%", "75-90%", "90-95%", "95+%")

colors <- c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b", "#000000")
#labels <- c("0-1%", "1-2%", "2-5%", "5-10%", "10-20%", "20-35%", "35-50%", "50-65%", "65-95%", "95+%")
labels <- c("0-1%", "1-2%", "2-5%", "5-10%", "10-20%", "20-30%", "30-50%", "50-70%", "70-95%", "95+%")


sp_minimal = theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
raceGroups <- colnames(tracts_f2)[8:16]
raceTitles <- c("Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Native American", 
                "Non-Hispanic Asian", "Non-Hispanic Pacific Islander", "Non-Hispanic Other",
                "Non-Hispanic Multiracial", "Hispanic", "Non-White")

for (demo in raceGroups) {
                  
  i <- which(raceGroups %in% demo)
  scale_fill_vpcustom <- scale_fill_manual(
    values=colors, 
    name = "Percentage of\ntracts Population",
    labels = labels,
    drop = FALSE)
    
  tempplot <- ggplot() + 
    geom_polygon(data = tracts_f2, colour = NA,
                 aes_string(
                   x = "long", y = "lat", group = "group",
                   fill = toString(demo))) +
    scale_fill_vpcustom +
    
    geom_polygon(data = state_f,
                 colour = "grey50", size = 0.25, fill = NA,
                 aes(x = long, y = lat, group = group)) + 
    coord_equal() + sp_minimal + 
    labs(list(title = paste(raceTitles[i], "Population as\na Percentage of Total tracts Population\n "), 
              subtitle = "Yolo"))
      #labs(list(subtitle = "This is the subtitle", caption = "This is the source"))
  
  #print(tempplot)
  
  grid.newpage()
  Notes   <- "Notes:"
  Sources <- "Sources: Shapefiles are Census Cartographic Boundary files. Race/Ethnic data come from\nCensus ACS 5-yr data."
  g <- arrangeGrob(tempplot, bottom = textGrob(paste(Sources,"\n"), x = 0.02, just="left", 
                                               gp = gpar(fontface = "italic", fontsize = 8)))
  #grid.draw(g)
  
  ggsave(plot = g,
         filename = toString(paste(datadir,yourstatefips00,"_", demo,".tiff",sep="")),
         width = 8, height = 6)
}




# savePlot <- function(myPlot) {
#   filename = toString(paste(datadir, "lblerg",".pdf",sep=""))
#   pdf(filename)
#   print(myPlot)
#   dev.off()
# }
# 
# savePlot(g)


########################################################################################
# REFERENCES USED IN CREATION OF THIS FILE
########################################################################################
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
#http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
#http://statmodeling.com/best-way-to-add-a-footnote-to-a-plot-created-with-ggplot2.html
#http://guides.main.library.emory.edu/Data_Services/cenlinks
#http://diversity.missouristate.edu/DiversityIndex.htm
#https://www.esri.com/library/whitepapers/pdfs/diversity-index-methodology.pdf
#http://www.city-data.com/forum/city-vs-city/1296212-us-census-defined-racial-diversity-index.html
#http://fivethirtyeight.com/features/the-most-diverse-cities-are-often-the-most-segregated/
#https://s4.ad.brown.edu/Projects/Diversity/Data/Data.htm
#http://www.coopercenter.org/demographics/Racial-Dot-Map
#http://enceladus.isr.umich.edu/race/calculate.html
#to map: Percent Foreign Born: Cities
#to map: Highest geographic concentration of counties of origin

# 6color http://colorbrewer2.org/#type=sequential&scheme=BuPu&n=6
# http://www.cookbook-r.com/Graphs/Output_to_a_file/
# http://stackoverflow.com/questions/22742737/function-to-save-ggplot

#3/16/17
# Combining shapefiles: http://stackoverflow.com/questions/19961898/append-combine-shape-files

########################################################################################
# ISSUES
########################################################################################
# Is join an inner join or left join per: http://stackoverflow.com/questions/22717892/r-ggplot2-mapping-issue-automate-missing-state-info
# Can add something equivalent to this? scale_fill_gradient(na.value="red")
# Or need to move NA to be first factor level

########################################################################################
# END OF FILE
########################################################################################