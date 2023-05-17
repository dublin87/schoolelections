
###################################################################################################
# Setup & Packages
###################################################################################################

install.packages("pacman")
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

#packages_v <- c("here", "tidyverse", "sf", "rvest", "jsonlite", "httr", "pacman", "tictoc", "pryr", "tigris", "tidycensus","rgdal","Matching", "raster", "stargazer","zoo","MASS","ggrepel","ggthemes","ggpmisc","ggpubr", "raster","rgdal","MatchIt","cobalt","cowplot","rgenoud", "tidysynth", "mapview","modelsummary","plm","ggmap") 

packages_simple <- c("tidyverse","tidycensus","sf","ggsflabel","scales", "lubridate", "ggplot2","did")

pacman::p_load(char = packages_simple)

# Set Working Directory Mac
#setwd("/Users/nick/Library/Mobile Documents/com~apple~CloudDocs/Documents/R Projects/school_facebook")

# Set Working Directory PC
setwd("C:/Users/nickv/Dropbox/R Projects/school_elections/schoolelections")

#### Load census API key 
census_api_key("1d2602068db46e014dbf54a7c7e195cb8f417e61", install = TRUE, overwrite = TRUE)

###################################################################################################
# Fetch Census Data by School District
###################################################################################################

### Create a dataframe that contains the ACS variable codes, to search for variables of interest
v19 <- load_variables(2019, "acs5", cache = TRUE)
v09 <- load_variables(2009, "acs5", cache = TRUE)
v06 <- load_variables(2009, "acs5", cache = TRUE)

### Create an index of ACS variables to pull
my_vars <- c(
  bachelor_degree = "B16010_041",
  high_school_ged = "B16010_015",
  GED = "B15003_018",
  median_income = "B19113_001",
  population = "B01003_001",
  associates = "B15003_021",
  masters = "B15003_023",
  professional = "B15003_024",
  doctorate = "B15003_025",
  some_college_1 = "B15003_019",
  some_college_2 = "B15003_020",
  black = "B02001_003",
  white = "B02001_002",
  male_under18 = "B05003_003",
  female_under18 = "B05003_014",
  male_over65 = "B15001_035",
  female_over65 = "B15001_076"
)


#### ALTERNATIVE METHOD

# place to store results and combine them
years <- 2012:2021
res <- vector("list",length(years))
names(res) <- years

# variables that you want
#        Tot Pop     White non-Hisp    FamPoverty
#vars <- c('B03001_001','B03002_003','B17010_002')

# loop over years, save data
# could also apply county filter, see help(get_acs)
# using smaller Deleware just for example
for (y in years){
  # download data
  ld <- as.data.frame(get_acs(year = y,
                              geography='school district (unified)',
                              survey='acs5',
                              variables = my_vars,
                              state="OH"))
  # reshape long to wide
  ld2 <- reshape(ld,
                 idvar="GEOID",
                 timevar="variable",
                 direction="wide",
                 drop=c("NAME","moe"))
  # insert into list and add in year
  res[[y]] <- ld2
  res[[y]]$year <- y
}

# Combining the data frames together for final analysis
combo <- do.call("rbind",res)

# Rename GEOID into "leaid" to make mergable with election data in next section.
combo <- combo %>%
  rename("leaid" = "GEOID")

combo$estimate.high_school_ged <- combo$estimate.high_school_ged + combo$estimate.GED
combo <- select(combo, -9)

#head(combo) # can see B03001_001 is missing for block groups
#summary(combo)

#########################################################################################################
# Fetch 2007-2011 ACS 3 Variables
#########################################################################################################

# place to store results and combine them
years <- 2009:2011
res <- vector("list",length(years))
names(res) <- years

my_vars2 <- c(
  male_bachelor_degree = "B15002_015",
  male_high_school = "B15002_011",
  median_income = "B19113_001",
  population = "B01003_001",
  male_associates = "B15002_014",
  male_masters = "B15002_016",
  male_professional = "B15002_017",
  male_doctorate = "B15002_018",
  male_some_college_1 = "B15002_012",
  male_some_college_2 = "B15002_013",
  black = "C02003_004",
  white = "C02003_003",
  male_under18 = "B05003_003",
  female_under18 = "B05003_014",
  male_over65 = "B15001_035",
  female_over65 = "B15001_076",
  female_high_school = "B15002_028",
  female_associates = "B15002_031",
  female_somecollege_1 = "B15002_029",
  female_somecollege_2 = "B15002_030",
  female_bachelor_degree = "B15002_032",
  female_masters = "B15002_033",
  female_professional = "B15002_034",
  female_doctorate = "B15002_035"
)

# variables that you want
#        Tot Pop     White non-Hisp    FamPoverty
#vars <- c('B03001_001','B03002_003','B17010_002')

# loop over years, save data
# could also apply county filter, see help(get_acs)
# using smaller Deleware just for example
for (y in years){
  # download data
  ld <- as.data.frame(get_acs(year = y,
                              geography='school district (unified)',
                              survey='acs5',
                              variables = my_vars2,
                              state="OH"))
  # reshape long to wide
  ld2 <- reshape(ld,
                 idvar="GEOID",
                 timevar="variable",
                 direction="wide",
                 drop=c("NAME","moe"))
  # insert into list and add in year
  res[[y]] <- ld2
  res[[y]]$year <- y
}

# Combining the data frames together for final analysis
combo2 <- do.call("rbind",res)

# Rename GEOID into "leaid" to make mergable with election data in next section.
combo2 <- combo2 %>%
  rename("leaid" = "GEOID")

combo2$estimate.bachelor_degree <- combo2$estimate.male_bachelor_degree + combo2$estimate.female_bachelor_degree
combo2$estimate.high_school_ged <- combo2$estimate.female_high_school + combo2$estimate.male_high_school
combo2$estimate.professional <- combo2$estimate.male_professional + combo2$estimate.female_professional
combo2$estimate.masters <- combo2$estimate.male_masters + combo2$estimate.female_masters
combo2$estimate.doctorate <- combo2$estimate.male_doctorate + combo2$estimate.female_doctorate
combo2$estimate.some_college_1 <- combo2$estimate.male_some_college_1 + combo2$estimate.female_somecollege_1
combo2$estimate.some_college_2 <- combo2$estimate.male_some_college_2+ combo2$estimate.female_somecollege_2
combo2$estimate.associates <- combo2$estimate.male_associates + combo2$estimate.female_associates

combo2 <- select(combo2, -7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-17,-18,-19,-20,-21,-22)

combo3 <- rbind(combo, combo2)

############################################################################################################
### Import school levy election data 
############################################################################################################

election.df <- as.data.frame(read.csv("data/raw_levy_12_19.csv"))

# Merge census data into district data
election.df$leaid <- as.character(election.df$leaid)
data <- left_join(election.df, combo3,  by = c('leaid', 'year'))  
data <- filter(data, year < 2022)

# Calculate demographic variables as a %
data$pct_white <- data$estimate.white / data$estimate.population
data$pct_black <- data$estimate.black / data$estimate.population
data$pct_under18 <- (data$estimate.male_under18 + data$estimate.female_under18) / data$estimate.population
data$pct_over65 <- (data$estimate.male_over65 + data$estimate.female_over65) / data$estimate.population
data$pct_hs <- data$estimate.high_school / data$estimate.population
data$pct_bachelors <- data$estimate.bachelor_degree / data$estimate.population
data$pct_advanced <- (data$estimate.masters + data$estimate.doctorate + data$estimate.professional) / data$estimate.population
data$pct_somecollege <- (data$estimate.some_college_1 + data$estimate.some_college_2) / data$estimate.population


# Import district Facebook profile creation history data

facebook <- as.data.frame(read.csv("data/facebook.csv"))
facebook$leaid <- as.character(facebook$leaid)

CS_data <- left_join(data, facebook, by = "leaid")
CS_data <- select(CS_data, -45, -13)


###########################################################################
# Callaway Sant'Anna
###########################################################################

# make required parameters numeric for did package
CS_data$leaid <- as.numeric(CS_data$leaid)
CS_data$for. <- as.numeric(CS_data$for.)
CS_data$year <- as.numeric(CS_data$year)
CS_data$fb_create_year <- as.numeric(CS_data$fb_create_year)
CS_data$new_binary <- ifelse(CS_data$new == "New",  1, 0)

### Test for complete cases before removing variables
CS_data_complete <- CS_data[complete.cases(CS_data), ]

### Remove variables if necessary
#CS_data_complete <- select(CS_data, 


CS_data_complete <- filter(CS_data_complete, year < 2020)
CS_data_complete$against <- as.numeric(CS_data_complete$against)
CS_data_complete$pct_for <- CS_data_complete$for. / (CS_data_complete$for. + CS_data_complete$against)
CS_data_complete$november_elec <- ifelse(CS_data_complete$month == "11", 1, 0)
CS_data_complete$mills_pct <- as.numeric(CS_data_complete$mills_pct)
CS_data_complete$years <- as.numeric(CS_data_complete$years)

CS_final <- CS_data_complete[complete.cases(CS_data_complete), ]

CS_levy <- filter(CS_final, type == "levy")
CS_bond <- filter(CS_final, type == "bond")
CS_incometax <- filter(CS_final, type == "income tax")
CS_combo <- filter(CS_final, type == "combo")

CS_levy <- rbind(CS_levy, CS_incometax)
CS_bond <- rbind(CS_bond, CS_combo)

CS_final$levy <- ifelse(CS_final$type == "levy", 1, 0)
##########################################################
# Levy Estimation
##########################################################

# Estimating the effect on 'for' votes
atts <- att_gt(yname = "pct_for", # LHS variable
               tname = "year", # time variable
               idname = "leaid", # id variable
               gname = "fb_create_year", # first treatment period variable
               data = CS_final, # data
               #xformla = NULL, # no covariates
               xformla = ~ estimate.population + mills_pct + years + pct_black + pct_bachelors + pct_advanced + pct_under18 + month + november_elec + estimate.median_income + levy,
               est_method = "reg", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "leaid", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT
agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)


##########################################################
# Bond Estimation
##########################################################

# Estimating the effect on 'for' votes
atts <- att_gt(yname = "pct_for", # LHS variable
               tname = "year", # time variable
               idname = "leaid", # id variable
               gname = "fb_create_year", # first treatment period variable
               data = CS_final, # data
               #xformla = NULL, # no covariates
               xformla = ~ estimate.population + mills_pct + years + pct_black + pct_bachelors + pct_advanced + pct_under18 + month + estimate.median_income,
               est_method = "reg", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "leaid", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT
agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)


##########################################################
# Income Tax Estimation
##########################################################

# Estimating the effect on 'for' votes
atts <- att_gt(yname = "pct_for", # LHS variable
               tname = "year", # time variable
               idname = "leaid", # id variable
               gname = "fb_create_year", # first treatment period variable
               data = CS_incometax, # data
               xformla = NULL, # no covariates
               #xformla = ~ estimate.population + mills_pct + years + pct_black + pct_bachelors + pct_advanced + pct_under18 + month + estimate.median_income,
               est_method = "reg", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 10000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "leaid", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT
agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)

##########################################################
# Combo
##########################################################

# Estimating the effect on 'for' votes
atts <- att_gt(yname = "pct_for", # LHS variable
               tname = "year", # time variable
               idname = "leaid", # id variable
               gname = "fb_create_year", # first treatment period variable
               data = CS_combo, # data
               xformla = NULL, # no covariates
               #xformla = ~ estimate.population + mills_pct + years + pct_black + pct_bachelors + pct_advanced + pct_under18 + month + estimate.median_income,
               est_method = "reg", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 10000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "leaid", # cluster level
               panel = FALSE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT
agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)














