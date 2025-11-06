# All files named _raw when read in
# Modified files are named _temp for output to fixes_precleaning

#######################################################################################
# MPV ----

url <- "https://mappingpoliceviolence.us/s/MPVDatasetDownload.xlsx"
destfile <- here::here("Data", "Raw", "MPVDatasetDownload.xlsx")
curl::curl_download(url, destfile)

# Note cols don't read in properly if too many initial rows are missing --
# To fix: use guessmax arg 
# https://github.com/tidyverse/readxl/issues/413#issuecomment-350520309

message("Scraping MPV")

mpv_raw <- read_excel(destfile, guess_max = 10000)

# Also need to modify the coltypes for age & zip since these
# don't get identified correctly by default.  But we do in the cleaning loop;
# will generate warnings b/c age has 'Unknown' for missing

#######################################################################################

# WA local data (1/1/2022- with backfills as found):
## Preserves the original coverage of FE: 
## * fatalities from all encounters, not just gunshots.  
## * Vehicular pursuits are included.
## Update procedure:
## * Manually with google searches/IncarcerNation
## * By script with MPV merge (see MakeData.R file)

## Variable names in raw file have been updated from FE original to final harmonized

message("Reading in local data for WA from WA-FEWP project")

walocal_raw <- readxl::read_xlsx(here::here("Data", "Raw", "wa_local.xlsx"))


#######################################################################################

# Clean FE data: ----
## From the previous repo (fewapo)
## This is essentially a frozen dataset now, though errors may be fixed in the future

message("Reading in clean legacy FE data from WA-FEWP project")

load(here::here("Data", "Clean", "FE_clean.rda"))

  
## If the archived dataset needs to be fixed:
## Fix it, read the clean data back into this repo
## Strip out the 90000+ IDs as these are from our local data collection, and are
## handled separately (above)

# fe_clean <- read.csv(here::here("Data", "Clean", "FEWaPo", "FE_clean.csv")) %>%
#   filter(feID < 90000) %>%
#   mutate(date = as.Date(date)) %>%
#   select(-X)
# save(fe_clean, file = here::here("Data", "Clean", "FE_clean.rda"))

######################################################################################

# Clean WaPo data: ----
## From the previous repo (fewapo)
## This is essentially a frozen dataset now, though errors may be fixed in the future

message("Reading in clean legacy WaPo data from WA-FEWP project")

load(here::here("Data", "Clean", "wapo_clean.rda"))

## If the archived dataset needs to be fixed:
## Fix it, read the clean data back into this repo
# wapo_clean <- read.csv(here::here("Data", "Clean", "FEWaPo", "WaPo_clean.csv")) %>%
#   mutate(date = as.Date(date)) %>%
#   select(-X)
# save(wapo_clean, file = here::here("Data", "Clean", "wapo_clean.rda"))

######################################################################################

# Leg District info ----

message(paste("Loading prebuilt WA legislative district data \n",
              "*Updates only needed for redistricting and elections*"))
load(here::here("Data", "Clean", "LegInfo_Clean.rda"))

#####################################################################################

# IncarcerNation-FE ID crosswalk info for 2021 ----

message("Loading IncarcerNation-FE ID crosswalk info for 2021")

inIDs2021 <- read.csv(here::here("Data", "Raw", "FE_IN_xwalk_2021.csv"))

