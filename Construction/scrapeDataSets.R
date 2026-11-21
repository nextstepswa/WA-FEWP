# All files named _raw when read in
# Modified files are named _temp for output to fixes_precleaning

#_____________________________________________________________
# MPV ----

# Scraping the airtable is not working reliably, so for now we download the dataset by hand

if(difftime(Sys.time(), 
            file.info(here::here("Data", "Raw", "Mapping Police Violence.csv"))$atime, 
            units="days") > 1) {
  
  message("Update the MPV datafile") 

  } else {
  
    message("Reading in downloaded MPV datafile") 
    mpv_raw <- read_csv("Data/Raw/Mapping Police Violence.csv", 
                        col_types = cols(
                          date = col_date(format = "%m/%d/%Y"),
                          zip = col_number(),
                          officer_charged = col_character(), 
                          off_duty_killing = col_character(), 
                          prosecutor_head = col_character(), 
                          prosecutor_race = col_character(), 
                          prosecutor_gender = col_character(), 
                          prosecutor_party = col_character(), 
                          prosecutor_term = col_character(), 
                          prosecutor_in_court = col_character(), 
                          prosecutor_special = col_character(), 
                          independent_investigation = col_character(), 
                          prosecutor_url = col_character())
    ) %>%
      mutate(mpvID = mpv_id, 
             feID = fe_id,
             wapoID = wapo_id
      ) %>%

      mutate(across(pop_white_census_tract:pop_hispanic_census_tract, ~as.numeric(sub("%", "", .))/100))
  }
  
  # ## Clean ----------------------------------------------------------------------
  # ## col formats and standardized IDs
  # 
  # mpv_raw <- df %>%
  #   convert_types() %>%
  #   mutate(zip = as.numeric(zip)) %>%
  #   mutate(mpvID = mpv_id, 
  #          feID = fe_id,
  #          wapoID = wapo_id
  #   )

#_____________________________________________________________
# WA local ----

# Cleaned data (1/1/2022- with backfills as found):
## Preserves the original coverage of FE: 
## * fatalities from all encounters, not just gunshots.  
## * Vehicular pursuits are included.
## Update procedure:
## * Manually with google searches/IncarcerNation
## * By script with MPV merge (see MakeData.R file)

## Variable names in raw file have been updated from FE original to final harmonized

message("Reading in clean local data for WA from WA-FEWP project")

walocal_raw <- readxl::read_xlsx(here::here("Data", "Raw", "wa_local.xlsx"))


#_____________________________________________________________
# FE ----

## Cleaned legacy data from the previous repo (fewapo)
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

#_____________________________________________________________
# WaPo ----

## Cleaned legacy data rom the previous repo (fewapo)
## This is essentially a frozen dataset now, though errors may be fixed in the future

message("Reading in clean legacy WaPo data from WA-FEWP project")

load(here::here("Data", "Clean", "wapo_clean.rda"))

## If the archived dataset needs to be fixed:
## Fix it, read the clean data back into this repo
# wapo_clean <- read.csv(here::here("Data", "Clean", "FEWaPo", "WaPo_clean.csv")) %>%
#   mutate(date = as.Date(date)) %>%
#   select(-X)
# save(wapo_clean, file = here::here("Data", "Clean", "wapo_clean.rda"))

#_____________________________________________________________
# Leg Districts ----

message(paste("Loading prebuilt WA legislative district data \n",
              "*Updates only needed for redistricting and elections*"))
load(here::here("Data", "Clean", "LegInfo_Clean.rda"))

#_____________________________________________________________
# IncarcerNation-feID crosswalk ----

## For 2021 only - this was the only year of overlap

message("Loading IncarcerNation-FE ID crosswalk info for 2021")

inIDs2021 <- read.csv(here::here("Data", "Raw", "FE_IN_xwalk_2021.csv"))

