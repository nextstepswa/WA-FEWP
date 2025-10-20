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

# To facilitate scripted cleaning
# Check periodically to see if they've modified the file structure
mpv_temp <- mpv_raw %>%  
  mutate(mpvID = `MPV ID`, 
         feID = as.numeric(`Fatal Encounters ID`),
         wapoID = `WaPo ID (If included in WaPo database)`,
         name = `Victim's name`,
         date = as.Date(`Date of Incident (month/day/year)`)
  ) %>%
  select(-`...53`) %>%
  select(mpvID:date, `Victim's age`:`Prosecutor Source Link`) #omits KBP, col of 1's

#######################################################################################

# New names (1/1/2022-):  WA ONLY ----
## Preserves the original coverage of FE: 
## fatalities from all encounters, not just gunshots.  
## Vehicular pursuits are included.

## Modify age and date for matching to MPV

message("Reading in new names for WA from WA-FEWP project")

walocal_raw <- readxl::read_xlsx(here::here("Data", "Raw", "wa_newnames.xlsx")) %>%
  mutate(date = as.Date(`Date of injury resulting in death (month/day/year)`)) %>%
  rename("age" = "Age") %>%
  select(-`Date of injury resulting in death (month/day/year)`)


#######################################################################################

# Clean FE data: ----
## From the previous repo (fewapo)
## This is essentially a frozen dataset now, though errors may be fixed in the future
## We strip out the 90000+ IDs as these are from our local data collection, and are
## handled separately (above)

fe_clean <- read.csv(here::here("Data", "Clean", "FEWaPo", "FE_clean.csv")) %>%
  filter(feID < 90000) %>%
  mutate(date = as.Date(date)) %>%
  select(-X)
save(fe_clean, file = here::here("Data", "Clean", "FE_clean.rda"))

######################################################################################

# Clean WaPo data: ----
## From the previous repo (fewapo)
## This is essentially a frozen dataset now, though errors may be fixed in the future

wapo_clean <- read.csv(here::here("Data", "Clean", "FEWaPo", "WaPo_clean.csv")) %>%
  mutate(date = as.Date(date)) %>%
  select(-X)
save(wapo_clean, file = here::here("Data", "Clean", "wapo_clean.rda"))

######################################################################################

# Leg District info ----

message(paste("Loading prebuilt WA legislative district data \n",
              "*Updates only needed for redistricting and elections*"))
load(here::here("Data", "Clean", "LegInfo_Clean.rda"))
