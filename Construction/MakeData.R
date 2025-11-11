rm(list=ls())

##### What this file does ################################################################
#
# This script:
# 1. Scrapes current data from MPV and loads WA local file (wa_newnames)
# 2. Cleans the data -- fixes and harmonizes
# 3. Merges in data from the FE archive (final cleaned version) for cases not tracked by MPV
# 4. Extracts the merged complete dataset for WA state only
#
# Resulting cleaned data is saved as outputs
#
# The code is designed to stop with an informative message if it finds errors
# that need to be manually addressed.  The typical reasons for this will be:
## new pursuit review/coding
## changes in source file formats (common for MPV)
## missing or duplicate cases resulting from the WA-MPV merge

# If it stops, the errors need to be manually fixed/updated
# and then this file can be re-run (repeatedly, if needed
# until no more errors/updates are flagged)

###########################################################################################

#library(devtools)
library(tidyverse)
library(readxl)
library(maps)
library(here)
library(fuzzyjoin)
library(lubridate)

# Functions ----

state_fullname_fn <- function(x){
  snames <- c(state.name, "District of Columbia")
  names(snames) <- c(state.abb, "DC")
  snames[x]
}

scrape_data_fn <- function(dataset_name, url, save_file) {
  dataset_name <- rio::import(url)
  write.csv(dataset_name, save_file)
  dataset_name
}

# %1$s is replaced with the first replace argument you pass into sprintf. %2$s uses the seconds etc.
make_url_fn <- function(x) {
  #paste(sprintf("<a href='%1$s'>%1$s</a>", x))
  paste0("<a href='", x, "'>", x, "</a>")
}


"<a href='http://masonwebtv.com/archives/14169'>http://masonwebtv.com/archives/14169</a>"

st.abb51 <- c(sort(state.abb), "DC")

# Scrape data (script) ----

## 1 external source is scraped: MPV
## 2 locally constructed data sources are added:

### WA newnames - uses original FE inclusion criteria, 
#### 2022+ with some additional backfilling
#### incorporates cases from IncarcerNation (manually for now)

### WA Legislative districts (and state leg info for WA)
#### Updates to this only needed for redistricting and elections
#### Updates require a manual geocoding update from geocodio

source(here("Construction", "scrapeDataSets.R"))


# Cleaning & Variable construction ----

## MPV cleaning ----

## Fix MPV errors:
## These errors are typically ID'd during merging and reported
## If they are not due to lag in info, or not fixed by MPV, they are fixed here

## After this, the harmonized variables are constructed.

source(here("Construction", "fixes_mpv.R"))
source(here("Construction", "harmonize_mpv.R"))


## WA local cleaning ----

## File format has been updated
## Includes cases from IncarcerNation with inID and inSource tags
## Legacy pursuit info from googlesheet (reviewed/updated) is incorpoarated

source(here("Construction", "harmonize_wa.R"))


## FE extra cases ----

## Two types of FE incidents are not in MPV:  pre-2013 and intentional exclusions.

## MPV intentionally excludes cases from FE that where the COD is
## not a homicide (i.e., suicides and accidents), and
## excludes vehicular homicides not committed by police.  
## Regarding pursuits, this is what they say on their website: 
## https://mappingpoliceviolence.us/aboutthedata

# A person was coded as having a Vehicle as a weapon if they were one or more of the following:
#   a driver who was killed while hitting, dragging or driving towards officers or civilians
#   a driver who was driving and/or being pursued by police at high speeds, presenting a danger to the public
# People who were killed by a civilian driver or crashed without being hit directly and intentionally 
# by police during a police pursuit are not included in the database. Note that an estimated 300 people 
# are killed in police pursuits each year and only a small proportion of these cases are included 
# in the database (most deadly pursuits end after the driver crashes themselves into something or 
# hits a civilian vehicle without being directly rammed/hit by police).

## Our WA data collection follows the original FE inclusion criteria,
## So we recover the excluded FE cases here, for a consistent WA series.

## This is the cleaned/harmonized FE dataset from the previous fewapo repo, 
### Remove all records for people not killed by police (does not include pursuits)

## Join on the inIDs for 2021 cases, since these cases are not in wa local
## By inspection 10/24/2025, all of the 2021 cases in IN are also in FE, 
## so inSource tag not needed
## *That may change if they continue to backfill*

fe_clean_xtra <- anti_join(fe_clean %>% filter(not.kbp == 0), 
                           mpv_clean,
                           by = "feID") %>%
  left_join(inIDs2021)



# WaPo extra cases ----
## It's not clear why these are not in MPV.  Need to dx and track down later
## For now we ignore them.  If they are included in fe_extra, or in walocal_clean
## we still get them in the data set.  If not, we assume MPV had a good reason to
## exclude them (for now)

wapo_clean_xtra <- anti_join(wapo_clean, mpv_clean,
                             by = "wapoID")


# Save clean source datasets, all cases ----

## CSV files ----

write.csv(mpv_clean, here("Data", "Clean", "MPV_clean.csv"))
write.csv(walocal_clean, here("Data", "Clean", "WAlocal_clean.csv"))
write.csv(fe_clean_xtra, here("Data", "Clean", "FE_clean_xtra.csv"))
write.csv(wapo_clean_xtra, here("Data", "Clean", "WaPo_clean_xtra.csv"))

## Rdata files ----
## These save lots of metadata as well

## Key date info

selection <- "all cases"
scrape_date <- Sys.Date()

last_date_wa <- max(walocal_clean$date)
last_date_mpv <- max(mpv_clean$date)
last_date_in <- max(walocal_clean$date[!is.na(walocal_clean$inID)])

last_data_update <- max(c(last_date_mpv, last_date_wa, last_date_in))

last_update_is_eoy <- month(last_data_update)==12 & day(last_data_update)==31
last_complete_mo <- ifelse(last_update_is_eoy | month(last_data_update)==1, 
                           12, 
                           month(last_data_update)-1)
last_complete_yr <- ifelse(last_update_is_eoy, 
                           year(last_data_update), 
                           year(last_data_update)-1)

save(list = c("mpv_clean", "walocal_clean", "fe_clean_xtra", "wapo_clean_xtra",
              "selection", "scrape_date", 
              "last_date_wa", "last_date_mpv", "last_date_in",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("Data", "Clean", "CleanSourceData.rda"))

update.message.wa <- paste("\n *** Last WA incident: ", last_date_wa, "\n\n")
update.message.mpv <- paste("\n *** Last MPV incident: ", last_date_mpv, "\n\n")
update.message.in <- paste("\n *** Last IN incident: ", last_date_in, "\n\n")


# Merge MPV and WA -----

## For the WA state data collected locally
### WA local is almost all 2022+, with a few backfills for earlier years
### MPV is 2022+ only, 2013-2021 will be added back later

## Our data (walocal_clean) has broader inclusion criteria, so MPV matches will be a subset
## Errors found during matching trigger stops
## Race is hierarchically assigned from wa first, mpv second
### WA local is more complete now with IN coding

## Returned file is named "merge_walocal_mpv"

source(here("Construction", "merge_mpv_wa.R"))

## Set consensus variables to WA values for now
## It's possible that MPV has different values for shared vars, can dx later

#orig.names = gsub(".wa", "", names(merge_walocal_mpv)[grep(".wa", names(merge_walocal_mpv))])
mpv.names = names(merge_walocal_mpv)[grep(".mpv", names(merge_walocal_mpv))]
wa.names = names(merge_walocal_mpv)[grep(".wa", names(merge_walocal_mpv))]

# Rename .wa vars to orig names, drop .mpv vars
wa_mpv <- merge_walocal_mpv %>%
  rename_with(~gsub(".wa", "", .), all_of(wa.names)) %>%
  select(-all_of(mpv.names))

# Write out ID crosswalk file for legacy pursuit merge later
# post-2022 so no feID

id_xwalk_from_2022 <- wa_mpv %>% select(contains("ID")) %>% select(-feID)
save(id_xwalk_from_2022, 
     file = here::here("Data", "Clean", "ID_xwalk_from_2022.rda"))

# Add cases before 2022 ----

## Resulting file:
##  2000-2012 all from FE clean, non-MPV years (added)
##  2013-2021 MPV + FE extra (added)
##  2022+     MPV + walocal (merged)

## FE WA cases 2000-2021 ---- 
## fe_clean_xtra = the final clean FE data from the previous repo (fewa), 
## antijoined to MPV so = non-MPV years (2000-2013) + excluded by MPV 2013-2021

fe_clean_xtra_wa <- fe_clean_xtra %>%
  filter(st == "WA") %>%
  rename(fleeing = flee)

# MPV cases 2013-2021 ----
## Note: MPV has some cases not included in FE during this period, but none in WA
## tack on inIDs for 2021

mpv_pre2022_wa <- mpv_clean_wa %>%
  filter(year < 2022) %>%
  left_join(inIDs2021)
 
## Add to existing merged walocal.mpv file and tag source
wa_mpv_fextra_all <- wa_mpv %>%
  bind_rows(fe_clean_xtra_wa, .id = "source1") %>% # 1=WA, 2=FE
  bind_rows(mpv_pre2022_wa, .id = "source2") %>%   # 1=MPV, 2=(WA+FE)
  mutate(source = case_when(
    (source1 ==1 & inSource==1) ~ "IN",
    source1 ==1 ~ "WA",
    source1 ==2 ~ "FE",
    TRUE ~ "MPV")
  ) %>%
  arrange(date) %>%
  select(-c(source1, source2))


# Legacy pursuits for 2015 - 11/2025 ----

## Cleaned legacy pursuit file should exist
## If not, or if there are updates, can run the script to create
## See the construction file for information on the process
## Merging later relies on all of the ID vars

# Check if need to rebuild, otherwise load existing file

if(file.exists(here::here("Data", "Clean", "Pursuits",
                          "legacy_pursuits.rda"))){

  lastmod.pursuit.rda <- file.info(here::here("Data", "Clean", "Pursuits", 
                                               "legacy_pursuits.rda"))$mtime
  lastmod.pursuit.makefile <- file.info(here::here("Construction", 
                                                   "legacy_pursuits.R"))$mtime
  lastmod.MPV.pursuits <- file.info(here::here("Data", "Clean", "Pursuits", 
                                               "MPV.coded.pursuits.xlsx"))$mtime
  lastmod.WA.pursuits <- file.info(here::here("Data", "Clean", "Pursuits", 
                                               "WAlocal.coded.pursuits.xlsx"))$mtime
  lastmod.FE.pursuits <- file.info(here::here("Data", "Clean", "Pursuits", 
                                               "FE.coded.pursuits.xlsx"))$mtime
  
  lastmod.makefiles <- max(lastmod.pursuit.makefile, lastmod.MPV.pursuits,
                           lastmod.WA.pursuits, lastmod.FE.pursuits)
  
  if(lastmod.makefiles > lastmod.pursuit.rda) {
    message("Rebuilding pursuit dataset")
    source(here::here("Construction", "makeWApursuits.R"))
    
  } else {
    message("Loading existing legacy pursuit dataset")
    load(here::here("Data", "Clean", "Pursuits", "legacy_pursuits.rda"))
  }
} else {
  message("Rebuilding pursuit dataset")
  source(here::here("Construction", "legacy_pursuits.R"))
}

# Final variable prep for 2015+ WA data ----

## Consensus variable construction: ----

## 1. Use WA data if available
##   Any errors we find in WA are fixed during the merging
##   so WA has the most reliable data 2022+.
##   

## 2. For 2013-2021 we have two sources:
##    * our cleaned FE WA data if it was excluded from MPV
##    * MPV data otherwise (they've done a lot of cleaning) 
##   To Do:  Check if our FE fixes for MPV cases and these years
##           have been incorporated into MPV.  

## 3. Pursuit coded data: two sources
##    * our legacy coding of cases thru 10/2025 (see legacy.pursuits.R)
##    * ongoing coding of cases via WAlocal file after this

## 4. Legislative year variable
## We create an approximate legislative year variable, to use for
## before and after assessments.  Some bills take effect immediately
## (typically in late May) and others 90 days after end of session 
## (typically late July), and that also varies by short/long leg year.

## Because we are mostly going to be interested in the before/after
## effects of the 2021 legislative reforms, and most of these took
## effect in late July 2021, we will use Aug 1 - Jul 30 as the
## the legislative year.


curr.yr <- lubridate::year(Sys.Date())
curr.mo <- lubridate::month(Sys.Date())
cut.yr <- ifelse(curr.mo > 7, curr.yr+1, curr.yr)                        

wa_all_draft <- wa_mpv_fextra_all %>% 
  
  ## Count the number of source IDs
  # When inSource=1 Incarceration is the only source.  These cases are incorporated into 
  # the WA local dataset, so have a WA local ID, but this is not an overlap.
  rowwise() %>%
  mutate(source.overlap = if_else(is.na(inSource), 
                                  sum(across(contains("ID"), ~!is.na(.))),
                                  1)) %>%
  ungroup() %>%

  ## Create unique record locator using source, yr and seqnum
  ## input file is arranged by date, but sequence can change if new cases added
  ## so this record locator is not guaranteed to be invariant over time
  ## but it can be used for matching from this point forward
  
  mutate(recID = paste(source, year, 1:nrow(wa_mpv_fextra_all), sep="-"),
         date = as.Date(date)) %>%
  
  ## Pursuits ----
  ## Merge on legacy pursuit coded data (165 incidents, 153 are 2015+)
  
  left_join(legacy.pursuits %>%
              mutate(legacy.pursuits=1) %>%
              select(contains("ID"), cod, mod, contains("pursuit"), contains("notes"), url_addl),
            by = c("mpvID", "waID", "feID", "wapoID", "inID"),
            suffix = c(".drft", ".prst")) %>%
  
  ## and use the cleaned coded pursuit variables when available
  mutate(cod = if_else(!is.na(cod.prst), cod.prst, cod.drft),
         mod = if_else(!is.na(mod.prst), mod.prst, mod.drft),
         pursuit.type = if_else(!is.na(pursuit.type.prst), pursuit.type.prst, pursuit.type.drft),
         pursuit.victim = if_else(!is.na(pursuit.victim.prst), pursuit.victim.prst, pursuit.victim.drft),
         pursuit.injury = if_else(!is.na(pursuit.injury.prst), pursuit.injury.prst, pursuit.injury.drft),
         pursuit.incinum = if_else(!is.na(pursuit.incinum.prst), pursuit.incinum.prst, pursuit.incinum.drft),
         url_addl = if_else(!is.na(url_addl.prst), url_addl.prst, url_addl.drft)
  ) %>%
  
  # Legislative years ----

  mutate(leg.year = 
           cut(as.Date(date), 
               breaks = as.Date(paste(2000:(cut.yr), "-08-01", sep="")),
               labels = c(paste(month.abb[8], 2000:(cut.yr-1), "-", 
                                month.abb[7], 2001:cut.yr,
                                sep = "")))) %>%
  
  # Shorten and harmonize agency.type names ----

  mutate(agency.type = case_when(
    grepl("Local", agency.type) ~ "Local PD",
    grepl("Tribal", agency.type) ~ "Tribal PD",
    grepl("Sheriff", agency.type) ~ "County SO",
    grepl("County", agency.type) ~ "Other County",
    grepl("Correc", agency.type) ~ "Corrections",
    agency.type == "State Police" ~ "WSP",
    grepl("Fed|US", agency.type) ~ "Federal",
    TRUE ~ agency.type)
  ) %>%
  
  # Any other tweaks
  
  mutate(
    # crisis vars not consistent, MPV foreknowledge has fewer NA and is likely updating:
    mh_crisis = if_else(is.na(foreknowledge), symptoms, foreknowledge),
    
    cod = if_else(grepl("Ketamine|ketamine", description) | (!is.na(feID) & feID==23756),"Ketamine", cod),
    
    # Medical emergencies will be coded as mod = Accident
    # All were in custody, likely cuffed or otherwise restrained, but too little evidence to code that.
    
    mod = case_when(
      grepl("Medical", cod) ~ "Accident",
      !is.na(mod) ~ mod,
      !is.na(mpvID) ~ "Homicide",
      grepl("suicide", description) ~ "Suicide", # in case these were missed
      grepl("own life", description) ~ "Suicide",
      grepl("Vehicle", pursuit.type) & cod=="Vehicle" ~ "Vehicle Accident",
      grepl("Gun|Knife|Taser|Asphyx", cod) ~ "Homicide",
      grepl("Drown|Burn|Drug|Jump|Ketamine", cod) ~ "Accident"),
    
    pursuit.type = if_else(is.na(pursuit.type), "Not pursuit related", pursuit.type),
    
    weapon = case_when(
      grepl("toy|Other|other|blunt", weapon) ~ "Other",
      grepl("firearm", weapon) ~ "Firearm",
      grepl("edged", weapon) ~ "Edged weapon",
      weapon=="vehicle" ~ "Vehicle",
      grepl("No|none", weapon) ~ "None",
      TRUE ~ "Unknown")
  ) %>%
           
  
  # Select consensus variables, along with the few source-specific vars  
  select(recID, mpvID, waID, feID, wapoID, inID,
         inSource, source, source.overlap, legacy.pursuit,
         name:year, cod, mod, not.kbp, race:age,
         city, county, zip, latitude, longitude,
         agency, agency.type, agency.ori, leg.year,
         armed, weapon, fleeing,
         pursuit.type, pursuit.victim, pursuit.injury, pursuit.incinum,
         description, contains("notes"),
         url_info, url_click, url_addl,
         # Officer variables
         officer_names_wa = officer_names,
         officer_names_mpv = officer.names,
         officer.previous,
         # MPV only variables
         context:context.detail, call4svc, bodycam, 
         mh_crisis, threat.level, threat.description,
         officer.names:officer.previous,
         # Links (most from WA)
         officer_url:ME_url, url_pic, url_prosecutor = url.prosecutor,
         # Accountability info (MPV)
         case.disposition, criminal.charges, charge.outcome, 
         prosecutor.special, independent.investigation,
         # MPV geographic variables
         census.tract:congress.dist 
         # EOs and parties taken from WA below
  ) %>%
  
  arrange(date)


# Join leg.info ----
# This is a premade dataset, read in via scrapeDataSets.R above
# Uses geocodio created file, with all FE incidents (thru 90094)
# Merge is by cityname, so will match all new cases from cities
# included thru 90094.  If a new city crops up (rare), this needs to
# be added to the geocodio csv file manually, and the make_leg_info
# file rerun to rebuild the leg info file.

## Add to all years dataset
wa_all <- left_join(wa_all_draft, wa_leg_info_city)

## check for missing cases, if so stop and fix

if(any(is.na(wa_all$WA_District))) {
  mld <- wa_all[is.na(wa_all$WA_District), 
                          c("waID", "mpvID", "feID", "city")]
  message("Missing LegDists for: \n\n")
  print(mld)
  message("Need to fix missing LDs")
}

## Create 2015 dataset

wa_2015 <- wa_all %>% filter(year > 2014)
  
# Save WA clean and merged datasets as Rdata files ----
## Note that the fe and mpv data include all cases, not just WA.
## For WA only analysis, use the wa_2015

## All years
selection <- "all years"
wa_clean <- wa_all

save(list = c("wa_clean", "fe_clean", "mpv_clean", "walocal_clean", 
              "selection", "scrape_date"),
     file = here("Data", "Clean", "WA_allyrs.rda"))

##  2015 and later ----
selection <- "2015+"
wa_clean_2015 <- wa_2015
fe_clean_2015 <- fe_clean %>% filter(date > "2014-12-31")
mpv_clean_2015 <- mpv_clean %>% filter(date > "2014-12-31")
walocal_clean_2015 <- walocal_clean %>% filter(year > 2014)
 

last.name.message <- paste("\n *** Last WA fatality: ",
                           wa_clean_2015$name[nrow(wa_clean_2015)],
                           wa_clean_2015$date[nrow(wa_clean_2015)],
                           "\n\n")

save(list = c("wa_clean_2015", "fe_clean_2015", "mpv_clean_2015", "walocal_clean_2015", 
              "selection", "scrape_date", 
              "last_date_mpv", "last_date_wa", "last_date_in",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("Data", "Clean", "WA_2015.rda"))


# Print summary of run

if(exists("resistant.message")){message(resistant.message)}
message(update.message.wa)
message(update.message.mpv)
message(last.name.message)
#message(wtsc.update.message)

# Quick descriptives
#Hmisc::describe(merge4)
