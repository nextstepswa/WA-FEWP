rm(list=ls())

##### What this file does ################################################################
#
# This script runs 4 main tasks:
# 1. Scrapes current data from MPV and loads WA newnames
# 2. Cleans the data -- fixes and harmonizes
# 3. Merges in data from the FE archive (final cleaned version) for cases not tracked by MPV
# 4. Extracts the merged complete dataset for WA state only
#
# Resulting cleaned data is saved as an output
#
# The code is designed to stop with an informative message if it finds errors
# that need to be manually addressed

# The typical reasons for this will be:
## new pursuit review/coding
## changes in source file formats (common for MPV)
## duplicate cases resulting from the WA-MPV merge

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
### WA newnames - uses original FE inclusion criteria, 2022+ with some additional backfilling
### Legislative districts (and state leg info for WA)
#### Updates to this only needed for redistricting and elections
#### This requires a manual geocoding update from geocodio

source(here("Construction", "scrapeDataSets.R"))


# Cleaning & Variable construction ----

## MPV cleaning ----

## Fix MPV errors:
## These errors are typically ID'd during merging and reported
## If they are not due to lag in info, or not fixed, they are fixed here

## After this, the harmonized variables are constructed.

source(here("Construction", "fixes_mpv.R"))
source(here("Construction", "harmonize_mpv.R"))


## WA newname cleaning ----

## File format was based on FE, so needs to be harmonized
## adds the WA cases not in MPV to the walocal_clean dataframe

source(here("Construction", "harmonize_wa.R"))



## FE extra cases ----

## Two types of FE incidents are not in MPV:  pre-2013 and intentional exclusions.

## MPV intentionally excludes cases from FE that where the COD is
## not a homicide (i.e., suicides and accidents), and excludes vehicular
## homicides not committed by police.  
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

## This is the pre-cleaned FE dataset from the previous repo

fe_clean_xtra <- anti_join(fe_clean, mpv_clean,
                           by = "feID")

## WaPo extra cases
## It's not clear why these are not in MPV.  Need to dx and track down later
## For now we ignore them.  If they are included in fe_extra, or in walocal_clean
## we'll still get them in the data set.

wapo_clean_xtra <- anti_join(wapo_clean, mpv_clean,
                             by = "wapoID")

## WA newname cleaning ----

## File format was based on FE, so needs to be harmonized
## adds the WA cases not in MPV to the walocal_clean dataframe

source(here("Construction", "harmonize_wa.R"))


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

last_data_update <- max(last_date_mpv, last_date_wa)

last_update_is_eoy <- month(last_data_update)==12 & 
  day(last_data_update)==31
last_complete_mo <- ifelse(last_update_is_eoy | month(last_data_update)==1, 
                           12, 
                           month(last_data_update)-1)
last_complete_yr <- ifelse(last_update_is_eoy, 
                           year(last_data_update), 
                           year(last_data_update)-1)

save(list = c("mpv_clean", "walocal_clean", "fe_clean_xtra", "wapo_clean_xtra",
              "selection", "scrape_date", 
              "last_date_wa", "last_date_mpv", 
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("Data", "Clean", "CleanSourceData.rda"))

update.message.wa <- paste("\n *** Last WA update: ", last_date_wa, "\n\n")
update.message.mpv <- paste("\n *** Last MPV update: ", last_date_mpv, "\n\n")


# Merge MPV and WA  -----

## For the WA state data collected locally (2022+, with a few backfills to earlier years)
## Our data (walocal_clean) has broader inclusion criteria, so MPV matches will be a subset
## Errors found during matching trigger stops
## Race is hierarchically assigned from wa first, mpv second
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
  

# Add cases prior to 2022 (start of WA data) ----

## Resulting file:
##  2000-2012 all from FE clean, non-MPV years (added)
##  2013-2021 MPV + FE extra (added)
##  2022+     MPV + walocal (merged)

## FE WA cases 2000-2021 ---- 
## fe_clean_xtra = the final clean FE data from the previous repo (fewa), antijoined to MPV
## non-MPV years + excluded by MPV 2013-2021

fe_clean_xtra_wa <- fe_clean_xtra %>%
  filter(st == "WA") %>%
  rename(fleeing = flee)

# MPV cases 2013-2021 ----
## Note: MPV has some cases not included in FE during this period, but none in WA

mpv_pre2022_wa <- mpv_clean_wa %>%
  filter(year < 2022)
 
## Add to existing merged walocal.mpv file
wa_mpv_fextra_all <- wa_mpv %>%
  bind_rows(fe_clean_xtra_wa, .id = "source1") %>%
  bind_rows(mpv_pre2022_wa, .id = "source2") %>%
  mutate(source = case_when(
    source1 ==1 ~ "WA",
    source1 ==2 ~ "FE",
    TRUE ~ "MPV")
  ) %>%
  arrange(date) %>%
  select(-c(source1, source2))

# Final variable prep for 2015+ WA data ----

# We construct the 2015+ dataset to store, but will restrict to 2015+ for
# pursuit coding later

## Consensus variable construction: ----

## 1. Use WA data if available
##   Any errors we find in WA are fixed during the merging
##   so WA has the most reliable data 2022+.
##   

## 2. For 2013-2021 we have our cleaned FE WA data if it was
##   excluded from MPV, but we are relying
##   on MPV data otherwise. 
##   To Do:  Check if our FE fixes for these cases and these years
##           have been incorporated into MPV.  

## 3. Legislative year variable
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

  ## Create unique record locator using source, yr and seqnum
  mutate(recID = paste(source, year, 1:nrow(wa_mpv_fextra_all), sep="-")) %>%
  
  ## all cases from MPV and WA are kbp, FE has only not.kbp
  mutate(not.kbp = case_when(
    source == "FE" ~ not.kbp,
    TRUE ~ 0)
  ) %>%
  
  mutate(leg.year = 
           cut(date, 
               breaks = as.Date(paste(2000:(cut.yr), "-08-01", sep="")),
               labels = c(paste(month.abb[8], 2000:(cut.yr-1), "-", 
                                month.abb[7], 2001:cut.yr,
                                sep = ""))),
         
         # crisis vars not consistent, MPV foreknowledge has fewer NA and is likely updating:
         mh_crisis = if_else(is.na(foreknowledge), symptoms, foreknowledge),
         
         # MPV cases are all homicides; code cases from that source
         homicide = if_else(is.na(homicide) & source=="MPV", 1, homicide)
  ) %>%
  
  # Select consensus variables, along with the few source-specific vars  
  select(recID, mpvID, waID, feID, wapoID, source,
         name:county, zip, latitude:agency.type, agency.ori, leg.year,
         description,
         url_info, url_click,
         mh_crisis,
         # WA only variables
         circumstances, 
         homicide, suicide, not.kbp, medical, 
         vpursuit.draft, raceOrig, officer_names,
         # MPV only variables
         context:context.detail, call4svc, bodycam, threat.level, threat.description,
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

# Finalize pursuit coding for 2015+ ----
## See the external file for information on the process
## Matching back on relies on feID, so merge the ID fields from WA and FE

pursuit_draft_2015 <- wa_all_draft %>% 
  filter(year > 2014) %>%
  mutate(feID = ifelse(is.na(feID), waID, feID))

source(here::here("Construction", "pursuit_coding.R"))

## Add vpursuit codes to pursuit_draft_2015
### Since the cod, url_info and description may have been improved during pursuit
###   review, we use final versions from the coded.pursuits df.
### Also adds the new coded vars vpursuit, victim, injury
### pursuit.type merges Active and Terminated pursuits into "Pursuit"
### feID is reverted to original missing values post 2022

wa_2015_draft <- left_join(pursuit_draft_2015 %>% select(-c("cod", "description", "url_info")), 
                              coded.pursuits %>% select(-c("name", "date")),
                              by = "feID") %>%
  mutate(feID = ifelse(feID > 90000, NA_real_, feID),
         pursuit.type = ifelse(grepl("Active|Terminated", vpursuit), "Pursuit", vpursuit)
  )

wa_2015_draft$url_click <- sapply(wa_2015_draft$url_info, make_url_fn)
  


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
  stop("Fix missing LDs")
}

## And to 2015 dataset

wa_2015<- left_join(wa_2015_draft,  wa_leg_info_city)
  
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
walocal_clean_2015 <- walocal_clean %>% filter(date > "2014-12-31") # no cases yet, but may be in future
 

last.name.message <- paste("\n *** Last WA fatality: ",
                           wa_clean_2015$name[nrow(wa_clean_2015)],
                           wa_clean_2015$date[nrow(wa_clean_2015)],
                           "\n\n")

save(list = c("wa_clean_2015", "fe_clean_2015", "mpv_clean_2015", "walocal_clean_2015", 
              "selection", "scrape_date", 
              "last_date_mpv", "last_date_wa",
              "last_data_update", "last_complete_mo", 
              "last_complete_yr", "last_update_is_eoy"),
     file = here("Data", "Clean", "WA_2015.rda"))


# Print summary of run

if(exists("resistant.message")){message(resistant.message)}
message(update.message.wa)
message(update.message.mpv)
message(last.name.message)
message(pursuit.coding.message)
message(wtsc.update.message)

# Quick descriptives
#Hmisc::describe(merge4)
