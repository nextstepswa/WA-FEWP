# Merge MPV and WA  -----

## If MPV has cases missing from walocal_clean, we stop and add those
##   this gives us a chance to verify MPV data and add IIT & ME urls

## If walocal_clean has cases that should be included in MPV, we report that
## And if there are inconsistencies in info, we report that too

# We merge wa_local and MPV (MPV for for 2022+ only: after FE stopped reporting)
# Almost all walocal_clean cases come from that time frame
## Some earlier cases in walocal_clean, found after FE stopped
##   None found in MPV by inspection, most are VP outside the MPV inclusion criteria
##   Might be good to automate that checking moving forward

## Merge using stringdist
merge_draft <- stringdist_full_join(
  mpv_clean_wa %>% filter(year > 2021), 
  walocal_clean,
  by = c("lname", "fname", "date", "city"),
  max_dist = 2,
  ignore_case = T) %>%
  
  rename_with(~gsub(".x", ".mpv", .x, fixed=T)) %>%
  rename_with(~gsub(".y", ".wa", .x, fixed=T)) %>%
  
  # select(-rowID) %>%
  
  select(contains("ID"), inSource, contains(".wa"), contains(".mpv"), !contains(".wa"))

## Validate the merge ----

## This is a multi-stage process, output needs to be carefully reviewed for errors
## every time the script is run.  

## If errors are found, fixes are made either to the 
## * pre- and/or post- cleaning fix files, if they are data errors
## * additions to the fe_newnames.xlsx file, if new record is needed
## * postmerge fix file, if they are bad matches after all other fixes
## And this script is run again until it runs cleanly

### 1. Unmatched MPV cases ----

## Should be none b/c MPV is a subset of WA
## Stop if found, and fix by inspection 

## A.  If there is no matching WA record at all, add one to fe_newnames.xlsx

## B. If there is an WA record, but info needed for match is missing/inconsistent
## This is usually temporary, and fix (MPV and/or WA) is implemented in fixes_postcleaning.
## That fix is deleted when no longer needed.

unmatched.MPV <- merge_draft %>% 
  filter(is.na(waID)) %>% 
  select(mpvID, fname.mpv, lname.mpv, date.mpv, city.mpv)

if(nrow(unmatched.MPV) > 0){
  
  message("\n *** Unmatched MPV records: *** \n\n")
  print(unmatched.MPV)
  stop("\n\n Stopping to fix unmatched MPV records \n\n")
  
} else {
  message("\n *** No unmatched MPV records *** \n\n")
}

### 2. Duplicate matches from the merge ----

## Stop and fix manually
## Most should be fixed by modifying incorrect info in the
## pre or post cleaning fix files.  But if that is not enough to
## prevent the match, then as a last resort the match is reversed by hand in
## the fixes_postmerge file

## Stop if found, and fix by inspection 
## A. If due to bad or missing info, fix in pre-cleaning
## B. If resistant, fix manually in fix_(fe/mpv)dupes.R (last resort).
## Note: this can't be automated; instead need to
##  modify the script in fix_(wa/mpv)dupes.R with the relevant IDs
##  set resistant to 1 below to source the script

## 

#### a. Duplicate waIDs ----

dupe.wa <- table(merge_draft$waID)

# This should normally be set to 0, unless there has been a new resistant
# duplicate match.  Then set at 1 and leave until no longer needed.

resistant <- 0

# Check if resistant should be reset
if(resistant==1) {
  if(!any(dupe.wa>1)){ 
    resistant <- 0
    resistant.message <- "reset resistant duplicate match to 0 for WA"
    message(resistant.message)
  } else {
    resistant.message <- "resistant duplicate match left at 1 for WA"
    message(resistant.message)
  }
}

if(any(dupe.wa > 1)){
  
  names(dupe.wa[dupe.wa>1])
  sort(unique(dupe.wa))
  message("\n *** Duplicate WA IDs, #times, 99999 means unmatched MPV case")
  print(dupe.wa[dupe.wa>1])
  
  if(resistant==0) {
    stop("\n\n Stopping to fix duplicate WA IDs \n\n")
  } else {
    message("\n\n Fixing WA dupes by script \n\n")
    source(here("Construction", "fix_fe_dupes.R"))
  }
  
} else {
  message("\n *** No duplicate WA IDs **** \n\n")
}

#### b. Duplicate mpvIDs ----

dupe.mpv <- table(merge_draft$mpvID)

# This should normally be set to 0, unless there is a new resistant
# duplicate match.  Then set at 1 and leave until no longer needed.

resistant <- 0

# Check if resistant should be reset
if(resistant==1) {
  if(!any(dupe.mpv>1)){ 
    resistant <- 0
    resistant.message <- "reset resistant duplicate match to 0 for MPV"
    message(resistant.message)
  } else {
    resistant.message <- "resistant duplicate match left at 1 for MPV"
    message(resistant.message)
  }
}

if(any(dupe.mpv>1)){
  
  names(dupe.mpv[dupe.mpv>1])
  sort(unique(dupe.mpv))
  message("\n *** Duplicate MPV IDs, #times \n\n")
  print(dupe.mpv[dupe.mpv>1])
  
  if(resistant==0) {
    stop("\n\n Stopping to fix duplicate MPV IDs \n\n")
  } else {
    message("\n\n Fixing MPV dupes by script \n\n")
    source(here("Construction", "fix_mpv_dupes.R"))
  }
  
} else {
  message("\n *** No duplicate MPV IDs **** \n\n")
}


### 3. Inconsistencies in merged info ----

## Fuzzy string matching allows for some errors in key variables
## Fix by inspection, in wa_local, or
## using the fixes_mpv.R (and report)

attribute.mismatch <- merge_draft %>%
  filter(!is.na(waID) & !is.na(mpvID)) %>%
  mutate(flname.mpv = paste(fname.mpv, lname.mpv),
         flname.wa = paste(fname.wa, lname.wa)) %>%
  filter(flname.mpv != flname.wa |
           date.wa != date.mpv |
           as.character(gender.mpv) != as.character(gender.wa) | 
           age.mpv != age.wa |  
           city.mpv != city.wa) %>%
  select(waID, mpvID, flname.wa, flname.mpv, date.wa, date.mpv,
         gender.wa, gender.mpv, age.wa, age.mpv, city.wa, city.mpv)

if(nrow(attribute.mismatch)>1){
  message("\n *** Attribute mismatches: \n")
  attribute.mismatch %>% arrange(mpvID) %>% print(., n=100)
  stop("\n *** Attribute mismatches need to be fixed ")
  
} else {
  message("\n *** No attribute mismatches \n\n")
}


#### d. County mismatch 
county.mismatch <- merge_draft %>%
  filter(county.mpv != county.wa & !is.na(mpvID) & county.mpv != "") %>%
  select(waID, mpvID, city.wa, city.mpv, county.wa, county.mpv, st.wa, st.mpv)

if(nrow(county.mismatch)>0){
  message("\n *** County mismatches for review: \n\n")
  county.mismatch %>% arrange(mpvID) %>% print(., n=100)
  stop("\n *** County mismatches need to be fixed")
  
} else {
  cat("\n *** No county mismatches \n\n")
}

cat(paste("\n *** ", sum(is.na(merge_draft$mpvID)), "additional cases in WA local data \n\n"))


# #### e. Agency mismatch -- Dx later
# agency.mismatch <- merge_draft %>%
#   filter(agency.mpv != agency.wa & !is.na(mpvID) & agency.mpv != "") %>%
#   select(waID, mpvID, city.wa, city.mpv, agency.wa, agency.mpv, st.wa, st.mpv)
# 
# if(nrow(agency.mismatch)>0){
#   message("\n *** Agency mismatches for review: \n\n")
#   agency.mismatch %>% arrange(mpvID) %>% print(., n=100)
#   stop("\n *** Agency mismatches need to be fixed")
#   
# } else {
#   cat("\n *** No agency mismatches \n\n")
# }

# Create consensus race variable when possible
# for now, just race.  
# Other vars (gender:agency) show .wa has most info

merge_walocal_mpv <- merge_draft %>%
  mutate(race.wa = if_else((race.wa=="Unknown" & !is.na(race.mpv)), race.mpv, race.wa))

