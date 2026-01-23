# WA local data cleaning script ----

## input:  walocal_raw
## interim: walocal_draft
## output: walocal_clean

# How cases are identified:

## 1. Google search alerts - entered manually

## 2. IncarcerNation
## For now cases from IncarcerNation are matched and entered manually
## from: https://incarcernation.com/
## A complete backfill was done 10/24/2025 to catch up
## Cases will need to be checked manually online moving forward
## Unless they give me access to the backend database
## 2 fields:  inID, inSource  -- the latter for cases identified via IN

## 3. MPV -- identified during the merge process later
## Entered here to allow for review and for completion of WA specific fields
## with the ME and IIT urls 

walocal_draft <- walocal_raw %>%
  mutate(name = case_when(
           name == "Name withheld by police" ~ "Unknown",
           is.na(name) ~ "Unknown",
           TRUE ~ name),
         name = str_remove(name, " Jr."),
         name = str_remove(name, " Jr$"),
         name = str_remove(name, " Sr."),
         name = str_remove(name, " III"),
         name = str_remove(name, " II"),
         name = str_remove(name, " IV"),
         name = str_remove(name, " V$"),
         
         name = str_replace_all(name, "-"," "),
         name = str_remove(name, "\\."),
         
         # prep for assignment
         fname = "NA", 
         lname = "NA") %>%
  
  mutate(
    race = case_when(
      is.na(race) | race == "race unspecified" | race == "" ~ "Unknown",
      grepl("Asian", race) ~ "API",
      grepl("Black", race) ~ "BAA",
      grepl("Latino", race) ~ "HL",
      grepl("Middle Eastern", race) ~ "ME",
      grepl("Native", race) ~ "NAA",
      grepl("White", race) ~ "WEA",
      TRUE ~ race),
    race = fct_relevel(race, "Unknown", after = Inf)
  ) %>%
  
  mutate(gender = str_to_sentence(gender),
         gender = case_when(
           is.na(gender) | gender == "" ~ "Unknown",
           grepl("Trans", gender) ~ "Other",
           TRUE ~ gender),
         gender = fct_relevel(`gender`, "Unknown", after = Inf)
  ) %>%
  
  # age needs missing value, since age is used to merge
  mutate(age = ifelse(is.na(age), 999, age) 
  ) %>%
  
  mutate(month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  mutate(st = "WA",
         state = state_fullname_fn(st),
         state.num = match(st, st.abb51)
  ) %>%
  
  # agency.type classifies only for single agency incidents
  # otherwise it is coded "Multiple agencies" for now
  # agency specific coding found in pursuit analysis file in Pursuits repo
  mutate(
    agency.type = case_when(
      agency=="" ~ "Unknown",
      grepl(",", agency) ~ "Multiple agencies",
      grepl("Campus|University|College|School", agency) ~ "Campus Police",
      grepl("State|Patrol", agency) ~ "State Police",
      grepl("Sheriff|Sherrif|sheriff", agency) ~ "County Sheriff",
      grepl("Tribe|Tribal|Nation[^a]", agency) ~ "Tribal Police",
      grepl("Police|police|Public Safety|Town|City", agency) ~ "Local Police",
      grepl("Tombstone|Corpus Christi|White Settlement|Charlotte-Mecklenburg|Patagonia", agency) ~ "Local Police",
      grepl("Correction", agency) ~ "Corrections Dept",
      #grepl("Alcohol Tobacco Firearms", agency) ~ "US ATF", too few
      grepl("U.S.*Border.", agency) ~ "US CBP",
      grepl("U.S. Federal Bureau", agency) ~ "US FBI",
      #grepl("U.S. Immigration", agency) ~ "US ICE", too few
      grepl("U.S. Marshal", agency) ~ "US Marshals",
      grepl("Port|National Guard|Metroparks", agency) ~ "Other",
      grepl("U.S.|National|Federal|Pentagon", agency) ~ "Other Federal",
      grepl("County|Harris Constable", agency) ~ "Other County",
      TRUE ~ "Other State"),
    agency.type = factor(
      agency.type, 
      levels = c("Local Police", "County Sheriff", "State Police", "Tribal Police",
                 "Campus Police", "Corrections Dept", 
                 "US CBP", "US FBI", "US Marshals", 
                 "Other Federal", "Other State", "Other County", "Other",
                 "Multiple agencies", "Unknown"))
  ) %>%
  
  # MPV is not coding types of force used separately from COD,
  # and it codes multiple types of force in this field, so we follow that
  # But if gunshot is in the field, we code that as COD
  mutate(cod = case_when(
           grepl("Gunshot", cod) ~ "Gunshot",
           grepl(",", cod) ~ "Multiple types of force",
           TRUE ~ cod),
         cod = factor(cod),
         cod = fct_relevel(cod, "Multiple types of force", after = Inf)
  ) %>%
  mutate(armed = case_when(
           armed == "Armed" ~ "Alleged Armed",
           armed == "Unarmed" ~ "Unarmed",
           TRUE ~ "Unknown"),
         armed = fct_relevel(armed, "Unknown", 
                             after = Inf)
  ) %>%
  mutate(weapon =case_when(
           grepl("Edged|knife|Knife|machete", weapon)  ~ "Alleged edged weapon",
           grepl("Firearm|Gun|Rifle", weapon)  ~ "Alleged firearm",
           weapon == "None" ~ "No weapon",
           weapon == "" | is.na(weapon) ~ "Unknown",
           TRUE ~ "Other"),
         weapon = fct_relevel(weapon, "Unknown", 
                              after = Inf)
  ) %>%
  # Some hierarchical coding of multiple types here, with Vehicle > Motorcycle > Foot > Other
  mutate(fleeing = case_when(
           is.na(fleeing) | fleeing == "" ~ "Unknown",
           grepl("Uncertain", fleeing) ~ "Unknown",
           grepl("Not|None", fleeing) ~ "Not fleeing",
           grepl("Veh|veh", fleeing) ~ "Vehicle",
           grepl("Motor|motor", fleeing) ~ "Motorcycle",
           grepl("foot|Foot", fleeing) ~ "Foot",
           TRUE ~ "Other"),
         fleeing = factor(fleeing, 
                       levels = c("Not fleeing", "Vehicle", "Motorcycle", "Foot", 
                                  "Other", "Unknown"))

  ) %>%
  # NOTE: the mh_crisis field has not been carefully populated
  # will use MPV variable for shared cases
  
  replace_na(list(mh_crisis = "Unknown")) 

# Pursuit variables ----

## Legacy cases are handled later after merging with MPV
## Post 10/2025 cases have been coded directly in the walocal xlsx file
  

### create clickable url for Rpubs reports
walocal_draft$url_click <- sapply(walocal_draft$url_info, make_url_fn)

# Victim first and last name processing ----

name.list <- str_split(walocal_draft$name, " ")
for(i in 1:length(name.list)) {
  walocal_draft$fname[i] <- name.list[[i]][1]
  walocal_draft$lname[i] <- name.list[[i]][length(name.list[[i]])]
}


walocal_clean <- walocal_draft %>%
  select(
    waID, inID, inSource, name, fname, lname,
    date, month, day, year,
    city, county, st, state, state.num, zip, 
    latitude, longitude,
    race, gender, age,
    cod, mod, 
    armed, weapon, fleeing, mh_crisis,
    pursuit.type:pursuit.incinum,
    agency, agency.type, officer_names,
    description, url_info, url_click, url_addl, officer_url,
    victim_url, IIT_url, ME_url
  )



