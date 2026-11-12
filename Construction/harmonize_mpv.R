# MPV cleaning script ----

## input:  mpv_temp (from fixes_preclean_mpv.R; IDs, name and date fields given harmonized names there)
## interim: mpv_draft
## output:  mpv_clean

num <- 1:51

mpv_draft <- mpv_temp %>%
  
  mutate(mpvID = mpv_id) %>%
  
  # Victim info ----

mutate(name = case_when(
  is.na(name) ~ "Unknown",
  grepl("Withheld", name) ~ "Unknown",
  TRUE ~ name),
  
  name = str_remove(name, " Jr"),
  name = str_remove(name, " Sr"),
  name = str_remove(name, " III"),
  name = str_remove(name, " II"),
  name = str_remove(name, " IV$"),
  name = str_remove(name, " V$"),
  name = str_remove(name, " aka.*"),
  name = str_replace_all(name, "-"," "),
  name = str_replace_all(name, "\\.|,",""),
  
  # preserve aka names as "name2"
  name2 = if_else(grepl(" or ", name), str_remove(name, ".* or "), ""),
  # and remove the aka from the primary name
  name = str_remove(name, " or .*"),
  
  fname = "NA", # prep for assignment below
  lname = "NA"
) %>%
  
  mutate(gender = if_else(is.na(gender), "Unknown", str_to_sentence(gender)),
         gender = fct_relevel(gender, "Unknown", after = Inf)
  ) %>%
  
  ## Race - not sure what's happening with imputation for MPV
  ## Using the new census categories https://www.census.gov/about/our-research/race-ethnicity/standards-updates.html
  mutate(race.orig = race,
         
         race = case_when(
           grepl("Indian", race.orig) ~ "AIAN",
           race.orig == "Asian" ~ "ASIA",
           race.orig == "Black" ~ "BAA",
           grepl("Hispanic", race.orig) ~ "HL",
           grepl("Pacific", race.orig) ~ "NHPI",
           race.orig == "White" ~ "WEA",
           grepl("Unknown", race.orig) ~ "Unknown",
           TRUE ~ race.orig),
         race = fct_relevel(race, "Unknown", after = Inf)
  ) %>%
  
  mutate(month = month(date, label=T),
         day = day(date),
         year = year(date)
  ) %>%
  
  # Location info ----

mutate(st = st.abb51[match(state, state.name51)],
       state.num = num[match(state, sort(state.name51))]
) %>%
  
  # Agency info ----

mutate(agency = agency_responsible,
       
       # only possible to code if single agency
       agency.type = case_when(
         is.na(agency) ~ "Unknown",
         grepl(",", agency) ~ "Multiple agencies",
         grepl("(Campus|University|College)", agency) ~ "University Campus Police",
         grepl("(State|Patrol)", agency) ~ "State Police",
         grepl("Sheriff", agency) ~ "County Sheriff",
         grepl("(Police|police|Public Safety|Town|City)", agency) ~ "Local Police",
         grepl("(Cleveland|Detroit|Geneva|Mescalero)", agency) ~ "Local Police",
         grepl("Correction", agency) ~ "Corrections Dept",
         grepl("County", agency) ~ "Other County unit",
         grepl("Bureau of Alchohol", agency) ~ "US ATF",
         grepl("Drug", agency) ~ "US DEA",
         grepl("Border", agency) ~ "US CBP",
         grepl("Federal Bureau", agency) ~ "US FBI",
         grepl("Immigration", agency) ~ "US ICE",
         grepl("Homeland", agency) ~ "US HS",
         grepl("U.S. Marshal", agency) ~ "US Marshals",
         grepl("(U.S.|Federal|Military|Pentagon|National|Air Force|Bureau|Internal)", agency) ~ "Other Federal unit",
         grepl("(Tribal|Navajo|Nation)", agency) ~ "Tribal Police",
         TRUE ~ "Other State unit"),
         #TRUE ~ "Check if all state units"),
       
       agency.type = factor(agency.type,
                            levels = c("Local Police", "County Sheriff", "State Police",
                                       "Tribal Police", "University Campus Police", "Corrections Dept",
                                       "US ATF", "US DEA", "US CBP", "US FBI", "US ICE", "US HS", "US Marshals", 
                                       "Other State unit", "Other County unit", "Other Federal unit",
                                       "Multiple agencies", "Unknown"
                                       #"Check if all state units"
                                       )),
       
       agency.ori = ori
) %>%
  
  # COD info ----
## Based on the use_of_force variable,
## which records the types of force used not the official cause of death
## We aggregate the multiples into one category

mutate(cod = use_of_force,
       cod = case_when(
         grepl(";", cod) ~ "Multiple types of force",
         grepl("Gunshot", cod) ~ "Gunshot",
         grepl("asph|restraint", cod) ~ "Asphyxiated/Restrained",
         grepl("Physical", cod) ~ "Physical force",
         grepl("Energy", cod) ~ "Taser",
         grepl("Chemical", cod) ~ "Chemical weapon", # too few to break out?
         grepl("Vehicle", cod) ~ "Vehicle",
         TRUE ~ "Other"),
       cod = factor(cod),
       cod = fct_relevel(cod, "Multiple types of force", after = Inf),
       cod = fct_relevel(cod, "Other", after = Inf)
) %>%
  
  # Use of force info ----
  ## how often alone, and in combination

mutate(
  uof = use_of_force,
  
  uof.chem = case_when(
    grepl('irritant;|; Chemical', use_of_force) ~ "Chemical plus other",
    grepl("irritant", use_of_force) ~ "Chemical weapon only",
    TRUE ~ "Not used"),

  uof.taser = case_when(
    grepl('Device;|; Conducted', use_of_force) ~ "Taser plus other",
    grepl("Device", use_of_force) ~ "Taser weapon only",
    TRUE ~ "Not used"),
  
  uof.gun = case_when(
    grepl('shot;|; Gun', use_of_force) ~ "Gunshot plus other",
    grepl("irritant", use_of_force) ~ "Gunshot only",
    TRUE ~ "Not used"),
  
  uof.phy = case_when(
    grepl('force;|; Phy', use_of_force) ~ "Physical plus other",
    grepl("force", use_of_force) ~ "Physical force only",
    TRUE ~ "Not used"),
  
  uof.prj = case_when(
    grepl('Projectile;|; Projectile', use_of_force) ~ "Projectile plus other",
    grepl("Projectile", use_of_force) ~ "Projectile only",
    TRUE ~ "Not used"),
  
  uof.asp = case_when(
    grepl('asphyxiation;|; Prone', use_of_force) ~ "Aspyxiation/Restraint plus other",
    grepl("asphyxiation", use_of_force) ~ "Asphyxiation/Restraint only",
    TRUE ~ "Not used"),
  
  uof.veh = case_when(
    grepl('Vehicle;|; Vehicle', use_of_force) ~ "Vehicle plus other",
    grepl("Vehicle", use_of_force) ~ "Vehicle only",
    TRUE ~ "Not used"),
  
) %>%
  
  ## Description ----
mutate(description = circumstances) %>%
  
  ## Case disposition  ----

mutate(case.disposition = disposition_official,
       
       ## Admin review
       admin.rev = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("Admin", case.disposition) ~ "Yes",
         TRUE ~ "No info"),
       admin.rev = factor(admin.rev,
                          levels = c("Yes", "No info", "Unknown")),
       
       # Note there is info on both external and internal review; not pulled out here
       admin.outcome = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("Admin.*no discipline", case.disposition) ~ "No discipline",
         grepl("Admin.*disciplined", case.disposition) ~ "Disciplined",
         grepl("Admin.*terminated", case.disposition) ~ "Terminated",
         grepl("Admin.*decertified", case.disposition) ~ "Decertified",
         grepl("Admin", case.disposition) ~ "Other",
         TRUE ~ "No info"),
       admin.outcome = factor(admin.outcome,
                          levels = c("Disciplined", "Terminated", "Decertified", "Other",
                            "No info", "Unknown")),
         
       civil.case = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("CI", case.disposition) ~ "Yes",
         TRUE ~ "No info"),
       civil.case = factor(civil.case,
                           levels = c("Yes", "No info", "Unknown")),
       civil.outcome = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("CI.*awarded", case.disposition) ~ "Damages awarded",
         grepl("CI.*settl", case.disposition) ~ "Settlement",
         grepl("CI.*blocked", case.disposition) ~ "Dismissed",
         grepl("CI.*dismiss", case.disposition) ~ "Dismissed",
         grepl("CI.*filed", case.disposition) ~ "Civil suit filed",
         grepl("CI.*not liable", case.disposition) ~ "Not liable",
         grepl("CI", case.disposition) ~ "Other",
         TRUE ~ "No civil suit"),
       civil.outcome = factor(civil.outcome,
                              levels = c("Civil suit filed", "Damages awarded", "Settlement", 
                                         "Not liable", "Dismissed", "Other",
                                         "No civil suit", "Unknown")),
       
       
       criminal.charge = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("CR", case.disposition) ~ "Yes",
         TRUE ~ "No info"),
       criminal.charge = factor(criminal.charge,
                                levels = c("Yes", "No info", "Unknown")),
       
       criminal.outcome = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("CR.*not indict", case.disposition) ~ "No indictment/charges",
         grepl("CR.*not file", case.disposition) ~ "No indictment/charges",
         grepl("CR.*dismiss", case.disposition) ~ "Dismissed",
         grepl("CR.*Mistrial", case.disposition) ~ "Mistrial",
         grepl("CR.*acquit", case.disposition) ~ "Acquitted",
         grepl("CR.*dropped", case.disposition) ~ "Dropped",
         grepl("CR.*convicted", case.disposition) ~ "Convicted",
         grepl("CR.*charged", case.disposition) ~ "Charged",
         grepl("CR", case.disposition) ~ "Other",
         TRUE ~ "No criminal case"),
       criminal.outcome = factor(criminal.outcome,
                                 levels = c("No indictment/charges",
                                            "Charged", "Dropped", "Dismissed", 
                                            "Convicted", 
                                            "Acquitted", "Mistrial", 
                                            "Other",
                                            "No criminal case", "Unknown")),
       
       
       federal.charge = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("Federal.*Civil", case.disposition) ~ "Civil rights suit",
         grepl("Federal.*Criminal", case.disposition) ~ "Criminal civil rights charges",
         TRUE ~ "No info"),
       federal.charge = factor(federal.charge,
                               levels = c("Civil rights suit", "Criminal civil rights charges", 
                                          "No info", "Unknown")),
       
       federal.outcome = case_when(
         is.na(case.disposition) ~ "Unknown",
         grepl("Federal. Prosecutor did not", case.disposition) ~ "No charges",
         grepl("Federal.*dismiss", case.disposition) ~ "Dismissed",
         grepl("Federal.*not liable", case.disposition) ~ "Not liable",
         grepl("Federal.*acquit", case.disposition) ~ "Acquitted",
         grepl("Federal", case.disposition) ~ "Other",
         TRUE ~ "No federal case"),
       federal.outcome = factor(federal.outcome,
                                levels = c("No charges", "Dismissed",  
                                           "Acquitted", "Not liable", 
                                           "Other",
                                           "No federal case", "Unknown"))
) %>%

  
  ## LE narrative ----

mutate(armed = allegedly_armed_with,
       
       armed = case_when(
         grepl("Unarmed", armed) ~ "Unarmed",
         grepl("Undetermined", armed) ~ "Unknown",         
         TRUE ~ "Alleged Armed")
) %>%
  
  mutate(weapon = allegedly_armed_with,
         
         weapon =case_when(
           grepl(";", weapon) ~ "Multiple weapons",
           grepl("Gun", weapon)  ~ "Firearm",
           grepl("Edged", weapon)  ~ "Edged weapon",
           grepl("Replica", weapon)  ~ "Replica weapon",
           grepl("Vehicle", weapon)  ~ "Vehicle",
           grepl("Other", weapon)  ~ "Other weapon",
           grepl("Unknown", weapon) ~ "Unknown weapon",
           TRUE ~ armed),
         weapon = fct_relevel(weapon, "Unarmed", after = 0),
         weapon = fct_relevel(weapon, "Multiple weapons", after = Inf),
         weapon = fct_relevel(weapon, "Other weapon", after = Inf),
         weapon = fct_relevel(weapon, "Unknown weapon", after = Inf),
         weapon = fct_relevel(weapon, "Unknown", after = Inf)
  ) %>%
  
  mutate(weapon.gun = case_when(
    grepl("Gun", allegedly_armed_with) ~ "Firearm",
    weapon == "Unarmed" ~ "Unarmed",
    weapon == "Unknown" ~ "Unknown",
    TRUE ~ "Another weapon"),
    weapon.gun = factor(weapon.gun,
                        levels = c("Firearm", "Another weapon", "Unarmed", "Unknown"))
    
  ) %>%
  
  # this variable is being updated in real time (https://mappingpoliceviolence.org/methodology)
  mutate(threat.description = if_else(is.na(threat_perception), 
                                      wapo_threat_level, 
                                      threat_perception)
  ) %>%
  
  mutate(footage = incident_footage,
         footage = case_when(
           grepl(";", footage) ~ "Multiple footage types",
           grepl("Body", footage) ~ "Bodycam",
           grepl("Bystand", footage) ~ "Bystander",
           grepl("Dash", footage) ~ "Dashcam",
           grepl("Surv", footage) ~ "Surveillance",
           grepl("Taser", footage) ~ "Taser",
           footage == "Other" ~ "Other",
           footage == "None" ~ "None",
           TRUE ~ "Unknown"),
         footage = fct_relevel(footage, "Multiple footage types", after = Inf),
         footage = fct_relevel(footage, "None", after = Inf),
         footage = fct_relevel(footage, "Unknown", after = Inf)
  ) %>%
  
  
  ## Officer info ----

mutate(offduty = off_duty_killing,
       offduty = ifelse(is.na(offduty), "Unknown", "Yes"),
       offduty = fct_relevel(offduty, "Unknown", after = Inf),
       
       officer.names = officer_names,
       officer.races = officer_races,
       officer.previous = officer_known_past_shootings, # needs cleaning
       officer.charges = officer_charged
) %>%  
  
  
  ## Context info ----

mutate(encounter.type = encounter_type,
       encounter.type = fct_relevel(encounter.type, "None/Unknown", after = Inf),
       
       initial.reason = initial_reason, # 2000 values
       
       symptoms = case_when(
         grepl("Drug", signs_of_mental_illness) ~ "Drug or alcohol",
         signs_of_mental_illness == "Yes" ~ "Mental illness",
         signs_of_mental_illness == "No" ~ "None",
         TRUE ~ "Unknown"),
       
       call4svc = call_for_service,
       call4svc = case_when(
         is.na(call4svc) | call4svc == "Unavailable" ~ "Unknown",
         TRUE ~ call4svc),
       call4svc = fct_relevel(call4svc, "Unknown", after = Inf)
) %>%
  
  ## Pursuit-related info ----
  
  # MPV only includes pursuit deaths when caused directly by police action
  #  * e.g., PIT/spike strip crash fatality or patrol car intentional vehicular fatality during a pursuit
  #  * excludes any pursuit-related fatalities otherwise caused by the fleeing vehicle

  # 3 fields include codes for potential pursuit-related fatalities
  #  * cod = Vehicle (only) these are always "Active pursuits"
  #  * weapon = Vehicle;
  #  * description includes "pursuit" or "chase" or "fled" or variants of these

  # These only allow for a first draft, lots of manual coding follows, and this is only done for WA
  # * "Involved pursuit" cases must be reviewed manually, not all descriptions include pursuit terms
  # * And it is not possible to code the pursuit.victim type or pursuit.incinum by script, 
  # * so that is done manually and read back in

# Code fleeing hierarchically, with Vehicle at top to identify possible pursuits
  mutate(flee.orig = flee,
         fleeing = case_when(
           is.na(flee) ~ "Unknown",
           grepl("Not", flee) ~ "Not fleeing",
           grepl("Car", flee) ~ "Vehicle",
           grepl("Foot", flee) ~ "Foot",
           TRUE ~ "Other"),
         fleeing = factor(fleeing, levels = c("Not fleeing", "Vehicle", "Foot", "Other", "Unknown"))
) %>%
  mutate(pursuit.type = if_else(grepl("Vehicle|vehicle", cod), "Active pursuit", "Unclear"),
         cod.vehicle = if_else(grepl("Vehicle|vehicle", cod), 1, 0),
         flee.vehicle = if_else(flee == "Vehicle", 1, 0),
         weapon.vehicle = if_else(grepl("Vehicle|vehicle", weapon), 1, 0),
         desc.pursuit = if_else(grepl("pursu|Pursu|chase|flee|fled", description), 1, 0)
  ) %>%
  rowwise() %>%
  mutate(pursuit.indicators = sum(cod.vehicle, flee.vehicle, weapon.vehicle, desc.pursuit, na.rm=T) 
  ) %>%
  
  ## Geographic-related info ----
  
  mutate(census.tract = tract,
         community.trulia = geography, # community vars have high missingness
         community.hud = urban_rural_uspsai,
         community.nchs = urban_rural_nchs,
         medhhinc.acs = hhincome_median_census_tract,
         census.pop = pop_total_census_tract, # a few cases missing pops
         census.nhw = pop_white_census_tract,
         census.nhb = pop_black_census_tract,
         census.na = pop_native_american_census_tract,
         census.asian = pop_asian_census_tract,
         census.pi = pop_pacific_islander_census_tract,
         census.om = pop_other_multiple_census_tract,
         census.hisp = pop_hispanic_census_tract,
         congress.dist = congressional_district_113, # about 30% missing CD
         congress.rep = paste(congressperson_firstname, congressperson_lastname),
         congress.rep.party = congressperson_party
  ) %>%
  
  ## Prosecutor info ----
mutate(chief.prosecutor = prosecutor_head, # 99% NA, need to clean for case status
       prosecutor.race = prosecutor_race,
       prosecutor.gender = prosecutor_gender,
       prosector.party = prosecutor_party,
       prosecutor.term = prosecutor_term,
       court.prosecutor = prosecutor_in_court,
       prosecutor.special = prosecutor_special, # 99% NA
       independent.investigation = independent_investigation,
       url.prosecutor = prosecutor_url
) %>%
  
  ## Media links ----

mutate(url_pic = victim_image, # 30% NA
       url_info = str_split_i(news_urls, ";", 1),
       url_info_all = news_urls # only a few have more than one
) %>%
  
  ## Create date/state/city-sorted rowID for cleaning ----
arrange(date, st, city) %>%
  mutate(rowID = row_number()) %>%
  
  ## Select a subset of the cleaned variables into the new df ----
select(mpvID, feID, wapoID, recordID=record_id, name, date, age, gender,race,
       street_address, city, county, state, st, zip, latitude, longitude,
       name2:call4svc, fleeing, pursuit.type:url_info_all, description) 


# Create clickable source url for Rpubs reports ----

mpv_draft$url_click <- sapply(mpv_draft$url_info, make_url_fn)

# Victim first and last name processing ----

name.list <- str_split(mpv_draft$name, " ")
for(i in 1:length(name.list)) {
  mpv_draft$fname[i] <- name.list[[i]][1]
  mpv_draft$lname[i] <- name.list[[i]][length(name.list[[i]])]
}

# Officer name data ----
## Create tidy officer name dataframe with all IDs
## one record per officer name (NAs stripped at the end)
## This is saved as a separate df

officer_names <- mpv_draft %>% 
  select(mpvID, feID, wapoID, st, year, officer.names) %>%
  mutate(name = officer.names,
         
         # fix long lists
         name = sub("Benjamin LeBlanc, officers.*",
                    "Benjamin LeBlanc, Officer Patrick Rubio, Officer Omar Tapia, Officer Luis Alvarado",
                    name),
         # fnames found here: https://news.yahoo.com/no-criminal-case.charges-6-spd-020200618.html
         name = sub("Austin, Cpl.*",
                    "Cpl. Anthony Guzzo, Officer Kyle Heuett, Officer Brandon Lynch, Officer Brandon Fabian, Officer Winston Brooks",
                    name),
         name = sub("Altoona Police Department:.*",
                    "Officer Kim Schuch, Officer Leah Wolff, Officer Gracia Larson, Officer Robert Schreier, Officer Michael Cullen, Deputy Joseph Wollum, Deputy Jacob Pake",
                    name),
         name = sub("Lake County Sheriff's Department deputy Victor Zamora.*",
                    "Deputy Victor Zamora, Officer Luke Schreiber, Officer Jacob Patzschke, Daniel Kolodzie",
                    name),
         name = sub("Officer Irwin, Norton, Davis. Becketts",
                    "Officer Irwin, Officer Norton, Officer Davis, Officer Becketts",
                    name),
         name = sub("Michael Dekun, James Heaslip, .*",
                    "Michael Dekun, James Heaslip, Ryan Young, BP agent Don Kress, BP agent Ryan Champion",
                    name),
         name = sub("Waukesha County Deputy James Soneberg.*",
                    "Deputy James Soneberg, Officer Daniel Bloedow",
                    name),
         name = sub("Lane County sheriff's dept: Sgt. Jason Wilson.*",
                    "Sgt. Jason Wilson, Deputy Derek Bastinelli, Deputy Marvin Combs, Deputy Brian Devault, Deputy Brian Jessee, Sgt. James Macfarlane, Officer Robert Merryman, Deputy Kody Reavis, Deputy Eric Tipler and Deputy Aaron Gardner",
                    name),
         name = sub("Arapahoe County Sheriff.*Deputy Robert Knudson",
                    "Deputy Robert Knudson",
                    name),
         name = sub("Everest Metropolitan Police Department Detective.*",
                    "Detective Sergeant Dan Goff, Deputy Matthew Bell",
                    name),
         name = sub("Reynaldo Rodriguez of the Bulloch County .*",
                    "Reynaldo Rodriguez, Gary Provost, Deputy Nathan Singletary",
                    name),
         name = sub(" and detectives Clay Saunders.*",
                    ", Detective Clay Saunders, Detective Jason Willis, Detective Thomas Akin",
                    name),
         name = sub("state law protects the identity of undercover officers.",
                    "Undercover Officer",
                    name),
         
         name = sub("\\(.*\\)", "", name), # delete parentheticals
         
         # fix comma issues; comma used for string separation later
         name = gsub(",  ", ", ", name),
         name = gsub(" and|, and", ",", name),
         name = gsub(" \\+", ",", name),
         
         name = sub("\\.$|\\. $|,$|, $", "", name), # delete trailing punctuation (need to do this last)
         
         # fix idiosyncratic problems
         name = sub(" and others", "", name),
         name = sub(", unknown", "", name),
         name = sub("32, ", "", name),
         name = sub("William Evans \\(deceased.*", "William Evans", name),
         name = sub("Geoffrey Barber Alex Jansen", "Geoffrey Barber, Alex Jansen", name),
         name = sub("Paulding County ", "", name),
         name = sub("fired several times", "", name),
         name = sub("Officers ", "", name),
         name = sub(", 2nd officer unknown", "", name),
         
         name = sub("declined.*|.*declined.*", NA, name),
         name = sub("5 officers involved", NA, name),
         name = sub("SWAT team arrived.*", NA, name),
         name = sub("4 officers", NA, name),
         name = sub("Multiple names in police report.*", NA, name)
         
  ) %>%
  
  separate_rows(name, sep = ',\\s*') %>%
  #separate_rows(name) %>%
  
  # Create title field
  mutate(title = case_when(
    grepl("Deputy", name) ~ "Deputy",
    grepl("Officer|officer", name) ~ "Officer",
    grepl("Det\\.|Detective|Det ", name) ~ "Detective",
    grepl("Cpl\\.", name) ~ "Corporal",
    grepl("Lt\\.", name) ~ "Lieutenant",
    grepl("Sgt\\.|Sgt", name) ~ "Seargant",
    grepl("Investigator", name) ~ "Investigator",
    grepl("Trooper First Class", name) ~ "Trooper First Class",
    grepl("Trooper", name) ~ "State Trooper",
    grepl("Senior Inspector", name) ~ "Senior Inspector",
    grepl("Senior Special Agent", name) ~ "Senior Special Agent",
    grepl("Sergeant", name) ~ "Sergeant",
    grepl("Sheriff", name) ~ "Sheriff",
    grepl("Safety Officer", name) ~ "Safety Officer",
    grepl("Patrolman", name) ~ "Patrolman",
    grepl("BP agent", name) ~ "Border Patrol Agent",
    grepl("Chief", name) ~ "Chief",
    grepl("MPO", name) ~ "MPO",
  ))  %>%
  
  # Delete titles from names
  mutate(name = ifelse(grepl("Roessel", name),
                       "Dahle Roessel",
                       name),
         name = sub(".*several times", "", name),
         name = sub("Deputy |Officer |officer |", "", name),
         name = sub("Det\\. |Detective |Det ", "", name),
         name = sub("Cpl\\. |Corporal |Lt\\. |Sgt. |Sgt |Investigator ", "", name),
         name = sub("Trooper First Class |Trooper |State Trooper ", "", name),
         name = sub("Senior Inspector |Sergeant |Patrolman |Sheriff ", "", name),
         name = sub("BP Agent |Chief |MPO ", "", name),
         name = sub("Safety Officer ", "", name),
         name = sub("Senior Special Agent ", "", name)
  ) %>%
  
  # fix initials and name decorators
  mutate(name = sub(" Jr\\.| Jr| Sr\\.| II| III|III ", "", name),
         name = sub("([A-Z])\\.([A-Z])\\.|([A-Z])\\. ([A-Z])\\.", "\\1\\2", name), # combine 2 initials
         name = gsub("\\.", "", name) # remove periods in all initials
  ) %>%
  
  # delete trailing whitespace
  mutate(name = sub(" *$| $", "", name)
  ) %>%
  
  # set up vectors for fname and lname
  mutate(fname = NA,
         lname = NA
  ) %>%  
  
  filter(!is.na(name)) 

name.list <- str_split(officer_names$name, " ")
for(i in 1:length(name.list)) {
  if(length(name.list[[i]]) > 1) {
    officer_names$fname[i] <- name.list[[i]][1]
    officer_names$lname[i] <- name.list[[i]][length(name.list[[i]])]
  } else { # only have last name 
    officer_names$lname[i] <- name.list[[i]][1]
  }
}

# Officer names WA ----

officer_names_wa <- officer_names %>% 
  filter(st == "WA")

officer_name_table_wa <- officer_names %>%
  filter(st == "WA") %>%
  group_by(lname, fname) %>%
  count() %>%
  arrange(desc(n))


# Final datasets ----

mpv_clean <- mpv_draft 

mpv_clean_wa <- mpv_clean %>% filter(st == "WA")



