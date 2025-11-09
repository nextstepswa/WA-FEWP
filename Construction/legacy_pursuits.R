# Construction of the merged legacy pursuit data

## A bit complicated as the two legacy periods must be merged separately
## * 2013-2021, includes MPV, WaPo and FE (IN for 2021 only)
## * 2022-2025, includes MPV, IN and WAlocal (thru 10/31/2025)

# ID xwalk info (in case not already in environment)
load(here::here("Data", "Clean", "ID_xwalk_from_2022.rda"))
inIDs2021 <- read.csv(here::here("Data", "Raw", "FE_IN_xwalk_2021.csv"))

# Legacy coded pursuit data
mpv_cp <- readxl::read_xlsx(here::here("Data","Clean", "Pursuits", "MPV.coded.pursuits.xlsx")) %>%
  select(mpvID:pursuit.incinum, url_addl, notes)
fe_cp <- readxl::read_xlsx(here::here("Data", "Clean", "Pursuits", "FE.coded.pursuits.xlsx")) %>%
  select(feID:pursuit.incinum, url_addl, notes)
walocal_cp <- readxl::read_xlsx(here::here("Data", "Clean", "Pursuits", "WAlocal.coded.pursuits.xlsx")) %>%
  select(waID:pursuit.incinum, url_addl, notes) %>%
  left_join(id_xwalk_from_2022) %>%
  select(contains("ID"), name:notes)

# ID xwalk for MPV-FE all years
mpv_feIDs <- mpv_cp %>% select(contains("ID")) # for FE MPV merge


# Full join FE and MPV:  will have incidents from 2013-2025
pursuits <- fe_cp %>%
  left_join(mpv_feIDs, by="feID") %>%
  left_join(inIDs2021) %>%
  
  full_join(mpv_cp,
            by = c("feID", "mpvID", "wapoID"),
            suffix = c(".fe", ".mpv")) %>%
  
  # use MPV variables when they exist
  mutate(name = if_else(!is.na(name.mpv), name.mpv, name.fe),
         date = if_else(!is.na(date.mpv), date.mpv, date.fe),
         cod = if_else(!is.na(cod.mpv), cod.mpv, cod.fe),
         pursuit.type = if_else(!is.na(pursuit.type.mpv), pursuit.type.mpv, pursuit.type.fe),
         pursuit.victim = if_else(!is.na(pursuit.victim.mpv), pursuit.victim.mpv, pursuit.victim.fe),
         pursuit.injury = if_else(!is.na(pursuit.injury.mpv), pursuit.injury.mpv, pursuit.injury.fe),
         pursuit.incinum = if_else(!is.na(pursuit.incinum.mpv), pursuit.incinum.mpv, pursuit.incinum.fe),
         url_addl = if_else(!is.na(url_addl.mpv), url_addl.mpv, url_addl.fe)
  ) %>%
  select(contains("ID"), name, date, cod, mod, pursuit.type:url_addl, contains("notes")) 

# Split into the 2 time periods

# 2013-21 -- needs the inIDs for 2021
pursuits.pre2022 <-  pursuits %>% filter(date < as.Date("2022-01-01")) %>%
  left_join(inIDs2021) %>%
  
  mutate(mod = case_when(
    is.na(mod) & cod=="Gunshot" ~ "Homicide", 
    is.na(mod) & cod=="Vehicle" ~ "Vehicle accident",
    TRUE ~ mod)) %>%
  select(contains("ID"), cod, mod, name:notes.fe)

# 2022-2025 - merge in WAlocal, will have a couple of backfilled cases from that
# No FE cases here
pursuits.post2021 <- pursuits %>% 
  filter(lubridate::year(date) > 2021) %>%
  left_join(id_xwalk_from_2022) %>%

  full_join(walocal_cp,
            by = c("mpvID", "wapoID"),
            suffix = c(".mf", ".wa")) %>%
  select(contains("ID"), contains(".mf"), contains(".wa"), contains("notes")) %>%
  
  # tag MPV cases ID'd via WAlocal -- these were missed using the scripted pursuit indicators
  mutate(notes.mpv = if_else(!is.na(mpvID) & is.na(name.mf), 
                             "Found by comparison to WAlocal coded pursuits", 
                             notes.mpv),
         
         # use WAlocal variables when they exist
         waID = if_else(!is.na(waID.wa), waID.wa, waID.mf),
         inID = if_else(!is.na(inID.wa), inID.wa, inID.mf),
         name = if_else(!is.na(name.wa), name.wa, name.mf),
         date = if_else(!is.na(date.wa), date.wa, date.mf),
         cod = if_else(!is.na(cod.wa), cod.wa, cod.mf),
         mod = if_else(!is.na(mod.wa), mod.wa, mod.mf),
         pursuit.type = if_else(!is.na(pursuit.type.wa), pursuit.type.wa, pursuit.type.mf),
         pursuit.victim = if_else(!is.na(pursuit.victim.wa), pursuit.victim.wa, pursuit.victim.mf),
         pursuit.injury = if_else(!is.na(pursuit.injury.wa), pursuit.injury.wa, pursuit.injury.mf),
         pursuit.incinum = if_else(!is.na(pursuit.incinum.wa), pursuit.incinum.wa, pursuit.incinum.mf),
         url_addl = if_else(!is.na(url_addl.wa), url_addl.wa, url_addl.mf)
  ) %>%
  select(contains("ID"), name:url_addl, contains("notes")) %>%
  
  # All post FE, so no FE IDs.  Will become missing when the two period dfs are added together
  select(-contains("feID"))

legacy.pursuits <- bind_rows(pursuits.pre2022, pursuits.post2021) %>%
  rename(notes.wa = notes) %>%
  select(contains("ID"), name, date, cod, mod, pursuit.type:url_addl, contains("notes")) %>%
  mutate(legacy.pursuit=1) %>%
  arrange(date)

legacy.pursuits.2015 <- legacy.pursuits %>% filter(lubridate::year(date) > 2014)

save(list = c("legacy.pursuits", "legacy.pursuits.2015"),
     file = here::here("Data", "Clean", "Pursuits", "legacy_pursuits.rda"))

# # tests
# legacy.pursuits %>% count(feID) %>% filter(n==2)
# legacy.pursuits %>% count(wapoID) %>% filter(n==2)
# legacy.pursuits %>% count(mpvID) %>% filter(n==2)
# legacy.pursuits %>% count(waID) %>% filter(n==2)
# legacy.pursuits %>% count(inID) %>% filter(n==2)

