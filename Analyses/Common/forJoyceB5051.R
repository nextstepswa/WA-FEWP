load("~/GitHub/fewapo/data-outputs/CleanMPVData.rda")

officer.names <- mpv_clean_wa %>%
  filter(date > as.Date("2021-07-01")) %>%
  arrange(date) %>%
  select(mpvID, feID, wapoID, name, date, agency, officer.names, url_info)
write_excel_csv(officer.names, 
                file = here::here("data-outputs", "officer-names.csv"))


load("~/GitHub/fewapo/data-outputs/WA940.rda")

merged.cases <- merged_data %>%
  filter(date > as.Date("2021-07-01")) %>%
  filter(homicide == 1) %>%
  arrange(date) %>%
  select(feID, wapoID, name, date, agency, officer_names, url_info, officer_url)
write_excel_csv(merged.cases, 
                file = here::here("data-outputs", "fewapo-cases-5051.csv"))
