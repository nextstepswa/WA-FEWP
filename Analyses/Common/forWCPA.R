comp <- homicides %>%
  filter(date > lubridate::ymd("2019-01-20")) %>%
  mutate(yr = if_else(date < lubridate::ymd("2020-01-20"), 1, 2),
         cod = case_when(cod.fe == "Asphyxiated/Restrained" ~ 1,
                         cod.fe == "Gunshot"  ~ 1,
                         cod.fe == "Tasered" ~ 1,
                         TRUE ~ 2))

with(comp, table(cod, yr))
