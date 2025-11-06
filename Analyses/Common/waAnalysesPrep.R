#----------------------------------------------------------------- 
# This child script creates the common datafiles and variables used in
# WA since 2015 analyses.
# It relies on the params set in the yaml header of the parent .Rmd files 
# (in case different time-frames are selected)
#-----------------------------------------------------------------

# Select homicides by police and use age from FE

homicides <- wa_clean_2015 %>% 
  filter(mod != "Suicide" & !grepl("Medical|Ketamine", cod)) %>%
  
  mutate(age = ifelse(age==999, NA_real_, age)) %>%
  arrange(date)

all.cases <- nrow(wa_clean_2015)
all.homicides <- nrow(homicides)


# Dates and years ----

## Remember for trend plotting later -- remove partial months
start_yr <- params$startyr
start_mo <- 1
start_date <- as.Date(paste0(start_yr, "-", start_mo, "-01"))

curr_mo <- month(Sys.Date())
curr_yr <- year(Sys.Date())

## Calendar years (all)
cal.yrs <- start_yr:curr_yr
num.cal.yrs <- length(cal.yrs)

## Legislative years (all)
leg.yrs <- unique(homicides$leg.year)
num.leg.yrs <- length(leg.yrs)

# Most recent case info ----
# Assumes homicide df is sorted by date

last.case <- nrow(homicides)
last.case.info <- homicides[last.case,] 

last.date <- last.case.info$date
last.name <- ifelse(last.case.info$name == "Unknown", 
                    "(Name not released)",        
                    last.case.info$name)
last.age <- ifelse(is.na(last.case.info$age), 
                   "(age not released)",
                   paste(last.case.info$age, "years old"))
last.agency <- last.case.info$agency

last.cod <- last.case.info$cod



last.url <- last.case.info$url_click

# Summary info ----
tot.by.yr <- table(homicides$year)
tot.this.yr <- tot.by.yr[[length(tot.by.yr)]]
tot.yr.is <- max(homicides$year)

num.suffix <- case_when(tot.this.yr == 1 ~ "st",
                        tot.this.yr == 2 ~ "nd",
                        tot.this.yr == 3 ~ "rd", 
                        TRUE ~ "th")

# Indices for plotting by time ----

## starts with first complete month
## ends with last complete month
## we use "mo" for numeric month and "mon" for alpha month abb

numyrs <- last_complete_yr - start_yr + 1

## set up first year
mon <- month.abb[start_mo:12]
year <- rep(start_yr, length(mon))

## generate middle months/years
for(i in 1:(numyrs-1)) {
  #print(i)
  thisyr = start_yr+i
  #print(thisyr)
  year <- c(year, rep(thisyr, 12))
  mon <- c(mon, month.abb)
}

## tack on last months/year for current year if
## last complete month is not December (of prev yr)
if (last_complete_mo != 12) {
  year <- c(year, rep(curr_yr, last_complete_mo))
  mon <- c(mon, month.abb[1:last_complete_mo])
}

## Sequential complete mo/yr index for date plotting

index <- data.frame(year = year,
                    mon = mon,
                    mon.yr = paste0(mon, ".", year),
                    index.date = lubridate::ymd(
                      paste0(year, "-", mon, "-01")
                    ))

# Significant dates ----

## Legislation
date.940.pass <- lubridate::ymd("2018-11-06")
date.940.WAC <- lubridate::ymd("2020-01-06")
date.firstbillpass <- lubridate::ymd("2021-03-09")
date.firstbillsign <- lubridate::ymd("2021-04-20")
date.lastbillsign <- lubridate::ymd("2021-04-26")
date.2021.law <- lubridate::ymd("2021-07-25")
date.2022.law <- lubridate::ymd("2022-03-17")
date.2023.law <- lubridate::ymd("2023-05-04")
date.2024.rollback <- lubridate::ymd("2024-06-05")

## Prosecution
date.sarey.charged <- lubridate::ymd("2020-08-20")
date.ellis.ago <- lubridate::ymd("2020-06-23")
date.ellis.charged <- lubridate::ymd("2021-05-27")
date.ellis.acquit <- lubridate::ymd("2023-12-21")
date.nelson.convicted <- lubridate::ymd("2024-06-27")

