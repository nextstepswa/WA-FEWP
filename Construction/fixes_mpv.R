# Fixes found during match cleaning and sent to MPV
# Fixed cases have been deleted, unfixed remain
# Check periodically using fixes_mpv_recheck.R

# Note a couple of mpvIDs have changed with transfer to mappingpoliceviolence.org data:
## "Branden Bouathet Chindavong" mpvid now 16760, was 15213
## "Alrico Ray Coleman" mpvid now 16722, was 15187

mpv_temp <- mpv_raw 

## Odd case, if this is the right name/agency/ORI/url then
## everything else is wrong -- maybe a botched merge? 
## really needs to have all the geocoded info changed to NA
mpv_temp$state[mpv_temp$mpvID == 11738] <- "New York"
mpv_temp$city[mpv_temp$mpvID == 11738] <- "Clifton Park"
mpv_temp$street_address[mpv_temp$mpvID == 11738] <- "100 Foxwood Dr"
mpv_temp$zip[mpv_temp$mpvID == 11738] <- 12065
mpv_temp$latitude[mpv_temp$mpvID == 11738] <- 42.8094066
mpv_temp$longitude[mpv_temp$mpvID == 11738] <- -73.7983222

## Names ----
mpv_temp$name[mpv_temp$mpvID == 277] <- "Stacey Stout" # not Stacy
mpv_temp$name[mpv_temp$mpvID == 347] <- "Todd Christopher Jones"
mpv_temp$name[mpv_temp$mpvID == 1075] <- "Anthony Darnell King"
mpv_temp$name[mpv_temp$mpvID == 8285] <- "Aaron Olsvik" # KCME file
mpv_temp$name[mpv_temp$mpvID == 9736] <- "Alexander Whittal" # KCME file

mpv_temp$name[mpv_temp$mpvID == 10228] <- "William Stephens"
mpv_temp$name[mpv_temp$mpvID == 10901] <- "Vince Torres"
mpv_temp$name[mpv_temp$mpvID == 11145] <- "Jeffrey Webley"
mpv_temp$name[mpv_temp$mpvID == 14260] <- "Jeffrey Kiner"
mpv_temp$name[mpv_temp$mpvID == 14428] <- "Daniel Judson Jolliffe" #suicide, reported to MPV
#mpv_temp$name[mpv_temp$mpvID == 14890] <- "Jack Paleli"
mpv_temp$name[mpv_temp$mpvID == 15032] <- "Michael Harris"
#mpv_temp$name[mpv_temp$mpvID == 15187] <- "Alrico Ray Coleman"
mpv_temp$name[mpv_temp$mpvID == 16760] <- "Branden Bouathet Chindavong" #mpvid now 16760, was 15213
mpv_temp$name[mpv_temp$mpvID == 17419] <- "Wayne Branham"

## Dates ----
mpv_temp$date[mpv_temp$mpvID == 8397] <- as.Date("2020-08-26")
mpv_temp$date[mpv_temp$mpvID == 2553] <- as.Date("2015-05-20")
mpv_temp$date[mpv_temp$mpvID == 11145] <- as.Date("2022-11-13")
mpv_temp$date[mpv_temp$mpvID == 13460] <- as.Date("2023-12-12")
mpv_temp$date[mpv_temp$mpvID == 14744] <- as.Date("2025-05-07") # mpv has 2024
mpv_temp$date[mpv_temp$mpvID == 15581] <- as.Date("2025-05-07") # midnight, could be 5/8/2025
mpv_temp$date[mpv_temp$mpvID == 16606] <- as.Date("2025-12-27") # midnight, could be 12/28/2025
mpv_temp$date[mpv_temp$mpvID == 17124] <- as.Date("2026-04-26") 


## Gender ----
mpv_temp$gender[mpv_temp$mpvID %in% c(646, 1902, 14999)] <- "Male"

## Age ----

mpv_temp$age[mpv_temp$mpvID==4586] <- 51 # KCME file
mpv_temp$age[mpv_temp$mpvID==7531] <- 45 # https://digitalarchives.wa.gov/Record/View/3A4F19884D27DB9CEB3CB30F609CC00E
mpv_temp$age[mpv_temp$mpvID==8100] <- 57 # KCME file
mpv_temp$age[mpv_temp$mpvID == 8285] <- 23 # KCME file
mpv_temp$age[mpv_temp$mpvID == 9736] <- 28 # KCME file

mpv_temp$age[mpv_temp$mpvID==10031] <- 39
mpv_temp$age[mpv_temp$mpvID==10877] <- 43
mpv_temp$age[mpv_temp$mpvID==11098] <- 34
mpv_temp$age[mpv_temp$mpvID==11120] <- 50
mpv_temp$age[mpv_temp$mpvID==11145] <- 34

mpv_temp$age[mpv_temp$mpvID==11273] <- 39 
mpv_temp$age[mpv_temp$mpvID==11293] <- 26 
mpv_temp$age[mpv_temp$mpvID==11546] <- 27 # unclear, almost no info online
mpv_temp$age[mpv_temp$mpvID==12355] <- 43
mpv_temp$age[mpv_temp$mpvID==12993] <- 38
mpv_temp$age[mpv_temp$mpvID==13050] <- 31 # every article
mpv_temp$age[mpv_temp$mpvID==13439] <- 57
mpv_temp$age[mpv_temp$mpvID==13985] <- 30 # obit DOB
mpv_temp$age[mpv_temp$mpvID==14890] <- 46 # coroner info
mpv_temp$age[mpv_temp$mpvID==14993] <- 39 # OII
mpv_temp$age[mpv_temp$mpvID==14260] <- 57
mpv_temp$age[mpv_temp$mpvID==15087] <- 32 # Obit
mpv_temp$age[mpv_temp$mpvID==15032] <- 53
mpv_temp$age[mpv_temp$mpvID==15172] <- 30 # Article

## Race ----

mpv_temp$race[mpv_temp$mpvID == 8285] <- "White" # KCME file
mpv_temp$race[mpv_temp$mpvID == 9736] <- "White" # KCME file
mpv_temp$race[mpv_temp$mpvID == 14260] <- "White" # may have been a pic in the Statesman Ex. article now 404


## City ----
mpv_temp$city[mpv_temp$mpvID==11084] <- "Brinnon"
mpv_temp$city[mpv_temp$mpvID==10743] <- "Zillah"
mpv_temp$city[mpv_temp$mpvID==10877] <- "Burien"
mpv_temp$city[mpv_temp$mpvID==11100] <- "Sunnydale"
mpv_temp$city[mpv_temp$mpvID==11254] <- "Fairwood"
mpv_temp$city[mpv_temp$mpvID==11445] <- "Longview"
mpv_temp$city[mpv_temp$mpvID==11548] <- "Tacoma"
mpv_temp$city[mpv_temp$mpvID==12587] <- "Longview"
mpv_temp$city[mpv_temp$mpvID==12920] <- "Tukwila"
mpv_temp$city[mpv_temp$mpvID==14302] <- "Sedro-Woolley"
mpv_temp$city[mpv_temp$mpvID==14422] <- "Spokane Valley"
mpv_temp$city[mpv_temp$mpvID==14744] <- "Ridgefield"
mpv_temp$city[mpv_temp$mpvID==15620] <- "Sedro-Woolley"
mpv_temp$city[mpv_temp$mpvID==15890] <- "Yelm"
mpv_temp$city[mpv_temp$mpvID==16607] <- "South Hill"
mpv_temp$city[mpv_temp$mpvID==16937] <- "Everett"

## County ----
mpv_temp$county[mpv_temp$mpvID==12372] <- "Kitsap"
mpv_temp$county[mpv_temp$mpvID==12858] <- "Spokane"
mpv_temp$county[mpv_temp$mpvID==14302] <- "Whatcom"

## FE ID mismatches ----
## we delete dupes, or reassign if match is wrong
mpv_temp$feID[mpv_temp$mpvID == 9808] <- 29769
mpv_temp$feID[mpv_temp$mpvID == 9078] <- 29967
#mpv_temp$feID[mpv_temp$mpvID == 9853] == 31440 #(MPV: Hebert; FE: UNK, updated in preclean)

## Officer names ---
mpv_temp$officer_names[mpv_temp$mpvID == 6913] <- "Deputy Joseph Wallace"


