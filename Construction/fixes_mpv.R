# Fixes found during match cleaning and sent to SS
# Fixed cases have been deleted, unfixed remain

# To facilitate cleaning
# Check periodically to see if they've modified the file structure
mpv_temp <- mpv_raw %>%  
  mutate(mpvID = `MPV ID`, 
         feID = as.numeric(`Fatal Encounters ID`),
         wapoID = `WaPo ID (If included in WaPo database)`,
         name = `Victim's name`,
         date = as.Date(`Date of Incident (month/day/year)`)
  ) %>%
  select(-`...53`) %>%
  select(mpvID:date, `Victim's age`:`Prosecutor Source Link`) #omits KBP, col of 1's


## Odd case, if this is the right name/agency/ORI/url then
## everything else is wrong -- maybe a botched merge? 
## really needs to have all the geocoded info changed to NA
mpv_temp$State[mpv_temp$mpvID == 11738] <- "NY"
mpv_temp$City[mpv_temp$mpvID == 11738] <- "Clifton Park"
mpv_temp$`Street Address of Incident`[mpv_temp$mpvID == 11738] <- "100 Foxwood Dr"
mpv_temp$Zipcode[mpv_temp$mpvID == 11738] <- 12065
mpv_temp$Latitude[mpv_temp$mpvID == 11738] <- 42.8094066
mpv_temp$Longitude[mpv_temp$mpvID == 11738] <- -73.7983222

## Names ----
mpv_temp$name[mpv_temp$mpvID == 347] <- "Todd Christopher Jones"
mpv_temp$name[mpv_temp$mpvID == 1075] <- "Anthony Darnell King"
mpv_temp$name[mpv_temp$mpvID == 277] <- "Stacey Stout"
mpv_temp$name[mpv_temp$mpvID == 14428] <- "Daniel Judson Jolliffe" #suicide, reported to MPV

## Dates ----
mpv_temp$date[mpv_temp$mpvID == 8397] <- as.Date("2020-08-26")
mpv_temp$date[mpv_temp$mpvID == 2553] <- as.Date("2015-05-20")
mpv_temp$date[mpv_temp$mpvID == 14744] <- as.Date("2025-05-07") # mpv has 2024

## Gender ----
mpv_temp$`Victim's gender`[mpv_temp$mpvID %in% 
                             c(646, 1902, 1584, 1973,
                               3141, 3800, 8615,
                               10733, 10130, 
                               10280)] <- "Male"

mpv_temp$`Victim's gender`[mpv_temp$mpvID == 5092] <- "Nonbinary"

## Age ----
mpv_temp$`Victim's age`[mpv_temp$mpvID==10031] <- "39"
mpv_temp$`Victim's age`[mpv_temp$mpvID==10877] <- "43"
mpv_temp$`Victim's age`[mpv_temp$mpvID==11098] <- "34"
mpv_temp$`Victim's age`[mpv_temp$mpvID==11120] <- "50"
mpv_temp$`Victim's age`[mpv_temp$mpvID==11273] <- "39" 
mpv_temp$`Victim's age`[mpv_temp$mpvID==11293] <- "26" 
mpv_temp$`Victim's age`[mpv_temp$mpvID==11546] <- "27" # unclear, almost no info online
mpv_temp$`Victim's age`[mpv_temp$mpvID==12355] <- "43"
mpv_temp$`Victim's age`[mpv_temp$mpvID==12993] <- "38"
mpv_temp$`Victim's age`[mpv_temp$mpvID==13050] <- "31" # every article
mpv_temp$`Victim's age`[mpv_temp$mpvID==13439] <- "57"
mpv_temp$`Victim's age`[mpv_temp$mpvID==13985] <- "30" # obit DOB


## City ----
mpv_temp$City[mpv_temp$mpvID==8028] <- "Seattle"
mpv_temp$City[mpv_temp$mpvID==11084] <- "Brinnon"
mpv_temp$City[mpv_temp$mpvID==10743] <- "Zillah"
mpv_temp$City[mpv_temp$mpvID==10877] <- "Burien"
mpv_temp$City[mpv_temp$mpvID==11100] <- "Sunnydale"
mpv_temp$City[mpv_temp$mpvID==11254] <- "Fairwood"
mpv_temp$City[mpv_temp$mpvID==11445] <- "Longview"
mpv_temp$City[mpv_temp$mpvID==11548] <- "Tacoma"
mpv_temp$City[mpv_temp$mpvID==12587] <- "Longview"
mpv_temp$City[mpv_temp$mpvID==12920] <- "Tukwila"
mpv_temp$City[mpv_temp$mpvID==14302] <- "Sedro-Woolley"
mpv_temp$City[mpv_temp$mpvID==14422] <- "Spokane Valley"
mpv_temp$City[mpv_temp$mpvID==14744] <- "Ridgefield"

## County ----
mpv_temp$County[mpv_temp$mpvID==8028] <- "King"
mpv_temp$County[mpv_temp$mpvID==12372] <- "Kitsap"
mpv_temp$County[mpv_temp$mpvID==12858] <- "Spokane"
mpv_temp$County[mpv_temp$mpvID==14302] <- "Whatcom"

## FE ID mismatches ----
## we delete dupes, or reassign if match is wrong
mpv_temp$feID[mpv_temp$mpvID == 1112] <- 15595   
mpv_temp$feID[mpv_temp$mpvID == 9808] <- 29769
mpv_temp$feID[mpv_temp$mpvID == 9078] <- 29967
mpv_temp$feID[mpv_temp$mpvID == 9853] == 31440 #(MPV: Hebert; FE: UNK, updated in preclean)

## Officer names ---
mpv_temp$`Names of Officers Involved`[mpv_temp$mpvID == 6913] <- "Deputy Joseph Wallace"


