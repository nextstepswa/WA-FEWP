# All files named _raw when read in
# Modified files are named _temp for output to fixes_precleaning

#_____________________________________________________________
# MPV ----

## Using Airtable from mappingpoliceviolence.org
## with code drafted by Claude

## Download data from an Airtable base
## Base:  appzVzSeINK1S3EVR
## Table: tblxearKzw8W7ViN8
## Shared view: shroOenW19l1m3w0H
##
## METHOD A (recommended): official Airtable API. Needs a personal access token
##   with scope data.records:read and access to this base (you must have access
##   to the base, not just the shared link).
## METHOD B: scrape the public shared-view link. No token needed, but it uses
##   undocumented endpoints and may break if Airtable changes things.

library(httr2)
library(jsonlite)
#library(dplyr)
library(purrr)

## scrape tokens and functions ----

base_id  <- "appzVzSeINK1S3EVR"
table_id <- "tblxearKzw8W7ViN8"
share_id <- "shroOenW19l1m3w0H"
out_file <- "airtable_data.csv"

# Turn list-columns (multi-selects, links, attachments) into plain text
flatten_cell <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  if (is.list(x)) {
    x <- map_chr(x, function(i) {
      if (is.list(i)) {
        nm <- i$foreignRowDisplayName %||% i$name %||% i$url %||% toJSON(i, auto_unbox = TRUE)
        as.character(nm)
      } else as.character(i)
    })
  }
  trimws(paste(as.character(x), collapse = "; "))
  #paste(as.character(x), collapse = "; ")
}

# Convert cols to appropriate formats
convert_types <- function(df) {
  num_pattern  <- "^\\s*-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?\\s*$"
  date_pattern <- "^\\d{4}-\\d{2}-\\d{2}([T ]\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?Z?)?$"
  
  df |> mutate(across(where(is.character), function(col) {
    vals <- trimws(col[!is.na(col) & trimws(col) != ""])
    if (length(vals) == 0) return(col)
    
    # Numeric: every non-empty value looks like a number
    # (skips things like zip codes or IDs with leading zeros, e.g. "00123")
    if (all(grepl(num_pattern, vals)) && !any(grepl("^-?0[0-9]", vals))) {
      return(as.numeric(col))
    }
    
    # Date / datetime: every non-empty value looks like an ISO date or datetime
    if (all(grepl(date_pattern, vals))) {
      cleaned <- sub("Z$", "", sub("T", " ", trimws(col)))
      return(parse_date_time(cleaned, orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                             tz = "UTC", quiet = TRUE))
    }
    
    col
  }))
}

## METHOD A: -----------------------------------------------------------------
##  official API, if dataset is shared with you

download_api <- function(token = Sys.getenv("AIRTABLE_TOKEN")) {
  if (token == "") stop("Set AIRTABLE_TOKEN, e.g. Sys.setenv(AIRTABLE_TOKEN = 'pat...')")
  
  records <- list()
  offset  <- NULL
  repeat {
    resp <- request(paste0("https://api.airtable.com/v0/", base_id, "/", table_id)) |>
      req_auth_bearer_token(token) |>
      req_url_query(pageSize = 100, offset = offset) |>
      req_retry(max_tries = 5) |>
      req_perform() |>
      resp_body_json()
    
    records <- c(records, resp$records)
    offset  <- resp$offset
    if (is.null(offset)) break
    Sys.sleep(0.25)  # stay under 5 requests/sec
  }
  
  map_dfr(records, function(r) {
    row <- map(r$fields, flatten_cell)
    c(list(record_id = r$id, created_time = r$createdTime), row) |> as_tibble()
  })
}

## METHOD B: -----------------------------------------------------------------
# from public shared view (no token)

download_shared <- function() {
  page_url <- paste0("https://airtable.com/", base_id, "/", share_id, "/", table_id)
  ua <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/120 Safari/537.36"
  
  html <- request(page_url) |>
    req_user_agent(ua) |>
    req_perform() |>
    resp_body_string()
  
  # The page embeds the signed URL used to load the shared view's data
  m <- regmatches(html, regexpr('urlWithParams":\\s*"[^"]+"', html))
  if (length(m) == 0) m <- regmatches(html, regexpr('urlWithParams:\\s*"[^"]+"', html))
  if (length(m) == 0) stop("Couldn't find the data URL in the page. Use Method A instead.")
  
  path <- sub('^urlWithParams":?\\s*"|^urlWithParams:\\s*"', "", m)
  path <- sub('"$', "", path)
  path <- gsub("\\\\u002F", "/", path, ignore.case = TRUE)
  path <- gsub("\\\\/", "/", path)
  path <- gsub("\\\\u0026", "&", path, ignore.case = TRUE)
  
  dat <- request(paste0("https://airtable.com", path)) |>
    req_user_agent(ua) |>
    req_headers(
      `x-airtable-application-id` = base_id,
      `x-requested-with` = "XMLHttpRequest",
      `x-time-zone` = "UTC",
      `x-user-locale` = "en"
    ) |>
    req_perform() |>
    resp_body_json()
  
  tbl  <- dat$data$table
  cols <- tbl$columns
  
  # Map select-option IDs to their labels
  choice_lookup <- purrr::map(cols, function(cl) {
    ch <- cl$typeOptions$choices
    if (is.null(ch)) return(NULL)
    setNames(map_chr(ch, "name"), names(ch))
  }) |> setNames(map_chr(cols, "id"))
  
  col_names <- setNames(map_chr(cols, "name"), map_chr(cols, "id"))
  
  map_dfr(tbl$rows, function(r) {
    vals <- imap(r$cellValuesByColumnId, function(v, cid) {
      lk <- choice_lookup[[cid]]
      if (!is.null(lk)) {
        ids <- unlist(v)
        v <- if (all(ids %in% names(lk))) unname(lk[ids]) else v
      }
      flatten_cell(v)
    })
    names(vals) <- col_names[names(vals)]
    c(list(record_id = r$id), vals) |> as_tibble()
  })
}

## Run ----------------------------------------------------------------------
## Only run if local file is > 1 day old
## use API if a token is set, otherwise fall back to the shared view

if(difftime(Sys.time(), file.info(here::here("Data", "Raw", "MPVDatasetDownload.xlsx"))$atime, units="days") > 1) {

  message("Scraping MPV")
  df <- if (Sys.getenv("AIRTABLE_TOKEN") != "") download_api() else download_shared()
  
  ## Clean ----------------------------------------------------------------------
  ## col formats and standardized IDs
  
  mpv_raw <- df %>%
    convert_types() %>%
    mutate(zip = as.numeric(zip)) %>%
    mutate(mpvID = mpv_id, 
           feID = fe_id,
           wapoID = wapo_id
    )
  
  ## Save ----------------------------------------------------------------------
  ## raw data into excel file
  
  out_file <- here::here("Data", "Raw", "MPVDatasetDownload.xlsx")
  
  writexl::write_xlsx(mpv_raw, out_file, na = "")
  message("Saved ", nrow(mpv_raw), " rows and ", ncol(mpv_raw), " variables to ", out_file)
  
  
} else {
  message("Using existing MPV file")
  
  mpv_raw <- read_excel(here::here("Data", "Raw", "MPVDatasetDownload.xlsx"),
                        guess_max = 10000)
}




#_____________________________________________________________
# WA local ----

# Cleaned data (1/1/2022- with backfills as found):
## Preserves the original coverage of FE: 
## * fatalities from all encounters, not just gunshots.  
## * Vehicular pursuits are included.
## Update procedure:
## * Manually with google searches/IncarcerNation
## * By script with MPV merge (see MakeData.R file)

## Variable names in raw file have been updated from FE original to final harmonized

message("Reading in clean local data for WA from WA-FEWP project")

walocal_raw <- readxl::read_xlsx(here::here("Data", "Raw", "wa_local.xlsx"))


#_____________________________________________________________
# FE ----

## Cleaned legacy data from the previous repo (fewapo)
## This is essentially a frozen dataset now, though errors may be fixed in the future

message("Reading in clean legacy FE data from WA-FEWP project")

load(here::here("Data", "Clean", "FE_clean.rda"))

  
## If the archived dataset needs to be fixed:
## Fix it, read the clean data back into this repo
## Strip out the 90000+ IDs as these are from our local data collection, and are
## handled separately (above)

# fe_clean <- read.csv(here::here("Data", "Clean", "FEWaPo", "FE_clean.csv")) %>%
#   filter(feID < 90000) %>%
#   mutate(date = as.Date(date)) %>%
#   select(-X)
# save(fe_clean, file = here::here("Data", "Clean", "FE_clean.rda"))

#_____________________________________________________________
# WaPo ----

## Cleaned legacy data rom the previous repo (fewapo)
## This is essentially a frozen dataset now, though errors may be fixed in the future

message("Reading in clean legacy WaPo data from WA-FEWP project")

load(here::here("Data", "Clean", "wapo_clean.rda"))

## If the archived dataset needs to be fixed:
## Fix it, read the clean data back into this repo
# wapo_clean <- read.csv(here::here("Data", "Clean", "FEWaPo", "WaPo_clean.csv")) %>%
#   mutate(date = as.Date(date)) %>%
#   select(-X)
# save(wapo_clean, file = here::here("Data", "Clean", "wapo_clean.rda"))

#_____________________________________________________________
# Leg Districts ----

message(paste("Loading prebuilt WA legislative district data \n",
              "*Updates only needed for redistricting and elections*"))
load(here::here("Data", "Clean", "LegInfo_Clean.rda"))

#_____________________________________________________________
# IncarcerNation-feID crosswalk ----

## For 2021 only - this was the only year of overlap

message("Loading IncarcerNation-FE ID crosswalk info for 2021")

inIDs2021 <- read.csv(here::here("Data", "Raw", "FE_IN_xwalk_2021.csv"))

