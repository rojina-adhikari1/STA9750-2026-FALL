# =========================================================================
# CUNY-Nauts — shared data script
# Every SQ file starts with: source("00_data.R")
# Creates: IPEDS (2015–2024, all US institutions), CUNY_IPEDS, CUNY_NYOD
# =========================================================================

library(dplyr)
library(tidyverse)
library(glue)
library(janitor)
options(timeout = 600)   # allow up to 10 minutes per file (default is 60 seconds)

dir.create("data/ipeds", recursive = TRUE, showWarnings = FALSE)
dir.create("data/nyod",  recursive = TRUE, showWarnings = FALSE)

# Manually downloaded files (committed in data_manual/) -> data/ipeds/
file.copy(list.files("data_manual", pattern = "\\.zip$", full.names = TRUE),
          "data/ipeds", overwrite = FALSE)

# -------------------------------------------------------------------------
# SOURCE 1: IPEDS Fall Enrollment (EF A) + Institutional Directory (HD)
# Download logic adapted from STA 9750 Mini-Project #01 (M. Weylandt)
# -------------------------------------------------------------------------
get_ipeds_enrollment <- function(start_year = 2015, end_year = 2024){
  
  data_dir <- file.path("data", "ipeds")
  
  # Download once; on failure delete the partial file and stop with a clear message
  fetch <- function(url, dest){
    if(!file.exists(dest)){
      message(glue("Downloading {basename(dest)} ..."))
      ok <- tryCatch({
        download.file(url, destfile = dest, quiet = TRUE, mode = "wb")
        TRUE
      }, error = function(e) FALSE)
      if(!ok || file.size(dest) == 0){
        if(file.exists(dest)) file.remove(dest)
        stop(glue("Could not download {basename(dest)}. Download it manually ",
                  "from the IPEDS Data Files page and save it in data_manual/ ",
                  "as {basename(dest)}"))
      }
    }
  }
  
  YEARS <- seq(start_year, end_year)
  
  EFA_ALL <- map(YEARS, function(yy){
    ef_file <- file.path(data_dir, glue("ef{yy}a.csv.zip"))
    fetch(glue("https://nces.ed.gov/ipeds/datacenter/data/EF{yy}A.zip"), ef_file)
    
    zip_files <- unzip(ef_file, list = TRUE)$Name
    csv_name  <- if (any(grepl("_rv", zip_files))) zip_files[grepl("_rv", zip_files)][1] else zip_files[1]
    read_csv(unz(ef_file, csv_name), show_col_types = FALSE) |>
      mutate(year = yy,
             enrollment_m_aian = EFAIANM, enrollment_f_aian = EFAIANW,
             enrollment_m_asia = EFASIAM, enrollment_f_asia = EFASIAW,
             enrollment_m_bkaa = EFBKAAM, enrollment_f_bkaa = EFBKAAW,
             enrollment_m_hisp = EFHISPM, enrollment_f_hisp = EFHISPW,
             enrollment_m_nhpi = EFNHPIM, enrollment_f_nhpi = EFNHPIW,
             enrollment_m_whit = EFWHITM, enrollment_f_whit = EFWHITW,
             enrollment_m_2mor = EF2MORM, enrollment_f_2mor = EF2MORW,
             enrollment_m_unkn = EFUNKNM, enrollment_f_unkn = EFUNKNW,
             enrollment_m_nral = EFNRALM, enrollment_f_nral = EFNRALW) |>
      filter((EFALEVEL %in% c(2, 12)) | (LINE %in% c(1, 15))) |>
      mutate(level = case_when(
        EFALEVEL == 2  ~ "all undergrad",
        EFALEVEL == 12 ~ "all graduate",
        LINE %in% c(1, 15) ~ "first year undergrad")) |>
      select(institution_id = UNITID, year, level, starts_with("enrollment_")) |>
      group_by(institution_id, year, level) |>
      summarize(across(starts_with("enrollment_"), sum), .groups = "drop")
  }) |> bind_rows()
  
  DESC_ALL <- map(YEARS, function(yy){
    hd_file <- file.path(data_dir, glue("hd{yy}.csv.zip"))
    fetch(glue("https://nces.ed.gov/ipeds/datacenter/data/HD{yy}.zip"), hd_file)
    
    suppressWarnings(
      read_csv(hd_file, show_col_types = FALSE,
               locale = locale(encoding = if_else(yy >= 2024, "utf-8", "windows-1252"))) |>
        select(institution_id = UNITID,
               institution_name = INSTNM,
               state = STABBR) |>
        mutate(year = yy)
    )
  }) |> bind_rows()
  
  inner_join(EFA_ALL, DESC_ALL, join_by(institution_id, year))
}

IPEDS <- get_ipeds_enrollment(start_year = 2015, end_year = 2024)

# CUNY subset (excludes the System Office, which has no students).
# NOTE: in IPEDS, School of Medicine is counted inside City College, and
# SPS, SLU, Journalism and Public Health inside the Graduate Center.
CUNY_IPEDS <- IPEDS |>
  filter(str_detect(institution_name, "CUNY"),
         institution_name != "CUNY System Office") |>
  mutate(total = rowSums(across(starts_with("enrollment_"))),
         sector = case_when(
           str_detect(institution_name, "Community College") ~ "Community",
           str_detect(institution_name, "Graduate School|School of Law") ~ "Graduate/Professional",
           TRUE ~ "Senior"))

# -------------------------------------------------------------------------
# SOURCE 2: NY Open Data — CUNY Enrollment Trends by College (CUNY OIRA)
# https://data.ny.gov/d/487n-fp4r
# -------------------------------------------------------------------------
nyod_file <- "data/nyod/cuny_enrollment_nyod.csv"
if (!file.exists(nyod_file)) {
  download.file("https://data.ny.gov/api/views/487n-fp4r/rows.csv?accessType=DOWNLOAD",
                destfile = nyod_file, mode = "wb")
}

CUNY_NYOD <- read_csv(nyod_file, show_col_types = FALSE) |>
  clean_names() |>
  mutate(year = as.integer(str_extract(fall_term, "\\d{4}"))) |>  # fixes "Fall"/"FALL"
  filter(year >= 2015)


# ===== CHUNK A: NY college list (CUNY / SUNY / Private) =====
# Uses the directory (HD) files already downloaded by SOURCE 1.
# Keeps the newest record per college.

read_hd_ny <- function(yy) {
  f  <- file.path("data", "ipeds", glue("hd{yy}.csv.zip"))
  hd <- suppressWarnings(
    read_csv(f, show_col_types = FALSE,
             locale = locale(encoding = if_else(yy >= 2024, "utf-8", "windows-1252")))
  ) |>
    rename_with(toupper)
  if (!"F1SYSNAM" %in% names(hd)) hd$F1SYSNAM <- NA_character_
  hd |>
    filter(STABBR == "NY") |>
    transmute(institution_id   = UNITID,
              hd_year          = yy,
              institution_name = INSTNM,
              CONTROL          = as.integer(CONTROL),
              ICLEVEL          = as.integer(ICLEVEL),
              F1SYSNAM         = as.character(F1SYSNAM))
}

NY_COLLEGES <- map(2015:2024, read_hd_ny) |>
  bind_rows() |>
  arrange(institution_id, desc(hd_year)) |>
  distinct(institution_id, .keep_all = TRUE) |>
  filter(ICLEVEL %in% c(1, 2)) |>
  mutate(
    system = case_when(
      str_detect(coalesce(F1SYSNAM, ""), "City University of New York")  ~ "CUNY",
      str_detect(coalesce(F1SYSNAM, ""), "State University of New York") ~ "SUNY",
      CONTROL == 1 ~ "Other public",
      CONTROL == 2 ~ "Private nonprofit",
      CONTROL == 3 ~ "Private for-profit"),
    sector     = if_else(ICLEVEL == 2, "Two-year", "Four-year"),
    still_open = hd_year == max(hd_year)) |>
  filter(!str_detect(institution_name, "System Office"),
         institution_name != "Fashion Institute of Technology") |>
  select(institution_id, institution_name, system, sector, still_open)

message("\nColleges per group (expect CUNY 7 two-year + 13 four-year; SUNY 29 + 29):")
print(count(NY_COLLEGES, system, sector))



# ===== CHUNK B: Full-time / part-time + international (IPEDS EF A) =====
# Same EF A files SOURCE 1 already downloaded; this time we keep the
# full-time / part-time rows and the international (nonresident) total.

EFA_LEVELS <- tribble(
  ~EFALEVEL, ~level,          ~attendance,
  1,         "All students",  "All",
  2,         "Undergraduate", "All",
  12,        "Graduate",      "All",
  22,        "Undergraduate", "Full-time",
  42,        "Undergraduate", "Part-time",
  32,        "Graduate",      "Full-time",
  52,        "Graduate",      "Part-time"
)

read_efa_detail <- function(yy) {
  f <- file.path("data", "ipeds", glue("ef{yy}a.csv.zip"))
  zip_files <- unzip(f, list = TRUE)$Name
  csv_name  <- if (any(grepl("_rv", zip_files))) zip_files[grepl("_rv", zip_files)][1] else zip_files[1]
  read_csv(unz(f, csv_name), show_col_types = FALSE) |>
    rename_with(toupper) |>
    filter(EFALEVEL %in% EFA_LEVELS$EFALEVEL) |>
    transmute(institution_id = UNITID,
              year           = yy,
              EFALEVEL,
              total          = EFTOTLT,   # all students
              nonresident    = EFNRALT)   # international students
}

ENROLL_DETAIL <- map(2015:2024, read_efa_detail) |>
  bind_rows() |>
  left_join(EFA_LEVELS, by = "EFALEVEL") |>
  select(-EFALEVEL) |>
  inner_join(NY_COLLEGES, by = "institution_id") |>   # New York colleges only
  relocate(institution_id, institution_name, system, sector,
           year, level, attendance, total, nonresident)

message("Years in ENROLL_DETAIL: ",
        paste(sort(unique(ENROLL_DETAIL$year)), collapse = ", "))
print(count(ENROLL_DETAIL, level, attendance))


# ===== CHUNK C: Checks on ENROLL_DETAIL =====

message("\nCheck 1: full-time + part-time = total")
print(ENROLL_DETAIL |>
        filter(level != "All students") |>
        select(institution_id, year, level, attendance, total) |>
        pivot_wider(names_from = attendance, values_from = total) |>
        filter(!is.na(`Full-time`), !is.na(`Part-time`), !is.na(All)) |>
        summarize(rows_checked  = n(),
                  exact_matches = sum(`Full-time` + `Part-time` == All)))

message("\nCheck 2: undergrad totals match the existing IPEDS table")
print(ENROLL_DETAIL |>
        filter(level == "Undergraduate", attendance == "All") |>
        select(institution_id, year, total) |>
        inner_join(IPEDS |>
                     filter(level == "all undergrad") |>
                     mutate(ipeds_total = rowSums(across(starts_with("enrollment_")))) |>
                     select(institution_id, year, ipeds_total),
                   by = c("institution_id", "year")) |>
        summarize(rows_checked  = n(),
                  exact_matches = sum(total == ipeds_total)))

message("\nCheck 3: CUNY total, Fall 2024 (CUNY's own number: 237,671)")
print(ENROLL_DETAIL |>
        filter(system == "CUNY", year == 2024,
               level == "All students", attendance == "All") |>
        summarize(cuny_total_2024 = sum(total)))


# ===== CHUNK D: Shared event timeline + chart colors =====
# Draft list — each SQ owner should confirm the events for their question.

EVENTS <- tribble(
  ~year, ~event,
  2017,  "Travel ban",
  2020,  "COVID-19",
  2021,  "CUNY Comeback",
  2022,  "CUNY Reconnect",
  2025,  "Free community college (25-55)"
)

SYS_COLORS <- c(
  "CUNY"               = "#1f5aa6",
  "SUNY"               = "#c8781e",
  "Private nonprofit"  = "#5e8c31",
  "Private for-profit" = "#8c3b6b",
  "Other public"       = "#7f7f7f"
)

print(EVENTS)


# ===== CHUNK E: CUNY Fall 2025 (CUNY Student Data Book, manual exports) =====
# Files in data_manual/ named cuny_<topic>_fall<YEAR>.csv
# (Tableau exports: UTF-16, tab-separated). Age files belong to SQ3 and are skipped.

# Data Book school names -> IPEDS ID (IPEDS counts some schools inside a parent)
CUNY_DATABOOK_IDS <- tribble(
  ~college,                 ~institution_id,
  "Baruch",                 190512,
  "Brooklyn",               190549,
  "City",                   190567,
  "Medical School",         190567,
  "Hunter",                 190594,
  "John Jay",               190600,
  "Lehman",                 190637,
  "Medgar Evers",           190646,
  "NYCCT",                  190655,
  "Queens",                 190664,
  "Staten Island",          190558,
  "York",                   190691,
  "Graduate School",        190576,
  "Journalism",             190576,
  "Professional Studies",   190576,
  "Labor & Urban Studies",  190576,
  "Public Health",          190576,
  "Law School",             190682,
  "BMCC",                   190521,
  "Bronx",                  190530,
  "Guttman",                475565,
  "Hostos",                 190585,
  "Kingsborough",           190619,
  "LaGuardia",              190628,
  "Queensborough",          190673
)

read_databook <- function(f) {
  raw <- read.delim(f, fileEncoding = "UTF-16", header = FALSE,
                    colClasses = "character", strip.white = TRUE)
  hdr <- as.character(unlist(raw[1, ]))
  blank <- which(is.na(hdr) | hdr == "")
  hdr[blank] <- paste0("col", blank)
  hdr[1:2] <- c("databook_sector", "college")
  names(raw) <- hdr
  
  raw[-1, ] |>
    filter(college != "Total", databook_sector != "University Total") |>
    pivot_longer(-c(databook_sector, college),
                 names_to = "category", values_to = "students") |>
    mutate(students = replace_na(suppressWarnings(parse_number(students)), 0),
           measure  = str_match(basename(f), "^cuny_(.+)_fall\\d{4}")[, 2],
           year     = as.integer(str_extract(basename(f), "\\d{4}")))
}

db_files <- list.files("data_manual", pattern = "^cuny_.+_fall\\d{4}\\.csv$",
                       full.names = TRUE)
db_files <- db_files[!grepl("^cuny_age_", basename(db_files))]

if (length(db_files) == 0) {message("No CUNY Data Book files in data_manual/ yet.")}else {
  message("CUNY Data Book files found: ",
          paste(basename(db_files), collapse = ", "))
  
  CUNY_DATABOOK_RAW <- map(db_files, read_databook) |> bind_rows()
  
  unmapped <- setdiff(unique(CUNY_DATABOOK_RAW$college), CUNY_DATABOOK_IDS$college)
  if (length(unmapped) > 0) {
    warning("Data Book schools without an IPEDS ID: ",
            paste(unmapped, collapse = ", "), call. = FALSE)
  }
  
  # One row per college x year x measure x category
  CUNY_DATABOOK <- CUNY_DATABOOK_RAW |>
    inner_join(CUNY_DATABOOK_IDS, by = "college") |>
    group_by(institution_id, year, measure, category) |>
    summarize(students = sum(students), .groups = "drop") |>
    left_join(NY_COLLEGES |> select(institution_id, institution_name, sector),
              by = "institution_id") |>
    mutate(system = "CUNY", source = "CUNY Data Book")
  
  # What did we load?
  print(CUNY_DATABOOK |>
          group_by(measure, year, category) |>
          summarize(students = sum(students), .groups = "drop"),
        n = Inf)
}



# ===== CHUNK F: Check — CUNY Data Book vs IPEDS by college, Fall 2024 =====
if (exists("CUNY_DATABOOK")) {
  message("\nCheck 4: CUNY Data Book vs IPEDS by college, Fall 2024")
  print(CUNY_DATABOOK |>
          filter(year == 2024, category == "Grand Total") |>
          select(institution_id, institution_name, databook = students) |>
          inner_join(ENROLL_DETAIL |>
                       filter(year == 2024, level == "All students",
                              attendance == "All") |>
                       select(institution_id, ipeds = total),
                     by = "institution_id") |>
          mutate(pct_diff = round((databook - ipeds) / ipeds * 100, 1)) |>
          select(-institution_id) |>
          arrange(desc(abs(pct_diff))),
        n = Inf)
}

