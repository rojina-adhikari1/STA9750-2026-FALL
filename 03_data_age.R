# =========================================================================
# CUNY-Nauts — SQ3 data: undergraduate enrollment by AGE at CUNY vs SUNY
# Owner: Rojina Adhikari
# Creates AGE_PANEL: one row per CUNY/SUNY college per year, with
# total, under-25 and 25+ undergraduates and the data source.
# =========================================================================

library(tidyverse)
library(glue)
library(janitor)

if (!exists("IPEDS")) source("00_data.R")


# ===== CHUNK 2: Settings ===================================================
# Age is required in odd years, optional in even years. We try all years and
# later keep only the years that are complete.
AGE_YEARS <- 2015:2025

# IPEDS EF B codes (verified by Checks A and B)
AGE_ALL     <- 1   # EFBAGE: all ages
AGE_UNDER25 <- 2   # EFBAGE: under 25, total
AGE_25PLUS  <- 7   # EFBAGE: 25 and over, total
LEVEL_UG    <- 2   # LSTUDY: 1 = all students, 2 = undergraduate, 5 = graduate
# (verified: with 1, four-year totals exceeded EF A
#  undergrad totals by the size of graduate enrollment)

# DATA ACQUISITION
# ===== CHUNK 3: Download age data ==========================================
# IPEDS keeps older files at /datacenter/data/ and newer ones (e.g. 2024)
# at /complete-data-files/. We try both addresses for every year.
# No manual files are needed any more.

get_ipeds_age <- function(years){
  map(years, function(yy){
    f <- file.path("data", "ipeds", glue("ef{yy}b.csv.zip"))
    
    # Download once; skip the year if it is not available
    if (!file.exists(f)) {
      message(glue("Downloading ef{yy}b.csv.zip ..."))
      urls <- c(
        glue("https://nces.ed.gov/ipeds/datacenter/data/EF{yy}B.zip"),
        glue("https://nces.ed.gov/ipeds/complete-data-files/EF{yy}B.zip")
      )
      ok <- FALSE
      for (u in urls) {
        ok <- tryCatch({
          suppressWarnings(
            download.file(u, destfile = f, quiet = TRUE, mode = "wb")
          )
          file.exists(f) && file.size(f) > 1000
        }, error = function(e) FALSE)
        if (ok) break                       # found it, stop trying
        if (file.exists(f)) file.remove(f)  # remove a bad/empty download
      }
      if (!ok) return(NULL)                 # year not available at either address
    }
    
    # Use the revised (_rv) file inside the zip when there is one
    zip_files <- unzip(f, list = TRUE)$Name
    csv_name  <- if (any(grepl("_rv", zip_files))) {
      zip_files[grepl("_rv", zip_files)][1]
    } else {
      zip_files[1]
    }
    
    read_csv(unz(f, csv_name), show_col_types = FALSE) |>
      rename_with(toupper) |>
      transmute(institution_id = UNITID, year = yy,
                EFBAGE, LSTUDY, n = EFAGE09)
  }) |> bind_rows()
}

AGE_RAW <- suppressWarnings(get_ipeds_age(AGE_YEARS))



# ===== CHUNK 4: Missing-years warning ======================================
missing_years <- setdiff(AGE_YEARS, unique(AGE_RAW$year))

if (length(missing_years) > 0) {
  warning(glue(
    "No IPEDS age data for: {paste(missing_years, collapse = ', ')}. ",
    "Fall 2025 IPEDS enrollment has not been released yet (checked Oct 2026). ",
    "SQ3 uses the years that are available, plus the CUNY Data Book."
  ), call. = FALSE)
} else {
  message("IPEDS age data loaded for all years: ", paste(AGE_YEARS, collapse = ", "))
}

# DATA CLEANING AND PREPARATION
# ===== CHUNK 5: Pick the CUNY and SUNY colleges ============================
# CONTROL 1 = public; ICLEVEL 1 = four-year, 2 = two-year.
# Excluded: system offices (no students) and the Fashion Institute of
# Technology (a SUNY community college by statute but four-year in IPEDS).
NY_PUBLIC <- read_csv("data/ipeds/hd2024.csv.zip", show_col_types = FALSE,
                      locale = locale(encoding = "utf-8")) |>
  rename_with(toupper) |>
  filter(STABBR == "NY", CONTROL == 1, ICLEVEL %in% c(1, 2),
         F1SYSNAM %in% c("City University of New York",
                         "State University of New York System")) |>
  transmute(institution_id   = UNITID,
            institution_name = INSTNM,
            system = if_else(F1SYSNAM == "City University of New York",
                             "CUNY", "SUNY"),
            sector = if_else(ICLEVEL == 2, "Community", "Four-year")) |>
  filter(!str_detect(institution_name, "System Office"),
         institution_name != "Fashion Institute of Technology")


# ===== CHUNK 6: Clean into one row per college per year ===================
AGE_CC <- AGE_RAW |>
  filter(LSTUDY == LEVEL_UG,
         EFBAGE %in% c(AGE_ALL, AGE_UNDER25, AGE_25PLUS)) |>
  mutate(group = case_when(EFBAGE == AGE_ALL     ~ "total",
                           EFBAGE == AGE_UNDER25 ~ "under25",
                           EFBAGE == AGE_25PLUS  ~ "age25plus")) |>
  select(institution_id, year, group, n) |>
  pivot_wider(names_from = group, values_from = n, values_fn = sum) |>
  inner_join(NY_PUBLIC, by = "institution_id") |>
  filter(total > 0) |>
  mutate(unknown_age  = total - under25 - age25plus,
         share_25plus = age25plus / total)

# Total undergrad enrollment for the same colleges, every year (EF A)
CC_TOTAL <- IPEDS |>
  filter(level == "all undergrad") |>
  mutate(total = rowSums(across(starts_with("enrollment_")))) |>
  select(institution_id, year, total) |>
  inner_join(NY_PUBLIC, by = "institution_id") |>
  filter(total > 0)

#DATA QUALITY CHECKS
# ===== CHUNK 7: Coverage by year ===========================================
# A year is usable for a group if >= 90% of its colleges reported age that
# year (age is optional in even years).
AGE_COVERAGE <- AGE_CC |>
  count(year, system, sector, name = "n_reporting") |>
  left_join(CC_TOTAL |>
              filter(year %in% unique(AGE_CC$year)) |>
              count(year, system, sector, name = "n_colleges"),
            by = c("year", "system", "sector")) |>
  mutate(pct_reporting = n_reporting / n_colleges)

# Years where all four groups are complete
AGE_YEARS_OK <- AGE_COVERAGE |>
  group_by(year) |>
  summarize(ok = all(pct_reporting >= 0.9) & n() == 4, .groups = "drop") |>
  filter(ok) |>
  pull(year)

#DATA QUALITY CHECKS
# ===== CHUNK 8: Checks A and B =============================================
message("\n---- SQ3 data checks ----")

message("\nColleges per group (expect CUNY 7 + 13, SUNY 29 + 29):")
print(count(NY_PUBLIC, system, sector))

message("\nShare of colleges reporting age, by year:")
print(AGE_COVERAGE |>
        mutate(group = paste(system, sector)) |>
        select(year, group, pct_reporting) |>
        pivot_wider(names_from = group, values_from = pct_reporting),
      n = Inf)

message("\nComplete years kept for analysis: ",
        paste(AGE_YEARS_OK, collapse = ", "))

message("\nCheck A — share of students with unknown age (should be small):")
print(AGE_CC |>
        summarize(max_unknown_share = max(unknown_age / total, na.rm = TRUE),
                  min_unknown       = min(unknown_age, na.rm = TRUE)))

message("\nCheck B — EF B total equals EF A undergrad total:")
print(AGE_CC |>
        inner_join(CC_TOTAL |> select(institution_id, year, efa_total = total),
                   by = c("institution_id", "year")) |>
        summarize(rows_checked  = n(),
                  exact_matches = sum(total == efa_total),
                  max_abs_diff  = max(abs(total - efa_total))))


#Data cleaning and preparation
# ===== CHUNK 9: Data Book name -> IPEDS ID table ===========================
# IPEDS reports CUNY sub-schools inside a parent institution:
# Medicine in City College; Journalism, SPS, SLU and Public Health in the
# Graduate Center. Several Data Book rows can share one IPEDS ID.
DATABOOK_IDS <- tribble(
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


# ===== CHUNK 10: Read the Data Book files ==================================
# Manual exports from https://insights.cuny.edu (Student Data Book >
# Enrollment), Column Header = Age Group, Fall, undergraduate degree levels,
# saved as data_manual/cuny_age_fall<YEAR>.csv.
# Tableau exports these as UTF-16, tab-separated, with commas in numbers.
read_cuny_age <- function(yy){
  f <- file.path("data_manual", glue("cuny_age_fall{yy}.csv"))
  if (!file.exists(f)) return(NULL)
  
  raw <- read.delim(f, fileEncoding = "UTF-16", header = FALSE, skip = 1,
                    colClasses = "character", strip.white = TRUE)
  names(raw)[1:5] <- c("databook_sector", "college", "under25", "age25plus", "total")
  
  raw |>
    filter(college != "Total", databook_sector != "University Total") |>
    mutate(across(c(under25, age25plus, total), readr::parse_number),
           across(c(under25, age25plus, total), ~ replace_na(.x, 0)),
           year = yy)
}

DATABOOK_RAW <- map(2015:2025, read_cuny_age) |> bind_rows()

# Any Data Book school names we could not map to an IPEDS ID?
unmapped <- setdiff(unique(DATABOOK_RAW$college), DATABOOK_IDS$college)
if (length(unmapped) > 0) {
  warning("Unmapped Data Book schools: ", paste(unmapped, collapse = ", "),
          call. = FALSE)
}

#DATA QUALITY CHECKS
# ===== CHUNK 11: Combine the Data Book + Check C ===========================
# Roll sub-schools up to their IPEDS parent: one row per college x year
AGE_DATABOOK <- DATABOOK_RAW |>
  inner_join(DATABOOK_IDS, by = "college") |>
  group_by(institution_id, year) |>
  summarize(across(c(under25, age25plus, total), sum), .groups = "drop") |>
  inner_join(NY_PUBLIC, by = "institution_id") |>
  mutate(share_25plus = age25plus / total,
         source = "CUNY Data Book")

# Check C: Data Book vs IPEDS in the year(s) both report
overlap_years <- intersect(unique(AGE_DATABOOK$year),
                           unique(AGE_CC$year[AGE_CC$system == "CUNY"]))

AGE_VALIDATION <- AGE_DATABOOK |>
  filter(year %in% overlap_years) |>
  select(institution_id, institution_name, year,
         db_total = total, db_25plus = age25plus) |>
  inner_join(AGE_CC |> select(institution_id, year,
                              ipeds_total = total, ipeds_25plus = age25plus),
             by = c("institution_id", "year")) |>
  mutate(pct_total  = (db_total  - ipeds_total)  / ipeds_total,
         pct_25plus = (db_25plus - ipeds_25plus) / ipeds_25plus)

message("\nCheck C — CUNY Data Book vs IPEDS (overlap years: ",
        paste(overlap_years, collapse = ", "), "):")
print(AGE_VALIDATION |>
        select(institution_name, year, db_total, ipeds_total, pct_total,
               db_25plus, ipeds_25plus, pct_25plus) |>
        arrange(desc(abs(pct_total))),
      n = Inf, width = Inf)

# DATA COMBINATION
# ===== CHUNK 12: Splice and build the final table ==========================
# IPEDS is the primary source. For CUNY years IPEDS lacks (2022, 2024, 2025)
# we use the CUNY Data Book, rescaled ("spliced") to the IPEDS definition
# using each college's IPEDS / Data Book ratio in 2023, the year both report.

# IPEDS rows: keep a year for a group if >= 90% of its colleges reported
IPEDS_OK <- AGE_COVERAGE |>
  filter(pct_reporting >= 0.9) |>
  select(year, system, sector)

PANEL_IPEDS <- AGE_CC |>
  semi_join(IPEDS_OK, by = c("year", "system", "sector")) |>
  select(institution_id, institution_name, system, sector, year,
         total, under25, age25plus) |>
  mutate(source = "IPEDS")

# College-level ratios IPEDS / Data Book in the overlap year
SPLICE_YEAR <- 2023

SPLICE_RATIOS <- AGE_DATABOOK |>
  filter(year == SPLICE_YEAR) |>
  select(institution_id, db_total = total, db_u25 = under25, db_25p = age25plus) |>
  inner_join(AGE_CC |>
               filter(year == SPLICE_YEAR) |>
               select(institution_id, ip_total = total, ip_u25 = under25,
                      ip_25p = age25plus),
             by = "institution_id") |>
  transmute(institution_id,
            r_total = ip_total / db_total,
            r_u25   = ip_u25   / db_u25,
            r_25p   = ip_25p   / db_25p)

# Data Book rows only for CUNY years IPEDS does not cover
cuny_ipeds_years <- PANEL_IPEDS |>
  filter(system == "CUNY") |>
  distinct(year) |>
  pull(year)

PANEL_DATABOOK <- AGE_DATABOOK |>
  filter(!year %in% cuny_ipeds_years) |>
  inner_join(SPLICE_RATIOS, by = "institution_id") |>
  transmute(institution_id, institution_name, system, sector, year,
            total     = total     * r_total,
            under25   = under25   * r_u25,
            age25plus = age25plus * r_25p,
            source    = "CUNY Data Book (spliced to IPEDS)")

# Final analysis table
AGE_PANEL <- bind_rows(PANEL_IPEDS, PANEL_DATABOOK) |>
  mutate(share_25plus = age25plus / total) |>
  arrange(system, sector, institution_name, year)


# ===== CHUNK 13: Final summary printout ====================================
message("\n---- AGE_PANEL: final SQ3 analysis table ----")
print(AGE_PANEL |>
        count(year, system, sector, source) |>
        mutate(group = paste(system, sector)) |>
        select(year, group, n, source) |>
        pivot_wider(names_from = group, values_from = n),
      n = Inf, width = Inf)

message("\nSplice ratios (IPEDS / Data Book, ", SPLICE_YEAR,
        "): 1.00 = identical; below 1 = Data Book higher")
print(SPLICE_RATIOS |>
        left_join(NY_PUBLIC |> select(institution_id, institution_name),
                  by = "institution_id") |>
        select(institution_name, r_total, r_25p) |>
        arrange(r_total),
      n = Inf)

message("\nYears available per system:")
print(AGE_PANEL |>
        group_by(system) |>
        summarize(years = paste(sort(unique(year)), collapse = ", ")),
      width = Inf)


