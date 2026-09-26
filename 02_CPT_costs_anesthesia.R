# ################################################################################################ #
# ## Professional Fee Costs: Anesthesia CPT Codes (CMS Physician Fee Schedule) ## ---------------- #
# ################################################################################################ #

# Requires: df.raw_codes, df.clinical (for OR_time_min), df.BEA_adj2025, and folder/flag variables
# defined in the main script.

# Anesthesia is priced separately from other professional fees because CMS uses a distinct formula:
# payment = (base units + time units) x a locality-specific anesthesia conversion factor, rather
# than the standard RVU x Conversion Factor formula used elsewhere.

# ------------------------------------------------------------------------------------------------ #
# -- Download anesthesia base units and locality conversion factors ------------------------------ #
# ------------------------------------------------------------------------------------------------ #

# CMS states that anesthesia base units are unchanged across recent years, so a single base-unit
# table can be used for the full study period; a 2013 table is used as a fallback for any code
# retired before the current table's vintage (e.g., CPT 00740, retired 2018).

anes_base_dir      <- file.path(f.grouper, "..", "anes_base_units")
anes_base_2013_dir <- file.path(f.grouper, "..", "anes_base_units_2013")

if (flag.downloadgov.PFS) {
    anes_base_url      <- "https://www.cms.gov/files/zip/2022-anesthesia-base-units-cpt-code.zip"
    anes_base_2013_url <- "https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/downloads/2013-anesthesia-baseunits-cpt.zip"
    options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    download.file(anes_base_url, destfile = file.path(f.rvu, "anes_base_units.zip"), mode = "wb", method = "libcurl")
    unzip(file.path(f.rvu, "anes_base_units.zip"), exdir = anes_base_dir)
    download.file(anes_base_2013_url, destfile = file.path(f.rvu, "anes_base_units_2013.zip"), mode = "wb", method = "libcurl")
    unzip(file.path(f.rvu, "anes_base_units_2013.zip"), exdir = anes_base_2013_dir)
}

df.anes_base_current <- read_excel(
    list.files(anes_base_dir, pattern = "\\.xlsx$", full.names = TRUE)[1],
    skip = 3, col_names = c("CPT_Code", "Base_Units"), col_types = c("text", "numeric"))

df.anes_base_2013 <- read_csv(
    list.files(anes_base_2013_dir, pattern = "\\.csv$", full.names = TRUE)[1],
    skip = 3, col_names = c("CPT_Code_raw", "Base_Units_2013"), col_types = cols(.default = "c")) %>%
    transmute(CPT_Code        = sprintf("%05s", CPT_Code_raw) %>% gsub(" ", "0", .),
              Base_Units_2013 = as.numeric(Base_Units_2013))

# fall back to the 2013 value only for codes absent from the current table (e.g., retired codes)
df.anes_base <- df.anes_base_current %>%
    full_join(df.anes_base_2013, by = "CPT_Code") %>%
    mutate(Base_Units = coalesce(Base_Units, Base_Units_2013)) %>%
    select(CPT_Code, Base_Units)

# annual anesthesia locality conversion-factor files (one row per Medicare carrier/locality)
anes_cf_colnames <- c("Carrier", "Locality", "LocalityName", "CF_cents")

find_anes_cf_file <- function(year_dir) {
    candidates <- list.files(year_dir, pattern = "^Anes.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
    if (length(candidates) != 1) stop("Expected exactly 1 Anes*.csv in: ", year_dir)
    candidates
}

read_one_anes_cf <- function(path, yr) {
    read_csv(path, col_names = anes_cf_colnames, col_types = cols(.default = "c")) %>%
        mutate(across(c(Carrier, Locality, LocalityName), trimws),
               CF_raw = as.numeric(CF_cents),
               # some years store whole cents (2260 = $22.60), others dollars-with-decimal already
               # (22.30) - detect by magnitude rather than assume, to avoid a silent 100x error
               Anes_CF   = if_else(CF_raw > 100, CF_raw / 100, CF_raw),
               Year_Code = yr) %>%
        select(Carrier, Locality, LocalityName, Anes_CF, Year_Code)
}

year_dirs <- list.dirs(f.rvu, recursive = FALSE)
year_dirs <- year_dirs[basename(year_dirs) %in% as.character(years_needed)]

df.anes_cf_locality <- map_dfr(year_dirs, function(dir) {
    yr <- as.numeric(basename(dir))
    read_one_anes_cf(find_anes_cf_file(dir), yr)
}) %>%
    filter(grepl(FACILITY_LOCALITY_MATCH, LocalityName, ignore.case = TRUE),
           !grepl("rest of", LocalityName, ignore.case = TRUE))  # exclude "rest of state" rows

# ------------------------------------------------------------------------------------------------ #
# -- Identify and classify anesthesia CPT rows --------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

time_based_exceptions <- c("01995", "01996")  # CMS uses a flat rate, not time units, for these

df.anes_rows <- df.raw_codes %>%
    filter(CPT_Code >= "00100", CPT_Code <= "01999") %>%
    mutate(anes_pricing_type = case_when(
        CPT_Code %in% time_based_exceptions ~ "flat_daily",
        CPT_Code == "01999"                 ~ "unlisted",
        TRUE                                ~ "time_based"),
        days_from_surgery = as.numeric(difftime(Date_Code, Date_RPLND, units = "days"))) %>%
    left_join(df.clinical %>% select(Last_Name, DOB, Date_RPLND, OR_time_min),
              by = c("Last_Name", "DOB", "Date_RPLND"))

# within each patient, the time-based anesthesia row closest to the surgery date (within +/-3 days)
# is treated as the index-procedure anesthesia and assigned the recorded OR time
df.anes_rows <- df.anes_rows %>%
    group_by(PMRN, Date_RPLND) %>%
    mutate(is_index_anesthesia = anes_pricing_type == "time_based" &
               abs(days_from_surgery) <= 3 &
               abs(days_from_surgery) == min(abs(days_from_surgery)[anes_pricing_type == "time_based"],
                                              na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(is_index_anesthesia = coalesce(is_index_anesthesia, FALSE),
           OR_time_min_used    = if_else(is_index_anesthesia, OR_time_min, NA_real_))

# ------------------------------------------------------------------------------------------------ #
# -- Apply anesthesia payment formula ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #
# payment = (base units + time units) x locality conversion factor, where 1 time unit = 15 minutes

df.anes_costs <- df.anes_rows %>%
    mutate(Year_Code = year(Date_Code)) %>%
    left_join(df.anes_base, by = "CPT_Code") %>%
    left_join(df.anes_cf_locality %>% select(Year_Code, Anes_CF), by = "Year_Code") %>%
    left_join(df.BEA_adj2025 %>% select(Year_Code, GDP_2025_multiplier), by = "Year_Code") %>%
    mutate(
        time_units      = if_else(is_index_anesthesia, OR_time_min_used / 15, 0),
        total_units     = Base_Units + time_units,
        payment_nominal = total_units * Anes_CF,
        payment_2025usd = payment_nominal * GDP_2025_multiplier)

df.PFS_costs_anes <- df.anes_costs %>%
    mutate(cost_component = "Anesthesia") %>%
    select(PMRN, Last_Name, DOB, Date_RPLND, Year_Code, Date_Code, days_from_surgery,
           CPT_Code, cost_component, payment_2025usd) %>%
    distinct()

if (flag.save.table) {
    write.csv(df.PFS_costs_anes,
              file.path(f.tables, paste0("PFS_costs_anesthesia_", date.analysis, ".csv")),
              row.names = FALSE)
}

# ------------------------------------------------------------------------------------------------ #
# -- Script End ---------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #