# ################################################################################################ #
# ## Professional Fee Costs: Non-Anesthesia CPT Codes (CMS Physician Fee Schedule) ## ------------ #
# ################################################################################################ #

# Requires: df.raw_codes, df.BEA_adj2025, and folder/flag variables defined in the main script.

# Prices each non-anesthesia CPT code (i.e., excluding the 00100-01999 anesthesia range, priced
# separately in 02_CPT_costs_anesthesia.R) using the CMS Physician Fee Schedule: payment = total RVU
# (work + facility practice expense + malpractice) x that year's Conversion Factor, inflated to 2025
# USD via the BEA GDP deflator.

# ------------------------------------------------------------------------------------------------ #
# -- Download CMS PFS RVU files ------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #

# Annual "PPRRVU" files, from the CMS PFS Relative Value Files page:
# https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/pfs-relative-value-files

rvu_colnames <- c(
    "HCPCS", "MOD", "DESCRIPTION", "STATUS_CODE", "col5_unused",
    "WORK_RVU", "PE_RVU_nonfac", "PE_RVU_nonfac_ind",
    "PE_RVU_fac", "PE_RVU_fac_ind", "MP_RVU",
    "TOTAL_nonfac", "TOTAL_fac", "PCTC_IND", "GLOBAL_DAYS",
    "PRE_OP", "INTRA_OP", "POST_OP", "MULT_PROC", "BILAT_SURG",
    "ASST_SURG", "CO_SURG", "TEAM_SURG", "ENDO_BASE",
    "CONV_FACTOR_FILE", "PHYS_SUPERVISION", "CALC_FLAG",
    "DIAG_IMAGING_FAM", "OPPS_PE_nonfac", "OPPS_PE_fac", "OPPS_MP"
)  # stable, documented CMS PFS layout across years

rvu_urls <- c(
    "2015" = "https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/downloads/rvu15a.zip",
    "2016" = "https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/downloads/rvu16a.zip",
    "2017" = "https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/downloads/rvu17a.zip",
    "2018" = "https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/downloads/rvu18a.zip",
    "2019" = "https://www.cms.gov/medicare/medicare-fee-for-service-payment/physicianfeesched/downloads/rvu19a.zip",
    "2020" = "https://www.cms.gov/files/zip/rvu20a-updated-01312020.zip",
    "2021" = "https://www.cms.gov/files/zip/rvu21a-updated-01052021.zip",
    "2022" = "https://www.cms.gov/files/zip/rvu22a.zip",
    "2023" = "https://www.cms.gov/files/zip/rvu23a-updated-01/31/2023.zip",
    "2024" = "https://www.cms.gov/files/zip/rvu24a-updated-04/01/2024.zip",
    "2025" = "https://www.cms.gov/files/zip/rvu25a-updated-01/10/2025.zip",
    "2026" = "https://www.cms.gov/files/zip/rvu26a-updated-12-29-2025.zip"
)  # confirm/extend this list for years outside 2015-2026

if (flag.downloadgov.PFS) {
    options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    iwalk(rvu_urls, function(url, yr) {
        zip_path <- file.path(f.rvu, paste0("RVU", yr, "A.zip"))
        out_dir  <- file.path(f.rvu, yr)
        download.file(url, destfile = zip_path, mode = "wb", method = "libcurl")
        unzip(zip_path, exdir = out_dir)
    })
}

# find the correct PPRRVU csv within a year's folder --------------------------------------------- #
find_rvu_file <- function(year_dir) {
    candidates <- list.files(year_dir,
                             pattern = "^PPRRVU.*\\.csv$",
                             full.names = TRUE,
                             ignore.case = TRUE)
    if (length(candidates) == 0) stop("No PPRRVU*.csv found in: ", year_dir)
    if (length(candidates) > 1) {
        # some years split QPP / non-QPP (Qualifying APM Participant) versions with a differential
        # conversion factor - use the non-QPP (standard) file for cohort-level estimates
        nonqpp <- candidates[grepl("nonQPP", candidates, ignore.case = TRUE)]
        if (length(nonqpp) == 1) return(nonqpp)
        stop("Multiple PPRRVU*.csv files found in ", year_dir, "; inspect and select manually: ",
             paste(basename(candidates), collapse = ", "))
    }
    candidates
}

read_one_rvu <- function(path, yr) {
    header_row <- grep("^HCPCS", readLines(path, n = 15))[1]
    if (is.na(header_row)) stop("No header row starting with 'HCPCS' found in: ", path)
    df <- read_csv(path, skip = header_row, col_names = rvu_colnames,
                    col_types = cols(.default = "c"), na = character())
    df %>%
        transmute(CPT_Code         = HCPCS,
                   Modifier         = MOD,
                   Status           = STATUS_CODE,
                   work_RVU         = as.numeric(WORK_RVU),
                   PE_RVU_fac       = as.numeric(PE_RVU_fac),
                   MP_RVU           = as.numeric(MP_RVU),
                   Year_Code        = yr)
}

year_dirs <- list.dirs(f.rvu, recursive = FALSE)
year_dirs <- year_dirs[basename(year_dirs) %in% as.character(years_needed)]

df.RVU <- map_dfr(year_dirs, function(dir) {
    yr <- as.numeric(basename(dir))
    read_one_rvu(find_rvu_file(dir), yr)
})

# use the unmodified base row per code/year (Modifier blank/NA) - the "whole service, professional +
# technical" rate, used when claim-level split-billing status is unknown
df.RVU_unmod <- df.RVU %>%
    filter(is.na(Modifier) | Modifier == "") %>%
    distinct(CPT_Code, Year_Code, .keep_all = TRUE)

# ------------------------------------------------------------------------------------------------ #
# -- Join to billing codes and price ------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

df.CPT_costs <- df.raw_codes %>%
    mutate(Year_Code = year(Date_Code)) %>%
    left_join(df.RVU_unmod %>% select(-Modifier), by = c("CPT_Code", "Year_Code")) %>%
    left_join(df.BEA_adj2025 %>% select(Year_Code, GDP_2025_multiplier, PFS_Conversion_Factor),
              by = "Year_Code") %>%
    mutate(payable         = Status %in% c("A", "R", "T"),
           total_RVU       = work_RVU + PE_RVU_fac + MP_RVU,
           payment_nominal = total_RVU * PFS_Conversion_Factor,
           payment_2025usd = payment_nominal * GDP_2025_multiplier) %>%
    distinct()

# exclude anesthesia codes (priced separately in 02_CPT_costs_anesthesia.R)
df.PFS_costs_nonanes <- df.CPT_costs %>%
    filter(payable, !(CPT_Code >= "00100" & CPT_Code <= "01999")) %>%
    mutate(days_from_surgery = as.numeric(difftime(Date_Code, Date_Surgery, units = "days")),
           cost_component    = "Other CPT") %>%
    select(PMRN, Last_Name, DOB, Date_Surgery, Year_Code, Date_Code, days_from_surgery,
           CPT_Code, cost_component, payment_2025usd) %>%
    distinct()

if (flag.save.table) {
    write.csv(df.PFS_costs_nonanes,
              file.path(f.tables, paste0("PFS_costs_nonanesthesia_", date.analysis, ".csv")),
              row.names = FALSE)
}

# ------------------------------------------------------------------------------------------------ #
# -- Script End ---------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #