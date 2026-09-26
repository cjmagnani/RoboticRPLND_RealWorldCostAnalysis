# ################################################################################################ #
# ## Facility Costs, Readmissions: MS-DRG Assignment + IPPS Pricing ## --------------------------- #
# ################################################################################################ #

# Requires: df.raw_codes, df.clinical, df.BEA_adj2025, df.drg_weights, df.wage_index, df.ipps_rates,
# GAF_EXPONENT, and the grouper-calling functions/objects, all created by
# 03_MSDRG_IPPS_index_admission.R (run that script first - readmissions are priced identically to
# the index admission, just re-identified as separate inpatient stays).

# Must set if a readmission is counted toward a follow-up window if it ADMITS vs DISCHARGES within
# that window - an MS-DRG payment is indivisible and cannot be pro-rated across a window boundary

# ------------------------------------------------------------------------------------------------ #
# -- Identify readmission stays from room-and-board billing codes -------------------------------- #
# ------------------------------------------------------------------------------------------------ #

# EDIT: rnb_floor_codes / rnb_icu_codes are your institution's internal per-diem room-and-board
# billing codes (not standard CPT codes), used to reconstruct contiguous inpatient stays from
# daily billing records. Consecutive days (no gap) are grouped into one stay; the stay containing
# day 0 (surgery date) is the index admission, and every other stay is a readmission.

rnb_floor_codes <- c("EDIT_ME")  # provide list of codes for Room & Board on regular ward/floor
rnb_icu_codes   <- c("EDIT_ME")  # provide list of codes for Room & Board on ICU units

df.all_stays <- df.raw_codes %>%
    select(PMRN, Last_Name, DOB, Date_RPLND, Date_Code, CPT_Code) %>%
    distinct() %>%
    filter(CPT_Code %in% c(rnb_floor_codes, rnb_icu_codes)) %>%
    mutate(day_offset = as.numeric(difftime(Date_Code, Date_RPLND, units = "days"))) %>%
    filter(day_offset >= 0) %>%
    distinct(PMRN, Last_Name, DOB, Date_RPLND, Date_Code, day_offset) %>%
    arrange(PMRN, day_offset) %>%
    group_by(PMRN, Last_Name, DOB, Date_RPLND) %>%
    mutate(gap     = coalesce(day_offset - lag(day_offset) != 1, day_offset != 0),
           stay_id = cumsum(gap)) %>%
    ungroup()

df.readmit_stays <- df.all_stays %>%
    group_by(PMRN, Last_Name, DOB, Date_RPLND) %>%
    mutate(index_stay = if (any(day_offset == 0)) stay_id[which(day_offset == 0)][1] else min(stay_id)) %>%
    filter(stay_id != index_stay) %>%
    group_by(PMRN, Last_Name, DOB, Date_RPLND, stay_id) %>%
    summarize(Readmit_Admit_Date     = as.Date(min(Date_Code)),
              Readmit_Discharge_Date = as.Date(max(Date_Code)),
              Readmit_LOS            = n_distinct(day_offset),
              Readmit_Admit_Day      = min(day_offset),
              Readmit_Discharge_Day  = max(day_offset),
              .groups = "drop") %>%
    arrange(PMRN, Readmit_Admit_Day) %>%
    group_by(PMRN, Last_Name, DOB, Date_RPLND) %>%
    mutate(Readmit_Seq = row_number()) %>%
    ungroup() %>%
    mutate(Readmit_ID = paste0(PMRN, "_R", Readmit_Seq))

# ------------------------------------------------------------------------------------------------ #
# -- Principal/secondary diagnoses and procedures, per readmission ------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# Principal diagnosis rule: (a) if the readmission has ICD-10-PCS codes, use the ICD-10-CM code
# billed on the same date as the lowest-ranked PCS code; (b) otherwise, the most frequently billed
# ICD-10-CM code on the admission date.

df.readmit_dx_all <- df.readmit_stays %>%
    select(PMRN, Last_Name, DOB, Date_RPLND,
           Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date) %>%
    left_join(df.raw_codes %>%
                  select(PMRN, Last_Name, DOB, Date_RPLND, Date_Code, ICD_CM_Code) %>%
                  distinct(),
              by = c("PMRN", "Last_Name", "DOB", "Date_RPLND"),
              relationship = "many-to-many") %>%
    filter(!is.na(ICD_CM_Code),
           Date_Code >= Readmit_Admit_Date,
           Date_Code <= Readmit_Discharge_Date)

df.readmit_pcs_all <- df.readmit_stays %>%
    select(PMRN, Last_Name, DOB, Date_RPLND,
           Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date) %>%
    left_join(df.ICD10PCS_codes %>%
                  select(PMRN, Last_Name, DOB, Date_RPLND,
                         Date_Code, original_ICD_10_PCS_rank, ICD_10_PCS) %>%
                  distinct(),
              by = c("PMRN", "Last_Name", "DOB", "Date_RPLND"),
              relationship = "many-to-many") %>%
    filter(!is.na(ICD_10_PCS),
           Date_Code >= Readmit_Admit_Date,
           Date_Code <= Readmit_Discharge_Date)

pick_principal_dx <- function(dx_df, pcs_df, stays_df) {
    via_pcs <- pcs_df %>%
        group_by(PMRN, Last_Name, DOB, Date_RPLND,
                 Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date) %>%
        filter(original_ICD_10_PCS_rank == min(original_ICD_10_PCS_rank)) %>%
        slice(1) %>%
        ungroup() %>%
        select(PMRN, Last_Name, DOB, Date_RPLND,
               Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date, pcs_date = Date_Code) %>%
        left_join(dx_df %>%
                      select(Readmit_ID, Date_Code, ICD_CM_Code),
                  by = c("Readmit_ID", "pcs_date" = "Date_Code"),
                  relationship = "many-to-many") %>%
        filter(!is.na(ICD_CM_Code)) %>%
        group_by(PMRN, Last_Name, DOB, Date_RPLND,
                 Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date) %>%
        count(ICD_CM_Code) %>%
        arrange(desc(n), ICD_CM_Code) %>%
        slice(1) %>%
        ungroup() %>%
        transmute(PMRN, Last_Name, DOB, Date_RPLND,
                  Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date,
                  Principal_Dx = ICD_CM_Code)

    via_admit <- dx_df %>%
        anti_join(via_pcs, by = "Readmit_ID") %>%
        semi_join(stays_df %>%
                      select(Readmit_ID, Readmit_Admit_Date),
                  by = c("Readmit_ID", "Readmit_Admit_Date")) %>%
        filter(Date_Code == Readmit_Admit_Date) %>%
        group_by(PMRN, Last_Name, DOB, Date_RPLND,
                 Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date) %>%
        count(ICD_CM_Code) %>%
        arrange(desc(n), desc(ICD_CM_Code)) %>%
        slice(1) %>%
        ungroup() %>%
        transmute(PMRN, Last_Name, DOB, Date_RPLND,
                  Readmit_ID, Readmit_Admit_Date, Readmit_Discharge_Date,
                  Principal_Dx = ICD_CM_Code)

    bind_rows(via_pcs, via_admit)
}
df.readmit_pdx <- pick_principal_dx(df.readmit_dx_all, df.readmit_pcs_all, df.readmit_stays)

# any readmission with no diagnosis code cannot be grouped - review manually before proceeding
df.readmit_stays %>% anti_join(df.readmit_pdx, by = "Readmit_ID") %>%
    select(Readmit_ID, PMRN, Readmit_Admit_Date, Readmit_LOS)

df.readmit_pproc <- df.readmit_pcs_all %>%
    group_by(Readmit_ID) %>%
    arrange(original_ICD_10_PCS_rank, ICD_10_PCS) %>%
    slice(1) %>%
    ungroup() %>%
    select(Readmit_ID, Principal_Proc = ICD_10_PCS)

df.readmit_sdx <- df.readmit_dx_all %>%
    right_join(df.readmit_pdx %>% select(PMRN, Last_Name, DOB, Date_RPLND, Readmit_ID,
                                          Readmit_Admit_Date, Readmit_Discharge_Date, Principal_Dx),
               by = c("PMRN", "Last_Name", "DOB", "Date_RPLND",
                      "Readmit_ID", "Readmit_Admit_Date", "Readmit_Discharge_Date")) %>%
    filter(is.na(Principal_Dx) | ICD_CM_Code != Principal_Dx) %>%
    distinct(Readmit_ID, ICD_CM_Code) %>%
    group_by(Readmit_ID) %>%
    summarize(Secondary_Dx = list(ICD_CM_Code),
              n_secondary_dx = n(),
              .groups = "drop")

df.readmit_sproc <- df.readmit_pcs_all %>%
    left_join(df.readmit_pproc, by = "Readmit_ID") %>%
    filter(ICD_10_PCS != Principal_Proc) %>%
    distinct(Readmit_ID, ICD_10_PCS) %>%
    group_by(Readmit_ID) %>%
    summarize(Secondary_Proc = list(ICD_10_PCS),
              n_secondary_proc = n(),
              .groups = "drop")

# ------------------------------------------------------------------------------------------------ #
# -- Assemble grouper input and run (reuses functions from 03_MSDRG_IPPS_index_admission.R) ------ #
# ------------------------------------------------------------------------------------------------ #

df.readmit_grouper_input <- df.readmit_stays %>%
    rename(Index_Date_RPLND = Date_RPLND) %>%
    left_join(df.clinical %>%
                  select(Last_Name, DOB, Date_RPLND, Age),
              by = c("Last_Name", "DOB", "Index_Date_RPLND" = "Date_RPLND")) %>%
    left_join(df.readmit_pdx %>%
                  select(Readmit_ID, Principal_Dx),
              by = "Readmit_ID") %>%
    left_join(df.readmit_pproc, by = "Readmit_ID") %>%
    left_join(df.readmit_sdx,   by = "Readmit_ID") %>%
    left_join(df.readmit_sproc, by = "Readmit_ID") %>%
    filter(!is.na(Principal_Dx)) %>%  # ungroupable without a diagnosis
    mutate(Date_RPLND     = as.Date(Readmit_Admit_Date),   # grouper functions read this field name
           Discharge_Date = as.Date(Readmit_Discharge_Date),
           LOS            = Readmit_LOS,
           # EDIT per your cohort if these assumptions don't apply
           Sex = 1L, Discharge_Status = 1L, Admission_Type = 3L,
           n_secondary_dx   = coalesce(n_secondary_dx, 0L),
           n_secondary_proc = coalesce(n_secondary_proc, 0L),
           Secondary_Dx   = ifelse(n_secondary_dx == 0,   list(character(0)), Secondary_Dx),
           Secondary_Proc = ifelse(n_secondary_proc == 0, list(character(0)), Secondary_Proc),
           FFY = if_else(month(Discharge_Date) >= 10,
                         year(Discharge_Date) + 1,
                         year(Discharge_Date))) %>%
    left_join(df.grouper_version_lookup, by = "FFY")
# confirm every readmission FFY is covered
stopifnot(!any(is.na(df.readmit_grouper_input$Grouper_Version)))

readmit_legacy <- df.readmit_grouper_input %>% filter(Grouper_Version %in% legacy_versions)
readmit_modern <- df.readmit_grouper_input %>% filter(Grouper_Version %in% modern_versions)

df.readmit_msdrg_legacy <- if (nrow(readmit_legacy) > 0) {
    readmit_legacy %>% split(.$Grouper_Version) %>%
        imap_dfr(~ bind_cols(.x, map_dfr(run_msgmce_batch(.x, .y), parse_msgmce_upload_line)))
} else NULL

df.readmit_msdrg_modern <- if (nrow(readmit_modern) > 0) {
    readmit_modern %>% split(.$Grouper_Version) %>%
        imap_dfr(function(ver_df, ver) {
            add_version_classpath(ver)
            component <- .jnew(msdrg_component_class(ver), build_runtime_options(ver))
            ver_df %>% rowwise() %>%
                group_map(~ bind_cols(.x, run_grouper_rJava(.x, component, ver)), .keep = TRUE) %>%
                bind_rows()
        })
} else NULL

df.readmit_msdrg <- bind_rows(df.readmit_msdrg_legacy, df.readmit_msdrg_modern) %>%
    mutate(MS_DRG     = str_pad(str_extract(MS_DRG, "\\d+"), 3, pad = "0"),
           Grouped_OK = Grouper_Return_Code %in% c("00", "OK"))
# "INVALID_PDX" is more common here than for index admissions, since the principal diagnosis is
# inferred rather than clinician-coded - review any FALSE rows before pricing

# ------------------------------------------------------------------------------------------------ #
# -- Price each readmission via IPPS (same tables/formula as index admission) -------------------- #
# ------------------------------------------------------------------------------------------------ #

df.readmit_facility_cost <- df.readmit_msdrg %>%
    filter(Grouped_OK) %>%
    left_join(df.drg_weights, by = c("FFY", "MS_DRG")) %>%
    left_join(df.wage_index,  by = "FFY") %>%
    left_join(df.ipps_rates,  by = "FFY") %>%
    mutate(GAF               = Wage_Index ^ GAF_EXPONENT,
           Operating_Payment = (Labor_Amount * Wage_Index + Nonlabor_Amount) * DRG_Weight,
           Capital_Payment   = Capital_Federal_Rate * GAF * DRG_Weight,
           Readmit_Facility_Nominal = Operating_Payment + Capital_Payment,
           Year_Code = year(Readmit_Discharge_Date)) %>%
    left_join(df.BEA_adj2025 %>%
                  select(Year_Code, GDP_2025_multiplier),
              by = "Year_Code") %>%
    mutate(Readmit_Facility_2025 = Readmit_Facility_Nominal * GDP_2025_multiplier)

if (flag.save.table) {
    write.csv(df.readmit_facility_cost,
              file.path(f.tables, paste0("MS_DRG_costs_readmissions_", date.analysis, ".csv")),
              row.names = FALSE)
}

# ------------------------------------------------------------------------------------------------ #
# -- Aggregate to one row per patient per follow-up window --------------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# NOTE: MS-DRG payment is indivisible and cannot be pro-rated across a window boundary
# choose "contain (complete readmission within window) or "admit" (simply admits within the window)

window_rule <- "contain"

readmit_cost_in_window <- function(upper, label) {
    keep <- switch(window_rule,
                    "admit"   = df.readmit_facility_cost %>%
                       filter(Readmit_Admit_Day >= 0, Readmit_Admit_Day <= upper),
                    "contain" = df.readmit_facility_cost %>%
                       filter(Readmit_Admit_Day >= 0, Readmit_Discharge_Day <= upper),
                    stop("window_rule must be 'admit' or 'contain'"))
    keep %>%
        group_by(PMRN) %>%
        summarize(Readmit_Facility_2025 = sum(Readmit_Facility_2025, na.rm = TRUE),
                  N_Readmissions_Costed = n(),
                  .groups = "drop") %>%
        mutate(window = label)
}

# EDIT: define one call per follow-up window used in your analysis, e.g.:
df.readmit_cost_by_window <- bind_rows(
    readmit_cost_in_window(30, "0 to 30d post-op"),
    readmit_cost_in_window(90, "0 to 90d post-op")
)

# expand so patients with no readmission contribute an explicit $0, not a missing row
df.readmit_cost_by_window <- df.clinical %>%
    distinct(PMRN) %>%
    tidyr::crossing(window = unique(df.readmit_cost_by_window$window)) %>%
    left_join(df.readmit_cost_by_window, by = c("window", "PMRN")) %>%
    mutate(across(c(Readmit_Facility_2025, N_Readmissions_Costed), ~ coalesce(., 0)))

if (flag.save.table) {
    write.csv(df.readmit_cost_by_window,
              file.path(f.tables,
                        paste0("MS_DRG_costs_readmissions_by_window_", date.analysis, ".csv")),
              row.names = FALSE)
}

# ------------------------------------------------------------------------------------------------ #
# -- Script End ---------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #