# ################################################################################################ #
# ## Facility Costs, Index Admission: MS-DRG Assignment + IPPS Pricing ## ------------------------ #
# ################################################################################################ #

# Requires: df.clinical, df.raw_codes (with ICD_10_PCS_<n> columns), df.BEA_adj2025, FACILITY_CCN,
# and folder/flag variables defined in the main script.

# Two stages:
#   Stage A - Assign each index admission an MS-DRG using the CMS Grouper software (version-
#             specific; see main script for download instructions)
#   Stage B - Price the assigned MS-DRG using the CMS Inpatient Prospective Payment System (IPPS)
#             base formula: operating + capital payment, by federal fiscal year (FFY) of discharge

# SCOPE: this reproduces the base IPPS prospective payment only. It excludes IME (teaching), DSH
# (disproportionate share), outlier payments, and new-technology add-ons, and so understates actual
# realized Medicare payment at a teaching hospital - it is a standardized, reproducible benchmark
# rather than a claim to actual reimbursement. Please interpret results with this understanding.

# ################################################################################################ #
# -- STAGE A: MS-DRG assignment ------------------------------------------------------------------ #
# ################################################################################################ #

# ------------------------------------------------------------------------------------------------ #
# -- Identify principal/secondary diagnoses and procedures, index admission ---------------------- #
# ------------------------------------------------------------------------------------------------ #

# Principal diagnosis: the ICD-10-CM code linked to the lowest-ranked (highest-priority) ICD-10-PCS
# code closest to the surgery date (If data already flags primary code or is given in long form,
# this is no longer required. Secondary diagnoses/procedures: all other codes billed within
# the index length of stay (LOS, inclusive of discharge day).

df.principal_dx <- df.ICD10PCS_codes %>%
    filter(original_ICD_10_PCS_rank == 1) %>%
    mutate(days_from_surgery = abs(as.numeric(difftime(Date_Code, Date_Surgery, units = "days")))) %>%
    group_by(PMRN, Last_Name, DOB, Date_Surgery) %>%
    slice_min(days_from_surgery, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(PMRN, Last_Name, DOB, Date_Surgery,
           Principal_Dx = ICD_CM_Code,
           Principal_Dx_Date = Date_Code)

df.LOS_lookup <- df.clinical %>% distinct(PMRN, Last_Name, DOB, Date_Surgery, LOS)

df.admission_codes <- df.raw_codes %>%
    select(PMRN, Last_Name, DOB, Date_Surgery, Date_Code, ICD_CM_Code) %>%
    distinct() %>%
    left_join(df.LOS_lookup, by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    mutate(days_from_surgery = as.numeric(difftime(Date_Code, Date_Surgery, units = "days"))) %>%
    filter(days_from_surgery >= 0, days_from_surgery <= LOS)

df.admission_codes_pcs <- df.ICD10PCS_codes %>%
    select(PMRN, Last_Name, DOB, Date_Surgery, Date_Code, original_ICD_10_PCS_rank, ICD_10_PCS) %>%
    distinct() %>%
    left_join(df.LOS_lookup, by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    mutate(days_from_surgery = as.numeric(difftime(Date_Code, Date_Surgery, units = "days"))) %>%
    filter(days_from_surgery >= 0, days_from_surgery <= LOS)

df.secondary_dx <- df.admission_codes %>%
    left_join(df.principal_dx %>% select(PMRN, Date_Surgery, Principal_Dx),
              by = c("PMRN", "Date_Surgery")) %>%
    filter(ICD_CM_Code != Principal_Dx) %>%
    distinct(PMRN, Last_Name, DOB, Date_Surgery, ICD_CM_Code) %>%
    group_by(PMRN, Last_Name, DOB, Date_Surgery) %>%
    summarize(Secondary_Dx = list(ICD_CM_Code), n_secondary_dx = n(), .groups = "drop") %>%
    right_join(df.principal_dx %>% select(PMRN, Last_Name, DOB, Date_Surgery),
               by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    mutate(n_secondary_dx = coalesce(n_secondary_dx, 0L),
           Secondary_Dx    = ifelse(n_secondary_dx == 0, list(character(0)), Secondary_Dx))

# principal procedure: lowest-ranked PCS code on the same date as the principal diagnosis; ties
# broken by clinical judgment where needed (edit tie_rank.temp for your own priority codes, if any)
df.principal_proc <- df.ICD10PCS_codes %>%
    group_by(PMRN, Last_Name, DOB, Date_Surgery) %>%
    filter(original_ICD_10_PCS_rank == min(original_ICD_10_PCS_rank)) %>%
    ungroup() %>%
    semi_join(df.principal_dx, by = c("PMRN", "Date_Surgery", "Date_Code" = "Principal_Dx_Date")) %>%
    distinct(PMRN, Last_Name, DOB, Date_Surgery, ICD_10_PCS) %>%
    arrange(PMRN, ICD_10_PCS) %>%
    group_by(PMRN, Last_Name, DOB, Date_Surgery) %>%
    slice(1) %>%
    ungroup() %>%
    rename(Principal_Proc = ICD_10_PCS)

df.secondary_proc <- df.admission_codes_pcs %>%
    left_join(df.principal_proc %>% select(PMRN, Date_Surgery, Principal_Proc),
              by = c("PMRN", "Date_Surgery")) %>%
    filter(ICD_10_PCS != Principal_Proc) %>%
    distinct(PMRN, Last_Name, DOB, Date_Surgery, ICD_10_PCS) %>%
    group_by(PMRN, Last_Name, DOB, Date_Surgery) %>%
    summarize(Secondary_Proc = list(ICD_10_PCS), n_secondary_proc = n(), .groups = "drop") %>%
    right_join(df.principal_proc %>% select(PMRN, Last_Name, DOB, Date_Surgery),
               by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    mutate(n_secondary_proc = coalesce(n_secondary_proc, 0L),
           Secondary_Proc   = ifelse(n_secondary_proc == 0, list(character(0)), Secondary_Proc))

# ------------------------------------------------------------------------------------------------ #
# -- Assemble grouper input, one row per admission ----------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

df.grouper_input <- df.principal_dx %>%
    distinct(PMRN, Last_Name, DOB, Date_Surgery) %>%
    # EDIT: code per your cohort if the following assumptions do not apply!
    mutate(Sex = 1L, Discharge_Status = 1L, Admission_Type = 3L) %>%
    left_join(df.clinical %>%
                  select(Last_Name, DOB, Date_Surgery, Age),
              by = c("Last_Name", "DOB", "Date_Surgery")) %>%
    left_join(df.principal_dx %>%
                  select(PMRN, Last_Name, DOB, Date_Surgery, Principal_Dx),
              by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    left_join(df.secondary_dx %>%
                  select(PMRN, Last_Name, DOB, Date_Surgery, Secondary_Dx, n_secondary_dx),
              by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    left_join(df.principal_proc %>%
                  select(PMRN, Last_Name, DOB, Date_Surgery, Principal_Proc),
              by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    left_join(df.secondary_proc %>%
                  select(PMRN, Last_Name, DOB, Date_Surgery, Secondary_Proc, n_secondary_proc),
              by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    left_join(df.LOS_lookup,
              by = c("PMRN", "Last_Name", "DOB", "Date_Surgery")) %>%
    mutate(Discharge_Date = as.Date(Date_Surgery) + LOS)

# Federal Fiscal Year (FFY = Oct 1 - Sep 30, named for the calendar year it ends in). ICD-10 MS-DRGs
# began with V33 = FFY2016 and increment by 1 each FFY. Extend/edit for your own study period.
df.grouper_version_lookup <- tibble(
    FFY              = ffy_needed,
    Grouper_Version  = paste0("V", 34 + (ffy_needed - 2017))  # V34 = FFY2017; adjust as needed
)

df.grouper_input <- df.grouper_input %>%
    mutate(FFY = if_else(month(Discharge_Date) >= 10,
                         year(Discharge_Date) + 1,
                         year(Discharge_Date))) %>%
    left_join(df.grouper_version_lookup, by = "FFY")

stopifnot(!any(is.na(df.grouper_input$Grouper_Version)), # every admission maps to a grouper version
          # grouper cap
          all(df.grouper_input$n_secondary_dx <= 24, df.grouper_input$n_secondary_proc <= 24))

# NO Java API exists for the oldest grouper versions (pre-V39, confirmed as of this writing) - route
# by version. Reassign legacy_versions/modern_versions to match whichever versions you downloaded.
legacy_versions <- c("V34", "V35", "V36", "V37", "V38")
modern_versions <- c("V39", "V40", "V41", "V42", "V43", "V44")

# ------------------------------------------------------------------------------------------------ #
# -- Legacy path (no Java API): MSGMCE batch-mode interface -------------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# MSGMCE ships as a Windows installer only. Once installed per-version (see main script), its real
# launcher (identify via any .bat referencing "msgmce.jar" + "Main", excluding an interactive-mode
# variant) forwards to: java -cp msgmce.jar com.mmm.his.msgmce.Main -i <input> -u <upload>
# It resolves msgmce.log and its rot_data/ reference table relative to the CURRENT working directory
# at runtime, and standard users typically cannot write to a Program Files install location - copy
# each version's install folder to a writable directory first, and call java directly (bypassing the
# .bat wrapper, which is prone to a cmd.exe argument-quoting bug) from that writable copy.

MSGMCE_INSTALL_DIR <- "C:/MSGMCE/"  # EDIT: writable per-version install root, subfolder by version

find_msgmce_paths <- function(ver) {
    dir <- file.path(MSGMCE_INSTALL_DIR, ver)
    java_exe <- list.files(dir, pattern = "^java\\.exe$",
                           recursive = TRUE, full.names = TRUE, ignore.case = TRUE)[1]
    jar_file <- list.files(dir, pattern = "^msgmce\\.jar$",
                           recursive = TRUE, full.names = TRUE, ignore.case = TRUE)[1]
    list(java = java_exe, jar = jar_file, run_dir = dirname(jar_file))
}
MSGMCE_PATHS <- setNames(map(legacy_versions, find_msgmce_paths), legacy_versions)

# 835-character fixed-width input record (per the MSGMCE Installation/User's Guide, Table 18)
pad_left  <- function(x, width) formatC(x, width = width, flag = "-")
pad_right <- function(x, width) formatC(x, width = width, flag = "0")
dx_field  <- function(code, poa = "Y") if (is.na(code) || code == "") strrep(" ", 8) else paste0(pad_left(toupper(gsub("\\.", "", code)), 7), poa)
proc_field <- function(code) if (is.na(code) || code == "") strrep(" ", 7) else pad_left(toupper(gsub("\\.", "", code)), 7)

build_msgmce_input_record <- function(row) {
    secondary_dx <- row$Secondary_Dx[[1]]; secondary_pr <- row$Secondary_Proc[[1]]
    paste0(
        pad_left(substr(row$Last_Name, 1, 31), 31), pad_left("", 13), pad_left("", 17),
        format(row$Date_Surgery, "%m/%d/%Y"), format(row$Discharge_Date, "%m/%d/%Y"),
        "01", "00", pad_right(as.character(row$LOS), 5), format(row$DOB, "%m/%d/%Y"),
        pad_right(as.character(row$Age), 3), "1", pad_left("", 7),
        dx_field(row$Principal_Dx, "Y"),
        paste0(c(vapply(secondary_dx, dx_field, character(1), poa = "Y"),
                 rep(strrep(" ", 8), 24 - length(secondary_dx))), collapse = ""),
        proc_field(row$Principal_Proc),
        paste0(c(vapply(secondary_pr, proc_field, character(1)),
                 rep(strrep(" ", 7), 24 - length(secondary_pr))), collapse = ""),
        pad_left("", 250), "Z", " ", pad_left("", 72), pad_left("", 25))
}

run_msgmce_batch <- function(ver_df, ver) {
    paths <- MSGMCE_PATHS[[ver]]
    records <- vapply(seq_len(nrow(ver_df)),
                      function(i) build_msgmce_input_record(ver_df[i, ]),
                      character(1))
    stopifnot(all(nchar(records) == 835))
    in_path <- tempfile(fileext = ".txt"); up_path <- tempfile(fileext = ".txt")
    con <- file(in_path, open = "wb"); writeLines(records, con, sep = "\r\n"); close(con)
    old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE); setwd(paths$run_dir)
    system2(paths$java,
            args = c("-Xms512m", "-Xmx1024m", "-cp", "msgmce.jar", "com.mmm.his.msgmce.Main",
                     "-i", shQuote(in_path), "-u", shQuote(up_path)), stdout = TRUE, stderr = TRUE)
    if (!file.exists(up_path)) stop(ver, ": grouper did not produce an upload file")
    readLines(up_path, warn = FALSE)
}

parse_msgmce_upload_line <- function(line) {
    tibble(MDC                 = trimws(substr(line, 843, 844)),
           MS_DRG              = trimws(substr(line, 845, 847)),
           Grouper_Return_Code = trimws(substr(line, 849, 850)))
}  # Return_Code "00" = OK, DRG assigned; anything else means the row should not be trusted

# ------------------------------------------------------------------------------------------------ #
# -- Modern path (Java API via rJava) ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #
# Builder pattern: RuntimeOptions -> MsdrgRuntimeOption -> MsdrgInput -> MsdrgClaim ->
# version-specific MsdrgComponent.process() -> claim.getOutput(). Package root differs by version:
# legacy-style (pre-2020 vintage) gov.agency.msdrg.model.*, modern gov.agency.msdrg.model.v2.* with
# transfer classes split into input/output subpackages. Confirm your versions' package layout
# against their own Java API Guide PDF (shipped inside the downloaded zip) before relying on this.

.jinit()

msdrg_class_path <- function(name, ver) {
    style <- if (as.numeric(sub("V", "", ver)) <= 39) "legacy" else "modern"
    root  <- if (style == "modern") "gov/agency/msdrg/model/v2" else "gov/agency/msdrg/model"
    switch(name,
           RuntimeOptions          = paste0(root, "/RuntimeOptions"),
           MsdrgRuntimeOption      = paste0(root, "/MsdrgRuntimeOption"),
           MsdrgOption             = paste0(root, "/MsdrgOption"),
           MsdrgHospitalStatusOptionFlag = paste0(root, "/enumeration/MsdrgHospitalStatusOptionFlag"),
           MsdrgAffectDrgOptionFlag = paste0(root, "/enumeration/MsdrgAffectDrgOptionFlag"),
           MarkingLogicTieBreaker  = paste0(root, "/enumeration/MarkingLogicTieBreaker"),
           MsdrgSex                = paste0(root, "/enumeration/MsdrgSex"),
           MsdrgDischargeStatus    = paste0(root, "/enumeration/MsdrgDischargeStatus"),
           MsdrgSeverity           = paste0(root, "/enumeration/MsdrgSeverity"),
           MsdrgGrouperReturnCode  = paste0(root, "/enumeration/MsdrgGrouperReturnCode"),
           MsdrgInputDxCode = if (style == "modern") paste0(root, "/transfer/input/MsdrgInputDxCode")  else paste0(root, "/transfer/MsdrgInputDxCode"),
           MsdrgInputPrCode = if (style == "modern") paste0(root, "/transfer/input/MsdrgInputPrCode")  else paste0(root, "/transfer/MsdrgInputPrCode"),
           MsdrgInput       = if (style == "modern") paste0(root, "/transfer/input/MsdrgInput")        else paste0(root, "/transfer/MsdrgInput"),
           MsdrgOutputData  = if (style == "modern") paste0(root, "/transfer/output/MsdrgOutputData")  else paste0(root, "/transfer/MsdrgOutputData"),
           MsdrgClaim       = paste0(root, "/transfer/MsdrgClaim"),
           MsdrgValue       = paste0(root, "/MsdrgValue"),
           stop("Unknown class name: ", name))
}

msdrg_component_class <- function(ver) {
    v <- as.numeric(sub("V", "", ver))
    gsub("\\.", "/", paste0("gov.agency.msdrg.v", v * 10, ".MsdrgComponent"))
}

add_version_classpath <- function(ver) {
    jars <- list.files(file.path(f.grouper, ver), pattern = "\\.jar$", recursive = TRUE, full.names = TRUE)
    jars <- jars[!grepl("-sources\\.jar$", jars, ignore.case = TRUE)]
    dep_jars <- list.files(f.grouper, pattern = "\\.jar$", full.names = TRUE, recursive = TRUE)
    dep_jars <- dep_jars[!grepl("-sources\\.jar$", dep_jars, ignore.case = TRUE)]
    invisible(lapply(c(jars, dep_jars), .jaddClassPath))
}  # note: .jaddClassPath() only appends within a session - restart R to swap a dependency version

get_enum <- function(class_path, field_name) .jfield(class_path, paste0("L", class_path, ";"), field_name)

build_runtime_options <- function(ver) {
    options <- .jnew(msdrg_class_path("RuntimeOptions", ver))
    .jcall(options, "V", "setPoaReportingExempt",
           get_enum(msdrg_class_path("MsdrgHospitalStatusOptionFlag", ver), "NON_EXEMPT"))
    .jcall(options, "V", "setComputeAffectDrg",
           get_enum(msdrg_class_path("MsdrgAffectDrgOptionFlag", ver), "COMPUTE"))
    .jcall(options, "V", "setMarkingLogicTieBreaker",
           get_enum(msdrg_class_path("MarkingLogicTieBreaker", ver), "CLINICAL_SIGNIFICANCE"))
    runtime_option <- .jnew(msdrg_class_path("MsdrgRuntimeOption", ver))
    option_key <- get_enum(msdrg_class_path("MsdrgOption", ver), "RUNTIME_OPTION_FLAGS")
    .jcall(runtime_option, "Ljava/lang/Object;", "put",
           .jcast(option_key, "java/lang/Object"), .jcast(options, "java/lang/Object"))
    runtime_option
}

norm_code <- function(x) toupper(trimws(gsub("\\.", "", x)))

build_msdrg_claim <- function(row, ver) {
    poa_default <- get_enum("com/mmm/his/cer/foundation/model/GfcPoa",
                            "Y")
    sex_enum <- get_enum(msdrg_class_path("MsdrgSex", ver),
                         if (row$Sex == 1) "MALE" else "FEMALE")
    discharge_status_enum <- get_enum(msdrg_class_path("MsdrgDischargeStatus", ver),
                                      "HOME_SELFCARE_ROUTINE")
    dx_class <- msdrg_class_path("MsdrgInputDxCode", ver)
    pr_class <- msdrg_class_path("MsdrgInputPrCode", ver)
    input_class <- msdrg_class_path("MsdrgInput", ver)

    principal_dx <- .jnew(dx_class, norm_code(row$Principal_Dx), poa_default)
    secondary_dx_list <- .jnew("java/util/ArrayList")
    for (code in row$Secondary_Dx[[1]])
        .jcall(secondary_dx_list, "Z", "add", .jcast(.jnew(dx_class, norm_code(code), poa_default),
                                                     "java/lang/Object"))

    all_procs <- c(row$Principal_Proc, row$Secondary_Proc[[1]])
    all_procs <- all_procs[!is.na(all_procs) & nzchar(trimws(all_procs))]
    proc_list <- .jnew("java/util/ArrayList")
    for (code in all_procs)
        .jcall(proc_list, "Z", "add", .jcast(.jnew(pr_class, norm_code(code)), "java/lang/Object"))

    builder_sig <- paste0("L", input_class, "$MsdrgInputBuilder;")
    builder <- .jcall(input_class, builder_sig, "builder")
    builder <- .jcall(builder, builder_sig, "withPrincipalDiagnosisCode", principal_dx)
    builder <- .jcall(builder, builder_sig, "withSecondaryDiagnosisCodes",
                      .jcast(secondary_dx_list, "java/util/List"))
    builder <- .jcall(builder, builder_sig, "withProcedureCodes",
                      .jcast(proc_list, "java/util/List"))
    builder <- .jcall(builder, builder_sig, "withAgeInYears", as.integer(row$Age))
    builder <- .jcall(builder, builder_sig, "withAgeDaysAdmit", 0L)
    builder <- .jcall(builder, builder_sig, "withAgeDaysDischarge", 0L)
    builder <- .jcall(builder, builder_sig, "withSex", sex_enum)
    builder <- .jcall(builder, builder_sig, "withDischargeStatus", discharge_status_enum)
    input <- .jcall(builder, paste0("L", input_class, ";"), "build")
    .jnew(msdrg_class_path("MsdrgClaim", ver), input)
}

run_grouper_rJava <- function(row, component, ver) {
    claim <- build_msdrg_claim(row, ver)
    .jcall(component, "V", "process", claim)
    output_opt <- .jcall(claim, "Ljava/util/Optional;", "getOutput")
    if (!.jcall(output_opt, "Z", "isPresent"))
        return(tibble(MS_DRG = NA_character_, MDC = NA_character_,
                      Grouper_Return_Code = "NO_OUTPUT"))
    output <- .jcast(.jcall(output_opt, "Ljava/lang/Object;", "get"),
                     msdrg_class_path("MsdrgOutputData", ver))
    msdrg_value_sig <- paste0("L", msdrg_class_path("MsdrgValue", ver), ";")
    final_drg <- .jcall(output, msdrg_value_sig, "getFinalDrg")
    final_mdc <- .jcall(output, msdrg_value_sig, "getFinalMdc")
    final_grc <- .jcall(output, paste0("L", msdrg_class_path("MsdrgGrouperReturnCode",
                                                             ver), ";"), "getFinalGrc")
    tibble(MS_DRG = as.character(.jsimplify(.jcall(final_drg, "Ljava/lang/Object;", "getValue"))),
           MDC    = as.character(.jsimplify(.jcall(final_mdc, "Ljava/lang/Object;", "getValue"))),
           Grouper_Return_Code = .jcall(final_grc, "Ljava/lang/String;", "name"))
}

# ------------------------------------------------------------------------------------------------ #
# -- Run the grouper on all admissions ----------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

grouper_legacy <- df.grouper_input %>% filter(Grouper_Version %in% legacy_versions)
grouper_modern <- df.grouper_input %>% filter(Grouper_Version %in% modern_versions)

df.msdrg_legacy <- if (nrow(grouper_legacy) > 0) {
    grouper_legacy %>% split(.$Grouper_Version) %>%
        imap_dfr(~ bind_cols(.x, map_dfr(run_msgmce_batch(.x, .y), parse_msgmce_upload_line)))
} else NULL

df.msdrg_modern <- if (nrow(grouper_modern) > 0) {
    grouper_modern %>% split(.$Grouper_Version) %>%
        imap_dfr(function(ver_df, ver) {
            add_version_classpath(ver)
            component <- .jnew(msdrg_component_class(ver), build_runtime_options(ver))
            ver_df %>% rowwise() %>%
                group_map(~ bind_cols(.x, run_grouper_rJava(.x, component, ver)), .keep = TRUE) %>%
                bind_rows()
        })
} else NULL

df.msdrg_index <- bind_rows(df.msdrg_legacy, df.msdrg_modern) %>%
    mutate(MS_DRG     = str_pad(str_extract(MS_DRG, "\\d+"), 3, pad = "0"),
           Grouped_OK = Grouper_Return_Code %in% c("00", "OK"))

stopifnot(all(df.msdrg_index$Grouped_OK))  # investigate any FALSE before pricing

# ################################################################################################ #
# -- STAGE B: IPPS facility cost ----------------------------------------------------------------- #
# ################################################################################################ #

# ------------------------------------------------------------------------------------------------ #
# -- Download IPPS rate/weight/wage-index files, per FFY ----------------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# From each fiscal year's IPPS Final Rule page (https://www.cms.gov/medicare/payment/
# prospective-payment-systems/acute-inpatient-pps/acute-inpatient-files-download):
#   Table 5  - MS-DRG relative weighting factors                    -> DRG_Weight
#   Table 2  - hospital-specific wage index, by CCN                 -> Wage_Index
#   Table 1A - national standardized amounts (labor/nonlabor split) -> Labor_Amount, Nonlabor_Amount
#   Table 1D - capital federal payment rate                          -> Capital_Federal_Rate

if (flag.downloadgov.DRG_IPPS) {
    parent <- "https://www.cms.gov/medicare/payment/prospective-payment-systems/acute-inpatient-pps/acute-inpatient-files-download"
    options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    links <- read_html(parent) %>% html_elements("a")
    df.year_pages <- tibble(text = html_text2(links), href = html_attr(links, "href")) %>%
        filter(grepl("final-rule", href, ignore.case = TRUE)) %>%
        mutate(href = if_else(grepl("^http", href), href, paste0("https://www.cms.gov", href)),
               FFY  = as.integer(str_extract(text, "20\\d{2}"))) %>%
        filter(FFY %in% ffy_needed) %>% distinct(FFY, .keep_all = TRUE)

    df.ipps_urls <- map2_dfr(df.year_pages$FFY, df.year_pages$href, function(ffy, url) {
        a <- read_html(url) %>% html_elements("a")
        tibble(FFY = ffy, text = html_text2(a), href = html_attr(a, "href")) %>%
            filter(grepl("\\.zip$", href, ignore.case = TRUE)) %>%
            mutate(href = if_else(grepl("^http", href), href, paste0("https://www.cms.gov", href)),
                   kind = case_when(grepl("table\\s*5\\b", text, ignore.case = TRUE) ~ "table5",
                                     grepl("table\\s*1a", text, ignore.case = TRUE)   ~ "rates",
                                     grepl("wage index table", text, ignore.case = TRUE) ~ "wageidx",
                                     TRUE ~ NA_character_)) %>%
            filter(!is.na(kind))
    })

    pwalk(list(df.ipps_urls$kind, df.ipps_urls$FFY, df.ipps_urls$href), function(kind, ffy, href) {
        dest <- file.path(f.ipps, sprintf("%s_%d.zip", kind, ffy))
        out  <- file.path(f.ipps, sprintf("%s_%d", kind, ffy))
        download.file(href, destfile = dest, mode = "wb", method = "libcurl", quiet = TRUE)
        unzip(dest, exdir = out)
    })
}

# ------------------------------------------------------------------------------------------------ #
# -- Read Table 5 (DRG relative weights) --------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# Layout is not stable across years; resolve the Final Rule (not Correction Notice) sheet and the
# weight column (single "Weights" column pre-2024, split "Before Cap"/"10% Cap Applied" from 2024).

read_table5 <- function(ffy) {
    dir <- file.path(f.ipps, paste0("table5_", ffy))
    f <- list.files(dir, pattern = "\\.xlsx?$", recursive = TRUE, full.names = TRUE)[1]
    sheets <- excel_sheets(f)
    keep <- sheets[!grepl("\\bCN\\b|Correction", sheets, ignore.case = TRUE)]
    if (length(keep) > 1) keep <- keep[grepl("\\bFR\\b", keep)][1] %||% keep[1]
    raw <- read_excel(f, sheet = keep, col_names = FALSE, .name_repair = "minimal")
    hdr <- as.character(unlist(raw[2, ])); dat <- raw[-(1:2), ]
    names(dat) <- make.unique(ifelse(is.na(hdr), paste0("X", seq_along(hdr)), trimws(hdr)))
    drg_col <- names(dat)[grepl("^MS-DRG$", names(dat))][1]
    wt_cols <- names(dat)[grepl("^Weights", names(dat))]
    wt_col  <- if (length(wt_cols) == 1) wt_cols else wt_cols[grepl("Cap Applied", wt_cols)][1]
    dat %>%
        transmute(FFY = as.integer(ffy),
                   MS_DRG     = str_pad(str_extract(.data[[drg_col]], "\\d+"), 3, pad = "0"),
                   DRG_Title  = .data[["MS-DRG Title"]],
                   DRG_Weight = suppressWarnings(as.numeric(.data[[wt_col]]))) %>%
        filter(!is.na(MS_DRG), !is.na(DRG_Weight))
}
df.drg_weights <- map_dfr(ffy_needed, read_table5)

# ------------------------------------------------------------------------------------------------ #
# -- Read Table 2 (hospital-specific wage index) ------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

read_wage_index <- function(ffy) {
    dir <- file.path(f.ipps, paste0("wageidx_", ffy))
    f <- list.files(dir, pattern = "\\.xlsx?$", recursive = TRUE, full.names = TRUE)
    f <- f[!grepl("IFC", f, ignore.case = TRUE)][1]
    sheets <- excel_sheets(f); s <- sheets[grepl("Table 2", sheets, ignore.case = TRUE)]
    s <- s[!grepl("\\bCN\\b|\\bCA\\b|Correction", s, ignore.case = TRUE)]
    if (length(s) > 1) s <- s[grepl("\\bFR\\b", s)][1]
    raw <- read_excel(f, sheet = s, col_names = FALSE, .name_repair = "minimal")
    hdr <- trimws(gsub("[\r\n]+", " ", as.character(unlist(raw[2, ])))); dat <- raw[-(1:2), ]
    names(dat) <- make.unique(ifelse(is.na(hdr), paste0("X", seq_along(hdr)), hdr))
    # require CURRENT FFY in the header - from FY2020 on, Table 2 also carries a PRIOR-year column
    wi_col  <- names(dat)[grepl("^3[,0-9 ]*\\s*FY\\s*",
                                names(dat)) & grepl(as.character(ffy), names(dat)) &
                              grepl("Wage Index", names(dat), ignore.case = TRUE)]
    ccn_col <- names(dat)[grepl("CCN", names(dat))][1]
    dat <- dat[!is.na(dat[[1]]), ]
    idx <- which(trimws(dat[[ccn_col]]) == as.character(FACILITY_CCN))
    stopifnot(length(idx) == 1)
    tibble(FFY = as.integer(ffy), Wage_Index = as.numeric(dat[[wi_col]][idx]))
}
df.wage_index <- map_dfr(ffy_needed, read_wage_index)

# ------------------------------------------------------------------------------------------------ #
# -- Read Table 1A/1D (standardized amounts, capital rate) --------------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# Table 1A gives labor/nonlabor dollar amounts directly (already split by the hospital's applicable
# labor-related share), for the full-update column (submitted quality data + meaningful EHR user).

read_ipps_rates <- function(ffy) {
    dir <- file.path(f.ipps, paste0("rates_", ffy))
    f <- list.files(dir, pattern = "\\.xlsx?$", recursive = TRUE, full.names = TRUE)[1]
    sheets <- excel_sheets(f)
    keep <- sheets[!grepl("\\bCN\\b|\\bCA\\b|CAA|IFC|Correct", sheets, ignore.case = TRUE)][1]
    raw <- as.data.frame(read_excel(f, sheet = keep, col_names = FALSE, .name_repair = "minimal"))

    find_cell <- function(pattern) {
        for (i in seq_len(nrow(raw))) for (j in seq_len(ncol(raw)))
            if (!is.na(raw[i, j]) && grepl(pattern, raw[i, j],
                                           ignore.case = TRUE)) return(c(row = i, col = j))
        stop("pattern not found: ", pattern)
    }
    a_start <- find_cell("TABLE 1A")["row"]; b_start <- find_cell("TABLE 1B")["row"]
    block <- raw[a_start:(b_start - 1), , drop = FALSE]
    hdr <- NULL
    for (i in seq_len(nrow(block))) for (j in seq_len(ncol(block))) {
        v <- block[i, j]
        if (!is.na(v) && grepl("Submitted Quality Data", v, ignore.case = TRUE) &&
            grepl("Meaningful EHR User", v,
                  ignore.case = TRUE) && !grepl("NOT", v, ignore.case = TRUE)) {
            hdr <- c(row = i, col = j); break
        }
    }
    labor    <- suppressWarnings(as.numeric(block[hdr["row"] + 2, hdr["col"]]))
    nonlabor <- suppressWarnings(as.numeric(block[hdr["row"] + 2, hdr["col"] + 1]))

    d_start <- find_cell("TABLE 1D")["row"]; e_start <- find_cell("TABLE 1E")["row"]
    d_block <- raw[d_start:(e_start - 1), , drop = FALSE]
    nat <- NULL
    for (i in seq_len(nrow(d_block))) for (j in seq_len(ncol(d_block)))
        if (!is.na(d_block[i, j]) && grepl("^National$",
                                           trimws(d_block[i, j]))) { nat <- c(row = i,
                                                                              col = j); break }
    capital <- suppressWarnings(as.numeric(d_block[nat["row"], nat["col"] + 1]))
    if (is.na(capital)) capital <- suppressWarnings(as.numeric(d_block[nat["row"] + 1,
                                                                       nat["col"] + 1]))

    tibble(FFY = ffy,
           Labor_Amount = labor,
           Nonlabor_Amount = nonlabor,
           Capital_Federal_Rate = capital)
}
df.ipps_rates <- map_dfr(ffy_needed, read_ipps_rates)

GAF_EXPONENT <- 0.6822 # capital geographic adjustment factor exponent (GAF = wage_index ^ exponent)

# ------------------------------------------------------------------------------------------------ #
# -- Compute and inflate facility cost per index admission --------------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# operating = (labor_amount x wage_index + nonlabor_amount) x DRG weight
# capital   = capital_federal_rate x GAF x DRG weight

df.facility_cost_index <- df.msdrg_index %>%
    filter(Grouped_OK) %>%
    select(PMRN, Last_Name, DOB, Date_Surgery, FFY, MS_DRG) %>%
    left_join(df.drg_weights, by = c("FFY", "MS_DRG")) %>%
    left_join(df.wage_index,  by = "FFY") %>%
    left_join(df.ipps_rates,  by = "FFY") %>%
    mutate(GAF               = Wage_Index ^ GAF_EXPONENT,
           Operating_Payment = (Labor_Amount * Wage_Index + Nonlabor_Amount) * DRG_Weight,
           Capital_Payment   = Capital_Federal_Rate * GAF * DRG_Weight,
           Facility_Payment_Nominal = Operating_Payment + Capital_Payment,
           Year_Code = year(Date_Surgery)) %>%
    left_join(df.BEA_adj2025 %>% select(Year_Code, GDP_2025_multiplier), by = "Year_Code") %>%
    mutate(Facility_Payment_2025 = Facility_Payment_Nominal * GDP_2025_multiplier)

stopifnot(!any(is.na(df.facility_cost_index$Facility_Payment_2025)))  # every admission should price

if (flag.save.table) {
    write.csv(df.facility_cost_index,
              file.path(f.tables, paste0("MS_DRG_costs_index_admission_", date.analysis, ".csv")),
              row.names = FALSE)
}

# ------------------------------------------------------------------------------------------------ #
# -- Script End ---------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #