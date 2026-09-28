# ################################################################################################ #
# ## Robotic vs Open RPLND: Real-World Medicare Cost Analysis ## --------------------------------- #
# ## Christopher J. Magnani, MS MS MPhil ## ------------------------------------------------------ #
# ## Main Script ## ------------------------------------------------------------------------------ #
# ################################################################################################ #

# We encourage you to adapt this code for your own project; however, please cite our paper:

#   Magnani CJ, Ramos F, Qian Z, et al. Robotic-Assisted vs. Open Retroperitoneal Lymph Node
#     Dissection for Testicular Cancer: Real-World Evidence of Lower Costs and Health System
#     Utilization. [Journal]. [Year];[Vol(Issue)]:[Pages]. doi:[DOI] PMID: .

# ------------------------------------------------------------------------------------------------ #
# -- Overview ------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #

# Prices each patient's actual billing record against publicly available Medicare fee schedules to
# estimate real-world procedural cost, in constant 2025 USD:
#   - Professional (physician) fees  -> CMS Physician Fee Schedule (PFS), by RVU
#   - Anesthesia                     -> CMS Anesthesia Base Units x locality conversion factor
#   - Facility / inpatient           -> CMS Inpatient Prospective Payment System (IPPS), priced via
#                                        MS-DRG assignment using the CMS Grouper software
#
# Companion scripts (source()'d below):
#   01_CPT_costs_nonanesthesia.R    - professional fee costs, non-anesthesia CPT codes
#   02_CPT_costs_anesthesia.R       - professional fee costs, anesthesia CPT codes
#   03_MSDRG_IPPS_index_admission.R - MS-DRG assignment + IPPS facility cost, index admission
#   04_MSDRG_IPPS_readmissions.R    - MS-DRG assignment + IPPS facility cost, readmissions
#   05_CostTable_Figure.R           - Combine costs and generate cost/utilization table/figure

# ------------------------------------------------------------------------------------------------ #
# -- Required input data ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

# Patient-level data cleaning is not included in this repository given confidentiality concerns for
# protected health information, in brief study cohort data should be formatted as follows:

# df.clinical  - one row per patient
#   Required: PMRN, Last_Name, DOB, Date_Surgery (this corresponds to index surgery or date),
#             Robotic (0/1 - can be substituted with comparison variable), Age, LOS
#   (plus any additional covariates used by downstream analysis scripts)

# df.raw_codes - one row per billed code per patient (long format)
#   Required: PMRN, Last_Name, DOB, Date_Surgery, Date_Code, CPT_Code, CPT_Code_original,
#             ICD_CM_Code, Department (optional; used to flag ER/IR encounters)
#   Plus one or more ICD_10_PCS_<n> columns (inpatient procedure codes, ranked by billing priority)
#   ICD_10_PCS could be given their own rows in long format with skips a step, in which case a
#   a variable to differentiate CPT/ICD-PCS would be helpful though the format/match to PFS ahould
#   be sufficient to differentiate

# CPT_Code_original preserves the as-billed code; CPT_Code may reflect manual corrections (e.g.,
# placeholder/unlisted codes crosswalked to a comparable listed code) - apply these to df.raw_codes
# and therefore allows an audit-trail column before running this pipeline.

# df.BEA_adj2025 - one row per calendar year (Year_Code), columns (2025 version provided in github):
#   GDP_2025_multiplier    - BEA "Implicit Price Deflator for GDP" (Table 1.1.9), rebased to 2025
#                            https://apps.bea.gov/iTable  (Table 1.1.9)
#   PFS_Conversion_Factor  - Medicare PFS Conversion Factor for that year
#                            https://www.ama-assn.org/system/files/cf-history.pdf

# ------------------------------------------------------------------------------------------------ #
# -- Folder structure (Data_raw/) ---------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

#   Data_raw/
#     # Requires download from CMS (see manuscript methods, supplementary information, and code comments
#     CMS_PFS/<year>/             - downloaded CMS Physician Fee Schedule RVU files
#     CMS_IPPS/<kind>_<FFY>/      - downloaded CMS IPPS rate/weight/wage-index files (kind = table5,
#                                   wageidx, rates)
#     CMS_Grouper/<version>/      - downloaded CMS MS-DRG grouper software (not included;
#                                   PHI-adjacent executable, download per instructions below)
#     # Included in this repository
#     cf_history_AMA_20260805.pdf - downloaded source for PFS conversion factors
#     inflation_adj2025.csv       - source for df.BEA_adj2025 above
#     BEA_GDPImplicitPriceDeflator_20260804_full.csv - downloaded complete BEA GPD price deflator
#     BEA_GDPImplicitPriceDeflator_20260804_2025dollars.xlsx - re-formatted BEA GPD price deflator

# ------------------------------------------------------------------------------------------------ #
# -- Setup --------------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

# folders for input data files and government references (edit to match your local structure)
f.data    <- "Data_raw/"
f.rvu     <- file.path(f.data, "CMS_PFS")
f.ipps    <- file.path(f.data, "CMS_IPPS")
f.grouper <- file.path(f.data, "CMS_Grouper")
# folders for results destinations
f.results <- "Results/"
f.figures <- file.path(f.results, "Figures")
f.tables  <- file.path(f.results, "Tables")

# analysis parameters (edit to match your cohort)
date.analysis <- format(Sys.Date(), "%Y%m%d") # can alternatively set as a constant
years_needed  <- 2015:2026     # calendar years of CPT/PFS data required
ffy_needed    <- 2016:2026     # federal fiscal years (FFY) of IPPS data required

# facility identifiers - EDIT THESE for your institution
FACILITY_CCN             <- "XXXXXX"  # 6-digit CMS Certification Number (wage-index lookup)
FACILITY_LOCALITY_MATCH  <- "xxxxx"  # substring matching your CMS PFS/anesthesia locality name

# toggles
flag.downloadgov.PFS      <- TRUE
flag.downloadgov.DRG_IPPS <- TRUE
flag.save.table           <- TRUE

# packages
library(tidyverse)
library(readxl)
library(lubridate)
library(utils)
library(tibble)
library(rvest)
library(rJava)
library(emmeans)
library(tableone)
library(ggplot2)
library(ggsci)
library(patchwork)
library(scales)

# ------------------------------------------------------------------------------------------------ #
# -- CMS MS-DRG Grouper software (manual download step) ------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #
# CMS distributes grouper software per version (one version per federal fiscal year, roughly V34 =
# FFY2017 through the current year). Two families, requiring different integration approaches:

#   Legacy versions (no Java API; Windows GUI installer "MSGMCE"):
#     Download page: https://www.cms.gov/medicare/payment/prospective-payment-systems/
#                     acute-inpatient-pps/ms-drg-classifications-and-software
#     Extract to:    Data_raw/CMS_Grouper/<Version>/
#     Install the extracted MSGMCEInstaller.exe locally, one version per subfolder to avoid the
#     installer overwriting a prior version.
# See 03_MSDRG_IPPS_index_admission.R for the batch-mode command-line interface used to call it
#     programmatically once installed.

#   Modern versions (Java API, called via rJava):
#     Download the "Java Source Code and Reference Implementation Binaries" zip from the same page
#     Extract to: Data_raw/CMS_Grouper/<Version>/
#     Also required (not bundled by CMS):
#       - gfc-base-api    (Solventum/3M open source; compile from source)
#                         https://github.com/solventum-oss/GFC-Grouper-Foundation-Classes
#       - protobuf-java   (Maven Central; version must match the grouper version's API guide)
#       - slf4j-api       (Maven Central)
# See 03_MSDRG_IPPS_index_admission.R for full version-handling and class-path logic.

# NOTE ON MISSING VERSIONS: if a grouper version for a required FFY cannot be located (e.g., very
# old versions are sometimes withdrawn from CMS's site), the nearest available adjacent version can
# be substituted; validate this decision against a small manual cross-check using the CMS MS-DRG
# Definitions Manual for the missing version before relying on it for a full cohort.

# ------------------------------------------------------------------------------------------------ #
# -- Run pipeline -------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

source("01_CPT_costs_nonanesthesia.R")
source("02_CPT_costs_anesthesia.R")
source("03_MSDRG_IPPS_index_admission.R")
source("04_MSDRG_IPPS_readmissions.R")
# combine professional (df.PFS_costs_nonanes, df.PFS_costs_anes) and facility
# (df.facility_cost_index, df.facility_cost_readmit) tables downstream as needed for your
# window-level aggregation and analysis scripts:
source("05_CostTable_Figure.R")

# Details on R session
sessionInfo()

# ------------------------------------------------------------------------------------------------ #
# -- Script End ---------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #