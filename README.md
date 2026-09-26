# Robotic RPLND: Real-World Cost Analysis

This repository contains the key processing scripts for our real-world cost analysis of Robotic Retroperitoneal Lymph Node Dissection (RPLND).

We include the cost estimation framework and implementation of a custom grouper for the Medicare Severity Diagnosis-Related Group (MS-DRG) and
cost assignments using publicly available government resources obtained from the Centers for Medicare & Medicaid Services (CMS) reimbursement
schedules for the Inpatient Prospective Payment System (IPPS) and Physician Fee Schedule (PFS). Descriptions are given for how to organize
the reference files to be called by the included R scripts.

## Project Structure

The analysis is broken down sequentially across the following R scripts:
* **`00_main.R`**: The master script to run the entire pipeline.
* **`01_CPT_costs_nonanesthesia.R`**: Processes direct surgical and institutional costs via non-anesthesia CPT codes.
* **`02_CPT_costs_anesthesia.R`**: Analyzes anesthesia-specific billing and time-based costs.
* **`03_MSDRG_IPPS_index_admission.R`**: Evaluates inpatient costs for the index admission using CMS MS-DRG weights.
* **`04_MSDRG_IPPS_readmissions.R`**: Captures and calculates healthcare utilization and costs from post-operative readmissions.
* **`05_CostTable_Figure.R`**: Generates the final aggregate cost tables and data visualizations.

### Prerequisites
You will need the CMS files as described in both script comments as well as the manuscripts methos and supplementary information.

### Execution
1. Clone this repository or download the ZIP file.
2. Ensure your raw CMS data files are structured within a `Data_raw/` directory as described in the script comments.
3. Open and execute `00_main.R` to run the full end-to-end analysis, follow commentary regarding any remaining installs and adapt as needed.
