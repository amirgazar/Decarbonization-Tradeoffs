# Reproducing the analysis

## Setup

Use R 4.4.2 or a compatible version and Python 3. Each script lists its package imports. The main R packages include data.table, dplyr, tidyr, readxl, lubridate, zoo, sf, fredr, httr, htmltools, jsonlite and digest. Cluster controls also require ssh. Python figures use numpy, pandas, scipy, matplotlib and openpyxl. Additional preparation scripts can require other packages. This repository does not lock package versions.

Open R from the repository root and set:

```r
Sys.setenv(PHASED_R1_ROOT = normalizePath(getwd()))
Sys.setenv(PHASED_DATA_ROOT = normalizePath(getwd()))
```

`PHASED_DATA_ROOT` may instead point to a separate data folder with the same numbered directory structure. Run Python scripts from the repository root or set `PHASED_R1_ROOT` in their environment. Set `PHASED_PYTHON_R1` if the required Python is not on your PATH.

## 1. Prepare inputs

The numbered preparation scripts create capacity, demand, generation, imports and randomization tables. Supply the external source tables before running them. Keep the same random columns across pathways for paired comparisons.

Required dispatch inputs include `Hourly_Installed_Capacity.csv`, facility tables, `Fossil_Fuel_Generation_Emissions.csv`, `Fossil_Fuel_hr_maxmin.csv`, wind and solar capacity-factor tables, `Imports_CF.csv`, `demand_data.csv`, `Random_Sequence.csv`, and `Operating_emission_models_R1.rds`. The dispatch model lists their exact relative paths. The large capacity and generation tables and saved R sessions are not included in the update.

The calibration script in `Support R1` fits the operating-emissions artifact using historical plant observations. Use its `ROOT OUTPUT` arguments to regenerate the artifact. The saved artifact contains a checksum of its helper source and fleet input. Recalibrate after changing calculations or the fleet. The supplied artifact has unchanged fitted coefficients and checksums matching the published source.

Optional historical data helpers accept these environment settings instead of personal paths: `PHASED_STATES_HISTORICAL_DATA`, `PHASED_SIMULATED_DATA_FACILITY_LEVEL`, `PHASED_SIMULATED_DATA_PARQUET`, `PHASED_ARC_SSH_FOSSIL_FUELS_USA`, `PHASED_AUTOMATION`, and `PHASED_FOSSIL_FUELS_RDS`. Supply the relevant input or output location before using those helpers.

## 2. Run dispatch

For a local check, open `2 Generation Expansion Model/5 Dispatch Curve/1 Run local review_R1.R`. Select `ACTION`, `HOURS`, `START_YEAR` and `PATHWAYS`. The default is a 2,000-hour test in 2050, using simulation 1. Set `PHASED_RANDOM_FILE` to select the saved random table. A partial test cannot supply annual production cost inputs.

For a full local run, use `2 Generation Expansion Model/7 Validation R1/run_dispatch_review_R1.R` with arguments:

```text
ROOT DATA_ROOT INPUT_EXTRACT NEW_OUTPUT 0 SIM_IDS fresh 2025 A,B1,B2,B3,C1,C2,C3,D
```

`SIM_IDS` is a comma-separated list. `INPUT_EXTRACT` holds the selected original random columns. `HOURS=0` runs the full supplied horizon. The runner keeps shared inputs in memory, writes each partition and checks the energy balance. Full runs require substantial memory, time and disk space.

For the cluster workflow, open `2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/1 ARC Control_R1.R`. Set the host, project, account, container, resources and a unique run name for your installation. The default simulation scope is 1 through 1000. Its menu separates generation, upload, submission, monitoring and download. Preparing this repository does not submit jobs.

Use the summary script in `Support R1` to combine full runs. Its settings file defines the run directories and simulation scope. The completed Final folder must contain `Yearly_Results.csv`, `Yearly_Facility_Level_Results.csv`, `Yearly_Results_Shortages.csv`, `Coverage_and_accounting.csv` and `SUMMARY_COMPLETE.txt`.

## 3. Calculate costs

```r
Sys.setenv(PHASED_FINAL_R1 = "/path/to/completed/Final")
source("7 Reproduction Information Document/Cost production audit R1/1 Run full cost pipeline_R1.R")
```

The runner requires all 1,000 simulations, eight dispatch pathways and years 2025 through 2050. It checks coverage and annual accounting before executing the 13 cost components at discount rates 1.5%, 2% and 2.5%. B3 has two cost presentations, giving nine cost pathways.

Run cost components through this runner. Their `__PROJECT_ROOT__` paths are templates that the runner binds to the selected inputs and a new output folder. Do not source those components directly.

Supply `4 External Data/NREL ATB/ATBe_2024.csv`, the CPI RDS in `3 Total Costs/0 Pilot Cost Sanity Checks R1/Inputs R1`, the county lookup in the cost runner's `Inputs R1`, and the other external tables referenced by the components. The small validated AP4 coefficient tables are included under `AP4_ARC/outputs/AP4_native_20260920_01/validated_tables`. The runner creates the numeric, inflation-adjusted ATB table from the source input. AP4 model regeneration requires its external model inputs; set `PHASED_AP4_ARC` before using its separate cluster controller.

The default air valuation is `AP4_hybrid`. `PHASED_AIR_MODEL_R1` can select a supported sensitivity mode. New output folders preserve previous runs. `Last_output_R1.txt` records the completed output location.

## 4. Generate figures and tables

```r
source("7 Reproduction Information Document/Final results review R1/Run_figures_and_tables_R1.R")
```

Keep `PHASED_FINAL_R1` set to the same annual ensemble. The runner reads the completed cost output, regenerates cost figures and produces annual and ecological tables and figures. The Python tools require the external reference tables, capacity workbook, county geometry and viewshed workbook. Static conceptual diagrams are retained as reference images. Historical notebooks are also available; their input files must be supplied.

## Checks and limits

The source-check workflow parses R and Python files and runs the operating-emissions tests. A successful syntax or unit check does not establish full scientific reproduction. Full dispatch, cluster jobs and the 1,000-simulation cost calculation must be run with the required datasets. Annual checks do not replace checks of hourly storage, ramp and import constraints.

Useful model comments describe assumptions and units. In particular, separate weather-profile draws do not preserve joint weather, and the storage input units must match the model's energy-capacity interpretation. These assumptions are retained from the supplied implementation.

Do not commit generated dispatch results, R sessions, notebook checkpoints, credentials or files of 100 MiB or more. Obtain EPA and FRED keys from environment variables (`EPA_API_KEY`, `FRED_API_KEY`). The original PDF and data portal provide further information about external data sources.
