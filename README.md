<h1 align="center">PHASED</h1>
<p align="center"><strong>Electricity decarbonization pathways</strong></p>
<p align="center">Probabilistic Hourly Assessment of Scenarios for Electrical Decarbonization</p>

<p align="center">
  <a href="https://amirgazar.github.io/us-powerplants-phased/index.html"><img src="https://img.shields.io/badge/Visit_the_website-163B50?style=for-the-badge" alt="Visit the U.S. Power Plants and PHASED website"></a>
  <a href="https://amirgazar.github.io/us-powerplants-phased/phased-model.html"><img src="https://img.shields.io/badge/Explore_PHASED-227C83?style=for-the-badge" alt="Explore the PHASED model"></a>
  <a href="https://doi.org/10.31224/4684"><img src="https://img.shields.io/badge/Read_the_preprint-596778?style=for-the-badge" alt="Read the study preprint"></a>
</p>

<p align="center">
  <a href="#explore-the-model-and-data">Model and data</a> ·
  <a href="#what-the-model-does">Overview</a> ·
  <a href="#repository-guide">Repository guide</a> ·
  <a href="#running-the-analysis">Run the analysis</a> ·
  <a href="#study-authors-and-citation">Citation</a>
</p>

---

PHASED compares electricity decarbonization pathways by linking hourly power-system operation with costs, air-pollution damages, greenhouse-gas damages and ecological impacts. This repository contains the model and analysis code for its New England application, covering 2025 through 2050.

| Study region | Analysis period | Dispatch pathways | Simulations |
| :---: | :---: | :---: | :---: |
| **New England, U.S.A.** | **2025–2050** | **8** | **1,000** |

## Explore the model and data

The companion website brings the power-plant database and PHASED resources together. Use it to find source data, inspect model components and access available output downloads.

<table>
<tr>
<td width="50%" valign="top">

### U.S. power-plant data

Find plants, inspect their reports and obtain generation and emissions data.

- [Search power plants and reports](https://amirgazar.github.io/us-powerplants-phased/datalookup.html)
- [Download state and national datasets](https://amirgazar.github.io/us-powerplants-phased/download.html)
- [Access historical EPA records](https://amirgazar.github.io/us-powerplants-phased/campd.html)

</td>
<td width="50%" valign="top">

### PHASED model resources

Follow the analysis from source inputs to model components and available results.

- [Browse the input catalogue](https://amirgazar.github.io/us-powerplants-phased/model-inputs.html)
- [Explore the interactive model diagram](https://amirgazar.github.io/us-powerplants-phased/model-components.html)
- [Browse model output downloads](https://amirgazar.github.io/us-powerplants-phased/model-outputs.html)

</td>
</tr>
</table>

> **Data access:** Large datasets are stored separately from the code. Each catalogue entry lists its download status, citation and reuse terms.

## What the model does

The analysis follows electricity supply and demand hour by hour across eight pathways: A, B1, B2, B3, C1, C2, C3 and D. It combines installed-capacity schedules, uncertain generation, electricity imports, storage operation and fossil-fuel dispatch. Power-plant emissions are calculated from the final operating conditions.

| Electricity system | Costs and damages | Ecological impacts |
| --- | --- | --- |
| Hourly generation and imports | Investment and operating costs | Land occupation |
| Storage and demand balance | Fuel and electricity purchases | Bird and bat mortality |
| Fossil-fuel operation and emissions | Climate and air-pollution damages | Water withdrawals |
| Unmet demand | Unmet-demand penalties | Visual impacts |

The full analysis uses 1,000 simulations. Shared draws preserve paired comparisons between pathways. Costs are evaluated at discount rates of 1.5%, 2% and 2.5%. Pathway B3 has two cost-accounting cases, giving nine cost presentations from eight dispatch pathways.

## Repository guide

| Folder | Contents |
| --- | --- |
| [0 Stochastic Power Plant Model](0%20Stochastic%20Power%20Plant%20Model/) | Historical plant-data preparation and probabilistic generation inputs. |
| [1 Decarbonization Pathways](1%20Decarbonization%20Pathways/) | Pathway definitions and hourly installed-capacity schedules. |
| [2 Generation Expansion Model](2%20Generation%20Expansion%20Model/) | Demand, generation, imports, random draws, dispatch and validation. |
| [3 Total Costs](3%20Total%20Costs/) | Thirteen cost components and shared cost-coefficient sampling. |
| [4 External Data](4%20External%20Data/) | Supporting source tables and reference material. |
| [5 Ecological impacts](5%20Ecological%20impacts/) | Ecological calculations and notebooks. |
| [6 Figures](6%20Figures/) | Figure scripts, notebooks and publication-table exports. |
| [7 Reproduction Information Document](7%20Reproduction%20Information%20Document/) | Cost and figure entry scripts, supporting checks and the original reproduction document. |

## Running the analysis

```mermaid
flowchart LR
    A[Prepare inputs] --> B[Run hourly dispatch]
    B --> C[Combine results]
    C --> D[Calculate costs]
    D --> E[Create figures and tables]
    classDef inputs fill:#eaf3f8,stroke:#466e85,color:#163b50;
    classDef model fill:#e8f4f2,stroke:#418980,color:#174e47;
    classDef outputs fill:#f7f1e5,stroke:#a18043,color:#62491c;
    class A inputs;
    class B,C model;
    class D,E outputs;
```

**Choose your starting point:** Use step 1 for a new setup. If you already have completed dispatch summaries, continue with step 4. Expand each step for its inputs, settings and commands.

<details>
<summary><strong>1. Set up the software and project paths</strong></summary>

The study used R 4.4.2. Figure scripts and notebooks also require Python 3. Scripts list their package imports. Main dependencies include:

- **R:** data.table, dplyr, tidyr, readxl, lubridate, zoo, sf, fredr, httr, htmltools, jsonlite and digest. Cluster controls also use ssh.
- **Python:** numpy, pandas, scipy, matplotlib and openpyxl.

Some preparation tools require additional packages. Package versions are not fully locked in this repository.

Open R or RStudio with this repository as the working directory, then set:

```r
Sys.setenv(
  PHASED_R1_ROOT = normalizePath(getwd()),
  PHASED_DATA_ROOT = normalizePath(getwd())
)
```

`PHASED_DATA_ROOT` can point to a separate input-data folder with the same numbered folder structure. Set `PHASED_PYTHON_R1` to your Python executable if it is not available on the system path. Run Python scripts from the repository root or set `PHASED_R1_ROOT` in their environment.

</details>

<details>
<summary><strong>2. Obtain and prepare the inputs</strong></summary>

Start with the [PHASED input catalogue](https://amirgazar.github.io/us-powerplants-phased/model-inputs.html) and the [power-plant datasets](https://amirgazar.github.io/us-powerplants-phased/download.html). Run the preparation scripts for the stages you need. Keep the same random columns across pathways when reproducing paired results.

Dispatch requires capacity schedules, facility tables, generation distributions, operating limits, wind and solar capacity factors, import profiles, hourly demand, random draws and the saved operating-emissions model. The [dispatch source](2%20Generation%20Expansion%20Model/5%20Dispatch%20Curve/dispatch_curve_base_v2.R) lists their relative paths in its data-loading section.

Large generated tables, including `Hourly_Installed_Capacity.csv`, `Fossil_Fuel_Generation_Emissions.csv` and `Random_Sequence.csv`, must be supplied or generated before a full run. The saved operating-emissions model must match its helper code and fleet input. Use the [calibration script](2%20Generation%20Expansion%20Model/5%20Dispatch%20Curve/2%20Advanced%20Research%20Computing/1%20ARC%20Codes/Support%20R1/0%20Calibrate%20Operating%20Emissions_R1.R) after changing those calculations or the fleet.

</details>

<details>
<summary><strong>3. Run dispatch and combine the results</strong></summary>

For a small local run, open [Run local review](2%20Generation%20Expansion%20Model/5%20Dispatch%20Curve/1%20Run%20local%20review_R1.R). Set the action, hours, start year and pathways. Its default is a 2,000-hour test in 2050 using simulation 1. Set `PHASED_RANDOM_FILE` to choose the saved random table. A partial run cannot supply the annual production cost analysis.

For full local runs, the [dispatch runner](2%20Generation%20Expansion%20Model/7%20Validation%20R1/run_dispatch_review_R1.R) accepts:

```text
ROOT DATA_ROOT INPUT_EXTRACT NEW_OUTPUT HOURS SIM_IDS fresh START_YEAR PATHWAYS
```

Use `HOURS=0` for the full supplied horizon, `START_YEAR=2025`, comma-separated simulation IDs, and `A,B1,B2,B3,C1,C2,C3,D` for all pathways. `INPUT_EXTRACT` must contain the selected original random columns. Choose a new output directory for each run.

For cluster runs, use [ARC Control](2%20Generation%20Expansion%20Model/5%20Dispatch%20Curve/2%20Advanced%20Research%20Computing/1%20ARC%20Codes/1%20ARC%20Control_R1.R). Configure the host, project, account, container and computing resources for your installation. Set a unique run name with `PHASED_RUN_NAME`. The menu provides separate actions for preparing files, uploading inputs, submitting jobs, checking progress, combining results and downloading summaries.

The [summary script](2%20Generation%20Expansion%20Model/5%20Dispatch%20Curve/2%20Advanced%20Research%20Computing/1%20ARC%20Codes/Support%20R1/6_Summarization_Code_Final_R1.R) combines completed runs using the selected run settings. The final summary folder must contain:

- `Yearly_Results.csv`
- `Yearly_Facility_Level_Results.csv`
- `Yearly_Results_Shortages.csv`
- `Coverage_and_accounting.csv`
- `SUMMARY_COMPLETE.txt`

</details>

<details>
<summary><strong>4. Calculate costs</strong></summary>

Point to the completed summary folder and run the [full cost pipeline](7%20Reproduction%20Information%20Document/Cost%20production%20audit%20R1/1%20Run%20full%20cost%20pipeline_R1.R):

```r
Sys.setenv(PHASED_FINAL_R1 = "/path/to/completed/Final")
source("7 Reproduction Information Document/Cost production audit R1/1 Run full cost pipeline_R1.R")
```

The runner checks all 1,000 simulations, eight dispatch pathways and years 2025 through 2050 before calculating costs. It runs the 13 components at each discount rate and writes results to a new folder. `Last_output_R1.txt` records the completed output location.

Run cost components through this entry script. It replaces their `__PROJECT_ROOT__` path markers with the selected inputs and output folders.

Required supporting inputs include the NREL ATB table at `4 External Data/NREL ATB/ATBe_2024.csv`, the CPI table in `3 Total Costs/0 Pilot Cost Sanity Checks R1/Inputs R1`, the county lookup in the cost runner's `Inputs R1` folder, and the external tables referenced by each component. The small validated AP4 coefficient tables are included under `3 Total Costs/5 Air Pollutant Emissions Costs/AP4_ARC/outputs/AP4_native_20260920_01/validated_tables`.

The default air-damage setting is `AP4_hybrid`. Set `PHASED_AIR_MODEL_R1` to use another supported valuation mode.

</details>

<details>
<summary><strong>5. Generate figures and tables</strong></summary>

Keep `PHASED_FINAL_R1` set to the same dispatch summaries, then run the [figure and table entry script](7%20Reproduction%20Information%20Document/Final%20results%20review%20R1/Run_figures_and_tables_R1.R):

```r
source("7 Reproduction Information Document/Final results review R1/Run_figures_and_tables_R1.R")
```

This stage reads the completed cost output and generates cost figures, annual summaries and ecological exhibits. It also requires the capacity workbook, county geometry, viewshed workbook and other reference inputs used by those scripts. The notebooks in folders 5 and 6 provide additional calculations and plots.

</details>

<details>
<summary><strong>Additional settings</strong></summary>

Historical data helpers use `PHASED_STATES_HISTORICAL_DATA`, `PHASED_SIMULATED_DATA_FACILITY_LEVEL`, `PHASED_SIMULATED_DATA_PARQUET`, `PHASED_ARC_SSH_FOSSIL_FUELS_USA`, `PHASED_AUTOMATION` and `PHASED_FOSSIL_FUELS_RDS` for their input or output locations. Set only the variables needed by the helper you are running. The separate AP4 cluster controller uses `PHASED_AP4_ARC`.

Supply EPA and FRED credentials through `EPA_API_KEY` and `FRED_API_KEY`. Keep credentials, saved R sessions, notebook checkpoints and large generated results outside version control.

</details>

## Validation and interpretation

The [source-check workflow](.github/workflows/source-checks.yml) checks R and Python syntax, runs the operating-emissions tests and checks tracked file sizes. Dispatch and cost scripts also check input coverage, energy accounting and output consistency.

Syntax and unit checks do not replace a complete run with the required datasets. Annual accounting checks do not establish that every hourly storage, ramp or import constraint is satisfied. Separate weather-profile draws do not preserve joint weather, and storage input units must match the model's energy-capacity interpretation.

The [original reproduction PDF](7%20Reproduction%20Information%20Document/Reproduction%20Information%20Document.pdf) provides background on the first submission. Use the entry scripts and instructions above for the current workflow.

---

## Study, authors and citation

**Cost uncertainties and ecological impacts drive tradeoffs between electrical system decarbonization pathways in New England, U.S.A.**

<p>
  Amir M. Gazar<sup>1,2</sup>, Chloe Jackson<sup>3</sup>, Georgia Mavrommati<sup>3</sup>, Rich B. Howarth<sup>4</sup>, Ryan S.D. Calder<sup>1,2,5,*</sup>
</p>
<details>
<summary>Affiliations and contact</summary>

<p>
  <sup>1</sup>Dept. of Population Health Sciences, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>2</sup>Global Change Center, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <sup>3</sup>School for the Environment, University of Massachusetts Boston, Boston, MA, 02125, USA<br/>
  <sup>4</sup>Environmental Program, Dartmouth College, Hanover, NH, 03755, USA<br/>
  <sup>5</sup>Dept. of Civil & Environmental Engineering, Virginia Tech, Blacksburg, VA, 24061, USA<br/>
  <strong>* Contact:</strong> rsdc@vt.edu
</p>

</details>

Use [citation.bib](citation.bib) for the archived preprint citation and the website's [citation page](https://amirgazar.github.io/us-powerplants-phased/citation.html) for related resources. Cite external datasets according to their source records.

## License

This repository is distributed under [Creative Commons Attribution 4.0 International](LICENSE.txt). See [AUTHORS.txt](AUTHORS.txt) for attribution. External datasets retain their own source-specific terms.
