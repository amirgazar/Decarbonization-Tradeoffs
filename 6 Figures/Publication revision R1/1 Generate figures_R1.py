# Purpose: Prefer prepared AP4 inputs, label the air valuation and record expanded sampling metadata.
"""Generate manuscript Figures 5 and 6 and SI Figures S5 and S6.

Run after the cost table export. Set PHASED_FIGURE_INPUT_R1 and
PHASED_FIGURE_OUTPUT_R1 to use a completed run rather than the retained example.
"""
from pathlib import Path
import hashlib
import json
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm
import numpy as np
import pandas as pd

from Cost_uncertainty_R1 import (
    PATHWAYS, COMPONENTS, LABELS, load_costs, load_components,
    paired_costs, variance_shares,
)

from Figure6_county_R1 import county_figures

BASE = Path(__file__).resolve().parent
# prefer the completed full ensemble so direct runs cannot silently publish the older 50-run preview.
ROOT = BASE.parents[1]
pointer = ROOT / "7 Reproduction Information Document/Cost production audit R1/Last_output_R1.txt"
completed = Path(pointer.read_text().strip()) if pointer.exists() else None
if completed is not None and (completed / "COMPLETE.txt").exists():
    DEFAULT_INPUT = completed / "Figure inputs R1"
    DEFAULT_OUTPUT = completed / "Figures R1"
else:
    if not os.environ.get("PHASED_FIGURE_INPUT_R1"):
        raise FileNotFoundError("Run the cost pipeline or set PHASED_FIGURE_INPUT_R1 to exported figure inputs")
    DEFAULT_INPUT = Path(os.environ["PHASED_FIGURE_INPUT_R1"])
    DEFAULT_OUTPUT = BASE / "Results R1"
INPUT = Path(os.environ.get("PHASED_FIGURE_INPUT_R1", str(DEFAULT_INPUT)))
OUTPUT = Path(os.environ.get("PHASED_FIGURE_OUTPUT_R1", str(DEFAULT_OUTPUT)))
METADATA_PATH = INPUT / "Figure_input_metadata_R1.json"
METADATA = json.loads(METADATA_PATH.read_text()) if METADATA_PATH.exists() else {"air_model": "AP3-based valuation", "cost_sampling": "generation_CAPEX_FOM_only", "data_status": "Retained example"}
AIR_LABEL = {"AP4_hybrid": "AP4; AP3 PM10; published CO", "AP4_common4": "AP4 (four pollutants)"}.get(METADATA["air_model"], METADATA["air_model"])
STACKS = ["CAPEX", "FOM", "VOM", "Imports", "Fuel", "GHG", "Unmet_demand", "Air_emissions"]
STACK_LABELS = ["CAPEX", "Fixed O&M", "Variable O&M", "Imports", "Fuel", "GHG emissions", "Unmet demand", "Air emissions"]
COLORS = ["#001f3f", "#003f5f", "#005f7f", "#007f9f", "#309fbf", "green", "red", "black"]
plt.rcParams.update({
    "font.family": "DejaVu Sans", "font.size": 12,
    "axes.spines.top": False, "axes.spines.right": False,
    "svg.fonttype": "none", "pdf.fonttype": 42,
})


def save(figure, name):
    for extension in ("png", "svg", "pdf"):
        figure.savefig(OUTPUT / f"{name}.{extension}", dpi=300, bbox_inches="tight", facecolor="white")
    plt.close(figure)


def figure5(totals, components):
    # retain the original cost categories and colors so the updated results remain comparable.
    plot = components.copy()
    for target, extra in (("CAPEX", "CAN_CAPEX"), ("FOM", "CAN_FOM"), ("VOM", "CAN_VOM"), ("GHG", "CAN_CH4")):
        plot[target] += plot[extra]
    plot["Imports"] = plot[["Imports_NYISO", "Imports_QC", "Imports_NBSO"]].sum(axis=1)
    means = plot.groupby("Pathway")[STACKS].mean().reindex(PATHWAYS)
    differences, summary = paired_costs(totals)
    figure, (left, right) = plt.subplots(1, 2, figsize=(11.2, 7.0), gridspec_kw={"width_ratios": [1.05, 1]})
    positions = np.arange(len(PATHWAYS))
    bottom = np.zeros(len(PATHWAYS))
    for name, label, color in zip(STACKS, STACK_LABELS, COLORS):
        values = means[name].to_numpy()
        left.bar(positions, values, bottom=bottom, label=label, color=color, edgecolor="white", linewidth=.25)
        bottom += values
    upper_limits = []
    for position, pathway in enumerate(PATHWAYS):
        values = totals.loc[totals.Pathway == pathway, "Total_Costs_mean_bUSD"]
        low, high = values.quantile([.05, .95])
        upper_limits.append(high)
        left.vlines(position, low, high, color="black", linewidth=1)
        left.hlines([low, high], position-.12, position+.12, color="black", linewidth=1)
        left.annotate(f"{values.mean():.0f}", (position, high), xytext=(0, 5), textcoords="offset points", ha="center", fontsize=11)
    left.set(xticks=positions, xticklabels=PATHWAYS, ylabel="Net present value (billion 2024 USD)", ylim=(0, max(upper_limits)*1.12))
    left.tick_params(axis="x", rotation=35)
    left.set_title("(a) Total costs", loc="left", fontweight="bold")
    left.grid(axis="y", alpha=.18)
    left.set_axisbelow(True)

    # retain the original seven expansion-pathway comparisons; the source table also reports A.
    comparisons = [p for p in PATHWAYS if p not in ("A", "B1")]
    # show box plots requested by the author; whiskers retain P05/P95 and all other draws remain visible.
    box_data=[differences[p].to_numpy() for p in comparisons]
    low=min(0,min(v.min() for v in box_data));high=max(0,max(v.max() for v in box_data));span=max(high-low,1)
    boxes=right.boxplot(box_data,positions=range(len(comparisons)),vert=False,widths=.55,whis=(5,95),patch_artist=True,
        medianprops={'color':'black','linewidth':1.3},flierprops={'marker':'.','markersize':2,'alpha':.3,'markeredgecolor':'#777777'})
    for i,box in enumerate(boxes['boxes']):box.set_facecolor(plt.get_cmap('tab10')(i));box.set_alpha(.75)
    summary['P25']=[differences[p].quantile(.25) for p in summary.index]
    summary['P75']=[differences[p].quantile(.75) for p in summary.index]
    right.axvline(0, color="gray", linestyle="--", linewidth=1)
    right.set(yticks=range(len(comparisons)), yticklabels=comparisons,
              xlim=(low-.06*span, high+.06*span),
              xlabel="Pathway minus B1 (billion 2024 USD)")
    right.invert_yaxis()
    right.grid(axis="x", alpha=.15)
    right.set_title("(b) Paired cost differences", loc="left", fontweight="bold")
    figure.legend(*left.get_legend_handles_labels(), loc="lower center", ncol=4, frameon=False, bbox_to_anchor=(.5, .06), fontsize=11)
    figure.text(.5, .014, f"{int(summary.N.iloc[0])} paired simulations; 2% discount rate; {AIR_LABEL}.\nBoxes: P25–P75; black lines: medians; whiskers: P05–P95; dots: remaining draws.", ha="center", fontsize=11)
    figure.subplots_adjust(bottom=.26, wspace=.38, top=.92)
    save(figure, "Figure5_R1")
    summary.to_csv(OUTPUT / "Figure5_paired_B1_R1.csv")


def figure6(totals):
    county = pd.read_csv(INPUT / "County_costs_per_simulation.csv")
    ids = set(totals.Simulation)
    pathways = ["A", "B1", "B2", "B3", "C1", "C2", "C3", "D"]
    county["Place"] = county.County.str.replace(" County", "", regex=False) + ", " + county.State
    if set(county.Simulation) != ids or set(county.Pathway) != set(pathways):
        raise ValueError("County and total-cost tables have different scope")
    if county.duplicated(["Place", "Pathway", "Simulation"]).any():
        raise ValueError("Duplicate county cost key")
    if not np.isfinite(county.npv_total_air_emission_USD).all():
        raise ValueError("County costs contain nonfinite values")
    # reject partial county groups because filling a missing simulation with zero would bias the map.
    for _, group in county.groupby(["Place", "Pathway"]):
        if set(group.Simulation) != ids:
            raise ValueError("County group is missing simulations")
    reference = totals.assign(Dispatch=totals.Pathway.str.replace(r"\(.*", "", regex=True))
    reference = reference[["Simulation", "Dispatch", "Air_emissions_mean_bUSD"]].drop_duplicates().set_index(["Simulation", "Dispatch"])
    summed = county.groupby(["Simulation", "Pathway"]).npv_total_air_emission_USD.sum()/1e9
    if set(reference.index) != set(summed.index) or not np.allclose(reference.Air_emissions_mean_bUSD, summed.reindex(reference.index), rtol=0, atol=1e-8):
        raise ValueError("County and total air costs do not reconcile")
    county_figures(county, ROOT, OUTPUT, save)


def figure_s5(totals, components):
    b1 = components[components.Pathway == "B1"].set_index("Simulation")[COMPONENTS].copy()
    b1["Total"] = totals[totals.Pathway == "B1"].set_index("Simulation").Total_Costs_mean_bUSD
    correlation = b1.corr(method="spearman")
    labels = LABELS + ["Total"]
    figure, axis = plt.subplots(figsize=(10, 9))
    palette = plt.get_cmap("BrBG").copy()
    palette.set_bad("#eeeeee")
    raster = axis.imshow(np.ma.masked_invalid(correlation), cmap=palette, vmin=-1, vmax=1)
    axis.set_xticks(range(len(labels)), labels, rotation=55, ha="right", fontsize=10)
    axis.set_yticks(range(len(labels)), labels, fontsize=10)
    for row in range(len(labels)):
        for col in range(len(labels)):
            value = correlation.iloc[row, col]
            label = "–" if pd.isna(value) else f"{value:.2f}"
            axis.text(col, row, label, ha="center", va="center", fontsize=8,
                      color="white" if pd.notna(value) and abs(value) > .65 else "black")
    figure.colorbar(raster, ax=axis, shrink=.8, label="Spearman correlation")
    axis.set_title("B1 cost correlations", fontweight="bold")
    # leave constant components undefined because zero correlation would imply an estimated relationship.
    figure.text(.5, .012, "A dash indicates a constant cost component; its correlation is undefined.", ha="center", fontsize=10)
    figure.tight_layout(rect=(0, .03, 1, 1))
    save(figure, "FigureS5_R1")
    correlation.to_csv(OUTPUT / "FigureS5_source_R1.csv")


def figure_s6(totals, components):
    shares = variance_shares(components, totals)
    values = shares[COMPONENTS].to_numpy().T
    figure, axis = plt.subplots(figsize=(8.8, 7.2))
    maximum = max(np.abs(values).max(), 1)
    raster = axis.imshow(values, aspect="auto", cmap="RdBu_r", vmin=-maximum, vmax=maximum)
    axis.set_xticks(range(len(PATHWAYS)), PATHWAYS)
    axis.set_yticks(range(len(COMPONENTS)), LABELS, fontsize=10)
    for row in range(len(COMPONENTS)):
        for col in range(len(PATHWAYS)):
            axis.text(col, row, f"{values[row,col]:.2f}", ha="center", va="center", fontsize=9,
                      color="white" if abs(values[row,col]) > maximum*.6 else "black")
    figure.colorbar(raster, ax=axis, label="Share of total cost variance (%)", shrink=.8)
    # name these as component contributions because the calculation is not a global input-sensitivity analysis.
    axis.set_title("Cost-component variance contributions", fontweight="bold")
    figure.tight_layout()
    save(figure, "FigureS6_R1")
    shares.to_csv(OUTPUT / "FigureS6_source_R1.csv")


def main():
    OUTPUT.mkdir(parents=True, exist_ok=True)
    totals = load_costs(INPUT / "All_Costs_per_Simulation.csv")
    components = load_components(INPUT / "Cost_components_R1.csv", totals)
    figure5(totals, components)
    figure6(totals)
    figure_s5(totals, components)
    figure_s6(totals, components)
    manifest = {
        "n_simulations": int(totals.Simulation.nunique()), "discount_rate": .02,
        "inputs": {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in INPUT.glob("*.csv")},
        "paired_display": "Box plots: P25–P75 boxes, median lines, P05–P95 whiskers, all remaining draws shown as points; no draw exclusion",
        "scope": METADATA,
        "fixed_coefficients": "Fossil fuel prices, AP4 coefficients, within-rate GHG schedules and unmet-demand value",
        "imports": "NYISO, Quebec and New Brunswick shown separately; B3(2) replaces Quebec purchases with Canadian hydro costs.",
        "figure_s6": "100 * Cov(component, total) / Var(total), not sensitivity to input parameters",
    }
    (OUTPUT / "Figure_manifest_R1.json").write_text(json.dumps(manifest, indent=2))
    print("Saved Figures 5, 6, S5 and S6 and their source tables.")


if __name__ == "__main__":
    main()
