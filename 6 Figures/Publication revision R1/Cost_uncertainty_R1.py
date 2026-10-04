# Purpose: Share pairing, component checks and uncertainty summaries across the publication figures.
"""Cost summaries shared by the publication figures."""
import numpy as np
import pandas as pd

PATHWAYS = ["A", "B1", "B2", "B3(1)", "B3(2)", "C1", "C2", "C3", "D"]
COMPONENTS = [
    "CAPEX", "FOM", "VOM", "Fuel", "Imports_NYISO", "Imports_QC", "Imports_NBSO",
    "GHG", "Air_emissions", "Unmet_demand", "CAN_CAPEX", "CAN_FOM", "CAN_VOM", "CAN_CH4",
]
LABELS = [
    "CAPEX", "Fixed O&M", "Variable O&M", "Fuel", "NYISO purchases",
    "Quebec purchases", "New Brunswick purchases", "GHG emissions", "Air emissions",
    "Unmet demand", "Canadian hydro CAPEX", "Canadian hydro fixed O&M",
    "Canadian hydro variable O&M", "Canadian reservoir methane",
]
KEYS = ["Simulation", "Pathway"]


def load_costs(path):
    data = pd.read_csv(path)
    if data.duplicated(KEYS).any() or set(data.Pathway) != set(PATHWAYS):
        raise ValueError("Cost tables must contain one row per simulation and pathway")
    ids = set(data.loc[data.Pathway == "B1", "Simulation"])
    if len(ids) < 2 or any(set(g.Simulation) != ids for _, g in data.groupby("Pathway")):
        raise ValueError("At least two shared simulations are required")
    columns = [c for c in data if c.endswith("_mean_bUSD")]
    if not np.isfinite(data[columns].to_numpy()).all():
        raise ValueError("Cost table contains nonfinite values")
    parts = [c for c in columns if c not in ("Total_Costs_mean_bUSD", "B1_Diff_mean_bUSD")]
    if not np.allclose(data[parts].sum(axis=1), data.Total_Costs_mean_bUSD, rtol=0, atol=1e-8):
        raise ValueError("Cost components do not sum to the total")
    return data


def load_components(path, totals):
    # check the separate import links because an adjusted B3 column is not a Canadian cost category.
    parts = pd.read_csv(path).set_index(KEYS)
    reference = totals.set_index(KEYS)
    if parts.index.has_duplicates or set(parts.index) != set(reference.index):
        raise ValueError("Component and total-cost simulation keys differ")
    parts = parts.reindex(reference.index)
    if not np.isfinite(parts[COMPONENTS].to_numpy()).all():
        raise ValueError("Component table contains missing or nonfinite costs")
    if not np.allclose(parts[COMPONENTS].sum(axis=1), reference.Total_Costs_mean_bUSD, rtol=0, atol=1e-8):
        raise ValueError("Separate import and Canadian components do not reconcile")
    return parts.reset_index()


def paired_costs(totals):
    # subtract B1 within each shared simulation because independent ranges discard pairing.
    wide = totals.pivot(index="Simulation", columns="Pathway", values="Total_Costs_mean_bUSD")
    differences = wide.subtract(wide.B1, axis=0)
    rows = []
    for pathway in PATHWAYS:
        values = differences[pathway]
        rows.append(dict(
            Pathway=pathway, N=len(values), Mean=values.mean(), Median=values.median(),
            SD=values.std(), P05=values.quantile(.05), P95=values.quantile(.95),
            Lower_than_B1=int((values < 0).sum()),
        ))
    return differences, pd.DataFrame(rows).set_index("Pathway")


def central_density(values):
    # fit the display curve to all draws, then stop at P05/P95 because Ryan requested the central 90%.
    values = np.asarray(values, dtype=float)
    lo, hi = np.quantile(values, [.05, .95])
    if np.isclose(lo, hi, rtol=0, atol=1e-12):
        return np.array([lo]), np.array([0.0])
    bandwidth = max(values.std(ddof=1) * len(values) ** (-.2), (hi-lo) * 1e-6)
    grid = np.linspace(lo, hi, 256)
    density = np.exp(-.5 * ((grid[:, None]-values[None, :])/bandwidth)**2).mean(axis=1)
    return grid, density / density.max()


def variance_shares(components, totals):
    # include covariance because adding component variances alone does not recover total variance.
    total = totals.set_index(KEYS).Total_Costs_mean_bUSD
    shares = []
    for pathway in PATHWAYS:
        group = components[components.Pathway == pathway].set_index(KEYS)[COMPONENTS]
        values = total.reindex(group.index)
        variance = values.var()
        if variance <= 0:
            raise ValueError(f"Total cost variance is zero for {pathway}")
        row = {name: 100 * group[name].cov(values) / variance for name in COMPONENTS}
        if not np.isclose(sum(row.values()), 100, atol=1e-8):
            raise ValueError(f"Variance shares do not sum to 100% for {pathway}")
        shares.append(dict(Pathway=pathway, **row))
    return pd.DataFrame(shares).set_index("Pathway")
