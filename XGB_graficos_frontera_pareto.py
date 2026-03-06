import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import seaborn as sns
import os
from pathlib import Path
from datetime import datetime

try:
    from adjustText import adjust_text
    HAS_ADJUST_TEXT = True
except ImportError:
    HAS_ADJUST_TEXT = False


def find_project_root(markers=("AGENTS.md", ".git")):
    try:
        cur = Path.cwd().resolve()
    except OSError as e:
        raise RuntimeError(
            "Invalid working directory. Run this notebook from inside the project folder."
        ) from e

    for p in (cur, *cur.parents):
        if any((p / m).exists() for m in markers):
            return p

    raise RuntimeError(
        f"Could not locate project root starting from {cur}. "
        f"Expected one of markers: {markers}."
    )


PROJECT_ROOT = find_project_root()
OUT_DIR = PROJECT_ROOT / "_out"
FIGS_DIR = PROJECT_ROOT / "_figs"
OUT_DIR.mkdir(parents=True, exist_ok=True)
FIGS_DIR.mkdir(parents=True, exist_ok=True)

# --- 1. DATOS ---
readm_path = OUT_DIR / "Readmission_Optuna2_Pareto_Phase2_20260305_1553_mar26.csv"
mort_path = OUT_DIR / "Death_Pareto_Phase2_20260305_2039_mar26.csv"

df_readm = pd.read_csv(readm_path)
df_mort = pd.read_csv(mort_path)


def normalize_pareto_columns(df):
    c_candidates = [
        "C_Index",
        "Phase2_Multi_Horizon_C_Index",
        "Phase1_Multi_Horizon_C_Index",
        "Phase2_Global_C_Index",
        "Phase1_Global_C_Index",
    ]
    ibs_candidates = [
        "IBS",
        "Phase2_IBS",
        "Phase2_Aalen_Johansen_Brier_Score",
        "Phase1_IBS",
    ]

    c_col = next((c for c in c_candidates if c in df.columns), None)
    ibs_col = next((c for c in ibs_candidates if c in df.columns), None)

    if c_col is None or ibs_col is None:
        raise ValueError(
            f"Missing C-Index/IBS columns. Available columns: {list(df.columns)}"
        )

    out = df.rename(columns={c_col: "C_Index", ibs_col: "IBS"}).copy()
    out["C_Index"] = pd.to_numeric(out["C_Index"], errors="coerce")
    out["IBS"] = pd.to_numeric(out["IBS"], errors="coerce")
    out["trial_id"] = pd.to_numeric(out["trial_id"], errors="coerce")
    out = out.dropna(subset=["C_Index", "IBS", "trial_id"]).copy()
    out["trial_id"] = out["trial_id"].astype(int)
    return out


df_readm = normalize_pareto_columns(df_readm)
df_mort = normalize_pareto_columns(df_mort)


def get_best_trial_row(df):
    if "Distance_to_Ideal" in df.columns:
        return df.loc[df["Distance_to_Ideal"].idxmin()]
    score = (1.0 - df["C_Index"]) ** 2 + df["IBS"] ** 2
    return df.loc[score.idxmin()]


def compute_pareto_front(df):
    """Extract non-dominated points: maximize C_Index, minimize IBS."""
    sorted_df = df.sort_values("IBS").reset_index(drop=True)
    pareto = [sorted_df.iloc[0]]
    best_c = sorted_df.iloc[0]["C_Index"]
    for _, row in sorted_df.iloc[1:].iterrows():
        if row["C_Index"] >= best_c:
            pareto.append(row)
            best_c = row["C_Index"]
    return pd.DataFrame(pareto)


# --- 2. PUBLICATION-QUALITY SETUP ---
mpl.rcParams.update({
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Times", "DejaVu Serif"],
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
    "axes.labelsize": 13,
    "axes.titlesize": 14,
    "xtick.labelsize": 11,
    "ytick.labelsize": 11,
    "figure.dpi": 300,
})

fig, axes = plt.subplots(1, 2, figsize=(13, 5.5))

PANEL_CONFIGS = [
    {
        "ax": axes[0],
        "df": df_readm,
        "color": "#2c7bb6",
        "title": "Readmission",
        "panel_letter": "A",
    },
    {
        "ax": axes[1],
        "df": df_mort,
        "color": "#1a9641",
        "title": "Mortality",
        "panel_letter": "B",
    },
]

for cfg in PANEL_CONFIGS:
    ax = cfg["ax"]
    df = cfg["df"]
    color = cfg["color"]
    winner = get_best_trial_row(df)
    pareto_df = compute_pareto_front(df)

    # All trials
    ax.scatter(
        df["IBS"], df["C_Index"],
        s=70, color=color, alpha=0.8, linewidth=0, zorder=3,
        label="Pareto candidates",
    )

    # Pareto staircase (step function: best achievable C for any IBS threshold)
    pf = pareto_df.sort_values("IBS")
    ax.step(
        pf["IBS"], pf["C_Index"],
        where="post", color=color, alpha=0.3, linewidth=1.5, linestyle="--",
        zorder=2,
    )

    # Winner highlight
    ax.scatter(
        winner["IBS"], winner["C_Index"],
        facecolors="none", edgecolors="#111111", s=260, linewidth=2.0, zorder=4,
        label=f"Selected (T-{int(winner['trial_id'])})",
    )

    # Ideal point (C=1, IBS=0) — subtle reference
    ax.scatter(
        0, 1, marker="*", s=120, color="#999999", alpha=0.4, zorder=1,
        label="Ideal point",
    )

    # Labels with adjustText to avoid overlap
    texts = []
    for _, row in df.iterrows():
        texts.append(
            ax.text(
                row["IBS"], row["C_Index"],
                f"T{int(row['trial_id'])}",
                fontsize=8, color="#444444", ha="center", va="bottom",
            )
        )

    if HAS_ADJUST_TEXT:
        adjust_text(
            texts, ax=ax,
            arrowprops=dict(arrowstyle="-", color="#bbbbbb", lw=0.5),
            expand=(1.4, 1.6),
            force_text=(0.8, 1.0),
        )

    # Auto-scale axes with padding so no trial is clipped
    #x_margin = max((df["IBS"].max() - df["IBS"].min()) * 0.0005, 0.005)
    y_margin = (df["C_Index"].max() - df["C_Index"].min()) * 0.15 + 0.001
    #ax.set_xlim(df["IBS"].min() - x_margin, df["IBS"].max() + x_margin)
    ax.set_ylim(df["C_Index"].min() - y_margin, df["C_Index"].max() + y_margin)

    ibs_min = df["IBS"].min()
    ibs_max_zoom = df["IBS"].quantile(0.2)  # Solo hasta el percentil 30 (zona densa)
    ax.set_xlim(ibs_min * 0.99, ibs_max_zoom * 1.01)
    
    ax.set_xlabel("Integrated Brier Score", fontsize=13)
    ax.set_ylabel("Multi-Horizon C-Index", fontsize=13)
    ax.set_title(cfg["title"], fontsize=14, fontweight="bold", pad=10)

    # Panel letter
    ax.text(
        0.02, 0.97, cfg["panel_letter"],
        transform=ax.transAxes, fontsize=16, fontweight="bold",
        va="top", ha="left",
    )

    ax.legend(
        fontsize=9, loc="lower left",
        frameon=True, facecolor="white", edgecolor="#cccccc",
        framealpha=0.9,
    )
    ax.grid(True, linestyle=":", alpha=0.4)

sns.despine()
plt.tight_layout()

# --- 3. EXPORT ---
timestamp = datetime.now().strftime("%Y%m%d_%H%M")
fig_path_png = FIGS_DIR / f"pareto_front_{timestamp}_mar26.png"
fig_path_pdf = FIGS_DIR / f"pareto_front_{timestamp}_mar26.pdf"

plt.savefig(fig_path_png, dpi=300, bbox_inches="tight")
plt.savefig(fig_path_pdf, bbox_inches="tight")
plt.show()

print(f"Saved: {fig_path_png}")
print(f"Saved: {fig_path_pdf}")
