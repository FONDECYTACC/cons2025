import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os

# --- 1. DATOS ---
readm_path = os.path.join("_out", "Readmission_Optuna2_Pareto_Phase2_20260227_2320.csv")
mort_path = os.path.join("_out", "Death_Pareto_Phase1_20260227_1822.csv")

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
    score = ((1.0 - df["C_Index"]) ** 2 + (df["IBS"]) ** 2)
    return df.loc[score.idxmin()]

# --- 2. CONFIGURACION COMPACTA (1/3 de pagina) ---
sns.set_theme(style="whitegrid", context="paper")

# Proporcion horizontal y baja (12x5)
fig, axes = plt.subplots(1, 2, figsize=(12, 5))

# Fuentes y escalas
L_SIZE = 13  # Ejes
T_SIZE = 10  # Labels de puntos
TITLE_SIZE = 13
LEG_SIZE = 10

# PANEL 1: READMISION
ax1 = axes[0]
sns.scatterplot(
    data=df_readm,
    x="IBS",
    y="C_Index",
    s=95,
    color="#2c7bb6",
    alpha=0.85,
    linewidth=0,
    ax=ax1,
    label="Pareto trials",
)
winner_r = get_best_trial_row(df_readm)
ax1.scatter(
    winner_r["IBS"],
    winner_r["C_Index"],
    facecolors="none",
    edgecolors="#111111",
    s=280,
    linewidth=1.8,
    label=f"Best trial (T-{int(winner_r['trial_id'])})",
)

# Linea de Pareto
df_r_s = df_readm.sort_values("IBS")
ax1.plot(df_r_s["IBS"], df_r_s["C_Index"], "--", color="gray", alpha=0.4)

for i in range(df_readm.shape[0]):
    ax1.text(df_readm["IBS"].iloc[i], df_readm["C_Index"].iloc[i] + 0.0001, f"T{df_readm['trial_id'].iloc[i]}", fontsize=T_SIZE)

ax1.set_xlabel("IBS (Brier Score)", fontsize=L_SIZE)
ax1.set_ylabel("C-Index", fontsize=L_SIZE)
ax1.set_title("Readmission", fontsize=TITLE_SIZE, pad=8)
ax1.legend(
    fontsize=LEG_SIZE,
    loc="lower center",
    bbox_to_anchor=(0.5, 0.02),
    frameon=True,
    facecolor="white",
    edgecolor="#cccccc",
    ncol=2,
    columnspacing=1.0,
    handletextpad=0.5,
)

# PANEL 2: MORTALIDAD
ax2 = axes[1]
sns.scatterplot(
    data=df_mort,
    x="IBS",
    y="C_Index",
    s=95,
    color="#1a9641",
    alpha=0.85,
    linewidth=0,
    ax=ax2,
    label="Pareto trials",
)
winner_m = get_best_trial_row(df_mort)
ax2.scatter(
    winner_m["IBS"],
    winner_m["C_Index"],
    facecolors="none",
    edgecolors="#111111",
    s=280,
    linewidth=1.8,
    label=f"Best trial (T-{int(winner_m['trial_id'])})",
)

# Linea de Pareto
df_m_s = df_mort.sort_values("IBS")
ax2.plot(df_m_s["IBS"], df_m_s["C_Index"], "--", color="gray", alpha=0.4)

for i in range(df_mort.shape[0]):
    ax2.text(df_mort["IBS"].iloc[i], df_mort["C_Index"].iloc[i] + 0.0002, f"T{df_mort['trial_id'].iloc[i]}", fontsize=T_SIZE)

ax2.set_xlabel("IBS (Brier Score)", fontsize=L_SIZE)
ax2.set_ylabel("C-Index", fontsize=L_SIZE)
ax2.set_title("Mortality", fontsize=TITLE_SIZE, pad=8)
ax2.legend(
    fontsize=LEG_SIZE,
    loc="lower center",
    bbox_to_anchor=(0.5, 0.02),
    frameon=True,
    facecolor="white",
    edgecolor="#cccccc",
    ncol=2,
    columnspacing=1.0,
    handletextpad=0.5,
)

# --- 3. EXPORTACION ---
plt.tight_layout()

sns.despine()

folder = "_figs"
if not os.path.exists(folder):
    os.makedirs(folder)

plt.savefig(f"{folder}/pareto_compacto_tesis.png", dpi=300, bbox_inches="tight")
plt.savefig(f"{folder}/pareto_compacto_tesis.pdf", bbox_inches="tight")

plt.show()
