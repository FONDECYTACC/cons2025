#@title Step 8: SHAP Analysis & Plots (DUAL, Multi-Horizon, source_tag-traceable)
# Corrected version - compatible with Step 5 outputs from XGboost_combined_mar26.ipynb

import os
import re
import glob
import json
import pickle
import numpy as np
import pandas as pd
import shap
import matplotlib.pyplot as plt
import matplotlib as mpl
from IPython.display import display, Markdown

# -----------------------------
# 0) Config
# -----------------------------
if "PROJECT_ROOT" not in globals():
    raise RuntimeError("PROJECT_ROOT is not defined. Run the root setup cell first.")

PROJECT_ROOT = os.path.abspath(str(PROJECT_ROOT))

IN_DIR = os.path.join(PROJECT_ROOT, "_out")
OUT_DIR = os.path.join(PROJECT_ROOT, "_out")
FIG_DIR = os.path.join(PROJECT_ROOT, "_figs")

# Set None to auto-use all eval_times found in raw logs
TARGET_HORIZONS = [12, 60]
MAX_BEESWARM_N = 90000
RNG = np.random.RandomState(2125)

# CI config (optional but enabled by default)
BOOTSTRAP_CI = True
N_BOOTSTRAP = 500
CI_ALPHA = 0.05
BOOTSTRAP_MAX_N = 90000

# Multicollinearity check
CORR_THRESHOLD = 0.90

# Unified DPI
FIG_DPI = 300

os.makedirs(IN_DIR, exist_ok=True)
os.makedirs(OUT_DIR, exist_ok=True)
os.makedirs(FIG_DIR, exist_ok=True)

# NOTE: These key candidates are matched against Step 5 output structure
# Step 5 saves: 'risk_pred_readm', 'risk_pred_death', 'probs_readm_matrix', 'probs_death_matrix', etc.
OUTCOME_CFG = {
    "readm": {
        "label": "Readmission",
        "shap_key_candidates": ["shap_r_all", "shap_readm_all"],
        "margin_key_candidates": ["risk_pred_readm", "risk_pred_r", "margin_pred_readm"],
        "probs_key_candidates": ["probs_readm_matrix", "probs_r_matrix", "surv_probs_readm_matrix"],
        "hz_times_candidates": ["times_r", "times_readm"],
        "hz_vals_candidates": ["h0_r", "H0_r", "h0_readm"],
    },
    "death": {
        "label": "Death",
        "shap_key_candidates": ["shap_d_all", "shap_death_all", "shap_mort_all"],
        "margin_key_candidates": ["risk_pred_death", "risk_pred_d", "risk_pred_mort", "margin_pred_death"],
        "probs_key_candidates": ["probs_death_matrix", "probs_d_matrix", "probs_mort_matrix", "surv_probs_death_matrix"],
        "hz_times_candidates": ["times_d", "times_death", "times_mort"],
        "hz_vals_candidates": ["h0_d", "H0_d", "h0_death", "h0_mort"],
    },
}

VAL_ID_KEYS = ["val_ids", "valid_ids", "val_idx", "val_index", "idx_val"]

mpl.rcParams.update({
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
    "axes.labelsize": 14,
    "axes.titlesize": 15,
    "xtick.labelsize": 12,
    "ytick.labelsize": 12,
    "figure.dpi": FIG_DPI
})

# -----------------------------
# 1) Pick latest complete Step 5 bundle (simplified)
# -----------------------------
TS_RE = re.compile(r"(\d{8}_\d{4})")  # Just find the timestamp anywhere

def pick_latest_complete_bundle(in_dir):
    """Find the most recent complete set of Step 5 output files."""
    shap_files = glob.glob(os.path.join(in_dir, "xgb6_corr_DUAL_SHAP_Aggregated_*.pkl"))
    
    candidates = []
    for shapf in shap_files:
        m = TS_RE.search(os.path.basename(shapf))
        if not m:
            continue
        tag = m.group(1)
        
        # Check for companion files - try both naming patterns
        for suffix in [f"_{tag}_mar26.pkl", f"_{tag}.pkl"]:
            rawf = os.path.join(in_dir, f"xgb6_corr_DUAL_final_ev_hyp{suffix}")
            hzf = os.path.join(in_dir, f"xgb6_corr_DUAL_BaselineHazards{suffix}")
            splitf = os.path.join(in_dir, f"xgb6_corr_DUAL_CV_Splits{suffix}")
            
            if all(os.path.exists(p) for p in (rawf, hzf, splitf)):
                dt = pd.to_datetime(tag, format="%Y%m%d_%H%M", errors="coerce")
                if pd.notna(dt):
                    candidates.append((dt, tag, shapf, rawf, hzf, splitf))
                    break  # Found complete set for this tag
    
    if not candidates:
        raise FileNotFoundError(
            f"No complete Step 5 bundle found in '{in_dir}'. "
            f"Need: xgb6_corr_DUAL_SHAP_Aggregated_*.pkl + "
            f"final_ev_hyp + BaselineHazards + CV_Splits"
        )
    
    candidates.sort(key=lambda x: x[0])
    return candidates[-1]  # Latest

source_tag, shap_file, raw_file, hz_file, split_file = pick_latest_complete_bundle(IN_DIR)[1:]

# For traceability, use source_tag in filenames
FILE_TAG = source_tag
RUN_TS = pd.Timestamp.now().strftime("%Y%m%d_%H%M")

# -----------------------------
# 2) Load artifacts
# -----------------------------
try:
    with open(shap_file, "rb") as f:
        shap_data = pickle.load(f)
except Exception as e:
    raise RuntimeError(f"Failed to load SHAP file {shap_file}: {e}") from e

try:
    with open(raw_file, "rb") as f:
        raw_data_log = pickle.load(f)
except Exception as e:
    raise RuntimeError(f"Failed to load raw data file {raw_file}: {e}") from e

try:
    with open(hz_file, "rb") as f:
        baseline_hazards_log = pickle.load(f)
except Exception as e:
    raise RuntimeError(f"Failed to load baseline hazards file {hz_file}: {e}") from e

try:
    with open(split_file, "rb") as f:
        cv_splits_log = pickle.load(f)
except Exception as e:
    raise RuntimeError(f"Failed to load CV splits file {split_file}: {e}") from e

required_keys = {"X_all", "feature_names"}
missing = required_keys - set(shap_data.keys())
if missing:
    raise KeyError(f"Missing keys in SHAP file: {missing}")

X_all = shap_data["X_all"]
feature_names = list(shap_data["feature_names"])

if not isinstance(X_all, pd.DataFrame):
    X_all = pd.DataFrame(X_all, columns=feature_names)

if list(X_all.columns) != feature_names:
    X_all = X_all.reindex(columns=feature_names)

if not X_all.index.is_unique:
    raise ValueError("X_all index must be unique.")

# -----------------------------
# 3) Helpers
# -----------------------------
def get_first(dct, keys, default=None):
    """Return first value from dct matching any key in keys."""
    for k in keys:
        if k in dct and dct[k] is not None:
            return dct[k]
    return default

def find_first_key(dct, keys):
    """Return first key from keys that exists in dct."""
    for k in keys:
        if k in dct:
            return k
    return None

def h0_at_t(times, h0_vals, t):
    """Get baseline cumulative hazard at time t using left-continuous interpolation."""
    times = np.asarray(times, dtype=float).ravel()
    h0_vals = np.asarray(h0_vals, dtype=float).ravel()
    if times.size == 0 or h0_vals.size == 0 or len(times) != len(h0_vals):
        return np.nan
    
    t = float(t)
    if t < times[0]:
        return 0.0  # Before first event, cumulative hazard is 0
    
    i = np.searchsorted(times, t, side="right") - 1
    i = max(0, min(i, len(h0_vals) - 1))  # Clip to valid range
    return float(h0_vals[i])

def fmt_horizon(h):
    h = float(h)
    return str(int(h)) if abs(h - round(h)) < 1e-9 else f"{h:g}"

def horizon_token(h):
    return f"{fmt_horizon(h).replace('.', 'p')}m"

def save_current_figure(stem, outcome, horizon=None):
    fig = plt.gcf()
    hz = f"_{horizon_token(horizon)}" if horizon is not None else ""
    base = f"xgb8_dual_{outcome}_{stem}{hz}_{FILE_TAG}"
    png = os.path.join(FIG_DIR, f"{base}.png")
    pdf = os.path.join(FIG_DIR, f"{base}.pdf")
    fig.savefig(png, dpi=FIG_DPI, bbox_inches="tight")
    fig.savefig(pdf, bbox_inches="tight")
    return [png, pdf]

def discover_horizons(raw_log):
    """Extract all unique eval_times from raw data log."""
    vals = []
    for rec in raw_log:
        ev = np.asarray(rec.get("eval_times", []), dtype=float).ravel()
        vals.extend([v for v in ev if np.isfinite(v)])
    return sorted(set(vals))

def bootstrap_mean_abs_shap(shap_vals, n_boot=200, alpha=0.05, seed=2026, max_n=None):
    """Bootstrap confidence intervals for mean absolute SHAP values."""
    n, p = shap_vals.shape
    rng = np.random.RandomState(seed)

    if max_n is not None and n > max_n:
        idx = rng.choice(n, size=max_n, replace=False)
        X_sub = shap_vals[idx, :]
    else:
        X_sub = shap_vals

    n_eff = X_sub.shape[0]
    point = np.abs(X_sub).mean(axis=0)

    boot = np.empty((n_boot, p), dtype=float)
    for b in range(n_boot):
        ib = rng.choice(n_eff, size=n_eff, replace=True)
        boot[b] = np.abs(X_sub[ib]).mean(axis=0)

    lo = np.quantile(boot, alpha / 2.0, axis=0)
    hi = np.quantile(boot, 1.0 - alpha / 2.0, axis=0)
    return point, lo, hi, n_eff

def collect_margin_by_id(raw_log, split_map, cfg):
    """Collect margin (log-hazard) predictions by patient ID, averaged across folds."""
    rows = []
    for rec in raw_log:
        if "imp_idx" not in rec or "fold_idx" not in rec:
            continue
        key = (int(rec["imp_idx"]), int(rec["fold_idx"]))
        split_rec = split_map.get(key)
        if split_rec is None:
            continue

        val_ids = get_first(split_rec, VAL_ID_KEYS, [])
        margins = np.asarray(get_first(rec, cfg["margin_key_candidates"], []), dtype=float).ravel()

        if len(val_ids) != len(margins) or len(val_ids) == 0:
            continue

        # FIXED: Ensure IDs are strings for consistent matching
        for i, pid in enumerate(val_ids):
            rows.append((str(pid), float(margins[i])))

    if not rows:
        return pd.DataFrame(columns=["id", "margin"])

    return pd.DataFrame(rows, columns=["id", "margin"]).groupby("id", as_index=False)["margin"].mean()

def collect_risk_by_id(raw_log, split_map, hz_map, cfg, horizon):
    """Collect absolute risk predictions by patient ID at a specific horizon."""
    rows = []
    t = float(horizon)

    for rec in raw_log:
        if "imp_idx" not in rec or "fold_idx" not in rec:
            continue
        key = (int(rec["imp_idx"]), int(rec["fold_idx"]))

        split_rec = split_map.get(key)
        hz_rec = hz_map.get(key)

        if split_rec is None:
            continue

        val_ids = get_first(split_rec, VAL_ID_KEYS, [])
        margins = np.asarray(get_first(rec, cfg["margin_key_candidates"], []), dtype=float).ravel()
        if len(val_ids) != len(margins) or len(val_ids) == 0:
            continue

        risk_vec = None
        eval_times = np.asarray(rec.get("eval_times", []), dtype=float).ravel()
        probs_mat = np.asarray(get_first(rec, cfg["probs_key_candidates"], []), dtype=float)

        # Try to get risk from survival probability matrix
        if eval_times.size > 0 and probs_mat.ndim == 2:
            j = np.where(np.isclose(eval_times, t))[0]
            if j.size > 0:
                jj = int(j[0])
                if probs_mat.shape[0] == len(val_ids) and probs_mat.shape[1] == eval_times.size:
                    risk_vec = 1.0 - probs_mat[:, jj]
                elif probs_mat.shape[1] == len(val_ids) and probs_mat.shape[0] == eval_times.size:
                    risk_vec = 1.0 - probs_mat[jj, :]
                if risk_vec is not None:
                    risk_vec = np.asarray(risk_vec, dtype=float).ravel()

        # Fallback: compute from baseline hazard and margins
        if risk_vec is None and hz_rec is not None:
            times = get_first(hz_rec, cfg["hz_times_candidates"], [])
            h0_vals = get_first(hz_rec, cfg["hz_vals_candidates"], [])
            H0_t = h0_at_t(times, h0_vals, t)
            if np.isfinite(H0_t):
                surv = np.exp(-np.exp(margins) * H0_t)
                risk_vec = 1.0 - surv

        if risk_vec is not None and len(risk_vec) == len(val_ids):
            risk_vec = np.clip(np.asarray(risk_vec, dtype=float).ravel(), 0.0, 1.0)
            for i, pid in enumerate(val_ids):
                rv = float(risk_vec[i])
                if np.isfinite(rv):
                    rows.append((str(pid), rv))

    if not rows:
        return pd.DataFrame(columns=["id", "risk"])

    return pd.DataFrame(rows, columns=["id", "risk"]).groupby("id", as_index=False)["risk"].mean()

def collect_risk_samples_by_id(raw_log, split_map, hz_map, cfg, horizon):
    """Collect all risk samples by patient ID (for CI computation)."""
    rows = []
    t = float(horizon)

    for rec in raw_log:
        if "imp_idx" not in rec or "fold_idx" not in rec:
            continue
        key = (int(rec["imp_idx"]), int(rec["fold_idx"]))
        split_rec = split_map.get(key)
        hz_rec = hz_map.get(key)
        if split_rec is None:
            continue

        val_ids = get_first(split_rec, VAL_ID_KEYS, [])
        margins = np.asarray(get_first(rec, cfg["margin_key_candidates"], []), dtype=float).ravel()
        if len(val_ids) != len(margins) or len(val_ids) == 0:
            continue

        risk_vec = None
        eval_times = np.asarray(rec.get("eval_times", []), dtype=float).ravel()
        probs_mat = np.asarray(get_first(rec, cfg["probs_key_candidates"], []), dtype=float)

        # Try survival probability matrix
        if eval_times.size > 0 and probs_mat.ndim == 2:
            j = np.where(np.isclose(eval_times, t))[0]
            if j.size > 0:
                jj = int(j[0])
                if probs_mat.shape[0] == len(val_ids) and probs_mat.shape[1] == eval_times.size:
                    risk_vec = 1.0 - probs_mat[:, jj]
                elif probs_mat.shape[1] == len(val_ids) and probs_mat.shape[0] == eval_times.size:
                    risk_vec = 1.0 - probs_mat[jj, :]

        # Fallback to baseline hazard
        if risk_vec is None and hz_rec is not None:
            times = get_first(hz_rec, cfg["hz_times_candidates"], [])
            h0_vals = get_first(hz_rec, cfg["hz_vals_candidates"], [])
            H0_t = h0_at_t(times, h0_vals, t)
            if np.isfinite(H0_t):
                risk_vec = 1.0 - np.exp(-np.exp(margins) * H0_t)

        if risk_vec is None:
            continue

        risk_vec = np.clip(np.asarray(risk_vec, dtype=float).ravel(), 0.0, 1.0)
        if len(risk_vec) != len(val_ids):
            continue

        for i, pid in enumerate(val_ids):
            rv = float(risk_vec[i])
            if np.isfinite(rv):
                rows.append((str(pid), rv))

    if not rows:
        return pd.DataFrame(columns=["id", "risk"])

    return pd.DataFrame(rows, columns=["id", "risk"])

def summarize_risk_ci(df_samples, alpha=0.05):
    """Summarize risk samples with mean and quantile CI."""
    if df_samples.empty:
        return pd.DataFrame(columns=["id", "risk_mean", "risk_ci_low", "risk_ci_high", "n_samples"])

    g = df_samples.groupby("id")["risk"]
    out = g.agg(risk_mean="mean", n_samples="size").reset_index()
    out["risk_ci_low"] = g.quantile(alpha / 2.0).values
    out["risk_ci_high"] = g.quantile(1.0 - alpha / 2.0).values
    return out

def correlation_pairs_report(X_df, threshold=0.85):
    """Find highly correlated feature pairs."""
    X_num = X_df.apply(pd.to_numeric, errors="coerce")
    valid_cols = [c for c in X_num.columns if X_num[c].std(skipna=True) > 0]
    X_num = X_num[valid_cols]
    corr = X_num.corr(method="pearson")

    pairs = []
    cols = list(corr.columns)
    for i in range(len(cols)):
        for j in range(i + 1, len(cols)):
            r = corr.iat[i, j]
            if np.isfinite(r) and abs(r) >= threshold:
                pairs.append((cols[i], cols[j], float(r), float(abs(r))))

    pairs_df = pd.DataFrame(pairs, columns=["feature_1", "feature_2", "pearson_r", "abs_r"])
    if len(pairs_df):
        pairs_df = pairs_df.sort_values("abs_r", ascending=False).reset_index(drop=True)
    return pairs_df, corr

# -----------------------------
# 4) Prepare maps/horizons + multicollinearity check
# -----------------------------
split_map = {
    (int(s["imp_idx"]), int(s["fold_idx"])): s
    for s in cv_splits_log
    if "imp_idx" in s and "fold_idx" in s
}
hz_map = {
    (int(h["imp_idx"]), int(h["fold_idx"])): h
    for h in baseline_hazards_log
    if "imp_idx" in h and "fold_idx" in h
}

available_horizons = discover_horizons(raw_data_log)

# FIXED: Handle empty TARGET_HORIZONS and validate requested horizons
if TARGET_HORIZONS is None or len(TARGET_HORIZONS) == 0:
    horizons = available_horizons if available_horizons else [12.0]
else:
    horizons = sorted(set(float(h) for h in TARGET_HORIZONS))
    # Warn about missing horizons
    missing_h = set(horizons) - set(available_horizons)
    if missing_h:
        print(f"Warning: Requested horizons not in data: {sorted(missing_h)}")
        print(f"Available horizons: {available_horizons}")

corr_pairs_df, corr_mat = correlation_pairs_report(X_all, threshold=CORR_THRESHOLD)
corr_pairs_file = os.path.join(OUT_DIR, f"xgb8_dual_feature_corr_pairs_{FILE_TAG}.csv")
corr_mat_file = os.path.join(OUT_DIR, f"xgb8_dual_feature_corr_matrix_{FILE_TAG}.csv")
corr_pairs_df.to_csv(corr_pairs_file, index=False)
corr_mat.to_csv(corr_mat_file)

display(Markdown(
    f"### Step 8 SHAP (DUAL, Multi-Horizon)\n"
    f"- Source bundle tag: **{source_tag}**\n"
    f"- Run time: **{RUN_TS}**\n"
    f"- Patients: **{X_all.shape[0]}**\n"
    f"- Features: **{X_all.shape[1]}**\n"
    f"- Horizons (months): **{', '.join(fmt_horizon(h) for h in horizons)}**\n"
    f"- SHAP scale: **log-hazard**\n"
    f"- Multicollinearity threshold: **|r| >= {CORR_THRESHOLD:.2f}**"
))

if len(corr_pairs_df) > 0:
    display(Markdown(f"Found **{len(corr_pairs_df)}** correlated feature pairs (|r| >= {CORR_THRESHOLD:.2f})."))
    display(corr_pairs_df.head(20))
else:
    display(Markdown(f"No feature pairs above |r| >= {CORR_THRESHOLD:.2f}."))

# -----------------------------
# 5) Run SHAP per outcome and horizon
# -----------------------------
saved_plot_files = []
saved_out_files = [corr_pairs_file, corr_mat_file]
all_case_rows = []
horizon_rows = []
processed_outcomes = []

# FIXED: id_to_row uses string keys for consistent matching
id_to_row = {str(idx): i for i, idx in enumerate(X_all.index)}

for outcome_name, cfg in OUTCOME_CFG.items():
    shap_key = find_first_key(shap_data, cfg["shap_key_candidates"])
    if shap_key is None:
        print(f"Skipping {cfg['label']}: missing SHAP key among {cfg['shap_key_candidates']}.")
        continue

    shap_vals = np.asarray(shap_data[shap_key], dtype=float)
    if shap_vals.shape != X_all.shape:
        raise ValueError(f"{cfg['label']} SHAP shape mismatch: {shap_vals.shape} vs X_all {X_all.shape}")

    # Recover baseline on margin (log-hazard) scale
    df_margin = collect_margin_by_id(raw_data_log, split_map, cfg)
    df_shap_sum = pd.DataFrame({
        "id": X_all.index.astype(str),
        "shap_sum": shap_vals.sum(axis=1)
    })
    tmp = df_shap_sum.merge(df_margin, on="id", how="inner")
    base_margin = float((tmp["margin"] - tmp["shap_sum"]).mean()) if len(tmp) > 0 else 0.0

    explanation = shap.Explanation(
        values=shap_vals,
        base_values=np.full(X_all.shape[0], base_margin, dtype=float),
        data=X_all.to_numpy(),
        feature_names=feature_names
    )

    # Global mean |SHAP| + CI
    if BOOTSTRAP_CI:
        point, ci_low, ci_high, ci_n = bootstrap_mean_abs_shap(
            shap_vals,
            n_boot=N_BOOTSTRAP,
            alpha=CI_ALPHA,
            seed=2125,
            max_n=BOOTSTRAP_MAX_N
        )
    else:
        point = np.abs(shap_vals).mean(axis=0)
        ci_low = np.full_like(point, np.nan, dtype=float)
        ci_high = np.full_like(point, np.nan, dtype=float)
        ci_n = int(shap_vals.shape[0])

    df_top = pd.DataFrame({
        "outcome": cfg["label"],
        "feature": feature_names,
        "mean_abs_shap_log_hazard": point,
        "ci95_low": ci_low,
        "ci95_high": ci_high,
        "bootstrap_n": int(ci_n),
        "n_bootstrap": int(N_BOOTSTRAP if BOOTSTRAP_CI else 0),
        "ci_alpha": float(CI_ALPHA if BOOTSTRAP_CI else np.nan),
    }).sort_values("mean_abs_shap_log_hazard", ascending=False).reset_index(drop=True)

    top_file = os.path.join(OUT_DIR, f"xgb8_dual_{outcome_name}_shap_top_features_{FILE_TAG}.csv")
    df_top.to_csv(top_file, index=False)
    saved_out_files.append(top_file)

    processed_outcomes.append(outcome_name)

    display(Markdown(f"## {cfg['label']}"))
    display(Markdown("SHAP values and global importance are on the **log-hazard** scale."))
    display(df_top.head(20))

    # Bar plot with CI (log-hazard SHAP)
    df_bar = df_top.head(20).sort_values("mean_abs_shap_log_hazard", ascending=True)

    # FIXED: Explicit dtype specification
    x = df_bar["mean_abs_shap_log_hazard"].to_numpy(dtype=float)
    has_ci = BOOTSTRAP_CI and np.isfinite(df_bar["ci95_low"]).all() and np.isfinite(df_bar["ci95_high"]).all()

    plt.figure(figsize=(11, 8))
    if has_ci:
        lo = df_bar["ci95_low"].to_numpy(dtype=float)
        hi = df_bar["ci95_high"].to_numpy(dtype=float)
        xerr = np.vstack([np.clip(x - lo, 0.0, None), np.clip(hi - x, 0.0, None)])
        plt.barh(df_bar["feature"], x, xerr=xerr, color="#4C72B0", alpha=0.9, ecolor="black", capsize=2)
        plt.title(f"{cfg['label']} Global mean |SHAP| (log-hazard) with 95% bootstrap CI")
    else:
        plt.barh(df_bar["feature"], x, color="#4C72B0", alpha=0.9)
        plt.title(f"{cfg['label']} Global mean |SHAP| (log-hazard)")
    plt.xlabel("mean |SHAP| (log-hazard)")
    plt.tight_layout()
    saved_plot_files.extend(save_current_figure("bar_ci", outcome_name))
    plt.show()
    plt.close()

    # Beeswarm (distribution on log-hazard SHAP scale)
    if X_all.shape[0] > MAX_BEESWARM_N:
        idx = RNG.choice(X_all.shape[0], MAX_BEESWARM_N, replace=False)
        exp_bee = explanation[idx]
    else:
        exp_bee = explanation

    plt.figure(figsize=(12, 8))
    shap.plots.beeswarm(exp_bee, max_display=20, show=False)
    plt.title(f"{cfg['label']} SHAP Beeswarm (log-hazard scale)")
    plt.tight_layout()
    saved_plot_files.extend(save_current_figure("beeswarm", outcome_name))
    plt.show()
    plt.close()

    # Horizon-specific risk ranking + waterfalls
    for h in horizons:
        df_risk_samples = collect_risk_samples_by_id(raw_data_log, split_map, hz_map, cfg, h)
        # FIXED: Use CI_ALPHA instead of hardcoded 0.05
        df_risk = summarize_risk_ci(df_risk_samples, alpha=CI_ALPHA)
        
        # FIXED: Ensure string IDs for consistent matching
        df_risk["id"] = df_risk["id"].astype(str)
        df_risk = df_risk[df_risk["id"].isin(id_to_row.keys())]
        
        n_h = int(len(df_risk))
        n_total = int(X_all.shape[0])

        horizon_rows.append({
            "outcome": cfg["label"],
            "horizon_months": float(h),
            "n_patients_with_risk": n_h,
            "n_total_patients": n_total,
            "coverage_pct": (100.0 * n_h / n_total) if n_total > 0 else np.nan
        })

        if n_h == 0:
            print(f"{cfg['label']} @ {fmt_horizon(h)}m: no absolute risk available; skipping waterfalls.")
            continue

        # Strictly rank by absolute risk (not SHAP score/log-hazard score)
        hi = df_risk.sort_values("risk_mean", ascending=False).iloc[0]
        lo = df_risk.sort_values("risk_mean", ascending=True).iloc[0]
        
        high_id = str(hi["id"])
        low_id = str(lo["id"])

        high_risk = float(hi["risk_mean"])
        low_risk = float(lo["risk_mean"])

        high_low = float(hi["risk_ci_low"])
        high_high = float(hi["risk_ci_high"])
        low_low = float(lo["risk_ci_low"])
        low_high = float(lo["risk_ci_high"])

        high_n = int(hi["n_samples"])
        low_n = int(lo["n_samples"])

        high_row = id_to_row[high_id]
        low_row = id_to_row[low_id]

        all_case_rows.append({
            "outcome": cfg["label"],
            "horizon_months": float(h),
            "case": "highest",
            "id": high_id,
            "risk_at_horizon": high_risk,
            "n_patients_with_risk": n_h
        })
        all_case_rows.append({
            "outcome": cfg["label"],
            "horizon_months": float(h),
            "case": "lowest",
            "id": low_id,
            "risk_at_horizon": low_risk,
            "n_patients_with_risk": n_h
        })

        # Waterfall high risk (SHAP still log-hazard contribution)
        plt.figure(figsize=(10, 7))
        shap.plots.waterfall(explanation[high_row], max_display=12, show=False)
        plt.title(
            f"{cfg['label']} highest absolute risk @ {fmt_horizon(h)}m "
            f"(ID {high_id}, risk={high_risk:.3f} [{high_low:.3f}, {high_high:.3f}], n={high_n})\n"
            f"Waterfall shows SHAP contributions on log-hazard scale"
        )
        plt.tight_layout()
        saved_plot_files.extend(save_current_figure("waterfall_high", outcome_name, h))
        plt.show()
        plt.close()

        # Waterfall low risk
        plt.figure(figsize=(10, 7))
        shap.plots.waterfall(explanation[low_row], max_display=12, show=False)
        plt.title(
            f"{cfg['label']} lowest absolute risk @ {fmt_horizon(h)}m "
            f"(ID {low_id}, risk={low_risk:.3f} [{low_low:.3f}, {low_high:.3f}], n={low_n})\n"
            f"Waterfall shows SHAP contributions on log-hazard scale"
        )
        plt.tight_layout()
        saved_plot_files.extend(save_current_figure("waterfall_low", outcome_name, h))
        plt.show()
        plt.close()

# -----------------------------
# 6) Export combined outputs
# -----------------------------
cases_file = os.path.join(OUT_DIR, f"xgb8_dual_shap_extreme_cases_{FILE_TAG}.csv")
horizon_file = os.path.join(OUT_DIR, f"xgb8_dual_horizon_sample_sizes_{FILE_TAG}.csv")
info_file = os.path.join(OUT_DIR, f"xgb8_dual_shap_run_info_{FILE_TAG}.json")

df_cases = pd.DataFrame(all_case_rows)
df_horizon = pd.DataFrame(horizon_rows)

df_cases.to_csv(cases_file, index=False)
df_horizon.to_csv(horizon_file, index=False)

saved_out_files.extend([cases_file, horizon_file])

run_info = {
    "source_bundle_tag": source_tag,
    "run_timestamp": RUN_TS,
    "file_tag_used_for_outputs": FILE_TAG,
    "source_files": {
        "shap": shap_file,
        "raw": raw_file,
        "baseline_hazards": hz_file,
        "cv_splits": split_file
    },
    "n_patients": int(X_all.shape[0]),
    "n_features": int(X_all.shape[1]),
    "horizons_months": [float(h) for h in horizons],
    "available_eval_times_months": [float(h) for h in available_horizons],
    "outcomes_processed": processed_outcomes,
    "shap_scale": "log-hazard",
    "global_importance_metric": "mean absolute SHAP (log-hazard)",
    "bootstrap_ci": {
        "enabled": bool(BOOTSTRAP_CI),
        "n_bootstrap": int(N_BOOTSTRAP) if BOOTSTRAP_CI else 0,
        "alpha": float(CI_ALPHA) if BOOTSTRAP_CI else None,
        "max_n_for_bootstrap": int(BOOTSTRAP_MAX_N) if BOOTSTRAP_CI else None
    },
    "multicollinearity_check": {
        "method": "pairwise Pearson correlation",
        "threshold_abs_r": float(CORR_THRESHOLD),
        "pairs_file": corr_pairs_file,
        "corr_matrix_file": corr_mat_file
    },
    "note": "Absolute risk is used for extreme-case ranking at each horizon. SHAP values remain log-hazard contributions."
}
with open(info_file, "w", encoding="utf-8") as f:
    json.dump(run_info, f, indent=2)

saved_out_files.append(info_file)

print("\nSaved plots to _figs (PNG/PDF):")
for p in saved_plot_files:
    print(" -", p)

print("\nSaved tables/metadata to _out:")
for p in saved_out_files:
    print(" -", p)
