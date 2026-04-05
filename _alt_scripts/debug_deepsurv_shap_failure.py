"""Deterministic standalone reproducer for DeepSurv SHAP failures."""

from __future__ import annotations

import argparse
import importlib
import importlib.util
import json
import pickle
import platform
import sys
import traceback
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import shap
import torch
import torch.nn as nn
from pycox.models import CoxPH
from sklearn.model_selection import StratifiedKFold
from sklearn.preprocessing import StandardScaler
import torchtuples as tt


THIS_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = THIS_DIR.parent
CONFIG_PATH = PROJECT_ROOT / "deepsurv_consensus_config_mar26.py"

DEFAULT_MAX_EPOCHS = 100
REQUIRED_BUNDLE_KEYS = {
    "pipeline_version",
    "seed",
    "k_folds",
    "eval_horizons",
    "consensus_config",
    "X_list",
    "y_death_list",
    "y_readm_list",
    "project_root",
    "feature_columns",
}


class SqueezeNet(nn.Module):
    """SHAP explainers prefer a 1D output for single-risk networks."""

    def __init__(self, net: nn.Module):
        super().__init__()
        self.net = net

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return self.net(x).squeeze(-1)


@dataclass
class ProbeResult:
    probe_name: str
    outcome: str
    status: str
    shape: list[int] | None
    bg_size: int | None
    test_size: int | None
    device: str
    error: str | None = None


def load_consensus_module():
    if not CONFIG_PATH.exists():
        raise FileNotFoundError(f"Consensus config not found: {CONFIG_PATH}")

    spec = importlib.util.spec_from_file_location(
        "deepsurv_consensus_config_mar26",
        CONFIG_PATH,
    )
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load config module from {CONFIG_PATH}")

    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def load_consensus_config() -> dict[str, Any]:
    module = load_consensus_module()
    if hasattr(module, "get_deepsurv_consensus_config"):
        cfg = module.get_deepsurv_consensus_config()
    else:
        cfg = {
            "lr": module.BEST_LR,
            "weight_decay": module.BEST_WD,
            "batch_size": module.BEST_BATCH,
            "dropout": module.BEST_DROPOUT,
            "nodes": list(module.BEST_NODES),
        }
    return {
        "lr": float(cfg["lr"]),
        "weight_decay": float(cfg["weight_decay"]),
        "batch_size": int(cfg["batch_size"]),
        "dropout": float(cfg["dropout"]),
        "nodes": [int(x) for x in cfg["nodes"]],
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bundle", required=True, help="Path to the debug bundle pickle.")
    parser.add_argument("--imp-idx", type=int, default=0)
    parser.add_argument("--fold-idx", type=int, default=0)
    parser.add_argument("--device", choices=["auto", "cpu", "cuda"], default="auto")
    parser.add_argument("--bg-size", type=int, default=100)
    parser.add_argument("--test-cap", type=int, default=500)
    parser.add_argument("--seed", type=int, default=2125)
    parser.add_argument("--out-dir", default=None)
    return parser.parse_args()


def json_default(obj: Any) -> Any:
    if isinstance(obj, np.integer):
        return int(obj)
    if isinstance(obj, np.floating):
        return float(obj)
    if isinstance(obj, np.bool_):
        return bool(obj)
    if isinstance(obj, Path):
        return str(obj)
    raise TypeError(f"Object of type {type(obj).__name__} is not JSON serializable")


def write_json(path: Path, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, default=json_default)


def set_global_seed(seed: int) -> None:
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
        torch.cuda.manual_seed_all(seed)
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = False


def resolve_device(requested: str) -> str:
    if requested == "auto":
        return "cuda" if torch.cuda.is_available() else "cpu"
    if requested == "cuda" and not torch.cuda.is_available():
        raise RuntimeError("CUDA was requested but is not available.")
    return requested


def module_version(name: str) -> str | None:
    try:
        module = importlib.import_module(name)
    except Exception:
        return None
    return getattr(module, "__version__", None)


def build_run_info(selected_device: str) -> dict[str, Any]:
    return {
        "timestamp": datetime.now().strftime("%Y%m%d_%H%M%S"),
        "python_version": sys.version,
        "platform": platform.platform(),
        "torch_version": torch.__version__,
        "shap_version": shap.__version__,
        "numpy_version": np.__version__,
        "pycox_version": module_version("pycox"),
        "torchtuples_version": module_version("torchtuples"),
        "cuda_available": bool(torch.cuda.is_available()),
        "selected_device": selected_device,
        "gpu_name": torch.cuda.get_device_name(0) if torch.cuda.is_available() else None,
        "torch_deterministic": bool(torch.backends.cudnn.deterministic) if torch.cuda.is_available() else None,
        "torch_benchmark": bool(torch.backends.cudnn.benchmark) if torch.cuda.is_available() else None,
    }


def load_bundle(path: str | Path) -> dict[str, Any]:
    path = Path(path).resolve()
    if not path.exists():
        raise FileNotFoundError(f"Bundle not found: {path}")
    with open(path, "rb") as f:
        bundle = pickle.load(f)
    if not isinstance(bundle, dict):
        raise TypeError("Bundle must be a dictionary.")
    missing = REQUIRED_BUNDLE_KEYS - set(bundle.keys())
    if missing:
        raise KeyError(f"Bundle missing keys: {sorted(missing)}")
    return bundle


def validate_bundle(bundle: dict[str, Any], imp_idx: int) -> None:
    X_list = bundle["X_list"]
    y_death_list = bundle["y_death_list"]
    y_readm_list = bundle["y_readm_list"]
    feature_columns = list(bundle["feature_columns"])

    if not isinstance(X_list, list) or not X_list:
        raise ValueError("Bundle X_list must be a non-empty list.")
    if imp_idx < 0 or imp_idx >= len(X_list):
        raise IndexError(f"imp_idx={imp_idx} out of range for {len(X_list)} imputations.")
    if len(y_death_list) != len(X_list) or len(y_readm_list) != len(X_list):
        raise ValueError("X_list, y_death_list, and y_readm_list must have equal length.")

    for i, (X_df, y_death, y_readm) in enumerate(zip(X_list, y_death_list, y_readm_list)):
        if not isinstance(X_df, pd.DataFrame):
            raise TypeError(f"X_list[{i}] must be a DataFrame.")
        if list(X_df.columns) != feature_columns:
            raise ValueError(f"X_list[{i}] feature columns do not match bundle feature_columns.")
        if not X_df.index.is_unique:
            raise ValueError(f"X_list[{i}] index is not unique.")
        if len(X_df) != len(y_death) or len(X_df) != len(y_readm):
            raise ValueError(
                f"Alignment error at imputation {i}: "
                f"X={len(X_df)}, y_death={len(y_death)}, y_readm={len(y_readm)}"
            )
        for name, arr in (("y_death_list", y_death), ("y_readm_list", y_readm)):
            if arr.dtype.names is None or set(arr.dtype.names) != {"event", "time"}:
                raise ValueError(f"{name}[{i}] must have fields 'event' and 'time'.")


def pick_first_existing(df: pd.DataFrame, candidates: list[str]) -> str | None:
    for col in candidates:
        if col in df.columns:
            return col
    return None


def build_plan_idx(X_curr: pd.DataFrame) -> np.ndarray:
    col_pg_pab = pick_first_existing(X_curr, ["plan_type_corr_pg_pab", "plan_type_corr_pg-pab"])
    col_pg_pr = pick_first_existing(X_curr, ["plan_type_corr_pg_pr", "plan_type_corr_pg-pr"])
    col_pg_pai = pick_first_existing(X_curr, ["plan_type_corr_pg_pai", "plan_type_corr_pg-pai"])
    col_m_pr = pick_first_existing(X_curr, ["plan_type_corr_m_pr", "plan_type_corr_m-pr"])
    col_m_pai = pick_first_existing(X_curr, ["plan_type_corr_m_pai", "plan_type_corr_m-pai"])

    plan_idx = np.zeros(len(X_curr), dtype=int)
    if col_pg_pr is not None:
        plan_idx[X_curr[col_pg_pr].astype(int) == 1] = 2
    if col_pg_pai is not None:
        plan_idx[X_curr[col_pg_pai].astype(int) == 1] = 3
    if col_m_pr is not None:
        plan_idx[X_curr[col_m_pr].astype(int) == 1] = 4
    if col_m_pai is not None:
        plan_idx[X_curr[col_m_pai].astype(int) == 1] = 5
    if col_pg_pab is not None:
        plan_idx[X_curr[col_pg_pab].astype(int) == 1] = 1
    else:
        non_ref_cols = [c for c in [col_pg_pr, col_pg_pai, col_m_pr, col_m_pai] if c is not None]
        if non_ref_cols:
            inferred_pg_pab = X_curr[non_ref_cols].astype(int).sum(axis=1) == 0
            plan_idx[inferred_pg_pab.to_numpy()] = 1
    return plan_idx


def build_competing_risk_targets(
    X_df: pd.DataFrame,
    y_death: np.ndarray,
    y_readm: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    t_d = np.asarray(y_death["time"], dtype=float)
    e_d = np.asarray(y_death["event"]).astype(bool)
    t_r = np.asarray(y_readm["time"], dtype=float)
    e_r = np.asarray(y_readm["event"]).astype(bool)

    if not (len(X_df) == len(t_d) == len(t_r)):
        raise ValueError("Input lengths do not align when building competing-risk targets.")

    events = np.zeros(len(X_df), dtype=int)
    times = t_d.astype(np.float32).copy()

    mask_r = e_r & (t_r <= t_d)
    events[mask_r] = 2
    times[mask_r] = t_r[mask_r].astype(np.float32)

    mask_d = e_d & (~mask_r)
    events[mask_d] = 1

    return events, times, build_plan_idx(X_df)


def choose_fold_indices(
    X_df: pd.DataFrame,
    events: np.ndarray,
    plan_idx: np.ndarray,
    k_folds: int,
    seed: int,
    fold_idx: int,
) -> tuple[np.ndarray, np.ndarray]:
    strat_labels = (events * 10) + plan_idx
    skf = StratifiedKFold(n_splits=k_folds, shuffle=True, random_state=seed)
    folds = list(skf.split(X_df, strat_labels))
    if fold_idx < 0 or fold_idx >= len(folds):
        raise IndexError(f"fold_idx={fold_idx} out of range for {len(folds)} folds.")
    return folds[fold_idx]


def ensure_scaled_finite(name: str, array: np.ndarray) -> None:
    if not np.isfinite(array).all():
        raise ValueError(f"{name} contains NaN or inf after scaling.")


def fit_deepsurv_model(
    *,
    X_train_s: np.ndarray,
    t_train: np.ndarray,
    e_train_bin: np.ndarray,
    X_val_s: np.ndarray,
    t_val: np.ndarray,
    e_val_bin: np.ndarray,
    config: dict[str, Any],
    device: str,
    checkpoint_tag: str,
    out_dir: Path,
) -> tuple[CoxPH, dict[str, Any]]:
    net = tt.practical.MLPVanilla(
        in_features=X_train_s.shape[1],
        num_nodes=list(config["nodes"]),
        out_features=1,
        batch_norm=True,
        dropout=float(config["dropout"]),
        output_bias=False,
    )
    model = CoxPH(net, tt.optim.Adam)
    model.set_device(device)
    model.optimizer.set_lr(float(config["lr"]))
    model.optimizer.param_groups[0]["weight_decay"] = float(config["weight_decay"])

    y_train_cs = (t_train.astype("float32"), e_train_bin.astype("int64"))
    y_val_cs = (t_val.astype("float32"), e_val_bin.astype("int64"))
    ckpt_path = out_dir / f"tt_es_{checkpoint_tag}.pt"

    train_log = model.fit(
        X_train_s,
        y_train_cs,
        batch_size=int(config["batch_size"]),
        epochs=DEFAULT_MAX_EPOCHS,
        callbacks=[
            tt.callbacks.EarlyStopping(
                patience=15,
                file_path=ckpt_path,
                load_best=True,
                rm_file=True,
            )
        ],
        verbose=False,
        val_data=(X_val_s, y_val_cs),
    )
    model.compute_baseline_hazards(X_train_s, y_train_cs)

    diagnostics = {
        "epochs_trained": int(len(getattr(train_log, "epochs", []))),
        "loss_history_length": int(len(getattr(train_log, "loss", []))),
        "model_device_before_shap": str(model.device),
        "baseline_hazard_len": int(len(model.baseline_hazards_)),
        "baseline_cumulative_hazard_len": int(len(model.baseline_cumulative_hazards_)),
    }
    return model, diagnostics


def select_probe_samples(
    *,
    X_train_s: np.ndarray,
    X_val_s: np.ndarray,
    X_val_raw: pd.DataFrame,
    val_ids: list[Any],
    seed: int,
    fold_idx: int,
    bg_size: int,
    test_cap: int,
) -> dict[str, Any]:
    rng = np.random.RandomState(seed + fold_idx)

    bg_n = min(int(bg_size), len(X_train_s))
    bg_idx = rng.choice(len(X_train_s), size=bg_n, replace=False)
    bg_data = X_train_s[bg_idx]

    if len(X_val_s) > int(test_cap):
        test_idx = rng.choice(len(X_val_s), size=int(test_cap), replace=False)
        test_data = X_val_s[test_idx]
        test_ids = [val_ids[i] for i in test_idx]
        test_raw = X_val_raw.iloc[test_idx].copy()
    else:
        test_data = X_val_s.copy()
        test_ids = list(val_ids)
        test_raw = X_val_raw.copy()

    tiny_rng = np.random.RandomState(seed + 1000 + fold_idx)
    tiny_bg_n = min(8, len(X_train_s))
    tiny_test_n = min(16, len(X_val_s))
    tiny_bg_idx = tiny_rng.choice(len(X_train_s), size=tiny_bg_n, replace=False)
    tiny_test_idx = tiny_rng.choice(len(X_val_s), size=tiny_test_n, replace=False)

    return {
        "bg_data": bg_data.astype(np.float32, copy=False),
        "test_data": test_data.astype(np.float32, copy=False),
        "test_ids": test_ids,
        "test_raw": test_raw,
        "tiny_bg_data": X_train_s[tiny_bg_idx].astype(np.float32, copy=False),
        "tiny_test_data": X_val_s[tiny_test_idx].astype(np.float32, copy=False),
    }


@contextmanager
def model_device_context(model: CoxPH, device: str):
    original_device = str(model.device)
    was_training = model.net.training
    model.set_device(device)
    try:
        yield
    finally:
        if was_training:
            model.net.train()
        else:
            model.net.eval()
        model.set_device(original_device)


def ensure_shap_array(values: Any) -> np.ndarray:
    if isinstance(values, list):
        values = values[0]
    arr = np.asarray(values)
    if arr.ndim == 1:
        arr = arr[:, None]
    return arr


def run_forward_probe(model: CoxPH, bg_data: np.ndarray, test_data: np.ndarray, device: str) -> tuple[list[int], None]:
    with model_device_context(model, device):
        model.net.eval()
        sq = SqueezeNet(model.net)
        bg_t = torch.tensor(bg_data, dtype=torch.float32, device=device)
        test_t = torch.tensor(test_data, dtype=torch.float32, device=device)
        with torch.no_grad():
            out_bg = sq(bg_t)
            out_test = sq(test_t)
        if out_bg.ndim != 1 or out_test.ndim != 1:
            raise ValueError(f"Forward outputs must be 1D, got {tuple(out_bg.shape)} and {tuple(out_test.shape)}")
        if not torch.isfinite(out_bg).all() or not torch.isfinite(out_test).all():
            raise ValueError("Forward outputs contain non-finite values.")
        return [int(out_test.shape[0])], None


def run_gradient_probe(model: CoxPH, test_data: np.ndarray, device: str) -> tuple[list[int], None]:
    with model_device_context(model, device):
        model.net.eval()
        model.net.zero_grad(set_to_none=True)
        sq = SqueezeNet(model.net)
        probe_data = test_data[: min(4, len(test_data))]
        probe_t = torch.tensor(probe_data, dtype=torch.float32, device=device, requires_grad=True)
        out = sq(probe_t)
        if out.ndim != 1:
            raise ValueError(f"Gradient probe output must be 1D, got {tuple(out.shape)}")
        out.sum().backward()
        grad_ok = False
        for param in model.net.parameters():
            if param.grad is None:
                continue
            if not torch.isfinite(param.grad).all():
                raise ValueError("Model gradient contains non-finite values.")
            grad_ok = True
        if not grad_ok:
            raise ValueError("No gradients were produced during the gradient probe.")
        if probe_t.grad is None or not torch.isfinite(probe_t.grad).all():
            raise ValueError("Input gradient is missing or non-finite.")
        model.net.zero_grad(set_to_none=True)
        return [int(out.shape[0])], None


def run_deep_explainer_probe(
    model: CoxPH,
    bg_data: np.ndarray,
    test_data: np.ndarray,
    device: str,
) -> tuple[list[int], np.ndarray]:
    with model_device_context(model, device):
        model.net.eval()
        sq = SqueezeNet(model.net)
        bg_t = torch.tensor(bg_data, dtype=torch.float32, device=device)
        test_t = torch.tensor(test_data, dtype=torch.float32, device=device)
        explainer = shap.DeepExplainer(sq, bg_t)
        values = ensure_shap_array(explainer.shap_values(test_t))
        if values.shape[0] != len(test_data):
            raise ValueError(f"SHAP rows {values.shape[0]} do not match test size {len(test_data)}")
        return list(values.shape), values


def run_gradient_explainer_probe(
    model: CoxPH,
    bg_data: np.ndarray,
    test_data: np.ndarray,
    device: str,
) -> tuple[list[int], np.ndarray]:
    with model_device_context(model, device):
        model.net.eval()
        sq = SqueezeNet(model.net)
        bg_t = torch.tensor(bg_data, dtype=torch.float32, device=device)
        test_t = torch.tensor(test_data, dtype=torch.float32, device=device)
        explainer = shap.GradientExplainer(sq, bg_t)
        values = ensure_shap_array(explainer.shap_values(test_t))
        if values.shape[0] != len(test_data):
            raise ValueError(f"GradientExplainer rows {values.shape[0]} do not match test size {len(test_data)}")
        return list(values.shape), values


def record_probe(
    *,
    probe_name: str,
    outcome: str,
    bg_data: np.ndarray | None,
    test_data: np.ndarray | None,
    device: str,
    runner,
    save_prefix: str | None,
    out_dir: Path,
    probe_results: list[dict[str, Any]],
    error_tracebacks: list[dict[str, Any]],
) -> None:
    try:
        shape, values = runner()
        if values is not None and save_prefix is not None:
            np.save(out_dir / f"{save_prefix}_shap_values_probe_{probe_name}.npy", values)
        probe_results.append(
            ProbeResult(
                probe_name=probe_name,
                outcome=outcome,
                status="success",
                shape=shape,
                bg_size=None if bg_data is None else int(len(bg_data)),
                test_size=None if test_data is None else int(len(test_data)),
                device=device,
                error=None,
            ).__dict__
        )
    except Exception as exc:
        probe_results.append(
            ProbeResult(
                probe_name=probe_name,
                outcome=outcome,
                status="failure",
                shape=None,
                bg_size=None if bg_data is None else int(len(bg_data)),
                test_size=None if test_data is None else int(len(test_data)),
                device=device,
                error=repr(exc),
            ).__dict__
        )
        error_tracebacks.append(
            {
                "probe_name": probe_name,
                "outcome": outcome,
                "error": repr(exc),
                "traceback": traceback.format_exc(),
            }
        )


def summarize_probe_status(probe_results: list[dict[str, Any]], probe_name: str) -> list[str]:
    return [row["status"] for row in probe_results if row["probe_name"] == probe_name]


def any_success(probe_results: list[dict[str, Any]], probe_name: str) -> bool:
    return any(status == "success" for status in summarize_probe_status(probe_results, probe_name))


def any_failure(probe_results: list[dict[str, Any]], probe_name: str) -> bool:
    return any(status == "failure" for status in summarize_probe_status(probe_results, probe_name))


def all_failure(probe_results: list[dict[str, Any]], probe_name: str) -> bool:
    statuses = summarize_probe_status(probe_results, probe_name)
    return bool(statuses) and all(status == "failure" for status in statuses)


def classify_failure(probe_results: list[dict[str, Any]]) -> str:
    forward_failed = any_failure(probe_results, "forward_sanity")
    grad_failed = any_failure(probe_results, "gradient_sanity")
    exact_failed = all_failure(probe_results, "exact_notebook_selected_device")
    cpu_passed = any_success(probe_results, "exact_notebook_cpu")
    cpu_failed = all_failure(probe_results, "exact_notebook_cpu")
    tiny_passed = any_success(probe_results, "tiny_sample_selected_device")
    tiny_failed = all_failure(probe_results, "tiny_sample_selected_device")
    grad_exp_passed = any_success(probe_results, "gradient_explainer_tiny_cpu")
    grad_exp_failed = all_failure(probe_results, "gradient_explainer_tiny_cpu")

    if forward_failed:
        return "model_forward_output_issue"
    if not forward_failed and grad_failed:
        return "autograd_gradient_issue"
    if exact_failed and cpu_passed:
        return "cuda_device_interaction_issue"
    if exact_failed and cpu_failed and tiny_passed:
        return "scale_or_memory_issue"
    if exact_failed and cpu_failed and tiny_failed and grad_exp_passed:
        return "deepexplainer_specific_incompatibility"
    if (
        any_success(probe_results, "forward_sanity")
        and any_success(probe_results, "gradient_sanity")
        and exact_failed
        and cpu_failed
        and tiny_failed
        and grad_exp_failed
    ):
        return "shap_model_stack_incompatibility"
    if any_success(probe_results, "exact_notebook_selected_device"):
        return "probe_success_no_failure_reproduced"
    return "mixed_or_inconclusive"


def main() -> None:
    args = parse_args()
    selected_device = resolve_device(args.device)
    set_global_seed(args.seed)

    bundle = load_bundle(args.bundle)
    validate_bundle(bundle, args.imp_idx)
    config = load_consensus_config()

    bundle_project_root = Path(bundle["project_root"]).resolve()
    if args.out_dir:
        out_dir = Path(args.out_dir).resolve()
    else:
        ts = datetime.now().strftime("%Y%m%d_%H%M")
        out_dir = bundle_project_root / "_out" / f"shap_debug_{ts}"
    out_dir.mkdir(parents=True, exist_ok=True)

    run_info = build_run_info(selected_device)
    run_info.update(
        {
            "bundle_path": str(Path(args.bundle).resolve()),
            "project_root": str(bundle_project_root),
            "imp_idx": int(args.imp_idx),
            "fold_idx": int(args.fold_idx),
            "requested_device": args.device,
            "bg_size": int(args.bg_size),
            "test_cap": int(args.test_cap),
        }
    )
    write_json(out_dir / "run_info.json", run_info)

    probe_results: list[dict[str, Any]] = []
    error_tracebacks: list[dict[str, Any]] = []

    X_df = bundle["X_list"][args.imp_idx].copy()
    y_death = np.asarray(bundle["y_death_list"][args.imp_idx]).copy()
    y_readm = np.asarray(bundle["y_readm_list"][args.imp_idx]).copy()

    events, times, plan_idx = build_competing_risk_targets(X_df, y_death, y_readm)
    train_idx, val_idx = choose_fold_indices(
        X_df,
        events,
        plan_idx,
        k_folds=int(bundle["k_folds"]),
        seed=int(args.seed),
        fold_idx=int(args.fold_idx),
    )

    X_train_df = X_df.iloc[train_idx].copy()
    X_val_df = X_df.iloc[val_idx].copy()
    val_ids = X_df.index[val_idx].tolist()

    X_train = X_train_df.to_numpy(dtype=np.float32, copy=True)
    X_val = X_val_df.to_numpy(dtype=np.float32, copy=True)
    scaler = StandardScaler().fit(X_train)
    X_train_s = scaler.transform(X_train).astype(np.float32)
    X_val_s = scaler.transform(X_val).astype(np.float32)
    ensure_scaled_finite("X_train_s", X_train_s)
    ensure_scaled_finite("X_val_s", X_val_s)

    t_train = times[train_idx]
    t_val = times[val_idx]
    e_train = events[train_idx]
    e_val = events[val_idx]

    fold_summary = {
        "train_size": int(len(train_idx)),
        "val_size": int(len(val_idx)),
        "feature_count": int(X_df.shape[1]),
        "train_event_counts": {str(i): int((e_train == i).sum()) for i in (0, 1, 2)},
        "val_event_counts": {str(i): int((e_val == i).sum()) for i in (0, 1, 2)},
        "selected_device": selected_device,
    }
    write_json(out_dir / "fold_summary.json", fold_summary)

    e_train_d = (e_train == 1).astype(np.int64)
    e_val_d = (e_val == 1).astype(np.int64)
    e_train_r = (e_train == 2).astype(np.int64)
    e_val_r = (e_val == 2).astype(np.int64)

    death_model, death_diag = fit_deepsurv_model(
        X_train_s=X_train_s,
        t_train=t_train,
        e_train_bin=e_train_d,
        X_val_s=X_val_s,
        t_val=t_val,
        e_val_bin=e_val_d,
        config=config,
        device=selected_device,
        checkpoint_tag=f"imp{args.imp_idx}_fold{args.fold_idx}_death",
        out_dir=out_dir,
    )
    readm_model, readm_diag = fit_deepsurv_model(
        X_train_s=X_train_s,
        t_train=t_train,
        e_train_bin=e_train_r,
        X_val_s=X_val_s,
        t_val=t_val,
        e_val_bin=e_val_r,
        config=config,
        device=selected_device,
        checkpoint_tag=f"imp{args.imp_idx}_fold{args.fold_idx}_readmission",
        out_dir=out_dir,
    )

    fold_summary["training"] = {
        "Death": death_diag,
        "Readmission": readm_diag,
    }
    write_json(out_dir / "fold_summary.json", fold_summary)

    sample_pack = select_probe_samples(
        X_train_s=X_train_s,
        X_val_s=X_val_s,
        X_val_raw=X_val_df,
        val_ids=val_ids,
        seed=int(args.seed),
        fold_idx=int(args.fold_idx),
        bg_size=int(args.bg_size),
        test_cap=int(args.test_cap),
    )

    outcome_models = {
        "Death": death_model,
        "Readmission": readm_model,
    }

    for outcome_name, model in outcome_models.items():
        save_prefix = outcome_name.lower()

        record_probe(
            probe_name="forward_sanity",
            outcome=outcome_name,
            bg_data=sample_pack["bg_data"],
            test_data=sample_pack["test_data"],
            device=selected_device,
            runner=lambda model=model: run_forward_probe(
                model,
                sample_pack["bg_data"],
                sample_pack["test_data"],
                selected_device,
            ),
            save_prefix=None,
            out_dir=out_dir,
            probe_results=probe_results,
            error_tracebacks=error_tracebacks,
        )

        record_probe(
            probe_name="gradient_sanity",
            outcome=outcome_name,
            bg_data=None,
            test_data=sample_pack["test_data"][: min(4, len(sample_pack["test_data"]))],
            device=selected_device,
            runner=lambda model=model: run_gradient_probe(
                model,
                sample_pack["test_data"],
                selected_device,
            ),
            save_prefix=None,
            out_dir=out_dir,
            probe_results=probe_results,
            error_tracebacks=error_tracebacks,
        )

        record_probe(
            probe_name="exact_notebook_selected_device",
            outcome=outcome_name,
            bg_data=sample_pack["bg_data"],
            test_data=sample_pack["test_data"],
            device=selected_device,
            runner=lambda model=model: run_deep_explainer_probe(
                model,
                sample_pack["bg_data"],
                sample_pack["test_data"],
                selected_device,
            ),
            save_prefix=save_prefix,
            out_dir=out_dir,
            probe_results=probe_results,
            error_tracebacks=error_tracebacks,
        )

        record_probe(
            probe_name="exact_notebook_cpu",
            outcome=outcome_name,
            bg_data=sample_pack["bg_data"],
            test_data=sample_pack["test_data"],
            device="cpu",
            runner=lambda model=model: run_deep_explainer_probe(
                model,
                sample_pack["bg_data"],
                sample_pack["test_data"],
                "cpu",
            ),
            save_prefix=save_prefix,
            out_dir=out_dir,
            probe_results=probe_results,
            error_tracebacks=error_tracebacks,
        )

        record_probe(
            probe_name="tiny_sample_selected_device",
            outcome=outcome_name,
            bg_data=sample_pack["tiny_bg_data"],
            test_data=sample_pack["tiny_test_data"],
            device=selected_device,
            runner=lambda model=model: run_deep_explainer_probe(
                model,
                sample_pack["tiny_bg_data"],
                sample_pack["tiny_test_data"],
                selected_device,
            ),
            save_prefix=save_prefix,
            out_dir=out_dir,
            probe_results=probe_results,
            error_tracebacks=error_tracebacks,
        )

        record_probe(
            probe_name="gradient_explainer_tiny_cpu",
            outcome=outcome_name,
            bg_data=sample_pack["tiny_bg_data"],
            test_data=sample_pack["tiny_test_data"],
            device="cpu",
            runner=lambda model=model: run_gradient_explainer_probe(
                model,
                sample_pack["tiny_bg_data"],
                sample_pack["tiny_test_data"],
                "cpu",
            ),
            save_prefix=save_prefix,
            out_dir=out_dir,
            probe_results=probe_results,
            error_tracebacks=error_tracebacks,
        )

    write_json(out_dir / "probe_results.json", probe_results)
    write_json(out_dir / "error_tracebacks.json", error_tracebacks)

    run_info["likely_failure_class"] = classify_failure(probe_results)
    run_info["probe_count"] = int(len(probe_results))
    run_info["error_count"] = int(len(error_tracebacks))
    write_json(out_dir / "run_info.json", run_info)

    print(
        json.dumps(
            {
                "out_dir": str(out_dir),
                "likely_failure_class": run_info["likely_failure_class"],
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
