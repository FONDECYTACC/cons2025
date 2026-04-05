"""Export notebook-ready DeepSurv inputs for standalone SHAP debugging.

This helper is meant to be imported from the DeepSurv notebook after the
train/test split cell has assigned the training objects back into:

- ``imputations_list_mar26``
- ``y_surv_death_list``
- ``y_surv_readm_list``

Example usage inside the notebook:

    from _alt_scripts.export_deepsurv_shap_debug_bundle import (
        export_bundle_from_namespace,
    )

    bundle_path = export_bundle_from_namespace(
        globals(),
        project_root=PROJECT_ROOT,
    )
    print(bundle_path)
"""

from __future__ import annotations

import argparse
import importlib.util
import pickle
from datetime import datetime
from pathlib import Path
from typing import Any, Mapping, Sequence

import numpy as np
import pandas as pd


THIS_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = THIS_DIR.parent
CONFIG_PATH = PROJECT_ROOT / "deepsurv_consensus_config_mar26.py"

DEFAULT_PIPELINE_VERSION = "v2_ipcw_haligned_repro"
DEFAULT_SEED = 2125
DEFAULT_K_FOLDS = 5
DEFAULT_EVAL_HORIZONS = [3, 6, 9, 12, 24, 36, 48, 60, 72, 84, 96, 108]
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


def load_consensus_config() -> dict[str, Any]:
    if not CONFIG_PATH.exists():
        raise FileNotFoundError(f"Consensus config not found: {CONFIG_PATH}")

    spec = importlib.util.spec_from_file_location(
        "deepsurv_consensus_config_mar26",
        CONFIG_PATH,
    )
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load module spec from {CONFIG_PATH}")

    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    if hasattr(module, "get_deepsurv_consensus_config"):
        config = module.get_deepsurv_consensus_config()
    else:
        config = {
            "lr": module.BEST_LR,
            "weight_decay": module.BEST_WD,
            "batch_size": module.BEST_BATCH,
            "dropout": module.BEST_DROPOUT,
            "nodes": list(module.BEST_NODES),
        }

    required = {"lr", "weight_decay", "batch_size", "dropout", "nodes"}
    missing = required - set(config.keys())
    if missing:
        raise KeyError(f"Consensus config missing keys: {sorted(missing)}")

    return {
        "lr": float(config["lr"]),
        "weight_decay": float(config["weight_decay"]),
        "batch_size": int(config["batch_size"]),
        "dropout": float(config["dropout"]),
        "nodes": [int(x) for x in config["nodes"]],
    }


def find_project_root(start: Path | None = None) -> Path:
    cur = (start or PROJECT_ROOT).resolve()
    for candidate in (cur, *cur.parents):
        if (candidate / ".git").exists() or (candidate / "AGENTS.md").exists():
            return candidate
    return cur


def _as_dataframe_list(X_list: Sequence[Any]) -> list[pd.DataFrame]:
    if not isinstance(X_list, Sequence) or len(X_list) == 0:
        raise ValueError("X_list must be a non-empty sequence of DataFrames.")

    out: list[pd.DataFrame] = []
    for i, item in enumerate(X_list):
        if not isinstance(item, pd.DataFrame):
            raise TypeError(f"X_list[{i}] must be a pandas DataFrame.")
        df = item.copy()
        if not df.index.is_unique:
            raise ValueError(f"X_list[{i}] index is not unique.")
        out.append(df)
    return out


def _validate_surv_list(name: str, values: Sequence[Any], expected_len: int) -> list[np.ndarray]:
    if not isinstance(values, Sequence) or len(values) != expected_len:
        raise ValueError(f"{name} must have length {expected_len}.")

    out: list[np.ndarray] = []
    for i, arr in enumerate(values):
        np_arr = np.asarray(arr).copy()
        if np_arr.ndim != 1:
            raise ValueError(f"{name}[{i}] must be a 1D structured array.")
        if np_arr.dtype.names is None or set(np_arr.dtype.names) != {"event", "time"}:
            raise ValueError(
                f"{name}[{i}] must have structured dtype with fields 'event' and 'time'."
            )
        out.append(np_arr)
    return out


def _validate_alignment(
    X_list: Sequence[pd.DataFrame],
    y_death_list: Sequence[np.ndarray],
    y_readm_list: Sequence[np.ndarray],
) -> None:
    feature_columns = list(X_list[0].columns)
    for i, (X_df, y_death, y_readm) in enumerate(zip(X_list, y_death_list, y_readm_list)):
        if list(X_df.columns) != feature_columns:
            raise ValueError(f"X_list[{i}] columns do not match X_list[0].")
        if len(X_df) != len(y_death) or len(X_df) != len(y_readm):
            raise ValueError(
                f"Alignment error at imputation {i}: "
                f"X={len(X_df)}, y_death={len(y_death)}, y_readm={len(y_readm)}"
            )


def build_debug_bundle(
    *,
    X_list: Sequence[pd.DataFrame],
    y_death_list: Sequence[np.ndarray],
    y_readm_list: Sequence[np.ndarray],
    project_root: str | Path,
    pipeline_version: str = DEFAULT_PIPELINE_VERSION,
    seed: int = DEFAULT_SEED,
    k_folds: int = DEFAULT_K_FOLDS,
    eval_horizons: Sequence[int] = DEFAULT_EVAL_HORIZONS,
    consensus_config: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    X_frames = _as_dataframe_list(X_list)
    y_death = _validate_surv_list("y_death_list", y_death_list, len(X_frames))
    y_readm = _validate_surv_list("y_readm_list", y_readm_list, len(X_frames))
    _validate_alignment(X_frames, y_death, y_readm)

    config = dict(consensus_config or load_consensus_config())
    feature_columns = list(X_frames[0].columns)

    bundle = {
        "pipeline_version": str(pipeline_version),
        "seed": int(seed),
        "k_folds": int(k_folds),
        "eval_horizons": [int(x) for x in eval_horizons],
        "consensus_config": config,
        "X_list": X_frames,
        "y_death_list": y_death,
        "y_readm_list": y_readm,
        "project_root": str(Path(project_root).resolve()),
        "feature_columns": feature_columns,
    }

    validate_bundle(bundle)
    return bundle


def validate_bundle(bundle: Mapping[str, Any]) -> None:
    missing = REQUIRED_BUNDLE_KEYS - set(bundle.keys())
    if missing:
        raise KeyError(f"Bundle missing keys: {sorted(missing)}")

    X_list = bundle["X_list"]
    y_death_list = bundle["y_death_list"]
    y_readm_list = bundle["y_readm_list"]
    feature_columns = list(bundle["feature_columns"])

    if not isinstance(X_list, list) or not X_list:
        raise ValueError("Bundle X_list must be a non-empty list.")
    if not isinstance(feature_columns, list) or not feature_columns:
        raise ValueError("Bundle feature_columns must be a non-empty list.")

    _validate_alignment(X_list, y_death_list, y_readm_list)

    first_columns = list(X_list[0].columns)
    if first_columns != feature_columns:
        raise ValueError("feature_columns do not match the first DataFrame columns.")


def export_bundle(bundle: Mapping[str, Any], out_path: str | Path | None = None) -> Path:
    validate_bundle(bundle)

    if out_path is None:
        project_root = Path(bundle["project_root"]).resolve()
        out_dir = project_root / "_out"
        out_dir.mkdir(parents=True, exist_ok=True)
        ts = datetime.now().strftime("%Y%m%d_%H%M")
        out_path = out_dir / f"deepsurv_shap_debug_bundle_{ts}_mar26.pkl"
    else:
        out_path = Path(out_path).resolve()
        out_path.parent.mkdir(parents=True, exist_ok=True)

    with open(out_path, "wb") as f:
        pickle.dump(dict(bundle), f, protocol=pickle.HIGHEST_PROTOCOL)

    return out_path


def export_bundle_from_namespace(
    namespace: Mapping[str, Any],
    *,
    x_key: str = "imputations_list_mar26",
    y_death_key: str = "y_surv_death_list",
    y_readm_key: str = "y_surv_readm_list",
    project_root: str | Path | None = None,
    pipeline_version: str | None = None,
    seed: int | None = None,
    k_folds: int | None = None,
    eval_horizons: Sequence[int] | None = None,
    out_path: str | Path | None = None,
) -> Path:
    required = [x_key, y_death_key, y_readm_key]
    missing = [name for name in required if name not in namespace]
    if missing:
        raise KeyError(f"Namespace is missing required objects: {missing}")

    resolved_project_root = project_root or namespace.get("PROJECT_ROOT") or find_project_root()
    resolved_pipeline_version = pipeline_version or namespace.get(
        "PIPELINE_VERSION",
        DEFAULT_PIPELINE_VERSION,
    )
    resolved_seed = seed if seed is not None else int(namespace.get("SEED", DEFAULT_SEED))
    resolved_k_folds = k_folds if k_folds is not None else int(
        namespace.get("K_FOLDS_TEST", namespace.get("K_FOLDS", DEFAULT_K_FOLDS))
    )
    resolved_eval_horizons = list(
        eval_horizons
        if eval_horizons is not None
        else namespace.get(
            "EVAL_HORIZONS_TEST",
            namespace.get("EVAL_HORIZONS", DEFAULT_EVAL_HORIZONS),
        )
    )

    bundle = build_debug_bundle(
        X_list=namespace[x_key],
        y_death_list=namespace[y_death_key],
        y_readm_list=namespace[y_readm_key],
        project_root=resolved_project_root,
        pipeline_version=resolved_pipeline_version,
        seed=resolved_seed,
        k_folds=resolved_k_folds,
        eval_horizons=resolved_eval_horizons,
    )
    return export_bundle(bundle, out_path=out_path)


def get_notebook_usage_snippet() -> str:
    return "\n".join(
        [
            "from _alt_scripts.export_deepsurv_shap_debug_bundle import (",
            "    export_bundle_from_namespace,",
            ")",
            "",
            "bundle_path = export_bundle_from_namespace(",
            "    globals(),",
            "    project_root=PROJECT_ROOT,",
            "    x_key='imputations_list_mar26',",
            "    y_death_key='y_surv_death_list',",
            "    y_readm_key='y_surv_readm_list',",
            ")",
            "print(bundle_path)",
        ]
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Notebook-facing helper for exporting a DeepSurv SHAP debug bundle. "
            "This script is typically imported from the notebook rather than run "
            "standalone."
        )
    )
    parser.add_argument(
        "--print-snippet",
        action="store_true",
        help="Print the notebook snippet used to export the bundle.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.print_snippet:
        print(get_notebook_usage_snippet())
        return

    print(
        "This helper is intended to be imported from the DeepSurv notebook. "
        "Run with --print-snippet to see the notebook usage."
    )


if __name__ == "__main__":
    main()
