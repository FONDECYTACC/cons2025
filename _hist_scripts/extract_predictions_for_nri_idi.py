"""
Extract predictions from XGBoost and Cox models for NRI/IDI calculation.

This script:
1. Loads XGBoost predictions from pickle file
2. Loads Cox predictions from results_boot RDS file
3. Extracts predictions at specified horizon
4. Saves combined CSV for NRI/IDI analysis in R

Usage:
    python cons/_hist_scripts/extract_predictions_for_nri_idi.py \
        --xgb-pickle "xgb/_out/xgb6_corr_DUAL_final_ev_hyp_20260223_2134.pkl" \
        --cox-rds "path/to/results_boot.rds" \
        --risk death \
        --horizon 12 \
        --output "predictions_for_nri_idi.csv"
"""

import pickle
import argparse
import numpy as np
import pandas as pd
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr


def load_xgb_predictions(pickle_path):
    """Load XGBoost predictions from pickle file."""
    with open(pickle_path, 'rb') as f:
        data = pickle.load(f)
    return data


def extract_xgb_at_horizon(xgb_data, risk, horizon):
    """Extract XGBoost predictions at specific horizon."""
    all_preds = []
    
    for fold in xgb_data:
        eval_times = fold['eval_times']
        h_idx = eval_times.index(horizon) if horizon in eval_times else None
        
        if h_idx is None:
            raise ValueError(f"Horizon {horizon} not found in XGBoost eval_times: {eval_times}")
        
        if risk == 'death':
            surv_mat = 1 - fold['probs_death_matrix']  # Convert risk to survival
            y = fold['y_val_d']
        else:
            surv_mat = 1 - fold['probs_readm_matrix']
            y = fold['y_val_r']
        
        pred_risk = 1 - surv_mat[:, h_idx]  # Survival to risk
        
        # Extract event and time from structured array
        events = np.array([x[0] for x in y])
        times = np.array([x[1] for x in y])
        
        df = pd.DataFrame({
            'fold_id': fold['fold_idx'],
            'patient_idx': range(len(pred_risk)),
            'pred_xgb': pred_risk,
            'time': times,
            'event': events.astype(int)
        })
        all_preds.append(df)
    
    return pd.concat(all_preds, ignore_index=True)


def load_cox_predictions(rds_path, risk, horizon):
    """Load Cox predictions from RDS file using rpy2."""
    # Read RDS file
    readRDS = ro.r['readRDS']
    results_boot = readRDS(rds_path)
    
    # Extract raw_predictions
    raw_predictions = results_boot.rx2('raw_predictions')
    config = results_boot.rx2('config')
    eval_times = list(config.rx2('eval_times'))
    
    h_idx = eval_times.index(horizon) if horizon in eval_times else None
    if h_idx is None:
        raise ValueError(f"Horizon {horizon} not found in Cox eval_times: {eval_times}")
    
    all_preds = []
    
    for i, item in enumerate(raw_predictions):
        risk_data = item.rx2(risk)
        
        # Check if there's an error
        if 'error' in risk_data.names:
            continue
        
        surv_mat = np.array(risk_data.rx2('surv_val_matrix'))
        y_val = risk_data.rx2('y_val')
        
        pred_risk = 1 - surv_mat[:, h_idx]
        times = np.array(y_val.rx2('time'))
        events = np.array(y_val.rx2('event'))
        
        df = pd.DataFrame({
            'fold_id': i,
            'patient_idx': range(len(pred_risk)),
            'pred_cox': pred_risk,
            'time': times,
            'event': events
        })
        all_preds.append(df)
    
    return pd.concat(all_preds, ignore_index=True)


def main():
    parser = argparse.ArgumentParser(description='Extract predictions for NRI/IDI')
    parser.add_argument('--xgb-pickle', required=True, help='Path to XGBoost pickle file')
    parser.add_argument('--cox-rds', required=True, help='Path to Cox results_boot RDS file')
    parser.add_argument('--risk', choices=['death', 'readmission'], default='death',
                       help='Risk type to analyze')
    parser.add_argument('--horizon', type=int, default=12, help='Time horizon in months')
    parser.add_argument('--output', default='predictions_for_nri_idi.csv',
                       help='Output CSV file path')
    
    args = parser.parse_args()
    
    print(f"Loading XGBoost predictions from: {args.xgb_pickle}")
    xgb_data = load_xgb_predictions(args.xgb_pickle)
    xgb_preds = extract_xgb_at_horizon(xgb_data, args.risk, args.horizon)
    print(f"  Loaded {len(xgb_preds)} XGBoost predictions")
    
    print(f"\nLoading Cox predictions from: {args.cox_rds}")
    cox_preds = load_cox_predictions(args.cox_rds, args.risk, args.horizon)
    print(f"  Loaded {len(cox_preds)} Cox predictions")
    
    # Merge predictions
    # Assumes same fold structure and patient ordering
    print(f"\nMerging predictions...")
    
    # Create unique identifier for each patient within each fold
    xgb_preds['uid'] = xgb_preds['fold_id'].astype(str) + '_' + xgb_preds['patient_idx'].astype(str)
    cox_preds['uid'] = cox_preds['fold_id'].astype(str) + '_' + cox_preds['patient_idx'].astype(str)
    
    merged = pd.merge(
        xgb_preds[['uid', 'fold_id', 'time', 'event', 'pred_xgb']],
        cox_preds[['uid', 'pred_cox']],
        on='uid',
        how='inner'
    )
    
    print(f"  Merged dataset: {len(merged)} patients")
    print(f"  Events: {merged['event'].sum()}")
    print(f"  Censored: {len(merged) - merged['event'].sum()}")
    
    # Summary statistics
    print(f"\nPrediction summaries:")
    print(f"  Cox - mean: {merged['pred_cox'].mean():.4f}, sd: {merged['pred_cox'].std():.4f}")
    print(f"  XGB - mean: {merged['pred_xgb'].mean():.4f}, sd: {merged['pred_xgb'].std():.4f}")
    
    # Save to CSV
    output_cols = ['fold_id', 'time', 'event', 'pred_cox', 'pred_xgb']
    merged[output_cols].to_csv(args.output, index=False)
    print(f"\nSaved to: {args.output}")
    
    # Also save a version with all folds combined (for overall NRI/IDI)
    output_all = args.output.replace('.csv', '_all_folds.csv')
    merged[output_cols].to_csv(output_all, index=False)
    print(f"Also saved: {output_all}")


if __name__ == '__main__':
    main()
