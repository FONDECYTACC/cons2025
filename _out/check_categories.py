import pandas as pd
file = r'_out\XGB12_corr_Functional_Forms_Dual_Aggregated_20260306_1834.xlsx'
for pred in ['dit_m', 'prim_sub_freq_rec', 'tenure_status_hous', 'ed_attainment_corr', 'eva_consumo']:
    d = pd.read_excel(file, sheet_name=f'D_{pred}')
    uniques = sorted(d['Feature_Value'].unique())
    print(f"{pred}: n_unique={len(uniques)}, min={min(uniques):.4f}, max={max(uniques):.4f}")
    if len(uniques) <= 10:
        print(f"  values: {uniques}")
        vc = d['Feature_Value'].value_counts().sort_index()
        print(f"  counts: {vc.to_dict()}")
    else:
        print(f"  first 15 unique: {uniques[:15]}")
    print()
