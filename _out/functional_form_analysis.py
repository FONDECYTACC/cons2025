import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures, SplineTransformer
from sklearn.pipeline import make_pipeline
import warnings
import re

warnings.filterwarnings('ignore')

file_path = r"_out\XGB12_corr_Functional_Forms_Dual_Aggregated_20260306_1834.xlsx"
print(f"Reading sheets from {file_path} ...")
xl = pd.ExcelFile(file_path)
sheets = xl.sheet_names
print(f"Sheets found: {sheets}")

# Read raw data sheets (exclude summary/meta)
raw_sheets = [s for s in sheets if s not in ['Effects_Summary', 'Meta']]
print(f"Raw sheets ({len(raw_sheets)}): {raw_sheets}")

dfs = []
for sheet in raw_sheets:
    # Parse outcome and predictor from sheet name, e.g., D_adm_age_rec3 -> Death, adm_age_rec3
    m = re.match(r'^([DR])_(.+)$', sheet)
    if not m:
        print(f"  Skipping sheet {sheet} (does not match pattern)")
        continue
    outcome = 'Death' if m.group(1) == 'D' else 'Readmission'
    predictor = m.group(2)
    df_sheet = pd.read_excel(file_path, sheet_name=sheet)
    df_sheet['Predictor'] = predictor
    df_sheet['Outcome'] = outcome
    dfs.append(df_sheet)
    print(f"  {sheet}: {df_sheet.shape[0]} rows")

df = pd.concat(dfs, ignore_index=True)
print(f"\nCombined data shape: {df.shape}")
print(df.head())

# Basic info
predictors = df['Predictor'].unique()
print(f"Predictors ({len(predictors)}): {predictors}")

# Sample per predictor for speed (max 30k rows each)
df_sample = (
    df.groupby(['Predictor', 'Outcome'], group_keys=False)
    .apply(lambda g: g.sample(min(30000, len(g)), random_state=42))
    .reset_index(drop=True)
)

# Candidate transformations
def fit_and_predict(x, y):
    """Fit various models and return dict of predictions on sorted x."""
    mask = np.isfinite(x) & np.isfinite(y)
    x = x[mask]
    y = y[mask]
    if len(x) < 10:
        return None, None
    
    order = np.argsort(x)
    xo = x[order].reshape(-1, 1)
    yo = y[order]
    
    preds = {}
    
    # Linear
    m = LinearRegression().fit(xo, yo)
    preds['Linear'] = m.predict(xo)
    
    # Quadratic
    pipe = make_pipeline(PolynomialFeatures(2, include_bias=False), LinearRegression())
    pipe.fit(xo, yo)
    preds['Quadratic'] = pipe.predict(xo)
    
    # Cubic
    pipe = make_pipeline(PolynomialFeatures(3, include_bias=False), LinearRegression())
    pipe.fit(xo, yo)
    preds['Cubic'] = pipe.predict(xo)
    
    # Log (if all > 0)
    if np.all(xo > 0):
        xl = np.log(xo)
        m = LinearRegression().fit(xl, yo)
        preds['Log'] = m.predict(xl)
    
    # Sqrt (if all >= 0)
    if np.all(xo >= 0):
        xs = np.sqrt(xo)
        m = LinearRegression().fit(xs, yo)
        preds['Sqrt'] = m.predict(xs)
    
    # Spline (3 knots, cubic)
    try:
        spline = SplineTransformer(n_knots=3, degree=3)
        xspl = spline.fit_transform(xo)
        m = LinearRegression().fit(xspl, yo)
        preds['Spline'] = m.predict(xspl)
    except Exception:
        pass
    
    return xo.ravel(), preds

# Compute fits per predictor (grouped by Predictor only, collapsing Death/Readmission if same predictor appears in both)
# Actually we should probably group by Predictor only since user wants functional form per variable.
print("\nFitting transformations per predictor...")
fit_results = []
for pred in predictors:
    sub = df_sample[df_sample['Predictor'] == pred]
    xo, preds = fit_and_predict(sub['Feature_Value'].values, sub['SHAP_Impact'].values)
    if xo is None:
        continue
    row = {'Predictor': pred, 'x': xo, 'y': sub['SHAP_Impact'].values[np.isfinite(sub['Feature_Value'].values) & np.isfinite(sub['SHAP_Impact'].values)][np.argsort(sub['Feature_Value'].values[np.isfinite(sub['Feature_Value'].values) & np.isfinite(sub['SHAP_Impact'].values)])]}
    row.update(preds)
    fit_results.append(row)
    print(f"  {pred}: done")

# R-squared summary
summary_rows = []
for row in fit_results:
    y = row['y']
    s = {'Predictor': row['Predictor']}
    for name in ['Linear', 'Quadratic', 'Cubic', 'Log', 'Sqrt', 'Spline']:
        if name in row:
            ss_res = np.sum((y - row[name])**2)
            ss_tot = np.sum((y - np.mean(y))**2)
            s[f'rsq_{name.lower()}'] = round(1 - ss_res/ss_tot, 3) if ss_tot > 0 else np.nan
        else:
            s[f'rsq_{name.lower()}'] = np.nan
    summary_rows.append(s)

summary_df = pd.DataFrame(summary_rows)
out_csv = r"_out\functional_forms_rsq_summary.csv"
summary_df.to_csv(out_csv, index=False)
print(f"\nSaved summary: {out_csv}")
print(summary_df.to_string(index=False))

# Plotting
print("\nGenerating plots...")
# Sample points for plotting (max 5000 per predictor)
plot_points = (
    df_sample.groupby('Predictor', group_keys=False)
    .apply(lambda g: g.sample(min(5000, len(g)), random_state=123))
    .reset_index(drop=True)
)

colors = {
    'Linear': '#E41A1C',
    'Quadratic': '#377EB8',
    'Cubic': '#4DAF4A',
    'Log': '#984EA3',
    'Sqrt': '#FF7F00',
    'Spline': '#000000'
}

pdf_path = r"_out\functional_forms_inference.pdf"
pdf = PdfPages(pdf_path)

panels_per_page = 12
n_pages = int(np.ceil(len(fit_results) / panels_per_page))

for page in range(n_pages):
    start = page * panels_per_page
    end = min((page + 1) * panels_per_page, len(fit_results))
    subset = fit_results[start:end]
    n_panels = len(subset)
    ncols = 3
    nrows = int(np.ceil(n_panels / ncols))
    
    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(14, 4 * nrows), squeeze=False)
    axes = axes.flatten()
    
    for idx, row in enumerate(subset):
        ax = axes[idx]
        pred = row['Predictor']
        pts = plot_points[plot_points['Predictor'] == pred]
        ax.scatter(pts['Feature_Value'], pts['SHAP_Impact'], alpha=0.2, s=5, color='grey', edgecolor='none')
        for name in ['Linear', 'Quadratic', 'Cubic', 'Log', 'Sqrt', 'Spline']:
            if name in row:
                ax.plot(row['x'], row[name], label=name, color=colors.get(name, None), linewidth=1.5)
        ax.set_title(pred, fontsize=9, fontweight='bold')
        ax.set_xlabel('Feature Value', fontsize=8)
        ax.set_ylabel('SHAP Impact', fontsize=8)
        ax.tick_params(labelsize=7)
    
    # Hide unused axes
    for idx in range(n_panels, len(axes)):
        axes[idx].set_visible(False)
    
    # Global legend from first axis
    handles, labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='lower center', ncol=6, title='Transformation', fontsize=9, title_fontsize=10)
    fig.suptitle('Functional Form Inference: Feature Value vs SHAP Impact\nBlack = Spline (flexible reference); Colored lines = parametric candidates', fontsize=11, y=1.02)
    plt.tight_layout(rect=[0, 0.03, 1, 0.98])
    pdf.savefig(fig, bbox_inches='tight')
    plt.close(fig)
    print(f"  Page {page+1}/{n_pages} done")

pdf.close()
print(f"Saved PDF: {pdf_path}")
