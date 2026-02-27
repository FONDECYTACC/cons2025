import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os

# --- 1. DATOS ---
data_readm = {
    'trial_id': [0, 11, 31, 41, 6, 23, 29],
    'C_Index': [0.647162, 0.646796, 0.646038, 0.645156, 0.643385, 0.641606, 0.640602],
    'IBS': [0.110314, 0.109979, 0.109782, 0.109776, 0.109626, 0.109616, 0.109525]
}
df_readm = pd.DataFrame(data_readm)

data_mort = {
    'trial_id': [41, 12, 15, 8, 22, 5, 19],
    'C_Index': [0.771700, 0.770500, 0.768000, 0.765000, 0.762000, 0.759000, 0.755000],
    'IBS': [0.019000, 0.018800, 0.018500, 0.018400, 0.018200, 0.018000, 0.017800]
}
df_mort = pd.DataFrame(data_mort)

# --- 2. CONFIGURACIÓN COMPACTA (1/3 de página) ---
sns.set_theme(style="whitegrid")

# Proporción horizontal y baja (12x5)
fig, axes = plt.subplots(1, 2, figsize=(12, 5))

# Fuentes maximizadas para tamaño reducido
L_SIZE = 14 # Ejes
T_SIZE = 10 # Labels de puntos

# PANEL 1: READMISIÓN
ax1 = axes[0]
sns.scatterplot(data=df_readm, x='IBS', y='C_Index', s=150, color='#2c7bb6', alpha=0.7, ax=ax1)
winner_r = df_readm.loc[df_readm['trial_id'] == 0]
ax1.scatter(winner_r['IBS'], winner_r['C_Index'], color='red', s=350, marker='*', edgecolors='black', label='T-0 (Winner)')

# Línea de Pareto
df_r_s = df_readm.sort_values('IBS')
ax1.plot(df_r_s['IBS'], df_r_s['C_Index'], '--', color='gray', alpha=0.4)

for i in range(df_readm.shape[0]):
    ax1.text(df_readm['IBS'][i], df_readm['C_Index'][i] + 0.0001, f"T{df_readm['trial_id'][i]}", fontsize=T_SIZE)

ax1.set_xlabel('IBS (Brier Score)', fontsize=L_SIZE)
ax1.set_ylabel('C-Index', fontsize=L_SIZE)
ax1.legend(fontsize=10, loc='lower right')

# PANEL 2: MORTALIDAD
ax2 = axes[1]
sns.scatterplot(data=df_mort, x='IBS', y='C_Index', s=150, color='#1a9641', alpha=0.7, ax=ax2)
winner_m = df_mort.loc[df_mort['trial_id'] == 41]
ax2.scatter(winner_m['IBS'], winner_m['C_Index'], color='red', s=350, marker='*', edgecolors='black', label='T-41 (Winner)')

# Línea de Pareto
df_m_s = df_mort.sort_values('IBS')
ax2.plot(df_m_s['IBS'], df_m_s['C_Index'], '--', color='gray', alpha=0.4)

for i in range(df_mort.shape[0]):
    ax2.text(df_mort['IBS'][i], df_mort['C_Index'][i] + 0.0002, f"T{df_mort['trial_id'][i]}", fontsize=T_SIZE)

ax2.set_xlabel('IBS (Brier Score)', fontsize=L_SIZE)
ax2.set_ylabel('C-Index', fontsize=L_SIZE)
ax2.legend(fontsize=10, loc='lower right')

# --- 3. EXPORTACIÓN ---
plt.tight_layout()

folder = "_figs"
if not os.path.exists(folder): os.makedirs(folder)

plt.savefig(f"{folder}/pareto_compacto_tesis.png", dpi=300, bbox_inches='tight')
plt.savefig(f"{folder}/pareto_compacto_tesis.pdf", bbox_inches='tight')

plt.show()