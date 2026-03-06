import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.cluster.hierarchy import linkage, leaves_list
from scipy.spatial.distance import squareform

import re
import pandas as pd
import ast

# At the top — add PROJECT_ROOT detection
from pathlib import Path

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
FIGS_DIR = PROJECT_ROOT / "_figs"
FIGS_DIR.mkdir(parents=True, exist_ok=True)

# --------------------------------------------------
# 1️⃣ Pega aquí EXACTAMENTE tu dput completo
# --------------------------------------------------

r_text = """
structure(list(var1 = c("evaluacindelprocesoteraputico_logro_minimo", 
"evaluacindelprocesoteraputico_logro_minimo", "prim_sub_freq_rec_2_2_6_days_wk", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_relinterp_logro_minimo", 
"eva_sm_logro_minimo", "eva_fam_logro_minimo", "eva_consumo_logro_minimo", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_fam_logro_minimo", 
"eva_consumo_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_sm_logro_minimo", "eva_consumo_logro_minimo", "eva_relinterp_logro_minimo", 
"eva_relinterp_logro_minimo", "eva_fisica_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"readmit_time_from_disch_m", "dg_psiq_cie_10_dg", "eva_consumo_logro_minimo", 
"cohabitation_family_of_origin", "eva_consumo_logro_minimo", 
"eva_fam_logro_minimo", "eva_ocupacion_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_fam_logro_minimo", "eva_ocupacion_logro_minimo", "eva_relinterp_logro_minimo", 
"ed_attainment_corr_2_completed_high_school_or_less", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_ocupacion_logro_minimo", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_consumo_logro_minimo", "eva_fam_logro_minimo", "eva_fam_logro_intermedio", 
"evaluacindelprocesoteraputico_logro_intermedio", "eva_sm_logro_intermedio", 
"eva_relinterp_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_relinterp_logro_intermedio", "primary_sub_mod_cocaine_paste", 
"eva_fam_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_consumo_logro_intermedio", "eva_consumo_logro_intermedio", 
"eva_fisica_logro_intermedio", "eva_sm_logro_intermedio", "eva_ocupacion_logro_intermedio", 
"eva_fam_logro_intermedio", "tr_outcome_dropout", "eva_transgnorma_logro_intermedio", 
"eva_consumo_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"tr_outcome_dropout", "eva_consumo_logro_intermedio", "tr_outcome_dropout", 
"evaluacindelprocesoteraputico_logro_intermedio", "tr_outcome_dropout", 
"eva_relinterp_logro_intermedio", "tr_outcome_dropout", "first_sub_used_alcohol", 
"eva_sm_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"dit_m", "eva_fisica_logro_intermedio", "eva_relinterp_logro_intermedio", 
"cohabitation_with_couple_children", "sex_rec_woman", "tr_outcome_dropout", 
"eva_consumo_logro_intermedio", "dg_psiq_cie_10_instudy", "eva_sm_logro_intermedio", 
"evaluacindelprocesoteraputico_logro_intermedio", "dit_m", "adm_age_rec3", 
"dit_m", "evaluacindelprocesoteraputico_logro_minimo", "dit_m", 
"marital_status_rec_single", "eva_consumo_logro_intermedio", 
"eva_fam_logro_intermedio", "dit_m", "tr_outcome_dropout", "tenure_status_household_renting", 
"dit_m", "eva_fam_logro_intermedio", "primary_sub_mod_cocaine_paste", 
"tr_outcome_referral", "death_time_from_disch_m", "eva_relinterp_logro_intermedio", 
"dit_m", "adm_age_rec3", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_ocupacion_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"evaluacindelprocesoteraputico_logro_minimo", "cohabitation_family_of_origin", 
"dg_psiq_cie_10_dg", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_consumo_logro_minimo", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_relinterp_logro_intermedio", "tr_outcome_dropout", "eva_fam_logro_intermedio", 
"readmit_event", "eva_ocupacion_logro_intermedio", "eva_relinterp_logro_minimo", 
"eva_fam_logro_minimo", "eva_ocupacion_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_relinterp_logro_intermedio", 
"dit_m", "dit_m", "eva_sm_logro_intermedio", "eva_fam_logro_intermedio", 
"eva_relinterp_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"primary_sub_mod_cocaine_powder", "polysubstance_strict", "sex_rec_woman", 
"dit_m", "eva_consumo_logro_minimo", "eva_fam_logro_intermedio", 
"eva_consumo_logro_minimo", "sex_rec_woman", "eva_fam_logro_intermedio", 
"eva_fam_logro_minimo", "eva_consumo_logro_intermedio", "eva_fam_logro_intermedio", 
"occupation_condition_corr24_unemployed", "eva_consumo_logro_intermedio", 
"dg_psiq_cie_10_instudy", "eva_relinterp_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"plan_type_corr_pg_pr", "tr_outcome_dropout", "eva_ocupacion_logro_minimo", 
"first_sub_used_other", "first_sub_used_alcohol", "eva_consumo_logro_intermedio", 
"adm_age_rec3", "eva_fisica_logro_intermedio", "dg_psiq_cie_10_instudy", 
"dg_psiq_cie_10_instudy", "eva_sm_logro_minimo", "dg_psiq_cie_10_instudy", 
"eva_fam_logro_intermedio", "adm_age_rec3", "dg_psiq_cie_10_instudy", 
"dg_psiq_cie_10_instudy", "cohabitation_with_couple_children", 
"dg_psiq_cie_10_instudy", "dg_psiq_cie_10_instudy", "eva_consumo_logro_intermedio", 
"dg_psiq_cie_10_instudy", "eva_consumo_logro_intermedio", "adm_age_rec3", 
"first_sub_used_alcohol", "eva_consumo_logro_intermedio", "sub_dep_icd10_status_drug_dependence", 
"cohabitation_family_of_origin", "adm_age_rec3", "readmit_event", 
"eva_fisica_logro_minimo", "death_time_from_disch_m", "dg_psiq_cie_10_dg", 
"eva_relinterp_logro_minimo", "readmit_time_from_disch_m", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_consumo_logro_intermedio", "eva_consumo_logro_minimo", "eva_ocupacion_logro_intermedio", 
"cohabitation_family_of_origin", "first_sub_used_cocaine_powder", 
"first_sub_used_alcohol", "cohabitation_family_of_origin", "eva_ocupacion_logro_intermedio", 
"first_sub_used_cocaine_paste", "eva_ocupacion_logro_minimo", 
"eva_sm_logro_minimo", "plan_type_corr_pg_pr", "eva_fam_logro_minimo", 
"cohabitation_with_couple_children", "adm_motive_sanitary_sector", 
"plan_type_corr_pg_pai", "adm_motive_sanitary_sector", "polysubstance_strict", 
"eva_relinterp_logro_minimo", "sex_rec_woman", "eva_ocupacion_logro_intermedio", 
"adm_age_rec3", "dg_psiq_cie_10_instudy", "evaluacindelprocesoteraputico_logro_minimo", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_ocupacion_logro_minimo"
), var2 = c("eva_consumo_logro_minimo", "eva_sm_logro_minimo", 
"prim_sub_freq_rec_3_daily", "eva_relinterp_logro_minimo", "eva_sm_logro_minimo", 
"eva_fisica_logro_minimo", "eva_relinterp_logro_minimo", "eva_sm_logro_minimo", 
"eva_fam_logro_minimo", "eva_sm_logro_minimo", "eva_relinterp_logro_minimo", 
"eva_fisica_logro_minimo", "eva_transgnorma_logro_minimo", "eva_fam_logro_minimo", 
"eva_transgnorma_logro_minimo", "eva_fisica_logro_minimo", "eva_transgnorma_logro_minimo", 
"eva_transgnorma_logro_minimo", "death_time_from_disch_m", "dx_f6_personality", 
"eva_fisica_logro_minimo", "cohabitation_with_couple_children", 
"eva_transgnorma_logro_minimo", "eva_fisica_logro_minimo", "eva_sm_logro_minimo", 
"eva_ocupacion_logro_minimo", "eva_transgnorma_logro_minimo", 
"eva_transgnorma_logro_minimo", "eva_ocupacion_logro_minimo", 
"ed_attainment_corr_3_completed_primary_school_or_less", "eva_consumo_logro_intermedio", 
"eva_fisica_logro_minimo", "eva_sm_logro_intermedio", "eva_ocupacion_logro_minimo", 
"eva_ocupacion_logro_minimo", "eva_fam_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_sm_logro_minimo", "eva_relinterp_logro_minimo", "eva_relinterp_logro_intermedio", 
"eva_sm_logro_intermedio", "primary_sub_mod_alcohol", "eva_relinterp_logro_intermedio", 
"eva_fam_logro_intermedio", "eva_consumo_logro_minimo", "eva_sm_logro_intermedio", 
"eva_fisica_logro_minimo", "eva_fisica_logro_intermedio", "eva_ocupacion_logro_minimo", 
"eva_sm_logro_intermedio", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_transgnorma_logro_minimo", "eva_relinterp_logro_intermedio", 
"eva_fisica_logro_intermedio", "eva_consumo_logro_minimo", "eva_fam_logro_intermedio", 
"eva_sm_logro_minimo", "eva_consumo_logro_minimo", "eva_fam_logro_minimo", 
"eva_fisica_logro_intermedio", "eva_relinterp_logro_minimo", 
"primary_sub_mod_alcohol", "eva_transgnorma_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_transgnorma_logro_intermedio", "eva_transgnorma_logro_intermedio", 
"marital_status_rec_single", "plan_type_corr_m_pai", "eva_fisica_logro_minimo", 
"eva_fisica_logro_intermedio", "dg_psiq_cie_10_dg", "eva_fisica_logro_minimo", 
"eva_sm_logro_minimo", "eva_sm_logro_minimo", "marital_status_rec_single", 
"eva_relinterp_logro_minimo", "eva_sm_logro_intermedio", "eva_consumo_logro_minimo", 
"marital_status_rec_separated_divorced_annulled_widowed", "eva_transgnorma_logro_intermedio", 
"eva_fisica_logro_intermedio", "eva_fam_logro_minimo", "eva_transgnorma_logro_minimo", 
"tenure_status_household_stays_temporarily_with_a_relative", 
"eva_ocupacion_logro_minimo", "eva_relinterp_logro_minimo", "primary_sub_mod_cocaine_powder", 
"tr_outcome_dropout", "porc_pobr", "eva_sm_logro_minimo", "eva_transgnorma_logro_minimo", 
"primary_sub_mod_alcohol", "eva_relinterp_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "eva_relinterp_logro_minimo", 
"eva_fam_logro_intermedio", "marital_status_rec_single", "dx_f3_mood", 
"eva_fisica_logro_minimo", "eva_sm_logro_intermedio", "eva_ocupacion_logro_intermedio", 
"eva_fisica_logro_minimo", "eva_ocupacion_logro_minimo", "eva_transgnorma_logro_intermedio", 
"readmit_time_from_disch_m", "eva_sm_logro_intermedio", "eva_sm_logro_intermedio", 
"eva_relinterp_logro_intermedio", "eva_fisica_logro_intermedio", 
"eva_fam_logro_minimo", "eva_consumo_logro_intermedio", "eva_transgnorma_logro_minimo", 
"eva_fisica_logro_minimo", "dg_psiq_cie_10_instudy", "eva_transgnorma_logro_minimo", 
"eva_sm_logro_minimo", "eva_ocupacion_logro_intermedio", "eva_transgnorma_logro_minimo", 
"primary_sub_mod_alcohol", "primary_sub_mod_alcohol", "plan_type_corr_m_pr", 
"tr_outcome_dropout", "eva_fam_logro_intermedio", "eva_fisica_logro_minimo", 
"eva_relinterp_logro_intermedio", "occupation_condition_corr24_inactive", 
"eva_transgnorma_logro_minimo", "eva_sm_logro_intermedio", "eva_ocupacion_logro_intermedio", 
"eva_ocupacion_logro_intermedio", "occupation_condition_corr24_inactive", 
"eva_sm_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_ocupacion_logro_minimo", "eva_ocupacion_logro_minimo", "plan_type_corr_pg_pai", 
"tr_outcome_adm_discharge_rule_violation_undet", "eva_sm_logro_intermedio", 
"primary_sub_mod_others", "primary_sub_mod_cocaine_paste", "eva_fisica_logro_minimo", 
"marital_status_rec_separated_divorced_annulled_widowed", "eva_transgnorma_logro_minimo", 
"dx_f6_personality", "eva_sm_logro_minimo", "eva_fisica_logro_intermedio", 
"eva_consumo_logro_minimo", "eva_ocupacion_logro_minimo", "polysubstance_strict", 
"eva_relinterp_logro_minimo", "eva_transgnorma_logro_minimo", 
"cohabitation_others", "eva_ocupacion_logro_minimo", "eva_fam_logro_minimo", 
"eva_transgnorma_logro_minimo", "eva_fisica_logro_minimo", "eva_relinterp_logro_minimo", 
"cohabitation_family_of_origin", "first_sub_used_cocaine_paste", 
"eva_fam_logro_minimo", "prim_sub_freq_rec_3_daily", "tenure_status_household_stays_temporarily_with_a_relative", 
"first_sub_used_alcohol", "death_time_from_disch_m", "eva_transgnorma_logro_intermedio", 
"center_id", "dx_f_any_severe_mental", "eva_fisica_logro_intermedio", 
"porc_pobr", "eva_fisica_logro_intermedio", "eva_ocupacion_logro_minimo", 
"eva_fisica_logro_intermedio", "eva_transgnorma_logro_minimo", 
"cohabitation_others", "primary_sub_mod_cocaine_powder", "first_sub_used_cocaine_powder", 
"tenure_status_household_renting", "eva_fisica_logro_minimo", 
"primary_sub_mod_cocaine_paste", "eva_fisica_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "occupation_condition_corr24_unemployed", 
"eva_fisica_logro_intermedio", "tenure_status_household_stays_temporarily_with_a_relative", 
"adm_motive_justice_sector", "plan_type_corr_m_pai", "adm_motive_another_sud_facility_fonodrogas_senda_previene", 
"primary_sub_mod_cocaine_paste", "eva_transgnorma_logro_intermedio", 
"cohabitation_with_couple_children", "eva_sm_logro_minimo", "primary_sub_mod_cocaine_paste", 
"tr_outcome_dropout", "eva_ocupacion_logro_intermedio", "eva_transgnorma_logro_intermedio", 
"eva_transgnorma_logro_intermedio"), assoc = c(0.83711780923882, 
0.802467426702571, 0.793103584137556, 0.77520306131881, 0.768178520175855, 
0.76719250067355, 0.766362240951157, 0.762346456950798, 0.76155691196306, 
0.73354304498773, 0.727379175488808, 0.722833998016515, 0.722181083770933, 
0.720792947329792, 0.720035870766128, 0.716838733864828, 0.714728557257476, 
0.712978822153262, 0.707006746274748, 0.702014779340416, 0.696715822959763, 
0.69129840726262, 0.682953183889042, 0.678028590006987, 0.672494678072799, 
0.671843325957547, 0.6704681964172, 0.669748977778129, 0.666357244708136, 
0.664686570021668, 0.661428421015166, 0.650744535823471, 0.643974730795412, 
0.64282166296846, 0.631813958238911, 0.611658346052745, 0.60892561908453, 
0.605205998206887, 0.602526023845648, 0.5828051674047, 0.563146494122444, 
0.559621524510426, 0.558871162996951, 0.553289256960816, 0.539344892770539, 
0.534331428092229, 0.53152719016331, 0.518624610521569, 0.510149068697816, 
0.503581431447093, 0.486344152036879, 0.462675914718396, 0.461642357720592, 
0.46045131900462, 0.459557386092814, 0.455939574506707, 0.453704379295099, 
0.451631060306019, 0.450969526959295, 0.447087676930584, 0.444638330716404, 
0.43990003053132, 0.438066706885394, 0.435107359004121, 0.434928072408616, 
0.434006360736843, 0.430669461626251, 0.430667435932057, 0.428660047420367, 
0.420771350232461, 0.416104842266024, 0.41225824649501, 0.412197243945799, 
0.411866413977491, 0.409088119058529, 0.405475382225122, 0.400788986587315, 
0.40050867422744, 0.398958869798775, 0.398528639562554, 0.396745811624769, 
0.393794008803624, 0.3937089605225, 0.390717261537593, 0.390422236038805, 
0.389904127406773, 0.385143203678887, 0.384508980430528, 0.384361974385906, 
0.383990780298321, 0.378531898034635, 0.37837526148715, 0.377630061886149, 
0.37682263673882, 0.376303753364269, 0.376282051863633, 0.376281524448277, 
0.375684258288421, 0.374784740447005, 0.373008297865848, 0.371316180790093, 
0.371099354154996, 0.370435783893856, 0.370210376085268, 0.36984889966301, 
0.368688320795364, 0.367696371727569, 0.367582939780527, 0.367454376543493, 
0.365202723736028, 0.364285429083288, 0.364180880279019, 0.363294609866006, 
0.363284245577706, 0.362675545899315, 0.362110740640351, 0.358655451625357, 
0.358133353792139, 0.357193937048775, 0.353906327832875, 0.352626298074351, 
0.348138271711056, 0.34642308838635, 0.34603927199774, 0.34590548878549, 
0.342194391732293, 0.33432138940454, 0.332107475086569, 0.331957611941704, 
0.328067851174434, 0.324957615172338, 0.321497869450126, 0.3095344176146, 
0.307415221005581, 0.304379108130098, 0.303997909493549, 0.30154460585833, 
0.301459726550581, 0.300610446005161, 0.294375840813771, 0.294318523430571, 
0.292143150675537, 0.290969794256577, 0.28974424974556, 0.289411381944461, 
0.289090778513154, 0.288938039332883, 0.28846675069159, 0.288108050112873, 
0.282415450269157, 0.278465456906241, 0.278398295881015, 0.278045135138623, 
0.27769098560223, 0.276986352468518, 0.275388956175151, 0.271973616245833, 
0.271745616737019, 0.269387079292103, 0.268530855638539, 0.268190510657962, 
0.266217937307677, 0.264104795770095, 0.260857869029608, 0.257130588473407, 
0.254785793739597, 0.252430588728275, 0.251901878087493, 0.248356729029594, 
0.248159457502041, 0.245918539773196, 0.241268033506961, 0.236014641846315, 
0.236013005268081, 0.235889842310641, 0.234805764946732, 0.234202608751926, 
0.230039814226642, 0.229648800680575, 0.225876514560915, 0.225522962832459, 
0.221822146886374, 0.221583332918691, 0.221539704994553, 0.219102942383595, 
0.218067586132561, 0.216190075672415, 0.215258954297962, 0.215053893832581, 
0.21414268174219, 0.212940629412792, 0.21089596019546, 0.208777507544135, 
0.207415788862218, 0.205448646909787, 0.204787053715661, 0.200223985556212
)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
-197L))
"""

# --------------------------------------------------
# 2️⃣ Extraer listas var1, var2, assoc
# --------------------------------------------------

def extract_vector(name):
    pattern = rf'{name} = c\((.*?)\)'
    match = re.search(pattern, r_text, re.S)
    content = match.group(1)

    # convertir a lista Python
    content = content.replace("\n", "")
    content = "[" + content + "]"
    content = content.replace("NA", "None")

    return ast.literal_eval(content)

var1 = extract_vector("var1")
var2 = extract_vector("var2")
assoc = extract_vector("assoc")

pairs_gt_02 = pd.DataFrame({
    "var1": var1,
    "var2": var2,
    "assoc": assoc
})

print(pairs_gt_02.shape)
pairs_gt_02.head()

# ------------------------------------------------------------
# 1️⃣ Reconstruir base desde tu dput (YA PEGADA)
# ------------------------------------------------------------

# ------------------------------------------------------------
# 2️⃣ Filtrar plan_type_strata
# ------------------------------------------------------------

df = pairs_gt_02[
    (pairs_gt_02["var1"] != "plan_type_strata") &
    (pairs_gt_02["var2"] != "plan_type_strata")
].copy()

# ------------------------------------------------------------
# 3️⃣ Construir matriz simétrica
# ------------------------------------------------------------

variables = sorted(set(df["var1"]).union(set(df["var2"])))
corr_matrix = pd.DataFrame(
    np.nan,
    index=variables,
    columns=variables
)

for _, row in df.iterrows():
    corr_matrix.loc[row["var1"], row["var2"]] = row["assoc"]
    corr_matrix.loc[row["var2"], row["var1"]] = row["assoc"]

#np.fill_diagonal(corr_matrix.values, 1)
corr_matrix = corr_matrix.copy()
for v in corr_matrix.index:
    corr_matrix.loc[v, v] = 1.0
# ------------------------------------------------------------
# 4️⃣ Clustering jerárquico rápido
# ------------------------------------------------------------

corr_for_cluster = corr_matrix.fillna(0)
distance_matrix = 1 - corr_for_cluster
linkage_matrix = linkage(squareform(distance_matrix), method="average")
order = leaves_list(linkage_matrix)

corr_matrix = corr_matrix.iloc[order, order]

# ------------------------------------------------------------
# 5️⃣ Filtrar solo > 0.6 (visual limpio)
# ------------------------------------------------------------

threshold = 0.6
mask_keep = corr_matrix >= threshold
corr_filtered = corr_matrix.where(mask_keep)

top_n = 40

top_pairs = df.sort_values("assoc", ascending=False).head(top_n)

vars_keep = set(top_pairs["var1"]).union(set(top_pairs["var2"]))

vars_keep = sorted(list(vars_keep))
vars_keep = [v for v in vars_keep if v in corr_matrix.index]

corr_reduced = corr_matrix.loc[vars_keep, vars_keep]
# ------------------------------------------------------------
# 6️⃣ Crear anotaciones con negrita > 0.9
# ------------------------------------------------------------



annot = corr_reduced.copy().astype(object)

for i in range(corr_reduced.shape[0]):
    for j in range(corr_reduced.shape[1]):
        val = corr_reduced.iloc[i, j]
        if pd.notna(val):
            formatted = f"{val:.2f}".replace(".", ",")
            if val > 0.9 and i != j:
                annot.iloc[i, j] = r"$\bf{" + formatted + "}$"
            else:
                annot.iloc[i, j] = formatted
        else:
            annot.iloc[i, j] = ""
# ------------------------------------------------------------
# 7️⃣ Heatmap publication-level
# ------------------------------------------------------------

plt.figure(figsize=(18, 16))

mask_upper = np.triu(np.ones_like(corr_reduced, dtype=bool))

# Fill NaN with a sentinel for gray background
ax = sns.heatmap(
    corr_reduced,
    mask=mask_upper,
    cmap="YlOrRd",
    vmin=0.2,
    vmax=0.85,          # tighter range for better contrast
    annot=annot,
    fmt="",
    annot_kws={"fontsize": 15.5},  # smaller annotation
    linewidths=0.5,
    linecolor="#eeeeee",
    square=True,
    cbar_kws={"shrink": 0.5, "label": "Asociación (Cramér's V/ Pearson /Spearman)"}
)

# Gray background for NaN cells
ax.set_facecolor("#f0f0f0")

cbar = ax.collections[0].colorbar
cbar.ax.tick_params(labelsize=11)

plt.xticks(rotation=45, fontsize=14, ha="right", rotation_mode="anchor")
plt.yticks(rotation=0, fontsize=14)
plt.title(None)#"Correlation Heatmap (Associations > 0.6)", fontsize=18, weight="bold")
# Replace the export block at the bottom
plt.tight_layout()
plt.savefig(FIGS_DIR / "correlation_heatmap_publication.svg", format="svg", bbox_inches="tight")
plt.savefig(FIGS_DIR / "correlation_heatmap_publication.png", format="png", dpi=300, bbox_inches="tight")
plt.show()