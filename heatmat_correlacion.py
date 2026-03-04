import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.cluster.hierarchy import linkage, leaves_list
from scipy.spatial.distance import squareform

import re
import pandas as pd
import ast

# --------------------------------------------------
# 1️⃣ Pega aquí EXACTAMENTE tu dput completo
# --------------------------------------------------

r_text = """
structure(list(var1 = c("plan_type_corr_pg_pr", "plan_type_corr_m_pr", 
"plan_type_corr_pg_pai", "plan_type_corr_m_pai", "evaluacindelprocesoteraputico_logro_minimo", 
"evaluacindelprocesoteraputico_logro_minimo", "prim_sub_freq_rec_2_2_6_days_wk", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_sm_logro_minimo", 
"eva_relinterp_logro_minimo", "eva_fam_logro_minimo", "eva_consumo_logro_minimo", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_fam_logro_minimo", 
"eva_consumo_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_consumo_logro_minimo", "eva_relinterp_logro_minimo", "eva_sm_logro_minimo", 
"eva_relinterp_logro_minimo", "eva_fisica_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"readmit_time_from_disch_m", "dg_psiq_cie_10_dg", "eva_consumo_logro_minimo", 
"cohabitation_family_of_origin", "eva_consumo_logro_minimo", 
"eva_fam_logro_minimo", "eva_fam_logro_minimo", "eva_ocupacion_logro_minimo", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_ocupacion_logro_minimo", 
"eva_relinterp_logro_minimo", "ed_attainment_corr_2_completed_high_school_or_less", 
"evaluacindelprocesoteraputico_logro_intermedio", "eva_ocupacion_logro_minimo", 
"eva_consumo_logro_minimo", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_fam_logro_minimo", "eva_fam_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_sm_logro_intermedio", "eva_relinterp_logro_intermedio", 
"evaluacindelprocesoteraputico_logro_intermedio", "sex_rec_woman", 
"eva_relinterp_logro_intermedio", "primary_sub_mod_cocaine_paste", 
"eva_fam_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_consumo_logro_intermedio", "eva_consumo_logro_intermedio", 
"eva_fisica_logro_intermedio", "eva_sm_logro_intermedio", "eva_ocupacion_logro_intermedio", 
"eva_fam_logro_intermedio", "tr_outcome_dropout", "eva_consumo_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"eva_consumo_logro_intermedio", "tr_outcome_dropout", "evaluacindelprocesoteraputico_logro_intermedio", 
"tr_outcome_dropout", "tr_outcome_dropout", "eva_relinterp_logro_intermedio", 
"tr_outcome_dropout", "first_sub_used_alcohol", "eva_sm_logro_intermedio", 
"eva_fisica_logro_intermedio", "eva_relinterp_logro_intermedio", 
"evaluacindelprocesoteraputico_logro_intermedio", "dit_m", "cohabitation_with_couple_children", 
"sex_rec_woman", "tr_outcome_dropout", "eva_consumo_logro_intermedio", 
"eva_sm_logro_intermedio", "dg_psiq_cie_10_instudy", "evaluacindelprocesoteraputico_logro_intermedio", 
"dit_m", "adm_age_rec3", "dit_m", "dit_m", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_consumo_logro_intermedio", "marital_status_rec_single", 
"eva_fam_logro_intermedio", "dit_m", "tenure_status_household_renting", 
"tr_outcome_dropout", "dit_m", "eva_fam_logro_intermedio", "primary_sub_mod_cocaine_paste", 
"death_time_from_disch_m", "tr_outcome_referral", "eva_relinterp_logro_intermedio", 
"dit_m", "dg_psiq_cie_10_dg", "adm_age_rec3", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_ocupacion_logro_intermedio", "cohabitation_family_of_origin", 
"evaluacindelprocesoteraputico_logro_intermedio", "evaluacindelprocesoteraputico_logro_minimo", 
"evaluacindelprocesoteraputico_logro_intermedio", "eva_consumo_logro_minimo", 
"tr_outcome_dropout", "eva_fam_logro_intermedio", "eva_relinterp_logro_intermedio", 
"evaluacindelprocesoteraputico_logro_intermedio", "eva_relinterp_logro_minimo", 
"eva_fam_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"readmit_event", "eva_ocupacion_logro_intermedio", "eva_relinterp_logro_intermedio", 
"eva_ocupacion_logro_intermedio", "evaluacindelprocesoteraputico_logro_intermedio", 
"dit_m", "eva_sm_logro_intermedio", "eva_fam_logro_intermedio", 
"eva_relinterp_logro_intermedio", "dit_m", "evaluacindelprocesoteraputico_logro_intermedio", 
"primary_sub_mod_cocaine_powder", "polysubstance_strict", "sex_rec_woman", 
"dit_m", "eva_consumo_logro_minimo", "eva_fam_logro_intermedio", 
"eva_consumo_logro_minimo", "sex_rec_woman", "eva_fam_logro_intermedio", 
"eva_fam_logro_minimo", "eva_consumo_logro_intermedio", "eva_fam_logro_intermedio", 
"occupation_condition_corr24_unemployed", "sub_dep_icd10_status_drug_dependence", 
"eva_consumo_logro_intermedio", "dg_psiq_cie_10_instudy", "eva_relinterp_logro_intermedio", 
"plan_type_corr_pg_pr", "evaluacindelprocesoteraputico_logro_intermedio", 
"tr_outcome_dropout", "eva_ocupacion_logro_minimo", "prim_sub_freq_rec_3_daily", 
"first_sub_used_alcohol", "dg_psiq_cie_10_instudy", "eva_consumo_logro_intermedio", 
"dg_psiq_cie_10_instudy", "adm_age_rec3", "first_sub_used_other", 
"dg_psiq_cie_10_instudy", "eva_sm_logro_minimo", "eva_fisica_logro_intermedio", 
"eva_fam_logro_intermedio", "adm_age_rec3", "dg_psiq_cie_10_instudy", 
"dg_psiq_cie_10_instudy", "dg_psiq_cie_10_instudy", "dg_psiq_cie_10_instudy", 
"cohabitation_with_couple_children", "eva_consumo_logro_intermedio", 
"dg_psiq_cie_10_instudy", "eva_consumo_logro_intermedio", "first_sub_used_alcohol", 
"eva_consumo_logro_intermedio", "cohabitation_family_of_origin", 
"adm_age_rec3", "sub_dep_icd10_status_drug_dependence", "adm_age_rec3", 
"readmit_event", "dg_psiq_cie_10_dg", "occupation_condition_corr24_unemployed", 
"eva_fisica_logro_minimo", "death_time_from_disch_m", "readmit_time_from_disch_m", 
"eva_relinterp_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_consumo_logro_intermedio", "primary_sub_mod_cocaine_paste", 
"first_sub_used_cocaine_powder", "first_sub_used_alcohol", "occupation_condition_corr24_inactive", 
"cohabitation_family_of_origin", "eva_consumo_logro_minimo", 
"eva_ocupacion_logro_intermedio", "first_sub_used_cocaine_paste", 
"cohabitation_family_of_origin", "eva_ocupacion_logro_intermedio", 
"eva_ocupacion_logro_minimo", "cohabitation_with_couple_children", 
"eva_fam_logro_minimo", "plan_type_corr_pg_pr", "adm_motive_sanitary_sector", 
"eva_sm_logro_minimo", "adm_motive_another_sud_facility_fonodrogas_senda_previene", 
"adm_motive_sanitary_sector", "plan_type_corr_pg_pai", "polysubstance_strict", 
"eva_relinterp_logro_minimo", "sex_rec_woman", "dg_psiq_cie_10_instudy", 
"eva_ocupacion_logro_intermedio", "adm_age_rec3", "cohabitation_with_couple_children", 
"evaluacindelprocesoteraputico_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"plan_type_corr_pg_pr"), var2 = c("plan_type_strata", "plan_type_strata", 
"plan_type_strata", "plan_type_strata", "eva_consumo_logro_minimo", 
"eva_sm_logro_minimo", "prim_sub_freq_rec_3_daily", "eva_relinterp_logro_minimo", 
"eva_fisica_logro_minimo", "eva_sm_logro_minimo", "eva_relinterp_logro_minimo", 
"eva_sm_logro_minimo", "eva_fam_logro_minimo", "eva_sm_logro_minimo", 
"eva_relinterp_logro_minimo", "eva_fisica_logro_minimo", "eva_fam_logro_minimo", 
"eva_transgnorma_logro_minimo", "eva_transgnorma_logro_minimo", 
"eva_fisica_logro_minimo", "eva_transgnorma_logro_minimo", "eva_transgnorma_logro_minimo", 
"death_time_from_disch_m", "dx_f6_personality", "eva_fisica_logro_minimo", 
"cohabitation_with_couple_children", "eva_transgnorma_logro_minimo", 
"eva_fisica_logro_minimo", "eva_transgnorma_logro_minimo", "eva_sm_logro_minimo", 
"eva_ocupacion_logro_minimo", "eva_transgnorma_logro_minimo", 
"eva_ocupacion_logro_minimo", "ed_attainment_corr_3_completed_primary_school_or_less", 
"eva_consumo_logro_intermedio", "eva_fisica_logro_minimo", "eva_ocupacion_logro_minimo", 
"eva_sm_logro_intermedio", "eva_ocupacion_logro_minimo", "eva_fam_logro_minimo", 
"evaluacindelprocesoteraputico_logro_minimo", "eva_sm_logro_minimo", 
"eva_relinterp_logro_minimo", "eva_relinterp_logro_intermedio", 
"plan_type_strata", "eva_sm_logro_intermedio", "primary_sub_mod_alcohol", 
"eva_relinterp_logro_intermedio", "eva_fam_logro_intermedio", 
"eva_consumo_logro_minimo", "eva_sm_logro_intermedio", "eva_fisica_logro_minimo", 
"eva_fisica_logro_intermedio", "eva_ocupacion_logro_minimo", 
"eva_sm_logro_intermedio", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_relinterp_logro_intermedio", "eva_transgnorma_logro_minimo", 
"eva_fisica_logro_intermedio", "eva_fam_logro_intermedio", "eva_consumo_logro_minimo", 
"eva_consumo_logro_minimo", "eva_sm_logro_minimo", "eva_fam_logro_minimo", 
"eva_fisica_logro_intermedio", "eva_relinterp_logro_minimo", 
"primary_sub_mod_alcohol", "eva_transgnorma_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "eva_transgnorma_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "evaluacindelprocesoteraputico_logro_minimo", 
"marital_status_rec_single", "plan_type_corr_m_pai", "eva_fisica_logro_minimo", 
"eva_fisica_logro_intermedio", "eva_fisica_logro_minimo", "dg_psiq_cie_10_dg", 
"eva_sm_logro_minimo", "eva_sm_logro_minimo", "marital_status_rec_single", 
"eva_relinterp_logro_minimo", "eva_consumo_logro_minimo", "eva_sm_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "marital_status_rec_separated_divorced_annulled_widowed", 
"eva_fisica_logro_intermedio", "eva_fam_logro_minimo", "tenure_status_household_stays_temporarily_with_a_relative", 
"eva_transgnorma_logro_minimo", "eva_ocupacion_logro_minimo", 
"eva_relinterp_logro_minimo", "primary_sub_mod_cocaine_powder", 
"porc_pobr", "tr_outcome_dropout", "eva_sm_logro_minimo", "eva_transgnorma_logro_minimo", 
"dx_f3_mood", "primary_sub_mod_alcohol", "eva_fam_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "marital_status_rec_single", 
"eva_relinterp_logro_minimo", "eva_relinterp_logro_intermedio", 
"eva_fisica_logro_minimo", "eva_sm_logro_intermedio", "eva_ocupacion_logro_minimo", 
"eva_transgnorma_logro_intermedio", "eva_fisica_logro_minimo", 
"eva_ocupacion_logro_intermedio", "eva_sm_logro_intermedio", 
"eva_relinterp_logro_intermedio", "eva_consumo_logro_intermedio", 
"readmit_time_from_disch_m", "eva_sm_logro_intermedio", "eva_transgnorma_logro_minimo", 
"eva_fisica_logro_intermedio", "eva_fam_logro_minimo", "eva_fisica_logro_minimo", 
"eva_transgnorma_logro_minimo", "eva_sm_logro_minimo", "eva_ocupacion_logro_intermedio", 
"dg_psiq_cie_10_instudy", "eva_transgnorma_logro_minimo", "primary_sub_mod_alcohol", 
"primary_sub_mod_alcohol", "plan_type_corr_m_pr", "tr_outcome_dropout", 
"eva_fam_logro_intermedio", "eva_fisica_logro_minimo", "eva_relinterp_logro_intermedio", 
"occupation_condition_corr24_inactive", "eva_transgnorma_logro_minimo", 
"eva_sm_logro_intermedio", "eva_ocupacion_logro_intermedio", 
"eva_ocupacion_logro_intermedio", "occupation_condition_corr24_inactive", 
"plan_type_strata", "eva_sm_logro_minimo", "evaluacindelprocesoteraputico_logro_minimo", 
"eva_ocupacion_logro_minimo", "plan_type_corr_pg_pai", "eva_ocupacion_logro_minimo", 
"tr_outcome_adm_discharge_rule_violation_undet", "eva_sm_logro_intermedio", 
"plan_type_strata", "primary_sub_mod_cocaine_paste", "eva_sm_logro_minimo", 
"eva_fisica_logro_minimo", "eva_consumo_logro_minimo", "marital_status_rec_separated_divorced_annulled_widowed", 
"primary_sub_mod_others", "dx_f6_personality", "eva_fisica_logro_intermedio", 
"eva_transgnorma_logro_minimo", "eva_ocupacion_logro_minimo", 
"polysubstance_strict", "eva_relinterp_logro_minimo", "eva_transgnorma_logro_minimo", 
"eva_ocupacion_logro_minimo", "eva_fam_logro_minimo", "cohabitation_others", 
"eva_transgnorma_logro_minimo", "eva_fisica_logro_minimo", "eva_relinterp_logro_minimo", 
"first_sub_used_cocaine_paste", "eva_fam_logro_minimo", "tenure_status_household_stays_temporarily_with_a_relative", 
"cohabitation_family_of_origin", "prim_sub_freq_rec_3_daily", 
"first_sub_used_alcohol", "death_time_from_disch_m", "dx_f_any_severe_mental", 
"plan_type_strata", "eva_transgnorma_logro_intermedio", "center_id", 
"porc_pobr", "eva_fisica_logro_intermedio", "eva_fisica_logro_intermedio", 
"eva_ocupacion_logro_minimo", "plan_type_strata", "primary_sub_mod_cocaine_powder", 
"first_sub_used_cocaine_powder", "plan_type_strata", "cohabitation_others", 
"eva_fisica_logro_intermedio", "eva_transgnorma_logro_minimo", 
"primary_sub_mod_cocaine_paste", "tenure_status_household_renting", 
"eva_fisica_logro_minimo", "eva_fisica_logro_intermedio", "tenure_status_household_stays_temporarily_with_a_relative", 
"eva_fisica_logro_intermedio", "occupation_condition_corr24_unemployed", 
"adm_motive_justice_sector", "eva_transgnorma_logro_intermedio", 
"plan_type_strata", "adm_motive_another_sud_facility_fonodrogas_senda_previene", 
"plan_type_corr_m_pai", "primary_sub_mod_cocaine_paste", "eva_transgnorma_logro_intermedio", 
"cohabitation_with_couple_children", "tr_outcome_dropout", "eva_sm_logro_minimo", 
"primary_sub_mod_cocaine_paste", "plan_type_strata", "eva_ocupacion_logro_intermedio", 
"eva_transgnorma_logro_intermedio", "prim_sub_freq_rec_3_daily"
), assoc = c(1, 1, 1, 1, 0.837401616004359, 0.799255399535011, 
0.791868085712479, 0.772959611636135, 0.768824181592058, 0.767611700270886, 
0.766076706473342, 0.761375129411457, 0.761355425554082, 0.733589313288128, 
0.725138172244005, 0.722818280271976, 0.72099621716526, 0.72039977804688, 
0.719381408247617, 0.717249540832458, 0.712320948771543, 0.710302633191785, 
0.709722271477472, 0.701687309653258, 0.696707974039839, 0.690705556483051, 
0.681830520819954, 0.678269549972999, 0.671582983467806, 0.670352578876458, 
0.669058809936928, 0.667592189150649, 0.6672207513891, 0.665685112549776, 
0.662406708383205, 0.647843597391099, 0.642153897726994, 0.642090148171936, 
0.630483008928546, 0.611941437105595, 0.609774200941068, 0.60584878566776, 
0.603574402679172, 0.582481131849872, 0.574951835017204, 0.563805536179935, 
0.560437720337147, 0.559800818468995, 0.555303684843656, 0.540692551331995, 
0.534234865700987, 0.531457234882175, 0.519513231981636, 0.511188752777731, 
0.507145893361044, 0.482664293726349, 0.463441708853403, 0.46261708965341, 
0.45913512941893, 0.457676483658503, 0.455397932456246, 0.452738924409282, 
0.452457136361697, 0.44974282979606, 0.444149634280281, 0.443894893245039, 
0.439538828831151, 0.435478070323306, 0.434458723554865, 0.432371919215453, 
0.432116661199306, 0.43132686350352, 0.429961703009267, 0.429095039016343, 
0.418482540841951, 0.414337899804518, 0.413987589697899, 0.412116470400903, 
0.409177027096306, 0.405934195293375, 0.405839529316627, 0.399478595574996, 
0.397844167693892, 0.397393193376078, 0.397068967713469, 0.396994330980415, 
0.392717799710107, 0.39219943448603, 0.391189231394736, 0.388770032294385, 
0.386094301374706, 0.384682696520629, 0.382778247815602, 0.38136114680255, 
0.380415212294432, 0.377603723224746, 0.377162000779052, 0.376592756524567, 
0.376555052561204, 0.376340445425905, 0.376291324348346, 0.376165412726512, 
0.374970447345987, 0.374029884834893, 0.373193041292967, 0.371225796917758, 
0.370740243423497, 0.370605575763183, 0.370227539572361, 0.369323940929939, 
0.367513736339478, 0.36738101130711, 0.365715381940587, 0.365554195180622, 
0.365511743430749, 0.364113225328271, 0.364059841640477, 0.363825510441918, 
0.361860802015888, 0.360719266260515, 0.358896007255991, 0.357941025890526, 
0.356978257847534, 0.356172490150019, 0.354200114644261, 0.352829029298928, 
0.348602741676257, 0.347181236786358, 0.346352334714867, 0.346229719974837, 
0.339881750685104, 0.336010414493436, 0.333167382807469, 0.332642418561015, 
0.327807892951724, 0.323642542348114, 0.321086653142902, 0.31931530520156, 
0.309677539954709, 0.307958448082839, 0.303311143284388, 0.301513402051373, 
0.300771777450441, 0.300223351330473, 0.298626861831047, 0.297091453971603, 
0.29469006099123, 0.293655813217934, 0.292874989823608, 0.292145788368441, 
0.291310634400994, 0.289842822432128, 0.289176897379407, 0.289127130234193, 
0.287821778017296, 0.286711431582978, 0.28427883020267, 0.280902792295116, 
0.280569783312498, 0.2792139513766, 0.278237493669673, 0.277691732447965, 
0.277223448690488, 0.275268292328714, 0.271862169759677, 0.271075738292281, 
0.268825691767765, 0.266067854747569, 0.265966996912134, 0.265893347191067, 
0.259896218412027, 0.25737768232534, 0.253562948677223, 0.252532229159701, 
0.251790448626884, 0.248999992975494, 0.247520174358762, 0.24723718811568, 
0.244257300094904, 0.242439747767375, 0.241737791063895, 0.238231308074015, 
0.237866423038656, 0.236609345748556, 0.234832559166373, 0.234686214254315, 
0.234607261413058, 0.230616261756181, 0.230382141837348, 0.227676749254506, 
0.222176400910348, 0.220133675107843, 0.21958696936449, 0.218684552132242, 
0.217897280869242, 0.217764229221452, 0.216233495567431, 0.216023602255354, 
0.215864546096826, 0.215568431245759, 0.214610941502557, 0.211876195289709, 
0.209330029027258, 0.20893107547723, 0.208469235786344, 0.205214251250783, 
0.202404325524751, 0.20052747287032, 0.200523961032966)), class = c("tbl_df", 
"tbl", "data.frame"), row.names = c(NA, -209L))
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

plt.figure(figsize=(20, 18))

mask_upper = np.triu(np.ones_like(corr_reduced, dtype=bool))
plt.rcParams.update({
    "font.family": "Arial",
    "font.size": 14,
    "axes.titlesize": 26,
    "axes.labelsize": 18
})
sns.heatmap(
    corr_reduced,
    mask=mask_upper,
    cmap="RdBu_r",
    center=0.75,
    annot=annot,
    fmt="",
    linewidths=0.3,
    square=True,
    cbar_kws={"shrink": 0.6}
)
cbar = plt.gca().collections[0].colorbar
cbar.ax.tick_params(labelsize=14)
cbar.set_label("Correlación", fontsize=16)

plt.xticks(rotation=45, fontsize=14, ha="right", rotation_mode="anchor")
plt.yticks(rotation=0, fontsize=14)
plt.title(None)#"Correlation Heatmap (Associations > 0.6)", fontsize=18, weight="bold")

plt.tight_layout()
plt.savefig("cons/_figs/correlation_heatmap_publication.svg", format="svg")
plt.show()
plt.tight_layout()
plt.savefig("cons/_figs/correlation_heatmap_publication.png",
            format="png",
            dpi=300,
            bbox_inches="tight")
plt.show()
