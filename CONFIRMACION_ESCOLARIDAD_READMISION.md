# Confirmación de la asociación entre escolaridad y primera readmisión

## Propósito

Conservar la trazabilidad de la comprobación de la categoría de referencia, el signo de los coeficientes y las estimaciones asociadas a escolaridad en el modelo de primera readmisión. Esta nota complementa el apartado 1.1 de `INSTRUCCIONES_CORRECCION_TESIS_CORR4.md`.

## Resultado principal

La categoría de referencia es `1-More than high school`. En comparación con ella, las dos categorías de menor escolaridad presentan coeficientes negativos y HR inferiores a 1 para la transición 1→2, correspondiente a la primera readmisión:

| Categoría | Coeficiente | EE | HR | IC 95% | p | FMI |
|---|---:|---:|---:|---:|---:|---:|
| Secundaria completa o menos | -0,08534 | 0,02201 | 0,9182 | 0,8794-0,9587 | 1,06×10⁻⁴ | 0,0033 |
| Primaria completa o menos | -0,24207 | 0,02697 | 0,7850 | 0,7446-0,8276 | 2,83×10⁻¹⁹ | 0,0016 |

La magnitud se aleja más de 1 en la categoría de menor escolaridad. Esto es compatible con un patrón monotónico en la codificación utilizada: a menor escolaridad, menor intensidad instantánea estimada de primera readmisión. Se trata de una asociación ajustada y no de una relación causal.

## Fuente principal

Archivo:

`G:\My Drive\Alvacast\SISTRAT 2023\cons\_out\pooled_HR_multistate.csv`

Filas 13 y 14, transición `1_2_readmission`, con 19.060 eventos.

El archivo fue producido por `prediction25_multistate_semimarkov.ipynb`. La celda que construye la tabla agrupa mediante reglas de Rubin los modelos `coxph(..., robust=TRUE, x=TRUE)` ajustados en las cinco bases imputadas.

La lista de covariables y estratos de esta transición coincide con `formula_shap_readmit_clean_updated`, incluida la estratificación por modalidad de tratamiento y por egreso administrativo o disciplinario. La equivalencia se documentó comparando la fórmula con `_out/_extract_formulas.log`.

La categoría de referencia también se encuentra documentada en:

- `prediction21.qmd`, línea 1263, dentro de `DUMMY_REFERENCE`.
- `_alt_scripts/val_holdout_02_build_sets.R`, líneas 97 a 121.
- `prediction1.qmd`, línea 2836, donde se identifica `ed_attainment_corr_1_more_than_high_school` como nivel omitido.

## Evidencia complementaria de SHAP

La tabla de dirección de predictores de `prediction22_converted_mod.html` informa para readmisión:

- `mean_abs_shap=0.0416`, rango 10.
- Diferencia Q4 menos Q1: -0,1052401, IC 95%: -0,1057542;-0,1047200.
- Etiqueta: `Risk-Decreasing`.

La codificación numérica corresponde al dígito inicial del nivel, por lo que valores mayores representan menor escolaridad. La diferencia negativa indica menor valor SHAP para el grupo de menor escolaridad que para el de mayor escolaridad.

`_out/shap_60m_variable_summary.csv` también registra, para readmisión, una correlación entre la característica y SHAP de -0,8154, descrita como `higher feature values decrease SHAP`. Los archivos de evaluación de la forma funcional consideran suficiente la codificación lineal u ordinal actual.

Estas comprobaciones son concordantes, pero no deben describirse como fuentes estadísticamente independientes. El modelo Cox agrupado reproduce la fórmula derivada de SHAP y ambos análisis proceden de la misma cohorte y del mismo proceso analítico.

## Cautela sobre mortalidad

El resumen SHAP a 60 meses presenta para mortalidad una correlación positiva, compatible con mayor riesgo predicho a menor escolaridad. Sin embargo, `ed_attainment_corr` no forma parte de `formula_death_updated2`, el modelo Cox final de mortalidad. La propia salida registra `Skipping: ed_attainment - not in death formula`.

Por tanto, la señal de mortalidad debe considerarse exploratoria y no puede presentarse como un resultado del Cox final de mortalidad. En el material aportado se menciona además un HR de muerte de 1,43 obtenido mediante una comprobación de Aalen-Johansen, pero no se identifica el archivo u objeto que lo contiene. Ese valor no debe incorporarse a la tesis hasta localizar su fuente reproducible.

## Comprobaciones adicionales y elementos no localizados

- Los GVIF de los dos indicadores educacionales son 1,8317 y 1,9156. No sugieren un problema evidente de colinealidad, aunque no informan el signo del coeficiente.
- Las pruebas de Schoenfeld citadas en el material aportado tampoco informan el signo y, además, aparecen rotuladas como correspondientes al modelo de muerte pese a que escolaridad no integra su fórmula final. No se utilizan para sostener la dirección del resultado.
- No se localizó una tabla descriptiva genuina de readmisión por escolaridad. Las tablas con DME disponibles comparan las particiones de entrenamiento y validación.
- No se localizó una tabla de coeficientes firmados para `formula_readmit_updated2`, el modelo completo de aproximadamente 60 variables. Su inclusión se observa en diagnósticos de VIF y Schoenfeld, pero esos diagnósticos no permiten inferir el signo.

## Conclusión para corregir la tesis

La dirección del modelo primario está suficientemente confirmada. La Tabla 20 se encuentra correctamente rotulada. Debe corregirse la narrativa que afirma que una mayor escolaridad se asoció con menor readmisión. La redacción debe indicar que, respecto de quienes tenían más que secundaria completa, las categorías de menor escolaridad mostraron una menor intensidad instantánea estimada de primera readmisión.
