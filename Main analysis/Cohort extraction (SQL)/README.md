
# Cohort Extraction

## Script Order
1) sepsis_last_icu
2) sepsis_features
3) sepsis_refine
4) sepsis_final

## 1) sepsis_last_icu.sql 
last_in_icu table contains each patient's most recent icu stay.\
icudetail table limits the mimic derived icustay_detail table to the encounters in last_in_icu.\
sepsis_last_icu table inner joins icudetail and mimic sepsis 3 table.

## 2) sepsis_features.sql 
adds additional features such as age, height, first day icu lab values to the sepsis_last_icu table.

## 3) sepsis_refine.sql 
further refines sepsis_features based on age and los inclusion criteria.

## 4) sepsis_final.sql 
final addition of features to the sepsis_refine table such as first day vitals, charlson score, vent status, and outcome.
