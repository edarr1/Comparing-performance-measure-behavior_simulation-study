SELECT *
FROM mimic_my.last_in_icu
WHERE subject_id IN ('14634306', '16032226', '15573438')
ORDER BY subject_id, icu_intime;