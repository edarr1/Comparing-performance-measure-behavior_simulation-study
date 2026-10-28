drop table if EXISTS mimic_my.last_in_icu;

--select patient's last ICU admit stay_id
create table mimic_my.last_in_icu as 

with pa as(
	select pa.subject_id,"max"(icu_intime) as last_icu_intime
	from mimiciv_derived.icustay_detail pa
	group by pa.subject_id
)
	select st.subject_id,st.stay_id,st.icu_intime
	from mimiciv_derived.icustay_detail st
	where st.icu_intime in (select pa.last_icu_intime from pa)
;

--icudetail
drop table if exists mimic_my.icudetail;
create table mimic_my.icudetail AS 
	SELECT icudetail.*
	from mimiciv_derived.icustay_detail icudetail
	where icudetail.stay_id in (select last_in_icu.stay_id from mimic_my.last_in_icu);


-- sepsis_last_icu
drop table if exists mimic_my.sepsis_last_icu;
create table mimic_my.sepsis_last_icu as 
select 
	sps.*,
    (sps.antibiotic_time::date - sps.suspected_infection_time::date) 
        as sus_anti_period,
    (sps.antibiotic_time::date - sps.culture_time::date) 
        as cul_anti_period,
	gender,
	dod,
	race, 
	-- hospital level factors
	los_hospital, 
	hospital_expire_flag,
	first_hosp_stay,
	los_icu,
	first_icu_stay,
	CASE
    WHEN  los_icu<=30 and hospital_expire_flag = 1 THEN 1
    ELSE 0 END AS death_within_30_days
from mimic_my.icudetail
inner join mimiciv_derived.sepsis3 sps
on sps.subject_id = icudetail.subject_id
and sps.stay_id = icudetail.stay_id
;

select count(subject_id) from mimic_my.icudetail;	--65369
select count(subject_id) from mimic_my.sepsis_last_icu; --27676

