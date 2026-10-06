/*
Project: Water conservation pilot
Purpose: Assign eligible districts to the pilot before baseline fieldwork
*/

clear all
set more off

global project "C:/Users/jdoe/Documents/water-pilot"

import delimited using "$project/data/raw/sampling_frame.csv", varnames(1) clear

drop if missing(district_id)
replace district_name = strtrim(district_name)
replace zone = strproper(strtrim(zone))

duplicates report district_id
tab zone
tab district_type
summarize target_households, detail

* Randomize within each zone so treatment is balanced geographically.
gen random_order = runiform()
sort random_order
gen treatment = _n <= 6

gen assignment_date = date("15jan2025", "DMY")
format assignment_date %td

bysort zone: egen treated_in_zone = total(treatment)
tab zone treatment, row
summarize target_households, detail

label variable target_households "Households targeted for baseline survey"
label variable assignment_date "Date treatment was assigned"

order district_id district_name zone district_type treatment random_order
sort district_id

save "$project/data/analysis_working.dta", replace

export delimited district_id district_name zone district_type treatment ///
    assignment_date using "$project/outputs/treatment_assignment.csv", replace
