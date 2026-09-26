global root ""

ssc install reghdfe, replace
ssc install estout, replace
ssc install heatplot, replace
ssc install palettes, replace
ssc install colrspace, replace

* ==================== Figure 4: Impact of All and Large Disasters on Sales Growth====================
import delimited "$root\merged_disaster_food_customer_12C.csv", stringcols(1 2) numericcols(8 18) clear
gen gvkeycid= gvkey+"_"+cid
egen individual=group(gvkeycid)
xtset individual year_srcdate
sum sales_growth, detail
keep if sales_growth <= r(p99)
preserve
import delimited "$root\states_list.csv", clear varnames(1)
describe
keep region statefp
rename statefp state_fips
destring state_fips region, replace force
duplicates drop state_fips, force
save "$root\state_region_crosswalk.dta", replace
restore
capture drop state_fips
capture drop county_fips_clean
capture confirm numeric variable county_fips
if _rc == 0 {
    gen state_fips = floor(county_fips / 1000)
}

capture confirm string variable county_fips
if _rc == 0 {
    gen county_fips_clean = substr("00000" + trim(county_fips), -5, 5)
    gen state_fips = real(substr(county_fips_clean, 1, 2))
}
merge m:1 state_fips using "$root\state_region_crosswalk.dta"
tab _merge
tab state_fips if _merge == 1
drop if _merge == 2
drop _merge
egen region_year = group(region year_srcdate), label

reghdfe sales_growth _all_tornado, absorb( individual region_year) cluster(county_fips)
est store A1
reghdfe sales_growth _all_hurricanetropicalstorm, absorb( individual region_year) cluster(county_fips)
est store A2
reghdfe sales_growth _all_flooding, absorb(individual region_year ) cluster(county_fips)
est store A3
reghdfe sales_growth _all_earthquake, absorb( individual region_year) cluster(county_fips)
est store A4
reghdfe sales_growth _all_tsunamiseiche, absorb( individual region_year) cluster(county_fips)
est store A5
reghdfe sales_growth _all_severestormthunderstorm, absorb(individual region_year) cluster(county_fips)
est store A6
reghdfe sales_growth _all_heat, absorb( individual region_year ) cluster(county_fips)
est store A7

coefplot A1 A2 A3 A4 A5 A6 A7, drop(_cons) yline(0) ///
    vertical ytitle("Sale Growth of Food Company") ///
	legend(off) ///
    coeflabels("_all_tornado"="Tornado" "_all_hurricanetropicalstorm"="Hurricane" ///
               "_all_flooding"="Flooding" "_all_earthquake"="Earthquake" ///
               "_all_tsunamiseiche"="Tsunami/Seiche" "_all_severestormthunderstorm"="Severe Storm" ///
               "_all_heat"="Heat") ///
    xlabel(, angle(90)) plotregion(fcolor(white)) graphregion(fcolor(white))

reghdfe sales_growth _large_tornado, absorb( individual region_year) cluster(county_fips)
est store A1
reghdfe sales_growth _large_hurricanetropicalstorm, absorb( individual region_year) cluster(county_fips)
est store A2
reghdfe sales_growth _large_flooding, absorb(individual region_year ) cluster(county_fips)
est store A3
reghdfe sales_growth _large_earthquake, absorb( individual region_year) cluster(county_fips)
est store A4
reghdfe sales_growth _large_tsunamiseiche, absorb( individual region_year) cluster(county_fips)
est store A5
reghdfe sales_growth _large_severestormthunderstorm, absorb(individual region_year) cluster(county_fips)
est store A6
reghdfe sales_growth _large_heat, absorb( individual region_year ) cluster(county_fips)
est store A7

coefplot A1 A2 A3 A4 A5 A6 A7, drop(_cons) yline(0) ///
    vertical ytitle("Sale Growth of Food Company") ///
	legend(off) ///
    coeflabels("_large_tornado"="Tornado" "_large_hurricanetropicalstorm"="Hurricane" ///
               "_large_flooding"="Flooding" "_large_earthquake"="Earthquake" ///
               "_large_tsunamiseiche"="Tsunami/Seiche" "_large_severestormthunderstorm"="Severe Storm" ///
               "_large_heat"="Heat") ///
    xlabel(, angle(90)) plotregion(fcolor(white)) graphregion(fcolor(white))

* ==================== Figure 5: Heatmap for the Frequency of Disasters Affecting Agricultural Sectors - Headquarters ====================
import delimited "$root\heterogeneity hazard to sector.csv", clear
replace food_sector = "Production" if food_sector == "Agricultural Production"
replace value = . if value == 0
rename value Coef
heatplot Coef food_sector hazard, colors(hcl diverging, reverse) aspectratio(1) cuts(-0.7,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,0.7) ylabel(, nogrid labsize(small)) xlabel(, angle(45) nogrid labsize(small))  ytitle("") xtitle("") graphregion(color(white)) plotregion(color(white) margin(zero)) ramp(right labels(-0.7(0.35)0.7) format(%4.2f) length(30) plotregion(color(white) margin(zero)) graphregion(color(white) margin(zero))) note("(a) Coefficient Plot for the Frequency of Disasters Effect ", size(small) position(6))
replace Coef = . if v4 == ""
heatplot Coef food_sector hazard, colors(hcl diverging, reverse) aspectratio(1) cuts(-0.7,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,0.7) ylabel(, nogrid labsize(small)) xlabel(, angle(45) nogrid labsize(small))  ytitle("") xtitle("") graphregion(color(white)) plotregion(color(white) margin(zero)) missing(color(white)) ramp(right labels(-0.7(0.35)0.7) format(%4.2f)  length(30) plotregion(color(white) margin(zero)) graphregion(color(white) margin(zero)) ) note("(b) Significant Coefficient Plot for the Frequency of Disasters Effect ", size(small) position(6))

* ==================== Table 1: Impact of Major Damaging Disasters on Food Industry Establishment and Employment Change ====================
import delimited "$root\food_industry_disaster.csv", numericcols(6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39) clear

xtset area_fips year
gen dtap_estabs_count = tap_estabs_count-L.tap_estabs_count
gen dtap_emplvl_est_5 = tap_emplvl_est_5 -L.tap_emplvl_est_5
gen averagewage= tap_wages_est_5/ tap_emplvl_est_5
gen daveragewage = averagewage-L.averagewage
foreach v in dtap_estabs_count dtap_emplvl_est_5 {
    clonevar `v'_original = `v'
    quietly summarize `v', detail
    local upper = r(p99)
    replace `v' = `upper' if `v' > `upper' & !missing(`v')
    display "`v' P99 = `upper'"
}
reghdfe dtap_estabs_count frequency_tornado, absorb(area_fips year) cluster(area_fips)
est store A1
reghdfe dtap_estabs_count frequency_hurricanetropicalstorm, absorb(area_fips year) cluster(area_fips)
est store A2
reghdfe dtap_estabs_count frequency_flooding, absorb(area_fips year) cluster(area_fips)
est store A3
reghdfe dtap_estabs_count frequency_earthquake, absorb(area_fips year) cluster(area_fips)
est store A4
reghdfe dtap_estabs_count frequency_tsunamiseiche, absorb(area_fips year) cluster(area_fips)
est store A5
reghdfe dtap_estabs_count frequency_severestormthunderstor, absorb(area_fips year) cluster(area_fips)
est store A6
reghdfe dtap_estabs_count frequency_heat, absorb(area_fips year) cluster(area_fips)
est store A7
esttab A1 A2 A3 A4 A5 A6 A7, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)


* Table 1 - Panel B
reghdfe dtap_emplvl_est_5 frequency_tornado, absorb(area_fips year) cluster(area_fips)
est store A1
reghdfe dtap_emplvl_est_5 frequency_hurricanetropicalstorm, absorb(area_fips year) cluster(area_fips)
est store A2
reghdfe dtap_emplvl_est_5 frequency_flooding, absorb(area_fips year) cluster(area_fips)
est store A3
reghdfe dtap_emplvl_est_5 frequency_earthquake, absorb(area_fips year) cluster(area_fips)
est store A4
reghdfe dtap_emplvl_est_5 frequency_tsunamiseiche, absorb(area_fips year) cluster(area_fips)
est store A5
reghdfe dtap_emplvl_est_5 frequency_severestormthunderstor, absorb(area_fips year) cluster(area_fips)
est store A6
reghdfe dtap_emplvl_est_5 frequency_heat, absorb(area_fips year) cluster(area_fips)
est store A7
esttab A1 A2 A3 A4 A5 A6 A7, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)


* ==================== Table 2: Change in Sale Concentration Moderation Effect ====================
import delimited "$root\merged_disaster_food_customer_12C.csv", stringcols(1 2) numericcols(8 18 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80) clear
gen gvkeycid= gvkey+"_"+cid
egen individual=group(gvkeycid)
xtset individual year_srcdate
sum sales_growth, detail
keep if sales_growth <= r(p99)
replace growth_mainsale_ratio = growth_mainsale_ratio * 100
replace allhurri_mainratio = _all_hurricanetropicalstorm* growth_mainsale_ratio
preserve
import delimited "$root\states_list.csv", clear varnames(1)
describe
keep region statefp
rename statefp state_fips
destring state_fips region, replace force
duplicates drop state_fips, force
save "$root\state_region_crosswalk.dta", replace
restore
capture drop state_fips
capture drop county_fips_clean
capture confirm numeric variable county_fips
if _rc == 0 {
    gen state_fips = floor(county_fips / 1000)
}

capture confirm string variable county_fips
if _rc == 0 {
    gen county_fips_clean = substr("00000" + trim(county_fips), -5, 5)
    gen state_fips = real(substr(county_fips_clean, 1, 2))
}
merge m:1 state_fips using "$root\state_region_crosswalk.dta"
tab _merge
tab state_fips if _merge == 1
drop if _merge == 2
drop _merge
egen region_year = group(region year_srcdate), label

reghdfe sales_growth _all_hurricanetropicalstorm , absorb(individual year_srcdate) cluster(county_fips)
est store A1
reghdfe sales_growth _all_hurricanetropicalstorm growth_mainsale_ratio , absorb(individual year_srcdate) cluster(county_fips)
est store A2
reghdfe sales_growth _all_hurricanetropicalstorm growth_mainsale_ratio allhurri_mainratio , absorb(individual year_srcdate) cluster(county_fips)
est store A3
margins, at(growth_mainsale_ratio=(0 10)) expression(_b[_all_hurricanetropicalstorm] + _b[allhurri_mainratio]*growth_mainsale_ratio)
reghdfe sales_growth _all_hurricanetropicalstorm growth_mainsale_ratio allhurri_mainratio roa size  rd_to_sales operating_leverage cash_holdings , absorb(individual year_srcdate) cluster(county_fips)
est store A4
margins, at(growth_mainsale_ratio=(0 10)) expression(_b[_all_hurricanetropicalstorm] + _b[allhurri_mainratio]*growth_mainsale_ratio)
reghdfe sales_growth _all_hurricanetropicalstorm growth_mainsale_ratio allhurri_mainratio roa size  rd_to_sales operating_leverage cash_holdings, absorb(individual year_srcdate region_year) cluster(county_fips)
est store A5
margins, at(growth_mainsale_ratio=(0 10)) expression(_b[_all_hurricanetropicalstorm] + _b[allhurri_mainratio]*growth_mainsale_ratio)
esttab A1 A2 A3 A4 A5, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)

* ==================== Table 3： Segment Sale Moderation Effect ====================
import delimited "$root\merged_disaster_food_customer_12S.csv", stringcols(1 2) numericcols(8 18 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76) clear
gen gvkeycid= gvkey+"_"+cid
egen individual=group(gvkeycid)
xtset individual year_srcdate
sum sales_growth, detail
keep if sales_growth <= r(p99)

gen hurri_busseg=_all_hurricanetropicalstorm* vd_busseg
gen hurri_geoseg=_all_hurricanetropicalstorm* vd_geoseg
preserve
import delimited "$root\states_list.csv", clear varnames(1)
describe
keep region statefp
rename statefp state_fips
destring state_fips region, replace force
duplicates drop state_fips, force
save "$root\state_region_crosswalk.dta", replace
restore
capture drop state_fips
capture drop county_fips_clean
capture confirm numeric variable county_fips
if _rc == 0 {
    gen state_fips = floor(county_fips / 1000)
}

capture confirm string variable county_fips
if _rc == 0 {
    gen county_fips_clean = substr("00000" + trim(county_fips), -5, 5)
    gen state_fips = real(substr(county_fips_clean, 1, 2))
}
merge m:1 state_fips using "$root\state_region_crosswalk.dta"
tab _merge
tab state_fips if _merge == 1
drop if _merge == 2
drop _merge
egen region_year = group(region year_srcdate), label

reghdfe sales_growth _all_hurricanetropicalstorm , absorb(individual year_srcdate) cluster(county_fips)
est store A1
reghdfe sales_growth _all_hurricanetropicalstorm  vd_busseg, absorb(individual year_srcdate) cluster(county_fips)
est store A2
reghdfe sales_growth _all_hurricanetropicalstorm  vd_busseg hurri_busseg, absorb(individual year_srcdate) cluster(county_fips)
est store A3
reghdfe sales_growth _all_hurricanetropicalstorm  vd_geoseg, absorb(individual year_srcdate) cluster(county_fips)
est store A4
reghdfe sales_growth _all_hurricanetropicalstorm  vd_geoseg hurri_geoseg, absorb(individual year_srcdate) cluster(county_fips)
est store A5
margins, at(vd_geoseg=(0 10)) expression(_b[_all_hurricanetropicalstorm] + _b[hurri_geoseg]*vd_geoseg)
reghdfe sales_growth _all_hurricanetropicalstorm vd_geoseg hurri_geoseg roa size gross_profit_margin operating_leverage cash_holdings , absorb(individual year_srcdate) cluster(county_fips)
est store A6
margins, at(vd_geoseg=(0 10)) expression(_b[_all_hurricanetropicalstorm] + _b[hurri_geoseg]*vd_geoseg)
reghdfe sales_growth _all_hurricanetropicalstorm vd_geoseg hurri_geoseg roa size gross_profit_margin operating_leverage cash_holdings  , absorb(individual year_srcdate region_year) cluster(county_fips)
est store A7
margins, at(vd_geoseg=(0 10)) expression(_b[_all_hurricanetropicalstorm] + _b[hurri_geoseg]*vd_geoseg)
esttab A1 A2 A3 A4 A5 A6 A7, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)

* ==================== Table 4: Cluster Scenario Moderation Effect ====================
import delimited "$root\merged_disaster_food_customer_12dataaxle.csv", numericcols(3 5 6 7 8 9 10 11 12) clear
xtset parent_number archive_version_year
sum sales_growth, detail
keep if sales_growth <= 1 &sales_growth >= -1
gen p_branch_cluster = branches_in_clusters/total_branches*100
gen hurri_bcluster = frequency_hurricane* p_branch_cluster
gen hurri_ncluster = frequency_hurricane* num_clusters
gen hurri_nbranch = frequency_hurricane* total_branches
gen age = archive_version_year - year_established
gen per_employee = parent_actual_employee_size/total_employee
gen size_parent_employee = log(parent_actual_employee_size)
gen size_employee = log(total_employee)
gen size_per_employee = log(per_employee)

reghdfe sales_growth frequency_hurricane, absorb( parent_number archive_version_year) cluster(county_fips)
est store A1
reghdfe sales_growth frequency_hurricane num_clusters, absorb( parent_number archive_version_year) cluster(county_fips)
est store A2
reghdfe sales_growth frequency_hurricane num_clusters hurri_ncluster, absorb( parent_number archive_version_year) cluster(county_fips)
est store A3
margins, at(num_clusters=(0 1)) expression(_b[frequency_hurricane] + _b[hurri_ncluster]*num_clusters)
reghdfe sales_growth frequency_hurricane num_clusters hurri_ncluster total_employee , absorb( parent_number archive_version_year) cluster(county_fips)
est store A4
margins, at(num_clusters=(0 1)) expression(_b[frequency_hurricane] + _b[hurri_ncluster]*num_clusters)
reghdfe sales_growth frequency_hurricane total_branches, absorb( parent_number archive_version_year ) cluster(county_fips)
est store A5
reghdfe sales_growth frequency_hurricane total_branches hurri_nbranch , absorb( parent_number archive_version_year ) cluster(county_fips)
est store A6
margins, at(total_branches=(0 1)) expression(_b[frequency_hurricane] + _b[hurri_nbranch]*total_branches)
reghdfe sales_growth frequency_hurricane total_branches hurri_nbranch total_employee, absorb( parent_number archive_version_year ) cluster(county_fips)
est store A7
margins, at(total_branches=(0 1)) expression(_b[frequency_hurricane] + _b[hurri_nbranch]*total_branches)
reghdfe sales_growth frequency_hurricane p_branch_cluster, absorb( parent_number archive_version_year) cluster(county_fips)
est store A8
reghdfe sales_growth frequency_hurricane p_branch_cluster hurri_bcluster , absorb( parent_number archive_version_year) cluster(county_fips)
est store A9
margins, at(p_branch_cluster=(0 10)) expression(_b[frequency_hurricane] + _b[hurri_bcluster]*p_branch_cluster)
reghdfe sales_growth frequency_hurricane p_branch_cluster hurri_bcluster total_employee, absorb( parent_number archive_version_year) cluster(county_fips)
est store A10
margins, at(p_branch_cluster=(0 10)) expression(_b[frequency_hurricane] + _b[hurri_bcluster]*p_branch_cluster)
esttab A1 A2 A3 A4 A5 A6 A7 A8 A9 A10, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)

* ==================== Figure A5: Robust Heatmap for the Frequency of Disasters Affecting Agricultural Sectors- Headquarters ====================
import delimited "$root\heterogeneity hazard to sector.csv", clear
replace food_sector = "Production" if food_sector == "Agricultural Production"
replace rcoef = . if rcoef == 0
heatplot rcoef food_sector hazard, colors(hcl diverging, reverse) aspectratio(1) cuts(-0.7,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,0.7) ylabel(, nogrid labsize(small)) xlabel(, angle(45) nogrid labsize(small))  ytitle("") xtitle("") graphregion(color(white)) plotregion(color(white) margin(zero)) ramp(right labels(-0.7(0.35)0.7) format(%4.2f) length(30) plotregion(color(white) margin(zero)) graphregion(color(white) margin(zero))) note("(a) Robust Coefficient Plot for the Frequency of Disasters Effect ", size(small) position(6))
replace rcoef = . if v8 == ""
heatplot rcoef food_sector hazard, colors(hcl diverging, reverse) aspectratio(1) cuts(-0.9,-0.6,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,0.6,0.9) ylabel(, nogrid labsize(small)) xlabel(, angle(45) nogrid labsize(small))  ytitle("") xtitle("") graphregion(color(white)) plotregion(color(white) margin(zero)) missing(color(white)) ramp(right labels(-0.9(0.45)0.9) format(%4.2f)  length(30) plotregion(color(white) margin(zero)) graphregion(color(white) margin(zero)) ) note("(b) Robust Significant Coefficient Plot for Percentage of Branches Affected by Different Disasters ", size(small) position(6))

* ==================== Table A4: Impact of Main Damaging Disasters on Non-food Firm Sale Groth ====================
import delimited "$root\merged_disaster_nonfood_customer.csv", stringcols(1 2) numericcols(8 18) clear
gen gvkeycid= gvkey+"_"+cid
egen individual=group(gvkeycid)
xtset individual year_srcdate
sum sales_growth, detail
keep if sales_growth <= r(p99)

reghdfe sales_growth _all_tornado, absorb( individual region_year) cluster(county_fips)
est store A1
reghdfe sales_growth _all_hurricanetropicalstorm, absorb( individual region_year) cluster(county_fips)
est store A2
reghdfe sales_growth _all_flooding, absorb(individual region_year ) cluster(county_fips)
est store A3
reghdfe sales_growth _all_earthquake, absorb( individual region_year) cluster(county_fips)
est store A4
reghdfe sales_growth _all_tsunamiseiche, absorb( individual region_year) cluster(county_fips)
est store A5
reghdfe sales_growth _all_severestormthunderstorm, absorb(individual region_year) cluster(county_fips)
est store A6
reghdfe sales_growth _all_heat, absorb( individual region_year ) cluster(county_fips)
est store A7
esttab A1 A2 A3 A4 A5 A6 A7, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)

* ==================== Table A5 Current and Leaad Effects of Natural Disasters on Food Firm Sales Growth====================
import delimited "$root\merged_disaster_food_customer_12C.csv", stringcols(1 2) numericcols(8 18) clear
gen gvkeycid= gvkey+"_"+cid
egen individual=group(gvkeycid)
xtset individual year_srcdate
sum sales_growth, detail
keep if sales_growth <= r(p99)
preserve
import delimited "$root\states_list.csv", clear varnames(1)
describe
keep region statefp
rename statefp state_fips
destring state_fips region, replace force
duplicates drop state_fips, force
save "$root\state_region_crosswalk.dta", replace
restore
capture drop state_fips
capture drop county_fips_clean
capture confirm numeric variable county_fips
if _rc == 0 {
    gen state_fips = floor(county_fips / 1000)
}

capture confirm string variable county_fips
if _rc == 0 {
    gen county_fips_clean = substr("00000" + trim(county_fips), -5, 5)
    gen state_fips = real(substr(county_fips_clean, 1, 2))
}
merge m:1 state_fips using "$root\state_region_crosswalk.dta"
tab _merge
tab state_fips if _merge == 1
drop if _merge == 2
drop _merge
egen region_year = group(region year_srcdate), label

preserve

    import delimited "$root\merged_disaster_food_customer_12.csv", ///
        stringcols(1 2) numericcols(8 18) clear

    capture confirm numeric variable year_srcdate
    if _rc {
        destring year_srcdate, replace force
    }

    keep county_fips year_srcdate ///
        _all_tornado ///
        _all_hurricanetropicalstorm ///
        _all_flooding ///
        _all_earthquake ///
        _all_tsunamiseiche ///
        _all_severestormthunderstorm ///
        _all_heat

    drop if missing(county_fips) | missing(year_srcdate)

    collapse (max) ///
        cur_tornado   = _all_tornado ///
        cur_hurricane = _all_hurricanetropicalstorm ///
        cur_flood     = _all_flooding ///
        cur_quake     = _all_earthquake ///
        cur_tsunami   = _all_tsunamiseiche ///
        cur_storm     = _all_severestormthunderstorm ///
        cur_heat      = _all_heat, ///
        by(county_fips year_srcdate)

    egen county_id = group(county_fips)
    xtset county_id year_srcdate

    gen lead_tornado   = F1.cur_tornado
    gen lead_hurricane = F1.cur_hurricane
    gen lead_flood     = F1.cur_flood
    gen lead_quake     = F1.cur_quake
    gen lead_tsunami   = F1.cur_tsunami
    gen lead_storm     = F1.cur_storm
    gen lead_heat      = F1.cur_heat

    keep county_fips year_srcdate ///
        cur_tornado cur_hurricane cur_flood cur_quake cur_tsunami cur_storm cur_heat ///
        lead_tornado lead_hurricane lead_flood lead_quake lead_tsunami lead_storm lead_heat

    tempfile disaster_lead_current
    save `disaster_lead_current', replace

restore

capture drop cur_* lead_* disaster_current disaster_lead
merge m:1 county_fips year_srcdate using `disaster_lead_current', keep(master match) nogen

est clear

local disasters "tornado hurricane flood quake tsunami storm heat"
local i = 1

foreach d of local disasters {

    capture drop disaster_current disaster_lead

    gen disaster_current = cur_`d'
    gen disaster_lead    = lead_`d'

    label var disaster_current "Disaster"
    label var disaster_lead "One-year lead shock"

    reghdfe sales_growth disaster_current disaster_lead, ///
        absorb(individual region_year) ///
        cluster(county_fips)

    estadd local SCFE "YES"
    estadd local RYFE "YES"

    est store A`i'

    local ++i
}

esttab A1 A2 A3 A4 A5 A6 A7, ///
    compress ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(disaster_current disaster_lead) ///
    order(disaster_current disaster_lead) ///
    coeflabels( ///
        disaster_current "Disaster" ///
        disaster_lead "One-year lead shock" ///
    ) ///
    mtitles("Tornado" "Hurricane" "Flooding" "Earthquake" "Tsunami" "Thunderstorm" "Heat") ///
    stats(SCFE RYFE N r2, ///
        labels("Supplier-Customer FE" "Region-Year FE" "Obs." "R-squared") ///
        fmt(%9s %9s %9.0fc %9.3f) ///
    )

	
	

* ======================== Table A6 Robustness Check for Sale Concentration Moderation Effect========================
import delimited "$root\merged_disaster_food_customer_12C.csv", stringcols(1 2) numericcols(8 18 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80) clear
gen long source_row = _n
gen gvkeycid = gvkey + "_" + cid
egen individual = group(gvkeycid)
xtset individual year_srcdate
summarize sales_growth, detail
gen byte trim_base = sales_growth <= r(p99)
replace growth_mainsale_ratio = growth_mainsale_ratio*100
replace allhurri_mainratio = _all_hurricanetropicalstorm*growth_mainsale_ratio
gen largehurri_mainratio = _large_hurricanetropicalstorm*growth_mainsale_ratio

* Large hurricanes
gen byte eligible_A6_1 = (trim_base) & !missing(sales_growth, _large_hurricanetropicalstorm, individual, year_srcdate, county_fips)
reghdfe sales_growth _large_hurricanetropicalstorm if trim_base, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A6_1 = e(sample)
estimates store A6_1
gen byte eligible_A6_2 = (trim_base) & !missing(sales_growth, _large_hurricanetropicalstorm, growth_mainsale_ratio, largehurri_mainratio, individual, year_srcdate, county_fips)
reghdfe sales_growth _large_hurricanetropicalstorm growth_mainsale_ratio largehurri_mainratio if trim_base, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A6_2 = e(sample)
estimates store A6_2

* Weighting
gen byte eligible_A6_3 = (trim_base & salecs > 0 & salecs < .) & !missing(sales_growth, _all_hurricanetropicalstorm, individual, year_srcdate, county_fips, salecs)
reghdfe sales_growth _all_hurricanetropicalstorm if trim_base & salecs > 0 & salecs < . [aw=salecs], absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A6_3 = e(sample)
estimates store A6_3
gen byte eligible_A6_4 = (trim_base & salecs > 0 & salecs < .) & !missing(sales_growth, _all_hurricanetropicalstorm, growth_mainsale_ratio, allhurri_mainratio, individual, year_srcdate, county_fips, salecs)
reghdfe sales_growth _all_hurricanetropicalstorm growth_mainsale_ratio allhurri_mainratio if trim_base & salecs > 0 & salecs < . [aw=salecs], absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A6_4 = e(sample)
estimates store A6_4

* Exclude COVID
gen byte eligible_A6_5 = (trim_base & year_srcdate <= 2019) & !missing(sales_growth, _all_hurricanetropicalstorm, individual, year_srcdate, county_fips)
reghdfe sales_growth _all_hurricanetropicalstorm if trim_base & year_srcdate <= 2019, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A6_5 = e(sample)
estimates store A6_5
gen byte eligible_A6_6 = (trim_base & year_srcdate <= 2019) & !missing(sales_growth, _all_hurricanetropicalstorm, growth_mainsale_ratio, allhurri_mainratio, individual, year_srcdate, county_fips)
reghdfe sales_growth _all_hurricanetropicalstorm growth_mainsale_ratio allhurri_mainratio if trim_base & year_srcdate <= 2019, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A6_6 = e(sample)
estimates store A6_6

esttab A6_1 A6_2 A6_3 A6_4 A6_5 A6_6, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)

* ======================== Table A7 Robustness Check for Segment Sale Moderation Effect========================
import delimited "$root\merged_disaster_food_customer_12S.csv", stringcols(1 2) numericcols(8 18 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75) clear
gen long source_row = _n
gen gvkeycid = gvkey + "_" + cid
egen individual = group(gvkeycid)
xtset individual year_srcdate
summarize sales_growth, detail
gen byte trim_base = sales_growth <= r(p99)
gen hurri_busseg = _all_hurricanetropicalstorm*vd_busseg
gen hurri_geoseg = _all_hurricanetropicalstorm*vd_geoseg
gen largehurri_busseg = _large_hurricanetropicalstorm*vd_busseg
gen largehurri_geoseg = _large_hurricanetropicalstorm*vd_geoseg

* Large hurricanes
gen byte eligible_A7_1 = (trim_base) & !missing(sales_growth, _large_hurricanetropicalstorm, vd_busseg, largehurri_busseg, individual, year_srcdate, county_fips)
reghdfe sales_growth _large_hurricanetropicalstorm vd_busseg largehurri_busseg if trim_base, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A7_1 = e(sample)
estimates store A7_1
gen byte eligible_A7_2 = (trim_base) & !missing(sales_growth, _large_hurricanetropicalstorm, vd_geoseg, largehurri_geoseg, individual, year_srcdate, county_fips)
reghdfe sales_growth _large_hurricanetropicalstorm vd_geoseg largehurri_geoseg if trim_base, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A7_2 = e(sample)
estimates store A7_2

* Weighting
gen byte eligible_A7_3 = (trim_base & salecs > 0 & salecs < .) & !missing(sales_growth, _all_hurricanetropicalstorm, vd_busseg, hurri_busseg, individual, year_srcdate, county_fips, salecs)
reghdfe sales_growth _all_hurricanetropicalstorm vd_busseg hurri_busseg if trim_base & salecs > 0 & salecs < . [aw=salecs], absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A7_3 = e(sample)
estimates store A7_3
gen byte eligible_A7_4 = (trim_base & salecs > 0 & salecs < .) & !missing(sales_growth, _all_hurricanetropicalstorm, vd_geoseg, hurri_geoseg, individual, year_srcdate, county_fips, salecs)
reghdfe sales_growth _all_hurricanetropicalstorm vd_geoseg hurri_geoseg if trim_base & salecs > 0 & salecs < . [aw=salecs], absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A7_4 = e(sample)
estimates store A7_4

* Exclude COVID
gen byte eligible_A7_5 = (trim_base & year_srcdate <= 2019) & !missing(sales_growth, _all_hurricanetropicalstorm, vd_busseg, hurri_busseg, individual, year_srcdate, county_fips)
reghdfe sales_growth _all_hurricanetropicalstorm vd_busseg hurri_busseg if trim_base & year_srcdate <= 2019, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A7_5 = e(sample)
estimates store A7_5
gen byte eligible_A7_6 = (trim_base & year_srcdate <= 2019) & !missing(sales_growth, _all_hurricanetropicalstorm, vd_geoseg, hurri_geoseg, individual, year_srcdate, county_fips)
reghdfe sales_growth _all_hurricanetropicalstorm vd_geoseg hurri_geoseg if trim_base & year_srcdate <= 2019, absorb(individual year_srcdate) cluster(county_fips)
gen byte sample_A7_6 = e(sample)
estimates store A7_6

esttab A7_1 A7_2 A7_3 A7_4 A7_5 A7_6, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)

* ======================== Table A8 Robustness Check for Scenario Moderation Effect========================
import delimited "$root\merged_disaster_food_customer_12dataaxle.csv", numericcols(3 5 6 7 8 9 10 11 12) clear
gen long source_row = _n
xtset parent_number archive_version_year
gen byte trim_base = sales_growth >= -1 & sales_growth <= 1
gen p_branch_cluster = branches_in_clusters/total_branches*100
gen hurri_ncluster = frequency_hurricane*num_clusters
gen hurri_nbranch = frequency_hurricane*total_branches
gen hurri_bcluster = frequency_hurricane*p_branch_cluster
gen largehurri_ncluster = large_hurricane*num_clusters
gen largehurri_nbranch = large_hurricane*total_branches
gen largehurri_bcluster = large_hurricane*p_branch_cluster

* Large hurricanes
gen byte eligible_A8_1 = (trim_base) & !missing(sales_growth, large_hurricane, num_clusters, largehurri_ncluster, parent_number, archive_version_year, county_fips)
reghdfe sales_growth large_hurricane num_clusters largehurri_ncluster if trim_base, absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_1 = e(sample)
estimates store A8_1
gen byte eligible_A8_2 = (trim_base) & !missing(sales_growth, large_hurricane, total_branches, largehurri_nbranch, parent_number, archive_version_year, county_fips)
reghdfe sales_growth large_hurricane total_branches largehurri_nbranch if trim_base, absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_2 = e(sample)
estimates store A8_2
gen byte eligible_A8_3 = (trim_base) & !missing(sales_growth, large_hurricane, p_branch_cluster, largehurri_bcluster, parent_number, archive_version_year, county_fips)
reghdfe sales_growth large_hurricane p_branch_cluster largehurri_bcluster if trim_base, absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_3 = e(sample)
estimates store A8_3

* Weighting
gen byte eligible_A8_4 = (trim_base & parent_actual_sales_volume > 0 & parent_actual_sales_volume < .) & !missing(sales_growth, frequency_hurricane, num_clusters, hurri_ncluster, parent_number, archive_version_year, county_fips, parent_actual_sales_volume)
reghdfe sales_growth frequency_hurricane num_clusters hurri_ncluster if trim_base & parent_actual_sales_volume > 0 & parent_actual_sales_volume < . [aw=parent_actual_sales_volume], absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_4 = e(sample)
estimates store A8_4
gen byte eligible_A8_5 = (trim_base & parent_actual_sales_volume > 0 & parent_actual_sales_volume < .) & !missing(sales_growth, frequency_hurricane, total_branches, hurri_nbranch, parent_number, archive_version_year, county_fips, parent_actual_sales_volume)
reghdfe sales_growth frequency_hurricane total_branches hurri_nbranch if trim_base & parent_actual_sales_volume > 0 & parent_actual_sales_volume < . [aw=parent_actual_sales_volume], absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_5 = e(sample)
estimates store A8_5
gen byte eligible_A8_6 = (trim_base & parent_actual_sales_volume > 0 & parent_actual_sales_volume < .) & !missing(sales_growth, frequency_hurricane, p_branch_cluster, hurri_bcluster, parent_number, archive_version_year, county_fips, parent_actual_sales_volume)
reghdfe sales_growth frequency_hurricane p_branch_cluster hurri_bcluster if trim_base & parent_actual_sales_volume > 0 & parent_actual_sales_volume < . [aw=parent_actual_sales_volume], absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_6 = e(sample)
estimates store A8_6

* Exclude COVID
gen byte eligible_A8_7 = (trim_base & archive_version_year <= 2019) & !missing(sales_growth, frequency_hurricane, num_clusters, hurri_ncluster, parent_number, archive_version_year, county_fips)
reghdfe sales_growth frequency_hurricane num_clusters hurri_ncluster if trim_base & archive_version_year <= 2019, absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_7 = e(sample)
estimates store A8_7
gen byte eligible_A8_8 = (trim_base & archive_version_year <= 2019) & !missing(sales_growth, frequency_hurricane, total_branches, hurri_nbranch, parent_number, archive_version_year, county_fips)
reghdfe sales_growth frequency_hurricane total_branches hurri_nbranch if trim_base & archive_version_year <= 2019, absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_8 = e(sample)
estimates store A8_8
gen byte eligible_A8_9 = (trim_base & archive_version_year <= 2019) & !missing(sales_growth, frequency_hurricane, p_branch_cluster, hurri_bcluster, parent_number, archive_version_year, county_fips)
reghdfe sales_growth frequency_hurricane p_branch_cluster hurri_bcluster if trim_base & archive_version_year <= 2019, absorb(parent_number archive_version_year) cluster(county_fips)
gen byte sample_A8_9 = e(sample)
estimates store A8_9

esttab A8_1 A8_2 A8_3 A8_4 A8_5 A8_6 A8_7 A8_8 A8_9, replace b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)
