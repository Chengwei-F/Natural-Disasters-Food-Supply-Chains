clear all
set more off

args project
if `"`project'"' == "" local project "`c(pwd)'"
local input `"`project'/input"'
local output `"`project'/output"'
capture mkdir "`output'"

foreach cmd in ftools reghdfe esttab estadd coefplot heatplot colorpalette {
    capture which `cmd'
    if _rc {
        display as error "Required command: `cmd'"
        exit 499
    }
}
capture findfile lcolrspace.mlib
if _rc exit 499

local tables "01 02 03 04 a04 a05 a06 a07 a08"
foreach t of local tables {
    confirm file "`input'/table_`t'.dta"
}
confirm file "`input'/figure_04.dta"
confirm file "`input'/heatmap.dta"
capture log close analysis
log using "`output'/analysis.log", text replace name(analysis)

local hazards "tornado hurricane flood earthquake tsunami storm heat"
local fmt "b(%9.4f) se(%9.4f) parentheses star(* 0.10 ** 0.05 *** 0.01)"

capture noisily {

    display as text _newline "Figure 4"
    use "`input'/figure_04.dta", clear
    foreach p in shock large {
        estimates clear
        local models ""
        local j = 0
        foreach h of local hazards {
            local ++j
            reghdfe sales_growth `p'_`h', absorb(pair_id region_year) cluster(county_id)
            estimates store M`j'
            local models "`models' M`j'"
        }
        coefplot `models', drop(_cons) vertical yline(0) legend(off) ///
            ytitle("Sales growth") ///
            coeflabels(`p'_tornado="Tornado" `p'_hurricane="Hurricane" ///
                `p'_flood="Flooding" `p'_earthquake="Earthquake" ///
                `p'_tsunami="Tsunami/Seiche" `p'_storm="Severe storm" `p'_heat="Heat") ///
            xlabel(, angle(90)) plotregion(fcolor(white)) graphregion(fcolor(white))
        local panel = cond("`p'" == "shock", "a", "b")
        graph export "`output'/figure_04_`panel'.png", replace
    }

    display as text _newline "Table 1"
    use "`input'/table_01.dta", clear
    foreach y in estab_change employment_change {
        estimates clear
        local models ""
        local j = 0
        foreach h of local hazards {
            local ++j
            reghdfe `y' shock_`h', absorb(county_id year) cluster(county_id)
            estimates store M`j'
            local models "`models' M`j'"
        }
        local panel = cond("`y'" == "estab_change", "A", "B")
        esttab `models', `fmt' title("Table 1 - Panel `panel'")
    }

    display as text _newline "Table 2"
    use "`input'/table_02.dta", clear
    estimates clear
    local controls "roa size rd_to_sales operating_leverage cash_holdings"
    local rhs1 "shock_hurricane"
    local rhs2 "shock_hurricane concentration"
    local rhs3 "shock_hurricane concentration hurricane_concentration"
    local rhs4 "`rhs3' `controls'"
    local rhs5 "`rhs4'"
    forvalues j = 1/5 {
        local fe "pair_id year"
        if `j' == 5 local fe "pair_id year region_year"
        reghdfe sales_growth `rhs`j'', absorb(`fe') cluster(county_id)
        estimates store M`j'
        if `j' >= 3 {
            margins, at(concentration=(0 10)) ///
                expression(_b[shock_hurricane] + _b[hurricane_concentration]*concentration)
        }
    }
    esttab M1 M2 M3 M4 M5, `fmt' title("Table 2")

    display as text _newline "Table 3"
    use "`input'/table_03.dta", clear
    estimates clear
    local controls "roa size gross_profit_margin operating_leverage cash_holdings"
    local rhs1 "shock_hurricane"
    local rhs2 "shock_hurricane business_div"
    local rhs3 "shock_hurricane business_div hurricane_business"
    local rhs4 "shock_hurricane geographic_div"
    local rhs5 "shock_hurricane geographic_div hurricane_geographic"
    local rhs6 "`rhs5' `controls'"
    local rhs7 "`rhs6'"
    forvalues j = 1/7 {
        local fe "pair_id year"
        if `j' == 7 local fe "pair_id year region_year"
        reghdfe sales_growth `rhs`j'', absorb(`fe') cluster(county_id)
        estimates store M`j'
        if `j' >= 5 {
            margins, at(geographic_div=(0 10)) ///
                expression(_b[shock_hurricane] + _b[hurricane_geographic]*geographic_div)
        }
    }
    esttab M1 M2 M3 M4 M5 M6 M7, `fmt' title("Table 3")

    display as text _newline "Table 4"
    use "`input'/table_04.dta", clear
    estimates clear
    reghdfe sales_growth shock_hurricane, absorb(firm_id year) cluster(county_id)
    estimates store M1
    local j = 1
    foreach x in clusters branches cluster_share {
        local step = cond("`x'" == "cluster_share", 10, 1)
        local ++j
        reghdfe sales_growth shock_hurricane `x', absorb(firm_id year) cluster(county_id)
        estimates store M`j'
        foreach control in none employment {
            local z ""
            if "`control'" == "employment" local z "employment"
            local ++j
            reghdfe sales_growth shock_hurricane `x' hurricane_`x' `z', ///
                absorb(firm_id year) cluster(county_id)
            estimates store M`j'
            margins, at(`x'=(0 `step')) ///
                expression(_b[shock_hurricane] + _b[hurricane_`x']*`x')
        }
    }
    esttab M1 M2 M3 M4 M5 M6 M7 M8 M9 M10, `fmt' title("Table 4")

    display as text _newline "Table A4"
    use "`input'/table_a04.dta", clear
    estimates clear
    local models ""
    local j = 0
    foreach h of local hazards {
        local ++j
        reghdfe sales_growth shock_`h', absorb(pair_id region_year) cluster(county_id)
        estimates store M`j'
        local models "`models' M`j'"
    }
    esttab `models', `fmt' title("Table A4")

    display as text _newline "Table A5"
    use "`input'/table_a05.dta", clear
    estimates clear
    local models ""
    local j = 0
    foreach h in tornado hurricane flood quake tsunami storm heat {
        local ++j
        capture drop disaster_current disaster_lead
        gen disaster_current = cur_`h'
        gen disaster_lead = lead_`h'
        reghdfe sales_growth disaster_current disaster_lead, ///
            absorb(pair_id region_year) cluster(county_id)
        estadd local SCFE "YES"
        estadd local RYFE "YES"
        estimates store M`j'
        local models "`models' M`j'"
    }
    esttab `models', `fmt' compress title("Table A5") ///
        keep(disaster_current disaster_lead) order(disaster_current disaster_lead) ///
        coeflabels(disaster_current "Disaster" disaster_lead "One-year lead shock") ///
        mtitles("Tornado" "Hurricane" "Flooding" "Earthquake" "Tsunami" "Thunderstorm" "Heat") ///
        stats(SCFE RYFE N r2, labels("Supplier-Customer FE" "Region-Year FE" ///
            "Obs." "R-squared") fmt(%9s %9s %9.0fc %9.3f))

    foreach t in a06 a07 a08 {
        local number = upper(subinstr("`t'", "0", "", .))
        display as text _newline "Table `number'"
        use "`input'/table_`t'.dta", clear
        estimates clear
        local fe "pair_id year"
        if "`t'" == "a08" local fe "firm_id year"
        local models ""
        local j = 0
        foreach specification in large weighted restricted {
            local shock "shock_hurricane"
            local interaction "hurricane"
            local sample "trim_base"
            local weight ""
            if "`specification'" == "large" {
                local shock "large_hurricane"
                local interaction "large"
            }
            if "`specification'" == "weighted" {
                local sample "trim_base & sales_weight > 0 & sales_weight < ."
                local weight "[aw=sales_weight]"
            }
            if "`specification'" == "restricted" local sample "trim_base & year <= 2019"

            if "`t'" == "a06" {
                foreach model in baseline interaction {
                    local rhs "`shock'"
                    if "`model'" == "interaction" {
                        local rhs "`shock' concentration `interaction'_concentration"
                    }
                    local ++j
                    reghdfe sales_growth `rhs' if `sample' `weight', ///
                        absorb(`fe') cluster(county_id)
                    estimates store M`j'
                    local models "`models' M`j'"
                }
            }
            if "`t'" == "a07" {
                foreach x in business geographic {
                    local ++j
                    reghdfe sales_growth `shock' `x'_div `interaction'_`x' ///
                        if `sample' `weight', absorb(`fe') cluster(county_id)
                    estimates store M`j'
                    local models "`models' M`j'"
                }
            }
            if "`t'" == "a08" {
                foreach x in clusters branches cluster_share {
                    local ++j
                    reghdfe sales_growth `shock' `x' `interaction'_`x' ///
                        if `sample' `weight', absorb(`fe') cluster(county_id)
                    estimates store M`j'
                    local models "`models' M`j'"
                }
            }
        }
        esttab `models', `fmt' title("Table `number'")
    }

    foreach f in 05 a05 {
        local number = upper(subinstr("`f'", "0", "", .))
        display as text _newline "Figure `number'"
        use "`input'/heatmap.dta", clear
        replace food_sector = "Production" if food_sector == "Agricultural Production"
        local coefficient "value"
        local significance "v4"
        if "`f'" == "a05" {
            local coefficient "rcoef"
            local significance "v8"
        }
        replace `coefficient' = . if `coefficient' == 0
        foreach panel in a b {
            local cuts "-0.7,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,0.7"
            local ticks "-0.7(0.35)0.7"
            local missing ""
            if "`panel'" == "b" {
                replace `coefficient' = . if `significance' == ""
                local missing "missing(color(white))"
            }
            if "`f'" == "a05" & "`panel'" == "b" {
                local cuts "-0.9,-0.6,-0.4,-0.2,-0.1,-0.05,0,0.05,0.1,0.2,0.4,0.6,0.9"
                local ticks "-0.9(0.45)0.9"
            }
            heatplot `coefficient' food_sector hazard, colors(hcl diverging, reverse) ///
                aspectratio(1) cuts(`cuts') ylabel(, nogrid labsize(small)) ///
                xlabel(, angle(45) nogrid labsize(small)) ytitle("") xtitle("") ///
                graphregion(color(white)) plotregion(color(white) margin(zero)) `missing' ///
                ramp(right labels(`ticks') format(%4.2f) length(30) ///
                    plotregion(color(white) margin(zero)) graphregion(color(white) margin(zero)))
            graph export "`output'/figure_`f'_`panel'.png", replace
        }
    }
}
local status = _rc
log close analysis
if `status' exit `status'
