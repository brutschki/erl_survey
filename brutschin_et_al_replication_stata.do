***************************************************************************************************
*This part of code replicates the results from Figure 4, Panels A and B and some of SI results
***************************************************************************************************


*******************************************
*Prepare the variables for the analysis
*******************************************

cd "C:\Users\brutschin\OneDrive - IIASA\Aarhus IIASA collab\Survey\R Analysis"

svyset dCountry
xtset dCountry

label variable climate_belief "Climate Belief"
label variable religion "Religious"
label variable conservative "Conservative"
label variable education_level "Education"
label variable income_level "Income"
label variable science_belief "Science"
label variable trust_industry "Trust - Industry"
label variable trust_university "Trust - University"
label variable trust_nat_gov "Trust - Nat Gov"
label variable trust_int_gov "Trust - Int gov"
label variable trust_non_gov "Trust - NGOs"
label variable age_cat "Age categorical"
label variable familiarity "Familiarity"





encode income, generate(income_i)

sem (Trust -> trust_industry trust_university trust_nat_gov trust_int_gov trust_non_gov)
predict Trust, latent

sem (Nature -> nature_risks nature_naive nature_no_meddle nature_alone nature_downfall)
predict Nature, latent

sem (Environment -> environment_concern environment_friendly environment_behavior)
predict Environment, latent

sem (Climate -> climate_worried climate_harm)
predict Climate, latent

encode gender, generate(gender_i)
encode country, generate(country_i)

label variable Trust "Trust"
label variable Nature "Nature"
label variable Environment "Environment"
label variable Climate "Climate"
label variable gender_i "Female"
label variable climate_harm "Climate Harm"
label variable environment_concern  "Env Concern"
label variable nature_no_meddle  "Nat No Meddle"



*alternative (included now in the main paper)

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Afforestation and reforestation", fe vce(cluster country_i)
estimates store AF

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Afforestation and reforestation", fe vce(cluster country_i)
estimates store AF1

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern  if technology=="Soil carbon sequestration", fe vce(cluster country_i)
estimates store SCS

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern  if technology=="Soil carbon sequestration", fe vce(cluster country_i)
estimates store SCS1

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Marine biomass and blue carbon", fe vce(cluster country_i)
estimates store MBBC

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Marine biomass and blue carbon", fe vce(cluster country_i)
estimates store MBBC1

coefplot AF SCS MBBC, , drop(_cons) xline(0) xscale(range(-0.2 0.4))graphregion(color(white)) plotregion(margin(zero)) bgcol(white) title("CDRn") xlabel(-0.2(0.1)0.4) legend(rows(1))

*table for the appendix
esttab AF AF1 SCS SCS1 MBBC MBBC1 using cdr1_predictive.rtf, star(* 0.10 ** 0.05 *** 0.01) r2 se onecell 


*CDR2

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Biochar", fe vce(cluster country_i)
estimates store Biochar

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Biochar", fe vce(cluster country_i)
estimates store Biochar1

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Enhanced weathering", fe vce(cluster country_i)
estimates store EW

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Enhanced weathering", fe vce(cluster country_i)
estimates store EW1

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Bioenergy with carbon capture and storage", fe vce(cluster country_i)
estimates store BECCS

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Bioenergy with carbon capture and storage", fe vce(cluster country_i)
estimates store BECCS1

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Direct air capture with carbon storage", fe vce(cluster country_i)
estimates store DACCS

xtreg broader_deployment age_cat gender_i education_level income_level religion  familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Direct air capture with carbon storage", fe vce(cluster country_i)
estimates store DACCS1

coefplot Biochar EW BECCS DACCS, drop(_cons) xline(0) xscale(range(-0.2 0.4))graphregion(color(white)) plotregion(margin(zero)) bgcol(white) title("CDRe") xlabel(-0.2(0.1)0.4) legend(rows(1))

*table for the appendix
esttab Biochar Biochar1 EW EW1 BECCS BECCS1 DACCS DACCS1 using results_cdr2.rtf, star(* 0.10 ** 0.05 *** 0.01) r2 se onecell 


*SRM


xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Stratospheric aerosol injection", fe vce(cluster country_i)
estimates store SAI

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Stratospheric aerosol injection", fe vce(cluster country_i)
estimates store SAI1

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Marine cloud brightening", fe vce(cluster country_i)
estimates store MCB

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Marine cloud brightening", fe vce(cluster country_i)
estimates store MCB1


xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Space-based geoengineering", fe vce(cluster country_i)
estimates store SBG

xtreg broader_deployment age_cat gender_i education_level income_level religion familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Space-based geoengineering", fe vce(cluster country_i)
estimates store SBG1

coefplot SAI MCB SBG, drop(_cons) xline(0) xscale(range(-0.2 0.4))graphregion(color(white)) plotregion(margin(zero)) bgcol(white) title("SRM") xlabel(-0.2(0.1)0.4) legend(rows(1))

esttab SAI SAI1 MCB MCB1 SBG SBG1 using results_srm.rtf, star(* 0.10 ** 0.05 *** 0.01) r2 se onecell 




****************
*R2 table
****************


* Create a loop to perform regression for each technology type

levelsof technology, local(tech_list)

*age

* Create a loop to perform the regression for each technology type
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment age_cat if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}


*gender

* Create a loop to perform the regression for each technology type
gen Rsq_gender = .
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment gender_i if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
	replace Rsq_gender = e(r2) if technology == "`tech'"
}

*Education
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment education_level if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}

*Income
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment income_level if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}


*Religion
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment religion if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}

*Conservative
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment conservative if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}

*Familiarity
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment familiarity if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}

*Science
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment science if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}

*Nature
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment nature_no_meddle if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}

*Environment
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment environment_concern if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}


*Climate
foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment climate_harm  if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}


*Trust - Industry

foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment trust_industry if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}

*Trust - Nat Gov

foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment trust_nat_gov if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}



*ALL

foreach tech of local tech_list {
    display "Running regression for technology: " "`tech'"
    
    xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology == "`tech'", fe vce(cluster country_i)
    
    
    * Alternatively, you can display the R-squared value:
    display "R-squared for " "`tech'" ": " %9.4f e(r2)
}


*********************
*Appendix

xtreg broader_deployment age_cat gender_i education_level income_level religion conservative familiarity science_belief nature_no_meddle trust_industry trust_nat_gov climate_harm environment_concern if technology=="Stratospheric aerosol injection", fe vce(cluster country_i)
estimates store SAI

vif, uncentered


xtreg broader_deployment age_cat gender_i education_level religion conservative familiarity nature_no_meddle trust_nat_gov if technology=="Stratospheric aerosol injection", fe vce(cluster country_i)
estimates store SAI2

esttab SAI SAI2 using results_srm_vif.rtf, star(* 0.10 ** 0.05 *** 0.01) r2 se onecell 
