* Leslie Huang

* set up the workspace
clear all
set more off
cd "/Users/lesliehuang/un-analysis-reg/"
capture log close
log using un_log_voting.log, replace

set seed 1234

* import data
import delimited data_by_country_year_topic_voting_only

encode country, gen(country1)
encode topic, gen(topic1)

gen log_gdp = log(gdp)

gen speech_pct = 100 * speech_proportion
gen agenda_pct = 100 * proportion_of_agenda

encode sc_membership, gen(sc_membership1)

egen country_topic = group(country1 topic1)

xtset country_topic year

*Inspect panel
*xtsum

* perm_member does not vary over time so cannot include it

* Agenda_proportion = Speech_proportion + temp_member_dummy + log_gdp w/ year and country-topic fixed effects
xtreg agenda_pct speech_pct temp_member log_gdp i.year, fe
outreg2 using table_voting, tex replace title(Table 1: All resolutions) ctitle(1) paren se bdec(3) nocons

* Add interactions for Speech_proportion * temp_member
xtreg agenda_pct speech_pct temp_member log_gdp c.speech_pct#temp_member i.year, fe
outreg2 using table_voting, tex append ctitle(2) paren se bdec(3)

* Add another interaction for GDP * temp_member
xtreg agenda_pct speech_pct temp_member log_gdp c.speech_pct#temp_member c.log_gdp#temp_member i.year, fe
outreg2 using table_voting, tex append ctitle(3) paren se bdec(3)


********************** Tests

* Hausman test for FE vs RE
xtreg agenda_pct speech_pct temp_member log_gdp c.speech_pct#temp_member c.log_gdp#temp_member i.year, fe
estimates store fixed
xtreg agenda_pct speech_pct temp_member log_gdp c.speech_pct#temp_member c.log_gdp#temp_member i.year, re
estimates store random
hausman fixed random

* P < 0.005 so use FE

* Test that time fixed effects are needed
* run the reg
testparm

* Yep

* Test for heteroskedastic SEs w/ Modified Wald test
* run the reg
xttest3

* Yep
* With Robust SEs
xtreg agenda_pct speech_pct temp_member log_gdp c.speech_proportion#temp_member c.log_gdp#temp_member i.year, fe vce(cluster country_topic)

* Testing for cross-sectional dependence
* Can't use xttest2 too many variables
* run the reg
xtcsd, pesaran
xtcsd, friedman
xtcsd, frees


* Alternatively: Beta regression with robust se

zoib proportion_of_agenda speech_proportion temp_member log_gdp c.speech_proportion#temp_member c.log_gdp#temp_member, vce(robust)
