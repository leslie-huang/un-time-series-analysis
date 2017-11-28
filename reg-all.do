* Leslie Huang

* set up the workspace
clear all
set more off
cd "/Users/lesliehuang/un-analysis-reg/"
capture log close
log using un_log.log, replace

set seed 1234

* import data
import delimited data_by_country_year_topic_all

encode country, gen(country1)
encode topic, gen(topic1)

gen log_gdp = log(gdp)

encode sc_membership, gen(sc_membership1)

egen country_topic = group(country1 topic1)

xtset country_topic year

xtsum

* perm_member does not vary over time so cannot include it

* Agenda_proportion = Speech_proportion + temp_member_dummy + log_gdp w/ year and country-topic fixed effects
xtreg proportion_of_agenda speech_proportion temp_member log_gdp i.year, fe
outreg2 using table1, tex replace title(Table 1: All resolutions) ctitle(1) paren se bdec(3) nocons

* Add interactions for Speech_proportion * temp_member
xtreg proportion_of_agenda speech_proportion temp_member log_gdp c.speech_proportion#temp_member i.year, fe
outreg2 using table1, tex append ctitle(2) paren se bdec(3)

* Add another interaction for GDP * temp_member
xtreg proportion_of_agenda speech_proportion temp_member log_gdp c.speech_proportion#temp_member c.log_gdp#temp_member i.year, fe
outreg2 using table1, tex append ctitle(3) paren se bdec(3)
