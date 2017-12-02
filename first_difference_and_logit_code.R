
# do first difference of variables and logit transformation
# data$log_gdp_diff <- c(0, diff(data$log_gdp, lag = 1, differences = 1) )
# data$speech_pct_diff <- c(0, diff(data$speech_pct, lag = 1, differences = 1) )
# data$speech_proportion_diff <- c(0, diff(data$speech_proportion, lag = 1, differences = 1) )
# data$agenda_pct_diff <- c(0, diff(data$agenda_pct, lag = 1, differences = 1) )
# 
# data$speech_pct_logit <- logit(data$speech_pct, percents = TRUE)
# data$agenda_pct_logit <- logit(data$agenda_pct, percents = TRUE)



# Formulas with first difference

f_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp + temp_member + factor(year)
f_without_time_fe_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp + temp_member

f2_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff + factor(year)
f2_without_time_fe_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff

f3_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff + sc_membership * log_gdp_diff + factor(year)
f3_without_time_fe_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff + sc_membership * log_gdp_diff


f4_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff + sc_membership * log_gdp_diff + log_gdp_diff * speech_proportion_diff + factor(year)


# Formulas with logit X and Y

f_lgt <- agenda_pct_logit ~ speech_pct_logit + log_gdp + temp_member + factor(year)
f_without_time_fe_lgt <- agenda_pct_logit ~ speech_pct_logit + log_gdp + temp_member

f2_lgt <- agenda_pct_logit ~ speech_pct_logit  + log_gdp + sc_membership + sc_membership * speech_pct_logit + factor(year)
f2_without_time_fe_lgt <- agenda_pct_logit ~ speech_pct_logit + log_gdp + sc_membership + sc_membership * speech_pct_logit

f3_lgt <- agenda_pct_logit ~ speech_pct_logit  + log_gdp + sc_membership + sc_membership * speech_pct_logit + sc_membership * log_gdp + factor(year)
f3_without_time_fe_lgt <- agenda_pct_logit ~ speech_pct_logit  + log_gdp + sc_membership + sc_membership * speech_pct_logit + sc_membership * log_gdp

f4_lgt <- agenda_pct_logit ~ speech_pct_logit + log_gdp + sc_membership + sc_membership * speech_pct_logit + sc_membership * log_gdp + log_gdp * speech_pct_logit + factor(year)

# with differencing
all_panel_tests(f3_diff, f3_without_time_fe_diff, data[data$topic == "climate_change", ], c_index)

# with logit transformation
all_panel_tests(f3_lgt, f3_without_time_fe_lgt, data[data$topic == "global_trade", ], c_index)





################################################################################################
# FE with First difference

for (topic in special_topics) {
  
  data_sub <- data[data$topic == topic, ]
  
  # go through the different specifications
  
  f_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp + temp_member + factor(year)
  
  f2_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff + factor(year)
  f3_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff + sc_membership * log_gdp_diff + factor(year)
  
  f4_diff <- agenda_pct_diff ~ speech_proportion_diff + log_gdp_diff + sc_membership + sc_membership * speech_proportion_diff + sc_membership * log_gdp_diff + log_gdp_diff * speech_proportion_diff + factor(year)
  
  
  fe_model_f <- plm(formula = f_diff, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2_diff, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3_diff, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4_diff, data = data_sub, index = c_index, effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, dep.var.labels = "Percent of Agenda", covariate.labels = c("Speech pct.", "Log GDP", "Not serving on SC", "Speech Pct. * Not serving", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Not serving", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"))
  
}

################################################################################################
# FE with logit transformed main vars

for (topic in special_topics) {
  
  data_sub <- data[data$topic == topic, ]
  
  # go through the different specifications
  
  f_lgt <- agenda_pct_logit ~ speech_pct_logit + log_gdp + temp_member + factor(year)
  f2_lgt <- agenda_pct_logit ~ speech_pct_logit  + log_gdp + sc_membership + sc_membership * speech_pct_logit + factor(year)
  f3_lgt <- agenda_pct_logit ~ speech_pct_logit  + log_gdp + sc_membership + sc_membership * speech_pct_logit + sc_membership * log_gdp + factor(year)
  f4_lgt <- agenda_pct_logit ~ speech_pct_logit + log_gdp + sc_membership + sc_membership * speech_pct_logit + sc_membership * log_gdp + log_gdp * speech_pct_logit + factor(year)
  
  
  
  fe_model_f <- plm(formula = f_lgt, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2_lgt, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3_lgt, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4_lgt, data = data_sub, index = c_index, effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, dep.var.labels = "Percent of Agenda", covariate.labels = c("Speech pct.", "Log GDP", "Not serving on SC", "Speech Pct. * Not serving", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Not serving", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"))
  
}
