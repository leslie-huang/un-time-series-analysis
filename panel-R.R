# Leslie Huang

# Panelssssss

### Set up the workspace
rm(list=ls())
setwd("/Users/lesliehuang/un-analysis-reg/")

set.seed(1234)

libraries <- c("foreign", "utils", "dplyr", "plyr", "devtools", "quanteda", "stringi", "topicmodels", "ldatuning", "lda", "plm", "stargazer", "ggplot2", "tseries", "lmtest", "car")
lapply(libraries, require, character.only=TRUE)

data <- read.csv("../un-jupyter-coding-merging/data_by_country_year_topic_voting_only.csv", stringsAsFactors = TRUE)

# filter to voting resolutions

data$log_gdp <- log(data$gdp)
data$speech_pct <- data$speech_proportion * 100
data$agenda_pct <- data$proportion_of_agenda * 100
data$country_topic <- with(data, paste(country, topic, sep = "_"))
data$country_topic <- as.factor(data$country_topic)
data$sc_membership <- as.factor(data$sc_membership)

# do first difference of variables
data$log_gdp_diff <- c(0, diff(data$log_gdp, lag = 1, differences = 1) )
data$speech_pct_diff <- c(0, diff(data$speech_pct, lag = 1, differences = 1) )
data$speech_proportion_diff <- c(0, diff(data$speech_proportion, lag = 1, differences = 1) )
data$agenda_pct_diff <- c(0, diff(data$agenda_pct, lag = 1, differences = 1) )

data$speech_pct_logit <- logit(data$speech_pct, percents = TRUE)
data$agenda_pct_logit <- logit(data$agenda_pct, percents = TRUE)
#######################################
# Function to run ALL the panel data tests for a given formula and data subset

all_panel_tests <- function(formula_spec, formula_spec_without_time_fe, data_subset, indices) {
  ### Set up all the equations for comparison:
  # OLS
  ols.reg <- lm(formula_spec, data = data_subset)
  # Pooling
  plm.pool <- plm(formula_spec, data = data_subset, index = indices, model = "pooling")
  # FE
  plm.reg <- plm(formula_spec, data = data_subset, index = indices, effect = "twoways")
  # RE 
  # RE cannot be solved with the specifications with interaction terms
  #re.reg <- plm(formula_spec, data = data_subset, index = c("country_topic", "year"), model = "random")
  # FE without time
  plm.reg.notime <- plm(formula_spec_without_time_fe, data = data_subset, index = indices, effect = "twoways")
  
  # Check that FE is better than OLS
  print("Check that FE is better than OLS using F test using pFtest:")
  print(pFtest(plm.reg, ols.reg))
  print("and pooled OLS using plmtest Lagrange Multiplier Test: ")
  print(plmtest(plm.pool, type="bp")))
  print("-----------------------------------------")
  
  # Decide between FE and RE
  print("Decide between FE and RE using Hausman test: ")
  #print(phtest(plm.reg, re.reg))
  print("-----------------------------------------")
  
  # FE constants for each unit
  #print("FE constants for each unit:")
  #print(fixef(plm.reg))
  print("-----------------------------------------")
  
  # FE for time
  print("Test that time FE are needed w F test with pFtest: ")
  print(pFtest(plm.reg.notime, plm.reg))
  
  print("and with Lagrange Multiplier test: ")
  print(plmtest(plm.reg, effect = "time", type="bp")) )
  
  print("-----------------------------------------")
  
  # Testing for serial correlation with the Breusch-Godfrey/Wooldridge test
  print("Testing for serial correlation with the Breusch-Godfrey/Wooldridge test, null is no serial correlation: ")
  print(pbgtest(plm.reg))
  print("-----------------------------------------")
  
  # Breusch-Pagan test for heteroskedasticity
  print("Breusch-Pagan test for heteroskedasticity, null is homoskedasticity: ")
  print(bptest(formula_spec, data = data_subset, studentize = F))
  print("-----------------------------------------")
  
  # Testing for stationarity with augmented Dickey-Fuller Test
  print("Testing for stationarity with augmented Dickey-Fuller Test, null is series has a unit root & is non stationary: ")
  panels <- plm.data(data_subset, index = indices)
  print(adf.test(panels$agenda_pct, k = 2)) # hard coded
  print("-----------------------------------------")
  
  
  print("Summary of the FE panel model: -----")
  print(summary(plm.reg))
  
  print("Summary of the pooled OLS")
  print(summary(ols.reg))
  
}

# Formulas

f <- agenda_pct ~ speech_pct + log_gdp + temp_member + factor(year)
f_without_time_fe <- agenda_pct ~ speech_pct + log_gdp + temp_member

f2 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + factor(year)
f2_without_time_fe <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion

f3 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + factor(year)
f3_without_time_fe <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp

f4 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + log_gdp * speech_proportion + factor(year)

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


################################
# Run it with country-topic spec
ct_index <- c("country_topic","year")
all_panel_tests(f, f_without_time_fe, data, ct_index)

################################
# Topic-specific printout
c_index <- c("country", "year")

# with differencing
all_panel_tests(f3_diff, f3_without_time_fe_diff, data[data$topic == "climate_change", ], c_index)

# with logit transformation
all_panel_tests(f3_lgt, f3_without_time_fe_lgt, data[data$topic == "global_trade", ], c_index)

################################################################################################
################################################################################################
# Now run all the models for specific topics

special_topics <- c("climate_change", "development", "island_nations", "mideast_peace", "global_trade", "africa_sec")

for (topic in special_topics) {
  
  data_sub <- data[data$topic == topic, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + factor(year)
  f2 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + factor(year)
  f3 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + factor(year)
  f4 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + log_gdp * speech_proportion + factor(year)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")

  
   # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, dep.var.labels = "Percent of Agenda", covariate.labels = c("Speech pct.", "Log GDP", "Not serving on SC", "Speech Pct. * Not serving", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Not serving", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"))
  
}

################################################################################################
################################################################################################
# Pooled OLS
for (topic in special_topics) {
  
  data_sub <- data[data$topic == topic, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + factor(year)
  f2 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + factor(year)
  f3 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + factor(year)
  f4 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + log_gdp * speech_proportion + factor(year)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, dep.var.labels = "Percent of Agenda", omit = "year", covariate.labels = c("Speech pct.", "Log GDP", "Not serving on SC", "Speech Pct. * Not serving", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Not serving", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"))
  
}


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

################################################################################################
#### Scatter plots

topics <- unique(lapply(data$topic, as.character))

for (topic in topics) {
  
  plot_title <- paste("% of Agenda about ", topic, " as a Function of Speeches", sep = "")
  
  pdf(file = paste(topic, "_graph.pdf",  sep = ""), width = 11, height = 8)
  plot(agenda_pct ~ speech_pct, data = data[data$topic == topic, ], xlim = c(0,50), ylab = "Percent of agenda", xlab = "Percent of speech", main = plot_title)
  dev.off()
  
}