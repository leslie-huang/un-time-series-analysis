# Leslie Huang

# Panelssssss

### Set up the workspace
rm(list=ls())
setwd("/Users/lesliehuang/un-analysis-reg/")

set.seed(1234)

libraries <- c("foreign", "utils", "dplyr", "plyr", "devtools", "quanteda", "stringi", "topicmodels", "ldatuning", "lda", "plm", "stargazer", "ggplot2", "tseries", "lmtest", "car", "data.table")
lapply(libraries, require, character.only=TRUE)

data <- read.csv("../un-jupyter-coding-merging/data_by_country_year_topic_all.csv", stringsAsFactors = TRUE)

# Indices for the plm models
ct_index <- c("country_topic","year")
c_index <- c("country", "year")

# Some important topics
special_topics <- c("demo_elec", "intl_law_settlements", "africa_sec", "climate_envir", "war_terrorism", "development_econ_assistance_poverty", "LLDCs", "island_nations", "mideast_peace", "global_trade", "drugs", "afghan_security", "women_children", "arms_treaties", "food_crisis")
topics <- unique(data$topic)

# Data permutations
data$log_gdp <- log(data$gdp)
data$speech_pct <- data$speech_proportion * 100
data$agenda_pct <- data$proportion_of_agenda * 100
data$sc_membership <- as.factor(data$sc_membership)
data$year <- as.factor(data$year)

# let's try recoding "never" and "not serving" together
data$sc_membership[data$sc_membership == "never"] <- "not_serving"

# and don't include junk
data <- dplyr::filter(data, topic != "junk")

# make another copy
data_by_type <- as.data.table(data)

# filter to just speech_pct exceeding 2%
#data <- dplyr::filter(data, speech_pct > 2)

# filter to just non permanent members
#data <- dplyr::filter(data, sc_membership != "permanent")

###########################################################################
###########################################################################
# Calc new measures within topic

# Calculate each country's speech pct difference from the median and mean for that year
for (topic_name in special_topics) {
  df_subset <- data[data$topic == topic_name, ] # subset to just one topic
  df_name <- paste("data", topic_name, sep = "_")  # name for later 
  
  # Calculate yearly means and medians
  df_subset <- dplyr::group_by(df_subset, year)
  df_subset <- dplyr::mutate(df_subset, yearly_mean_speech_proportion = mean(speech_proportion))
  df_subset <- dplyr::mutate(df_subset, yearly_sd_speech_proportion = sd(speech_proportion))
  df_subset <- dplyr::mutate(df_subset, yearly_median_speech_proportion = median(speech_proportion))
  
  # calculate diff from that year's mean and median for each obs
  df_subset <- dplyr::mutate(df_subset, speech_proportion_diff_from_median = yearly_median_speech_proportion - speech_proportion)
  df_subset <- dplyr::mutate(df_subset, speech_proportion_diff_from_mean = yearly_mean_speech_proportion - speech_proportion)
  # and z-score
  df_subset <- dplyr::mutate(df_subset, speech_proportion_zscore = (speech_proportion - yearly_mean_speech_proportion ) / yearly_sd_speech_proportion)
  
  # calculate diff from country-mean and country-median
  df_subset <- dplyr::ungroup(df_subset)
  df_subset <- dplyr::group_by(df_subset, country)
  df_subset <- dplyr::mutate(df_subset, country_mean_speech_proportion = mean(speech_proportion))
  df_subset <- dplyr::mutate(df_subset, country_sd_speech_proportion = sd(speech_proportion))
  df_subset <- dplyr::mutate(df_subset, country_median_speech_proportion = median(speech_proportion))
  
  df_subset <- dplyr::mutate(df_subset, speech_proportion_diff_from_country_median = country_median_speech_proportion - speech_proportion)
  df_subset <- dplyr::mutate(df_subset, speech_proportion_diff_from_country_mean = country_mean_speech_proportion - speech_proportion)
  # and z-score
  df_subset <- dplyr::mutate(df_subset, speech_proportion_country_zscore = (speech_proportion - country_mean_speech_proportion ) / country_sd_speech_proportion)
  
  
  # dummy for 50 percentile
  df_subset <- dplyr::mutate(df_subset, above_yearly_median = speech_proportion > yearly_median_speech_proportion)
  #dummy for above mean
  df_subset <- dplyr::mutate(df_subset, above_yearly_mean = speech_proportion > yearly_mean_speech_proportion)
  
  
  assign(df_name, df_subset)
  
}


###########################################################################
###########################################################################
# Group by level of sc_membership and average speech pct

data_by_type <- within(data_by_type, sc_membership <- relevel(sc_membership, ref = "not_serving"))

# where is the median?
aggregate_median <- median(data_by_type$speech_pct)

# drop bottom 50 percentile
data_by_type <- data_by_type[data_by_type$speech_pct >= aggregate_median, ]

data_leveled <- data_by_type[, .(mean(speech_pct), mean(agenda_pct), median(speech_pct)), by = .(topic, year, sc_membership)]
colnames(data_leveled) <- c("topic", "year", "sc_membership", "mean_speech_pct", "mean_agenda_pct", "median_speech_pct")

# These models work!!!!
grouped_lm <- lm(mean_agenda_pct ~ mean_speech_pct + sc_membership + mean_speech_pct * sc_membership, data = data_leveled)
grouped_fe <- plm(mean_agenda_pct ~ mean_speech_pct + sc_membership + mean_speech_pct * sc_membership, data = data_leveled, index = "year", model = "within")

stargazer(grouped_lm, grouped_fe, digits = 3, title = "Effect of Speech Pct. on Agenda Pct. by Security Council Membership Type", single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", column.labels = c("OLS", "Year Fixed Effects"), covariate.labels = c("Speech pct. (Membership Group Mean)", "Permanent SC member", "Temp. SC member", "Permanent member * Speech pct.", "Temp. member * Speech Pct"), notes.append = TRUE, notes = "Membership categorical variable reference level is 'not permanent or currently serving as temporary member.' ")

###########################################################################
###########################################################################
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
  print(plmtest(plm.pool, type="bp"))
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
  print(plmtest(plm.reg, effect = "time", type="bp")) 
  
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

f <- agenda_pct ~ speech_proportion + log_gdp + temp_member + factor(year)
f_without_time_fe <- agenda_pct ~ speech_pct + log_gdp + temp_member

f2 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + factor(year)
f2_without_time_fe <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion

f3 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + factor(year)
f3_without_time_fe <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp

f4 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + log_gdp * speech_proportion + factor(year)


################################
# Run it with country-topic spec

all_panel_tests(f, f_without_time_fe, data, ct_index)

################################
# Topic-specific printout

# with differencing
all_panel_tests(f3, f3_without_time_fe, data[data$topic == "climate_change", ], c_index)

######################################################################################
######################################################################################
# Now run all the models for specific topics and output tables

# FE, untransformed speech_proportion

for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
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
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech pct.", "Log GDP", "Temporary member", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"),
            notes.append = TRUE, notes = "Independent variable is untransformed percentage of speech about the given topic."
            )
}

######################################################################################
######################################################################################

# FE, X = z-score
for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + factor(year)
  f2 <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + sc_membership * speech_proportion_zscore + factor(year)
  f3 <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + sc_membership * speech_proportion_zscore + sc_membership * log_gdp + factor(year)
  f4 <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + sc_membership * speech_proportion_zscore + sc_membership * log_gdp + log_gdp * speech_proportion_zscore + factor(year)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Z-score", "Log GDP", "Temporary member", "Speech Z-score * Perm. member", "Speech Z-score * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Z-score * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is z-score (calculated from topic-year mean and SD)."
            )
}

######################################################################################
# FE, X = % above median
for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + factor(year)
  f2 <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_median + factor(year)
  f3 <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_median + sc_membership * log_gdp + factor(year)
  f4 <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_median + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_median + factor(year)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Median", "Log GDP", "Temporary member", "Speech Diff. from Median * Perm. member", "Speech Diff. from Median * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Median * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-year-topic observation from the topic-year median."
            )
}


######################################################################################
# FE, X = % above mean
for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + factor(year)
  f2 <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_mean + factor(year)
  f3 <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_mean + sc_membership * log_gdp + factor(year)
  f4 <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_mean + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_mean + factor(year)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Mean", "Log GDP", "Temporary member", "Speech Diff. from Mean * Perm. member", "Speech Diff. from Mean * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Mean * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-year-topic observation from the topic-year mean."
  )
}

######################################################################################
# Run with X = % above country median
for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + factor(year)
  f2 <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + factor(year)
  f3 <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + factor(year)
  f4 <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_country_median + factor(year)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Median", "Log GDP", "Temporary member", "Speech Diff. from Median * Perm. member", "Speech Diff. from Median * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Median * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-year-topic observation from the country-topic median."
  )
  
}


######################################################################################
# Run with X = % above country mean
for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + factor(year)
  f2 <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + factor(year)
  f3 <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + factor(year)
  f4 <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_country_median + factor(year)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Mean", "Log GDP", "Temporary member", "Speech Diff. from Mean * Perm. member", "Speech Diff. from Mean * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Mean * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-year-topic observation from the country-topic mean."
  )
  
}


######################################################################################
######################################################################################
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
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", omit = "year", 
            covariate.labels = c("Speech pct.", "Log GDP", "Permanent member", "Temp. member", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"),
            notes.append = TRUE, notes = "Independent variable is untransformed percentage of speech about the given topic."
  )
  
}


######################################################################################
#### Scatter plots of correlations

# Untransformed

for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", topic, " as a Function of Speech Pct", sep = "")
  
  pdf(file = paste("untransformed_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(agenda_pct ~ speech_proportion, data = get(data_name), ylab = "Percent of agenda", xlab = "Speech Pct", main = plot_title)
  dev.off()
  
}


# % diff from Mean
for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", topic, " as a Function of Difference from Yearly Mean of Speech Pct", sep = "")
  
  pdf(file = paste("diff_mean_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(agenda_pct ~ speech_proportion_diff_from_mean, data = get(data_name), ylab = "Percent of agenda", xlab = "Difference from Yearly Mean of Speech Pct", main = plot_title)
  dev.off()
  
}

# % diff from Median
for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", topic, " as a Function of Difference from Yearly Median of Speech Pct", sep = "")
  
  pdf(file = paste("diff_median_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(agenda_pct ~ speech_proportion_diff_from_median, data = get(data_name), ylab = "Percent of agenda", xlab = "Difference from Yearly Median of Speech Pct", main = plot_title)
  dev.off()
  
}

# Zscore
for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", topic, " as a Function of Z-score of Speech Pct", sep = "")
  
  pdf(file = paste("diff_zscore_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(agenda_pct ~ speech_proportion_zscore, data = get(data_name), ylab = "Percent of agenda", xlab = "Difference from Z-score of Speech Pct (calculated on yearly basis)", main = plot_title)
  dev.off()
  
}