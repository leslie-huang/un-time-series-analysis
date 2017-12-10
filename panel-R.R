# Leslie Huang

# Panelssssss

### Set up the workspace
rm(list=ls())
setwd("/Users/lesliehuang/un-analysis-reg/")

set.seed(1234)

libraries <- c("foreign", "utils", "dplyr", "plyr", "devtools", "quanteda", "stringi", "topicmodels", "ldatuning", "lda", "plm", "stargazer", "ggplot2", "tseries", "lmtest", "car", "data.table")
lapply(libraries, require, character.only=TRUE)

data <- read.csv("../un-jupyter-coding-merging/data_by_country_year_topic_all.csv", stringsAsFactors = TRUE)

voting_only <- read.csv("../un-jupyter-coding-merging/data_by_country_year_topic_voting_only.csv", stringsAsFactors = TRUE)

consensus_only <- read.csv("../un-jupyter-coding-merging/data_by_country_year_topic_consensus_only.csv", stringsAsFactors = TRUE)


# Indices for the plm models
ct_index <- c("country_topic", "session_num")
c_index <- c("country", "session_num")

# Data permutations

recode_data <- function(df) {
  
  # fix some session numbers that were incorrect
  df[(df$record_name == "A/55/PV.15") & (df$country == "Colombia"),]$session_num <- "59"
  df[(df$record_name == "A/49/PV.12") & (df$country == "Malaysia"),]$session_num <- "50"
  df[(df$record_name == "A/49/PV.12") & (df$country == "Malaysia"),]$session_num <- "50"
  df[(df$record_name == "A/50/PV.27") & (df$country == "Maldives"),]$session_num <- "51"
  df[(df$record_name == "A/50/PV.4") & (df$country == "Nepal"),]$session_num <- "59"
  df[(df$year == "2009") & (df$country == "Niger"),]$session_num <- "64"
  df[(df$year == "1999") & (df$country == "Niger"),]$session_num <- "54"
  
  df[(df$record_name == "A/65/PV.21") & (df$country == "Grenada"),]$session_num <- "66"
  df[(df$year == "2011") & (df$country == "Libya"),]$session_num <- "66"

  df[(df$record_name == "A/65/PV.11") & (df$country == "Oman"),]$session_num <- "64"
  
  
  df$log_gdp <- log(df$gdp)
  df$speech_pct <- df$speech_proportion * 100
  df$agenda_pct <- df$proportion_of_agenda * 100
  df$year <- as.factor(df$year)
  df$session_num <- as.factor(df$session_num)
  
  # let's try recoding "never" and "not serving" together
  df$sc_membership[df$sc_membership == "never"] <- "not_serving"
  df$sc_membership <- droplevels(df$sc_membership)
  
  # and don't include junk
  df <- dplyr::filter(df, topic != "junk")

  
  
  return(df)
}

data <- recode_data(data)
data_voting <- recode_data(voting_only)
data_consensus <- recode_data(consensus_only)

# Some important topics
special_topics <- c("AIDS", "syria", "demo_elec", "africa_sec", "human_rights", "climate_envir", "war_terrorism", "LLDCs", "island_nations", "mideast_peace", "drugs", "pko")
special_topics_labels <- as.data.frame(t(c("Disease", "Syria conflict", "Democracy", "Africa security and conflict", "Human rights", "Climate change, the environment, and natural resources", "War and terrorism", "Landlocked least developed states", "Small island developing states", "Middle East peace", "Transnational crime", "Peacekeeping operations")))
colnames(special_topics_labels) <- special_topics
topics <- unique(data$topic)

# make another copy
data_by_type <- as.data.table(data)
data_by_type_voting <- as.data.table(data_voting)
data_by_type_consensus <- as.data.table(data_consensus)

# filter to just speech_pct exceeding 2%
#data <- dplyr::filter(data, speech_pct > 2)

###########################################################################
###########################################################################
# Calc new measures within topic

# Calculate each country's speech pct difference from the median and mean for that year
for (topic_name in topics) {
  df_subset <- data[data$topic == topic_name, ] # subset to just one topic
  df_name <- paste("data", topic_name, sep = "_")  # name for later 
  
  # Calculate yearly means and medians
  df_subset <- dplyr::group_by(df_subset, session_num)
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
  
  df_subset <- unique(df_subset)
  
  assign(df_name, df_subset)
  
}

###########################################################################
###########################################################################
# save an img

#save.image("un_reg_base.RData")


######################################################################################
######################################################################################
# Now run all the models and output tables

# FE, untransformed speech_proportion

for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + factor(session_num)
  f2 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + factor(session_num)
  f3 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + factor(session_num)
  f4 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + log_gdp * speech_proportion + factor(session_num)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")


  fn <- paste(topic, "table.tex", sep = "_")

   # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", #special_topics_labels[topic][[1]],
                                                                                         topic,
                sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda",
            covariate.labels = c("Speech pct.", "Log GDP", "Temporary member", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"),
            notes.append = TRUE, notes = "Independent variable is untransformed percentage of speech about the given topic.",
            out = fn
            )
}

######################################################################################
######################################################################################

# FE, X = z-score
for (topic in topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + factor(session_num)
  f2 <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + sc_membership * speech_proportion_zscore + factor(session_num)
  f3 <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + sc_membership * speech_proportion_zscore + sc_membership * log_gdp + factor(session_num)
  f4 <- agenda_pct ~ speech_proportion_zscore + log_gdp + sc_membership + sc_membership * speech_proportion_zscore + sc_membership * log_gdp + log_gdp * speech_proportion_zscore + factor(session_num)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  fn <- paste(topic, "table.tex", sep = "_")
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about", 
              #special_topics_labels[topic][[1]], 
              topic,
              sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Z-score", "Log GDP", "Temporary member", "Speech Z-score * Perm. member", "Speech Z-score * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Z-score * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is z-score (calculated from topic-session mean and SD).",
            out = fn
            
            )
}

######################################################################################
######################################################################################
# FE, X = % above topic-year median
for (topic in topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + factor(session_num)
  f2 <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_median + factor(session_num)
  f3 <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_median + sc_membership * log_gdp + factor(session_num)
  f4 <- agenda_pct ~ speech_proportion_diff_from_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_median + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_median + factor(session_num)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  fn <- paste(topic, "table.tex", sep = "_")
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about", #special_topics_labels[topic][[1]],
        topic,
        sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Median", "Log GDP", "Temporary member", "Speech Diff. from Median * Perm. member", "Speech Diff. from Median * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Median * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-session-topic observation from the topic-session median.",
            out = fn
            )
}

######################################################################################
######################################################################################
# FE, X = % above topic-year mean
for (topic in topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + factor(session_num)
  f2 <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_mean + factor(session_num)
  f3 <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_mean + sc_membership * log_gdp + factor(session_num)
  f4 <- agenda_pct ~ speech_proportion_diff_from_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_mean + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_mean + factor(session_num)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  fn <- paste(topic, "table.tex", sep = "_")
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about", 
     #special_topics_labels[topic][[1]], 
     topic,
     sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Mean", "Log GDP", "Temporary member", "Speech Diff. from Mean * Perm. member", "Speech Diff. from Mean * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Mean * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-year-topic observation from the topic-year mean.",
            out = fn
  )
}


######################################################################################
######################################################################################
######################################################################################
######################################################################################
# Run with X = % above country median
for (topic in topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + factor(session_num)
  f2 <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + factor(session_num)
  f3 <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + factor(session_num)
  f4 <- agenda_pct ~ speech_proportion_diff_from_country_median + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_country_median + factor(session_num)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  fn <- paste(topic, "table.tex", sep = "_")
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", 
          #special_topics_labels[topic][[1]], 
          topic,
          sep = " "), 
          single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Median", "Log GDP", "Temporary member", "Speech Diff. from Median * Perm. member", "Speech Diff. from Median * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Median * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-session-topic observation from the country-topic median.",
            out = fn
  )
  
}

######################################################################################
######################################################################################
# Run with X = % above country mean
for (topic in special_topics) {
  
  data_sub <- get(paste("data", topic, sep = "_"))
  # try with top 50 percentile
  #data_sub <- data_sub[data_sub$above_yearly_median == TRUE, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + factor(session_num)
  f2 <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + factor(session_num)
  f3 <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + factor(session_num)
  f4 <- agenda_pct ~ speech_proportion_diff_from_country_mean + log_gdp + sc_membership + sc_membership * speech_proportion_diff_from_country_median + sc_membership * log_gdp + log_gdp * speech_proportion_diff_from_country_median + factor(session_num)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, effect = "twoways")
  
  fn <- paste(topic, "table.tex", sep = "_")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", special_topics_labels[topic][[1]], sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", 
            covariate.labels = c("Speech Diff. from Mean", "Log GDP", "Temporary member", "Speech Diff. from Mean * Perm. member", "Speech Diff. from Mean * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Diff. from Mean * Log GDP"),
            notes.append = TRUE, notes = "Main independent variable is difference between speech percent for a given country-session-topic observation from the country-topic mean.",
            out = fn
  )
  
}

######################################################################################
######################################################################################
######################################################################################
######################################################################################
# Pooled OLS
for (topic in special_topics) {
  
  data_sub <- data[data$topic == topic, ]
  
  # go through the different specifications
  f <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + factor(session_num)
  f2 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + factor(session_num)
  f3 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + factor(session_num)
  f4 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + log_gdp * speech_proportion + factor(session_num)
  
  fe_model_f <- plm(formula = f, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  fe_model_f2 <- plm(formula = f2, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  fe_model_f3 <- plm(formula = f3, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  fe_model_f4 <- plm(formula = f4, data = data_sub, index = c_index, model = "pooling", effect = "twoways")
  
  
  # output the table for this topic
  stargazer(fe_model_f, fe_model_f2, fe_model_f3, fe_model_f4, digits = 3, title = paste("Effect of Speeches on Percent of Agenda about ", topic, sep = " "), single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", omit = "session_num", 
            covariate.labels = c("Speech pct.", "Log GDP", "Permanent member", "Temp. member", "Speech Pct. * Perm. member", "Speech Pct. * Temp. member", "Log GDP * Perm. member", "Log GDP * Temp. member", "Speech Pct. * Log GDP"),
            notes.append = TRUE, notes = "Independent variable is untransformed percentage of speech about the given topic."
  )
  
}

