# Leslie Huang

# Panelssssss

### Set up the workspace
rm(list=ls())
setwd("/Users/lesliehuang/un-analysis-reg/")

set.seed(1234)

libraries <- c("foreign", "utils", "dplyr", "plyr", "devtools", "quanteda", "stringi", "topicmodels", "ldatuning", "lda", "plm", "stargazer", "ggplot2", "tseries", "lmtest", "car", "data.table")
lapply(libraries, require, character.only=TRUE)

load("un_reg_base.RData")

######################################################################################
#### Scatter plots of correlations

# Untransformed

for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", special_topics_labels[topic][[1]], " as a Function of Speech %", sep = "")
  
  pdf(file = paste("untransformed_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(proportion_of_agenda ~ speech_proportion, data = get(data_name), ylab = "Pct of agenda", xlab = "Speech Pct", main = plot_title)
  dev.off()
  
}


# % diff from Mean
for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", special_topics_labels[topic][[1]], " as a Function of Difference from Yearly Mean of Speech %", sep = "")
  
  pdf(file = paste("diff_mean_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(proportion_of_agenda ~ speech_proportion_diff_from_mean, data = get(data_name), ylab = "Pct of agenda", xlab = "Difference from Yearly Mean of Speech Pct", main = plot_title)
  dev.off()
  
}

# % diff from Median
for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", special_topics_labels[topic][[1]], " as a Function of Difference from Yearly Median of Speech Pct", sep = "")
  
  pdf(file = paste("diff_median_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(proportion_of_agenda ~ speech_proportion_diff_from_median, data = get(data_name), ylab = "Pct of agenda", xlab = "Difference from Yearly Median of Speech Pct", main = plot_title)
  dev.off()
  
}

# Zscore
for (topic in special_topics) {
  
  plot_title <- paste("% of Agenda about ", special_topics_labels[topic][[1]], " as a Function of Z-score of Speech Pct", sep = "")
  
  pdf(file = paste("diff_zscore_", topic, ".pdf",  sep = ""), width = 11, height = 8)
  data_name <- paste("data", topic, sep = "_")
  plot(proportion_of_agenda ~ speech_proportion_zscore, data = get(data_name), ylab = "Pct of agenda", xlab = "Difference from Z-score of Speech Pct (calculated on yearly basis)", main = plot_title)
  dev.off()
  
}