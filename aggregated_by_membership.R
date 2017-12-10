# Leslie Huang

# Panelssssss

### Set up the workspace
rm(list=ls())
setwd("/Users/lesliehuang/un-analysis-reg/")

set.seed(1234)

libraries <- c("foreign", "utils", "dplyr", "plyr", "devtools", "quanteda", "stringi", "topicmodels", "ldatuning", "lda", "plm", "stargazer", "ggplot2", "tseries", "lmtest", "car", "data.table")
lapply(libraries, require, character.only=TRUE)

load("un_reg_base.RData")

###########################################################################
###########################################################################
# Group by level of sc_membership and average speech pct

data_by_type <- within(data_by_type, sc_membership <- relevel(sc_membership, ref = "not_serving"))
data_by_type_voting <- within(data_by_type_voting, sc_membership <- relevel(sc_membership, ref = "not_serving"))
data_by_type_consensus <- within(data_by_type_consensus, sc_membership <- relevel(sc_membership, ref = "not_serving"))

# where is the median?
# aggregate_median <- median(data_by_type$speech_proportion)
# aggregate_median_voting <- median(data_by_type_voting$speech_proportion)
# aggregate_median_consensus <- median(data_by_type_consensus$speech_proportion)

# drop bottom 50 percentile
#data_by_type <- data_by_type[data_by_type$speech_pct >= aggregate_median, ]

data_leveled <- data_by_type[, .(mean(speech_proportion), mean(proportion_of_agenda), median(speech_proportion)), by = .(topic, session_num, sc_membership)]
data_leveled_voting <- data_by_type_voting[, .(mean(speech_proportion), mean(proportion_of_agenda), median(speech_proportion)), by = .(topic, session_num, sc_membership)]
data_leveled_consensus <- data_by_type_consensus[, .(mean(speech_proportion), mean(proportion_of_agenda), median(speech_proportion)), by = .(topic, session_num, sc_membership)]

colnames(data_leveled) <- c("topic", "session_num", "sc_membership", "mean_speech_proportion", "mean_agenda_proportion", "median_speech_proportion")
colnames(data_leveled_consensus) <- c("topic", "session_num", "sc_membership", "mean_speech_proportion", "mean_agenda_proportion", "median_speech_proportion")
colnames(data_leveled_voting) <- c("topic", "session_num", "sc_membership", "mean_speech_proportion", "mean_agenda_proportion", "median_speech_proportion")


# OLS models for voting, consensus, all
grouped_lm <- lm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled)
grouped_voting <- lm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled_voting)
grouped_consensus <- lm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled_consensus)

# output table
stargazer(grouped_voting, grouped_consensus, grouped_lm, digits = 3, title = "Effect of Speech Pct. on Agenda Pct. by Security Council Membership Type", single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", column.labels = c("Voting", "Consensus", "All resolutions"),
          covariate.labels = c("Speech pct. (Membership Group Mean)", "Permanent SC member", "Temp. SC member", "Permanent member * Speech pct.", "Temp. member * Speech pct."), 
          notes.append = TRUE, notes = "Membership categorical variable reference level is 'not permanent or currently serving as temporary member.' "
)

# Fixed effects and pooled OLS

grouped_fe <- plm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled, index = "session_num", model = "within")
grouped_pooled <- plm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled, index = "session_num", model = "pooling")

grouped_fe_voting <- plm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled_voting, index = "session_num", model = "within")
grouped_pooled_voting <- plm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled_voting, index = "session_num", model = "pooling")

grouped_fe_consensus <- plm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled_consensus, index = "session_num", model = "within")
grouped_pooled_consensus <- plm(mean_agenda_proportion ~ mean_speech_proportion + sc_membership + mean_speech_proportion * sc_membership, data = data_leveled_consensus, index = "session_num", model = "pooling")

# output table comparing OLS/FE/Pooled for all resolutions

stargazer(grouped_lm, grouped_fe, grouped_pooled, digits = 3, title = "Effect of Speech Pct. on Agenda Pct. by Security Council Membership Type", single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", column.labels = c("OLS", "Year Fixed Effects", "Pooled OLS"),
          covariate.labels = c("Speech pct. (Membership Group Mean)", "Permanent SC member", "Temp. SC member", "Permanent member * Speech pct.", "Temp. member * Speech pct."), 
          notes.append = TRUE, notes = "Membership categorical variable reference level is 'not permanent or currently serving as temporary member.' "
)

# for voting res only


stargazer(grouped_voting, grouped_fe_voting, grouped_pooled_voting, digits = 3, title = "Effect of Speech Pct. on Agenda Pct. by Security Council Membership Type (Voting Resolutions)", single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", column.labels = c("OLS", "Year Fixed Effects", "Pooled OLS"),
          covariate.labels = c("Speech pct. (Membership Group Mean)", "Permanent SC member", "Temp. SC member", "Permanent member * Speech pct.", "Temp. member * Speech pct."), 
          notes.append = TRUE, notes = "Membership categorical variable reference level is 'not permanent or currently serving as temporary member.' "
)

# for consensus
stargazer(grouped_consensus, grouped_fe_consensus, grouped_pooled_consensus, digits = 3, title = "Effect of Speech Pct. on Agenda Pct. by Security Council Membership Type (Consensus Resolutions)", single.row = FALSE, omit.stat = c("f"), dep.var.labels = "Percent of Agenda", column.labels = c("OLS", "Year Fixed Effects", "Pooled OLS"),
          covariate.labels = c("Speech pct. (Membership Group Mean)", "Permanent SC member", "Temp. SC member", "Permanent member * Speech pct.", "Temp. member * Speech pct."), 
          notes.append = TRUE, notes = "Membership categorical variable reference level is 'not permanent or currently serving as temporary member.' "
)