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

f <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + factor(session_num)
f_without_time_fe <- agenda_pct ~ speech_pct + log_gdp + sc_membership

f2 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + factor(session_num)
f2_without_time_fe <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion

f3 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + factor(session_num)
f3_without_time_fe <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp

f4 <- agenda_pct ~ speech_proportion + log_gdp + sc_membership + sc_membership * speech_proportion + sc_membership * log_gdp + log_gdp * speech_proportion + factor(session_num)


################################
# Run it with country-topic spec

all_panel_tests(f, f_without_time_fe, data, ct_index)

################################
# Topic-specific printout

# with differencing
all_panel_tests(f3, f3_without_time_fe, data[data$topic == "climate_change", ], c_index)
