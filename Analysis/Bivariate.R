#*******************************************************************************
# Fits data to logistic model and generates model summary, ORs, ROC/AUC for
# bivariate association between BULLIED and MAKEFRIEND.
# [USED IN PAPER - TABLE 2]
#*******************************************************************************

top_2018 <- read.csv("CleanedData(Num)/clean_2018", header = TRUE)
top_2019 <- read.csv("CleanedData(Num)/clean_2019", header = TRUE)
top_2020 <- read.csv("CleanedData(Num)/clean_2020", header = TRUE)
top_2021 <- read.csv("CleanedData(Num)/clean_2021", header = TRUE)
top_2022 <- read.csv("CleanedData(Num)/clean_2022", header = TRUE)
top_2023 <- read.csv("CleanedData(Num)/clean_2023", header = TRUE)


fit_glm <- function(df, year = "Unknown") {
  # factorize categorical variables
  df$SEX <- relevel(factor(df$SEX), ref = "0")
  df$INCOME <- relevel(factor(df$INCOME), ref = "0")
  df$BULLIED <- relevel(factor(df$BULLIED), ref = "0")
  df$MAKEFRIEND <- relevel(factor(df$MAKEFRIEND), ref = "0")
  df$ADHD_DIAGNOSIS <- relevel(factor(df$ADHD_DIAGNOSIS), ref = "1")
  df$ADHD_SEVERITY <- relevel(factor(df$ADHD_SEVERITY), ref = "0")
  df$ADHD_MEDICATION <- relevel(factor(df$ADHD_MEDICATION), ref = "0")
  df$ADHD_BT <- relevel(factor(df$ADHD_BT), ref = "0")

  unique_names <- unique(df$BULLIED)
  print(unique_names)
  glmodel <- glm(BULLIED ~ MAKEFRIEND, family = binomial, data = df)

  # estimates in R summary
  confidence_int <- confint.default(glmodel, level = 0.95)

  # odds ratio = exp(coeff)
  odds_ratio <- exp(coef(glmodel))
  odds_ratio_ci <- exp(confidence_int)

  or_table <- cbind(odds_ratio, odds_ratio_ci)
  print(sprintf("%s", year))
  print(round(or_table, 2))

  # prediction <- predict(glmodel, factored_df, type = "response")

  # roc_object <- roc(factored_df$BULLIED, prediction)
  # auc_value <- auc(roc_object)

  # plot(roc_object, main = paste("ROC Curve", year))
  # print(auc_value)

  cat("===================================================\n\n")
}

fit_glm(top_2018, 2018)
fit_glm(top_2019, 2019)
fit_glm(top_2020, 2020)
fit_glm(top_2021, 2021)
fit_glm(top_2022, 2022)
fit_glm(top_2023, 2023)

#  References used for code:
#  1. Odds Ratios for Logsitic Reg: https://numiqo.com/tutorial/odds-ratio-logistic-regression
#  2. CI for OR: https://stats.stackexchange.com/questions/304833/how-to-calculate-odds-ratio-and-95-confidence-interval-for-logistic-regression#:~:text=%E2%80%A2%20exp(1.4345)%E2%89%884.20%20%E2%80%A2%20exp(1.4345+1.96%C3%970.5346)%E2%89%8811.97%20%E2%80%A2%20exp(1.4345%E2%88%921.96%C3%970.5346)%E2%89%881.472%20In,qnorm(c(0.025%2C0.5%2C0.975))%20*%20summary(m)$coefficients%5B%22DSH%22%2C2%5D)%20%5B1%5D%201.472098%204.197368%2011.967884
#  3. OR and OR_CI: https://www.statology.org/r-logistic-regression-odds-ratio/