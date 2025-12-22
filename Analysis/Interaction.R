#*******************************************************************************
# Fits data to logistic model for the association between BULLIED and MAKEFRIEND
# after accounting for confounders and interaction term (ADHD_SEV). Generates
# CI and OR for all combinations of ADHD_SEV and MFRIEND.
# [USED IN PAPER - p-val of interaction terms]
#*******************************************************************************

top_2018 <- read.csv("CleanedData(Num)/clean_2018", header = TRUE)
top_2019 <- read.csv("CleanedData(Num)/clean_2019", header = TRUE)
top_2020 <- read.csv("CleanedData(Num)/clean_2020", header = TRUE)
top_2021 <- read.csv("CleanedData(Num)/clean_2021", header = TRUE)
top_2022 <- read.csv("CleanedData(Num)/clean_2022", header = TRUE)
top_2023 <- read.csv("CleanedData(Num)/clean_2023", header = TRUE)

OR_table <- function(data_year, year) {
  data_year$SEX <- relevel(factor(data_year$SEX), ref = "0")
  data_year$INCOME <- relevel(factor(data_year$INCOME), ref = "0")
  data_year$BULLIED <- relevel(factor(data_year$BULLIED), ref = "0")
  data_year$MAKEFRIEND <- relevel(factor(data_year$MAKEFRIEND), ref = "0")
  data_year$ADHD_DIAGNOSIS <- relevel(factor(data_year$ADHD_DIAGNOSIS), ref = "1")
  data_year$ADHD_SEVERITY <- relevel(factor(data_year$ADHD_SEVERITY), ref = "0")
  data_year$ADHD_MEDICATION <- relevel(factor(data_year$ADHD_MEDICATION), ref = "0")
  data_year$ADHD_BT <- relevel(factor(data_year$ADHD_BT), ref = "0")

  glmodel <- glm(BULLIED ~ SEX + INCOME + ADHD_MEDICATION +
                   ADHD_BT + MAKEFRIEND * ADHD_SEVERITY,
                 family = binomial, data = data_year)

  glm_summary <- summary(glmodel)

  coefs <- glm_summary$coefficients[, 1]
  pvals <- glm_summary$coefficients[, 4]
  exp_coefs <- exp(coefs)

  ci <- confint.default(glmodel)
  eci <- exp(ci)

  table <- cbind(exp_coefs, eci, pvals)
  print(table)

  cat("=====================================================================\n")
}

OR_table(top_2018, "2018")
OR_table(top_2019, "2019")
OR_table(top_2020, "2020")
OR_table(top_2021, "2021")
OR_table(top_2022, "2022")
OR_table(top_2023, "2023")