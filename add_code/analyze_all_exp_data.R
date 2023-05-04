# ------------------------------------------------------------------------------
# (C) Joachim Gassen 2023, gassen@wiwi.hu-berlin.de
# License: MIT. See LICENSE file for details.
#
# Some preliminary analyses over all four waves that this experiment has been 
# run at Humboldt-Universitöt zu Berlin
#
# For questions and remarks about the analysis, please reach out to the author.
# ------------------------------------------------------------------------------

library(tidyverse)
library(fixest)
library(lubridate)
library(DBI)

DATA_PATH <- "data_completed_exp"

data_files <- list.files(DATA_PATH)

read_data <- function(f) {
  con <- dbConnect(RSQLite::SQLite(), file.path(DATA_PATH, f))
  res <- dbSendQuery(con, "SELECT * FROM answers")
  df <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  df %>% 
    mutate(wave = str_remove(f, fixed("_croom_exp_done.sqlite3"))) %>%
    select(wave, everything())
}

exp_data <- bind_rows(
  lapply(data_files, read_data)
) %>% mutate(
  online = wave %in% c("kore_sose20", "kore_sose21")
)

exp_data %>%
  group_by(wave, full_cost) %>%
  summarise(
    nobs = n(),
    mean_price = mean(price),
    sd_price = sd(price),
    mean_time = mean(time),
    sd_time = sd(time)
  )


univariate_analsis <- function(df) {
  tt <- t.test(price ~ full_cost, data = df)
  rt <- wilcox.test(price ~ full_cost, data = df)
                    
  df$high_price <- df$price > 12
  df$high_price[df$price == 12] <- NA
  
  ct <- chisq.test(df$high_price[!is.na(df$high_price)], 
                   df$full_cost[!is.na(df$high_price)])
  print_df <- rbind(c(tt$statistic, tt$p.value),
                    c(rt$statistic, rt$p.value),
                    c(ct$statistic, ct$p.value))
  
  colnames(print_df) <- c("Statistik", "P Value (two-sided)")
  rownames(print_df) <- c("T-Test",
                          "Rank Wilcoxon Test",
                          "Chi-square Test (price <> 12 €)")
  
  knitr::kable(print_df, "markdown", digits = c(2, 4)) 
}

univariate_analsis(exp_data)
univariate_analsis(exp_data %>% filter(wave == "kore_sose19"))
univariate_analsis(exp_data %>% filter(wave == "kore_sose20"))
univariate_analsis(exp_data %>% filter(wave == "kore_sose21"))
univariate_analsis(exp_data %>% filter(wave == "kore_sose22"))
univariate_analsis(exp_data %>% filter(wave == "kore_sose23"))

feols(price ~ full_cost, data = exp_data)
feols(price ~ full_cost | wave, data = exp_data)
feols(price ~ full_cost*time | wave, data = exp_data)
feols(price ~ full_cost*wave, data = exp_data)
feols(price ~ full_cost*online | wave, data = exp_data)

