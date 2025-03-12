rm(list=ls())
library(tidyverse)
library(stringr)

# Reading in flow data
dat_ema_flow <- read_csv("flow_final.csv")

# Remove admin entries
dat_ema_flow = dat_ema_flow[!(dat_ema_flow$secret_user_id %in% c("9999-999", "[admin account] (2628b895-18fa-4cf3-a2b4-ec7253aabde4)",
                                                                "[admin account] (a97ac66e-d8da-4427-bfd5-7e4df4059f05)")),]

dat_ema_flow$missed = ifelse(is.na(dat_ema_flow$start_Time),1,0)

# Calculate total compliance (DOES NOT REMOVE POOR QUALITY DATA FIRST)
total_compliance = 1 - sum(dat_ema_flow$missed)/nrow(dat_ema_flow)

# Extract time of day and date
dat_ema_flow$tod = word(dat_ema_flow$activity_flow)
dat_ema_flow$date = as.Date(dat_ema_flow$schedule_Time)
dat_ema_flow$dow = weekdays(as.Date(dat_ema_flow$schedule_Time))

# Check subject-level compliance and number of assessments per person
dat_ema_compliance_subj = dat_ema_flow %>%
  group_by(secret_user_id) %>% 
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  ungroup()

dat_ema_compliance_subj_tod = dat_ema_flow %>%
  group_by(secret_user_id, activity_flow) %>% 
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  ungroup()

dat_ema_compliance_tod = dat_ema_flow %>%
  group_by(tod) %>%
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  ungroup()

dat_ema_compliance_dow = dat_ema_flow %>%
  group_by(dow) %>%
  summarise(pct_compliance=100-100*mean(missed), n=n()) %>%
  ungroup()

# Expected number of assessments for EMA: 60
# Expected number of assessments for saliva + EMA: 16
# So total expected number is supposed to be between 60 and 76 assessments
# Some of these participants have more than 76 assessments - WHY? One reason is changing assessment schedules
# Some participants have both male and female versions of Evening assessment scheduled

# Question: How do we deal with participants who had:
# 1. Errors in assigned assessments (e.g. two Morning Assessments, female Evening assessment despite being male)
# 2. Too many assessments
# 3. Otherwise poor-quality data?

### Example of plotting frequencies for a multiple-response categorical item
since_physical_activity = as.integer(unlist(str_split(dat_ema_flow$since_physical_activity,", ")))
hist(since_physical_activity,breaks=c(0,1,2,3,4))




