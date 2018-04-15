library(tidyverse)
library(stringr)
library(readr)
library(splitstackshape)
library(sigmoid)
library(car)
library(ggpubr)

datafest <- read_csv("data/datafest2018.csv", n_max = 100000)
uscities <- read_csv("data/uscities.csv")

# SKETCH
# 1. Get distinct job postings
# 2. Separate industry vals
# 3. Aggregate by region
# 4. Compare to nat'l avg



### 1. Get distinct job postings in US, non null industry vals

# get distinct jobs
jobs_distinct_temp <- datafest %>% 
  select(-clicks,-localClicks,-date,-jobAgeDays) %>%
  distinct() 

# find duplicates
duplicates <- jobs_distinct_temp %>% 
  count(jobId) %>% 
  filter(n > 2) %>%
  pull(jobId)

# remove duplicates
jobs_distinct <- jobs_distinct_temp %>% 
  filter(!(jobId %in% duplicates))

# filter out missing values, filter to US
jobs_indust <- jobs_distinct %>% 
  filter(!is.na(industry), !is.na(city), country == "US")

#jobs counts by city before separation
jobs_count_city <- jobs_indust %>% 
  count(stateProvince, city) 

# change column name
colnames(jobs_count_city)[[3]] <- "total_jobs"



### 2. Separate out jobs by industry values, count jobs

# separate out multiple industry tags into multiple rows
jobs_indust <- jobs_indust %>% 
  cSplit(splitCols = "industry", sep = ",", direction = "long") %>% 
  as.data.frame()

# modify industry labels
jobs_indust <- jobs_indust %>%
  mutate(industry = case_when(
    industry == "HEALTH_CARE" ~ "Healthcare",
    industry == "RETAIL" ~ "Retail",
    industry == "AGRICULTURE_AND_EXTRACTION" ~ "Agriculture",
    industry == "ENERGY_AND_UTILITIES" ~ "Energy",
    industry == "CONSTRUCTION" ~ "Construction",
    industry == "BANKS_AND_FINANCIAL_SERVICES" ~ "Finance",
    industry == "EDUCATION_AND_SCHOOLS" ~ "Education",
    industry == "CONSULTING_AND_BUSINESS_SERVICES" ~ "Consulting",
    industry == "HUMAN_RESOURCES_AND_STAFFING" ~ "Human Resources",
    industry == "INSURANCE" ~ "Insurance",
    industry == "MEDIA_NEWS_AND_PUBLISHING" ~ "Media",
    industry == 'RESTAURANTS_TRAVEL_AND_LEISURE' ~ "Restaurants & Travel",
    industry == "INDUSTRIAL_MANUFACTURING" ~ "Manufacturing",
    industry == "FOOD_AND_BEVERAGES" ~ "Food & Beverages",
    industry == "AUTO" ~ "Auto",
    industry == "INTERNET_AND_SOFTWARE" ~ "Internet & Software",
    industry == "AEROSPACE_AND_DEFENSE" ~ "Aerospace & Defense",
    industry == "ORGANIZATION" ~ "Organization",
    industry == "COMPUTERS_AND_ELECTRONICS" ~ "Electronics",
    industry == "CONSUMER_GOODS_AND_SERVICES" ~ "Consumer Goods",
    industry == "TRANSPORT_AND_FREIGHT" ~ "Transport & Freight",
    industry == "GOVERNMENT" ~ "Government",
    industry == "TELECOMMUNICATIONS" ~ "Telecommunications",
    industry == "REAL_ESTATE" ~ "Real Estate",
    industry == "PHARMACEUTICALS" ~ "Pharmaceuticals"
  ))

# count jobs by city, industry, after separation
jobs_indust_count <- jobs_indust %>%
  group_by(stateProvince,city) %>% 
  count(industry) %>% 
  inner_join(jobs_count_city, by = c("stateProvince", "city"))



### 3. Aggregate by metropolitan areas

# csa: Combined Statistical Area --- largest statistical area defined by US Census
# msa: Metropolitan Statistical Area --- 2nd largest statistical area defined by US Census

csa <- read_csv("data/CSA.csv", col_names = c("id1", "csa", "id2", "msa"))
msa <- read_csv("data/MSA.csv", col_names = c("id1", "msa", "id2", "county"))

# aggregate city -> county -> MSA -> CSA; filtered by population_proper
csa_combined <- msa %>% 
  separate(county, into = c("county_name", "state_id"), sep = ", ") %>% 
  inner_join(csa, by = "msa") %>%
  select(csa, county_name, state_id, msa) %>% 
  inner_join(uscities, by = c("county_name", "state_id")) %>% 
  inner_join(jobs_indust_count, by = c("city", "state_id" = "stateProvince")) 

# extract city from csa name
csa_combined$csa_city <- csa_combined$csa %>% 
  str_replace(",.*","") %>% 
  str_replace("\\/.*","") %>% 
  str_replace("-.*","") %>% 
  str_trim()

# extract state from csa name
csa_combined$csa_state <- csa_combined$csa %>% 
  str_replace(".*,","")%>% 
  str_replace("\\/.*","") %>% 
  str_replace("-.*","") %>% 
  str_replace("\\(.*","") %>% 
  str_trim()

# shorten csa name
csa_combined$csa <- csa_combined$csa %>% 
  str_replace(",.*", "")

# fix SF mislabel
csa_combined <- csa_combined %>% 
  mutate(csa_city = case_when(csa_city == "San Jose" ~ "San Francisco",
                              csa_city != "San Jose" ~ csa_city))

# get csa jobs, population
csa_pop <- csa_combined %>% 
  select(-industry, -n) %>% 
  distinct() %>%
  group_by(csa) %>% 
  summarise(total_jobs = sum(total_jobs),
            pop = sum(population_proper))



### 4. Comparison to national averages

# total jobs
jobs <- csa_pop %>%
  summarise(n = sum(total_jobs)) %>%
  pull() 

# compute national industry job proportions
natl_avg <- csa_combined %>%
  group_by(industry) %>%
  summarise(n = sum(n)) %>%
  group_by(industry) %>%
  summarise(natl_prop = n/jobs) 

# calculate log ratio between observed and expected proportions of industry jobs
industry_diff_csa <- csa_combined %>% 
  group_by(csa, industry) %>%
  summarise(indust_jobs = sum(n)) %>%
  inner_join(csa_pop, by = "csa") %>%
  inner_join(natl_avg, by = "industry") %>% 
  mutate(expect_jobs = total_jobs*natl_prop) %>% 
  mutate(log_ratio = log(indust_jobs / expect_jobs)) %>%
  arrange(csa)

# get summary stats on log ratio distribution
log_ratio_stats <- industry_diff_csa %>%  
  ungroup() %>% 
  summarise(mean = mean(log_ratio), se = sd(log_ratio))

# normal approximation of log ratio distribution
industry_diff_csa <- industry_diff_csa %>% 
  mutate(log_ratio_adj = (log_ratio - log_ratio_stats[[1]]) / log_ratio_stats[[2]])

# write .csv file
write_csv(industry_diff_csa, "data/industry_diff_csa.csv")
