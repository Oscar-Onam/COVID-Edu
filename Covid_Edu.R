#### ---------------------------------- Install packages and load library ---------------------------------####  

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, wbstats, WDI, readr, plotly, ggplot2, reshape2, lubridate,
               acled.api, leaflet, cartography, sf, tmap, tmaptools, leaflet.opacity,
               leaflet.providers)

#### ----------------------------------- Read in data directly from the web ------------------------------#### 

dcov1 <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  mutate(year_cov1 = year(date), month_cov1 = month(date), week_cov1 = week(date)) %>%
  mutate(country_year_week_cov = paste(iso_code, year_cov1, week_cov1, sep = " ")) %>%
  filter(year_cov1 %in% c(2021, 2022, 2023)) %>%
  group_by(country_year_week_cov) %>%
  select (-iso_code) %>% 
  summarize(across(c(year_cov1, month_cov1, week_cov1, stringency_index, population_density,
                      median_age, gdp_per_capita,	extreme_poverty,	cardiovasc_death_rate, 
                     diabetes_prevalence,	female_smokers, male_smokers, handwashing_facilities,  
                     tests_units,life_expectancy,	human_development_index, population),  
                   last),
            across(c(total_cases, new_cases, new_cases_smoothed,total_deaths,	
                     new_deaths, new_deaths_smoothed, total_cases_per_million,	
                     new_cases_per_million, new_cases_smoothed_per_million, 
                     total_deaths_per_million,new_deaths_per_million, 
                     new_deaths_smoothed_per_million,	reproduction_rate,
                     icu_patients,icu_patients_per_million,	hosp_patients,	
                     hosp_patients_per_million,	weekly_icu_admissions, 	
                     weekly_icu_admissions_per_million,	weekly_hosp_admissions,	
                     weekly_hosp_admissions_per_million, total_tests,	
                     new_tests,	total_tests_per_thousand,	new_tests_per_thousand,	
                     new_tests_smoothed,	new_tests_smoothed_per_thousand,	
                     positive_rate,	tests_per_case,	total_vaccinations,	
                     people_vaccinated,	people_fully_vaccinated, total_boosters,
                     new_vaccinations,	new_vaccinations_smoothed,	
                     total_vaccinations_per_hundred, people_vaccinated_per_hundred,	
                     people_fully_vaccinated_per_hundred,	total_boosters_per_hundred,	
                     new_vaccinations_smoothed_per_million,	new_people_vaccinated_smoothed,	
                     new_people_vaccinated_smoothed_per_hundred, aged_65_older,	
                     aged_70_older, hospital_beds_per_thousand,excess_mortality_cumulative_absolute,  	
                     excess_mortality_cumulative,excess_mortality, 
                     excess_mortality_cumulative_per_million), 
                   sum)) 
  


# WHO global data set of public health and social measures applied during the COVID-19 pandemic

dcov2 <- read_csv(
  'https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV_PHM/CLEAN_PHSM?$format=csv',
  na = c("NA", "")) %>%
  
  # select, rename and create relevant vars
  select(c(COUNTRY_CODE, COUNTRY_TERRITORY_AREA, DATASET, WHO_CATEGORY, 
           WHO_SUBCATEGORY, WHO_MEASURE, ADMIN_LEVEL, AREA_COVERED,TARGETED, 
           DATE_START,DATE_END,ENFORCEMENT,NON_COMPLIANCE_PENALTY,
           FOLLOWING_MEASURE_NUMBER,PREV_MEASURE_NUMBER, MEASURE_STAGE)) %>% 
  rename(iso_code = COUNTRY_CODE, country = COUNTRY_TERRITORY_AREA, 
         admin_level = ADMIN_LEVEL, who_cate = WHO_CATEGORY,   
         who_subcate = WHO_SUBCATEGORY, who_measure = WHO_MEASURE, 
         target = TARGETED, start_date = DATE_START, end_date = DATE_END,
         enforce = ENFORCEMENT) %>% 
  rename_all(tolower) %>% 
  mutate(year_start_cov2 = year(start_date),
         month_start_cov2 = month(start_date),
         week_start_cov2 = as.numeric(week(start_date)),
         year_end_cov2 = year(end_date),
         month_end_cov2 = month(end_date),
         week_end_cov2 = as.numeric(week(end_date)),
         country_year_week_cov = paste(iso_code, 
                                        year_start_cov2, 
                                        week_start_cov2, 
                                        sep = " "),# create ID variable for merging with other dataset#
         cumm_week_start_cov2 =
           ifelse(year_start_cov2 ==2021, week_start_cov2+53,
           ifelse(year_start_cov2 ==2022, week_start_cov2+106,
           ifelse(year_start_cov2 ==2023, week_start_cov2+159,
           ifelse(is.na(week_start_cov2), 0,
           week_start_cov2
         )))),
         cumm_week_end_cov2 =
           ifelse(year_end_cov2 ==2021, week_end_cov2+53,
           ifelse(year_end_cov2 ==2022, week_end_cov2+106,
           ifelse(year_end_cov2 ==2023, week_end_cov2+159,
           ifelse(is.na(week_end_cov2), 0,
           week_end_cov2
         )))),
         # calculate duration of interventions in weeks
         duration_weeks_cov2 =cumm_week_end_cov2-cumm_week_start_cov2 
  )


# Merge cov1 and cov2 datasets

  dcov <- left_join(dcov2, dcov1, by = "country_year_week_cov") %>%
             select(country_year_week_cov, iso_code, country, who_cate, 
                    who_subcate, who_measure, everything())
  
  rm(list = c("dcov1", "dcov2"))
  
  
  
  # Get ACLED data
  
  dcrisis <- acled.api(
    email.address = "oonam@unicef.org",
    access.key = "1yBQttnWmTlDl63AmNyX",
    country = NULL,
    region = NULL,
    start.date = "2021-01-01",
    end.date = "2023-07-31",
    add.variables = c("latitude","longitude", "geo_precision", "time_precision")
  )
