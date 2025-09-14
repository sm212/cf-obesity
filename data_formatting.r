library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

sheets = excel_sheets('data/521_Joanne_Barrett.xlsx')
annual_reviews = sheets[grepl('Annual', sheets)]

# Read in sheets and combine into one dataframe
dfs = vector('list', length(annual_reviews))
for (i in seq_along(annual_reviews)){
  ar = annual_reviews[[i]]
  year = as.integer(gsub('Annual review year ', '', ar))
  dfs[[i]] = read_excel('data/521_Joanne_Barrett.xlsx',
                        sheet = ar, col_types = 'text')
  dfs[[i]]$year = year
}

reviews = bind_rows(dfs)

# Combine the different columns into one
tmp = reviews |>
  mutate(bmi = ifelse(is.na(bmi_value), s01bmi, bmi_value),
         bmi = case_when(year %in% c(2010, 2011, 2012) ~ bmi_value_db,
                         year %in% c(2016, 2017) ~ mbmi,
                         T ~ bmi),
         bmi_percentile = ifelse(is.na(bmi_percentile), s01bmipercentile, bmi_percentile),
         diag_dt = ifelse(is.na(diag_dt), s03diagnosisdate, diag_dt),
         pancreatic_enzyme_suppl = ifelse(is.na(pancreatic_enzyme_suppl), s07pancreaticenzymesupplements, pancreatic_enzyme_suppl),
         pancreatic_suppl = case_when(pancreatic_enzyme_suppl %in% c('N', 'No') ~ 'No',
                                      pancreatic_enzyme_suppl %in% c('Y', 'Yes') ~ 'Yes',
                                      pancreatic_enzyme_suppl %in% c('NK', 'Unknown') ~ 'Unknown',
                                      T ~ pancreatic_enzyme_suppl),
         fev = ifelse(is.na(cli_fev1_pct), s03cliqtrfev1pctpredicted, cli_fev1_pct),
         diabetes = s06cmpscfrddiabdiagnosis,    # Only from 2016 onwards
         emp_status = s09employmentstatus,       # Only from 2016 onwards
         ht = ifelse(is.na(cli_qtr_ht), s01height, cli_qtr_ht),
         wt = ifelse(is.na(cli_qtr_wt), s01weight, cli_qtr_wt)) |>    
  select(regid_anon, death_dt_anon, patientdied, bmi, bmi_percentile, ht, wt,
         year, diag_dt, pancreatic_suppl, fev, diabetes, emp_status)

# Add in demographic data
demographics = read_excel('data/521_Joanne_Barrett.xlsx', 
                          sheet = 'demographic_data')

# Add in CTFR data. If multiple drugs in single year, take the drug
# with the longest duration:
cftr = read_excel('data/521_Joanne_Barrett.xlsx', sheet = 'CFTRm_history') |>
  mutate(start_date = lubridate::ymd(stdt),
         end_date = lubridate::ymd(enddt),
         start_year = lubridate::year(start_date),
         end_year = lubridate::year(end_date))

grid = expand_grid(regid_anon = unique(demographics$regid_anon),
                   year = 2007:2023) |>
  left_join(cftr |> select(-contains('dt')), 
            by = c('regid_anon' = 'regid_anon', 'year' = 'start_year')) |>
  mutate(start_date = ifelse(is.na(start_date), lubridate::ymd(paste0(year, '-01-01')), start_date),
         end_date = ifelse(is.na(end_date), lubridate::ymd(paste0(year, '-12-31')), end_date),
         datediff = end_date - start_date,
         drugname = ifelse(is.na(drugname), 'None', drugname)) 

lkp_cftr = grid |>
  group_by(regid_anon, year) |>
  filter(datediff == max(datediff)) |>
  group_by(regid_anon) |>
  fill(drugname, .direction = 'down') |>
  select(regid_anon, year, drugname) |>
  ungroup() |>
  arrange(regid_anon, year)

cross_sec = tmp |>
  left_join(demographics |>
              select(regid_anon, sex = s01sex, ethn = s01ethnicity,
                     birth_dt_anon = birthdate_anon, 
                     mut1 = legname_mut1, mut2 = legname_mut2)) |>
  mutate(birth_year = lubridate::year(birth_dt_anon),
         age = year - birth_year,
         ageg = ifelse(age < 18, '0-17', '18+'),
         bmi = as.double(bmi),
         bmi_percentile = as.double(bmi_percentile),
         bmi_grp = case_when(ageg == '0-17' & bmi_percentile < 5 ~ 'Underweight',
                             ageg == '0-17' & bmi_percentile < 85 ~ 'Healthy weight',
                             ageg == '0-17' & bmi_percentile < 95 ~ 'Overweight',
                             ageg == '0-17' & bmi_percentile >= 95 ~ 'Obese',
                             ageg == '18+' & bmi < 18.5 ~ 'Underweight',
                             ageg == '18+' & bmi < 25 ~ 'Healthy weight',
                             ageg == '18+' & bmi < 30 ~ 'Overweight',
                             ageg == '18+' & bmi >= 30 ~ 'Obese')) |>
  left_join(lkp_cftr)

readr::write_csv(cross_sec, 'data/cross_sec.csv')

table(cross_sec$sex)
table(substr(cross_sec$ethn, 1,1))
sort(table(cross_sec$mut1))
hist(cross_sec$age)
