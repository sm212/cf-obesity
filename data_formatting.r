library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)

sheets = excel_sheets('data/521A_Barrett_Output.xlsx')
missing_pct = readr::read_csv('missing_percentiles.csv')
annual_reviews = sheets[grepl('AR_', sheets)]

# Read in sheets and combine into one dataframe
dfs = vector('list', length(annual_reviews))
for (i in seq_along(annual_reviews)){
  ar = annual_reviews[[i]]
  year = as.integer(gsub('AR_', '', ar))
  dfs[[i]] = read_excel('data/521A_Barrett_Output.xlsx',
                        sheet = ar, col_types = 'text')
  dfs[[i]]$year = year
}

reviews = bind_rows(dfs)

# Combine the different columns into one
tmp = reviews |>
  mutate(bmi = case_when(year <= 2008 ~ bmi_value,
                         year <= 2014 ~ bmi_value_db,
                         T ~ s01bmi),
         bmi_percentile = bmi_percentile,
         height = ifelse(is.na(cli_qtr_ht), s01height, cli_qtr_ht),
         weight = ifelse(is.na(cli_qtr_wt), s01weight, cli_qtr_wt),
         pancreatic_enzyme_suppl = ifelse(is.na(pancreatic_enzyme_suppl), s07pancreaticenzymesupplements, pancreatic_enzyme_suppl),
         pancreatic_enzyme_suppl = case_when(grepl('Y', pancreatic_enzyme_suppl) ~ T,
                                             pancreatic_enzyme_suppl %in% c('N', 'No') ~ F,
                                             T ~ NA),
         cfrd_trt = ifelse(is.na(cfrd_treatment), s06cmpscfrdtreat, cfrd_treatment),
         cfrd_trt = grepl('y', cfrd_trt, ignore.case = T),
         suppl_feed = ifelse(is.na(suppl_feed_none_unk), s07supplementalfeedingnone, suppl_feed_none_unk),
         suppl_feed = !grepl('No|1', suppl_feed),
         fev1 = ifelse(is.na(cli_fev1_pct), s03cliqtrfev1pctpredicted, cli_fev1_pct),
         fvc = ifelse(is.na(cli_fvc_pct), s03cliqtrfvcpctpredicted, cli_fvc_pct),
         review_dt = ifelse(is.na(visit_dt), s01encounterdate, visit_dt)) |>
  select(regid_anon, year, review_dt, bmi, bmi_percentile, height, weight,
         pancreatic_enzyme_suppl, cfrd_trt, suppl_feed, fev1, fvc)

# Add in demographic data
demographics = read_excel('data/521A_Barrett_Output.xlsx', 
                          sheet = 'Demographics')

# Add in CTFR data. If multiple drugs in single year, take the drug
# with the longest duration:
# cftr = read_excel('data/521_Joanne_Barrett.xlsx', sheet = 'CFTRm_history') |>
#   mutate(start_date = lubridate::ymd(stdt),
#          end_date = lubridate::ymd(enddt),
#          start_year = lubridate::year(start_date),
#          end_year = lubridate::year(end_date))
# 
# grid = expand_grid(regid_anon = unique(demographics$regid_anon),
#                    year = 2007:2023) |>
#   left_join(cftr |> select(-contains('dt')), 
#             by = c('regid_anon' = 'regid_anon', 'year' = 'start_year')) |>
#   mutate(start_date = ifelse(is.na(start_date), lubridate::ymd(paste0(year, '-01-01')), start_date),
#          end_date = ifelse(is.na(end_date), lubridate::ymd(paste0(year, '-12-31')), end_date),
#          datediff = end_date - start_date,
#          drugname = ifelse(is.na(drugname), 'None', drugname)) 
# 
# lkp_cftr = grid |>
#   group_by(regid_anon, year) |>
#   filter(datediff == max(datediff)) |>
#   group_by(regid_anon) |>
#   fill(drugname, .direction = 'down') |>
#   select(regid_anon, year, drugname) |>
#   ungroup() |>
#   arrange(regid_anon, year)

cross_sec = tmp |>
  left_join(demographics |>
              mutate(mut1 = grepl('F508del', legacyname_mut1), 
                     mut2 = grepl('F508del', legacyname_mut2)) |>
              select(regid_anon, sex = s01sex, ethn = s01ethnicity,
                     birth_dt = dob_anon, death_dt = dod_anon, mut1, mut2)) |>
  mutate(birth_dt = ymd(birth_dt),
         review_dt = as.Date(as.integer(review_dt), origin = '1899-12-30'),
         review_dt = ymd(review_dt),
         age = interval(birth_dt, review_dt) / years(1),
         bmi = as.double(bmi),
         height = as.double(height),
         weight = as.double(weight),
         bmi = ifelse(is.na(bmi) & !is.na(height) & !is.na(weight), weight / (height / 100)^2, bmi),
         bmi_percentile = as.double(bmi_percentile)) |>
  filter(age > 0)

# Add in missing BMI centiles for children (needed for weight categories)
# Lookup done in python - code in repo.

missing_child_bmi = cross_sec |>
  filter(between(age, 2, 17), is.na(bmi_percentile), !is.na(bmi)) |>
  select(regid_anon, birth_dt, review_dt, year, age, sex, bmi) |>
  left_join(missing_pct |>
             select(regid_anon, year, bmi_pct = bmi_percentile))

cross_sec = cross_sec |> 
  left_join(missing_child_bmi |>
              select(regid_anon, year, bmi_pct)) |>
  mutate(bmi_percentile = ifelse(is.na(bmi_percentile), bmi_pct, bmi_percentile),
         ageg = ifelse(age < 18, '0-17', '18+'),
         bmi_grp = case_when(ageg == '0-17' & bmi_percentile < 5 ~ 'Underweight',
                             ageg == '0-17' & bmi_percentile < 85 ~ 'Healthy weight',
                             ageg == '0-17' & bmi_percentile < 95 ~ 'Overweight',
                             ageg == '0-17' & bmi_percentile >= 95 ~ 'Obese',
                             ageg == '18+' & bmi < 18.5 ~ 'Underweight',
                             ageg == '18+' & bmi < 25 ~ 'Healthy weight',
                             ageg == '18+' & bmi < 30 ~ 'Overweight',
                             ageg == '18+' & bmi >= 30 ~ 'Obese')) 

readr::write_csv(cross_sec, 'data/cross_sec.csv')
