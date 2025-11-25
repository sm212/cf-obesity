library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(lubridate)

sheets = excel_sheets('data/521A_Barrett_Output.xlsx')
child_zscores = readr::read_csv('child_zscores.csv')
annual_reviews = sheets[grepl('AR_', sheets)]
cftr = read_excel('data/521A_Barrett_Output.xlsx', sheet = 'CFTRm') |>
  filter(is.na(drugsdrugnameother))

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
         review_dt = ifelse(is.na(visit_dt), s01encounterdate, visit_dt),
         review_dt = as.integer(review_dt),
         review_dt = as.Date(review_dt, origin = '1899-12-30'),
         hosp_iv_days = ifelse(is.na(hosp_iv_days), s02hospivoveralltotaldays, hosp_iv_days),
         home_iv_days = ifelse(is.na(home_iv_days), s02homeivoveralltotaldays, home_iv_days)) |>
  select(regid_anon, year, review_dt, bmi, bmi_percentile, height, weight,
         pancreatic_enzyme_suppl, cfrd_trt, suppl_feed, fev1, fvc, home_iv_days, hosp_iv_days)

# Add in demographic data, microbiology, and CFTR time
demographics = read_excel('data/521A_Barrett_Output.xlsx', 
                          sheet = 'Demographics')

microbiol = reviews |>
  select(regid_anon, year, contains('cult')) |> 
  pivot_longer(cult_normal_flora:s05culturespeciesxanthomonas, 
               names_to = 'cult', values_to = 'val') |>
  filter(!grepl('normal', cult)) |>
  mutate(val = ifelse(is.na(val), 0, 1)) |>
  group_by(regid_anon, year) |>
  summarise(n_microbiol = sum(val)) |>
  ungroup()

# TODO
# cftr_time = expand_grid(review_dt = seq.Date(as.Date('2007-01-01'), as.Date('2023-12-31')),
#                         drugname = unique(cftr$drugname)) |>
#   left_join()

cross_sec = tmp |>
  left_join(demographics |>
              mutate(mut1 = grepl('F508del', legacyname_mut1), 
                     mut2 = grepl('F508del', legacyname_mut2)) |>
              select(regid_anon, sex = s01sex, ethn = s01ethnicity,
                     birth_dt = dob_anon, death_dt = dod_anon, mut1, mut2)) |>
  left_join(microbiol) |>
  mutate(birth_dt = ymd(birth_dt),
         review_dt = ymd(review_dt),
         age = interval(birth_dt, review_dt) / years(1),
         bmi = as.double(bmi),
         height = as.double(height),
         weight = as.double(weight),
         bmi = ifelse(is.na(bmi) & !is.na(height) & !is.na(weight), weight / (height / 100)^2, bmi),
         bmi_percentile = as.double(bmi_percentile)) |>
  filter(age >= 2)

# Add in z-scores for children & missing BMI percentiles
child_z_scores = cross_sec |>
  filter(age >= 2, age < 18, !is.na(bmi)) |>
  select(regid_anon, birth_dt, review_dt, year, age, sex, bmi)
readr::write_csv(child_z_scores, 'data/child_z_scores.csv')

cross_sec = cross_sec |> 
  left_join(child_zscores |>
              select(regid_anon, year, z_score, bmi_pct = bmi_percentile)) |>
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
