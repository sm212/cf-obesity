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
         ht = as.double(ht),
         wt = as.double(wt),
         bmi = ifelse(is.na(bmi) & !is.na(ht) & !is.na(wt), wt / (ht / 100)^2, bmi),
         bmi_percentile = as.double(bmi_percentile)) |>
  left_join(lkp_cftr)

cross_sec = cross_sec |>
         mutate(bmi_grp = case_when(ageg == '0-17' & bmi_percentile < 5 ~ 'Underweight',
                             ageg == '0-17' & bmi_percentile < 85 ~ 'Healthy weight',
                             ageg == '0-17' & bmi_percentile < 95 ~ 'Overweight',
                             ageg == '0-17' & bmi_percentile >= 95 ~ 'Obese',
                             ageg == '18+' & bmi < 18.5 ~ 'Underweight',
                             ageg == '18+' & bmi < 25 ~ 'Healthy weight',
                             ageg == '18+' & bmi < 30 ~ 'Overweight',
                             ageg == '18+' & bmi >= 30 ~ 'Obese'))

readr::write_csv(cross_sec, 'data/cross_sec.csv')

# Add in missing BMI centiles for children (needed for weight categories)
#
# The BMI lookups don't have every possible value of age and BMI, so just
# get the row which is closest.
lkp_files = dir('data/dedupe_lkp/', pattern = '*.csv', full.names = T)

dfs = vector('list', length(lkp_files))
for (i in 1:length(lkp_files)){
  dfs[[i]] = readr::read_csv(lkp_files[i], show_col_types = F)
}

lkp_bmi = bind_rows(dfs)

get_bmi = function(main, lkp, match_col = bmi, id_col = regid_anon){
  # This looks a bit weird because it's vectorised and does some matrix
  # algebra. The gist is, for each row in main, find the row in lkp which has
  # the smallest euclidean distance (on age, sex, and match_col).
  main =  main |>
    filter(!is.na(sex), !is.na(age), !is.na({{match_col}})) |>
    mutate(sex_num = ifelse(sex == 'M', 1, 2)) 
  
  match_ids = select(main, {{id_col}})
  m_main = main |>
    select(sex_num, age, {{match_col}}) |>
    as.matrix()
  
  m_lkp = lkp |>
    mutate(sex_num = ifelse(sex == 'male', 1, 2)) |>
    select(sex_num, age = age_years, {{match_col}}) |>
    as.matrix()
  
  # Want to compute pairwise distance matrix D, D[i,j] = ||A[i,] - B[j,]||^2:
  # D[i,j] = (A[i,]-B[j,])(A[i,]-B[j,])^T [ok bc rows. A,B have same #columns]
  #        = A[i,]A[i,]^T + B[j,]B[j,]^T - 2A[i,]B[j,]^T 
  # The first bit is over all pairs, so use outer():
  dist = outer(rowSums(m_main^2), rowSums(m_lkp^2), '+') - 2 * m_main %*% t(m_lkp)
  nearest = max.col(-dist, ties.method = 'first')
  
  out = data.frame(regid_anon = match_ids,
                   centile = lkp$centile[nearest],
                   z_score = lkp$z_score[nearest])
  return(out)
}

toc = Sys.time()
a = get_bmi(cross_sec |> filter(age == 2, is.na(bmi_percentile)), 
            lkp_bmi |> filter(floor(age_years) == 2))
tic = Sys.time()
tic-toc
