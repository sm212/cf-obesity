library(dplyr)
library(ggplot2)
library(tidyr)
library(splines)

df = readr::read_csv('data/cross_sec.csv')
transplant = readxl::read_excel('data/521A_Barrett_Output.xlsx', sheet = 'Transplants')

# Recreate figure 1 of Szentpetery et al
df_year = data.frame(year = 2007:2022,
                     year_grp = rep(c('2007-08', '2009-10', '2011-12', '2013-14',
                                      '2015-16', '2017-18', '2019-20', '2021-22'),
                                    each = 2))
df |>
  left_join(df_year) |>
  filter(!is.na(year_grp), !is.na(bmi_grp)) |>
  group_by(year, bmi_grp) |>
  summarise(n = n()) |>
  mutate(p = n / sum(n)) |>
  ungroup() |> 
  filter(bmi_grp != 'Healthy weight') |> 
  mutate(clr = case_when(bmi_grp == 'Obese' ~ '#a5a5a5',
                         bmi_grp == 'Overweight' ~ '#ffda72',
                         bmi_grp == 'Underweight' ~ '#98c3e5'),
         size = case_when(p < 0.03 ~ 1,
                          p < 0.06 ~ 2,
                          p < 0.09 ~ 3,
                          p < 0.12 ~ 4,
                          p < 0.15 ~ 5,
                          p < 0.18 ~ 6,
                          p < 0.21 ~ 7,
                          T ~ 8)) |>
  ggplot(aes(year, n, colour = bmi_grp)) +
  geom_point(aes(size = 1.5 * size), show.legend = T) +
  scale_colour_manual(values = c('Obese' = '#a5a5a5', 'Overweight' = '#ffda72', 'Underweight' = '#98c3e5')) +
  scale_size_identity() +
  labs(x = 'Year', y = NULL, title = 'Change in distribution of weight groups, 2007-2023',
       subtitle = 'UK CF registry data. All people aged 2+ with a valid BMI included')
ggsave('img/tmp.png', dpi = 300, height = 5, width = 5, units = 'in')


# Models
# Remove people after lung transplant + get rid of weird BMI / FEV1 values
lung_transplant = transplant |>
  filter(lungtx == 1)

df_mod = df |>
  left_join(lung_transplant) |>
  mutate(yearlungtx = ifelse(is.na(yearlungtx), 9999, yearlungtx)) |>
  filter(!is.na(bmi), !is.na(fev1), 
         bmi < 50, fev1 < 200, bmi > 10, year < yearlungtx)

# Overall trends:
m1_child = lm(fev1 ~ bmi, data = df_mod |> filter(ageg == '0-17'))
m1_adult = lm(fev1 ~ bmi, data = df_mod |> filter(ageg == '18+'))

m2_child = lm(fev1 ~ ns(bmi, df = 4), data = df_mod |> filter(ageg == '0-17'))
m2_adult = lm(fev1 ~ ns(bmi, df = 4), data = df_mod |> filter(ageg == '18+'))

df_plt = data.frame(bmi = seq(10, 50, by = 0.1))
df_plt$child1 = predict(m1_child, newdata = df_plt)
df_plt$adult1 = predict(m1_adult, newdata = df_plt)
df_plt$child2 = predict(m2_child, newdata = df_plt)
df_plt$adult2 = predict(m2_adult, newdata = df_plt)

df_plt |>
  pivot_longer(-bmi, names_to = 'tmp', values_to = 'fev1') |>
  mutate(ageg = gsub('\\d+', '', tmp),
         model = ifelse(grepl('1', tmp), 'Linear model', 'Spline')) |>
  select(-tmp) |>
  ggplot(aes(bmi, fev1)) +
  geom_point(data = df_mod |> slice_sample(prop = 0.1), alpha = 0.1) +
  geom_line(aes(colour = ageg), linewidth = 1.3) +
  facet_wrap(~model)

# Change in BMI on FEV1:
df_mod2 = expand_grid(regid_anon = unique(df_mod$regid_anon),
                      year = unique(df_mod$year)) |>
  left_join(df_mod |>
              select(regid_anon, year, sex, age, ageg, bmi, bmi_grp, fev1,
                     n_microbiol, pancreatic_enzyme_suppl)) |>
  # fill(sex:pancreatic_enzyme_suppl) |>
  group_by(regid_anon) |>
  arrange(year) |>
  mutate(chng_bmi = bmi - lag(bmi)) |>
  ungroup()

m_child = lm(fev1 ~ ns(bmi, df = 4) * chng_bmi + age + sex,
       data = df_mod2 |> filter(ageg == '0-17'))
m_adult = lm(fev1 ~ ns(bmi, df = 4) * chng_bmi + age + sex,
             data = df_mod2 |> filter(ageg == '18+'))

df_plt = expand_grid(bmi = seq(10, 50, by = 1),
                     chng_bmi = seq(-5, 5, by = 0.1),
                     age = seq(2, 90, by = 1),
                     sex = c('F', 'M')) |>
  left_join(df_mod |> select(age, bmi, bmi_grp))
df_plt$child = predict(m_child, newdata = df_plt)
df_plt$adult = predict(m_adult, newdata = df_plt)

df_plt |>
  group_by(bmi) |>
  summarise(child = mean(child, na.rm = T),
            adult = mean(adult)) |>
  ggplot(aes(x = bmi)) +
  geom_line(aes(y = child)) 


d_child = df_mod2 |>
  filter(ageg == '0-17')
d_child = d_child[complete.cases(d_child),]
d_child$pred = predict(m_child, newdata = d_child)

d_adult = df_mod2 |>
  filter(ageg == '18+')
d_adult = d_adult[complete.cases(d_adult),]
d_adult$pred = predict(m_adult, newdata = d_adult)

lines = bind_rows(d_child, d_adult) |>
  select(bmi, ageg, pred) |>
  mutate(bmi = round(bmi, 0)) |>
  group_by(bmi, ageg) |>
  summarise(pred = mean(pred)) |>
  ungroup()

ggplot() +
  geom_point(data = bind_rows(d_child, d_adult) |>
               slice_sample(prop = 0.1), alpha = 0.1,
             mapping = aes(x = bmi, y = fev1, colour = ageg)) +
  geom_line(data = lines |>
              mutate(pred = ifelse(ageg == '0-17' & bmi >= 35, NA, pred)), 
            linewidth = 1.3,
            mapping = aes(x = bmi, y = pred, colour = ageg))


# Version 2 - use lagged time-varying variables (IV days etc)
mean_bmi = data.frame(sex = c(rep(c('M', 'F'), each = 7)),
                      age_group = c(rep(c('16-24', '25-34', '35-44', '45-54',
                                          '55-64', '65-74', '75+'), times = 2)),
                      mean_bmi = c(23.8, 26.9, 28, 28.5, 29, 28.9, 27.5,
                                   24.9, 27.2, 27.9, 28.4, 28.8, 27.9, 27.5))

df_mod3 = df_mod2 |> 
  mutate(ageg2 = paste(5 * (age %/% 5), '-', 5 * (age %/% 5) + 4)) |>
  group_by(ageg2) |>
  mutate(bmi = bmi - mean(bmi, na.rm = T),
         n_microbiol = n_microbiol - mean(n_microbiol, na.rm = T)) |>
  group_by(regid_anon) |>
  mutate(bmi = lag(bmi), n_microbiol = lag(n_microbiol),
         pancreatic_enzyme_suppl = lag(pancreatic_enzyme_suppl))

m_child = lm(fev1 ~ ns(bmi, 4) * chng_bmi + age + sex + n_microbiol +
               pancreatic_enzyme_suppl, data = df_mod3 |> filter(ageg == '0-17'))
m_adult = lm(fev1 ~ ns(bmi, 4) * chng_bmi + age + sex + n_microbiol +
               pancreatic_enzyme_suppl, data = df_mod3 |> filter(ageg == '18+'))

# Get predictions for a range of scenarios
df_pred = expand_grid(age = seq(5, 75, by = 5),
                      sex = c('M', 'F'),
                      n_microbiol = c(-20, 0, 10),
                      pancreatic_enzyme_suppl = c(T, F),
                      bmi = seq(-20, 20, by = 0.1)) |>
  group_by(age, sex, n_microbiol, pancreatic_enzyme_suppl) |>
  mutate(chng_bmi = bmi - lag(bmi)) |>
  ungroup()

df_lab = df_pred |>
  select(age:pancreatic_enzyme_suppl) |>
  unique() |>
  mutate(rn = row_number())

df_pred = df_pred |>
  left_join(df_lab)

grp_size = nrow(df_pred) / nrow(df_lab)

pred = vector('double', length = nrow(df_pred))
pred_lo = vector('double', length = nrow(df_pred))
pred_hi = vector('double', length = nrow(df_pred))
row_idx = 0
for (i in 1:nrow(df_lab)){
  start_idx = row_idx + 1
  end_idx = row_idx + grp_size
  
  # Predict using the right age model
  age = df_pred$age[start_idx]
  if (age <= 17){
    p = predict(m_child, newdata = df_pred[start_idx:end_idx, ], se = T)
  }else{
    p = predict(m_adult, newdata = df_pred[start_idx:end_idx, ], se = T)
  }
  
  # Save - locations match the rows of df_pred
  pred[start_idx:end_idx] = p$fit
  pred_lo[start_idx:end_idx] = p$fit - 1.96 * p$se.fit
  pred_hi[start_idx:end_idx] = p$fit + 1.96 * p$se.fit
  
  row_idx = row_idx + grp_size
}

df_pred$pred = pred
df_pred$lo = pred_lo
df_pred$hi = pred_hi

df_pred2 = df_pred |>
  group_by(bmi, chng_bmi, age) |>
  summarise(pred = mean(pred),
            lo = mean(lo),
            hi = mean(hi)) |>
  ungroup()

df_lab2 = df_pred |>
  select(sex, age) |>
  distinct() |>
  mutate(rn = row_number())

df_pred2 |>
  mutate(ageg = ifelse(age <= 17, '0-17', '18+')) |>
  # left_join(df_lab2) |>
  # mutate(rn = as.factor(rn)) |>
  filter(pred >= 0) |>
  ggplot() +
  #geom_ribbon(aes(x = bmi, ymin = lo, ymax = hi, fill = as.factor(age)), alpha = 0.2) +
  geom_line(aes(x = bmi, y = pred, colour = as.factor(age))) +
  facet_wrap(~ageg)
