library(dplyr)
library(ggplot2)
library(tidyr)
library(splines)

df = readr::read_csv('data/cross_sec.csv') 
transplant = readxl::read_excel('data/521A_Barrett_Output.xlsx', sheet = 'Transplants')
modulators = readxl::read_excel('data/521A_Barrett_Output.xlsx', sheet = 'CFTRm')

mod_status = df |>
  select(regid_anon, year, review_dt) |>
  left_join(modulators) |>
  mutate(review_dt = lubridate::ymd(review_dt),
         stdt = lubridate::ymd(stdt),
         enddt = lubridate::ymd(enddt),
         modulator = review_dt >= stdt, review_dt < enddt) |>
  group_by(regid_anon, year) |>
  summarise(modulator = max(modulator)) |>
  mutate(modulator = ifelse(is.na(modulator), 0, modulator),
         modulator = as.logical(modulator))

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
                         bmi_grp == 'Underweight' ~ '#98c3e5',
                         bmi_grp == 'Healthy weight' ~ 'coral'),
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
  scale_colour_manual(values = c('Obese' = '#a5a5a5', 'Overweight' = '#ffda72', 
                                 'Underweight' = '#98c3e5', 'Healthy weight' = 'coral')) +
  scale_size_identity() +
  ylim(0, 4500) +
  labs(x = 'Year', y = NULL, title = 'Change in distribution of weight groups, 2007-2022',
       subtitle = 'UK CF registry data. All people aged 2+ with a valid BMI included',
       caption = 'Notes: Circle size is based on # people in each group',
       colour = NULL) +
  theme(panel.background = element_rect(colour = 'transparent', fill = 'transparent'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/tmp.png', dpi = 300, width = 9.11, height = 4.12, units = 'in')

# Split out children and adult
df |>
  left_join(df_year) |>
  filter(!is.na(year_grp), !is.na(bmi_grp), ageg == '0-17') |>
  group_by(year, bmi_grp) |>
  summarise(n = n()) |>
  mutate(p = n / sum(n),
         bmi_grp = ifelse(bmi_grp == 'Healthy weight', 'Target range', bmi_grp)) |>
  ungroup() |> 
  filter(bmi_grp != 'Target range') |> 
  mutate(clr = case_when(bmi_grp == 'Obese' ~ '#a5a5a5',
                         bmi_grp == 'Overweight' ~ '#ffda72',
                         bmi_grp == 'Underweight' ~ '#98c3e5',
                         bmi_grp == 'Target range' ~ 'coral'),
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
  scale_colour_manual(values = c('Obese' = '#a5a5a5', 'Overweight' = '#ffda72', 
                                 'Underweight' = '#98c3e5', 'Healthy weight' = 'coral')) +
  scale_size_identity() +
  #ylim(0, 1000) +
  labs(x = 'Year', y = NULL, title = 'Change in distribution of weight groups in 15 year old children, 2007-2022',
       subtitle = 'UK CF registry data. All people aged 2+ with a valid BMI included',
       caption = 'Notes: Circle size is based on # people in each group',
       colour = NULL) +
  theme(panel.background = element_rect(colour = 'transparent', fill = 'transparent'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/trend-child.png', dpi = 300, width = 9.11, height = 4.12, units = 'in')

df |>
  left_join(df_year) |>
  filter(!is.na(year_grp), !is.na(bmi_grp), ageg == '18+') |>
  group_by(year, bmi_grp) |>
  summarise(n = n()) |>
  mutate(p = n / sum(n),
         bmi_grp = ifelse(bmi_grp == 'Healthy weight', 'Target range', bmi_grp)) |>
  ungroup() |> 
  #filter(bmi_grp != 'Target range') |>
  mutate(clr = case_when(bmi_grp == 'Obese' ~ '#a5a5a5',
                         bmi_grp == 'Overweight' ~ '#ffda72',
                         bmi_grp == 'Underweight' ~ '#98c3e5',
                         bmi_grp == 'Target range' ~ 'coral'),
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
  scale_colour_manual(values = c('Obese' = '#a5a5a5', 'Overweight' = '#ffda72', 
                                 'Underweight' = '#98c3e5', 'Target range' = 'coral')) +
  scale_size_identity() +
  ylim(0, 4000) +
  labs(x = 'Year', y = NULL, title = 'Change in distribution of weight groups in adults, 2007-2022',
       subtitle = 'UK CF registry data. All people aged 18+ with a valid BMI included',
       caption = 'Notes: Circle size is based on # people in each group',
       colour = NULL) +
  theme(panel.background = element_rect(colour = 'transparent', fill = 'transparent'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/trend-adult.png', dpi = 300, width = 9.11, height = 4.12, units = 'in')


# Models
# Remove people after lung transplant + get rid of weird BMI / FEV1 values.
# Update 26-03-19: Remove people aged < 6 with an FEV, as FEV measurements unreliable
lung_transplant = transplant |>
  filter(lungtx == 1)

df_mod = df |>
  left_join(lung_transplant) |>
  mutate(yearlungtx = ifelse(is.na(yearlungtx), 9999, yearlungtx),
         remove = age < 6 & !is.na(fev1)) |>
  filter(!is.na(bmi), !is.na(fev1), !remove,
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
              select(regid_anon, year, sex, age, ageg, bmi, z_score, bmi_grp, fev1,
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
  filter(ageg == '18+') |>
  select(-z_score) # Not available for adults
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
  mutate(ageg = ifelse(age <= 17, '0-17', '18+'),
         lab = case_when(age == 5 ~ '05-09',
                         age == 10 ~ '10-14',
                         age == 15 ~ '14-17',
                         age == 20 ~ '20-24',
                         age == 25 ~ '25-29',
                         age == 30 ~ '30-34',
                         age == 35 ~ '35-39',
                         age == 40 ~ '40-44',
                         age == 45 ~ '45-49',
                         age == 50 ~ '50-54',
                         age == 55 ~ '55-59',
                         age == 60 ~ '60-64',
                         age == 65 ~ '65-69',
                         age == 70 ~ '70-75',
                         age == 75 ~ '75-80')) |>
  # left_join(df_lab2) |>
  # mutate(rn = as.factor(rn)) |>
  filter(pred >= 0) |>
  ggplot() +
  geom_ribbon(aes(x = bmi, ymin = lo, ymax = hi, fill = as.factor(age)), alpha = 0.2) +
  geom_line(aes(x = bmi, y = pred, colour = lab)) +
  facet_wrap(~ageg) +
  labs(x = 'Centred BMI (0 is mean BMI for the age-group, higher values mean the patient is above mean BMI)',
       y = 'FEV1', colour = '5 year ageband',
       title = 'Increases in BMI are more beneficial for low-weight patients',
       subtitle = '') +
  theme(panel.background = element_rect(colour = 'black', fill = 'transparent'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/tmp-fev-bmi.png', dpi = 300)

# Lag time varying covariates
df_mod4 = df_mod2 |>
  group_by(regid_anon) |>
  mutate(lag_bmi = lag(bmi), n_microbiol = lag(n_microbiol), lag_fev = lag(fev1),
         pancreatic_enzyme_suppl = lag(pancreatic_enzyme_suppl))

m_child = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl, data = df_mod4 |> filter(ageg == '0-17'))
m_adult = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl, data = df_mod4 |> filter(ageg == '18+'))

# Predictions for typical patients:
fev_pred = function(df_args, other_args){
 df_pred = expand_grid(!!!df_args) |>
   mutate(!!!other_args)
 
 if ('init_bmi' %in% names(df_args)){
   df_pred$lag_bmi = df_pred$init_bmi + df_pred$chng_bmi
 }
 
 if (other_args$age < 18){
   pred = predict(m_child, df_pred, se = T)
 } else{
   pred = predict(m_adult, df_pred, se = T)
 }
 
 df_pred$pred = pred$fit
 df_pred$lower = pred$fit - 1.96 * pred$se.fit
 df_pred$upper = pred$fit + 1.96 * pred$se.fit
 
 return(df_pred)
}

all_preds = bind_rows(
  fev_pred(list(sex = c('M', 'F'), lag_bmi = seq(10, 30, by = 0.1)),
           list(age = 10, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, chng_bmi = 0, lag_fev = 70)) |>
    mutate(ageg = 'Child', vary = 'BMI'),
  fev_pred(list(sex = c('M', 'F'), init_bmi = seq(10, 30, by = 5), 
                chng_bmi = seq(0, 20, by = 0.1)),
           list(age = 10, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 70)) |>  
    mutate(ageg = 'Child', vary = 'Change in BMI'),
  fev_pred(list(sex = c('M', 'F'), lag_bmi = seq(10, 30, by = 0.1)),
           list(age = 40, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, chng_bmi = 0, lag_fev = 66)) |>
    mutate(ageg = 'Adult', vary = 'BMI'),
  fev_pred(list(sex = c('M', 'F'), init_bmi = seq(10, 30, by = 5), 
                chng_bmi = seq(0, 20, by = 0.1)),
           list(age = 40, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 66)) |>  
    mutate(ageg = 'Adult', vary = 'Change in BMI')
)

# Not very useful - probably better to just show an actual plot of the data...
all_preds |>
  filter(vary == 'BMI') |>
  ggplot(aes(fill = sex)) +
  geom_ribbon(aes(x = lag_bmi, ymin = lower, ymax = upper), alpha = 0.4) +
  geom_line(aes(x = lag_bmi, y = pred)) +
  facet_grid(ageg~sex) 

# Something like this
df_mod4 |>
  filter(ageg == '0-17', between(z_score, -3, 3)) |>
  ggplot(aes(z_score, fev1)) +
  geom_point(alpha = 0.03, size = 0.3) +
  geom_smooth(se = T) +
  ylim(0, 150) +
  labs(x = 'Z score', y = 'FEV1', 
       title = 'Association between current FEV & current BMI: Children') +
  theme(panel.background = element_blank(),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/assoc-fev-bmi-child.png', dpi = 300, height = 4.12, width = 6, units = 'in')

df_mod4 |>
  filter(ageg == '18+', between(bmi, 10, 40)) |>
  ggplot(aes(bmi, fev1)) +
  geom_point(alpha = 0.03, size = 0.3) +
  geom_smooth(se = T) +
  geom_vline(aes(xintercept = 25), lty = 2, colour = 'coral') +
  ylim(0, 150) +
  labs(x = 'BMI', y = 'FEV1', 
       title = 'Association between current FEV & current BMI: Adults') +
  theme(panel.background = element_blank(),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/assoc-fev-bmi-adult.png', dpi = 300, height = 4.12, width = 6, units = 'in')

all_preds |>
  filter(vary == 'Change in BMI', sex == 'F',
         !(ageg == 'Adult' & init_bmi == 10)) |>
  mutate(init_bmi = as.factor(init_bmi),
         ageg = factor(ageg, levels = c('Child', 'Adult'))) |> 
  ggplot(aes(fill = init_bmi, colour = init_bmi)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~ageg, nrow = 1) +
  ylim(-20, 30) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'Initial BMI',
       fill = 'Initial BMI', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi.png', dpi = 300, height = 5, width = 10, units = 'in')

  
# Split by on / off modulators
df_mod4 = df_mod4 |>
  left_join(mod_status)

m_child = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl + chng_bmi * modulator, 
             data = df_mod4 |> filter(ageg == '0-17'))
m_adult = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl + chng_bmi * modulator,
             data = df_mod4 |> filter(ageg == '18+'))

all_preds_mod = bind_rows(
  fev_pred(list(sex = c('M', 'F'), init_bmi = seq(10, 30, by = 5), 
                chng_bmi = seq(0, 20, by = 0.1), modulator = c(T, F)),
           list(age = 10, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 70)) |>  
    mutate(ageg = 'Child', vary = 'Change in BMI'),
  fev_pred(list(sex = c('M', 'F'), init_bmi = seq(10, 30, by = 5), 
                chng_bmi = seq(0, 20, by = 0.1), modulator = c(T, F)),
           list(age = 40, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 66)) |>  
    mutate(ageg = 'Adult', vary = 'Change in BMI')
)

all_preds_mod |>
  filter(vary == 'Change in BMI', sex == 'F',
         !(ageg == 'Adult' & init_bmi == 10)) |>
  mutate(init_bmi = as.factor(init_bmi),
         modulator = ifelse(modulator, 'On modulator', 'Off modulator'),
         ageg = factor(ageg, levels = c('Child', 'Adult'))) |> 
  ggplot(aes(fill = init_bmi, colour = init_bmi)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  facet_grid(ageg~modulator) +
  ylim(-20, 30) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'Modulator status',
       fill = 'Modulator status', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi-mod.png', dpi = 300, height = 5, width = 10, units = 'in')


# Update after December meeting - change chart to show each weight group
# Just get typical BMI values for each category. For children the BMI values
# correspond to the 4, 70, 85, and 97th centiles respectively
# Also remove anyone age < 6 from data as FEV is unreliable

df_mod4 = filter(df_mod4, age >= 6, year != 2017)

m_child = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl + chng_bmi, 
             data = df_mod4 |> filter(ageg == '0-17'))
m_adult = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl + chng_bmi,
             data = df_mod4 |> filter(ageg == '18+'))

df_plt = bind_rows(
  fev_pred(list(sex = c('M', 'F'), init_bmi = c(14.2, 18.5, 19.8, 23.1), 
                chng_bmi = seq(0, 20, by = 0.1)),
           list(age = 10, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 70)) |>  
    mutate(ageg = 'Child', vary = 'Change in BMI'),
  fev_pred(list(sex = c('M', 'F'), init_bmi = c(15, 23, 28, 33), 
                chng_bmi = seq(0, 20, by = 0.1)),
           list(age = 40, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 66)) |>  
    mutate(ageg = 'Adult', vary = 'Change in BMI')) |>
  filter(sex == 'F') |>
  mutate(weight_cat = case_when(init_bmi %in% c(14.2, 15) ~ 'Underweight',
                                init_bmi %in% c(18.5, 23) ~ 'Healthy weight',
                                init_bmi %in% c(19.8, 28) ~ 'Overweight',
                                init_bmi %in% c(23.1, 33) ~ 'Obese'),
         weight_cat = factor(weight_cat, levels = c('Underweight', 'Healthy weight',
                                                    'Overweight', 'Obese')),
         ageg = factor(ageg, levels = c('Child', 'Adult'))) 

df_plt |> 
  ggplot(aes(fill = weight_cat, colour = weight_cat)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~ageg, nrow = 1) +
  ylim(-20, 30) +
  xlim(0, 10) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'BMI category',
       fill = 'Initial BMI', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi-cat.png', dpi = 300, height = 5, width = 10, units = 'in')

df_plt |>
  filter(ageg == 'Child') |>
  ggplot(aes(fill = weight_cat, colour = weight_cat)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  ylim(-20, 30) +
  xlim(0, 10) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'BMI category',
       fill = 'BMI category', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi-cat-child.png', dpi = 300, height = 5, width = 10, units = 'in')

df_plt |>
  filter(ageg == 'Adult') |>
  ggplot(aes(fill = weight_cat, colour = weight_cat)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  ylim(-20, 30) +
  xlim(0, 10) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'BMI category',
       fill = 'BMI category', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi-cat-adult.png', dpi = 300, height = 5, width = 10, units = 'in')


m_child = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl + chng_bmi * modulator, 
             data = df_mod4 |> filter(ageg == '0-17'))
m_adult = lm(fev1 - lag_fev ~ ns(lag_bmi, 4) * chng_bmi + lag_fev + age + sex + n_microbiol +
               pancreatic_enzyme_suppl + chng_bmi * modulator,
             data = df_mod4 |> filter(ageg == '18+'))

all_preds_mod = bind_rows(
  fev_pred(list(sex = c('M', 'F'), init_bmi = c(14.2, 18.5, 19.8, 23.1), 
                chng_bmi = seq(0, 20, by = 0.1), modulator = c(T, F)),
           list(age = 10, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 70)) |>  
    mutate(ageg = 'Child', vary = 'Change in BMI'),
  fev_pred(list(sex = c('M', 'F'), init_bmi = c(15, 23, 28, 33), 
                chng_bmi = seq(0, 20, by = 0.1), modulator = c(T, F)),
           list(age = 40, n_microbiol = mean(df_mod2$n_microbiol, na.rm = T),
                pancreatic_enzyme_suppl = T, lag_fev = 66)) |>  
    mutate(ageg = 'Adult', vary = 'Change in BMI')
)

df_plt_mod = all_preds_mod |>
  filter(sex == 'F') |>
  mutate(weight_cat = case_when(init_bmi %in% c(14.2, 15) ~ 'Underweight',
                                init_bmi %in% c(18.5, 23) ~ 'Healthy weight',
                                init_bmi %in% c(19.8, 28) ~ 'Overweight',
                                init_bmi %in% c(23.1, 33) ~ 'Obese'),
         weight_cat = factor(weight_cat, levels = c('Underweight', 'Healthy weight',
                                                    'Overweight', 'Obese')),
         modulator = ifelse(modulator, 'On modulator', 'Off modulator'),
         ageg = factor(ageg, levels = c('Child', 'Adult'))) 

df_plt_mod |> 
  ggplot(aes(fill = weight_cat, colour = weight_cat)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  facet_grid(ageg~modulator) +
  ylim(-20, 30) +
  xlim(0, 10) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'BMI category',
       fill = 'BMI category', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi-mod-cat.png', dpi = 300, height = 5, width = 10, units = 'in')

df_plt_mod |> 
  filter(ageg == 'Child') |>
  ggplot(aes(fill = weight_cat, colour = weight_cat)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~modulator) +
  ylim(-20, 30) +
  xlim(0, 10) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'BMI category',
       fill = 'BMI category', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi-mod-cat-child.png', dpi = 300, height = 5, width = 10, units = 'in')

df_plt_mod |> 
  filter(ageg == 'Adult') |>
  ggplot(aes(fill = weight_cat, colour = weight_cat)) +
  geom_ribbon(aes(x = chng_bmi, ymin = lower, ymax = upper), alpha = 0.2, colour = 'transparent') +
  geom_line(aes(x = chng_bmi, y = pred), linewidth = 1.2) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~modulator) +
  ylim(-20, 30) +
  xlim(0, 10) +
  labs(x = 'Change in BMI', y = 'Change in FEV1', colour = 'BMI category',
       fill = 'BMI category', title = 'How does changing BMI affect FEV1') +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'black'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/chng-fev-chng-bmi-mod-cat-adult.png', dpi = 300, height = 5, width = 10, units = 'in')

# Table 1
tab1_p1 = df |>
  mutate(bmi = ifelse(bmi > 70, NA, bmi),
         z_score = ifelse(abs(z_score) > 4, NA, z_score)) |>
  group_by(regid_anon) |>
  arrange(year) |>
  slice(1) |>
  group_by(ageg) |>
  summarise(n = n(),
            age_lo = min(age, na.rm = T),
            age_hi = max(age, na.rm = T),
            med_bmi = median(bmi, na.rm = T),
            bmi_lo = min(bmi, na.rm = T),
            bmi_hi = max(bmi, na.rm = T),
            med_z = median(z_score, na.rm = T),
            z_lo = min(z_score, na.rm = T),
            z_hi = max(z_score, na.rm = T))

tab1_p2 = df |>
  group_by(regid_anon) |>
  arrange(year) |>
  slice(1) |>
  ungroup() |>
  filter(!is.na(bmi_grp)) |>
  count(ageg, bmi_grp) |>
  pivot_wider(names_from = 'bmi_grp', values_from = 'n')

tab1_p1 |>
  left_join(tab1_p2)

paed = df |>
  group_by(regid_anon) |>
  arrange(year) |>
  slice(1) |>
  filter(ageg == '0-17') |>
  pull(regid_anon)

df |>
  filter(regid_anon %in% paed) |>
  count(regid_anon) |>
  pull(n) |>
  median()

df |>
  filter(!regid_anon %in% paed) |>
  count(regid_anon) |>
  pull(n) |>
  median()

df |>
  filter(ageg == '18+') |>
  group_by(year, bmi_grp) |>
  summarise(n = n()) |>
  mutate(p = n / sum(n),
         over_ob = bmi_grp %in% c('Overweight', 'Obese')) |>
  summarise(p_over_ob = sum(p * over_ob))


child = df |>
  filter(ageg == '0-17') |>
  group_by(year) |>
  summarise(n = n(),
            n_valid_bmi = sum(!is.na(bmi_grp)),
            med = median(z_score, na.rm = T),
            q25 = quantile(z_score, .25, na.rm = T),
            q75 = quantile(z_score, .75, na.rm = T),
            n_under = sum(bmi_grp == 'Underweight', na.rm = T),
            n_healthy = sum(bmi_grp == 'Healthy weight', na.rm = T),
            n_over = sum(bmi_grp == 'Overweight', na.rm = T),
            n_obese = sum(bmi_grp == 'Obese', na.rm = T)) 

adult = df |>
  filter(ageg == '18+') |>
  group_by(year) |>
  summarise(n = n(),
            n_valid_bmi = sum(!is.na(bmi_grp)),
            med = median(bmi, na.rm = T),
            q25 = quantile(bmi, .25, na.rm = T),
            q75 = quantile(bmi, .75, na.rm = T),
            n_under = sum(bmi_grp == 'Underweight', na.rm = T),
            n_healthy = sum(bmi_grp == 'Healthy weight', na.rm = T),
            n_over = sum(bmi_grp == 'Overweight', na.rm = T),
            n_obese = sum(bmi_grp == 'Obese', na.rm = T))

bind_rows(child |> mutate(grp = 'Child'),
          adult |> mutate(grp = 'Adult')) |>
  readr::write_csv('numbers.csv')

# Extra bits for abstract

# 'Higher BMI z-scores are associated with better FEV on / off modulators'
a = df |>
  left_join(mod_status) |>
  filter(ageg == '0-17', age >= 6) |>
  mutate(z_grp = case_when(z_score < -1 ~ 'Low',
                           z_score > 1 ~ 'High',
                           T ~ 'Normal'),
         z_grp = factor(z_grp, levels = c('Normal', 'Low', 'High'))) |>
  filter(!is.na(z_grp), !is.na(fev1), !is.na(modulator))

summary(lm(fev1 ~ z_grp, data = a |> filter(modulator)))
summary(lm(fev1 ~ z_grp, data = a |> filter(!modulator)))
summary(lm(fev1 ~ z_grp * modulator, data = a))


# Table similar to Salvatore et al
df |>
  filter(ageg != '0-17', year == max(year), !is.na(bmi_grp)) |>
  left_join(mod_status) |>
  group_by(bmi_grp) |>
  summarise(sex_male = sum(sex == 'M'),
            sex_female = sum(sex == 'F'),
            fev_lt40 = sum(fev1 < 40 & age >= 6, na.rm = T),
            fev_4070 = sum(fev1 >= 40 & fev1 < 70 & age >= 6, na.rm = T),
            fev_7090 = sum(fev1 >= 70 & fev1 < 90 & age >= 6, na.rm = T),
            fev_90p = sum(fev1 >= 90 & age >= 6, na.rm = T),
            diabetes = sum(cfrd_trt),
            no_diabetes = sum(!cfrd_trt),
            pert = sum(pancreatic_enzyme_suppl, na.rm = T),
            no_pert = sum(!pancreatic_enzyme_suppl, na.rm = T),
            mod = sum(modulator),
            no_mod = sum(!modulator)) |>
  readr::write_csv('chris-tab.csv')


# Scatter of BMI over time in adults
t = df |>
  filter(ageg == '18+', between(bmi, 0, 60)) |>
  mutate(bmi_grp = ifelse(bmi_grp == 'Healthy weight', 'Target range', bmi_grp),
         bmi_grp = factor(bmi_grp, levels = c('Underweight', 'Target range',
                                              'Overweight', 'Obese')))

meds = t |>
  group_by(year) |>
  summarise(m = median(bmi),
            q25 = quantile(bmi, 0.25),
            q75 = quantile(bmi, 0.75))

ggplot() +
  geom_jitter(data = t, size = 1,
              mapping = aes(x = bmi, y = as.factor(year), colour = bmi_grp),
              width = 0, height = 0.3, alpha = 0.1) +
  geom_point(data = meds, mapping = aes(x = m, y = as.factor(year)),
             size = 3, colour = 'black') +
  coord_flip() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(y = NULL, x = 'BMI', colour = NULL, title = 'BMI distribution over time',
       subtitle = 'All adults age 18+ with a BMI measurement are shown.\nBlack dots are the median annual BMI') +
  theme(panel.background = element_rect(colour = 'transparent', fill = 'transparent'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        legend.position = 'bottom',
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/bmi-dist-adult.png', height = 6, width = 7, units = 'in')



# Scatter of BMI over time in children
t = df |>
  filter(ageg != '18+', between(bmi, 0, 60)) |>
  mutate(bmi_grp = ifelse(bmi_grp == 'Healthy weight', 'Target range', bmi_grp),
         bmi_grp = factor(bmi_grp, levels = c('Underweight', 'Target range',
                                              'Overweight', 'Obese')))

meds = t |>
  group_by(year) |>
  summarise(m = median(z_score),
            q25 = quantile(z_score, 0.25),
            q75 = quantile(z_score, 0.75))

ggplot() +
  geom_jitter(data = t |> filter(abs(z_score) < 3), size = 1,
              mapping = aes(x = z_score, y = as.factor(year), colour = bmi_grp),
              width = 0, height = 0.3, alpha = 0.3) +
  geom_point(data = meds, mapping = aes(x = m, y = as.factor(year)),
             size = 3, colour = 'black') +
  coord_flip() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(y = NULL, x = 'BMI', colour = NULL, title = 'BMI distribution over time',
       subtitle = 'All children age 2-17 with a BMI measurement are shown.\nBlack dots are the median annual BMI') +
  theme(panel.background = element_rect(colour = 'transparent', fill = 'transparent'),
        strip.background = element_rect(colour = 'black'),
        legend.key = element_blank(),
        legend.position = 'bottom',
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = 'grey90'),
        plot.title.position = 'plot',
        plot.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0.02,
                                     colour = 'grey50'))
ggsave('img/bmi-dist-child.png', height = 6, width = 7, units = 'in')


df |>
  left_join(mod_status) |>
  mutate(ageg2 = case_when(between(age, 18, 20) ~ '18-20',
                           between(age, 20, 29) ~ '20-29',
                           between(age, 30, 39) ~ '30-39',
                           between(age, 40, 49) ~ '40-49',
                           between(age, 50, 999) ~ '50+',
                           T ~ 'ignore')) |>
  filter(year %in% range(year), ageg2 != 'ignore', !is.na(bmi_grp)) |>
  group_by(bmi_grp, year) |>
  summarise(n = n(),
            n_1820 = sum(ageg2 == '18-20'),
            n_2029 = sum(ageg2 == '20-29'),
            n_3039 = sum(ageg2 == '30-39'),
            n_4049 = sum(ageg2 == '40-49'),
            n_50p = sum(ageg2 == '50+'),
            sex_male = sum(sex == 'M'),
            sex_female = sum(sex == 'F'),
            fev_lt40 = sum(fev1 < 40 & age >= 6, na.rm = T),
            fev_4070 = sum(fev1 >= 40 & fev1 < 70 & age >= 6, na.rm = T),
            fev_7090 = sum(fev1 >= 70 & fev1 < 90 & age >= 6, na.rm = T),
            fev_90p = sum(fev1 >= 90 & age >= 6, na.rm = T),
            diabetes = sum(cfrd_trt),
            no_diabetes = sum(!cfrd_trt),
            pert = sum(pancreatic_enzyme_suppl, na.rm = T),
            no_pert = sum(!pancreatic_enzyme_suppl, na.rm = T),
            mod = sum(modulator),
            no_mod = sum(!modulator)) |>
  readr::write_csv('tmp2.csv')

df |>
  group_by(regid_anon) |>
  arrange(year) |>
  mutate(year_diff = year - lag(year, 2)) |>
  filter(year_diff == 2) |>
  group_by(ageg, regid_anon) |>
  summarise(bmi_diff = bmi - lag(bmi)) |>
  filter(abs(bmi_diff) < 40) |>
  group_by(ageg) |>
  summarise(lo = min(bmi_diff, na.rm = T),
          med = median(bmi_diff, na.rm = T),
          q25 = quantile(bmi_diff, 0.25, na.rm = T),
          q75 = quantile(bmi_diff, 0.75, na.rm = T),
          hi = max(bmi_diff, na.rm = T))

df |>
  group_by(regid_anon) |>
  arrange(year) |>
  mutate(bmi_diff = bmi - lag(bmi),
         bmi_diff2 = bmi - lag(bmi, 2)) |>
  group_by(ageg) |>
  filter(abs(bmi_diff2) < 40) |>
  summarise(lo = min(bmi_diff2, na.rm = T),
            med = median(bmi_diff2, na.rm = T),
            q25 = quantile(bmi_diff2, 0.25, na.rm = T),
            q75 = quantile(bmi_diff2, 0.75, na.rm = T),
            hi = max(bmi_diff2, na.rm = T),
            n_5p = sum(bmi_diff2 >= 5),
            n = n())
