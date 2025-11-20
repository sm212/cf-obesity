library(dplyr)
library(ggplot2)

# No BMI percentile for 2016, 2017 (just weight & height percentiles)
# No 0-17s in 2023 reviews (max birth date is 15/12/2005)
# 58.4% of people are in the CTFR tab - missing means no CTFR?
# Lots of mutations in demographics - how to group?
# For rows missing BMI or BMI percentile - lookup tables to get the missing one?
cross_sec = readr::read_csv('data/cross_sec.csv') |>
  mutate(bmi_grp = factor(bmi_grp, levels = c('Underweight', 'Healthy weight',
                                              'Overweight', 'Obese')),
         mutation_type = case_when(mut1 & mut2 ~ 'Homozygous F508del',
                                   mut1 + mut2 == 1 ~ 'Heterozygous F508del',
                                   T ~ 'Other - no F508del'))

thm = function(){
  theme() %+replace% 
    theme(legend.position = 'bottom',
          plot.title.position = 'plot',
          text = element_text(size = 13),
          plot.title = element_text(size = 16, face = 'bold.italic'),
          plot.subtitle = element_text(size = 14, face = 'italic', 
                                       colour = 'grey60', hjust = 0.02),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'grey90'))
}

trend = function(df, count_vars, group_vars = NULL){
  if(is.null(group_vars)) group_vars = count_vars[1:length(count_vars)-1]
  
  # Confidence intervals using wilson score method
  out = df |>
    filter(!is.na(bmi_grp)) |>
   group_by(across(all_of(count_vars))) |>
  summarise(n = n()) |>
    ungroup() |>
  group_by(across(all_of(group_vars))) |>
  mutate(p = n / sum(n),
          sd = sqrt(p * (1 - p) / n),
          lo = (2 * n + 1.96^2 - 1.96 * sqrt(1.96^2 + 4 * n * (1 - p))) / (2 * (sum(n) + 1.96^2)),
          hi = (2 * n + 1.96^2 + 1.96 * sqrt(1.96^2 + 4 * n * (1 - p))) / (2 * (sum(n) + 1.96^2))) |>
    ungroup() 
  
  return(out)
}

plot_trend = function(df, facet = NULL, fname = NULL, ...){
  p = ggplot(df) +
    geom_ribbon(aes(x = year, ymin = lo, ymax = hi, fill = bmi_grp), alpha = 0.2) +
    geom_point(aes(x = year, y = p, fill = bmi_grp), pch = 21, colour = 'white', size = 4) +
    geom_line(aes(x = year, y = p, colour = bmi_grp)) +
    scale_x_continuous(breaks = seq(from = 2007, to = 2023, by = 2)) +
    scale_y_continuous(labels = scales::percent_format(0.1)) +
    labs(x = 'Year', y = 'Percent', title = 'BMI categories over time',
         subtitle = 'Reviews with a non-missing BMI or BMI percentile only',
         fill = NULL, colour = NULL) +
    thm()
  
  if(!is.null(facet)){
    p = p +
      facet_wrap(as.formula(paste0('~', facet)), ncol = 1)
  }
  
  if(!is.null(fname)){
    ggsave(plot = p, filename = paste0('img/', fname, '.png'), dpi = 300,
           units = 'in', ...)
  }
  
  return(p)
}

overall_bmi_grp = trend(cross_sec, c('year', 'bmi_grp'))
ageg_bmi_grp    = trend(cross_sec, c('year', 'ageg', 'bmi_grp')) 
cfrd_bmi_grp    = trend(cross_sec |>
                          mutate(cfrd_trt = ifelse(cfrd_trt, 'On CF related diabetes treatment', 'Not on CF related diabetes treatment')),
                        c('year', 'cfrd_trt', 'bmi_grp'))
pncr_bmi_grp    = trend(cross_sec |> 
                          filter(!is.na(pancreatic_enzyme_suppl)) |>
                          mutate(pancreatic_enzyme_suppl = ifelse(pancreatic_enzyme_suppl, 
                                                                  'Taking pancreatic enzyme supplements',
                                                                  'Not taking pancreatic enzyme supplements')), 
                        c('year', 'pancreatic_enzyme_suppl', 'bmi_grp')) 
f508_bmi_grp = trend(cross_sec, c('year', 'mutation_type', 'bmi_grp'))

plot_trend(overall_bmi_grp)
plot_trend(ageg_bmi_grp, 'ageg', 'grps_ageg', height = 6, width = 6)
plot_trend(cfrd_bmi_grp, 'cfrd_trt', 'cfrd', height = 6, width = 6)
plot_trend(pncr_bmi_grp, 'pancreatic_enzyme_suppl', 'pancreas', height = 6, width = 6)
plot_trend(f508_bmi_grp, 'mutation_type', 'mutataion', height = 6, width = 6)

trend_overall = cross_sec |>
  mutate(ageg = 'All ages') |>
  bind_rows(cross_sec) |>
  filter(!is.na(ageg)) |>
  mutate(ageg = factor(ageg, levels = c('All ages', '0-17', '18+'))) 

counts = trend_overall |>
  count(ageg, year) |>
  mutate(n = scales::label_number(accuracy = 1, big.mark = ',')(n))

ggplot(trend_overall) +
  geom_boxplot(aes(x = year, y = bmi, group = year), outliers = F) +
  geom_label(data = counts, mapping = aes(x = year, y = 3, label = n),
            size = 2.5, colour = 'grey70', label.size = NA) +
  facet_wrap(~ageg, ncol = 1) +
  thm() +
  labs(x = 'Year', y = 'BMI', title = 'BMI over time',
       subtitle = 'Only people with a non-missing age shown')
ggsave('img/trends.png', dpi = 300, height = 6, width = 7.17, units = 'in')

n_year = cross_sec |>
  select(regid_anon, year) |>
  distinct() |>
  complete(regid_anon, year) |> 
  left_join(cross_sec |>
              select(regid_anon, year) |>
              mutate(present = T)) |>
  mutate(present = ifelse(is.na(present), F, present)) |>
  group_by(regid_anon) |>
  mutate(rn = row_number(),
         rn = ifelse(present, rn, 100),
         type = case_when(present & rn == min(rn) ~ 'first appt',
                          present & lag(present) ~ 'stayed in',
                          !present & lag(present) ~ 'missed checkup',
                          present & !lag(present) ~ 'came back',
                          !present & !lag(present) ~ 'stayed out')) |> 
  ungroup() |>
  count(year, type)

n_year |>
  filter(!is.na(type)) |>
  pivot_wider(names_from = 'type', values_from = 'n') |>
  rowwise() |>
  mutate(total = sum(`first appt`, `stayed in`, `came back`, na.rm = T)) |>
  select(year, total, `first appt`, 
         `attended & attended previous appt` = `stayed in`,
         `attended & missed previous appt` = `came back`) |>
  readr::write_csv('data/attend_types.csv')
