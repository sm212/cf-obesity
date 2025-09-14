cross_sec |> 
  group_by(year) |> 
  mutate(miss_bmi_all = is.na(bmi) & is.na(bmi_percentile),
         bmi_no_pct = !is.na(bmi) & is.na(bmi_percentile),
         can_calc = miss_bmi_all & !is.na(ht) & !is.na(wt)) |>
  summarise(Number = n(),
            `Missing BMI` = sum(is.na(bmi)),
            `Missing BMI percentile` = sum(is.na(bmi_percentile)),
            `Missing both BMI & percentile` = sum(miss_bmi_all),
            `Missing height` = sum(is.na(ht)),
            `Missing weight` = sum(is.na(wt)),
            `Can calculate missing BMI` = sum(can_calc),
            `Can calculate missing BMI percentile` = sum(bmi_no_pct)) |>
  readr::write_csv('data/missing.csv')
