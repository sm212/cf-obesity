copy (select sex, age_years, bmi, centile, z_score
from
    (
        select
            sex,
            age_years,
            bmi,
            centile,
            z_score,
            row_number() over (partition by sex, age_years, centile order by bmi) as rn
        from '{{input}}'
    )
where
    rn = 1 and
    (centile between 0.1 and 99.6)
order by 1,2,3
) to '{{output}}' (header, delimiter ',')
