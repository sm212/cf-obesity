# This takes a very long time to run!!
# The code makes a dataframe of every possible combination of
# sex, age (between 2 and 18), and BMI then uses a package
# from the RCP to calculate BMI z-scores and percentiles.
# The result is way too many rows. Several rows have the same
# centile since the BMI and ages vary so slowly.
# I've done some extra processing outside of this script to
# just keep the unique values (rounded to 3 dp).
# 
# You'll need to install the rcpchgrowth before running this.
# Get it from here:
# https://github.com/rcpch/rcpchgrowth-python/ 

import pandas as pd, itertools, numpy as np
from datetime import datetime, timedelta
from dateutil.relativedelta import relativedelta
from rcpchgrowth import Measurement

def expand_grid(vals):
    rows = itertools.product(*vals.values())
    df = pd.DataFrame.from_records(rows, columns=vals.keys())
    return df

start_date = datetime(2001, 1, 1) 
end_date   = start_date + relativedelta(years=18)
bmi        = [i for i in np.arange(0.1, 40, step=0.01)]
sex        = ['male', 'female']

# BMI percentiles can't be calculated for children aged under 2.
# Only select dates which correspond to ages 2-17
dates = []
obs_date = start_date + timedelta(days=1)

while obs_date <= end_date:
    if (obs_date - start_date).days >= 2 * 365.25 and (obs_date - start_date).days < 18 * 365.25:
        dates.append(obs_date)
    obs_date += timedelta(days=1)

# Calulate ages for each start_date & date pair. Convert to np.array for subsetting later
age_years = np.array([np.floor((date - start_date).days / 365.25) for date in dates])
dates = np.array(dates)

# Calculate BMI - same method as the NHS calcuator 
# https://www.nhs.uk/health-assessment-tools/calculate-your-body-mass-index/calculate-bmi-for-children-teenagers

def calc_bmi(row):
    measurement = Measurement(sex=row['sex'], birth_date=row['start_date'], observation_date=row['obs_date'],
                              observation_value=row['bmi'], measurement_method='bmi', reference='uk-who').measurement
    
    age_dec = measurement['measurement_dates']['chronological_decimal_age']
    z_score = measurement['measurement_calculated_values']['chronological_sds']
    pctile  = measurement['measurement_calculated_values']['chronological_centile']

    out = f'{row.sex},{age_dec},{row.bmi},{z_score},{pctile}'
    return out

for age in range(2, 18):
    
    # Make dataframe for all dates which correspond to current age only
    # df has c3M rows (processing them is the slow bit, takes a few hours per df)
    age_dates = dates[age_years == age]
    df = expand_grid({'sex' : sex, 'start_date' : [start_date], 'obs_date': age_dates, 'bmi' : bmi})

    if (age < 10):
        age_str = f'0{age}'
    else:
        age_str = age

    outf = f'data/raw_lkp/lkp_child_bmi{age_str}.csv'

    print(f'Making BMI lookup for age {age}:')
    with open(outf, 'a') as f:
        for j in range(df.shape[0]):
            s = calc_bmi(df.iloc[j])
            print(s, file=f)
            print(f'\tProcessed row {j} of {df.shape[0]}', flush=True, end='\r')