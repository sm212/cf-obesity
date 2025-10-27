import pandas as pd, itertools, numpy as np
from datetime import datetime, timedelta
from dateutil.relativedelta import relativedelta
from rcpchgrowth import Measurement

df = pd.read_csv('data/missing_child_bmi.csv')
df['sex'] = df['sex'].replace({'F' : 'female', 'M' : 'male'})
df['birth_dt'] = pd.to_datetime(df['birth_dt']).dt.date
df['review_dt'] = pd.to_datetime(df['review_dt']).dt.date

def calc_bmi(row):
    measurement = Measurement(sex=row['sex'], birth_date=row['birth_dt'], observation_date=row['review_dt'],
                              observation_value=row['bmi'], measurement_method='bmi', reference='uk-who',
                              gestation_weeks=40, gestation_days=0).measurement
    
    z_score = measurement['measurement_calculated_values']['chronological_sds']
    pctile  = measurement['measurement_calculated_values']['chronological_centile']

    out = f'{row.regid_anon},{row.year},{row.sex},{row.bmi},{pctile}'
    return out

outf = 'missing_percentiles.csv'
with open(outf, 'a') as f:
    for j in range(df.shape[0]):
            s = calc_bmi(df.iloc[j])
            print(s, file=f)
            print(f'\tProcessed row {j} of {df.shape[0]}', flush=True, end='\r')