import pandas as pd, numpy as np
from rcpchgrowth import Measurement

df = pd.read_csv('data/cross_sec.csv')
df = df[(df['year'] == 2017) & (df['ageg'] == '0-17') & (df['bmi'].notna()) & (df['bmi'] > 0)]
df['birth_dt'] = pd.to_datetime(df['birth_dt']).dt.date
df['review_dt'] = pd.to_datetime(df['review_dt']).dt.date
df['sex'] = df['sex'].replace({'F' : 'female', 'M' : 'male'})

print(df.shape)

def calc_bmi(row):
    measurement = Measurement(sex=row['sex'], birth_date=row['birth_dt'], observation_date=row['review_dt'],
                              observation_value=row['bmi'], measurement_method='bmi', reference='uk-who',
                              gestation_weeks=40, gestation_days=0).measurement
    
    z_score = measurement['measurement_calculated_values']['chronological_sds']
    pctile  = measurement['measurement_calculated_values']['chronological_centile']

    out = pd.Series({'regid_anon': row.regid_anon, 'calc_pctile' : pctile})
    return out

calc = df.apply(calc_bmi, axis = 1)
df = pd.concat((df, calc), axis = 1)
df['diff'] = df['bmi_percentile'] - df['calc_pctile']
weird = df.loc[np.abs(df['diff']) > 1, 'regid_anon']
print(weird.shape)
df.to_csv('tmp.csv')