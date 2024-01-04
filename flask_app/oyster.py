import pandas as pd
df = pd.read_csv('static/data/All_Oyster_Parameters-2023-Jun-05.txt', sep='|')

data_providers = df.ManagedAreaName.unique()
n_data_providers = len(data_providers)
programs = df.ProgramName.unique()
parameters = df.ParameterName.unique()

def get_plot(program_id, parameter, start_year, end_year):
    try:
        data = df[(df.ProgramID==program_id)&(df.ParameterName==parameter)]
        data_years = data[(data.Year>=start_year)&(data.Year<=end_year)]
    except:
        pass
    return data_years[['ProgramID','ProgramName','ManagedAreaName','ParameterName','SampleDate','ResultValue']].to_html()