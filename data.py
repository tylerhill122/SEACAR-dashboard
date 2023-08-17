import pandas as pd

df = pd.read_csv("static/data/Discrete WQ - 5008.txt", sep='|')

# Cleaning ProgramLocation names
# adding station number column
df['Station'] = [df.ProgramLocationID[i].replace('WIN_21FLUFSW_','').split('-')[2] for i in df.index]
# allow numerical indexing on station column
df['Station'] = df['Station'].astype(int)
# program name column
df['Program'] = [df.ProgramLocationID[i].replace('WIN_21FLUFSW_','').split('-')[1] for i in df.index]
# drop old column
df.drop(columns=['ProgramLocationID'], inplace=True)

# Full name for sites
prog_short = df.Program.unique()
prog_long = ['Aripeka','Weeki Wachee','Chassahowtizka','Anclote','Hudson','Pithlachascotee','Homosassa','Crystal','Withlacoochee']
df.Program = df.Program.replace(prog_short, prog_long)

def get_data(program, station):
    data = df[(df.Program==program) & (df.Station==station)]
    
    temp = round(data[data.ParameterName=='Water Temperature'].ResultValue.mean(),2)
    disox = round(data[data.ParameterName=='Dissolved Oxygen'].ResultValue.mean(),2)
    spc = round(data[data.ParameterName=='Specific Conductivity'].ResultValue.mean(),2)
    cdom = round(data[data.ParameterName=='Colored dissolved organic matter, CDOM'].ResultValue.mean(),2)
    ph = round(data[data.ParameterName=='pH'].ResultValue.mean(),2)
    disoxsat = round(data[data.ParameterName=='Dissolved Oxygen Saturation'].ResultValue.mean(),2)
    
    return data, temp, disox, spc, cdom, ph, disoxsat