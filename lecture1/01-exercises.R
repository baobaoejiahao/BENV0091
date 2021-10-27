library(tidyverse)

# Load data
df <- read_csv('data/owid_co2.csv')

# Rename column 
df <- rename(df, Mtco2 = co2)

# Add a Mtco2 column
df <- mutate(df, Gtco2 = Mtco2/1e3)

# Arrange by Gtco2 
df <- arrange(df, desc(Gtco2))

# Remove World
df <- filter(df, country!='World')

# Filter to 2019 only and only countries with > 1 GtCO2 
df <- filter(df, Gtco2 > 1, year==2019)

df 
