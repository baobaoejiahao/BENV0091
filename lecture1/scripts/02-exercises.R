library(tidyverse)
library(glue) # This is a package that is used to include variables in strings! (See usage below)

# Load data
co2 <- read_csv('data/owid_co2.csv')
energy <- read_csv('data/owid_energy.csv')

# Join the data! 
df <- left_join(co2, energy, by=c("country", "year"))

# Number of NAs in each column
print(colSums(is.na(df)))

# 1. Drop World and remove NAs (with pipe!)
df <- df %>% 
  filter(country!='World') %>%
  drop_na()

# 2. Create new variables for CO2/E, E/GDP, CO2/capita and GDP/capita
df <- df %>% 
  mutate(co2_intensity=co2/primary_energy_consumption,
         energy_intensity=primary_energy_consumption/gdp,
         co2_per_cap=co2/population,
         gdp_per_cap=gdp/population)

# 3. Set country names to lower case
df <- df %>% 
  mutate(country = tolower(country))

# 4. Save combined data frame to a new file in the data directory
write_csv(df, 'output/owid_joined.csv')

# 5. Which country had the highest GDP/capita in 2000?
df_5 <- df %>%
  filter(year==2000) %>%
  filter(gdp_per_cap==max(gdp_per_cap))
answer <- df_5$country
print(glue("Q5: {answer}"))

# 6. Which country had the largest per capita CO2 emissions in 1965
df_6 <- df %>% 
  filter(year==1965) %>% 
  filter(co2_per_cap==max(co2_per_cap))
answer <- df_6$country
print(glue("Q6: {answer}"))

# 7. What proportion of countries had a GDP per capita of under $1000 in 1990?
df_7 <- df %>% 
  filter(year==1990)
proportion <- mean(df_7$gdp_per_cap < 1000)
pct <- round(100*proportion, 2)
print(glue("Q7: {pct}%"))

# 8. What was the percentage change in global CO2 emissions between 1965 and 2016?
co2_1990 <- df %>% 
  filter(year==1990) %>% 
  pull(co2) %>% # pull(df, column) retrieves column from df 
  sum()

co2_2016 <- df %>% 
  filter(year==2016) %>% 
  pull(co2) %>%
  sum()
  
factor = co2_2016 / co2_1990
pct_change = 100*(factor - 1)
pct_change <- round(pct_change, 2)
print(glue("Q8: {pct_change}%"))
