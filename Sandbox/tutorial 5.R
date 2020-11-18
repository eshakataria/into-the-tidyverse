#tidying data
#tidy data is that: 1. Every column is a variable. 2. Every row is an observation. 3. Every cell is a single value.
#today looking at data formats that aren't tidy, learn how to wrangle w them

#four fundamental tidyr functions, which are (more-or-less) mirrors of each other 
  #1. pivot_longer: take wide data and make it longer 
  #2. pivot_wider: take long data and make it wider 
  #3. separate: separating a single cell into multiple columns 
  #4. unite: smooshing multiple columns into a single cell

#pivot_longer

library(tidyr)
library(readr)

billboard%>%
  slice(1:10)

#wide format, because a single row can extend widely (i.e., there are 76 weeks per row in this dataset!

#use pivot longer to tidy it

billboard%>%
  pivot_longer(cols= starts_with("wk"),
               names_to="week", 
               values_to= "ranking")%>%
  drop_na%>% #drop every row from the dataset that contained an NA
  group_by(track)%>%
  slice(1:5)%>%
  ungroup()

#you want each column to be a variable and every row an observation


#pivot_wider

us_rent_income%>%
  pivot_wider(names_from="variable", values_from=c("estimate","moe"))

#want to see what proportion of a person’s income is being spent on rent? To answer this question, you’d want to multiply the monthly rent by 12, the divide the yearly rent by the yearly income to get a proportion.


us_rent_income%>%
  pivot_wider(names_from="variable", values_from=c("estimate","moe"))%>%
  select(locale=NAME, estimate_income, estimate_rent)%>%
  group_by(locale)%>%
  summarise(p_income_spent_on_rent= 12*estimate_rent / estimate_income)%>%
  arrange(p_income_spent_on_rent)


#my way at first 
us_rent_income%>%
  pivot_wider(names_from="variable", values_from=c("estimate","moe"))%>%
  mutate(prop= 12*estimate_rent/estimate_income)%>%
  arrange(prop)


#separate

library(readr)
library(here)

conformity <- here("Data", "JustCon5_TPP_Order1.csv")%>%
  read_csv()%>%
  select(sub_id = mTurkCode,
         starts_with("assault"),
         starts_with("theft")) %>%
  # Don't worry about this for the time being
  slice(-1) %>%
  type_convert()

conformity%>%
  



