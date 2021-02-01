#plotting data
#ggplot 
#The grammar of graphics starts with the observation that all plots have an x-axis and y-axis, which requires a mapping between your data and the axes
# all plots can be understood as mappings between your data and a plotting aesthetic (such as the x/y-axis).

#jhopkins we already know that we want to map time to the x-axis, and case counts to the y-axis.
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(dplyr)
library(tidyr)


covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>%
  read.csv() %>%
  clean_names() %>%
  select(-c(uid:fips, country_region:combined_key)) %>%
  rename(county = admin2, state = province_state) %>%
  pivot_longer(cols = -c(county, state), names_to = "date", values_to = "cases") %>%
  mutate(date = str_remove(date, "x"),
         date = mdy(date)) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup()

#pipe data directly into ggplot 
covid%>%
  ggplot(mapping= aes(x=date, y=cases))

#geometries; diff types of visualizations 
covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_line()

covid%>%
  filter(state== "California")%>%
  ggplot(mapping = aes(x=date, y=cases))+
  geom_bar(stat= "identity")

covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_area()


covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_point()

covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_boxplot() +
  ggtitle("covid-19 cases in the USA over time")

covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_violin(scale = "width") +
  ggtitle("covid-19 cases in the USA over time")


covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  filter(time > "Jun") %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 5, binwidth = 1000) +
  ggtitle("covid-19 cases in the USA over time")


covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_point() +
  ggtitle("covid-19 cases in the USA over time")


#all above have been for bivariate data 

#but many univeriate geometries 
covid %>%
  filter(date == as.Date("2020-09-24")) %>%
  ggplot(mapping = aes(x=cases)) +
  geom_histogram() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")


covid %>%
  filter(date == as.Date("2020-09-24")) %>%
  ggplot(mapping = aes(x=cases)) +
  geom_density() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")
covid %>%
  filter(date == as.Date("2020-09-24")) %>%
  ggplot(mapping = aes(x=cases)) +
  geom_freqpoly() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")

covid %>%
  filter(date == as.Date("2020-09-24")) %>%
  ggplot(mapping = aes(x=cases)) +
  geom_dotplot() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")


#layering 
#barplot for showing central tendencies 
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("covid-19 cases in the USA over time")

#boxplots good for showing shape of distribution 
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_dotplot(binaxis = "y", stackdir = "center",
               dotsize = 2, binwidth = 1000) +
  ggtitle("covid-19 cases in the USA over time")
#ordering matters w layers

#more aesthetics 
#color, fill, alpha (transparency), size, linetype, and group.

covid %>%
  ggplot(mapping = aes(x=date, y=cases, fill=state)) +
  geom_area() +
  theme(legend.position = "bottom")


elections <- here("Data", "countypres_2000-2016.csv") %>%
  read_csv() %>%
  filter(year == 2016) %>%
  filter(party %in% c("democrat", "republican")) %>%
  group_by(state, candidate) %>%
  summarise(candidatevotes = sum(candidatevotes, na.rm=T)) %>%
  group_by(state) %>%
  mutate(lean_democrat = candidatevotes / first(candidatevotes)) %>%
  filter(candidate == "Hillary Clinton") %>%
  ungroup() %>%
  select(state, lean_democrat)

covid %>%
  inner_join(elections) %>%
  filter(state != "District of Columbia") %>%
  ggplot(mapping = aes(x=date, y=cases, color=lean_democrat, group=state)) +
  geom_line()

#The color aesthetic is used for lines, outlines, and (most of the time) single dots in geom_point. The fill aesthetic is used for filling in outlines.

covid %>%
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>%
  mutate(date = month(date, label = TRUE)) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  ggplot(mapping = aes(x=date, y=cases, fill=state)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme(legend.position = "bottom")

covid %>%
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>%
  mutate(date = month(date, label = TRUE)) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  ggplot(mapping = aes(x=date, y=cases, color=state)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme(legend.position = "bottom")
