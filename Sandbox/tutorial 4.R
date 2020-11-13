2+6
sum(2,6)
mean(2,6)

#have to use c 
mean(c(2,6))

#make our own function for mean
custom_mean <- function(this_vector) {sum(this_vector) / length(this_vector)}

#this works bc supposed to be vector
custom_mean(c(2,6))
my_vector <-  c(2, 6)
#same thing: providing an argument to custom mean which gets assigned to this vector
custom_mean(my_vector)
custom_mean(this_vector = c(2, 6))
custom_mean(this_vector = my_vector)

library("here")
library("readr")
library("dplyr")

covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>%
  read.csv()

covid %>%
  slice(1:10)

#FILTER, rows/observations
#only want california counties
covid %>%
  filter(Province_State=="California")

"California" == "California"

#SELECT, columns
#only want counties where at least 1000 cases on 9/14/20
covid %>%
  filter(Province_State== "California") %>%
  select(fips = FIPS, county = Admin2, 'X9.18.20': 'X9.24.20') %>%
  filter(`X9.24.20` >= 1000)

#just look at yolo county
covid%>%
  filter(Province_State=="California", Admin2=="Yolo")%>%
  select(fips=FIPS, county=Admin2, 'X9.18.20': 'X9.24.20')
#already renaming fips and admin2 columns above
# : means from here to there
#backtick telling R to interpret it as a literal

covid %>%
  filter(Province_State=="California", Admin2=="Yolo")%>%
  select(state=Province_State, fips=FIPS, country=Admin2, latest_cases = 'X9.24.20')%>%
  select(-state) #dont want state veriable anymore, get rid of it using minus

#MUTATE, creating new variables
# to do statistical tests, need normal distribution so need to manipulate data

covid %>%
  filter(Province_State =="California") %>%
  slice(1:20)%>%
  select(fips=FIPS, country=Admin2, latest_cases='X9.24.20')%>%
  mutate(latest_cases_log = log(latest_cases))
#taking log of latest cases

covid %>%
  filter(Province_State=="California")%>%
  slice(1:5)%>%
  select(fips=FIPS, county= Admin2, latest_cases= `X9.24.20`) %>%
  mutate(latest_cases=log(latest_cases))
#replace latest cases with log version of it 

#now overite FIPS column instead 
covid %>%
  filter(Province_State=="California")%>%
  slice(1:5)%>%
  select(fips=FIPS, county= Admin2, latest_cases= `X9.24.20`) %>%
  mutate(fips=log(latest_cases))


#can also create a column containing a signle value for every observation
covid %>%
  filter(Province_State=="California")%>%
  slice(1:5)%>%
  select(fips=FIPS, county= Admin2, latest_cases= `X9.24.20`) %>%
  mutate(state="California")

#can apply the function across mutiple columns 
covid %>%
  select(state=Province_State, county=Admin2,'X9.18.20': 'X9.24.20')%>%
  filter(state =="California")%>%
  slice(1:10)%>%
  mutate(across(.cols='X9.18.20': 'X9.24.20',
        .fns= ~log(.x+1)))
#creating a lamda function, mutate columbs add one to each case count to avoid log(0)= negative infinity


#creating our custom function like the lamda function we used earlier 
avoid_log_trap <- function(x) {
  log(x + 1)
}

covid %>%
  select(state=Province_State, county=Admin2,'X9.18.20': 'X9.24.20')%>%
  filter(state =="California")%>%
  slice(1:10)%>%
  mutate(across(.cols='X9.18.20': 'X9.24.20',
                .fns= avoid_log_trap))
#similar to lamda function, tilde signals u are creating lamda function 

#SUMMARIZE

#interested in total number of covid cases on one day 
covid%>%
  filter(Province_State=="California")%>%
  select(fips=FIPS, country=Admin2, latest_cases='X9.24.20')%>%
  summarise(total_cases= sum(latest_cases))
 
#daily sum 
covid%>%
  filter(Province_State=="California")%>%
  select(fips=FIPS, country=Admin2, 'X9.18.20' : 'X9.24.20')%>%
  summarise(across(.cols= 'X9.18.20': 'X9.24.20',
                   .fns = sum))

#ARRANGE, reorder observations
#according to number of covid cases
covid %>%
  filter(Province_State =="California")%>%
  slice(1:5)%>%
  select(fips = FIPS, county=Admin2, latest_cases='X9.24.20')%>%
  arrange(latest_cases)

#reverse alphebetical according to county names using desc
covid %>%
  filter(Province_State =="California")%>%
  slice(1:5)%>%
  select(fips = FIPS, county=Admin2, latest_cases='X9.24.20')%>%
  arrange(desc(county))


#GROUP_BY
#summarising total number of covid cases at state level, grouping together observations by county that belongs to same state
covid %>%
  rename(state=Province_State, latest_cases='X9.24.20')%>%
  group_by(state)%>%
  summarise(n_cases=sum(latest_cases))%>%
  ungroup() %>%
  arrange(desc(n_cases))
#need to remember to ungroup 

#JOIN, take peices of info stored in different dataframes, find a way to join them 
#ex. to see whether counties are doing systematically better/worse depending on how urban they are, or whether certain kinds of public health policies/philosophies are associated with covid19 prevalence


#1. dplyr::inner_join 2. dplyr::left_join 3. dplyr::right_join 4. dplyr::full_join


urbanicity<- here("Data", "NCHSURCodes2013.xlsx")%>%
  readxl::read_excel(na = c(".")) %>%
  janitor::clean_names() %>%
  select(fips_code, urbanicity = x2013_code, population = county_2012_pop)


 #pull in  county level voting data from 2016 electin, gives you number of votes for trump and number of votes for clintn
#want to create a single-number index of how much a particular county voted for Trump over Clinton.
  #so find the ratio votesTrump/votesClinton
elections<- here("Data","countypres_2000-2016.csv")%>%
  read_csv()%>%
  filter(year==2016)%>%
  filter(party%in%c("democrat","republican"))%>%
  group_by(state, county, FIPS) %>%
  mutate(lean_republican=candidatevotes/ first(candidatevotes))%>%
  ungroup()%>%
  filter(party=="republican")%>%
  select(state, county, FIPS, lean_republican)
  
elections %>%
  slice(1:5)
  
  
#time to join 
covid%>%
  select(FIPS, county=Admin2, state=Province_State, latest_cases= 'X9.24.20')%>%
  filter(state=="California")%>%
  slice(1:10)%>%
  left_join(elections)%>%
  left_join(urbanicity, by=c("FIPS"="fips_code")) #equating the two fips are the same thing

#left join means we are talking the 10 sliced data from covid and joining it w/electoins and urbanicity, keeping only 10 rows 

#right join means we are taking all of the elections/urbanicity and joining it with the 10 covid rows, keeping all rows of election 
covid%>%
  select(FIPS, county=Admin2, state=Province_State, latest_cases= 'X9.24.20')%>%
  filter(state=="California")%>%
  slice(1:10)%>%
  right_join(elections)%>% #now way more rows bc 3158 rows of elections but we'll just take 20
  slice(1:20)

#What if you only wanted to keep rows where there was a match in the LHS and RHS dataframes?
#ex. puerto rico has entries in covid but not elections

elections%>%
  filter(state=="Puerto Rico") #no data

#since left join, keeping everything in LHS, so produces NA when joining

covid %>%
  select(FIPS, county = Admin2, state = Province_State, latest_cases = `X9.24.20`) %>%
  filter(state %in% c("Puerto Rico", "California")) %>%
  # Note how the combination of group_by and slice results in an output where we keep
  # the first five rows from *both* Puerto Rico and California!
  group_by(state) %>%
  slice(1:5) %>%
  ungroup() %>%
  left_join(elections)
#produces NA 

#might only want to keep matching rows in LHS and RHS, use inner join
covid %>%
  select(FIPS, county = Admin2, state = Province_State, latest_cases = `X9.24.20`) %>%
  filter(state %in% c("Puerto Rico", "California")) %>%
  group_by(state) %>%
  slice(1:5) %>%
  ungroup() %>%
  inner_join(elections)
#only california kept

#full join to keep all rows from LHS and RHS
covid %>%
  select(FIPS, county = Admin2, state = Province_State, latest_cases = `X9.24.20`) %>%
  filter(state %in% c("Puerto Rico", "California")) %>%
  group_by(state) %>%
  slice(1:5) %>%
  ungroup() %>%
  # For the purpose of illustration, restricting elections to 5 Alabama counties
  full_join(elections %>%
              filter(state == "Alabama") %>%
              slice(1:5))
#no matching rows so thats why some NAs 


#EXERCISES 

#1 

incarceration <- here("Data", "incarceration_trends.csv")%>%
  read_csv()

#2 

ca_jail <- incarceration%>%
  filter(state=="CA", year=="2018")

#3 
ca_jail%>%
  select(fips,total_pop, total_jail_pop)

#4
ca_jail%>%
  select(fips,total_pop, total_jail_pop)%>%
  mutate(prop_jail = total_jail_pop / total_pop)
  
#5
ca_jail%>%
  select(fips,total_pop, total_jail_pop)%>%
  mutate(prop_jail = total_jail_pop / total_pop)%>%
  select(fips, prop_jail)%>%
  slice(1:10)%>%
  left_join(elections, by=c("fips"="FIPS"))%>%
  arrange(lean_republican)

#by first ten rows and last ten rows seems like morel lean reublican, more jail prop

#6
ca_jail%>%
  select(fips,total_pop, total_jail_pop)%>%
  mutate(prop_jail = total_jail_pop / total_pop)%>%
  select(fips, prop_jail)%>%
  slice(1:10)%>%
  left_join(elections, by=c("fips"="FIPS"))%>%
  mutate(more_trump= lean_republican>1)

#7 mean/sd of prop_jail based on trump/clinton
new_data<-ca_jail%>%
  select(fips,total_pop, total_jail_pop)%>%
  mutate(prop_jail = total_jail_pop / total_pop)%>%
  select(fips, prop_jail)%>%
  left_join(elections, by=c("fips"="FIPS"))%>%
  mutate(more_trump= lean_republican>1)

mean(new_data$prop_jail[new_data$more_trump==TRUE]) #.002942808
sd(new_data$prop_jail[new_data$more_trump==TRUE]) #.00093

mean(new_data$prop_jail[new_data$more_trump==FALSE], na.rm=TRUE) #.002124033
sd(new_data$prop_jail[new_data$more_trump==FALSE], na.rm=TRUE) #.0007442979
#na.rm = TRUE to remove missing values

?sd

new_data<-ca_jail%>%
  select(fips,total_pop, total_jail_pop)%>%
  mutate(prop_jail = total_jail_pop / total_pop)%>%
  select(fips, prop_jail)%>%
  left_join(elections, by=c("fips"="FIPS"))%>%
  mutate(more_trump= lean_republican>1)%>%
  group_by(prop_jail, more_trump)%>%
  summarise(prop_jail= mean(prop_jail,na.rm=TRUE, sd(prop_jail, na.rm= TRUE)))%>%
  ungroup()

#idk how to summarise mean/sd for prop_jail based off trump/hillary counties 
#ask 

