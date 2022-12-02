library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
setwd("~/data")
`%notin%` <- Negate(`%in%`)

teams <- read_csv("analyst teams.csv") %>%
  unite(Name, First:Last, sep = " ") %>%
  filter(Team %notin% c('WFA','L2','Manager','Robert Half','TAM')) %>%
  select(Name, Team)

owners <- read_csv("svc_owner.csv")


## SVC REPORT LOAD
svc_raw <- read_csv("report1610311835277.csv")
# SVC data prep
svc_report <- svc_raw %>%
  slice(1:(n()-5)) %>%
  rename(svc = 'Service Timecard: Service Timecard ID',
         analyst = 'Service Timecard: Owner Name',
         cs_segment = 'Account CS Segment',
         fi_segment = 'Account FI Team',
         resolution = 'Resolution',
         subresolution = 'Sub-Resolution',
         date = 'Service Timecard: Created Date') %>%
  left_join(teams, by= c("analyst" = "Name")) %>%
  left_join(owners, by = "subresolution") %>%
  left_join(startdate) %>% 
  filter(!is.na(Team)) %>%
  mutate(date = mdy(date),
         startdate = mdy(startdate)) %>%
  filter(date > "2020-07-31")

## CASE REPORT LOAD
case_raw <- read_csv('report1610312438709.csv')
# case load percentage by segment team
case_per <- case_raw %>%
  slice(1:(n()-5)) %>%
  rename(analyst = 'Case Owner',
         date = 'Opened Date') %>%
  mutate(date = mdy(date)) %>%
  filter(date > "2020-07-27") %>%
  semi_join(teams, by=c("analyst" = "Name")) %>%
  left_join(teams, by=c("analyst" = "Name")) %>%
  mutate(total = n()) %>%
  group_by(Team) %>%
  mutate(case_per = n()/total * 100) %>%
  select(Team, case_per) %>%
  distinct() %>%
  arrange(desc(case_per))

#case totals by segment team
case_totals <- case_raw %>%
  slice(1:(n()-5)) %>%
  rename(analyst = 'Case Owner',
         date = 'Opened Date') %>%
  mutate(date = mdy(date)) %>%
  filter(date > "2020-07-27") %>%
  semi_join(teams, by=c("analyst" = "Name")) %>%
  left_join(teams, by=c("analyst" = "Name")) %>%
  group_by(Team) %>%
  mutate(case_total = n()) %>%
  select(Team, case_total) %>%
  distinct() %>%
  arrange(desc(case_total))

#svc totals by segment team
svc_team_totals <-svc_report %>%
  group_by(Team) %>%
  mutate(svc_total = n()) %>%
  select(Team, svc_total) %>%
  distinct() %>%
  arrange(desc(svc_total))
#svc percentage by segment team
svc_team_per <- svc_report %>%
  mutate(total = n()) %>%
  group_by(Team) %>%
  mutate(svc_per = n()/total * 100) %>%
  select(Team, svc_per) %>%
  distinct() %>%
  arrange(desc(svc_per))

svccase_ratio <- case_totals %>%
  left_join(svc_team_totals) %>%
  left_join(case_per) %>%
  left_join(svc_team_per) %>%
  mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
  arrange(desc(ratio)) %>%
  rename("Total Cases" = "case_total",
         "Total SVCs" = "svc_total",
         "Case Percentage" = "case_per",
         "SVC Percentage" = "svc_per",
         "SVCs per 100 Cases" = "ratio")

### All Support SVCs by Month

svc_total_all <- svc_report %>% 
  mutate(month = month(date)) %>%
  group_by(month) %>%
  mutate(svc_total = n()) %>%
  select(month, svc_total) %>%
  distinct() 

case_total_all <- case_raw %>%
  slice(1:(n()-5)) %>%
  rename(analyst = 'Case Owner',
         date = 'Opened Date') %>%
  mutate(date = mdy(date)) %>%
  filter(date > "2020-07-27") %>%
  semi_join(teams, by=c("analyst" = "Name")) %>%
  left_join(teams, by=c("analyst" = "Name")) %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  mutate(case_total = n()) %>%
  select(month, case_total) %>%
  distinct() 

svccase_ratio_all <- case_total_all %>%
  left_join(svc_total_all) %>% 
  mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
  arrange(desc(ratio)) %>%
  rename("Total Cases" = "case_total",
         "Total SVCs" = "svc_total") %>%
         #"SVCs per 100 Cases" = "ratio") %>%
  arrange(month)
  
#count SVCs per month, per subresolution type, and then spread data per month
svc_total_by_subresolution <- svc_report %>% 
  mutate(month = month(date),
         month = case_when(month == '8' ~ 'August',
                           month == '9' ~ 'September',
                           month == '10' ~ 'October',
                           month == '11' ~ 'November',
                           month == '12' ~ 'December'),
         subresolution = case_when(is.na(subresolution) ~ "Not Coded",
                                   TRUE ~ subresolution)) %>% 
  group_by(subresolution, month) %>%
  summarize(total = n()) %>% 
  spread(month, total) %>%
  select(subresolution, August, September, October, November, December) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(Total = sum(c_across(where(is.numeric))))

#plots
ggplot(svccase_ratio_all, aes(month, ratio)) + 
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=3))

### Enterprise

ent_svc <- svc_report %>%
  filter(Team == 'Enterprise') %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  mutate(svc_total = n()) %>%
  select(month, svc_total) %>%
  distinct() 

ent_case <- case_raw %>%
  slice(1:(n()-5)) %>%
  rename(analyst = 'Case Owner',
         date = 'Opened Date') %>%
  mutate(date = mdy(date)) %>%
  filter(date > "2020-07-27") %>%
  semi_join(teams, by=c("analyst" = "Name")) %>%
  left_join(teams, by=c("analyst" = "Name")) %>%
  filter(Team == 'Enterprise') %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  mutate(case_total = n()) %>%
  select(month, case_total) %>%
  distinct() 

ent_svccase_ratio <- ent_case %>%
  left_join(ent_svc) %>% 
  mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
  arrange(desc(ratio)) %>%
  rename("Total Cases" = "case_total",
         "Total SVCs" = "svc_total") %>%
        # "SVCs per 100 Cases" = "ratio") %>%
  arrange(month)

ent_by_subresolution <- svc_report %>% 
  mutate(month = month(date),
         subresolution = case_when(is.na(subresolution) ~ "Not Coded",
                                   TRUE ~ subresolution)) %>% 
  filter(Team == 'Enterprise') %>%
  group_by(subresolution, month) %>%
  summarize(total = n()) %>% 
  spread(month, total) 
#change NA values to 0
ent_by_subresolution[is.na(ent_by_subresolution)] <- 0



##############################

# How to consider the cases where they are handed off to the segments 
# of review to implementation for FiveStar.
# For Five star analysts/team, should number of cases be increased by their 
# respective number of referred to implementation SVCs? 
svc_to_imp_totals <- svc_report %>%
  filter(subresolution == "Implementation") %>%
  group_by(Team) %>%
  mutate(total = n()) %>%
  select(Team, total) %>%
  distinct
# Adding this amount changes their ratio by a negligble amount

### L2 Performance Data

allsvc <- read_csv("report1609787552507.csv") %>%
  slice(1:(n()-5)) %>%
  rename(svc = 'Service Timecard: Service Timecard ID',
         analyst = 'Service Timecard: Owner Name',
         cs_segment = 'Account CS Segment',
         fi_segment = 'Account FI Team',
         resolution = 'Resolution',
         subresolution = 'Sub-Resolution',
         date = 'Service Timecard: Created Date',
         l2 = 'Assigned To') %>%
  filter(!is.na(resolution),
         !is.na(subresolution))%>%
  left_join(teams, by= c("analyst" = "Name")) %>%
  filter(!is.na(Team)) %>%
  mutate(date = mdy(date)) %>%
  filter(date > "2020-7-27")

l2_per <- allsvc %>%
  mutate(total = n()) %>%
  group_by(l2) %>%
  mutate(svc_per = n()/total * 100) %>%
  select(l2, svc_per) %>%
  distinct() %>%
  arrange(desc(svc_per))

l2 <- allsvc %>%
  group_by(l2) %>%
  mutate(total = n()) %>%
  select(l2, total) %>%
  distinct() %>%
  arrange(desc(total)) 

by_owner <- svc_report %>%
  mutate(total = n()) %>%
  group_by(owner) %>%
  mutate(per = n()/total * 100) %>%
  select(owner, per) %>%
  distinct() %>%
  arrange(desc(per))


test <- svc_report %>% 
  mutate(month = month(date),
         year = year(date)) %>%
  filter(!is.na(subresolution),
         year == max(year),
         month == max(month)) %>%
  group_by(subresolution) %>%
  mutate(total = n()) %>%
  select(subresolution, total) %>%
  distinct() %>%
  arrange(desc(total)) %>%
  ungroup() %>% 
  top_n(10)
