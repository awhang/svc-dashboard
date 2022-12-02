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

startdate <- read_csv("staffdates.csv") %>%
  unite(Name, First:Last, sep = " ") %>%
  rename(startdate = "Hire Date",
         analyst = "Name") %>% 
  filter(Role %notin% c('WFA','L2','Manager','Robert Half','TAM'),
         !is.na(startdate)) %>%
  select(!Role)


longreport <- read_csv("report1609302593041.csv") %>%
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

ent_svc_per_month <- longreport %>%
  filter(Team == 'Enterprise') %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  mutate(count = n()) %>%
  select(month, count) %>%
  distinct() 

ent_subres_per_month <- longreport %>% 
  
  
ggplot(ent_svc_per_month, aes(month, count)) + 
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=3))

svc <- longreport %>% 
  mutate(week = week(date)) %>%
  group_by(date) %>%
  mutate(count = n()) %>%
  select(date, count) %>%
  distinct()

ggplot(svc, aes(date, count)) + 
  geom_jitter() + 
  labs(title="SVCs over time per Day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=3))

by_segment_fix <- longreport %>%
  filter(subresolution == "Setup Fix") %>%
  mutate(total = n()) %>%
  group_by(Team) %>%
  mutate(per = n()/total * 100) %>%
  select(Team, per) %>%
  distinct() %>%
  arrange(desc(per)) %>%
  filter(Team %notin% c("Five Star","Pune"))


ggplot(by_segment_fix, aes(x=reorder(Team, -per), y=per)) +
  geom_col() +
  labs(title="Fix Requests by Team", x="", y="Percentage of Fix SVCs")

by_subresolution <- longreport %>%
  group_by(subresolution) %>%
  mutate(total = n()) %>%
  select(subresolution, total) %>%
  distinct() %>%
  arrange(desc(per)) %>%
  filter(!is.na(subresolution)) %>%
  ungroup() %>% 
  top_n(10)

by_sub_raw <- longreport %>%
  group_by(subresolution) %>%
  mutate(total = n()) %>%
  select(subresolution, total, owner) %>%
  distinct %>% 
  arrange(desc(total))

pa <- longreport %>%
  filter(subresolution == 'Platform Admin') %>%
  group_by(Team) %>%
  mutate(total = n()) %>%
  select(Team, total) %>% 
  distinct %>% 
  arrange(desc(total))

write_csv(by_subresolution, "by_subresolution.csv")

by_resolution <- longreport %>%
  mutate(total = n()) %>%
  group_by(resolution) %>%
  mutate(per = n()/total * 100) %>%
  select(resolution, per) %>%
  distinct() %>%
  arrange(desc(per))

ggplot(by_resolution, aes(x=reorder(resolution, -per), y=per)) + 
  geom_col() +
  labs(title="SVCs by Resolution", x="", y="Percentage of Total SVCs") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

by_team <- longreport %>%
  mutate(total = n()) %>%
  group_by(Team) %>%
  mutate(svc_per = n()/total * 100) %>%
  select(Team, svc_per) %>%
  distinct() %>%
  arrange(desc(svc_per))

svc_team_totals <-longreport %>%
  group_by(Team) %>%
  mutate(svc_total = n()) %>%
  select(Team, svc_total) %>%
  distinct() %>%
  arrange(desc(svc_total))

# How to consider the cases where they are handed off to the segments 
# of review to implementation for FiveStar.
# For Five star analysts/team, should number of cases be increased by their 
# respective number of referred to implementation SVCs? 
svc_to_imp_totals <- longreport %>%
  filter(subresolution == "Implementation") %>%
  group_by(Team) %>%
  mutate(total = n()) %>%
  select(Team, total) %>%
  distinct

# Adding this amount changes their ratio by a negligble amount

case_raw <- read_csv('report1609302195294.csv')


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

svccase_ratio <- case_totals %>%
  left_join(svc_team_totals) %>%
  left_join(case_per) %>%
  left_join(by_team) %>%
  mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
  arrange(desc(ratio)) %>%
  rename("Total Cases" = "case_total",
         "Total SVCs" = "svc_total",
         "Case Percentage" = "case_per",
         "SVC Percentage" = "svc_per",
         "SVCs per 100 Cases" = "ratio")

write_csv(svccase_ratio, "svccase_ratio.csv")

ggplot(by_Team, aes(x=reorder(Team, -per), y=per)) + 
  geom_col() +
  labs(title="SVCs by Team", x="", y="Percentage of Total SVCs") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
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



###


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

by_owner <- longreport %>%
  mutate(total = n()) %>%
  group_by(owner) %>%
  mutate(per = n()/total * 100) %>%
  select(owner, per) %>%
  distinct() %>%
  arrange(desc(per))
