library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
setwd("~/data")
`%notin%` <- Negate(`%in%`)

teams <- read_csv("analyst teams.csv") %>%
  unite(Name, First:Last, sep = " ") %>%
  filter(Team %notin% c('WFA','L2','Manager','Robert Half')) %>%
  select(Name, Team)

owners <- read_csv("svc_owner.csv")
  
report <- read_csv("report1607711188026.csv") %>%
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
  distinct


write_csv(report, "l2svcreport.csv")

str(report)

by_segment_imp <- report %>%
  filter(subresolution == "Implementation") %>%
  mutate(total = n()) %>%
  group_by(Team) %>%
  mutate(per = n()/total * 100) %>%
  select(Team, per) %>%
  distinct() %>%
  arrange(desc(per))
write.csv(by_segment_imp, "by_segment_imp.csv")

qplot(x=reorder(Team, -per), y=per, data = by_segment_imp, geom="col") + 
  labs(title="Referred to Imp by Team", x="", y="Percentage of Referred SVCs")

by_segment_PA <- report %>%
  filter(subresolution == "Platform Admin") %>%
  mutate(total = n()) %>%
  group_by(Team) %>%
  mutate(per = n()/total * 100) %>%
  select(Team, per) %>%
  distinct() %>%
  arrange(desc(per))
write.csv(by_segment_PA, "by_segment_PA.csv")

qplot(x=reorder(Team, -per), y=per, data = by_segment_PA, geom="col") + 
  labs(title="PA Requests by Team", x="", y="Percentage of PA SVCs")

by_segment_fix <- report %>%
  filter(subresolution == "Setup Fix") %>%
  mutate(total = n()) %>%
  group_by(Team) %>%
  mutate(per = n()/total * 100) %>%
  select(Team, per) %>%
  distinct() %>%
  arrange(desc(per))
write.csv(by_segment_fix, "by_segment_fix.csv")

qplot(x=reorder(Team, -per), y=per, data = by_segment_fix, geom="col") + 
  labs(title="Fix Requests by Team", x="", y="Percentage of Fix SVCs")

by_subresolution <- report %>%
  mutate(total = n()) %>%
  group_by(subresolution) %>%
  mutate(per = n()/total * 100) %>%
  select(subresolution, per, owner) %>%
  distinct() %>%
  arrange(desc(per)) %>%
  ungroup %>%
  top_n(15,per)
write.csv(by_subresolution, "by_subresolution.csv")

qplot(x=reorder(subresolution, -per), y=per, data = by_subresolution, geom="col") + 
  labs(title="Top 10 Subres since November 2020", x="", y="Percentage of Total SVCs") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

by_owner <- report %>%
  mutate(total = n()) %>%
  group_by(owner) %>%
  mutate(per = n()/total * 100) %>%
  select(owner, per) %>%
  distinct() %>%
  arrange(desc(per))
