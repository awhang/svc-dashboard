library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
library(plotly)
library(flexdashboard)
library(knitr)
library(DT)

`%notin%` <- Negate(`%in%`)

## Analysts by segment teams
teams <- read_csv('analyst teams.csv') %>%
  unite(Name, First:Last, sep = ' ') %>%
  filter(Team %notin% c('WFA','L2','Manager','Robert Half','TAM')) %>%
  select(Name, Team)

maxDate = '2021-07-01'

## SVC REPORT LOAD
svc_report <- read_csv('SVC Data Pull.csv') %>%
  slice(1:(n()-5)) %>%
  rename(svc = 'Service Timecard: Service Timecard ID',
         analyst = 'Service Timecard: Owner Name',
         cs_segment = 'Account CS Segment',
         fi_segment = 'Account FI Team',
         resolution = 'Resolution',
         subresolution = 'Sub-Resolution',
         date = 'Service Timecard: Created Date',
         l2 = 'Assigned To') %>%
  left_join(teams, by= c('analyst' = 'Name')) %>%
  mutate(date = mdy(date),
         date = floor_date(date, 'month')) %>%
  filter(!is.na(Team),
         date > '2020-07-31',
         date < maxDate)

## CASE REPORT LOAD
case_raw <- read_csv('Case Data Pull.csv') %>%
  slice(1:(n()-5)) %>%
  rename(analyst = 'Case Owner',
         date = 'Opened Date') %>%
  mutate(date = mdy(date),
         date = floor_date(date, 'month')) %>%
  filter(date > '2020-07-31',
         date < maxDate) %>%
  semi_join(teams, by=c('analyst' = 'Name')) %>%
  left_join(teams, by=c('analyst' = 'Name'))

###### Functions 
dummy <- 'All'
totalRatio <- function(filter_var) {

  svc_total_all <- svc_report %>% 
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(svc_total = n()) %>% 
    select(date, svc_total) %>% 
    distinct() %>% 
    arrange(date) 
  
  case_total_all <- case_raw %>%
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(case_total = n()) %>% 
    select(date, case_total) %>% 
    distinct() %>% 
    arrange(date)
  
  svccase_ratio_all <- case_total_all %>%
    left_join(svc_total_all) %>% 
    mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
    arrange(desc(ratio)) %>%
    rename('Total Cases' = 'case_total',
           'Total SVCs' = 'svc_total') %>%
    arrange(date) %>%
    mutate(date = format(date, '%m/%Y'))
}

#plot totalRatio
totalRatioPlot <- function(filter_var) {
  svc_total_all <- svc_report %>% 
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(svc_total = n()) %>% 
    select(date, svc_total) %>% 
    distinct() %>% 
    arrange(date) 
  
  case_total_all <- case_raw %>%
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(case_total = n()) %>% 
    select(date, case_total) %>% 
    distinct() %>% 
    arrange(date)
  
  svccase_ratio_all <- case_total_all %>%
    left_join(svc_total_all) %>% 
    mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
    arrange(desc(ratio)) %>%
    rename('Total Cases' = 'case_total',
           'Total SVCs' = 'svc_total') %>%
    arrange(date)
  
  p <- ggplot(svccase_ratio_all, aes(date, ratio)) + 
    geom_bar(stat='identity',aes(fill=ratio)) +
    scale_x_date(date_labels = '%m-%Y',date_breaks = '1 month', name = 'Month') +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),legend.position = 'none')
  
  ggplotly(p)
}

totalSVCCasePlot <- function(filter_var) {
  
  svc_total_all <- svc_report %>% 
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(svc_total = 10*n()) %>% 
    select(date, svc_total) %>% 
    distinct() %>% 
    arrange(date) 
  
  case_total_all <- case_raw %>%
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(case_total = n()) %>% 
    select(date, case_total) %>% 
    distinct() %>% 
    arrange(date)
  
  svccase_all <- case_total_all %>%
    left_join(svc_total_all) %>% 
    arrange(date)
  
  p <- ggplot(svccase_all, aes(date)) + 
    geom_line(aes(y= svc_total, colour = 'svc_total')) +
    geom_line(aes(y= case_total, colour = 'case_total')) +
    scale_x_date(date_labels = '%m-%Y',date_breaks = '1 month', name = 'Month') +
    ylab('') +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),legend.position = 'none')
  
  ggplotly(p)
}
avgRatio <- function(filter_var) {
  svc_total_all <- svc_report %>% 
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(svc_total = n()) %>% 
    select(date, svc_total) %>% 
    distinct() %>% 
    arrange(date) 
  
  case_total_all <- case_raw %>%
    filter(Team == filter_var | filter_var == dummy) %>%
    group_by(date) %>%
    mutate(case_total = n()) %>% 
    select(date, case_total) %>% 
    distinct() %>% 
    arrange(date)
  
  avg_ratio_all <- case_total_all %>%
    left_join(svc_total_all) %>% 
    mutate(ratio = round(svc_total/case_total*100, digits=3)) %>%
    arrange(desc(ratio)) %>%
    rename('Cases' = 'case_total',
           'SVCs' = 'svc_total') %>%
    arrange(date) %>%
    ungroup() %>%
    summarise(across(everything(),mean)) %>%
    select(Cases, SVCs)

}

#top resolution plot
topResLastMonth <- function(filter_var) {
  topRes <- svc_report %>% 
    filter((Team == filter_var | filter_var == dummy),
           !is.na(subresolution),
           date == max(date)) %>%
    mutate(totalSVC = n()) %>%
    group_by(subresolution) %>%
    mutate(total = n(),
           percent = total/totalSVC*100) %>%
    select(subresolution, percent) %>%
    distinct() %>%
    arrange(desc(percent)) %>%
    ungroup() %>% 
    top_n(10)
  
  p <- ggplot(topRes, aes(x=reorder(subresolution, -percent),y=percent)) + 
    geom_col(aes(fill=percent)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=3)) +
    xlab('') +
    theme(legend.position = 'none')
  ggplotly(p)
}

 # count by Subresolution by month
resBreakdown <- function(filter_var) {
  svc_total_by_subresolution <- svc_report %>% 
    filter(Team == filter_var | filter_var == dummy) %>%
    mutate(subresolution = case_when(is.na(subresolution) ~ 'Not Coded',
                                     is.na(resolution) ~ 'Not Coded',
                                     TRUE ~ subresolution)) %>% 
    group_by(resolution, subresolution, date) %>%
    summarise(total = n()) %>%
    mutate(date = format(date, '%Y-%m')) %>% 
    #arrange(date) %>%
    spread(date, total) %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    mutate(Total = sum(c_across(where(is.numeric)))) %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    arrange(desc(Total)) %>%
    filter(resolution != 'NA', subresolution != 'Not Coded')
  
}