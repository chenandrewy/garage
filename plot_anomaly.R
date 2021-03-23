# 2021 03 Andrew Chen: plot a selected anomaly
rm(list = ls())
library(tidyverse)
library(lubridate)

### URLS FOR REFERENCE
# signal doc https://github.com/OpenSourceAP/CrossSection/raw/master/SignalDocumentation.xlsx
# PredictorPortsFull.csv https://drive.google.com/file/d/1L9SoIqupVDqOwzy3Vt9QD-y7W4ZOeIgy/view?usp=sharing
# Longshort ret wide 'https://drive.google.com/uc?export=download&id=1cEXQ9ko2ZPoC-X1GICE-0-Q_WX7PqxZL'

### USER ENTRY
signaltarget = 'AssetGrowth'
signalbench = 'BM'
vol = 5 # % monthly
years_presamp = 5

### IMPORT DATA

## signal doc
download.file(url = "https://github.com/OpenSourceAP/CrossSection/raw/master/SignalDocumentation.xlsx", destfile = 'deleteme.xlsx', mode="wb")
signaldoc0 = readxl::read_excel('deleteme.xlsx', sheet = 'BasicInfo') 
signaldoc1 = signaldoc0 %>% 
  transmute(
    signalname = Acronym
    , pubdate = as.Date(paste0(Year, '-12-31'))
    , sampend = as.Date(paste0(SampleEndYear, '-12-31'))
  )

## long-short returns (this may take a minute)
ret0 = readr::read_csv('https://drive.google.com/uc?export=download&id=1cEXQ9ko2ZPoC-X1GICE-0-Q_WX7PqxZL')
ret0 = ret0 %>% gather(key='signalname',value='ret',-c(date))


### DO STUFF
doctarget = signaldoc1  %>% filter(signalname == signaltarget)

## standardize and accumulate
ret1 = ret0 %>%
  left_join(
    ret0 %>% group_by(signalname) %>% summarize(sd = sd(ret,na.rm=T))
  ) %>%
  mutate(retnew = ret/sd*vol) %>%
  filter(date >= doctarget$sampend - years(years_presamp) ) %>%
  group_by(signalname) %>% arrange(signalname,date) %>%
  mutate(cret = cumprod(1+retnew/100)) %>%
  ungroup

## subset and plot
plotme = ret1 %>% filter(signalname %in% c(signaltarget, signalbench)) 


# date cutoffs fix me
yloc = (max(plotme$cret)-1)/2
ggplot(plotme, aes(x=date,y=cret)) +
  geom_line(aes(linetype = signalname, color = signalname)) + 
  xlab("") + ylab('Cummulative Long-Short Return (Monthly Vol = 5%)') +
  geom_vline(xintercept = doctarget$pubdate, color = 'red') +
  geom_text(aes(x=doctarget$pubdate, label="\nPublication Date", y=yloc), colour="red", angle=90) +
  geom_vline(xintercept = doctarget$sampend, color = 'blue') +
  geom_text(aes(x=doctarget$sampend, label="\nOriginal Sample Ends", y=yloc), colour="blue", angle=90)   






