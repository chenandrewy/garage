# 2021 03 Andrew: checking comparison with the RAPS paper

library(tidyverse)
library(lubridate)


# RAPS DATA https://drive.google.com/open?id=0ByAFEgH1yFDCU3dZNWRXb0xZdFU
old0 = readr::read_csv('https://drive.google.com/uc?export=download&id=0ByAFEgH1yFDCU3dZNWRXb0xZdFU')

names(old0)[1:10]

old = old0 %>% transmute(yearm = Year*100+Month,BM)

# NEW DATA https://drive.google.com/uc?export=download&id=1cEXQ9ko2ZPoC-X1GICE-0-Q_WX7PqxZL
new0 = readr::read_csv('https://drive.google.com/uc?export=download&id=1cEXQ9ko2ZPoC-X1GICE-0-Q_WX7PqxZL')


names(new0)[1:10]

new = new0 %>% transmute(yearm = year(date)*100+month(date), BMnew = BM)


# FRENCH DATA: equal weight monthly
? read.csv
ff0 = read.csv('Portfolios_Formed_on_BE-ME.CSV'
               , skip = 1163
               , nrows = 2300-1163
               , stringsAsFactors = F) %>% as_tibble()


ff = ff0 %>% transmute(yearm = X, Lo.20, Hi.20)  %>%
  mutate_all(.funs=as.numeric) %>%
  transmute(yearm, BMff = Hi.20-Lo.20)

all = full_join(old,new) %>%
  left_join(ff) %>%
  arrange(yearm) %>%
  filter(!is.na(BM))

all %>% print(n=500)



