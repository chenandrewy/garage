# compare to Ken French's data
# Andrew Chen 2021 04

library(tidyverse)
library(data.table)

dir.create('temp/')

# DOWNLOAD AND PROCESS French data (ff) ====
## download monthly
ffweb = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_BE-ME_CSV.zip'
download.file(ffweb,'temp/deleteme.zip')
unzip('temp/deleteme.zip', exdir = 'temp')

ffweb2 = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip'
download.file(ffweb2,'temp/deleteme.zip')
unzip('temp/deleteme.zip', exdir = 'temp')

# Equal Weight returns begin on line 1165 
ff1 = read.csv('temp/Portfolios_Formed_on_BE-ME.csv', skip=1164, nrows = 2302 - 1164 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yearm = X, ret = Hi.20 - Lo.20, port = 'ff_ew5'
  )

# VW returns
ff2 = read.csv('temp/Portfolios_Formed_on_BE-ME.csv', skip=23, nrows = 1161 - 23 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yearm = X, ret = Hi.20 - Lo.20, port = 'ff_vw5'
  ) 

# HML
ff3 = read.csv('temp/F-F_Research_Data_Factors.csv', skip=3, nrows = 1141 - 3 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yearm = X, ret = HML, port = 'ff_hml'
  )

ff = rbind(ff1,ff2,ff3)

# DOWNLOAD AND PROCESS OPENAP DATA (cz)====

# April 2021 PredictorPortsFull.csv paths:
# https://drive.google.com/file/d/10YTTmjqsXMB_gwmUyYiRPfATmh0fv8dr/view?usp=sharing
# https://drive.google.com/uc?export=download&id=10YTTmjqsXMB_gwmUyYiRPfATmh0fv8dr

download.file(
  url = 'https://drive.google.com/uc?export=download&id=10YTTmjqsXMB_gwmUyYiRPfATmh0fv8dr'
  , destfile = 'temp/temp.csv'
)
raw = fread('temp/temp.csv')

cz = raw %>% 
  filter(
    signalname == 'BMdec', port == 'LS'
    ) %>% 
  transmute(
    yearm = year(as.Date(date))*100 + month(as.Date(date))
    , ret
    , port = 'cz_ew5'
  ) %>% 
  as_tibble()


# MERGE AND CHECK ====
both = rbind(ff,cz) 


info = data.frame(
  levels      = c('ff_hml', 'ff_vw5', 'ff_ew5', 'cz_ew5')
  , labels    = c('Ken French HML (FF93)', 'Ken French VW', 'Ken French EW','OpenAP (a la FF92)')
  , colors    = c('gray', 'black','blue','red')
  , linetypes = c('solid', 'dashed','solid','twodash')
  , rank      = c(1,4,3,2)
) %>% arrange(rank)

plotme = both %>% 
  mutate(
    date = as.Date(as.character(yearm*100+28), '%Y%m%d')
  ) %>% 
  mutate(
    port = factor(port, levels = info$levels, labels = info$labels)
  )  

# plot close up
datebegin = as.Date('2019-01-01')
ggplot(
  plotme %>% 
    filter(date >= datebegin, date <= '2020-12-31') %>% 
    group_by(port) %>% arrange(date) %>% 
    mutate(
      ret = if_else(abs(date - datebegin) <= 31 , 0, ret)
      , cret = 100*(cumprod(1+ret/100) - 1)
    )
  , aes(x=date, y=cret, group=port)) +
  geom_line(aes(linetype = port, color = port), size = 0.75) +
  ylab('Cumulative Return on Value (%)')+
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_blank()
    , legend.position = c(0.25, 0.25)
    , legend.background = element_rect(fill='white')
  )  +
  scale_color_manual(
    breaks = info$labels
    , values= info$colors
  ) +
  scale_linetype_manual(
    values = info$linetypes
  ) 

temp = 1
ggsave(filename = paste0("temp/openap_vs_french_close.png")
       , width = 10*temp, height = 6*temp)



# plot close up
datebegin = as.Date('1980-01-01')
ggplot(
  plotme %>% 
    filter(date >= datebegin, date <= '2020-12-31') %>% 
    group_by(port) %>% arrange(date) %>% 
    mutate(
      ret = if_else(abs(date - datebegin) <= 31 , 0, ret)
      , cret = cumprod(1+ret/100) - 1
    )
  , aes(x=date, y=cret, group=port)) +
  geom_line(aes(linetype = port, color = port), size = 0.75) +
  ylab('Cumulative Return on Value')+
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_blank()
    , legend.position = c(0.3, 0.7)
    , legend.background = element_rect(fill='white')
  )  +
  scale_color_manual(
    breaks = info$labels
    , values= info$colors
  ) +
  scale_linetype_manual(
    values = info$linetypes
  ) 

temp = 1
ggsave(filename = paste0("temp/openap_vs_french_long.png")
       , width = 10*temp, height = 6*temp)

