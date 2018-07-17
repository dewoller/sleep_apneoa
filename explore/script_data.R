library(tidyverse)
library(lubridate)

opioid_scripts <- read.csv("C:/Users/User/Desktop/opioid_scripts.csv")
View(opioid_scripts)

ggplot(opioid_scripts) +
  geom_point(mapping=aes(x=spply_dt,y=quantity))

ggplot(opioid_scripts) +
  geom_point(mapping=aes(x=spply_dt,y=atc_meaning))

opioid_scripts %>%
 as.tibble() %>%
   mutate(ym=paste(year(spply_dt), month(spply_dt))) %>%
  group_by( ym, atc_meaning ) %>%
  summarise( quantity = sum(quantity)) %>%
  ggplot() +
  geom_point(mapping=aes(x=ym,y=quantity, color=atc_meaning))

  

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), month(spply_dt))) %>%
  filter( atc_meaning == 'Pethidine' | atc_meaning=='Tapentadol') %>%
  group_by( ym, atc_meaning ) %>%
  summarise( quantity = sum(quantity)) %>%
  ggplot() +
  geom_point(mapping=aes(x=ym,y=quantity, color=atc_meaning))

