library(tidyverse)
library(lubridate)

opioid_scripts <- read.csv("C:/Users/User/Desktop/opioid_scripts.csv")
View(opioid_scripts)
opioid_people <- read.csv("C:/Users/User/Desktop/opioid_people.csv") %>%
  as.tibble()
View(opioid_people)

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
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  #filter( atc_meaning == 'Pethidine' | atc_meaning=='Tapentadol') %>%
  group_by( ym, atc_meaning ) %>%
  summarise( quantity = sum(quantity)) %>%
  ggplot() +
  geom_point(mapping=aes(x=ym,y=quantity, color=atc_meaning))



opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  #filter( atc_meaning == 'Pethidine' | atc_meaning=='Tapentadol') %>%
  group_by( yob, atc_meaning ) %>%
  summarise( quantity = sum(quantity)) %>%
  ggplot() +
  geom_point(mapping=aes(x=yob,y=quantity, color=atc_meaning))

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  filter( atc_meaning == 'Codeine, combinations excl. psycholeptics') %>%
  group_by( yob, atc_meaning ) %>%
  summarise( quantity = sum(quantity)) %>%
  ggplot() +
  geom_point(mapping=aes(x=yob,y=quantity, color=atc_meaning))

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  distinct(pin, yob) %>%
  count(yob) %>%
  ggplot() +
  geom_point(mapping=aes(x=yob, y=n))
  
opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  distinct(pin, yob, sex) %>%
  count(yob, sex) %>%
  ggplot() +
  geom_point(mapping=aes(x=yob, y=n, colour=sex))

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  distinct(pin, yob, sex) %>%
  ggplot() +
  geom_histogram(mapping=aes(x=yob, fill=sex))

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  distinct(pin, yob, sex) %>%
  ggplot() +
  geom_histogram(mapping=aes(x=yob,fill=sex)) +
  facet_wrap(~sex)

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  distinct(pin, yob, sex) %>%
  ggplot() +
  geom_histogram(mapping=aes(x=1,fill=sex)) +
  facet_wrap(~sex)

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  distinct(pin, yob, sex) %>%
count(sex)

opioid_scripts %>%
  as.tibble() %>%
  mutate(ym=paste(year(spply_dt), sprintf("%02d",month(spply_dt)))) %>%
  inner_join(opioid_people) %>%
  distinct(pin, yob, sex, atc_meaning) %>%
  ggplot() +
  geom_histogram(mapping=aes(x=yob,fill=sex)) +
  facet_grid(atc_meaning~sex)
