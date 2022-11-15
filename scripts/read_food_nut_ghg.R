library(tidyverse)
library(readxl)
library(janitor)

food<-read_excel('data/mccance_6_nutrients.xlsx', sheet=1) %>% clean_names()
om<-read_excel('data/mccance_6_nutrients.xlsx', sheet=2) %>% clean_names() %>% select(food_code, omega_3_g)

## join omega and micronutrient values
food<-left_join(food, om, by = 'food_code') %>% 
      mutate(across(calcium_mg:omega_3_g, as.numeric)) %>% 
      mutate(across(calcium_mg:omega_3_g, ~ replace_na(., 0)))
      

## filter foods of interest (from master excel)
focs<-c('12-937', ## eggs whole raw
        '18-488', ## Chicken, meat, average, raw
        '18-267', ## Pork, mince, raw
        '18-064', ## Beef, sirloin steak, raw, lean
        '18-481', ## Lamb, mince, raw
        '14-319', ## Apples, eating, raw, flesh and skin
        '14-318', ## Bananas, flesh only
        '13-517', ## Tomatoes, standard, raw
        '11-788', ## 	Porridge oats, unfortified
        '11-861', ## Rice, white, long grain, raw
        '13-489', ## Potatoes, old, raw, flesh only
        '12-346', ## Cheese, Cheddar, English
        '12-313' ##	Milk, semi-skimmed, pasteurised, average
        )

focs_lab<-data.frame(food_code = focs, food_lab = c('Eggs', 'Chicken', 'Pork', 'Beef', 'Lamb', 'Apples','Bananas', 'Tomatoes', 'Oats', 'Rice','Potatoes', 'Cheese', 'Milk'))

foodF<-food %>% filter(food_code %in% focs) %>% 
      left_join(focs_lab)

## join RDA vals
source('scripts/rda_reader.R')
foodF$ca_rda<-foodF$calcium_mg/rda$rni_women[rda$nutrient=='calcium']*100
foodF$fe_rda<-foodF$iron_mg/rda$rni_women[rda$nutrient=='iron']*100
foodF$se_rda<-foodF$selenium_mug/rda$rni_women[rda$nutrient=='selenium']*100
foodF$zn_rda<-foodF$zinc_mg/rda$rni_women[rda$nutrient=='zinc']*100
foodF$om_rda<-foodF$omega_3_g/rda$rni_women[rda$nutrient=='omega_3']*100

## now cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
# TY BEAL: https://twitter.com/TyRBeal/status/1344662877458886656
foodF$ca_rda[foodF$ca_rda>100]<-100
foodF$fe_rda[foodF$fe_rda>100]<-100
foodF$se_rda[foodF$se_rda>100]<-100
foodF$zn_rda[foodF$zn_rda>100]<-100
foodF$om_rda[foodF$om_rda>100]<-100

## now add prob nutrient adequacy (mean rda), and price to reach 40% nutrient adequacy
foodF <- foodF %>% rowwise() %>%
  mutate(nut_adq = mean(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda)),
         portion_adq = 40 / nut_adq * 100,
         nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda))) 

## join GHG
ghg<-read_excel('data/cline_foods.xlsx') %>% mutate(food_lab = product)

foodF<-left_join(foodF, ghg, by = 'food_lab') %>% select(-description)

write.csv(foodF, file = 'data/ghg_nutrient_other_foods.csv')
