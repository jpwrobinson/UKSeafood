library(tidyverse)

## sup tables

# 1 GHG estimates
load('data/nutrient_ghg_species.rds')

## setup fish groups of interest
cats<-data.frame(isscaap = unique(all$group))
cats$group<-c('Whitefish', 'Tuna', 'Pelagic (large)', 'Bivalve', 'Pelagic (small)', 'Freshwater fish', 'Salmonidae', NA,
                'Crustacean', 'Bivalve', 'Freshwater fish', 'Cephalopod', 'Whitefish', NA, 'Tilapia', 'Crustacean', 'Crustacean', 
              'Bivalve', NA, 'Bivalve', 'Crustacean','Crustacean')

all$group2<-cats$group[match(all$group, cats$isscaap)]

all<-all %>% mutate(nut_score = round(nut_score, 0)) %>% select(group2, common_name:mid, nut_score)
colnames(all)<-c('Species group', 'Common name', 'Scientific name', 'Wild-caught or farmed','Edible fraction', 'Low GHG', 'High GHG', 'Median GHG', '5-nutrient density, %')
write.csv(all, file = 'Table_S1.csv')

# 2 UK products and production method
drops<-c('Other marine fish')
prod<-data.frame(
common_name = c('Alaska pollock', 'Atlantic cod', 'Haddock', 'Atlantic herring', 
	'Atlantic mackerel', 'Norway Lobster', 'Atlantic salmon', 'Queen scallop', 
	'Blue mussel', 'Shrimp, miscellaneous', 'Shrimp, warmwater', 'Rainbow trout',
	 'Skipjack tuna','Sea trout'),
method = c('Midwater trawl','Bottom trawl','Bottom trawl','Purse seine',
	'Purse seine','Bottom trawl','Net pen','Dredge','Longline',
	'Bottom trawl or intensive ponds','Bottom trawl or intensive ponds',
	'Raceway','Purse seine','Raceway'))


uk<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
		filter(top90 == TRUE & !species %in% drops & !is.na(mid))  %>% 
		left_join(prod) %>% 
		rowwise() %>% 
		mutate(nut_score2 = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, iodine_rda, vita_rda, vitd_rda, vitb12_rda, folate_rda)),
       			 nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda))) %>% 
		select(common_name, scientific_name, farmed_wild, method, low, max, mid, tot, prop_imported, apparent_consumption, nut_score, nut_score2)  %>% 
		mutate_at(vars(low:mid), ~round(., 1))  %>% 
		mutate_at(vars(tot, apparent_consumption:nut_score2), ~round(., 0))  %>% 
		mutate(prop_imported = round(prop_imported, 2))

colnames(uk)<-c('Common name', 'Scientific name', 'Wild-caught or farmed', 'Dominant production method', 'Low GHG', 'High GHG', 'Median GHG', 'Annual production, tonnes',
	'Proportion imported, %', 'Annual apparent consumption, tonnes', '5-nutrient density, %', '10-nutrient density, %')

write.csv(uk, file = 'Table_S2.csv')
