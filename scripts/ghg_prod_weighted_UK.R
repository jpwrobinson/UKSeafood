library(tidyverse)
load(file = 'data/ghg_all_dal_data.rds')

## get UK species
drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  rowwise() %>% 
  ## redo nut_score
  mutate(nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, iodine_rda, vita_rda, vitd_rda, vitb12_rda, folate_rda)),
        nut_score2 = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda))) %>% 
  group_by(species, farmed_wild, tot, class) %>% 
  summarise_at(vars(low:nut_score2, vitamin_d:iodine_rda), mean) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')'))

## add UK name to ghg
ghg<-ghg %>% mutate(uk_name = common_name)  %>% 
		mutate(uk_name = case_when(
			# str_detect(uk_name, 'shrimp|prawn') ~ 'Shrimp, miscellaneous',
			str_detect(uk_name, 'Atlantic mackerel') ~ 'Mackerel',
			str_detect(uk_name, 'Atlantic cod') ~ 'Cod',
			str_detect(uk_name, 'Skipjack tuna') ~ 'Tuna, skipjack',
			str_detect(uk_name, 'Atlantic herring') ~ 'Herring',
			str_detect(uk_name, 'Norway lobster') ~ 'Lobster, Norway',
			str_detect(uk_name, 'Queen scallop') ~ 'Scallop',
			str_detect(uk_name, 'Atlantic salmon') ~ 'Salmon',
			str_detect(uk_name, 'Sea trout') ~ 'Trout',
			str_detect(uk_name, 'Rainbow trout') ~ 'Trout',
			str_detect(uk_name, 'Saithe') ~ 'Saithe',
			str_detect(uk_name, 'Blue mussel') ~ 'Sea mussels',
			TRUE ~ uk_name)) 

shrimp<-ghg %>% filter(str_detect(common_name, 'shrimp|prawn')) %>% 
        mutate(uk_name = 'Shrimp, miscellaneous',
               common_name = 'Shrimp, miscellaneous',
               scientific_name = 'Shrimp, miscellaneous',
               group = 'Shrimps, prawns')

warmies<-c('Fenneropenaeus merguiensis', 'Penaeus esculentus', 'Macrobrachium rosenbergii', 
           'Penaeus monodon', 'Farfantepenaeus aztecus', 'Litopenaeus setiferus', 'Farfantepenaeus notialis', 'Penaeus vannamei')
cold<-c('Pleoticus muelleri', 'Pandalus borealis')

shrimp_warm<-ghg %>% filter(scientific_name %in% warmies) %>% 
  mutate(uk_name = 'Shrimp, warmwater',
         common_name = 'Shrimp, warmwater',
         scientific_name = 'Shrimp, warmwater',
         group = 'Shrimps, prawns')

## replace shrimp in ghg with new averaged values
ghg<-ghg %>% filter(!str_detect(common_name, 'shrimp'))
ghg<-rbind(ghg, shrimp %>% select(names(ghg)), shrimp_warm %>% select(names(ghg)))


## summarise GHG, weighting by production method
prod<-data.frame(
uk_name = c('Alaska pollock', 'Cod', 'Haddock', 'Herring', 
	'Mackerel', 'Lobster, Norway', 'Salmon', 'Scallop', 
	'Sea mussels', 'Shrimp, miscellaneous', 'Shrimp, warmwater', 'Trout',
	 'Tuna, skipjack','Trout'),
method = c('Midwater trawl','Bottom trawl','Bottom trawl','Purse seine',
	'Purse seine','Bottom trawl','Net pen','Dredge','Longline',
	'Bottom trawl or intensive ponds','Bottom trawl or intensive ponds',
	'Raceway','Purse seine','Raceway'))

## get gear specific vals
ghg<-ghg %>% filter(uk_name %in% nut$species) %>%  
			group_by(uk_name, scientific_name, farmed_wild, gear) %>%
			summarise(low = min(ghg_low), max = max(ghg_high)) %>%
			mutate(mid = low + ((max - low)/2))

prod$id<-paste(prod$uk_name, prod$method, sep = '_')
ghg$id<-paste(ghg$uk_name, ghg$gear, sep = '_')
ghg$dom<-prod$method[match(ghg$id, prod$id)]


write.csv(ghg %>% filter(!is.na(dom)), file = 'data/ghg_uk_dominant_production_method.csv')

