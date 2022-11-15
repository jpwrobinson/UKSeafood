pacman::p_load(ggrepel, tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/00_plotting.R')


load('data/nutrient_ghg_species.rds')
all<-all %>% rowwise() %>% 
      mutate(edible_fraction = edible_fraction / 100,
             n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
             nt_co2 = mid / edible_fraction / n_targets / 10) %>% ## estimate the CO2 equivalent per RDA target
      ungroup() %>% droplevels() %>% 
      mutate(common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2))))

## setup fish groups of interest
cats<-data.frame(isscaap = unique(all$group))
cats$group<-c('Whitefish', 'Tuna', 'Pelagic (large)', 'Bivalve', 'Pelagic (small)', 'Freshwater fish', 'Salmonidae', NA,
              'Crustacean', 'Bivalve', 'Freshwater fish', 'Cephalopod', 'Whitefish', NA, 'Tilapia', 'Crustacean', 'Crustacean', 
              'Bivalve', NA, 'Bivalve', 'Crustacean','Crustacean')

all$group2<-cats$group[match(all$group, cats$isscaap)]

## estimate mean nt_co2 by group
groups<-all %>% group_by(group2) %>% 
  summarise(nt_co2 = mean(nt_co2), n_targets = mean(n_targets)) %>% 
  mutate(product = group2) %>% 
  filter(!is.na(group2))

## now UK production focus only
drops<-c('Other marine fish')

nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) 

## replace ghg with dominant production values
ghg_w<-read.csv(file = 'data/ghg_uk_dominant_production_method.csv') %>% 
    mutate(species = uk_name)
nut<-nut %>% left_join(ghg_w, by = c('species', 'farmed_wild', 'scientific_name'))

## save the new GHG values where they exist
nut$mid<-nut$mid.y
nut$low<-nut$low.y
nut$max<-nut$max.y

nut$mid[is.na(nut$mid.y)]<-nut$mid.x[is.na(nut$mid.y)]
nut$low[is.na(nut$low.y)]<-nut$low.x[is.na(nut$low.y)]
nut$max[is.na(nut$max.y)]<-nut$max.x[is.na(nut$max.y)]

## estimate kg CO2 per nutrient target
nut<-nut %>% 
  rowwise() %>%
  mutate(edible_fraction = edible_fraction / 100,
         n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda,  vita_rda, vitd_rda, vitb12_rda, folate_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = mid / edible_fraction / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2))))

nut$group2<-cats$group[match(nut$group, cats$isscaap)]

## estimate mean nt_co2 by group
groups_uk<-nut %>% group_by(species) %>% 
  summarise(nt_co2 = mean(nt_co2), n_targets = mean(n_targets)) %>% 
  mutate(product = species) %>% 
  filter(!is.na(species))

## read other foods
food<-read.csv('data/ghg_nutrient_other_foods.csv') 
## correct foods to the farm gate (-processing - packaging - transport to distribution centre)
food$median <- food$median - 0.59 - 0.05 - 0.09

food<- food  %>%  rowwise() %>% 
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
         nt_co2 = median / n_targets / 10 ) %>% ## estimate the median CO2 equivalent per RDA target (correct kg to 100g)
  ungroup() %>% droplevels() %>% 
  filter(product %in% c('Chicken', 'Pork', 'Beef', 'Lamb')) ## take ASMs only

## Global products (Fig 1C)
fig_dat<-rbind(groups %>% select(product, nt_co2, n_targets),
               food %>% select(product, nt_co2, n_targets)) %>% 
  filter(n_targets > 0) %>% 
  mutate(product = factor(product, levels = levels(fct_reorder(product, nt_co2))))

gmain<-ggplot(fig_dat, aes(product, nt_co2, fill = n_targets)) + 
  geom_segment(aes(xend = product, y = -Inf, yend = nt_co2), col='#636363') +
  geom_point(size=3.5, col='black', pch=21) +
  coord_flip() +
  scale_x_discrete(limits=levels(fig_dat$product)) +
  scale_y_continuous(expand=c(0.03,0)) +
  labs(x = '', y = expression(paste(kg~CO[2],'-',eq~per~nutrient~target)), fill='# nutrient targets (NT)') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(limits=c(1, 4),breaks=c(1,2,3,4), palette='RdYlGn',direction=1) +
  scale_color_distiller(limits=c(1, 4),breaks=c(1,2,3,4), palette='RdYlGn',direction=1) +
  guides(color='none') +
  th +
  theme(legend.position = c(0.8, 0.4), axis.text.y = element_text(size=11), 
        legend.title=element_text(size=10, colour='black'),
        plot.subtitle = element_text(size =11, colour='black'),
        panel.grid.major.x=element_line(size=0.2, colour='grey'))


## UK products (Fig S5)
fig_dat_uk<-rbind(groups_uk %>% select(product, nt_co2, n_targets),
               food %>% select(product, nt_co2, n_targets)) %>% 
  filter(n_targets > 0) %>% 
  mutate(product = factor(product, levels = levels(fct_reorder(product, nt_co2))))

guk<-ggplot(fig_dat_uk, aes(product, nt_co2, fill = n_targets)) + 
  geom_segment(aes(xend = product, y = -Inf, yend = nt_co2), col='#636363') +
  geom_point(size=3.5, col='black', pch=21) +
  coord_flip() +
  scale_x_discrete(limits=levels(fig_dat_uk$product)) +
  scale_y_continuous(expand=c(0.03,0)) +
  labs(x = '', y = expression(paste(kg~CO[2],'-',eq~per~nutrient~target)), fill='# nutrient targets (NT)') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(limits=c(1, 5),breaks=c(1,2,3,4,5), palette='RdYlGn',direction=1) +
  scale_color_distiller(limits=c(1, 5),breaks=c(1,2,3,4,5), palette='RdYlGn',direction=1) +
  guides(color='none') +
  th +
  theme(legend.position = c(0.8, 0.4), axis.text.y = element_text(size=11),
        legend.title=element_text(size=10, colour='black'),
        plot.subtitle = element_text(size =11, colour='black'),
        panel.grid.major.x=element_line(size=0.2, colour='grey'))


## Figure S3 - all species in the Dal database, CO2 per NT
gl<-ggplot(all, aes(common_name, nt_co2)) + 
  # geom_segment(aes(xend = common_name, y = -Inf, yend = nt_co2), col='#636363') +
  geom_point(data = all %>% filter(farmed_wild == 'Wild'), aes(fill = n_targets),size=3, col='black', pch=21) +
  geom_point(data = all %>% filter(farmed_wild != 'Wild'), aes(col = n_targets),size=3, pch=19) +
  coord_flip() +
  scale_x_discrete(limits=levels(all$common_name)) +
  scale_y_continuous(expand=c(0.01,0)) +
  labs(x = '', y = expression(paste(kg~CO[2],'-',eq~per~nutrient~target)), fill='# nutrient targets (NT)') +
  # scale_shape_manual(values = c(21, 19)) +
  scale_fill_distiller(limits=c(1, 4),breaks=c(1,2,3,4), palette='RdYlGn',direction=1) +
  scale_color_distiller(limits=c(1, 4),breaks=c(1,2,3,4), palette='RdYlGn',direction=1) +
  guides(color='none') +
  th +
  theme(legend.position = c(0.8, 0.4), 
    panel.grid.major=element_line(size=0.2, colour='grey'),
    panel.grid.minor.x=element_line(size=0.1, colour='grey'),
      axis.text.y = element_text(size=9), legend.title=element_text(size=10))
