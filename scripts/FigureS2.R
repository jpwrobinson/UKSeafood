library(tidyverse)
library(readxl)
library(janitor)
theme_set(theme_bw())

load('data/nutrient_ghg_species.rds')

## include edible portion estimates
all<-all %>% mutate(edible_fraction = edible_fraction / 100,
                    mid2 = mid / edible_fraction,
                    low2 = low / edible_fraction,
                    max2 = max / edible_fraction)

## setup fish groups of interest
cats<-data.frame(isscaap = unique(all$group))
cats$group<-c('Whitefish', 'Tuna', 'Pelagic (large)', 'Bivalve', 'Pelagic (small)', 'Freshwater fish', 'Salmonidae', NA,
              'Crustacean', 'Bivalve', 'Freshwater fish', 'Cephalopod', 'Whitefish', NA, 'Tilapia', 'Crustacean', 'Crustacean', 
              'Bivalve', NA, 'Bivalve', 'Crustacean','Crustacean')
all$group2<-cats$group[match(all$group, cats$isscaap)]

groups<-all %>% group_by(group2) %>% 
  summarise(mid = mean(mid), mid2 = mean(mid2)) %>% 
  mutate(x = group2) %>% 
  filter(!is.na(group2))

# UK GHG
drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  group_by(common_name, farmed_wild, top90, species, scientific_name) %>% 
  summarise_at(vars(mid, low, max, edible_fraction), mean) %>% 
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

nut<-nut %>% mutate(edible_fraction = edible_fraction / 100,
                    mid2 = mid / edible_fraction,
                    low2 = low / edible_fraction,
                    max2 = max / edible_fraction)

plotter<-groups %>% rename('Live weight' = mid, 'Edible portion' = mid2) %>% 
  pivot_longer(-c(x, group2), names_to = 'type', values_to = 'mid')

plotter2<-nut %>% ungroup() %>% select(mid, mid2, common_name, farmed_wild) %>% 
  rename('Live weight' = mid, 'Edible portion' = mid2) %>% 
  mutate(x = paste(common_name, farmed_wild, sep = ', ')) %>% 
  pivot_longer(-c(x, common_name, farmed_wild), names_to = 'type', values_to = 'mid')


g1<-ggplot(plotter, aes(fct_reorder(group2,mid), mid, col=type)) + 
  geom_path(aes(group=group2), col='grey') +
  geom_point(size=2.5) +
  coord_flip() +
  scale_colour_manual(values = rev(c('black', '#e41a1c'))) +
  labs(x = '', y='kg CO2-eq') +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.8, 0.4), legend.title = element_blank()) +
  scale_y_continuous(breaks=seq(0, 60, 5))


g2<-ggplot(plotter2, aes(fct_reorder(x,mid), mid, col=type)) + 
  geom_path(aes(group=x), col='grey') +
  geom_point(size=2.5) +
  coord_flip() +
  scale_colour_manual(values = rev(c('black', '#e41a1c'))) +
  labs(x = '', y='kg CO2-eq') +
  theme(panel.grid.major.y = element_blank(),
        legend.position = 'none') +
  scale_y_continuous(breaks=seq(0, 60, 5))


pdf(file = 'fig/FigureS2.pdf', height=5, width=10)
print(
  cowplot::plot_grid(g1, g2, nrow=1, labels=c('A', "B"))
)
dev.off()