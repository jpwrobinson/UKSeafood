pacman::p_load(ggrepel, tidyverse, skimr, janitor, cowplot, funk, install=FALSE)
theme_set(theme_sleek())
source('scripts/00_plotting.R')
set.seed(42)

load('data/nutrient_ghg_species.rds')

## setup fish groups of interest
cats<-data.frame(isscaap = unique(all$group))
cats$group<-c('Whitefish', 'Tuna', 'Pelagic (large)', 'Bivalve', 'Pelagic (small)', 'Freshwater fish', 'Salmonidae', NA,
                'Crustacean', 'Bivalve', 'Freshwater fish', 'Cephalopod', 'Whitefish', NA, 'Tilapia', 'Crustacean', 'Crustacean', 
              'Bivalve', NA, 'Bivalve', 'Crustacean','Crustacean')

all$group2<-cats$group[match(all$group, cats$isscaap)]


## estimate mean and range of C02 and nutrients by groups
wild_f<-all %>% group_by(farmed_wild, tax) %>% 
        summarise(se = se(nut_score), mean = mean(nut_score),
                  lower = min(low), upper = max(max), mid = mean(mid)) %>% 
        mutate(lower_nut = mean - 2*se, upper_nut = mean + 2*se, x = paste0(farmed_wild,'\n', tolower(tax)))

groups<-all %>% group_by(group2) %>% 
  summarise(lower_nut = min(nut_score),upper_nut=max(nut_score), mean = mean(nut_score),
            lower = min(low), upper = max(max), mid = mean(mid)) %>% 
  mutate(x = group2) %>% 
  filter(!is.na(group2))

## read other foods
## Nutrient and emissions for non-fish foods
source('scripts/read_food_nut_ghg.R')
food<-read.csv('data/ghg_nutrient_other_foods.csv') %>% 
      filter(product %in% c('Chicken', 'Pork', 'Beef', 'Lamb', 'Eggs', 'Milk', 'Cheese')) ## take ASFs only

## correct foods to the farm gate (-processing - packaging - transport to distribution centre)
food$median <- food$median - 0.59 - 0.05 - 0.09

# main panel: nutrient adequacy vs CO2 by species
g0<-ggplot(data=groups, aes(x = mid, y =mean)) + 
  # geom_linerange(data=groups, aes(x = mid, y =mean, xmin = lower, xmax = upper)) +
  # geom_pointrange(data=groups, aes(x = mid, y =mean, ymin = lower_nut, ymax = upper_nut), pch=21, col='black', fatten=10) +
  geom_point(col='#d73027', size=3) +
  geom_point(data =food, aes(x = median, y = nut_score), col='black', size=3) +
  geom_text_repel(data=groups, aes(x = mid, y =mean, label=x),size=3, force=1.5, point.padding=0.1) +
  geom_text_repel(data=food, aes(x = median, y =nut_score, label=food_lab),size=3, force=1.5, point.padding=0.1) +
  # geom_label(aes(label=common_name), alpha=0.5,size=2.5) +
  scale_x_continuous(breaks=seq(0, 25, by = 5)) +
  labs(x = expression(paste(kg~CO[2],'-',eq)), y = 'Nutrient density, %') +
  th + theme(
    legend.position = c(0.8, 0.9), legend.title=element_blank(),
    axis.ticks=element_line(color='black'),
    plot.margin = unit(c(5.5, 0,5.5,5.5), 'points')) +
    # guides(point = 'legend', text='none') +
  scale_fill_manual(values = colcol2)

g0B<-ggplot(all, aes(mid, nut_score)) + 
  geom_point(aes(fill=farmed_wild), size=2, alpha=0.8, pch=21, col='black') +
  # geom_label(aes(label=common_name), alpha=0.5,size=2.5) +
  labs(x = expression(paste(kg~CO[2],'-',eq)), y = 'Nutrient density, %') +
  scale_x_continuous(breaks=seq(0, 25, by = 5)) +
  th + theme(legend.position = c(0.8, 0.9), axis.ticks=element_line(color='black'),legend.title=element_blank()) +
  # guides(point = 'legend', text='none') +
  scale_fill_manual(values = colcol2)

# inset: fish vs invert and farmed vs wild
g_inset<-ggplot(wild_f, aes(x = mid, y =mean, fill=farmed_wild)) +
      geom_linerange(aes(xmin = lower, xmax = upper), col='grey') +
      geom_linerange(aes(ymin = lower_nut, ymax = upper_nut), col='grey') +
      geom_point(pch=21, col='black', size=4) +
      geom_text_repel(aes(label=x),size=2.5, force=3, point.padding = 0.2) +
      scale_fill_manual(values = colcol2) +
      # scale_x_continuous(breaks=seq(0, 25, by = 5)) +
      th + theme(legend.position = 'none', 
                 axis.ticks=element_line(color='black'),
                 plot.margin = unit(c(5.5, 5.5,5.5,1), 'points')) +
      labs(x = expression(paste(kg~CO[2],'-',eq)), y = '') 


g2<-ggplot(all, aes(mid, nut_score, fill=farmed_wild)) +
  # geom_text(aes(label=common_name))
  geom_point(size=2, pch=21, col='black') +
  geom_text_repel(aes(col=farmed_wild, label=common_name),size=1.5) +
  labs(x = expression(paste(kg~CO[2],'-',eq)), y = 'Nutrient density, %') +
  th + theme(legend.position = c(0.8, 0.8), axis.ticks=element_line(color='black'),legend.title = element_blank()) +
  guides(fill='legend', point = 'none', text='none') +
  scale_colour_manual(values = colcol2) +
  scale_x_continuous(breaks=seq(0, 25, by = 5)) +
  scale_fill_manual(values = colcol2)

source('scripts/Figure1C.R')

pdf(file = 'fig/Figure1.pdf', height=4, width=14)
print(
  plot_grid(g0, g_inset, gmain, nrow=1, labels=c('A','B', 'C'), rel_widths=c(1,1,1.2))
)
dev.off()

pdf(file = 'fig/FigureS1.pdf', height=5, width=8)
print(g2)
dev.off()

pdf(file = 'fig/FigureS3.pdf', height=12, width=8)
print(gl)
dev.off()

pdf(file = 'fig/FigureS5.pdf', height=4, width=6)
print(guk)
dev.off()

