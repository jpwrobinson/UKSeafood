source('scripts/00_plotting.R')


drops<-c('Other marine fish')
nut<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
  mutate(prop_tot = tot / all * 100, 
    class = ifelse(prop_tot >= 5, 'High production', 'Low production')) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>% 
  rowwise() %>% 
  ## redo nut_score
  mutate(nut_score = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, iodine_rda, vita_rda, vitd_rda, vitb12_rda, folate_rda)),
        nut_score2 = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda))) %>% 
  group_by(species, farmed_wild, tot, class) %>% 
  summarise_at(vars(low:nut_score2, vitamin_d:iodine_rda), mean) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')'))

## replace ghg with dominant production values
ghg_w<-read.csv(file = 'data/ghg_uk_dominant_production_method.csv') %>% 
    mutate(species = uk_name)
nut<-nut %>% left_join(ghg_w, by = c('species', 'farmed_wild'))

## save the new GHG values where they exist
nut$mid<-nut$mid.y
nut$low<-nut$low.y
nut$max<-nut$max.y

nut$mid[is.na(nut$mid.y)]<-nut$mid.x[is.na(nut$mid.y)]
nut$low[is.na(nut$low.y)]<-nut$low.x[is.na(nut$low.y)]
nut$max[is.na(nut$max.y)]<-nut$max.x[is.na(nut$max.y)]

nutS<-read.csv('data/UK_GHG_nutrient_catch_bysector.csv') %>% 
  filter(species %in% nut$species) %>%
  # select(-tax) %>% 
  group_by(species, farmed_wild, tot, catch, source) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')')) %>% 
  mutate(source = case_when(
      str_detect(source, 'farmed') ~ 'Farmed (UK)',
        str_detect(source, 'wild') ~ 'Wild (UK)',
        str_detect(source, 'imported') ~ 'Imported'))

nutS$source<-factor(nutS$source, levels=unique(nutS$source)[c(2,1,3)])
nutS$class<-nut$class[match(nutS$species, nut$species)]
nutS<-nutS %>% group_by(species, class, source, tot, mid) %>% summarise(catch = sum(catch))

nutS$species<-factor(nutS$species, levels=levels(fct_reorder(nut$species, nut$tot)[!duplicated(fct_reorder(nut$species, nut$tot))]))

## read apparent consumption
ac<-read.csv( file = 'data/UK_GHG_nutrient_catch.csv') %>% 
  filter(species %in% nutS$species) %>% 
  mutate(apparent_consumption = apparent_consumption * edible_fraction/100) %>%
  distinct(species, apparent_consumption)

nutS$apparent_consumption<-ac$apparent_consumption[match(nutS$species, ac$species)]

## 1. GHG
g1<-ggplot(nut, aes(mid, fct_reorder(species, tot), col=farmed_wild)) + 
  geom_segment(aes(x = low, xend = max, y =  fct_reorder(species, tot), yend= fct_reorder(species, tot))) +
  geom_point(data = nut %>% filter(species != 'Sea mussels'),aes(x = mid, y =  fct_reorder(species, tot)), size=3,) +
  geom_point(data =nut %>% filter(species == 'Sea mussels'),
    aes(x = mid, y =  fct_reorder(species, tot)), size=3, position = position_dodge(width=0.5)) +
  labs(x = expression(paste(kg~CO[2],'-',eq)), y ='') +
  facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
  scale_colour_manual(values = colcol2) +
  # scale_y_discrete(labels=nut$species[as.factor(levels(fct_reorder(nut$id, nut$mid)))]) +
  scale_x_continuous(expand=c(0.01, 0.01)) +
  th+ 
  theme(legend.position = c(0.8, 0.1), panel.grid.major.x = element_line(size=0.2, colour ='grey'),
    legend.title=element_blank(), strip.text.y = element_blank())

nut3<-nut %>% ungroup() %>% group_by(species, class) %>% 
    summarise_at(vars(ca_rda:om_rda, vita_rda:iodine_rda), mean) %>%   
    pivot_longer(ca_rda:iodine_rda, names_to = 'nutrient', values_to = 'rda') 

nut3$nutrient<-factor(nut3$nutrient, levels = rev(unique(nut3$nutrient)[c(8,3,5,10, 7,4,2,1,9,6)]))
nut3$lab<-nut3$nutrient; levels(nut3$lab) <-rev(c('Vitamin B12', 'Selenium','Omega-3','Iodine', 'Vitamin D', 'Zinc', 'Iron','Calcium','Folate', 'Vitamin A')) 
nut3$species<-factor(nut3$species, levels=levels(fct_reorder(nut$species, nut$tot)[!duplicated(fct_reorder(nut$species, nut$tot))]))

nut_plot<-nut3 %>% group_by(species) %>% 
  arrange(factor(nutrient, levels = rev(levels(nutrient))), .by_group=TRUE) %>% 
  mutate(label_ypos=cumsum(rda) - 0.5*rda) %>% 
  mutate(lab = recode(lab, 'Calcium'='misc', 'Vitamin A' = 'misc', 'Folate' = 'misc')) %>% 
  group_by(species, class, lab) %>% 
  summarise(rda = sum(rda), label_ypos = mean(label_ypos))
  

nut4<-nut3 %>% filter(lab %in% c('Calcium', 'Iron', 'Selenium', 'Zinc', 'Omega-3')) %>% 
  droplevels() %>% 
  group_by(species) %>% 
  arrange(factor(nutrient, levels = rev(levels(nutrient))), .by_group=TRUE) %>% 
  mutate(label_ypos=cumsum(rda) - 0.5*rda) %>% 
  mutate(lab = recode(lab, 'Calcium'='Other (Ca + Vit-A)', 'Vitamin A' = 'Other (Ca + Vit-A)')) 


## 2. nutrient density
g2<-ggplot(nut_plot, aes(rda, species, fill=lab)) + 
  geom_bar(stat='identity',col='white', size=0.1) +
  geom_text(data = nut_plot %>% filter(rda >= 15 & lab != 'misc'),
            aes(x = label_ypos, label= paste0(round(rda, 0), '%')),  color="white", size=2) +
  labs(x = 'Nutrient density, %', y ='') +
  facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
  scale_fill_manual(values = nut.cols2) +
  scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
  th+ 
  theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
        axis.ticks = element_blank(), 
        legend.position = 'none', axis.text.y = element_blank(), 
        axis.title.y = element_blank(), strip.text.y = element_blank())

## 2B. nutrient density (5 nut version)
gS<-ggplot(nut4, aes(rda, species,fill=lab)) + 
  geom_bar(stat='identity', col='white' ,size=0.1) +
  geom_text(data = nut4 %>% filter(rda >= 15),
            aes(x = label_ypos, label= paste0(round(rda, 0), '%')),  color="white", size=3) +
  labs(x = 'Nutrient density, %', y ='') +
  facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
  scale_fill_manual(values = nut.cols3) +
  scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
  th+ 
  theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
        axis.ticks = element_blank(), 
        legend.position = 'right', 
        # axis.text.y = element_blank(), 
        axis.title.y = element_blank(), strip.text.y = element_blank())

## 3. production 
g3<-ggplot(nutS) +
      geom_bar(stat = 'identity',aes(catch, species, fill=source)) +
      geom_errorbarh(col='#E90600',
        aes(xmin = apparent_consumption,xmax = apparent_consumption, y=species)) +
      labs(x = expression(tonnes~yr^{'-1'}), y = '', parse=TRUE) +
      # facet_grid(cols = vars(source), scales='free_y', space = 'free_y') + 
      facet_grid(rows = vars(class), scales='free_y', space = 'free_y') + 
      scale_fill_manual(values = colcol3) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = c(0.85,0.5), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.y = element_blank())


top<-plot_grid(g1, g3, g2, nrow = 1, align = 'h', 
  rel_widths=c(0.9, 0.6, 0.8), labels=c('A', 'B', 'C'), hjust=0)

pdf(file = 'fig/Figure2.pdf', height=5, width=13)
print(top)
dev.off()


pdf(file = 'fig/FigureS7.pdf', height=5, width=9)
print(gS)
dev.off()