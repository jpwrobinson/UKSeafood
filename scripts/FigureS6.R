source('scripts/00_plotting.R')

## nut ghg for axis order
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

## available seafood
nutS<-read.csv('data/UK_GHG_nutrient_catch_bysector.csv') %>% 
  filter(species %in% nut$species) %>%
  group_by(species, farmed_wild, tot, catch, source) %>% 
  mutate(species=factor(species), id = paste0(species, ' (', farmed_wild, ')')) %>% 
  mutate(source = case_when(
      str_detect(source, 'farmed') ~ 'Farmed (UK)',
        str_detect(source, 'wild') ~ 'Wild (UK)',
        str_detect(source, 'imported') ~ 'Imported'))

nutS$source<-factor(nutS$source, levels=unique(nutS$source)[c(2,1,3)])
nutS$class<-nut$class[match(nutS$species, nut$species)]
nutS<-nutS %>% group_by(species, class, source, tot, mid) %>% summarise(catch = sum(catch)) %>% 
		group_by(species) %>% 
		summarise(tot = unique(tot))

## read export data
ex<-readxl::read_excel('data/uk_export.xlsx') %>% clean_names() %>% 
	pivot_longer(-c(species, indicator), names_to = 'year', values_to='value') %>% 
	mutate(year = as.numeric(str_replace_all(year, 'x', ''))) %>% 
	filter(indicator == 'live_weight_tonnes' & year == 2019) %>% 
	group_by(species, indicator) %>% 
	summarise(value = mean(value))

ex$available<-nutS$tot[match(ex$species, nutS$species)]
ex$prop_export<-ex$value / ex$available * 100

## read apparent consumption
ac<-read.csv( file = 'data/UK_GHG_nutrient_catch.csv') %>% 
  filter(species %in% nut$species) %>% 
  distinct(species, apparent_consumption, tot) %>% 
  mutate(prop_ac = apparent_consumption / tot * 100)

## order according to nut GHG
ex$species<-factor(ex$species, levels=levels(fct_reorder(nut$species, nut$mid)[!duplicated(fct_reorder(nut$species, nut$mid))]))
ac$species<-factor(ac$species, levels=levels(fct_reorder(nut$species, nut$mid)[!duplicated(fct_reorder(nut$species, nut$mid))]))

## 3. production 
gg<-ggplot(ex, aes(value, species)) +
      geom_bar(stat = 'identity', fill='grey50') +
      labs(x =  'exported seafood in 2019, t', y = '', parse=TRUE) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06), breaks=c(0, 25000, 50000, 75000, 100000,125000,150000)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            # axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.y = element_blank())


gg1<-ggplot(ex, aes(prop_export, species)) +
      geom_bar(stat = 'identity', fill='grey50') +
      labs(x = 'proportion of available seafood exported in 2019, %', y = '', parse=TRUE) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.y = element_blank())

gg2<-ggplot(ac, aes(prop_ac, species)) +
      geom_bar(stat = 'identity', fill='grey50') +
      labs(x = 'domestic use relative to total production in 2019, %', y = '', parse=TRUE) +
      scale_x_continuous(labels=scales::comma, expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(), strip.text.y = element_blank())

pdf(file = 'fig/FigureS6.pdf', height=5, width=15)
print(plot_grid(gg, gg1,gg2, nrow = 1, labels=c('A', 'B', 'C'), align='h', rel_widths=c(1, 0.7, 0.7)))
dev.off()