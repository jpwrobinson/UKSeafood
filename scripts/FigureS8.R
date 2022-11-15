source('scripts/00_plotting.R')
set.seed(43)


## good fish guide scores
gfg<-readxl::read_excel('data/GFG_Export_2022-11-09.xlsx') %>% clean_names() %>% 
  rename(common_name = species_common_name, 
         scientific_name = species_scientific_name, 
         farmed_wild = farmed_or_wild_caught) %>% 
  mutate(farmed_wild = recode(farmed_wild, 'Wild caught' = 'Wild')) %>% 
  filter(! public_rating %in% c('Under review')) %>%
  filter(!(farmed_production_methods == 'Closed system, RAS' & common_name == 'Atlantic salmon')) %>% 
  filter(!(wild_capture_methods == 'Net (gill or fixed)' & common_name == 'Skipjack tuna')) %>% 
  group_by(common_name, scientific_name, farmed_wild) %>% 
  mutate(public_rating = as.numeric(public_rating),
         farmed_wild = recode(farmed_wild, 'Caught at sea' = 'Wild'), 
         scientific_name = recode(scientific_name, 'Euthynnus pelamis, Katsuwonus pelamis' = 'Katsuwonus pelamis',
                                  'Theragra chalcogramma' = 'Gadus chalcogrammus'),
         id = paste(farmed_wild, scientific_name, sep='_'),
         total_score = as.numeric(public_rating)) %>% 
  group_by(farmed_wild) %>% 
  ## rescale ratings between 0-1
  mutate(total_score = rescale(total_score, to = c(1,0))) %>% 
  group_by(common_name, farmed_wild, scientific_name, id) %>% 
  summarise(lower = min(total_score), upper = max(total_score), total_score = median(total_score)) %>% ungroup()

# read nutrient/ghg data, join with gfg
drops<-c('Other marine fish')
nutCO<-read.csv('data/UK_GHG_nutrient_catch.csv') %>%
  mutate(edible_fraction = edible_fraction / 100) %>% 
  filter(top90 == TRUE & !species %in% drops & !is.na(mid)) %>%
  select(-tax) %>%
  rowwise() %>%
  mutate(n_targets = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda, vita_rda, vitb12_rda, vitd_rda, folate_rda) > 25),  ## estimate nutrition targets (25% RDA) for each species)
        n_targets2 = sum(c(ca_rda, fe_rda, se_rda, zn_rda, om_rda) > 25), 
         nt_co2 = mid / edible_fraction / n_targets / 10, ## estimate the CO2 equivalent per RDA target
         common_name = factor(common_name, levels = levels(fct_reorder(common_name, nt_co2)))) %>% 
  mutate(id = paste(farmed_wild, scientific_name, sep='_')) %>% 
  left_join(gfg %>% select(-farmed_wild, -common_name, -scientific_name), by = 'id') %>% 
  filter(id != 'Wild_Mytilus edulis')


g0<-ggplot(nutCO, aes(nt_co2, total_score, col=farmed_wild)) + 
        geom_point(size=3) + 
        geom_text_repel(aes(label = common_name), col='black',segment.color='grey',max.overlaps=Inf, box.padding = 1.75, size=2.5,show.legend = FALSE) +
        th +
        scale_color_manual(values = colcol2) +
        labs(y = 'Sustainability score', x  = expression(paste(kg~CO[2],'-',eq~per~nutrient~target))) +
          theme(legend.position = c(0.8, 0.75), 
            plot.margin=unit(c(0.1, 0.5, 0.1, 0.1), 'cm'), 
            axis.ticks=element_line(colour='black'),
                legend.title=element_blank()) 

g0B<-ggplot(nutCO %>% filter(!is.na(total_score)), 
        aes(fct_reorder(common_name, total_score),total_score, col=farmed_wild)) + 
        geom_pointrange(size=0.4, aes(ymin = lower, ymax = upper)) + 
        geom_point(size=3) + 
        th +
        coord_flip(clip='off') +
        scale_x_discrete(position = 'top') +
        scale_y_continuous(limits=c(0,1.1),
            sec.axis = sec_axis(~ . * 1, breaks=c(0, 1), labels=c('Less\nsustainable', 'More\nsustainable'))) +
        scale_color_manual(values = colcol2) +
        labs(y = 'Sustainability score', x  = '') +
          theme(legend.position = 'none', 
            plot.margin=unit(c(0.1, 0.1, 0.1, 1), 'cm'), 
            axis.ticks=element_line(colour='black'),
            axis.ticks.x.top=element_blank(),
                legend.title=element_blank()) 

source('scripts/Figure3.R')
nut_rad$price_key_kg<-c(16.34, 8.03, 8.54, 6.45, 9.64, 5.76, 5.08, 10.42, 24.56, 5.47, 16.12)
nutCO$price_key_kg<-nut_rad$price_key_kg[match(nutCO$common_name, nut_rad$common_name)]

g1<-ggplot(nutCO, aes(nt_co2, price_key_kg, col=farmed_wild)) + 
        geom_point(size=3) + 
        geom_text_repel(aes(label = common_name), col='black',segment.color='grey',max.overlaps=0, box.padding = 1.75, size=2.5,show.legend = FALSE) +
        th +
        scale_color_manual(values = colcol2) +
        labs(y = 'GBP per kg', x  = expression(paste(kg~CO[2],'-',eq~per~nutrient~target))) +
          theme(legend.position = c(0.8, 0.8), 
            plot.margin=unit(c(1, 1.5, 0.1, 1.5), 'cm'), 
            axis.ticks=element_line(colour='black'),
                legend.title=element_blank()) 


pdf(file = 'fig/FigureS8.pdf', height = 9, width=9)
top<-plot_grid(g0, g0B, labels=c('A', 'B'), nrow=1, rel_widths = c(1, 0.8), align='h')
print(plot_grid(top, g1, labels=c('', 'C'), nrow=2))
dev.off()
