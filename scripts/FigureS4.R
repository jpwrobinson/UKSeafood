source('scripts/00_plotting.R')


## 1. all products
all<-read.csv('data/UK_GHG_nutrient_catch.csv') %>% 
      mutate(uk = (1-prop_imported) * tot, imp = prop_imported * tot) %>% 
      select(species, tot, uk, imp, common_name, scientific_name, tax, top90) %>% 
      pivot_longer(uk:imp, names_to = 'type', values_to = 'catch') %>% 
      filter(tot > 100 & catch !=0)

tots<-all %>% distinct(species, top90, tot)  %>% 
            mutate(tc = sum(tot), t80 = cumsum(tot), pos = t80/tc, top80 = ifelse(pos <= 0.8, TRUE, FALSE))


gsup<-ggplot(all, aes(catch, fct_reorder(species, tot), col = type)) +
      geom_segment(size=0.5, aes(x= 0.01, xend = catch, yend = fct_reorder(species, tot))) +
      geom_point(size=2.5, position = position_dodge()) +
      labs(x = expression(tonnes~yr^{'-1'}), y = '', parse=TRUE) +
      scale_color_manual(values = colcol4, labels=c('UK', 'Imported')) +
      scale_x_log10(breaks=c(10^(2:5)), labels=c('100', '1,000', '10K', '100K'), limits=c(10,10e5), expand=c(0, 0.06)) +
      th+ 
      theme(plot.margin=unit(c(0.1, 0.5, 0.1, 0.5), 'cm'), 
            axis.ticks = element_blank(), 
            legend.position = c(0.9,0.4), 
            panel.grid.major.x = element_line(size=0.2, colour ='grey'),
            axis.text.y = element_text(size=9), 
            axis.title.y = element_blank())


pdf(file = 'fig/FigureS4.pdf', height=12, width=6)
print(gsup)
dev.off()