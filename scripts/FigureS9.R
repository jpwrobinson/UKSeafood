
## read stock assessment indicators
# https://data.cefas.co.uk/view/20996
# https://jncc.gov.uk/our-work/ukbi-b2-sustainable-fisheries/#indicator-description
stock<-read.csv('data/stock_data_timeries.csv') %>% 
      filter(Year >= 1990 & Year < 2020) %>% 
      mutate(F_stat = ifelse(FishingPressure > FMSY, 'Over', 'Under'),
            B_stat = ifelse(StockSize < Blim, 'Over', 'Under'),
            F_stat = ifelse(is.na(F_stat), 'Unknown', F_stat),
            B_stat = ifelse(is.na(B_stat), 'Unknown', B_stat))

stock$group<-nut$group[match(stock$SpeciesName, nut$scientific_name)]
stock$common_name<-nut$common_name[match(stock$SpeciesName, nut$scientific_name)]
stock$F_stat<-factor(stock$F_stat, levels=c('Unknown', 'Over', 'Under'))

# ## recreate CEFAS fig
# ggplot(stock, aes(Year, fill=F_stat)) + geom_bar(position='fill')
# ggplot(stock, aes(Year, fill=B_stat)) + geom_bar(position='fill') ## B_stat mostly unknown

## check only species in nut/ghg
g2<-ggplot(stock %>% filter(SpeciesName %in% nut$scientific_name & group!='Salmons, trouts, smelts'), aes(Year, fill=F_stat)) + 
    geom_bar(position='fill') +
    facet_wrap(~common_name, nrow=1) +
    labs(x = '', y ='% stocks of UK interest') +
    scale_fill_manual(values = rev(c('#4daf4a', '#e41a1c', '#999999')), labels=c( 'Unknown', 'F > Fmsy', 'F < Fmsy')) + 
    scale_x_continuous(breaks=seq(1990, 2019, by = 5), expand=c(0,0)) + 
    scale_y_continuous(labels=scales::percent, expand=c(0,0)) + th +
    theme(legend.position = 'right', axis.text.x = element_text(size=9, angle=0, hjust=0.5, vjust=0.5),
            axis.ticks = element_line(colour='black'),
            strip.text.x = element_text(colour='black', size=11, face=1))

g3<-ggplot(stock %>% filter(SpeciesName %in% nut$scientific_name & group!='Salmons, trouts, smelts'), aes(Year, fill=B_stat)) + 
    geom_bar(position='fill') +
    facet_wrap(~common_name, nrow=1) +
    labs(x = '', y ='% stocks of UK interest')  +
    scale_fill_manual(values = rev(c('#e41a1c','#4daf4a', '#999999')), labels=c( 'Unknown', 'B > Blim', 'B < Blim')) + 
    scale_x_continuous(breaks=seq(1990, 2019, by = 5), expand=c(0,0)) + 
    scale_y_continuous(labels=scales::percent, expand=c(0,0)) + th +
    theme(legend.position = 'right', axis.text.x = element_text(size=9, angle=0, hjust=0.5, vjust=0.5),
            axis.ticks = element_line(colour='black'),
            strip.text.x = element_blank())

pdf(file = 'fig/FigureS9.pdf', height = 7, width=12)
print(plot_grid(g2, g3, labels=c('A', 'B'), nrow=2))
dev.off()
