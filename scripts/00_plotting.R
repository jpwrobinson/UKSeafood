library(tidyverse)
library(funk)
library(cowplot)
theme_set(theme_sleek())

th<-theme(axis.text.y=element_text(size=11.5, colour='black'),
          axis.text.x=element_text(size=11.5, colour='black'),
          axis.line = element_line(colour = "black"), 
          axis.line.x.top = element_line(colour = "white"), 
          axis.title = element_text(size=12, colour='black'),
          panel.border = element_blank(),
          strip.text = element_text(face="bold", colour='black', size=10),
          legend.position ='none',
          legend.background=element_rect(fill='white'),
          legend.title=element_blank(),
          axis.ticks=element_blank())


colcol<-c('#17397B', '#C3840C')
colcol2<-c('Farmed' = '#C3840C', 'Wild' ='#17397B')
colcol3<-c('Farmed (UK)' = '#C3840C', 'Wild (UK)' ='#17397B', 'Imported' = 'grey')
colcol4<-c('uk' ='#17397B', 'imp' = 'grey')

nut.cols<-c('Calcium'='#de2d26', 
            'Iron'='#e6ab02', # old = 636363
            'Zinc'='#3182bd', 
            'Omega-3' = '#d95f02', #  old = F77D29
             'Selenium' = '#776EB0',
             'Iodine' = '#35978f',
            'Vitamin A'='#31a354',
            'Vitamin D' = '#fb9a99', 
            'Vitamin B12' = '#b2df8a', 
            'Folate' = '#b15928')


nut.cols2<-c(
    # minerals
    'Calcium'='#969696', # grey
    'Iron'='#e6ab02', 'Zinc'='#3182bd', 
    'Selenium' = '#776EB0',
    # fat
    'Omega-3' = '#d95f02',  # orange
    'Iodine' = '#35978f',
    # vitamins
    'Vitamin A'='#969696', # grey
    'Vitamin D' = '#31a354', # 
    'Vitamin B12' = '#fb9a99', # pink
    'Folate' = '#969696', # grey
    'Misc' = '#969696' # grey
    )

nut.cols3<-c(
    # minerals
    # 'Calcium'='#969696', # grey
    'Iron'='#e6ab02', 'Zinc'='#3182bd', 
    'Selenium' = '#776EB0',
    # fat
    'Omega-3' = '#d95f02',  # orange
    # vitamins
    'Other (Ca + Vit-A)' = '#969696' # grey
    )

nut.cols4<-c(
    # minerals
    'Iron'= 'white',
    'Selenium' = 'white',
    'Zinc'='white', 
    # fat
    'Omega-3' = 'white',
    'Iodine' = 'white',
    # vitamins
    'Vitamin D' = 'white',
    'Vitamin B12' = 'white',
    'misc' = '#969696' # grey
    )
