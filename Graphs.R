### Analyse Doc Tamara
# Damien Beillouin
#16 Février 


######1/ Initialisation ######
# 1. dataload
DATA <- read.csv("~/Downloads/Fwd Premiere extraction données questionnaire PSP/extraction_enquete_PSP_15022022-10h00.csv", comment.char="#")
NOM_DEP<- read.csv("~/Downloads/departements-region.csv")
  
#2. packages
library(maps)
library(ggplot2)
library(plyr)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(forcats)

####### A.  Map des départements ######

# count by DEp

COUNT <- DATA                             %>% 
  group_by(Quel.est.votre.département..) %>% 
  tally()                                %>% 
  rename(num_dep = Quel.est.votre.département..)   %>% 
  mutate(num_dep = as.character(num_dep))
## attention certains ont mis leurs codes postaux.

# on représente les zones sur une carte de france

# je transforme la "carte" en data frame
france.map <- map_data('france') %>% 
  rename(dep_name = region) %>% 
  left_join(.,NOM_DEP)


# fonction pour avoir le centre de chaque region pour les labels
milieu <- function(x) mean(range(x,na.rm=TRUE))

# nouveau data frame avec les nom des regions et leur coordonnées
centre <- ddply(france.map, .(region), colwise(milieu, .(lat,long)))

## merge files
france.map2<-left_join(france.map,COUNT, by= "num_dep")

map_theme <- theme(title=element_text(),
                   plot.title=element_text(margin=margin(20,20,20,20), size=18, hjust = 0.5),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.grid.major= element_blank(), 
                   panel.background= element_blank()) 

ggplot(france.map2, aes(long,lat, group = group, fill= n)) +
  geom_polygon() +
  coord_map() +
  scale_fill_gradientn(colours = terrain.colors(10), name = "Nombre répondant")+
  labs(x = "", 
       y = "", 
       title = "Nombre répondant à l'enquête", 
       subtitle = "Données de Tamara",
       caption = "ououh") +
  map_theme



########B. Quel est votre age #######

COUNT <- DATA                        %>%
  group_by(Quel.est.votre.âge..)     %>% 
  tally()                            %>%
  mutate(Quel.est.votre.âge.. = fct_relevel(Quel.est.votre.âge.., 
                            "entre 20 et 29 ans",
                            "entre 30 et 39 ans",
                            "entre 40 et 49 ans", 
                            "entre 50 et 59 ans",
                            "entre 60 et 69 ans", 
                            "70 ans ou plus", 
                            ""))

COL<-brewer.pal(n = 8, name = "Dark2")
ggplot(COUNT,
       aes(x = Quel.est.votre.âge.., y = n, fill = Quel.est.votre.âge..)) +
  geom_col() +
  scale_fill_manual(values = COL) +
  labs(x = "class of Age",
       y = "Count",
       title = "Age des participants",
       caption = "Data source: tamara") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")+
  coord_flip()



COUNT <- DATA                                                %>%
  group_by(Quel.est.votre.âge..,Quel.est.votre.genre.. )     %>% 
  tally()                            %>%
  mutate(Quel.est.votre.âge.. = fct_relevel(Quel.est.votre.âge.., 
                                            "entre 20 et 29 ans",
                                            "entre 30 et 39 ans",
                                            "entre 40 et 49 ans", 
                                            "entre 50 et 59 ans",
                                            "entre 60 et 69 ans", 
                                            "70 ans ou plus", 
                                            ""))

ggplot(COUNT, aes(x = Quel.est.votre.âge.., y = n, fill = Quel.est.votre.âge..)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "\n ..", y = "Count \n", fill = "Age Group \n",
       title = "age par genre \n") +
  facet_grid(. ~ Quel.est.votre.genre..) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12),
        legend.title = element_text(face="bold", size = 10),  
        strip.background = element_rect(fill="lightgreen", colour="black", size=1),
        strip.text = element_text(face="bold", size=rel(1.2)))+
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")+
  coord_flip()





dreaded_lang %>%
  mutate(label = sprintf("%1.1f%%", pct)) %>%
  bar_chart(language, pct, highlight = "R", bar_color = "black") +
  geom_text(aes(label = label, hjust = -0.1), size = 5) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion()
  ) +
  labs(
    x = NULL,
    y = "Developers Who are Developing with the Language but<br>Have not Expressed Interest in Continuing to Do so",
    title = "Top 10 Most Dreaded Programming Languages",
    subtitle = "*R Placed 8th*",
    caption = "Source: Stackoverflow Developer Survey 2019"
  ) +
  mdthemes::md_theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )