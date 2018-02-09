library(data.table)
library(tidyverse)
library(ggExtra)
library(lme4)


dw<-read.csv("./data/e054mega17.csv")
lcddat<-read.csv("./data/e014splst_LCD_edited.csv")
cdrnamedat<-read.csv("./data/ccesrPlantSpeciesData.csv")


#### wide to long
dl<-melt(dw, id.vars = names(dw)[1:16],
         measure.vars = names(dw)[17:256],
         variable.name = "species", value.name = "biomass")

#### match names to long CDR list
dl$cdr_name_long<-cdrnamedat$Species[match(as.character(dl$species), as.character(cdrnamedat$Specid))]

#check mis-matches
sort(unique(dl$species[is.na(dl$cdr_name_long)]))
#only a few missing - will fix these below


#### match to "lcd" species names
dl$lcd_species<-lcddat$LCD_name[match(as.character(dl$cdr_name_long), as.character(lcddat$Species))]

#check mis-matches
dl<-dl[!is.na(dl$Exp),]
dl$lcd_species<-as.character(dl$lcd_species)
dl<-dl[!is.na(dl$biomass) & dl$biomass>0,]

sort(unique(dl$cdr_name_long[is.na(dl$lcd_species)]))

#fix missing records:
dl$lcd_species[dl$cdr_name_long=="Agropyron smithii"]<-"Agropyron repens"       #common error
dl$lcd_species[dl$cdr_name_long=="Aster junciformis"]<-"Aster sp."              #lumping
dl$lcd_species[dl$cdr_name_long=="Bouteloua gracilis"]<-"Bouteloua gracilis"
dl$lcd_species[dl$cdr_name_long=="Bromus ciliatus"]<-"Bromus inermis"           #common error
dl$lcd_species[dl$cdr_name_long=="Bromus sp."]<-"Bromus inermis"                #common error
dl$lcd_species[dl$cdr_name_long=="Calamagrostis canadensis"]<-"Calamagrostis canadensis"
dl$lcd_species[dl$cdr_name_long=="Celastrus scandens"]<-"Celastrus scandens"
dl$lcd_species[dl$cdr_name_long=="Chenopodium sp."]<-"Chenopodium album"        #lumping
dl$lcd_species[dl$cdr_name_long=="Corn litter"]<-"Miscellaneous litter"
dl$lcd_species[dl$cdr_name_long=="Cornus racemosa"]<-"Miscellaneous woody plants"
dl$lcd_species[dl$cdr_name_long=="Equisetum hyemale"]<-"Equisetum sp."
dl$lcd_species[dl$cdr_name_long=="Helianthus giganteus"]<-"Helianthus laetiflorus"  #common error
dl$lcd_species[dl$cdr_name_long=="Juncus sp."]<-"Miscellaneous rushes"
dl$lcd_species[dl$cdr_name_long=="Miscellaneous herbs 2"]<-"Miscellaneous species"
dl$lcd_species[dl$cdr_name_long=="Miscellaneous sedges"]<-"Miscellaneous sedges"
dl$lcd_species[dl$cdr_name_long=="Parthenocissus inserta"]<-"Parthenocissus vitacea"        #lumping
dl$lcd_species[dl$cdr_name_long=="Parthenocissus sp."]<-"Parthenocissus vitacea"        #lumping
dl$lcd_species[dl$cdr_name_long=="Pedicularis canadensis"]<-"Pedicularis canadensis"
dl$lcd_species[dl$cdr_name_long=="Penstemon sp."]<-"Penstemon grandiflorus"           #most common
dl$lcd_species[dl$cdr_name_long=="Polygonum persicaria"]<-"Agropyron repens"
dl$lcd_species[dl$cdr_name_long=="Populus grandidentata"]<-"Polygonum convolvulus"          #most common
dl$lcd_species[dl$cdr_name_long=="Senecio sp."]<-"Senecio sp."
dl$lcd_species[dl$cdr_name_long=="Sisymbrium altissimum"]<-"Sisymbrium altissimum"
dl$lcd_species[dl$cdr_name_long=="Stellaria media"]<-"Stellaria media"



sort(unique(dl$species[is.na(dl$lcd_species)]))
dl$lcd_species[dl$species=="Fraxamer"]<-"Miscellaneous woody plants"
dl$lcd_species[dl$species=="Miscgra1"]<-"Miscellaneous species"
dl$lcd_species[dl$species=="Mischer1"]<-"Miscellaneous species"
dl$lcd_species[dl$species=="Paripens"]<-"Paripens"           #?

#patrick's modifications
library(dplyr)
dl_ag<-dl %>% 
  group_by(Year, Plot, OldField, Transect, BurnTrt, BurnSincePreviousHarvest,YearAb,lcd_species) %>% 
  summarise(biomass = sum(biomass))


#manipulate data to add zeros and prepare for plotting and mixed effects model
clean_54<-ungroup(filter(dl_ag, OldField!="LS", lcd_species != "Miscellaneous litter", lcd_species != "Mosses & lichens",lcd_species !=  "Miscellaneous woody plants",
                         Year != "2017"))

clean_54<-clean_54 %>% 
  spread(key = lcd_species, value = biomass, fill = 0) %>% 
  gather(key = lcd_species, value = biomass, -c(1:7)) %>% 
  group_by(Year, OldField,Transect) %>% 
  mutate(rel_biomass = biomass/sum(biomass))

mean_rank<- clean_54%>% 
  group_by(lcd_species) %>%
  summarise(Mean_bmass=mean(biomass),Mean_rel_bmass=mean(rel_biomass), frequency = sum(biomass>0)) %>% 
  arrange(desc(Mean_rel_bmass)) %>% 
  filter(lcd_species != "Miscellaneous litter", lcd_species != "Mosses & lichens") %>% 
  mutate(rank = 1:n())

#make plots####
#all variability
clean_54 %>% 
  left_join(mean_rank) %>% 
  filter(!is.na(rank)) %>% 
  ggplot(aes(x=rank,y=rel_biomass, group = rank)) +
  #geom_boxplot(size = 0.1,outlier.shape = 20,outlier.size = 0.5, fill = NA)+
  geom_point(aes(y=Mean_rel_bmass), col = 2, size = 0.5)+
  geom_jitter(pch = 20, size = 0.5, width = 0.25)+
  theme_bw()+
  removeGrid()
ggsave("./figures/all_rankvar.png", height =8.5, width = 11)

clean_54 %>% 
  left_join(mean_rank) %>% 
  filter(!is.na(rank)) %>% 
  filter(rank<21) %>% 
  ggplot(aes(x=rel_biomass,y=biomass, group = lcd_species))+
  geom_point()+
  facet_wrap(~lcd_species)+
  geom_smooth()+
  geom_smooth(method = "lm", col = 2)+
  scale_x_log10()+
  scale_y_log10()

clean_54 %>% 
  left_join(mean_rank) %>% 
  filter(!is.na(rank)) %>% 
  filter(rank>70, rank<91) %>% 
  ggplot(aes(x=rel_biomass,y=biomass, group = lcd_species))+
  geom_point()+
  facet_wrap(~lcd_species)+
  geom_smooth()+
  geom_smooth(method = "lm", col = 2)+
  scale_x_log10()+
  scale_y_log10()

clean_54 %>% 
  left_join(mean_rank) %>% 
  filter(!is.na(rank)) %>% 
  filter() %>% 
  ggplot(aes(x=rel_biomass,y=biomass, group = lcd_species ,color = rank))+
  #geom_point()+
  #facet_wrap(~lcd_species)+
  geom_smooth(method = "lm", se=F)+
  scale_color_viridis_c()+
  scale_x_log10()+
  scale_y_log10()

slopes<-clean_54 %>% 
  left_join(mean_rank) %>% 
  filter(!is.na(rank)) %>% 
  filter(biomass >0) %>%
  group_by(lcd_species,rank, frequency) %>% 
  do(lm1 = lm(log(biomass)~log(rel_biomass), data = .)) %>% 
  tidy(lm1) %>% 
  filter(term == "log(rel_biomass)")

ggplot(slopes,aes(x=estimate))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 1, col = 1)+
  scale_x_continuous(limits = c(-1,3))

ggplot(slopes,aes(x=rank, y=estimate, size = frequency))+
  geom_point(pch = 1)+
  geom_hline(yintercept = 1, col ="pink")+
  scale_y_continuous(limits = c(-1, 3))+
  scale_size_continuous(breaks = c(5,100,500,1000))

#lmer models####
clean_54_short<-clean_54 %>% 
  left_join(mean_rank) %>% 
  filter(rank<22) %>% 
  filter(biomass>0) %>% 
  droplevels()

model1<-lmer(log(biomass) ~ -1 + log(rel_biomass) + 
               #(-1 + log(rel_biomass) | lcd_species) + 
               #(-1 + log(rel_biomass) | Year) + 
               #(-1 + log(rel_biomass) | OldField) + 
               (-1 + log(rel_biomass) | OldField:lcd_species:Year), 
             data = clean_54_short)
summary(model1)
