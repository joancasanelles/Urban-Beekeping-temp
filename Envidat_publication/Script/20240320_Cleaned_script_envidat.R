##########################################
# Paper: opinion urban beekeeping
# Author: Joan Casanelles Abella
# Date: 01.02.2024
# Script: plots for the paper
##########################################
#### Clear workspace ---------------------------------
rm(list = ls()); graphics.off()
setwd("~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Analyses/Data/")

#### Packages ---------------------------------
## Plotting
library(wesanderson)
library(ggplot2)
## Data managemnt
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)
## Exporting 
library(ggpubr)
## Spatial analyses
library(terra)

#### Data ---------------------------------
## Map cities
cities_coords=read.csv("Coordinates_cities_studies/20240308_cities_coords.csv", blank.lines.skip = T)
## Proportion bees in the community
prop_wb_hb = read.csv("Dominance_honeybees/20240301_proportion_HB_WB.csv", header = T, dec = ",")
## RAD beees
rad.bees=read.csv("Dominance_honeybees/20240308_rad_bees.csv")
## RAD beehives
# Data from Zurich can be found in ENVIDAT: 
rad_beehives = read.csv("Distribution_beehives_beekeepers/20240417_rad_beehives_beekeepers.csv")
## Growth
growth.beehives = read.csv("Increase_hives_time_world/20240308_growth_beekeeping.csv")

#### Figure 1 ---------------------------------
## Plot
rad_beehives_plot=ggplot(rad_beehives[rad_beehives$N..hives <100,], aes(x=(Std_owner_ID), y=N..hives, color=City)) + # remove IDs with more than 100 hives, unlikely
  geom_point(na.rm = T, size=3) +
  theme_classic(base_size = 25) +
  scale_color_brewer(palette = "Paired") +
  xlab("ID") +
  scale_y_continuous("10log(N. beehives)", trans = "log10", breaks = c(1,3,10, 20, 30, 50, 100, 200, 300), labels =  c(1,3,10, 20, 30, 50,  100, 200, 300))
## Export
ggsave(plot = rad_beehives_plot, filename = "~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/rad_beehives.jpeg", device = "jpeg", dpi = 320, width = 4.5,height = 4, scale = 2)

#### Figure 2. Growth urban beekeeping  ---------------------------------
## Prepare data
growth.beehives$Density=as.numeric(growth.beehives$Density)
growth.beehives$Total.N..hives=as.numeric(growth.beehives$Total.N..hives)

## Plot World
# Create color palette
palette.wes=c("#0A9F9D", "#E54E21", "#6C8645","#CEB175", "#C18748", "#35274A", "#0B775E")
# Increase number of hives
nhives=ggplot(growth.beehives[growth.beehives$City %in% c("Greater London" ,"Berlin","Paris","Hamburg","New York", "Montreal", "Toronto"),], 
              aes(x=Year, y=Total.N..hives, group=City, color=City)) + 
  geom_line(size = 3) +
  scale_color_manual(values =  palette.wes ) +
  theme_classic(base_size = 25) +
  ylab("N. hives")
# Increase density
densityhives=ggplot(growth.beehives[growth.beehives$City %in% c("Greater London" ,"Berlin","Paris","Hamburg","New York", "Montreal", "Toronto"),], aes(x=Year, y=Density, group=City, color=City)) + 
  geom_line(size = 3) +
  scale_color_manual(values =  palette.wes ) +
  theme_classic(base_size = 25) +
  ylab("Density (hives/km2)")
# Arranging
arrangedplot=ggarrange(nhives, densityhives, ncol = 2, nrow = 1, labels = c("(A)", "(B)"), common.legend = T)
# Exporting
ggsave(plot = arrangedplot,filename = paste("~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/growth.hives.png"), 
       width =11, height = 5.5, device = "png")

## In Switzerland
# Select Switzerland
growth.ch=growth.beehives[growth.beehives$Country=="Switzerland",]
# Increase number of hives
nhives_ch=ggplot(growth.ch, aes(x=Year, y=Total.N..hives, group=City, color=City)) + 
  geom_line(size = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic(base_size = 25) +
  ylab("N. hives")
# Increase number of beekeepers
nbeekeepers_ch=ggplot(growth.ch, aes(x=Year, y=Total.N..beekeepers, group=City, color=City)) + 
  geom_line(size = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic(base_size = 25) +
  ylab("N. beekeepers")
# Arranging
arrangedplot_ch=ggarrange(nhives_ch, nbeekeepers_ch, ncol = 2, nrow = 1, labels = c("(C)", "(D)"), common.legend = T)
# Exporting
ggsave(plot = arrangedplot_ch,filename = paste("~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/growth.hives.ch.png"), 
       width =11, height = 5.5, device = "png")

#### Figure 3 (map) ---------------------------------
# create data for world coordinates using 
# map_data() function
world_coordinates <- map_data("world")

# create world map using ggplot() function
maps_cities=ggplot() +
  
  # geom_map() function takes world coordinates 
  # as input to plot world map
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region), fill="lightgray"
  ) +
  geom_point(data=cities_coords, aes(x=Lon, y=Lat), col="darkred") +
  theme_classic() +
  ylab("") +
  xlab("")
theme(axis.ticks = element_blank(), axis.text = element_blank())
ggsave(plot = maps_cities, filename = "~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/maps_cities.jpeg", device = "jpeg", dpi = 320, width = 4,height = 3)


####  Figure 3 (barplots) ---------------------------
## Select columns
prop_wb_hb2= prop_wb_hb[, c(1,8,9,12,14,18,19)]
## Prepare data
prop_wb_hb2_l= melt(data = prop_wb_hb2, id.vars = c("Article_ID","Continent","Country", "Number.sites", "Sampling.type"), value.name = "proportion", variable.name = "bee")
## Code for Xa-xis
prop_wb_hb2_l$sampling.type.numeric=revalue(prop_wb_hb2_l$Sampling.type, c("Active"=1, "Passive"=2, "Both"=3, "both"=3))
prop_wb_hb2_l$sampling.type.numeric=as.numeric(prop_wb_hb2_l$sampling.type.numeric)
ex.nat=data.frame(Continent=c("America", "Europe", "Africa", "Oceania"), origin=c("Exotic", "Native", "Native", "Exotic"))
prop_wb_hb2_l=merge(prop_wb_hb2_l, ex.nat, by="Continent")
prop_wb_hb2_l$proportion=as.numeric(prop_wb_hb2_l$proportion)
prop_wb_hb2_l$origin.numeric=revalue(prop_wb_hb2_l$origin, c("Native"=1, "Exotic"=2))
prop_wb_hb2_l$sort.id=paste(prop_wb_hb2_l$sampling.type.numeric, prop_wb_hb2_l$origin.numeric, sep="")
prop_wb_hb2_l$sort.id=as.numeric(prop_wb_hb2_l$sort.id)
## Plot
prop.plot=ggplot(data = prop_wb_hb2_l, aes(y=proportion, x= reorder(Article_ID, sort.id), fill=bee)) +
  geom_bar(stat="identity") +
  theme_classic(base_size = 25) +
  xlab("Study ID") +
  ylab("Proportion of bees sampled") +
  scale_fill_manual(values=c('black','#E1B80D'),
                    name="Bee group",
                    breaks=c("Proportion.honeybees", "Proportion.wildbees"),
                    labels=c("Honeybees", "Wildbees")) +
  theme(axis.text.x=element_blank())
## Export
ggsave(plot = prop.plot, 
       filename = "~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/prop.plot.jpeg", 
       device = "jpeg", dpi = 320, width = 10,height = 6)

pal.ex=c("#DC3220","#005AB5")
exotic.native=ggplot(data = prop_wb_hb2_l, aes(y=origin, x= reorder(Article_ID, sort.id), fill=origin)) + geom_tile() +
  theme_classic(base_size = 25) +
  xlab("") +
  ylab("")+
  scale_fill_manual(values = pal.ex) +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank())
ggsave(plot = exotic.native, filename = "~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/exotic.native.jpeg", 
       device = "jpeg", dpi = 320, width = 10,height = 1.7)
#### Figure 3 (RADS) -----------------------------
## Prepare data
rad.bees$ID_2=paste(rad.bees$City,", ", rad.bees$Year, sep = "")
## Id
rad.bees$Article_ID=as.factor(rad.bees$Article_ID)
## Abundance honeybees
rad.bees.honeybees=rad.bees[rad.bees$Bee.merged %in% c("Apis mellifera", "Apismellifera", "Apis (Apis) mellifera Linnaeus, 1758", "Apis mellifera*", "Apis mellifera Linnaeus 1758"),]
rad.bees.honeybees.Europe=rad.bees.honeybees[rad.bees.honeybees$Continent %in% c("Europa", "Europe"),]
## Palette
palette_studies_urbanbees_america= c("#375E97", "#F98866", "#89DA59", 
                                     "#80BD9E", "#FA812F", "#D9B44A",
                                     "#07575B", "#F62A00", "#F18D9E",
                                     "#FAAF08", "#763626", "#0F1B07",
                                     "#9B4F0F", "#CB0000", "#F7EFE2",
                                     "#4897D8", "#3F6C45", "#2D4262")
palette_studies_urbanbees_europe= c("#D09683", "#75B1A9", "#D9B44A",
                                    "#4F6457", "#F4CC70", "#ACD0C0")


rad.bees.Europe=rad.bees[rad.bees$Continent %in% c("Europa", "Europe"),]
## Plot
rad_bees_plot_america=ggplot(rad.bees[rad.bees$Continent=="America",], aes(x=(Bee_ID_per_study), y=(Abundance), fill= ID_2)) + 
  scale_fill_manual(values = palette_studies_urbanbees_america) +
  scale_color_manual(values = palette_studies_urbanbees_america) +
  geom_point(na.rm = T, pch=21, aes(color=ID_2), size=0.75) +
  geom_point(data=rad.bees.honeybees[rad.bees.honeybees$Continent=="America",], aes(x=Bee_ID_per_study, y=(Abundance)), size=6, pch=22) +
  theme_classic(base_size = 25) +
  xlab("Bee ID") +
  scale_y_continuous("log(Abundance (N. ind.))", trans="log10")  + guides(fill = guide_legend(ncol = 1))
rad_bees_plot_europe=ggplot(rad.bees.Europe, aes(x=(Bee_ID_per_study), y=(Abundance), fill= ID_2)) + 
  scale_fill_manual(values = palette_studies_urbanbees_europe) +
  scale_color_manual(values = palette_studies_urbanbees_europe) +
  geom_point(na.rm = T, pch=21, aes(color=ID_2), size=0.75) +
  geom_point(data=rad.bees.honeybees.Europe, aes(x=Bee_ID_per_study, y=(Abundance)), size=6, pch=22) +
  theme_classic(base_size = 25) +
  xlab("Bee ID") +
  scale_y_continuous("log(Abundance (N. ind.))", trans="log10")  + guides(fill = guide_legend(ncol = 1))
##Export
ggsave(plot = rad_bees_plot_america, filename = "~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/rad_bees_plot_america.jpeg", 
       device = "jpeg", dpi = 320, width = 10,height = 6, scale = 1)
ggsave(plot = rad_bees_plot_europe, filename = "~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/rad_bees_plot_europe.jpeg", 
       device = "jpeg", dpi = 320, width = 10,height = 6, scale = 1)


#### Figure 4 ---------------------------------
### Data
# Hive data is confidential as it contains precise coordinates, it was obtained in 2019  from Kanton Zürich Veterinary office 
# with confidentiality agreement: https://www.zh.ch/de/gesundheitsdirektion/veterinaeramt.html

# Overwarming map was obtained to Prof. Dr. Eberhard Parlow (Universität Basel). It was commisioned by the Kanton Zürich for the
# Klimaanalyse Stadt Zürich KLAZ (2012) by Wymann et al.: https://www.zh.ch/de/umwelt-tiere/umweltschutz/umweltpraxis/definitionsseite/2012/68/zup068_2012_a0030_klimaanalyse-pdf.html
# NDVI data was processed at WSL by Christian Ginzler.

## Plot histograms overwarming
hives.overwarming.agg = read.csv("Distribution_beehives_urban_gradients/overwarming.hives.buffers.csv")
histogram.overwarming=ggplot(hives.overwarming.agg, aes(x=mean.temp.corr)) +
  geom_histogram() +
  facet_wrap(~order.id,scales='free') +
  theme_classic(base_size = 25) +
  theme(  strip.background = element_blank(),
          strip.text.x = element_blank()) +
  xlab("Mean overwheating (°C)") +
  scale_y_continuous(limits=c(0,30)) +
  scale_x_continuous(limits=c(-3,5)) +
  ylab("N. hives")
# Export
ggsave(plot = histogram.overwarming,filename = paste("~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/histogram.overwarming.png"), 
       width =15, height = 5, device = "png")



## Plot histograms NDVI
histogram.NDVI.data = read.csv("Distribution_beehives_urban_gradients/NDVI.hives.buffers.csv")
histogram.NDVI=ggplot(histogram.NDVI.data[histogram.NDVI.data$mean.NDVI>0,], aes(x=mean.NDVI)) +
  geom_histogram() +
  facet_wrap(~buffer,scales='free') +
  theme_classic(base_size = 25) +
  theme(  strip.background = element_blank(),
          strip.text.x = element_blank()) +
  xlab("NDVI") +
  scale_y_continuous(limits=c(0,25)) +
  scale_x_continuous(limits=c(0,0.75)) +
  ylab("N. hives")
ggsave(plot = histogram.NDVI,filename = paste("~/OneDrive - Eidg. Forschungsanstalt WSL/Postdoc_Freising/PROJECTS/Urban_beekeeping_perspective/Figures/histogram.NDVI.png"), 
       width =15, height = 5, device = "png")

