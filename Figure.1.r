

# library
library(dplyr)
library(foreach)
library(doParallel)
library(vegan)
library(ggpubr)
library(ggnewscale) #
library(tidyverse)
library(reshape2)
library('ggbreak')
library(ggplot2)
library(eulerr)
library(gghalves)
library(xlsx)
library(tidyr)
library(biscale)
library(sf)
library(hrbrthemes)
library(ggtext)
library(scatterpie)
library("ggsci")
library(ggspatial)
library(ggprism)
library(purrr) # set_names()

######################################## map ---------------

load("../Data/1.china_data_pro.RData")

#读取中国地图文件信息
china_shp <- "../Data/中国省级地图GS（2019）1719号.geojson"
nine <- "../Data/九段线GS（2019）1719号.geojson"
china <- sf::read_sf(china_shp)
nine_line <- sf::read_sf(nine)

china_data_pro %>% arrange(desc(radius)) -> china_data_pro
china_data_pro$lon %>% factor(china_data_pro$lon, levels=china_data_pro$lon)
china_data_pro$lat %>% factor(china_data_pro$lat, levels=china_data_pro$lat)

p <- ggplot()+
  geom_sf(data = china,fill="#f9fbe7",size=.125,color="black") + 
  geom_sf(data = nine_line,size=.125) + 
  coord_sf(crs ="+proj=laea +lat_0=40 +lon_0=104",  
           xlim = c(-2500000,2877844),
           ylim = c(-2387082,1654989)) +
  scatterpie::geom_scatterpie(data=china_data_pro,sorted_by_radius = F,
                              aes(x = Longitude,y = Latitude,r=radius*30000/3),
                              cols=c('farmland','forest','grass','gobi','tailings'),color="black",
                              alpha=0.9) +
  
  annotation_north_arrow(location = "tl", which_north = "false",
                         style = north_arrow_fancy_orienteering) +
  theme(plot.subtitle = element_text(size = 27, colour = "black", hjust = 0.5),
        axis.line = element_line(colour = "gray30", 
                                 size = 1, linetype = "solid"), axis.ticks = element_line(colour = "gray30"), 
        axis.text = element_text(colour = "gray30" ,size = 20), 
        axis.text.x = element_text(colour = "gray30" ,size = 20), 
        axis.text.y = element_text(colour = "gray30" ,size = 20)) +
  #scale_fill_ucscgb(name="Habitat")+#填充颜色ggsci
  scale_fill_manual(values = c("farmland" = "#addd8e", 
                               "forest" = "#f4d488", 
                               "grass" = "#2c7fb8", 
                               "gobi" = "#7986cb", 
                               "tailings" = "#EA5C15"))+
  labs(x = "Longitude", y = "Latitude", col = "gray30", size = 30,
       fill = "Habitat")+
  theme(axis.ticks = element_line(size = 0.6), 
        axis.title = element_text(size = 22, 
                                  colour = "gray30")) +
  theme(plot.subtitle = element_text(size = 32)) + 
  theme(legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

p

p2 <- p+annotation_scale(location = "bl") +
  geom_scatterpie_legend(china_data_pro$radius*30000/3,x = -2200000,y=-1800000,n = 2 ,
                         labeller=function(x) x%/%3000/(10/3))
p2

library(ggprism)
#九段线单独列出
library(cowplot)
china_shp <- "全国.json"
china <- st_read(china_shp)

pp <- ggplot() + theme(axis.text.x = element_text(size = 8 ,angle = 60), 
                       axis.text.y = element_text(size = 8))+
  geom_sf(data = china, colour = "black", fill="#f9fbe7")+
  coord_sf(xlim = c(117131.4,2115095), ylim = c(-4028017,-1877844),
           crs = "+proj=laea +lat_0=40 +lon_0=104")+
  theme(aspect.ratio = 1.25,
        panel.border = element_rect(fill = NA, colour = "#525252"),
        plot.margin = unit(c(0,0,0,0),"mm")) + theme(axis.text.x = element_text(colour = "black"), 
                                                     axis.text.y = element_text(colour = "black")) +
  theme(axis.text.x = element_text(vjust = 0.5))

aa <- ggdraw() + 
  draw_plot(p2) +
  draw_plot(pp, x = 0.675, y = 0.08, width = 0.1, height = 0.3)

aa

######################################## MGE accumulative curves ---------------

# load
load("../Data/1.Rarefaction.analysis.Rdata")

#
ICE <- melt(resta_ICE) %>% mutate(variable = str_replace(Var2 , "resample", '')) %>% mutate(type = "ICE")
Integron <- melt(resta_Integron) %>% mutate(variable = str_replace(Var2 , "resample", '')) %>% mutate(type = "Integron")
Plasmid <- melt(resta_Plasmid) %>% mutate(variable = str_replace(Var2 , "resample", '')) %>% mutate(type = "Plasmid")
Phage <- melt(resta_Phage) %>% mutate(variable = str_replace(Var2 , "resample", '')) %>% mutate(type = "Phage")
Tn <- melt(resta_Tn) %>% mutate(variable = str_replace(Var2 , "resample", '')) %>% mutate(type = "Tn")
IS <- melt(resta_IS) %>% mutate(variable = str_replace(Var2 , "resample", '')) %>% mutate(type = "IS")

#
ICE.avg <- data.frame(means = colMeans(data.frame(resta_ICE))) %>% 
  mutate(site=unique(ICE$variable)) %>% mutate(type = "ICE")
Integron.avg <- data.frame(means = colMeans(data.frame(resta_Integron))) %>% 
  mutate(site=unique(Integron$variable)) %>% mutate(type = "Integron")
Plasmid.avg <- data.frame(means = colMeans(data.frame(resta_Plasmid))) %>% 
  mutate(site=unique(Plasmid$variable)) %>% mutate(type = "Plasmid")
Phage.avg <- data.frame(means = colMeans(data.frame(resta_Phage))) %>% 
  mutate(site=unique(Phage$variable)) %>% mutate(type = "Phage")
Tn.avg <- data.frame(means = colMeans(data.frame(resta_Tn))) %>% 
  mutate(site=unique(Tn$variable)) %>% mutate(type = "Tn")
IS.avg <- data.frame(means = colMeans(data.frame(resta_IS))) %>% 
  mutate(site=unique(IS$variable)) %>% mutate(type = "IS")

#
ICE.sd <- resta_ICE %>% data.frame() %>% apply(2, FUN=function(x) sd(x)) %>% data.frame(sd=.) %>% mutate(type = "ICE")
Integron.sd <- resta_Integron %>% data.frame() %>% apply(2, FUN=function(x) sd(x)) %>% data.frame(sd=.) %>% mutate(type = "Integron")
Plasmid.sd <- resta_Plasmid %>% data.frame() %>% apply(2, FUN=function(x) sd(x)) %>% data.frame(sd=.) %>% mutate(type = "Plasmid")
Phage.sd <- resta_Phage %>% data.frame() %>% apply(2, FUN=function(x) sd(x)) %>% data.frame(sd=.) %>% mutate(type = "Phage")
Tn.sd <- resta_Tn %>% data.frame() %>% apply(2, FUN=function(x) sd(x)) %>% data.frame(sd=.) %>% mutate(type = "Tn")
IS.sd <- resta_IS %>% data.frame() %>% apply(2, FUN=function(x) sd(x)) %>% data.frame(sd=.) %>% mutate(type = "IS")

# (1) IS ------
IS$variable  <- as.double(IS$variable)
IS.avg$site <- as.double(IS.avg$site)
IS.avg$sd <- IS.sd$sd

# 
rare.df.seperate <- ggplot(IS.avg, aes(x = site, y = means, color = type, group = type)) +
  geom_errorbar(aes(ymin = means - sd, ymax = means + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_point(aes(fill = type), size = 3, shape = 21, alpha = 0.7) +
  theme_pubr(legend = "none", border = TRUE, margin = TRUE) +
  scale_fill_manual(values = c("#f59f44")) +
  scale_color_manual(values = c("#f59f44")) +
  labs(x = 'Samples No.', y = 'Cumulative No.') +
  scale_x_continuous(limits = c(0, 311), breaks = c(0, 60, 120, 180, 240, 310)) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) 

rare.df.seperate

# (2) Plasmid ------
Plasmid$variable  <- as.double(Plasmid$variable)
Plasmid.avg$site <- as.double(Plasmid.avg$site)
Plasmid.avg$sd <- Plasmid.sd$sd

# 
rare.df.seperate <- ggplot(Plasmid.avg, aes(x = site, y = means, color = type, group = type)) +
  geom_errorbar(aes(ymin = means - sd, ymax = means + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_point(aes(fill = type), size = 3, shape = 21, alpha = 0.7) +
  theme_pubr(legend = "none", border = TRUE, margin = TRUE) +
  scale_fill_manual(values = c("#cd383d")) +
  scale_color_manual(values = c("#cd383d")) +
  labs(x = 'Samples No.', y = 'Cumulative No.') +
  scale_x_continuous(limits = c(0, 311), breaks = c(0, 60, 120, 180, 240, 310)) +
  ylim(0, 200000) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) 

rare.df.seperate

# (3) ICE ------
ICE$variable  <- as.double(ICE$variable)
ICE.avg$site <- as.double(ICE.avg$site)
ICE.avg$sd <- ICE.sd$sd

# 
rare.df.seperate <- ggplot(ICE.avg, aes(x = site, y = means, color = type, group = type)) +
  geom_errorbar(aes(ymin = means - sd, ymax = means + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_point(aes(fill = type), size = 3, shape = 21, alpha = 0.7) +
  theme_pubr(legend = "none", border = TRUE, margin = TRUE) +
  scale_fill_manual(values = c("#f7bfa4")) +
  scale_color_manual(values = c("#f7bfa4")) +
  labs(x = 'Samples No.', y = 'Cumulative No.') +
  scale_x_continuous(limits = c(0, 311), breaks = c(0, 60, 120, 180, 240, 310)) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) 

rare.df.seperate

# (4) Integron ------
Integron$variable  <- as.double(Integron$variable)
Integron.avg$site <- as.double(Integron.avg$site)
Integron.avg$sd <- Integron.sd$sd

# 
rare.df.seperate <- ggplot(Integron.avg, aes(x = site, y = means, color = type, group = type)) +
  geom_errorbar(aes(ymin = means - sd, ymax = means + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_point(aes(fill = type), size = 3, shape = 21, alpha = 0.7) +
  theme_pubr(legend = "none", border = TRUE, margin = TRUE) +
  scale_fill_manual(values = c("#2e7d32")) +
  scale_color_manual(values = c("#2e7d32")) +
  labs(x = 'Samples No.', y = 'Cumulative No.') +
  scale_x_continuous(limits = c(0, 311), breaks = c(0, 60, 120, 180, 240, 310)) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) 

rare.df.seperate

# (5) Phage ------
Phage$variable  <- as.double(Phage$variable)
Phage.avg$site <- as.double(Phage.avg$site)
Phage.avg$sd <- Phage.sd$sd

# 
rare.df.seperate <- ggplot(Phage.avg, aes(x = site, y = means, color = type, group = type)) +
  geom_errorbar(aes(ymin = means - sd, ymax = means + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_point(aes(fill = type), size = 3, shape = 21, alpha = 0.7) +
  theme_pubr(legend = "none", border = TRUE, margin = TRUE) +
  scale_fill_manual(values = c("#aedacb")) +
  scale_color_manual(values = c("#aedacb")) +
  labs(x = 'Samples No.', y = 'Cumulative No.') +
  scale_x_continuous(limits = c(0, 311), breaks = c(0, 60, 120, 180, 240, 310)) +
  ylim(0, 22000) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) 

rare.df.seperate

# (6) Tn ------
Tn$variable  <- as.double(Tn$variable)
Tn.avg$site <- as.double(Tn.avg$site)
Tn.avg$sd <- Tn.sd$sd

# 
rare.df.seperate <- ggplot(Tn.avg, aes(x = site, y = means, color = type, group = type)) +
  geom_errorbar(aes(ymin = means - sd, ymax = means + sd), width = 0.2, position = position_dodge(0.9)) +
  geom_point(aes(fill = type), size = 3, shape = 21, alpha = 0.7) +
  theme_pubr(legend = "none", border = TRUE, margin = TRUE) +
  scale_fill_manual(values = c("#d0c0a5")) +
  scale_color_manual(values = c("#d0c0a5")) +
  labs(x = 'Samples No.', y = 'Cumulative No.') +
  scale_x_continuous(limits = c(0, 311), breaks = c(0, 60, 120, 180, 240, 310)) +
  #ylim(0, 22000) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) 

rare.df.seperate

######################################## MGE annotation composition ------------

load("../Data/1.No.of.MGEs.basedSequence.RData")

MGEs.percentage <- ggdonutchart(No.of.MGEs.basedSequence[,c(1,2)], "MGEnumber",
                                label = "MGE.type",                               
                                fill = "MGE.type",                            
                                color = "white",   size = 0,                           
                                palette = c("#f7bfa4",  "#2e7d32" ,"#aedacb", 
                                            "#d0c0a5", "#cd383d", "#f59f44"),
                                ggtheme = theme_pubr()) +
  
  theme(axis.text.x = element_blank(), legend.position = "NULL")

MGEs.percentage

######################################## MGE abundance, diversity and composition ----

load("../Data/1.MGE.abundance.diversity.RData")

#
TotalMGE_Depth_df$Habitat <- sample.information$Habitat[match(TotalMGE_Depth_df$sampleID, sample.information$sampleID)]

TotalMGE_Depth_df$Habitat <- factor(TotalMGE_Depth_df$Habitat, 
                                    levels= c("Tailings", "Farmland","Forest","Grass","Gobi"))

p.abun <- ggplot(TotalMGE_Depth_df, aes(Habitat, DepthPG)) +
  geom_boxplot(aes(fill = Habitat), width=0.9,size=0.5,outlier.color = NA)+  
  scale_fill_manual(values = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) +
  scale_color_manual(values = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) +
  ylab("Total MGE abundance (coverage, /Gb)") + xlab("") + 
  theme_test(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ylim(0, 45000) +
  theme(axis.ticks = element_line(colour = "black"), axis.text = element_text(colour = "black")) + 
  scale_x_discrete(labels=c("Tailings", "Farmland","Forest", "Grassland","Desert")) +
  theme(legend.position = "none", panel.grid = element_blank()) 

p.abun

# 
TotalMGE_diversity_df <- MGE.diversity.df %>% group_by(sampleID) %>% 
  summarise(Diversity = sum(Diversity))

TotalMGE_diversity_df$Habitat <- sample.information$Habitat[match(TotalMGE_diversity_df$sampleID, sample.information$sampleID)]

TotalMGE_diversity_df$Habitat <- factor(TotalMGE_diversity_df$Habitat, 
                                        levels= c("Tailings", "Farmland","Forest","Grass","Gobi"))

p.div <- ggplot(TotalMGE_diversity_df, aes(Habitat, Diversity)) +
  geom_boxplot(aes(fill = Habitat), width=0.9,size=0.5,outlier.color = NA)+  
  scale_fill_manual(values = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) +
  scale_color_manual(values = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) +
  ylab("No.of MGEs") + xlab("") + 
  theme_test(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(axis.ticks = element_line(colour = "black"), axis.text = element_text(colour = "black")) + 
  scale_x_discrete(labels=c("Tailings", "Farmland","Forest", "Grassland","Desert")) +
  theme(legend.position = "none", panel.grid = element_blank()) 

p.div

#
load("../Data/1.MGE.composition.Rdata")
allMGE_df.2 <- allMGE_df.2 %>% group_by(Habitat, MGE.type) %>% summarise(percent=mean(percent))
sum(allMGE_df.2$percent) # 5 

# plot ----
allMGE_df.2$MGE.type <- factor(allMGE_df.2$MGE.type, 
                               levels = c("IS", "Plasmid", "Tn", "Phage", "Integron", "ICE"))

allMGE_df.2$Habitat <- factor(allMGE_df.2$Habitat, 
                              levels = c("Tailings", "Farmland","Forest","Grass","Gobi"))

p.com.abun <- ggplot(allMGE_df.2, aes(x = Habitat, y = percent, fill=MGE.type))  + 
  geom_col(width = 0.85) +  
  scale_fill_manual(values = c("#f59f44",  "#cd383d" ,"#d0c0a5", 
                               "#aedacb", "#2e7d32", "#f7bfa4")) + 
  theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) + 
  theme_pubr(border = T, legend = "top", base_size = 12) + 
  scale_y_continuous(expand = c(0.0001, 0.015), limits = c(0,1.01)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  xlab("") + ylab("Composition of MGE types") + #+ guides(fill = guide_legend(nrow=2)) 
  theme(legend.position="right") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(fill="MGE Types") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
  #ylim(0, 150000) +
  theme(axis.ticks = element_line(colour = "black"), axis.text = element_text(colour = "black")) + 
  scale_x_discrete(labels=c("Tailings", "Farmland","Forest", "Grassland","Desert")) +
  theme(legend.position = "none", panel.grid = element_blank()) 

p.com.abun

######################################## classified MGE annotation ----

MGE.combined.df <- read.xlsx("../Data/1.MGE.blast.database.percentage.xlsx", sheetIndex = 1)
MGE.combined.df$MGE.types <- factor(MGE.combined.df$MGE.types, levels = c("Plasmid", "Phage", "ICE", 
                                                                          "Tn", "IS",  "Integron"))
ggplot(MGE.combined.df) +
  aes(x = MGE.types, y = Class.per, fill = MGE.types, color =  MGE.types) +
  geom_col() +
  scale_fill_manual(values = c("#cd383d",  "#aedacb" ,"#f7bfa4", 
                               "#d0c0a5", "#f59f44", "#2e7d32")) +
  scale_color_manual(values = c("#cd383d",  "#aedacb" ,"#f7bfa4", 
                                "#d0c0a5", "#f59f44", "#2e7d32")) +
  xlab("") + ylab("Percentage of status of MGEs (%)") +
  theme_bw(base_size = 15) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white"),
        legend.position = "NULL") 
