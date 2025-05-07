
# library
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggprism)
library(ggpubr)
library(ggsci)
library(patchwork)
library(vegan)

############### ARG abundance and diversity --------------------------
load("./Data/5.ARG.abundance.diversity.RData")

# abundance
ARGsubtype_df_nonReg_hab_sum$Habitats <- factor(ARGsubtype_df_nonReg_hab_sum$Habitats, levels = c('Taillings','Farmlnd','Forest', 'Grassland','Desert'))

ARGsubtype_df_nonReg_hab_sum %>% ggplot()+
  geom_violin(aes(x=Habitats,y=sum_depthPG, fill = Habitats),trim = FALSE)+
  geom_boxplot(aes(x=Habitats,y=sum_depthPG), width = 0.05, fill = "white",size = 0.8, outlier.shape = NA)+
  geom_jitter(aes(x=Habitats,y=sum_depthPG, fill = Habitats),width = 0.06,shape=21, alpha = 0.5)+
  stat_compare_means(aes(x = Habitats, y = sum_depthPG),label.y.npc =0.9,label.x.npc = 'centre' )+
  scale_fill_manual(values = c('#d0f5eb','#fdfedc','#f4d488','#a2bfd8','#fde4ec')) +
  scale_color_manual(values = c('#d0f5eb','#fdfedc','#f4d488','#a2bfd8','#fde4ec')) +
  theme_pubr(border = T, base_size = 18,legend = "none") +labs(x=NULL)

ARGsubtype_df_nonReg_hab_sum  %>% group_by(Habitats) %>% summarise(mean(sum_depthPG))
compare_means(sum_depthPG ~ Habitats,  data = ARGsubtype_df_nonReg_hab_sum, method = "wilcox.test")

ARGsubtype_df_nonReg_hab_sum_addsig <- ARGsubtype_df_nonReg_hab_sum  %>% group_by(Habitats) %>% summarise(MAX=max(sum_depthPG)) %>% 
  mutate(sig = c('a','b','b','b','b'))
fig_a <- ggplot()+
  geom_violin(data = ARGsubtype_df_nonReg_hab_sum,aes(x=Habitats,y=sum_depthPG, fill = Habitats),trim = FALSE)+ 
  geom_jitter(data = ARGsubtype_df_nonReg_hab_sum,aes(x=Habitats,y=sum_depthPG, fill = Habitats),width = 0.06,shape=21, alpha = 0.75)+
  geom_boxplot(data = ARGsubtype_df_nonReg_hab_sum,aes(x=Habitats,y=sum_depthPG), width = 0.055, fill = "white",size = 0.6, outlier.shape = NA)+
  stat_compare_means(data = ARGsubtype_df_nonReg_hab_sum,aes(x = Habitats, y = sum_depthPG),label.y.npc =0.9,label.x.npc = 'centre', size = 8)+
  scale_fill_manual(values = c('#d0f5eb','#fdfedc','#f4d488','#a2bfd8','#fde4ec')) +
  scale_color_manual(values = c('#d0f5eb','#fdfedc','#f4d488','#a2bfd8','#fde4ec')) +
  theme_pubr(border = T, base_size = 18,legend = "none") +labs(y= "Total ARG abundance\n(coverage, x/Gb)")+
  theme(axis.text.x = element_text(vjust = 1, hjust = 1,angle = 30)) +
  geom_text(data = ARGsubtype_df_nonReg_hab_sum_addsig , aes(x=Habitats,y=MAX+500, label=sig), size = 8) #sig label
fig_a

# diversity --
ARGsubtype_df_nonReg_hab_No_subtype$Habitats <- factor(ARGsubtype_df_nonReg_hab_No_subtype$Habitats, levels = c('Taillings','Farmlnd','Forest', 'Grassland','Desert'))

ARGsubtype_df_nonReg_hab_No_subtype  %>% group_by(Habitats) %>% summarise(MEAN=mean(Num)) %>% arrange(desc(MEAN))
compare_means(Num ~ Habitats,  data = ARGsubtype_df_nonReg_hab_No_subtype, method = "wilcox.test")

ARGsubtype_df_nonReg_hab_No_subtype_addsig <- ARGsubtype_df_nonReg_hab_No_subtype  %>% group_by(Habitats) %>% summarise(MAX=max(Num))  %>% 
  mutate(sig = c('c','a','b','b','d'))

fig_b <- ggplot()+
  geom_violin(data = ARGsubtype_df_nonReg_hab_No_subtype,aes(x=Habitats,y=Num, fill = Habitats),trim = FALSE)+ 
  geom_jitter(data = ARGsubtype_df_nonReg_hab_No_subtype,aes(x=Habitats,y=Num, fill = Habitats),width = 0.06,shape=21, alpha = 0.75)+
  geom_boxplot(data = ARGsubtype_df_nonReg_hab_No_subtype,aes(x=Habitats,y=Num), width = 0.055, fill = "white",size = 0.6, outlier.shape = NA)+

  stat_compare_means(data = ARGsubtype_df_nonReg_hab_No_subtype,aes(x = Habitats, y = Num),label.y.npc =0.1,label.x.npc = 'left', size = 8)+
  scale_fill_manual(values = c('#d0f5eb','#fdfedc','#f4d488','#a2bfd8','#fde4ec')) +
  scale_color_manual(values = c('#d0f5eb','#fdfedc','#f4d488','#a2bfd8','#fde4ec')) +
  theme_pubr(border = T, base_size = 18,legend = "none") +labs(y= "Total ARG diversity\n(coverage, x/Gb)")+
  theme(axis.text.x = element_text(vjust = 1, hjust = 1,angle = 30)) +
  geom_text(data = ARGsubtype_df_nonReg_hab_No_subtype_addsig , aes(x=Habitats,y=MAX+50, label=sig), size = 8) 
fig_b

############## freedom forest -----------------------

# library
library(randomForest)
library(rfPermute)
library(A3) #检验整体效应
library(ggplot2)
library(dplyr)
library(ggprism)
library(patchwork)
library(reshape2)
library(ggpubr)
library(cowplot)
library(readxl)

# load
load("./data/5.freedom.forest.ARGabundance.RData")
unique(freedom.forest.ARGabundance$Habitat)
Phy.information <- read_excel("./Data/3.Physicochemical.information.xlsx") # Phylogenetic information

# 
Plot.Go <- freedom.forest.ARGabundance %>% filter(Habitat == "Gobi")
Plot.Gr <- freedom.forest.ARGabundance %>% filter(Habitat == "Grass")
Plot.Fa <- freedom.forest.ARGabundance %>% filter(Habitat == "Farmland")
Plot.Fo <- freedom.forest.ARGabundance %>% filter(Habitat == "Forest")
Plot.T <- freedom.forest.ARGabundance %>% filter(Habitat == "Tailings")

# Farmland --
Plot.Fa$Phy <- row.names(Plot.Fa)
Plot.Fa$Group <- sapply(Plot.Fa$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Fa$Phy <- factor(Plot.Fa$Phy ,level = Plot.Fa$Phy)

#
p_Farmland <- ggplot(Plot.Fa) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,22))+
  theme_pubr(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da", 
                               "MGE" = "#80cbc4")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  #labs(subtitle = "Method Genizi") +
  labs(title = "Farmland") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') 

p_Farmland

# Tailings ---
Plot.T$Phy <- row.names(Plot.T)
Plot.T$Group <- sapply(Plot.T$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.T$Phy <- factor(Plot.T$Phy ,level = Plot.T$Phy)

p_Tailings <-ggplot(Plot.T) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,17))+
  theme_pubr(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da", 
                               "MGE" = "#80cbc4")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  #labs(subtitle = "Method Genizi") +
  labs(title = "Farmland") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') 

p_Tailings

# Forest -----
Plot.Fo$Phy <- row.names(Plot.Fo)
Plot.Fo$Group <- sapply(Plot.Fo$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Fo$Phy <- factor(Plot.Fo$Phy ,level = Plot.Fo$Phy)

p_Forest <-ggplot(Plot.Fo) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,27))+
  theme_pubr(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da", 
                               "MGE" = "#80cbc4")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  #labs(subtitle = "Method Genizi") +
  labs(title = "Forest") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') 

p_Forest

# Grass -----

Plot.Gr$Phy <- row.names(Plot.Gr)
Plot.Gr$Group <- sapply(Plot.Gr$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Gr$Phy <- factor(Plot.Gr$Phy ,level = Plot.Gr$Phy)

p_Grass <- ggplot(Plot.Gr) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,20))+
  theme_pubr(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da", 
                               "MGE" = "#80cbc4")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  #labs(subtitle = "Method Genizi") +
  labs(title = "Farmland") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') 

p_Grass

# Gobi ----

Plot.Go$Phy <- row.names(Plot.Go)
Plot.Go$Group <- sapply(Plot.Go$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Go$Phy <- factor(Plot.Go$Phy ,level = Plot.Go$Phy)

p_Gobi <- ggplot(Plot.Go) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,7))+
  theme_pubr(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da", 
                               "MGE" = "#80cbc4")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  #labs(subtitle = "Method Genizi") +
  labs(title = "Farmland") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') 

p_Gobi

############## correlation analysis -----------------------

load("./data/5.Combined.abundance.RData")

log10_minor_break = function (...){
  
  function(x) {
    
    minx         = floor(min(log10(x), na.rm=T))-1;  # floor(向下取整，向下舍入)
    
    maxx         = ceiling(max(log10(x), na.rm=T))+1; # ceiling(向上舍入)
    
    n_major      = maxx-minx+1;
    
    major_breaks = seq(minx, maxx, by=1)
    
    minor_breaks = 
      
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      
      rep(major_breaks, each = 9)
    
    return(10^(minor_breaks))
    
  }
  
}


library(ggh4x) # 调整ggplot2图形刻度

datColl_colorDf <- cbind.data.frame(dataColl=c("Tailings", "Farmland", "Forest", "Grass","Gobi"),
                                    Color=c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb'),
                                    stringsAsFactors=F)


Colors = sapply(combination$Habitat,
                
                function(x) datColl_colorDf$Color[which(datColl_colorDf$dataColl == x)])

# #0277bd  #f7bfa4  #2e7d32  "#f59f44",  "#cd383d" #d0c0a5 #aedacb
for(yVar in c("MGE.abundance", "ICE.abun", "Integron.abun", "IS.abun", "Plasmid.abun", "Tn.abun", "Phage.abun")){  # colnames(dat)[3:(ncol(dat)-2)]
  
  # yvar = "Phage.abun"
  
  plotDat <- combination 
  
  colnames(plotDat)[which(colnames(plotDat) == yvar)] <- "yVar"
  
  Phage.abun <- ggplot(plotDat,aes(x=yVar, y=Total.ARG.abundance)) + 
    
    geom_point( fill = "#aedacb", shape=21, color = "black",size=4.5, show.legend = FALSE) +
    stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'spearman', 
             label.x.npc = 'left', label.y.npc = 'top', size = 5) + 
    # geom_smooth(method = "lm",na.rm = T,se = T, show.legend = FALSE, size = 1) +
    # stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., stat(p.value.label), sep = '~`,`~')),
    #              label.x.npc = 'right', label.y.npc = 'top', size = 3) +
    
    #facet_wrap(vars(Habitat), scales = "free",  nrow = 1,ncol = 6) +
    
    xlab("Total MGE abundance (coverage ×/Gb)") + ylab("Total ARG abundance (coverage ×/Gb)")  +
    theme_pubr() +  theme(panel.grid = element_blank()) + 
    theme(axis.text = element_text(colour = "gray2")) +
    scale_x_log10(guide = "axis_minor", minor_breaks=log10_minor_break()) +
    scale_y_log10(guide = "axis_minor",minor_breaks=log10_minor_break()) +
    theme(strip.text.x = element_text(margin = margin(0, 0, 0, 0)))   # 调整head title 空隙
  # 使用 geom_text 显示方程
  
  Phage.abun
  
}

MGE.abundance+ Plasmid.abun+ Phage.abun+ ICE.abun + Tn.abun + IS.abun+Integron.abun + plot_layout(ncol = 7, widths = c(1, 1, 1, 1, 1, 1, 1), guides = "collect")

