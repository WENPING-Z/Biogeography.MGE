

########## freedom.forest -------------------- 

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

load("./Data/3.freedom.forest.MGEabundance.RData")
unique(freedom.forest.ARGabundance$Habitat)
Phy.information <- read_excel("./Data/3.Physicochemical.information.xlsx") # Phylogenetic information

# 
Plot.Go <- freedom.forest.ARGabundance %>% filter(Habitat == "Gobi")
Plot.Gr <- freedom.forest.ARGabundance %>% filter(Habitat == "Grass")
Plot.Fa <- freedom.forest.ARGabundance %>% filter(Habitat == "Farmland")
Plot.Fo <- freedom.forest.ARGabundance %>% filter(Habitat == "Forest")
Plot.T <- freedom.forest.ARGabundance %>% filter(Habitat == "Tailings")

#
Plot.Fa$Phy <- row.names(Plot.Fa)
Plot.Fa$Group <- sapply(Plot.Fa$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Fa$Phy <- factor(Plot.Fa$Phy ,level = rev(Plot.Fa$Phy))

p_Farmland <- ggplot(Plot.Fa) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,18))+
  theme_bw(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  labs(title = "Farmland") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') + coord_flip() 

p_Farmland

#
Plot.T$Phy <- row.names(Plot.T)
Plot.T$Group <- sapply(Plot.T$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.T$Phy <- factor(Plot.T$Phy ,level = rev(Plot.T$Phy))

p_Tailings <-ggplot(Plot.T) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,16))+
  theme_bw(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  labs(title = "Tailings") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') + coord_flip() 

p_Tailings

#
Plot.Fo$Phy <- row.names(Plot.Fo)
Plot.Fo$Group <- sapply(Plot.Fo$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Fo$Phy <- factor(Plot.Fo$Phy ,level = rev(Plot.Fo$Phy))

p_Forest <-ggplot(Plot.Fo) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,20))+
  theme_bw(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  labs(title = "Forest") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') + coord_flip()

p_Forest

#
Plot.Gr$Phy <- row.names(Plot.Gr)
Plot.Gr$Group <- sapply(Plot.Gr$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Gr$Phy <- factor(Plot.Gr$Phy ,level = rev(Plot.Gr$Phy))

p_Grass <- ggplot(Plot.Gr) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,15))+
  theme_bw(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  labs(title = "Grass") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') + coord_flip()

p_Grass

#
Plot.Go$Phy <- row.names(Plot.Go)
Plot.Go$Group <- sapply(Plot.Go$Phy, function(x) Phy.information$Group[which(Phy.information$Factor == x)])
Plot.Go$Phy <- factor(Plot.Go$Phy ,level = rev(Plot.Go$Phy))

p_Gobi <- ggplot(Plot.Go) +
  aes(x = Phy, weight = X.IncMSE, fill = Group) +
  geom_bar(color = 'black',alpha = 0.9 ,width = 0.8) +
  scale_y_continuous(expand = c(0, 0))+
  expand_limits(y=c(0,7))+
  theme_bw(base_size = 17)+ 
  scale_fill_manual(values = c("Physicochemical factor" = "#f0f4c3",
                               "Geographical factor" = "#1781B5",
                               "Microbiome abundance" = "#ffe0b2",
                               "Climate factor" = "#C9F4FF",
                               "Metal factor" = "#9fa8da")) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none") +labs(x = "", y = "Increased in MSE (%)")+ 
  labs(title = "Gobi") +
  theme(axis.title = element_text(size = 20, vjust = 1)) + 
  theme(axis.text.x = element_text(vjust = 1,hjust = 1)) + 
  labs(x = '') + coord_flip()

p_Gobi

##################### Procrust test -----------------------

# load
Procruster.result <- read_excel("./Data/3.Procruster.result.xlsx")

# 
Procruster.result$Habitat <- factor(Procruster.result$Habitat, 
                                    levels = rev(c("Tailings", "Farmland", "Forest", "Grassland", "Gobi")))
Procruster.result$MGE.type <- factor(Procruster.result$MGE.type, 
                                     levels = c("Plasmid", "Phage", "ICE", "Transposon", "IS", "Integron")) 

# 
Procruster.result %>% 
  ggplot(aes(x = MGE.type, y = Habitat, fill = M2)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#ffd180", "#e65100"), 
    breaks = c(0, 0.2, 0.5, 0.8),                   
    labels = c("0", "0.2", "0.5", "0.8") 
  ) +
  
  theme_test(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  
    axis.ticks = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  labs(x = "",y = "") 


###################### links ------------------------
Correlation.result <- read_excel("./data/3.Procruster.result.xlsx", sheet = "Sheet2")
Correlation.result$Habitat <- factor(Procruster.result$Habitat, 
                                     levels = rev(c("Tailings", "Farmland", "Forest", "Grassland", "Gobi")))
Correlation.result

# 
Correlation.result <- Correlation.result %>% 
  mutate(log_links = log10(No.of.links)) # 使用log10变换处理量级差异

# 绘制热图
ggplot(Correlation.result, aes(x = "Links", y = factor(Habitat, levels = Habitat), fill = log_links)) +
  geom_tile(color = "Black", linewidth = 0.5) +   # 添加白色边框
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#bbdefb", "#1976d2"),  # 蓝白渐变
    breaks = c(2, 3, 4, 5, 6),                   # 对应log10值
    labels = c("100", "1,000", "10,000", "100,000", "1,000,000") # 原始值标注
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Number of Links"
  ) +
  theme_test(base_size = 16) +
  theme(
    axis.text.x = element_blank(),                # 隐藏x轴文字
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  ) 


