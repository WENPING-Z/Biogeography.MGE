

############################ mobile ARGs abundance -------------

# library
library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(ggbreak)

# load
load("./data/6.Mobility.RData")

PlotData <- PlotDat %>% group_by(Treatment, mobility) %>% summarise(Avg.abundance = mean(Abundance))

PlotData$mobility <- factor(PlotData$mobility, levels = c("unknow","mobile"))
PlotData$Treatment <- factor(PlotData$Treatment, levels = c("Tailings","Farmland", "Forest", "Grassland", "Gobi"))

PlotData <- PlotData %>% group_by(Treatment) %>% mutate(Total = sum(Avg.abundance))
PlotData$Percentage <- PlotData$Avg.abundance/PlotData$Total*100

p_mobile.percentage.abundance.1 <- ggplot(PlotData, aes(x = Treatment, y = Percentage, fill = mobility)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Habitat", y = "Percentage of MGE-carrying ACC abundance (%)") +
  scale_fill_manual(values = c("mobile" = "#4db6ac", "unknow" = "grey85")) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white"),
        legend.position = "right") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

p_mobile.percentage.abundance.1

p_mobile.percentage.abundance.2 <- p_mobile.percentage.abundance.1 +  scale_y_break(c(10, 12), scales = 0.2, space = 0.1,)
p_mobile.percentage.abundance.2

########### MGE-ARG information -----------------

# library
library(dplyr)
library(reshape2)
library(ggplot2)
library(data.table)
library(rstatix)
library(patchwork)
library(ggpubr)

# load
load("./data/6.MGE.ARG.diversity.RData")

PlotData <- ARG.diversity.mobile.diversity.df %>% group_by(Habitat, MGE.type) %>%
  summarise(Avg = mean(percentage), Sd = sd(percentage))

PlotData$Habitat <- factor(PlotData$Habitat, levels = c("Tailings", "Farmland", "Forest", "Grass", "Gobi"))
PlotData$MGE.type <- factor(PlotData$MGE.type, levels = c("plasmid", "phage","ICE","Tn", "IS",  "integron"))

p_diversity <- ggplot(PlotData, aes(x = MGE.type, y = Avg)) +
  
  geom_errorbar(aes(ymin = Avg - Sd, ymax = Avg + Sd, fill = MGE.type), width = 0.3) +
  geom_point(aes(fill = MGE.type), size = 5, shape = 21, alpha = 1) +
  facet_wrap(. ~ Habitat, nrow =1, scales = "free_y") +
  ylim(0,25) +
  labs(x = "", y = "Percentage of ARG subtypes co-occuring with MGEs") +
  scale_color_manual(values = c( "#cd383d" ,"#aedacb", "#f7bfa4", "#d0c0a5","#f59f44",  "#2e7d32"
  ))+
  scale_fill_manual(values = c("#cd383d" ,"#aedacb", "#f7bfa4", "#d0c0a5","#f59f44",  "#2e7d32"))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    legend.position = "right"
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) 

p_diversity

# 差异性计算 ---
compare_star_df <- NULL
compare_letter_df <- NULL

for (hab in unique(ARG.diversity.mobile.diversity.df$Habitat)) {
  # hab = unique(ARG.diversity.mobile.diversity.df$Habitat)[1]
  
  data_df <- ARG.diversity.mobile.diversity.df %>% filter(Habitat == hab)
  
  compare_df <- compare_means(percentage ~ MGE.type,  data = data_df, method = "wilcox.test", p.adjust.method = "fdr") %>%  data.frame()
  
  letter_df1 <- compare_df %>% mutate(Comparison =paste(group1, group2,sep="-"))%>% dplyr::select(Comparison, p.adj)
  letter_df2 <- compare_df %>% mutate(Comparison = paste(group2, group1,sep="-"))%>% dplyr::select(Comparison, p.adj) #为保证与a-b排序一致，俩都保留，与下面取交集
  letter_df <- rbind(letter_df1,letter_df2)
  
  letter_df <- data.frame(a=letter_df[,1], b=letter_df[,2]) # 
  
  model = aov(percentage ~ MGE.type,  data = data_df)
  TUK = TukeyHSD(model, ordered = T)
  
  # 
  combined_df <- merge(letter_df,
                       data.frame(TUK$MGE.type), 
                       by.x = "a", by.y = 0) %>% arrange(desc(diff))
  
  Test <- rcompanion::cldList(b~ a, data = combined_df,
                              threshold = 0.05,
                              remove.space=F)
  # 
  compare_df <- compare_df %>% mutate(Habitat = hab)
  Test <- Test %>% mutate(Habitat = hab)
  
  # combined
  compare_star_df <- bind_rows(compare_star_df, compare_df)
  
  compare_letter_df <- bind_rows(compare_letter_df, Test)
  
}

########### MGE-MGE-ARG information -----------------

load("./data/6.MGE-MGE-ARG.diversity.RData")

PlotData <- No.of.MGE.MGE.carrying.ARGsubtype.sp

ggplot(PlotData, aes(x = Group, y = percentage)) +
  # geom_point(aes(color = Group.2, fill = Group.2, shape = Group.2), 
  #            size = 3, shape = 10, position = "jitter" ) +
  # geom_jitter(aes(fill = Group.2, shape = Group.2), 
  #             size = 2.5, width = 0.2, color = "black", alpha = 0.7) +
  geom_boxplot(aes(fill = Group.2), 
               color = "black", alpha = 0.7, linewidth = 0.3) +
  facet_wrap(. ~ Habitat, nrow =1, scales = "free_y") +
  #scale_shape_manual(values = c(23,21, 24)) +
  scale_fill_manual(values = c("#509dc2", "#f3e6b3", "#388e3c")) +
  #scale_color_manual(values = c("#509dc2", "#f3e6b3", "#388e3c")) +
  labs(x = "", y = "Percentage of ARG subtypes co-occuring with MGEs") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    legend.position = "right"
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) 


#

compare_star_df <- NULL
compare_letter_df <- NULL

for (hab in unique(No.of.MGE.MGE.carrying.ARGsubtype.sp$Habitat)) {
  # hab = unique(No.of.MGE.MGE.carrying.ARGsubtype.sp$Habitat)[1]
  
  data_df <- No.of.MGE.MGE.carrying.ARGsubtype.sp %>% filter(Habitat == hab)
  
  compare_df <- compare_means(percentage ~ Group,  data = data_df, method = "wilcox.test", p.adjust.method = "fdr") %>%  data.frame()
  
  letter_df1 <- compare_df %>% mutate(Comparison =paste(group1, group2,sep="-"))%>% dplyr::select(Comparison, p.adj)
  letter_df2 <- compare_df %>% mutate(Comparison = paste(group2, group1,sep="-"))%>% dplyr::select(Comparison, p.adj) #为保证与a-b排序一致，俩都保留，与下面取交集
  letter_df <- rbind(letter_df1,letter_df2)
  
  letter_df <- data.frame(a=letter_df[,1], b=letter_df[,2]) # 为什么这里要重新
  

  model = aov(percentage ~ Group,  data = data_df)
  TUK = TukeyHSD(model, ordered = T)
  
  
  combined_df <- merge(letter_df,
                       data.frame(TUK$Group), 
                       by.x = "a", by.y = 0) %>% arrange(desc(diff))
  
  Test <- rcompanion::cldList(b~ a, data = combined_df,
                              threshold = 0.05,
                              remove.space=F)
  # 
  compare_df <- compare_df %>% mutate(Habitat = hab)
  Test <- Test %>% mutate(Habitat = hab)
  
  # combined
  compare_star_df <- bind_rows(compare_star_df, compare_df)
  
  compare_letter_df <- bind_rows(compare_letter_df, Test)
  
}

compare_star_df.div.MGE <- compare_star_df
compare_letter_df.div.MGE <- compare_letter_df

######## top 20 MGE-ARG subtypes -------------

# library
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(ggbreak)

# load

p1.T <- ggplot(Mobile.T.sim.df) +
  geom_bar(aes(x = ARGsubtype, y = Avg.DepthPG, fill =  MGE.type),  stat = "identity", 
           color = "black", alpha = 1)+
  scale_fill_manual(values = c("plasmid" = "#cd383d", "phage" = "#aedacb",
                               "ICE" = "#f7bfa4", "IS" = "#f59f44",
                               "Tn" = "#d0c0a5", "integron" = "#2e7d32",
                               "Nomobile" = "grey95")) +  # 颜色
  #expand_limits(y=c(0,100))+
  #scale_y_continuous(limits = c(0, 100), oob = scales::oob_squish, breaks = seq(0, 100, 10)) +
  labs(x = "", y = "Average abundance of ARGs (Coverage, ×/Gb)", fill = "MGE.type") + 
  #ylim(0,300) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # 移除次要网格线
  ) + coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

p1.T

# 2. Farmland -----------

p1.Fa <- ggplot(Mobile.Fa.sim.df) +
  geom_bar(aes(x = ARGsubtype, y = Avg.DepthPG, fill =  MGE.type),  stat = "identity", 
           color = "black", alpha = 1)+
  scale_fill_manual(values = c("plasmid" = "#cd383d", "phage" = "#aedacb",
                               "ICE" = "#f7bfa4", "IS" = "#f59f44",
                               "Tn" = "#d0c0a5", "integron" = "#2e7d32",
                               "Nomobile" = "grey95")) +  # 颜色
  #expand_limits(y=c(0,100))+
  #scale_y_continuous(limits = c(0, 100), oob = scales::oob_squish, breaks = seq(0, 100, 10)) +
  labs(x = "", y = "Average abundance of ARGs (Coverage, ×/Gb)", fill = "MGE.type") + 
  #ylim(0,300) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # 移除次要网格线
  ) + coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

p1.Fa

# 3. Forest -----------
p1.Fo <- ggplot(Mobile.Fo.sim.df) +
  geom_bar(aes(x = ARGsubtype, y = Avg.DepthPG, fill =  MGE.type),  stat = "identity", 
           color = "black", alpha = 1)+
  scale_fill_manual(values = c("plasmid" = "#cd383d", "phage" = "#aedacb",
                               "ICE" = "#f7bfa4", "IS" = "#f59f44",
                               "Tn" = "#d0c0a5", "integron" = "#2e7d32",
                               "Nomobile" = "grey95")) +  # 颜色
  #expand_limits(y=c(0,100))+
  #scale_y_continuous(limits = c(0, 100), oob = scales::oob_squish, breaks = seq(0, 100, 10)) +
  labs(x = "", y = "Average abundance of ARGs (Coverage, ×/Gb)", fill = "MGE.type") + 
  #ylim(0,300) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # 移除次要网格线
  ) + coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

p1.Fo

# 4. Grassland -----------

p1.Gr <- ggplot(Mobile.Gr.sim.df) +
  geom_bar(aes(x = ARGsubtype, y = Avg.DepthPG, fill =  MGE.type),  stat = "identity", 
           color = "black", alpha = 1)+
  scale_fill_manual(values = c("plasmid" = "#cd383d", "phage" = "#aedacb",
                               "ICE" = "#f7bfa4", "IS" = "#f59f44",
                               "Tn" = "#d0c0a5", "integron" = "#2e7d32",
                               "Nomobile" = "grey95")) +  # 颜色
  #expand_limits(y=c(0,100))+
  #scale_y_continuous(limits = c(0, 100), oob = scales::oob_squish, breaks = seq(0, 100, 10)) +
  labs(x = "", y = "Average abundance of ARGs (Coverage, ×/Gb)", fill = "MGE.type") + 
  #ylim(0,300) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # 移除次要网格线
  ) + coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


p1.Gr
# 以高4 宽 8 导出  5.mobile.ARG.Gr

# 5. Gobi -----------
p1.Go <- ggplot(Mobile.Go.sim.df) +
  geom_bar(aes(x = ARGsubtype, y = Avg.DepthPG, fill =  MGE.type),  stat = "identity", 
           color = "black", alpha = 1)+
  scale_fill_manual(values = c("plasmid" = "#cd383d", "phage" = "#aedacb",
                               "ICE" = "#f7bfa4", "IS" = "#f59f44",
                               "Tn" = "#d0c0a5", "integron" = "#2e7d32",
                               "Nomobile" = "grey95")) +  # 颜色
  #expand_limits(y=c(0,100))+
  #scale_y_continuous(limits = c(0, 100), oob = scales::oob_squish, breaks = seq(0, 100, 10)) +
  labs(x = "", y = "Average abundance of ARGs (Coverage, ×/Gb)", fill = "MGE.type") + 
  #ylim(0,300) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # 移除次要网格线
  ) + coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

p1.Go

# 以高4 宽 8 导出  5.mobile.ARG.Go





