

############## HGT frequency ------------------------------

load("./data/7.HGT.efficiency.RData")
total.orfCluster <- read_excel("./data/7.orfCluster.information.xlsx")

HGT.efficiency.df$No.of.orfCluster <- sapply(HGT.efficiency.df$Treatment, function(x)
  total.orfCluster$No.of.orfCluster[which(total.orfCluster$Treatment == x)])
HGT.efficiency.df$Group <- sapply(HGT.efficiency.df$Treatment, function(x)
  total.orfCluster$Group[which(total.orfCluster$Treatment == x)])

HGT.efficiency.df$percentage <- HGT.efficiency.df$Num/HGT.efficiency.df$No.of.orfCluster*100

# plot
PlotData <- HGT.efficiency.df
PlotData$Treatment <- factor(PlotData$Treatment, levels = c("Tailings", "Farmland", "Forest" , "Grassland", "Gobi"))

p_HGT <- ggplot(data = PlotData, aes(x = Treatment, y = percentage)) + 
  geom_bar(stat = "identity", fill = "#1781b5", color = "#1781b5") + 
  scale_fill_manual(values = c("#1781b5","#c9f4ff")) +
  scale_color_manual(values = c("#1781b5","#c9f4ff")) +
  labs(x = "", y = "HGT efficiency (%)", fill = "Category") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # 移除次要网格线
  )

p_HGT

############## HGT abundance -----------------------------

load("./data/7.HGT.abundance.RData")

plotDat.totalAbund <- plotDat.HGT.ARG %>% 
  dplyr::group_by(Treatment) %>% dplyr::summarise(Abundance = sum(uniARG.mean))

plotDat.HGT.ARG$totalAbund <- sapply(plotDat.HGT.ARG$Treatment, 
                                     function(x) plotDat.totalAbund$Abundance[plotDat.totalAbund$Treatment == x])

# 
plotDat.HGT.ARG <- plotDat.HGT.ARG %>%
  mutate(fraction = uniARG.mean/totalAbund) %>%
  mutate(ymax= cumsum(fraction)) %>%
  mutate(ymin= c(0, head(ymax, n=-1))) %>%
  mutate(label_pos = (ymax + ymin) / 2) %>% 
  arrange(desc(uniARG.mean))

plotDat.HGT.ARG <- plotDat.HGT.ARG %>% filter(category3 != "Notransfer")
plotDat.HGT.ARG <- plotDat.HGT.ARG %>% mutate(fraction = fraction*100)

# filter
plotDat.HGT.ARG$Treatment <- factor(plotDat.HGT.ARG$Treatment, 
                                    levels = c("Tailings", "Farmland", "Forest" , "Grassland", "Gobi"))

P_HGTpercentage <- ggplot(plotDat.HGT.ARG) + # xmin ~ xmax is where the ring is 
  geom_col(aes(x= Treatment, y = fraction, fill = category3))+
  scale_fill_manual(values = c("#1565c0", "#eceff1")) +
  theme_bw() +  
  labs(x = "", y = "Percentage of transferable ARGs") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), # 调整X轴标签角度和位置
    axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # 移除次要网格线
  )

P_HGTpercentage

############## Chi.test ------------------------------------

load("./data/7.Chi.test.RData")
#
PlotDat <- plotD.trans.mobil.depend.treatment %>% filter(Mobility == "mobile")
PlotDat$Freq <- PlotDat$Freq*100

PlotDat$Treatment <- factor(PlotDat$Treatment, levels = c("Tailings", "Farmland", "Forest", "Grassland", "Gobi"))
PlotDat$Transferability <- factor(PlotDat$Transferability, levels = c("transferable", "Notransferable"))

p.correlation.mobile.HGT <- ggplot(PlotDat) +
  geom_col(aes(x=Transferability, y=Freq, fill= Transferability), color = "black") + #有dependency 但区别不明显
  scale_fill_manual(values=c("#1660ab", "#f5f3de")) +
  facet_wrap(.~ Treatment, nrow = 1, scales = "free_y") + 
  theme_pubr(base_size = 12) + 
  #scale_y_continuous (expand = c (0, 0)) +
  theme(panel.grid = element_blank()) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    legend.position = "right",
    strip.text = element_text(size = 13),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(colour = "black")) + 
  xlab("") + 
  ylab("Percentage of mobile ARG (%)") 

p.correlation.mobile.HGT

#
