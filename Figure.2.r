
# library
library(dplyr)
library(ggpmisc)
library(geosphere)
library(ggpubr)
library(vegan)
library(stringr)


######################################## DRR ----------------------

load("./data/2.MGE.distance.tmp.habitat.Rdata") # 服务器算的bray和地理的距离

# 1.1 Fa
mantel_r <- vegan::mantel(d.geo_fa, bray_fa,  method = 'pearson', permutations = 999, na.rm = TRUE)

dis_fa <- cbind(data.frame(geo = as.vector(as.dist(d.geo_fa))) , data.frame(bray=as.vector(bray_fa)) ) %>% 
  ggplot(aes(x = geo/1000000, y = bray))+
  geom_point(shape = 21,fill = '#addd8e' ,size =6 ,alpha = 0.5)+
  geom_smooth(method = 'lm' ,colour = 'blue',alpha = 0)+
  stat_poly_eq(formula = y~x,size=5,family="serif",method = "lm",color="black",output.type = "numeric", 
               parse = T,label.x = 0.4,hjust=0,label.y =0.3,
               mapping =aes(label = paste("italic(R)^2~`=`","~",sprintf("\"%#.*f\"",3,after_stat(`r.squared`)),"*\"\"*",
                                          ifelse(after_stat(`p.value`)<=0.001,"'***'",
                                                 ifelse(after_stat(`p.value`)<=0.01,"'**'",
                                                        ifelse(after_stat(`p.value`)<=0.05,"'*'",NA))),"*\", \"*",
                                          "italic(Slope)~`=`","~",sprintf("\"%#.*f\"",3,after_stat(b_1)),sep = ""))) +
  labs(y = 'MGE Bray-Curtis dissimilarity' ,x = 'geo distance (km)') +
  theme_test(base_size = 22,base_family = 'serif')+
  theme(plot.subtitle = element_text(family = "serif", size = 15, colour = "gray0")) + 
  labs(subtitle = paste("(b) Farmland",round(mantel_r$statistic,digits = 3),round(mantel_r$signif,digits = 3)) )+ 
  theme(axis.ticks = element_line(colour = "gray0"), 
        axis.text = element_text(colour = "gray0"), 
        axis.text.x = element_text(colour = "black")) 

dis_fa

#fo
vegan::mantel(d.geo_fo, bray_fo,  method = 'pearson', permutations = 999, na.rm = TRUE) -> mantel_r

dis_fo <- cbind(data.frame(geo = as.vector(as.dist(d.geo_fo))) , data.frame(bray=as.vector(bray_fo)) ) %>% 
  ggplot(aes(x = geo/1000000, y = bray))+
  geom_point(shape = 21,fill = '#f4d488' ,size =6 ,alpha = 0.5)+
  geom_smooth(method = 'lm' ,colour = 'blue',alpha = 0)+
  stat_poly_eq(formula = y~x,size=5,family="serif",method = "lm",color="black",output.type = "numeric", parse = T,label.x = 0.4,hjust=0,label.y =0.3,
               mapping =aes(label = paste("italic(R)^2~`=`","~",sprintf("\"%#.*f\"",3,after_stat(`r.squared`)),"*\"\"*",
                                          ifelse(after_stat(`p.value`)<=0.001,"'***'",
                                                 ifelse(after_stat(`p.value`)<=0.01,"'**'",
                                                        ifelse(after_stat(`p.value`)<=0.05,"'*'",NA))),"*\", \"*",
                                          "italic(Slope)~`=`","~",sprintf("\"%#.*f\"",3,after_stat(b_1)),sep = "")))+
  labs(y = 'MGE Bray-Curtis dissimilarity' ,x = 'geo distance (km)')+
  theme_test(base_size = 22,base_family = 'serif')+
  theme(plot.subtitle = element_text(family = "serif", size = 15, colour = "gray0")) +
  labs(subtitle = paste("(c) Forest",round(mantel_r$statistic,digits = 3),round(mantel_r$signif,digits = 3)) )+ 
  theme(axis.ticks = element_line(colour = "gray0"), 
        axis.text = element_text(colour = "gray0"), 
        axis.text.x = element_text(colour = "black"))
 
dis_fo

#ta
vegan::mantel(d.geo_t, bray_t,  method = 'pearson', permutations = 999, na.rm = TRUE) -> mantel_r

dis_t <- cbind(data.frame(geo = as.vector(as.dist(d.geo_fa))) , data.frame(bray=as.vector(bray_fa)) ) %>% 
  ggplot(aes(x = geo/1000000, y = bray))+
  geom_point(shape = 21,fill = '#EA5C15' ,size = 6 ,alpha = 0.5)+
  geom_smooth(method = 'lm' ,colour = 'blue',alpha = 0)+
  stat_poly_eq(formula = y~x,size=5,family="serif",method = "lm",color="black",output.type = "numeric", parse = T,label.x = 0.4,hjust=0,label.y =0.3,
               mapping =aes(label = paste("italic(R)^2~`=`","~",sprintf("\"%#.*f\"",3,after_stat(`r.squared`)),"*\"\"*",
                                          ifelse(after_stat(`p.value`)<=0.001,"'***'",
                                                 ifelse(after_stat(`p.value`)<=0.01,"'**'",
                                                        ifelse(after_stat(`p.value`)<=0.05,"'*'",NA))),"*\", \"*",
                                          "italic(Slope)~`=`","~",sprintf("\"%#.*f\"",3,after_stat(b_1)),sep = "")))+
  labs(y = 'MGE Bray-Curtis dissimilarity' ,x = 'geo distance (km)')+
  theme_test(base_size = 22,base_family = 'serif')+
  theme(plot.subtitle = element_text(family = "serif", size = 15, colour = "gray0")) +
  labs(subtitle = paste("(a) Tailings",round(mantel_r$statistic,digits = 3),round(mantel_r$signif,digits = 3)) )+ 
  theme(axis.ticks = element_line(colour = "gray0"), 
        axis.text = element_text(colour = "gray0"), 
        axis.text.x = element_text(colour = "black"))

dis_t

#go
vegan::mantel(d.geo_go, bray_go,  method = 'pearson', permutations = 999, na.rm = TRUE) -> mantel_r

dis_go <- cbind(data.frame(geo = as.vector(as.dist(d.geo_fa))) , data.frame(bray=as.vector(bray_fa)) ) %>% 
  ggplot(aes(x = geo/1000000, y = bray))+
  geom_point(shape = 21,fill = '#7986cb' ,size = 6 ,alpha = 0.5)+
  geom_smooth(method = 'lm' ,colour = 'blue',alpha = 0)+
  stat_poly_eq(formula = y~x,size=5,family="serif",method = "lm",color="black",output.type = "numeric", parse = T,label.x = 0.4,hjust=0,label.y =0.3,
               mapping =aes(label = paste("italic(R)^2~`=`","~",sprintf("\"%#.*f\"",3,after_stat(`r.squared`)),"*\"\"*",
                                          ifelse(after_stat(`p.value`)<=0.001,"'***'",
                                                 ifelse(after_stat(`p.value`)<=0.01,"'**'",
                                                        ifelse(after_stat(`p.value`)<=0.05,"'*'",NA))),"*\", \"*",
                                          "italic(Slope)~`=`","~",sprintf("\"%#.*f\"",3,after_stat(b_1)),sep = "")))+
  labs(y = 'Plasmid distance' ,x = 'geo distance (km)')+
  theme_test(base_size = 22,base_family = 'serif')+
  theme(plot.subtitle = element_text(family = "serif", size = 15, colour = "gray0")) +
  labs(subtitle = paste("(e) Gobi desert",round(mantel_r$statistic,digits = 3),round(mantel_r$signif,digits = 3)) )+ 
  theme(axis.ticks = element_line(colour = "gray0"), 
        axis.text = element_text(colour = "gray0"), 
        axis.text.x = element_text(colour = "black"))

dis_go

#gr
vegan::mantel(d.geo_gr, bray_gr,  method = 'pearson', permutations = 999, na.rm = TRUE) -> mantel_r

dis_gr <- cbind(data.frame(geo = as.vector(as.dist(d.geo_fa))) , data.frame(bray=as.vector(bray_fa)) ) %>% 
  ggplot(aes(x = geo/1000000, y = bray))+
  geom_point(shape = 21,fill = '#2c7fb8' ,size =6 ,alpha = 0.5)+
  geom_smooth(method = 'lm' ,colour = 'blue',alpha = 0)+
  stat_poly_eq(formula = y~x,size=5,family="serif",method = "lm",color="black",output.type = "numeric", parse = T,label.x = 0.4,hjust=0,label.y =0.3,
               mapping =aes(label = paste("italic(R)^2~`=`","~",sprintf("\"%#.*f\"",3,after_stat(`r.squared`)),"*\"\"*",
                                          ifelse(after_stat(`p.value`)<=0.001,"'***'",
                                                 ifelse(after_stat(`p.value`)<=0.01,"'**'",
                                                        ifelse(after_stat(`p.value`)<=0.05,"'*'",NA))),"*\", \"*",
                                          "italic(Slope)~`=`","~",sprintf("\"%#.*f\"",3,after_stat(b_1)),sep = "")))+
  labs(y = 'Plasmid distance' ,x = 'geo distance (km)')+
  theme_test(base_size = 22,base_family = 'serif')+
  theme(plot.subtitle = element_text(family = "serif", size = 15, colour = "gray0")) +
  labs(subtitle = paste("(d) Grassland",round(mantel_r$statistic,digits = 3),round(mantel_r$signif,digits = 3)) )+ 
  theme(axis.ticks = element_line(colour = "gray0"), 
        axis.text = element_text(colour = "gray0"), 
        axis.text.x = element_text(colour = "black"))

dis_gr

# combined
library(cowplot)
plot_grid(dis_ta, dis_fa, dis_fo, dis_gr, dis_go, ncol = 5)  

##################################### global map  --------------------

# load data
load("./data/2.Map.diversity.RData")
MGE.diversity.df$Lat <- as.double(MGE.diversity.df$Lat)
MGE.diversity.df$Long <- as.double(MGE.diversity.df$Long)

MGE.diversity.df$regin <- c(1:287)

# 
p <- ggplot() + 
  geom_polygon(data=map_data('world'), aes(x=long, y=lat, group=group), fill='#eae7d4') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.75), 
        axis.line = element_line(color = "black")) + 
  scale_x_continuous(breaks=c(-180,-120, -60, 0, 60, 120, 180), expand=c(0,0),
                     labels=c('180°','120°W','60°W','0','60°E','120°E','180°'))+ 
  scale_y_continuous(breaks=c(-90, -60, -30, 0, 30, 60, 90),expand=c(0, 0),
                     labels=c('90°S','60°S','30°S','0','30°N','60°N', '90°N'))+ 
  labs(x='Longitude',y='Latitude')

p

#
MGE.diversity.df$MGE.diversity <- MGE.diversity.df$MGE.diversity/10000 
MGE.diversity.df$r <- sqrt(MGE.diversity.df$MGE.diversity)*3

p1 <- p +
  geom_scatterpie(data = MGE.diversity.df, sorted_by_radius = T,
                  aes(x = Long, y = Lat, r = r, 
                      fill = factor(c("Plasmid", "Phage", "ICE", "Tn", "IS","Integron"))),
                  cols = c("Plasmid", "Phage", "ICE", "Tn", "IS","Integron"),
                  alpha = 0.75, 
                  color = "gray20", 
                  size = 0.02) +
  scale_fill_manual(name = "", 
                    values = c("Plasmid" = "#cd383d", "Phage" = "#aedacb", "ICE" = "#f7bfa4", 
                               "Tn" = "#d0c0a5", "IS" = "#f59f44", "Integron" = "#2e7d32")) +
  geom_point(data = MGE.diversity.df, 
             aes(x = Long, y = Lat, size = MGE.diversity), 
             show.legend = TRUE, color = "transparent") +
  
  theme_bw()+
  theme(plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0),
        plot.caption = element_markdown(face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_line(color = "black"),
        axis.text = element_text(face = "bold", family="serif"),
        axis.title = element_text(face = "bold", family="serif"))
p1 

# 
p2 <- p1 + scatterpie::geom_scatterpie_legend(MGE.diversity.df$MGE.diversity,
                                              x = 0, y = -60,
                                              n = 3, 
                                              labeller = function(x) x) 
p2

