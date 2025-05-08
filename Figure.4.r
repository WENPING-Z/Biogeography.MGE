
# library
library(dplyr)
library(ggplot2)
library(highcharter)
library(ggsci)
library(data.table)
library(stringr)

load("./data/4.PloatData.RData")

##### IS --------
IS.shared.filter.df$ProcessedHabitat <- as.character(IS.shared.filter.df$ProcessedHabitat)

IS.shared.filter.df <- IS.shared.filter.df %>% filter(Shared.type == "crossHabitat") %>% 
  group_by(ProcessedHabitat) %>% summarise(Count = n())

across.df <- IS.shared.filter.df

across.df <- across.df %>% mutate(Habitat1 = sapply(strsplit(across.df$ProcessedHabitat, ".", fixed = T), "[[", 1),
                                  Habitat2 = sapply(strsplit(across.df$ProcessedHabitat, ".", fixed = T), "[[", 2)) %>%
  select(Habitat1, Habitat2, Count)


colnames(across.df) <- c("from" , "to", "weight")
across.df$to <- paste0(across.df$to, ".","1")

#
ordered_levels <- c("Tailings.1", "Farmland.1", "Forest.1", "Grassland.1", "Desert.1")
across.df$to <- factor(across.df$to, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(to, levels = ordered_levels))

ordered_levels <- c("Tailings", "Farmland", "Forest", "Grassland", "Desert")
across.df$from <- factor(across.df$from, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(from, levels = ordered_levels))


# 
highchart( ) %>%
  hc_chart(type = 'sankey', backgroundColor = 'white',borderRadius = 45) %>%
  hc_add_series(data = across.df,
                colorByPoint = TRUE, 
                colors = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) %>%
  hc_colorAxis(dataClassColor = "category") %>% 
  hc_add_theme(hc_theme_gridlight())  #

##### Phage -------
Phage.shared.filter.df <- Phage.shared.filter.df %>% filter(Shared.type == "crossHabitat") %>% 
  group_by(ProcessedHabitat) %>% summarise(Count = n()) %>%
  mutate(Habitat1 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 1),
         Habitat2 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 2)) %>%
  select(Habitat1, Habitat2, Count)

across.df <- Phage.shared.filter.df

colnames(across.df) <- c("from" , "to", "weight")
across.df$to <- paste0(across.df$to, ".","1")

#
ordered_levels <- c("Tailings.1", "Farmland.1", "Forest.1", "Grassland.1", "Desert.1")
across.df$to <- factor(across.df$to, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(to, levels = ordered_levels))

ordered_levels <- c("Tailings", "Farmland", "Forest", "Grassland", "Desert")
across.df$from <- factor(across.df$from, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(from, levels = ordered_levels))

#  
highchart( ) %>%
  hc_chart(type = 'sankey', backgroundColor = 'white',borderRadius = 45) %>%
  hc_add_series(data = across.df,
                colorByPoint = TRUE, 
                colors = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) %>%
  hc_colorAxis(dataClassColor = "category") %>% 
  hc_add_theme(hc_theme_gridlight())  


##### Plasmid ---------
Plasmid.shared.filter.df <- Plasmid.shared.filter.df %>% filter(Shared.type == "crossHabitat") %>% 
  group_by(ProcessedHabitat) %>% summarise(Count = n()) %>%
  mutate(Habitat1 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 1),
         Habitat2 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 2)) %>%
  select(Habitat1, Habitat2, Count)

across.df <- Plasmid.shared.filter.df

colnames(across.df) <- c("from" , "to", "weight")
across.df$to <- paste0(across.df$to, ".","1")

#
ordered_levels <- c("Tailings.1", "Farmland.1", "Forest.1", "Grassland.1", "Desert.1")
across.df$to <- factor(across.df$to, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(to, levels = ordered_levels))

ordered_levels <- c("Tailings", "Farmland", "Forest", "Grassland", "Desert")
across.df$from <- factor(across.df$from, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(from, levels = ordered_levels))


# 
highchart( ) %>%
  hc_chart(type = 'sankey', backgroundColor = 'white',borderRadius = 45) %>%
  hc_add_series(data = across.df,
                colorByPoint = TRUE, 
                colors = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) %>%
  hc_colorAxis(dataClassColor = "category") %>% 
  hc_add_theme(hc_theme_gridlight())  

##### ICE ---------

ICE.shared.filter.df <- ICE.shared.filter.df %>% filter(Shared.type == "crossHabitat") %>% 
  group_by(ProcessedHabitat) %>% summarise(Count = n()) %>%
  mutate(Habitat1 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 1),
         Habitat2 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 2)) %>%
  select(Habitat1, Habitat2, Count)

across.df <- ICE.shared.filter.df

colnames(across.df) <- c("from" , "to", "weight")
across.df$to <- paste0(across.df$to, ".","1")

#
ordered_levels <- c("Tailings.1", "Farmland.1", "Forest.1", "Grassland.1", "Desert.1")
across.df$to <- factor(across.df$to, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(to, levels = ordered_levels))

ordered_levels <- c("Tailings", "Farmland", "Forest", "Grassland", "Desert")
across.df$from <- factor(across.df$from, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(from, levels = ordered_levels))


#  
highchart( ) %>%
  hc_chart(type = 'sankey', backgroundColor = 'white',borderRadius = 45) %>%
  hc_add_series(data = across.df,
                colorByPoint = TRUE, 
                colors = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) %>%
  hc_colorAxis(dataClassColor = "category") %>% 
  hc_add_theme(hc_theme_gridlight())


##### Tn ----------
Tn.shared.filter.df <- Tn.shared.filter.df %>% filter(Shared.type == "crossHabitat") %>% 
  group_by(ProcessedHabitat) %>% summarise(Count = n()) %>%
  mutate(Habitat1 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 1),
         Habitat2 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 2)) %>%
  select(Habitat1, Habitat2, Count)

across.df <- Tn.shared.filter.df

colnames(across.df) <- c("from" , "to", "weight")
across.df$to <- paste0(across.df$to, ".","1")

#
ordered_levels <- c("Tailings.1", "Farmland.1", "Forest.1", "Grassland.1", "Desert.1")
across.df$to <- factor(across.df$to, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(to, levels = ordered_levels))

ordered_levels <- c("Tailings", "Farmland", "Forest", "Grassland", "Desert")
across.df$from <- factor(across.df$from, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(from, levels = ordered_levels))


#  
highchart( ) %>%
  hc_chart(type = 'sankey', backgroundColor = 'white',borderRadius = 45) %>%
  hc_add_series(data = across.df,
                colorByPoint = TRUE, 
                colors = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) %>%
  hc_colorAxis(dataClassColor = "category") %>% 
  hc_add_theme(hc_theme_gridlight())  

##### Integron ---------
Integron.shared.filter.df <- Integron.shared.filter.df %>% filter(Shared.type == "crossHabitat") %>% 
  group_by(ProcessedHabitat) %>% summarise(Count = n()) %>%
  mutate(Habitat1 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 1),
         Habitat2 = sapply(strsplit(ProcessedHabitat, ".", fixed = T), "[[", 2)) %>%
  select(Habitat1, Habitat2, Count)

across.df <- Integron.shared.filter.df

colnames(across.df) <- c("from" , "to", "weight")
across.df$to <- paste0(across.df$to, ".","1")

#
ordered_levels <- c("Tailings.1", "Farmland.1", "Forest.1", "Grassland.1", "Desert.1")
across.df$to <- factor(across.df$to, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(to, levels = ordered_levels))

ordered_levels <- c("Tailings", "Farmland", "Forest", "Grassland", "Desert")
across.df$from <- factor(across.df$from, levels = ordered_levels)
across.df <- across.df %>% arrange(factor(from, levels = ordered_levels))


# 
highchart( ) %>%
  hc_chart(type = 'sankey', backgroundColor = 'white',borderRadius = 45) %>%
  hc_add_series(data = across.df,
                colorByPoint = TRUE, 
                colors = c('#EA5C15','#addd8e','#f4d488','#2c7fb8','#7986cb')) %>%
  hc_colorAxis(dataClassColor = "category") %>% 
  hc_add_theme(hc_theme_gridlight())  





