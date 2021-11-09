


install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

# libraries
require("pacman")
pacman::p_load(spData, spDataLarge, sf, tidyverse, janitor, ggrepel, viridis, hrbrthemes, 
               RColorBrewer, ggtext, cowplot, patchwork)

# Data
## shp file
rm(list = ls())

africa_geom <- world %>%                                     #world from sf package
  mutate(across(where(is.character),str_trim)) %>%          #remove  whitespace 
  dplyr::filter(continent == "Africa") %>%                  # africa only 
  rename(name = name_long) %>%                              #rename colname to match other dataset
  mutate_at(c("name"), funs(toupper))                        #match case to upper

## dataset
afcon_df <- afcon  %>% 
  mutate(across(where(is.character),str_trim)) %>%  #remove whitespace 
  mutate_at(                                        #rename any mismatching country names
    "name", recode,
    "IVORY COAST" = "CÔTE D'IVOIRE",
    "ZAIRE" = "DEMOCRATIC REPUBLIC OF THE CONGO",
    "CONGO" = "REPUBLIC OF THE CONGO"
  )


## merge 
comprehensive_df <-  afcon_df %>%
  left_join(
    ., africa_geom, by = "name"
  ) 

#rename DRC to a shorter name
comprehensive_df$name<-gsub("DEMOCRATIC REPUBLIC OF THE CONGO", "DRC", comprehensive_df$name)

# Plot
## plot A
africa_map<- ggplot() +
  geom_sf(data= africa_geom, aes(geometry = geom), fill = "#FAFAFA") +  # africa map, no other data
  geom_sf(data= comprehensive_df, aes(geometry = geom, fill = totcon))+  # overlay 
  geom_label(data=subset(comprehensive_df, totcon > 1500), aes(x, y, label = name),
             size = 2.3, alpha = 0.9) +
  scale_fill_viridis()+
  guides(fill=guide_legend(title= "Total Conflict")) +
  theme_void()


## plot B
plotB <- comprehensive_df %>%
  dplyr::filter(! is.na(subregion)) %>%
  ggplot() +
  geom_point(aes(pop/1000000, totcon, color= subregion))+
  scale_color_ipsum() +
  ylab("Total conflict") +
  xlab("Population (x10^6)")+
  theme_ipsum()


# Bring it all together

final_plot<- ggdraw(xlim = c(0, 30), ylim = c(0, 30)) +
  draw_plot(africa_map, x = 1, y = 10, width = 17, height = 20) +
  draw_plot(plotB, x = 15, y = 0, width = 15, height = 15) +
  annotate(geom = "text", label = "The map to the left displays spatial pattern of  \n conflict in 42 African countries, 1966-78.\n Countries with total conflict > 1500 are named.", 
           x = 18, y = 25, size = 4, hjust = 0, color = "black",
           lineheight = 1.6) +
  annotate(geom = "richtext", label = "A scatter plot of total conflict against population. \n \n Countries are coloured by subregion: \n \n <span style='color: #d18975;'>Eastern</span>, 
           <span style='color: #8fd175;'>Middle</span>, 
           <span style='color: #3f2d54;'>Northern</span>,
           <span style='color: #75b8d1;'>Southern</span> and <span style='color: #2d543d;'>Western</span> Africa.", 
           x = 2, y = 5, size = 4, hjust = 0, color = "black",
           lineheight = 1.6)  # not sure why I have to use newline characyer twice though :(



final_plot +
  plot_annotation(title = 'Where is the conflict happening in Africa?',
                  caption = "@martha_mawia | Data: Anselin, L. and John O'Loughlin (1992), Anselin, L. 1995.",
                  theme = theme(plot.title = element_text(size = 18,
                                                          colour = "black", family = "Optima",
                                                          face = "bold"),
                                plot.caption = element_text(size = 7,
                                                            colour = "black")))


ggsave(paste0("Africa_conflict.png"),
       dpi = 640,
       width = 9,
       height = 8)

