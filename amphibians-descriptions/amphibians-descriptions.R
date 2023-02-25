# PKGS ----
{
  library(tidyverse)
  library(amphiBR)
  library(ggtext)
  library(ggrepel)
  library(png)
  library(grid)
  library(here)
  library(glue)
  library(geomtextpath)
  library(streamgraph)
}

decades <- paste0(floor(1761:2020/10)*10, "s")

sp_desc <- amphibiansBR %>%
  mutate(year_desc = str_extract(author, "[0-9]{4}") %>% as.numeric(),
         decade = paste0(floor(year_desc/10)*10, "s")) # %>%
  #filter(year_desc >= 1761 & year_desc <=2020 )
  
desc_mean_time <- sp_desc %>%
  group_by(family) %>%
  summarise(first = min(year_desc), last =  max(year_desc), n_sp = n()) %>%
  mutate(avg_year_desc = (((last - first)+ 1)/n_sp)) %>%
  arrange(desc(avg_year_desc)) %>%
  filter(avg_year_desc != 1) %>%
  mutate(sp_order = factor(n():1,level = n():1,labels = family),
         avg_year_desc = round(avg_year_desc,2))




ggplot() +
  geom_bar(data = desc_mean_time,
    aes(x=avg_year_desc, y = fct_rev(fct_inorder(sp_order))),
             width =  .5,
             fill = alpha("#E4D4C8",.5),
             color = alpha("#D0B49F",1),
    stat = "identity") +
  geom_label(data = desc_mean_time,
            aes(x = -3, y = fct_rev(fct_inorder(sp_order)),  label = glue("{n_sp}")),
            family = "Barlow Condensed Medium",
            size = 6,
            fill = "black",
            label.size = 0,
            color = "white",
            vjust = 0.5,
            label.r = unit(0,"lines")) +
  geom_label(data = desc_mean_time,
                 aes(x = 0, y = fct_rev(fct_inorder(sp_order)), label = first ),
                family = "Barlow Condensed Medium",
                size = 6,
                fill = "#F3CC3C",
                label.size = 0,
                color = "black",
                hjust = 1,
                vjust = 0.5,
                label.r = unit(0,"lines"),
             label.padding = unit(0.2,"lines")) +
  geom_label(data = desc_mean_time,
             aes(x = avg_year_desc, y = fct_rev(fct_inorder(sp_order)), label = first ),
             family = "Barlow Condensed Medium",
             size = 6,
             fill = "#5DBDD3",
             label.size = 0,
             color = "black",
             vjust = 0.5,
             hjust = 0,
             label.r = unit(0,"lines"),
             label.padding = unit(0.2,"lines")) +
  geom_rect(
    aes(xmin=25, xmax=50, ymin = 2.5, ymax = 3.5),
    fill = alpha("#E4D4C8",.5),
    color = alpha("#D0B49F",1)
  )+
  geom_richtext(#data = desc_mean_time,
    aes(x = 21.5, y = 3, label = "Total de espécies<br>descritas na família"),
    family = "Barlow Condensed Medium",
    size = 4,
    fill = "black",
    label.size = 0,
    color = "white",
    hjust = 1,
    vjust = 0.5,
    label.r = unit(0,"lines"),
    label.padding = unit(0.3,"lines")) +
  geom_richtext(#data = desc_mean_time,
             aes(x = 25, y = 3, label = "Descrição da primeira<br>espécie na família" ),
             family = "Barlow Condensed Medium",
             size = 4,
             fill = "#F3CC3C",
             label.size = 0,
             color = "black",
             hjust = 0.5,
             vjust = 0.5,
             label.r = unit(0,"lines"),
             label.padding = unit(0.3,"lines")) +
  geom_richtext(
    #data = desc_mean_time,
    aes(x = 50, y = 3, label = "Descrição da espécie<br>mais recente na família"),
    family = "Barlow Condensed Medium",
    size = 4,
    fill = "#5DBDD3",
    label.size = 0,
    color = "black",
    vjust = 0.5,
    hjust = 0.5,
    label.r = unit(0,"lines"),
    label.padding = unit(0.3,"lines")
  ) +
  geom_richtext(
    #data = desc_mean_time,
    aes(x = 37.5, y = 3, label = "Tempo médio em anos entre descrições de espécies na família"),
    family = "Barlow Condensed Medium",
    size = 4.5,
    fill = NA,
    label.size = 0,
    color = "black",
    vjust = 0.5,
    hjust = 0.5,
    label.r = unit(0,"lines"),
    label.padding = unit(0.3,"lines")
  ) +
  #annotate("text", x = 32.5, y = 3, label = "")+
  
  scale_x_continuous(breaks = c(seq(0,14,1),seq(15,60,5)),
    expand = expansion(mult = c(0.03,0.05))) +
  coord_cartesian(clip = "off")+
  theme_bw(base_size = 18,
           base_family = "Barlow Condensed") +
  theme(axis.title = element_blank(),
        axis.text.y = element_markdown(color = "gray10"),
        #axis.ticks.y = element_blank(),
        plot.caption = element_markdown(lineheight = 1.3),
        panel.grid.major = element_line(color = alpha("gray80",.5), linewidth = 0.2, linetype = "dashed"),
        panel.grid.minor = element_line(color = "white", linewidth = 0.2),
        axis.text.x = element_markdown(size = 16, color = "black"),
        plot.title = element_markdown(family = "Barlow Condensed Medium", size = 28),
        plot.subtitle = element_markdown(family = "Carrois Gothic", size = 16, color = "#333333", lineheight = 1.3),
        plot.margin = margin(5,5,5,5)
        
        
  )

# saving ----

file <- here("brlist-amphibians","figures","amphibians_redlist.pdf") 


ggsave(file,
       device = cairo_pdf,
       dpi = 300,
       bg = "white",
       width = 12,
       height = 9,
       units = c("in")
)

# converting png ----

pdftools::pdf_convert(pdf = file,
                      format = "png",
                      dpi = 300, 
                      filenames = glue("{str_remove(file, '.pdf')}.png"),
                      antialias = "both"
)
