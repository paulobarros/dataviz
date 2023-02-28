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
  library(systemfonts)
}

pb <- rasterGrob(readPNG(here("amphibians-descriptions","assets","pbavatar.png")), interpolate = TRUE)

sp_desc <- amphibiansBR %>%
  mutate(year_desc = str_extract(author, "[0-9]{4}") %>% as.numeric(),
         decade = paste0(floor(year_desc/10)*10, "s")) # %>%
  #filter(year_desc >= 1761 & year_desc <=2020 )

taxons <- sp_desc %>%
  select(order,family) %>%
  unique()

median_years <- sp_desc %>%
  arrange(family, year_desc) %>%
  group_by(family) %>%
  reframe(diff_years = (lead(year_desc)-year_desc)+1) %>%
  group_by(family) %>%
  reframe(median_year = median(diff_years, na.rm = T)) %>%
  arrange(desc(median_year)) %>%
  mutate(median_year = if_else(family == "Alsodidae",1,median_year ))
  
desc_mean_time <- sp_desc %>%
  group_by(family) %>%
  summarise(first = min(year_desc), last =  max(year_desc), n_sp = n()) %>%
  mutate(avg_year_desc = (((last - first)+ 1)/n_sp)) %>%
  left_join(median_years, by = "family") %>%
  left_join(taxons, by = "family") %>%
  filter(order == "Anura") %>%
  arrange(order,desc(median_year),n_sp) %>%
  mutate(sp_order = factor(n():1,level = n():1,labels = family),
         median_year = round(median_year,2))
  
  


setas_bu <- tibble(x = c(37,42),
                xend = c(31,45),
                y = c(6),
                yend = c(4,10),
                colour = c("black","#D0B49F"))

setas_bd <- tibble(x = c(40,44),
                   xend = c(35,47),
                   y = c(6),
                   yend = c(9,4),
                   colour = c("#F3CC3C","#5DBDD3"))

setas_text <- tibble(x = c(30.5,34.8,45,47.2),
                     y = c(4.2,9,10,4),
                     hjust = c(1,1,0,0),
                     label = c("Total de espécies<br>na família",
                               "Ano em que foi descrita<br/>a primeira espécie<br/>na família",
                               "Tempo médio (mediana) em anos<br/>entre as descrições de espécies<br>na família",
                               "Ano de descrição<br/>mais recente<br/>na família"))

ggplot() +
  # ------------- BARPLOT --------------------
  geom_bar(data = desc_mean_time,
    aes(x=median_year, y = fct_rev(fct_inorder(sp_order))),
             width =  .5,
             fill = alpha("#E4D4C8",.5),
             color = alpha("#D0B49F",1),
    stat = "identity") +
  # --------------- YEARS -----------------------------
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
             aes(x = median_year, y = fct_rev(fct_inorder(sp_order)), label = last ),
             family = "Barlow Condensed Medium",
             size = 6,
             fill = "#5DBDD3",
             label.size = 0,
             color = "black",
             vjust = 0.5,
             hjust = 0,
             label.r = unit(0,"lines"),
             label.padding = unit(0.2,"lines")) +
  # ----------- SETAS -----------------------
  geom_curve(
    data = setas_bu,
    aes(x =x, xend = xend, y=y, yend=yend),
    colour=setas_bu$colour,
    arrow = arrow(angle = 30, length = unit(0.1, "inches"),
                  ends = "last", type = "closed"),
    curvature = -0.5,
    linewidth = .8
  )+
  geom_curve(
    data = setas_bd,
    aes(x =x, xend = xend, y=y, yend=yend),
    colour=setas_bd$colour,
    arrow = arrow(angle = 30, length = unit(0.1, "inches"),
                  ends = "last", type = "closed"),
    curvature = 0.5,
    linewidth = .8
  )+
  # ------------------ LEGEND --------------------------
  geom_rect(
    aes(xmin=40, xmax=45, ymin =5.75 , ymax = 6.35),
    fill = alpha("#E4D4C8",.5),
    color = alpha("#D0B49F",1)
  ) +
  geom_label(#data = desc_mean_time,
    aes(x = 37, y = 6, label = "XXX"),
    family = "Barlow Condensed Medium",
    size = 6,
    fill = "black",
    label.size = 0,
    color = "white",
    hjust = 0.5,
    vjust = 0.5,
    label.r = unit(0,"lines"),
    label.padding = unit(0.5,"lines"),
    #label.colour = NA
    ) +
  geom_label(
    #data = desc_mean_time,
    aes(x = 40, y = 6, label = "XXXX" ),
    family = "Barlow Condensed Medium",
    size = 6,
    fill = "#F3CC3C",
    label.size = 0,
    color = "black",
    hjust = 0.5,
    vjust = 0.5,
    label.r = unit(0,"lines"),
    label.padding = unit(0.5,"lines"),
    #label.colour = NA
    ) +
  geom_label(
    #data = desc_mean_time,
    aes(x = 45, y = 6, label = "XXXX"),
    family = "Barlow Condensed Medium",
    size = 6,
    fill = "#5DBDD3",
    label.size = 0,
    color = "black",
    vjust = 0.5,
    hjust = 0.5,
    label.r = unit(0,"lines"),
    label.padding = unit(0.5,"lines"),
    #label.colour = NA
  ) +
  # ----- TEXTO SETAS ------
  geom_richtext(
    data = setas_text,
    aes(x = x, y = y, label = label, hjust = hjust),
    family = "Barlow Semi Condensed",
    size = 6,
    fill = NA,
    label.size = 0,
    color = "gray20",
    vjust = 0.5,
    label.r = unit(0,"lines"),
    label.padding = unit(0.5,"lines"),
    label.colour = NA
  ) +
  # ------------------ PLOT TEXT INFO ------------------------
  geom_richtext(
    aes(x = 58, y = 16.5, label = "Quanto tempo leva...?"),
    family = "Flotta",
    size = 14,
    fill = NA,
    label.size = 0,
    color = "black",
    vjust = 1,
    hjust = 1,
    label.r = unit(0,"lines"),
    label.padding = unit(0.2,"lines"),
    label.colour = NA
  ) +
  geom_richtext(
    aes(x = 58, y = 15, label = "Explorando o tempo médio de descrição de novas<br>espécies nas famílias de <b>Anuros do Brasil.</b><br>Foram consideradas as <b>descrições entre 1758 e 2021.</b>"),
    family = "Barlow Semi Condensed",
    size = 6,
    fill = NA,
    label.size = 0,
    color = "gray30",
    vjust = 1,
    hjust = 1,
    label.r = unit(0,"lines"),
    label.padding = unit(0.2,"lines"),
    label.colour = NA
  ) +
  # ------ SIGNATURE ---------
  annotation_custom(pb, xmin = -11, xmax = 1, ymin =  3, ymax = -4) +
  # -------------------- TRICKERY ----------------------------
  guides(colour = "none")+
  scale_x_continuous(breaks = c(seq(0,14,1),seq(15,60,5)),
    expand = expansion(mult = c(0.02,0.04))) +
  coord_cartesian(clip = "off")+
  labs(caption= "Dados do pacote <b>amphiBR</b> disponível em: <b>paulobarros.github.io/amphiBR</b>")+
  theme_bw(base_size = 18,
           base_family = "Barlow Semi Condensed") +
  theme(axis.title = element_blank(),
        axis.text.y = element_markdown(family = "Barlow Semi Condensed",color = "gray10"),
        #axis.ticks.y = element_blank(),
        plot.caption = element_markdown(lineheight = 1.3),
        panel.grid.major = element_line(color = alpha("gray80",.5), linewidth = 0.2, linetype = "dashed"),
        panel.grid.minor = element_line(color = "white", linewidth = 0.2),
        axis.text.x = element_markdown(family = "Barlow Semi Condensed",size = 16, color = "gray40"),
        plot.title = element_markdown(family = "Barlow Semi Condensed", size = 28),
        plot.margin = margin(5,5,10,5)
        
        
  )

# saving ----

file <- here("amphibians-descriptions","figures","amphibians_desc.pdf") 


ggsave(file,
       device = cairo_pdf,
       dpi = 300,
       bg = "white",
       width = 16,
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
