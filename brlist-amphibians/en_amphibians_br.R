# PKGS ----
{
  library(tidyverse)
  library(BRlist)
  library(ggtext)
  library(ggrepel)
  library(png)
  library(grid)
  library(here)
  library(glue)
  
}

# path ----

# endangered counts ----

spp_ameaca <- amphibiaBRlist %>%
  count(category) %>%
  mutate(ordem = c(3,4,2,5,1)) %>%
  arrange(ordem) %>%
  mutate(x = 14,
         y = seq(from = 8,to = 1,by = -1.5),
         label = glue("<b>{n}</b> spp")
  )

# order ----

ordem <- amphibiaBRlist %>%
  count(family) %>%
  arrange(desc(n)) %>%
  mutate(ordem_lvl = 1:n()) %>%
  mutate(family_fct = factor(ordem_lvl, levels = ordem_lvl, labels = family)) %>%
  rename(n_total = n)

# colors ----

cores <- rev(c("#FFD600","#FF9100","#E53935","#37474F","#0D0D0D"))

# legend ----

legenda <- tibble(labels = c("VU","EN","CR","CR (PEX)","EX"),
                  labels_alt = c("VU","EN","CR","PEX","EX"),
                  x = (15),
                  y = seq(from = 8,to = 1,by = -1.5),
                  color = c("black","black","white","white","red"))

# exctinct ----

extintas <- tibble(y = c(13.8,3.8),
                   yend = c(13,7),
                   x = c(13,7),
                   xend = c(11,6),
                   labels = c("<i>Boana cymbalum</i>","<i>Phrynomedusa fimbriata</i>"),
                   hjust = c(0.5,0),
                   xlabel = c(10.6,6),
                   ylabel = c(13,7.2))

# grobs ----

rip <- rasterGrob(readPNG(here("brlist-amphibians","assets","rip.png")), interpolate = TRUE)
pb <- rasterGrob(readPNG(here("brlist-amphibians","assets","pbavatar.png")), interpolate = TRUE)

# PLOT ----

amphibiaBRlist %>%
  count(family,category) %>%
  left_join(ordem, by = "family") %>%
  mutate(family_fct = factor(ordem_lvl, levels = ordem_lvl, labels = family)) %>%
  ggplot()+
  geom_bar(aes(x=fct_reorder(family_fct,ordem_lvl, .desc = T),y = n,
               fill = fct_rev(fct_relevel(category,c("VU","EN","CR","CR (PEX)","EX")))),
           stat = "identity") +
  geom_text(data = ordem,
            aes(x = family_fct,
                y = n_total,
                label = family),
            hjust = 0,
            nudge_y = .1,
            size = 6,
            family = "Barlow Condensed") +
  geom_point(data = legenda,
             aes(x = y, 
                 y = x,
                 fill = labels),
             size = 22,
             shape = 21,
             color = alpha("black",.6),
  ) +
  geom_richtext(data = spp_ameaca,
                aes(x = y, 
                    y = x,
                    label = label),
                family = "Barlow Condensed",
                size = 7,
                #fontface = "italic",
                fill = NA,
                color = NA,
                text.colour = "black") +
  geom_richtext(data = legenda,
                aes (x =y,
                     y = x,
                     label = labels_alt),
                fill = NA,
                color = NA,
                text.colour = c("black","black","white","white","red"),
                family = "Barlow Condensed Medium",
                size = 9,
                nudge_x = -0.1) +
  guides(fill = "none") +
  geom_curve(data = extintas,
             aes(x = x, y = y, xend =xend, yend = yend),
             arrow = arrow(angle = 30, length = unit(0.1, "inches"),
                           ends = "last", type = "closed"),
             curvature = .2,
             linewidth = .8
  ) +
  annotation_custom(rip, xmin = 6.5, xmax = 5.5, ymin =  9.3, ymax = 11.1) +
  annotation_custom(rip, xmin = 10.2, xmax = 11.2, ymin =  13.3, ymax = 15.1) +
  annotation_custom(pb, xmin = -2.5, xmax = 0.5, ymin =  0, ymax = 4) +
  geom_richtext(data = extintas,
                aes(x = xlabel,y=ylabel, label = labels,
                    hjust = hjust),
                family = "Barlow Condensed",
                size = 6,
                #fontface = "italic",
                fill = NA,
                color = NA,
                text.colour = "black"
  ) +
  labs(title = "Anfíbios do Brasil ameaçados de Extinção - Portaria MMA 300/2022",
       subtitle = "Avaliação realizada pelo Centro Nacional de Pesquisa e Conservação de Répteis e Anfíbios (RAN/ICMBio).<br/>Foram <b>avaliadas 1096 espécies</b>, sendo <b>59 consideradas ameaçadas de extinção</b>.<br/>A avaliação considerou as espécies brasileiras descritas até Novembro de 2018.",
       caption = "<b>VU:</b> vulnerável, <b>EN:</b> ameaçada, <b>CR:</b> criticamente ameaçada,<br/><b>PEX:</b> criticamente ameaçada e provavelmente extinta, <b>EX:</b> extinta.<br/>Dados disponiveis no pacote <b>BRlist</b> no github.") +
  coord_flip(clip = "off")+
  scale_fill_manual(values = cores) +
  scale_y_continuous(breaks = seq(0,20,1),
                     expand = expansion(mult= c(0.01,0.05)))+
  theme_bw(base_size = 18,
           base_family = "Barlow Condensed") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_markdown(lineheight = 1.3),
        panel.grid.major = element_line(color = alpha("gray80",.3), linewidth = 0.2, linetype = "dashed"),
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
