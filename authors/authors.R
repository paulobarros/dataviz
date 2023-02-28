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
  library(RCurl)
  library(showtext)
}

pb <- rasterGrob(readPNG(getURLContent("https://raw.githubusercontent.com/paulobarros/dataviz/main/brlist-amphibians/assets/pbavatar.png")),
                 interpolate = TRUE)

fa_path <- systemfonts::font_info(family = "Font Awesome 6 Free Solid")[["path"]]
font_add(family = "fa-solid", regular = fa_path)

authors <- amphibiansBR %>%
  select(order,family,species,author) %>%
  mutate(year = str_extract(author, "[0-9]{4}")) %>%
  mutate(authors_list = str_remove_all(author, ",\\s[0-9]{4}|\\(|\\)") %>%
           str_replace("&",",") %>%
           str_remove_all("\\s"))
        
auth_list <- authors$authors_list %>%
    str_split(",",simplify = TRUE) %>% as_tibble(.name_repair = "universal") %>%
  rename_at(vars(everything()),~paste0("author_",1:16)) %>%
  mutate(across(everything(),~if_else(.x == "", NA_character_, .x))) %>%
  mutate(n_auth = rowSums(!is.na(select(., everything()))), .before = 1) %>%
  mutate(author_4 = if_else(author_4 == "Bal-do", "Baldo",author_4),
         author_4 = if_else(author_4 == "Her-nández-Ruz","Hernández-Ruz",author_4),
         author_4 = if_else(author_4 == "Hernández-Ruz","Hernández-Ruíz",author_4),
         author_2 = if_else(author_2 == "Veiga-Menon-cello","Veiga-Menoncello",author_2)) %>%
  mutate(across(starts_with("author"),~if_else(.x == "Hernández-Ruz","Hernández-Ruíz",.x)),
         author_3 = if_else(author_3 == "BlancnewoccurrenceCosta-", "Blanc",author_3))
  
  
authors_data <- bind_cols(authors %>%
  select(order,family,species,year),
  auth_list %>%
    select(n_auth)) %>%
  mutate(decade = paste0(floor(as.numeric(year)/10)*10, "s"))



authorship <- authors_data %>%
  group_by(decade) %>%
  count(species) %>%
  group_by(decade) %>%
  reframe(spp_per_decade = sum(n)) %>%
  left_join(
    authors_data %>%
      group_by(decade) %>%
      reframe(max_auth = max(n_auth)),
    
    by = "decade"
 
  ) %>%
  mutate(century = case_when(
    str_detect(decade,"17") ~ "SEC<br>XVIII",
    str_detect(decade,"18") ~ "SEC<br>XIX",
    str_detect(decade,"19") ~ "SEC<br>XX",
    str_detect(decade,"20") ~ "SEC<br>XXI"
  ),
  # dec_fonts = case_when(
  #   str_detect(decade,"17") ~ glue("<span style='font-family:Garamond;font-weight:bold;'>{decade}</span>"),
  #   str_detect(decade,"18") ~ glue("<span style='font-family:Elephant;font-weight:bold;'>{decade}</span>"),
  #   TRUE ~ glue("<span style='font-family:Dosis'>{decade}</span>")
  # ),
  x_dec = rep(c(4.90,5.10),14),
  y_dec = seq(0.5,14,0.5),
  x_tick = rep(c(4.94,5),14),
  xend_tick = rep(c(5,5.06),14),
  dec_hjust = rep(c(1,0),14),
  x_spp = rep(c(4.65,5.37),14),
  x_auth = rep(c(4.65,5.37),14),
  )

col_decades <- c("#180537","#003125","#4B4453","#006151")

leg_text <- tibble(x = c(5.60,5.60),
                   y = c(5,4),
                   label = c("<span style='color:#F8B152;font-family: \"Font Awesome 6 Free Solid\"'>&#xf52e;</span><span style='color:#63184E'>...</span>Espécies descritas na década.",
                             "<span style='color:#63184E;font-family: \"Font Awesome 6 Free Solid\"'>&#xf0c0;</span><span style='color:#F8B152'>...</span>Maior número de Autores numa descrição."),
                   type = c("spp","auth")
                   )

seta <- tibble(x=5.4, xend = 5.65, y = 14, yend = 13)

papers <- tibble(x = c(5.60,5.60,5.60),
                 y = c(13,12.5,11.5),
                 labels = c("<b><i>Leptodactylus paranaru</i></b>",
                            "<b><i>Leptodactylus payaya</i></b>",
                            "O trabalho de descrição destas duas espécies<br>contou com 16 autores no total.<br><br>Magalhães, F. de M., M. L. Lyra, T. R. de Carvalho,<br>D. Baldo, F. Brusquetti, P. Burella, G. R. Colli, M. C. Gehara,<br>A. A. Giaretta, C. F. B. Haddad, J. A. Langone, J. A. López,<br>M. F. Napoli,D. J. Santana, R. O. de Sá, and A. A. Garda. 2020.<br><b>Taxonomic review of South American Butter Frogs:<br>Phylogeny, geographic patterns, and species<br>delimitation in the Leptodactylus latrans species group<br>(Anura: Leptodactylidae).</b>Herpetological Monographs 34: 131–177.")) 

titulo <- tibble(x = c(4.5),
                 y = c(18),
                 label = "Quem quer ser<br>Taxonomista?")

subtitulo <- tibble(x = 5.25,
                    y = 17,
                    label = "Explorando o número máximo de autores em<br>um artigo de descrição de anfíbio ao longo das décadas.")

ggplot(data = authorship) +
  # ---  TIMELINE -----------------
  geom_richtext(aes(y = y_dec, x = x_dec, label = decade,hjust = dec_hjust),
                nudge_y = 0,
                label.colour = NA,
                fill = NA,
                color = "#180537",
                size = 7,
                vjust = 0.5,
                family = "Highway Gothic Narrow",
                #fontface = "bold"
  )+
  geom_segment(aes(x = x_tick, xend = xend_tick, y=y_dec, yend= y_dec),
               color = "#180537",
               linewidth = 1)+
  geom_segment(aes(x = 5, xend =5, y = -0.5, yend = 14.5),
             color = "#180537",
             linewidth = 1.2)+
  # SETAS ------
  geom_curve(data = seta,
             aes(x=x,xend=xend,y=y,yend=yend),
             color = "#180537" ,
             curvature = -0.2,
             linewidth = 0.7,
  arrow = arrow(angle = 30, length = unit(0.1, "inches"),
              ends = "last", type = "closed")
  )+
  # ----- SPP COUNT
  geom_richtext(aes(y = y_dec, x = x_spp, label = glue("<span style='font-family: \"Font Awesome 6 Free Solid\"'>&#xf52e;</span><span style='color:#63184E'>...</span>{spp_per_decade}")),
                hjust = 1,
                label.colour = NA,
                fill = "#63184E",
                size = 8,
                color = "#F8B152",
                vjust = 0.5,
                family = "Barlow Semi Condensed",
                #fontface = "bold",
                label.padding = unit(c(0.35, 0.6, 0.0, 0.6), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")
                
  )+
  # ----- AUTHORS COUNT
  geom_richtext(aes(y = y_dec, x = x_auth, label = glue("{max_auth}<span style='color:#F8B152'>...</span><span style='font-family: \"Font Awesome 6 Free Solid\"'>&#xf0c0;</span>")),
                #nudge_x = 0.005,
                hjust = 0,
                label.colour = NA,
                fill = "#F8B152",
                size = 8,
                color = "#63184E",
                vjust = 0.5,
                family = "Barlow Semi Condensed",
                #fontface = "bold",
                label.padding = unit(c(0.35, 0.6, 0.0, 0.6), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")
  )+
  # ---- LEGEND -----
  geom_richtext(data = leg_text %>% filter(type == "spp"),
                aes(x=x,y=y,label = label),
                fill = "#63184E",
                hjust = 0,
                size = 8,
                color = "#F8B152",
                vjust = 0.5,
                label.colour = NA,
                family = "Barlow Semi Condensed",
                label.padding = unit(c(0.35, 0.6, 0.2, 0.6), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")
                ) +
  geom_richtext(data = leg_text %>% filter(type == "auth"),
                aes(x=x,y=y,label = label),
                fill = "#F8B152",
                hjust = 0,
                size = 8,
                color = "#63184E",
                vjust = 0.5,
                label.colour = NA,
                family = "Barlow Semi Condensed",
                label.padding = unit(c(0.35, 0.6, 0.2, 0.6), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")
  ) +
  # PAPER -------------
  geom_richtext(data = papers,
                aes(x=x,y=y, label=labels),
                fill = NA,
                hjust = 0,
                vjust = 1,
                label.padding = unit(c(0.35, 0.6, 0.2, 0.6), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines"),
                label.colour = NA,
                family = "Barlow Semi Condensed",
                size = 6,
                )+ 
  # TITLE -------
geom_richtext(data = titulo,
              aes(x=x,y=y, label=label),
              fill = NA,
              hjust = 0,
              vjust = 1,
              label.padding = unit(c(0.35, 0.6, 0.2, 0.6), "lines"),
              label.margin = unit(c(0, 0, 0, 0), "lines"),
              label.r = unit(0, "lines"),
              label.colour = NA,
              family = "Sunday Best",
              #fontface = "bold",
              size = 14,
              color = "#180537"
)+ 
  # SUBTITLE -------
geom_richtext(data = subtitulo,
              aes(x=x,y=y, label=label),
              fill = NA,
              hjust = 0,
              vjust = 0.5,
              label.padding = unit(c(0.35, 0.6, 0.2, 0.6), "lines"),
              label.margin = unit(c(0, 0, 0, 0), "lines"),
              label.r = unit(0, "lines"),
              label.colour = NA,
              family = "Barlow Semi Condensed",
              #fontface = "bold",
              size = 9,
              color = "#180537"
)+ 
  # # ------ SIGNATURE ---------
  annotation_custom(pb, xmin = 5.3, xmax = 6.8, ymin =  0, ymax = 2) +
  # -------------------- TRICKERY ----------------------------
  guides(colour = "none")+
  scale_color_manual(values = col_decades)+
  scale_x_continuous(breaks = seq(4.5,7,0.05),
                     expand = expansion(mult = c(0.1,0.7))
                     ) +
  scale_y_continuous(expand = expansion(mult = c(0.005,0.01)))+
  coord_cartesian(clip = "off")+
  labs(
    caption= "Dados do pacote <b>amphiBR</b> disponível em: <b>paulobarros.github.io/amphiBR</b>")+
  theme_bw(base_size = 18,
           base_family = "Barlow Semi Condensed"
           ) +
   theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_markdown(lineheight = 1.3),
        #panel.grid.major = element_line(color = alpha("gray80",.5), linewidth = 0.2, linetype = "dashed"),
        #panel.grid.minor = element_line(color = "white", linewidth = 0.2),
        #axis.text.x = element_markdown(family = "Dosis",size = 18,  color = alpha("#0B0909",.9), angle = 90),
        plot.title = element_markdown(family = "Barlow Semi Condensed", size = 28),
        plot.margin = margin(30,30,30,5),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
        
  )

# saving ----

file <- here("authors","figures","authors.pdf") 


ggsave(file,
       device = cairo_pdf,
       dpi = 500,
       bg = "white",
       width = 16,
       height = 12,
       units = c("in")
)

# converting png ----

pdftools::pdf_convert(pdf = file,
                      format = "png",
                      dpi = 300, 
                      filenames = glue("{str_remove(file, '.pdf')}.png"),
                      antialias = "both"
)



