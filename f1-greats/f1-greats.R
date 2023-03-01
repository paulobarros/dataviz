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

# Avatar -----
pb <- rasterGrob(readPNG(here("f1-greats","assets","pb.png")), interpolate = TRUE)
asphalt <- rasterGrob(readPNG(here("f1-greats","assets","asphalt.png")), interpolate = TRUE)
f1logo <- rasterGrob(readPNG(here("f1-greats","assets","f1logo.png")), interpolate = TRUE)
goats <- rasterGrob(readPNG(here("f1-greats","assets","goats.png")), interpolate = TRUE)

fa_path <- systemfonts::font_info(family = "Font Awesome 6 Free Solid")[["path"]]
sysfonts::font_add(family = "fa-solid", regular = fa_path)

# Data -----
# Source: <https://en.wikipedia.org/wiki/List_of_Formula_One_World_Drivers%27_Champions>

f1 <- read_csv(here("f1-greats","assets","drivers_champs.csv")) %>%
  filter(str_detect(driver, "Michael Schumacher|Lewis Hamilton")) %>%
  mutate(title = rep(paste0(1:7,c("st","nd","rd","th","th","th","th")),2), .before = year,
         driver = str_remove(driver, "(\\[\\d+\\])") %>% str_trim("both")) %>%
  select(title,year,driver,chassis,age,poles,wins,podiums,fastest_lap,pct_points,pct_margin,races_remaining) %>%
  mutate(across(age:races_remaining,~as.numeric(.x)),
         driver_fac = factor(rep(c(1,3),each=7),rep(c(1,3),each=7),labels = rep(c("Michael Schumacher","Lewis Hamilton"),7)),
         ovr_season = rowSums(across(poles:races_remaining))
         ) %>%
  #pivot_longer(age:races_remaining, names_to = "stats", values_to = "value") %>%
  arrange(desc(ovr_season)) %>%
  mutate(grid_pos = rep(2:3,7),
         pos_label = 1:14)
  

# COLORS -----

b194 <- c("#035f2c","#2771c8","#609edb")

# HEADER TEXT -----

header <- tibble(x = c(1.8,1.8),
                 y = c(17,16),
                 label = c("A league of their own...",
                           "Ranking the world titles of the two<br>greatests pilots of all time"),
                 type = c("title","subtitle"))

# ICONS ----
legend <- tibble(x = c(2),
                 y = c(0.4,-0.1,-0.6,-1.1),
                 label = c(
"<span style='color:#E1C340;font-family: \"Font Awesome 6 Free Solid\"'>&#xf091;</span> <b>Race Wins</b>",
"<span style='color:#A16AE8;font-family: \"Font Awesome 6 Free Solid\"'>&#xf2f2;</span> <b>Pole Positions</b>",
"<span style='color:#B4FEE7;font-family: \"Font Awesome 6 Free Solid\"'>&#xe561;</span> <b>Podiums</b>",
"<span style='color:#B8EE30;font-family: \"Font Awesome 6 Free Solid\"'>&#xf201;</span> <b>Overall Season Performance</b>: The sum of all season stats:<br>total points percentage, poles, fastest laps,<br>remaining races when clinching title,<br>podiums and margin to the second place in points percentage.<br><br>Data Source: Wikipedia. Links on github Readme."
                 ))

# DUMMY DATA ----

dummy <- tibble(x = c(2,3,2,3),
                y = c(-1,-4,15,16),
                value = c(NA,NA,NA,NA)
                )

# CARS -----------

b194_car <- rasterGrob(readPNG(here("f1-greats","assets","B194.png")), interpolate = TRUE)

# PLOT ------

ggplot(data = f1
       ) +
  annotation_custom(asphalt,
                    xmin= -Inf,
                    xmax= Inf,
                    ymin= -Inf,
                    ymax= Inf
                    )+
  # Dummy data --------
  geom_point(data = dummy,
             aes(x=x,y=y))+
  # GRID POSITION SEASON -------
  geom_point(aes(y=fct_rev(fct_inorder(as.factor(year))),x = grid_pos),
           size = 44,
           color = "#f0d508",
           shape = 15) +
  # F1 LOGO
  annotation_custom(f1logo, xmin = 3, xmax =3.7, ymin = -1, ymax =-1.7)+
  # GOATS -----------
annotation_custom(goats, xmin = 2.8, xmax =3.8, ymin = 13.5, ymax =18.5)+
  geom_richtext(aes(y=fct_rev(fct_inorder(as.factor(year))),x = grid_pos, label = pos_label ),
                hjust = 0.5,
                label.colour = NA,
                #fill = "#f0d508",
                fill = NA,
                size = 24,
                color = "black",
                vjust = 0.5,
                family = "Tw Cen MT Condensed",
                fontface = "bold",
                label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
                label.margin = unit(c(1, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")) +
  # DRIVER NAME ----
  geom_richtext(aes(y=fct_rev(fct_inorder(as.factor(year))),x = grid_pos, label = driver ),
                hjust = 0,
                label.colour = NA,
                nudge_x = 0.18,
                nudge_y = 0.7,
                #fill = "#f0d508",
                fill = NA,
                size = 10,
                color = "white",
                vjust = 1,
                family = "Barlow Condensed Medium",
                #fontface = "bold",
                label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
                label.margin = unit(c(1, 0, 0, 0), "lines"),
                label.r = unit(0, "lines"))+
  # CHASSI ------
  geom_richtext(aes(y=fct_rev(fct_inorder(as.factor(year))),x = grid_pos, label = chassis ),
                hjust = 0,
                label.colour = NA,
                nudge_x = 0.18,
                nudge_y = 0.42,
                #fill = "#f0d508",
                fill = NA,
                size = 7,
                color = "white",
                vjust = 1,
                family = "Barlow Condensed",
                #fontface = "bold",
                label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
                label.margin = unit(c(1, 0, 0, 0), "lines"),
                label.r = unit(0, "lines"))+
  # YEAR ------
geom_richtext(aes(y=fct_rev(fct_inorder(as.factor(year))),x = grid_pos, label = glue("{year} ({title})") ),
              hjust = 0.5,
              label.colour = NA,
              angle = 90,
              nudge_x = -0.15,
              nudge_y = 0,
              #fill = "#f0d508",
              fill = NA,
              size = 9,
              color = "white",
              vjust = 0.5,
              family = "Barlow Condensed Medium",
              #fontface = "bold",
              label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
              label.margin = unit(c(1, 0, 0, 0), "lines"),
              label.r = unit(0, "lines"))+
  
# STATS ------
geom_richtext(aes(y=fct_rev(fct_inorder(as.factor(year))),x = grid_pos, label = glue("<span style='color:#E1C340;font-family: \"Font Awesome 6 Free Solid\"'>&#xf091;</span> {wins}  <span style='color:#A16AE8;font-family: \"Font Awesome 6 Free Solid\"'>&#xf2f2;</span> {poles}  <span style='color:#B4FEE7;font-family: \"Font Awesome 6 Free Solid\"'>&#xe561;</span> {podiums} <span style='color:#B8EE30;font-family: \"Font Awesome 6 Free Solid\"'>&#xf201;</span> {round(ovr_season,2)}") ),
              hjust = 0,
              label.colour = NA,
              nudge_x = 0.18,
              nudge_y = 0.18,
              fill = "#0A0708",
              #fill = NA,
              size = 8,
              color = "white",
              vjust = 1,
              family = "Barlow Condensed",
              fontface = "bold",
              label.padding = unit(c(0.3, 0.2, 0.3, 0.2), "lines"),
              label.margin = unit(c(1, 0, 0, 0), "lines"),
              label.r = unit(0, "lines"))+
  # AGE ------
geom_richtext(aes(y=fct_rev(fct_inorder(as.factor(year))),x = grid_pos, label = glue("{age} years old") ),
              hjust = 0,
              label.colour = NA,
              nudge_x = 0.18,
              nudge_y = -0.25,
              #fill = "#f0d508",
              fill = NA,
              size = 7,
              color = "white",
              vjust = 1,
              family = "Barlow Condensed",
              #fontface = "bold",
              label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
              label.margin = unit(c(1, 0, 0, 0), "lines"),
              label.r = unit(0, "lines"))+
  # HEADER ----------------
  # TITLE ---------------------
  geom_richtext(data = header %>% filter(type == "title"),
                aes(y=y,x = x, label = label),
                hjust = 0,
                label.colour = NA,
                nudge_x = 0,
                nudge_y = 0,
                #fill = "#0A0708",
                fill = NA,
                size = 20,
                color = "white",
                vjust = 1,
                family = "Barlow Condensed Medium",
                #fontface = "bold",
                label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
                label.margin = unit(c(1, 0, 0, 0), "lines"),
                label.r = unit(0, "lines"))+
  # SUBTITLE ---------------------
geom_richtext(data = header %>% filter(type == "subtitle"),
              aes(y=y,x = x, label = label),
              hjust = 0,
              label.colour = NA,
              nudge_x = 0,
              nudge_y = 0,
              #fill = "#f0d508",
              fill = NA,
              size = 10,
              color = "white",
              vjust = 1,
              family = "Barlow Condensed",
              #fontface = "bold",
              label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
              label.margin = unit(c(1, 0, 0, 0), "lines"),
              label.r = unit(0, "lines"))+
  # LEGEND --------------
  geom_richtext(data = legend,
                aes(y=y,x = x, label = label),
                hjust = 0,
                label.colour = NA,
                nudge_x = -0.18,
                nudge_y = 0,
                #fill = "#f0d508",
                fill = NA,
                size = 8,
                color = "white",
                vjust = 1,
                family = "Barlow Condensed",
                #fontface = "bold",
                label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
                label.margin = unit(c(1, 0, 0, 0), "lines"),
                label.r = unit(0, "lines"))+
  # SIGNATURE ------------
  annotation_custom(pb, xmin = 2.9, xmax = 3.8, ymin =  -2.5, ymax = -3.5) +
  guides(fill = "none")+
  scale_x_continuous(breaks = seq(0,5,1),
                     expand = expansion(mult = c(0.12,0.35)))+
  scale_y_discrete(expand = expansion(mult = c(0.05,0.26)))+
  coord_cartesian(clip = "off")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    plot.margin=grid::unit(c(0,0,0,0), "in"),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background= element_blank()
    
        )














  # saving ----

file <- here("f1-greats","figures","f1-greats.pdf") 


ggsave(file,
       device = cairo_pdf,
       dpi = "retina",
       bg = NULL,
       width = 9+(9*0.65),
       height = 16+(16*0.65),
       units = c("in")
)


# converting png ----

pdftools::pdf_convert(pdf = file,
                      format = "png",
                      dpi = 300, 
                      filenames = glue("{str_remove(file, '.pdf')}.png"),
                      antialias = "both"
)
