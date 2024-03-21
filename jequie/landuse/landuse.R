{
    library(tidyverse)
    library(ggtext)
    library(geobr)
    library(sf)
    library(raster)
}


lu_22 <- raster::raster("~/Downloads/brasil_sentinel_coverage_2022.tif")

jee <- geobr::read_municipality(2918001)


plot(sf::st_geometry(jee))


jee_lu22 <- raster::crop(x = lu_22, y = jee) |>
    raster::mask(mask = jee)

plot(jee_lu22)

raster::writeRaster(jee_lu22,
"~/Downloads/JeeLU22.tif", overwrite=TRUE)
