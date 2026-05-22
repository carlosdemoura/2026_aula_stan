#library(meteobr)
library(dplyr)
library(ggplot2)
library(deldir)
library(sf)
library(ggforce)

mg_shape <- geobr::read_state(code_state = "MG", year = 2020)

plot_voronoi_MG <- function(mes) {
  mg_stations <-
    stations %>%
    filter(state == "Minas Gerais") %>%
    select(station.id, lat, lon)
  
  mes_str <- sprintf("%02d", mes)
  df_mes <- df_wo_na %>%
    filter(substr(mes, 6, 7) == mes_str) %>%
    inner_join(mg_stations, by = c("estacao" = "station.id"))
  
  pontos_sf <- st_as_sf(df_mes, coords = c("lon", "lat"), crs = 4326)
  vor <- st_voronoi(st_union(pontos_sf))
  vor_sf <- st_collection_extract(vor) |> 
    st_as_sf()
  vor_mg <- st_intersection(vor_sf, mg_shape)
  vor_mg$temp <- df_mes$temp
  
  
  ggplot() +
    geom_sf(data = mg_shape, fill = NA, color = "black") +
    #geom_sf(data = vor_mg, fill = NA, color = "black") +
    geom_voronoi_tile(aes(x = lon, y = lat, fill = temp), 
                      data = df_mes, 
                      color = "white", alpha = 0.7) +
    geom_point(aes(x = lon, y = lat), data = df_mes, color = "red") +
    #scale_fill_viridis_c(option = "plasma", name = "Temp (°C)") +
    scale_fill_gradient(low = "white", high = "red", name = "Temp (°C)") +
    theme_minimal() +
    labs(title = paste("Diagrama de Voronoi das estações de MG - Mês", mes))
}

plot_voronoi_MG(5)
