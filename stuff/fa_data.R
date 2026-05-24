pak::pak("carlosdemoura/meteobr")
library(meteobr)
library(tidyverse)

st = stations |> filter(state == "Minas Gerais") |> pull(station.id)
ibge =
  readxl::read_xls("~/Downloads/DTB_2024(1)/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls", skip=6) |>
  select(`Código Município Completo`, `Região Geográfica Intermediária`) |>
  `colnames<-`(c("town.id", "regiao"))
df =
  get_data(first.day = "2025-01-01", last.day = "2025-12-31", stations = st, vars = "temperature_air") |>
  filter(station %in% st) |>
  mutate(
    time = substr(time,1,7)
  ) |>
  group_by(station, time) |>
  summarise(
    temp = mean(temperature_air, na.rm = T),
    .groups = "drop"
  ) |>
  rename(mes = "time", estacao = "station")

st_w_na =
  df |>
  filter(is.na(temp)) |>
  pull(estacao) |>
  unique()

df_wo_na = 
  df |>
  filter(!estacao %in% st_w_na) |>
  left_join(select(stations,"station.id", "town.id"), by = c("estacao" = "station.id")) |>
  left_join(ibge, by = "town.id") |>
  mutate(
    regiao = case_when(
      regiao %in% c("3102", "3103", "3104", "3112") ~ "NORTE",
      regiao %in% c("3110", "3111") ~ "TRIANGULO",
      TRUE ~ "SUL"
    )
  ) |>
  select(-town.id) |>
  pivot_wider(values_from = "temp", names_from = "mes") |>
  relocate(regiao) |>
  arrange(regiao, estacao)

write.csv(df_wo_na, "stuff/meteo.csv", row.names = F)

# 
# 
# ggplot(df_wo_na, aes(x=mes, y=temp, group=estacao)) +
#   geom_line()
# 
# 
# 
# ggplot(df_wo_na, aes(x = mes, y = temp, color = estacao, group = estacao)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   labs(
#     title = "Temperatura ao longo do tempo por estação",
#     x = "Mês",
#     y = "Temperatura",
#     color = "Estação"
#   ) +
#   theme_minimal()
