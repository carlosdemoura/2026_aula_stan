library(meteobr)
library(tidyverse)

st = stations |> filter(state == "Minas Gerais") |> pull(station.id)

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
  filter(!estacao %in% st_w_na)


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
