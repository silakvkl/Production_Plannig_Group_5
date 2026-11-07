# Ortak tahmin sayfasını bu konuya bağlar.
trend_ui <- function(id) {
  fc_sayfa_ui(id, "Trend", c("regresyon", "holt"))
}

trend_server <- function(id, veri) {
  fc_sayfa_server(id, veri)
}
