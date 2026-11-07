# Ortak tahmin sayfasını bu konuya bağlar.
arima_ui <- function(id) {
  fc_sayfa_ui(id, "Box–Jenkins / ARIMA", c("arima"))
}

arima_server <- function(id, veri) {
  fc_sayfa_server(id, veri)
}
