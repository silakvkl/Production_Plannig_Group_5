# Ortak tahmin sayfasını bu konuya bağlar.
hareketli_ortalama_ui <- function(id) {
  fc_sayfa_ui(id, "Hareketli Ortalama", c("ma", "wma"))
}

hareketli_ortalama_server <- function(id, veri) {
  fc_sayfa_server(id, veri)
}
