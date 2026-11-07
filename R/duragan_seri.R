# Ortak tahmin sayfasını bu konuya bağlar.
duragan_seri_ui <- function(id) {
  fc_sayfa_ui(id, "Durağan Seri", c("ses", "adaptif"))
}

duragan_seri_server <- function(id, veri) {
  fc_sayfa_server(id, veri)
}
