# Ortak tahmin sayfasını bu konuya bağlar.
mevsimsel_ui <- function(id) {
  fc_sayfa_ui(id, "Mevsimsel Yöntemler", c("mevsim_basit", "ayristirma", "winters"))
}

mevsimsel_server <- function(id, veri) {
  fc_sayfa_server(id, veri)
}
