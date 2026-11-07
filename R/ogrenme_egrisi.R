ogrenme_egrisi_ui <- function(id) {
  yer_tutucu_ui(id, "Öğrenme Eğrisi",
    "Üretim deneyimi arttıkça işlem süresindeki değişimi incelemek için kullanılır.
     Bu modül geliştirildiğinde kendine özgü veri sütunları ayrıca tanımlanacaktır.")
}

ogrenme_egrisi_server <- function(id, veri) {
  yer_tutucu_server(id, veri)
}
