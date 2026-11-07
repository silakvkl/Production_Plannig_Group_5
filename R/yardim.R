yardim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Yardım / Hakkında"),
    card(
      card_header("Veri biçimi"),
      p("Dosyada başlık satırı ve en az iki sütun bulunmalıdır."),
      p("Tarih sütunu tarih veya dönem etiketi olabilir. Talep sütunu sayısal olmalıdır."),
      p("Excel dosyasında ilk sayfa okunur. CSV için ayırıcı ve ondalık
         işaretini yüklemeden önce seçin."),
      gecis("veri", "Veri sayfasına git", "database")
    ),
    card(
      card_header("Uygulamanın kapsamı"),
      p("Bu sürüm forecasting hesaplamalarını, grafikleri ve model karşılaştırmasını içerir."),
      p("Yöntemlerin formülleri kendi sayfalarında; kapsam ve varsayımlar Forecasting Rehberi sayfasındadır. Öğrenme eğrisi henüz yer tutucudur."),
      p("Yüklenen veri yalnızca mevcut kullanıcı oturumunda tutulur.")
    )
  )
}

yardim_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    # Bu sayfada henüz sunucu işlemi gerekmiyor.
  })
}
