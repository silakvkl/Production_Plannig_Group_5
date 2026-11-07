ana_sayfa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Üretim planlama çalışma alanınıza hoş geldiniz"),
    p("Verinizi tek bir yerde hazırlayın, ardından ilgili yöntemin sayfasına geçin."),
    card(
      card_header("Nasıl kullanılır?"),
      tags$ol(
        tags$li("Veri sayfasında dosyanızı yükleyin veya örnek veriyi açın."),
        tags$li("Tarih / dönem ve talep sütunlarını seçin."),
        tags$li("Sol menüden üzerinde çalışmak istediğiniz modülü açın.")
      ),
      p("Forecasting yöntemleri çalışır. Öğrenme eğrisi sonraki aşamada eklenecektir.")
    ),
    h3("Hızlı geçiş"),
    uiOutput(ns("kartlar"))
  )
}

ana_sayfa_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    output$kartlar <- renderUI({
      kartlar <- lapply(Filter(function(m) m$id != id, moduller), function(m) {
        card(card_header(tagList(icon(m$ikon), " ", m$baslik)),
             p(if (is.na(m$kategori)) "Çalışma alanı" else m$kategori),
             gecis(m$id, "Sayfayı aç", "arrow-right"))
      })
      do.call(layout_column_wrap, c(list(width = 1/3), kartlar))
    })
  })
}
