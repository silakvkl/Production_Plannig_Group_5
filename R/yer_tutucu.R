yer_tutucu_ui <- function(id, baslik = "Yeni modül",
                            aciklama = "Bu sayfa için yöntem daha sonra eklenecek.") {
  ns <- NS(id)
  tagList(
    h2(baslik),
    tags$details(
      class = "mb-4",
      tags$summary("Bu ne işe yarar?"),
      p(class = "mt-3", aciklama)
    ),
    uiOutput(ns("uyari")),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Parametreler"),
           div(class = "bos-alan", "Bu modül yakında eklenecek")),
      card(card_header("Sonuçlar"),
           div(class = "bos-alan", "Bu modül yakında eklenecek"))
    )
  )
}

yer_tutucu_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    output$uyari <- renderUI({
      v <- veri()
      if (is.null(v)) {
        return(div(class = "alert alert-warning",
          p("Önce Veri sekmesinden dosya yükleyin"),
          gecis("veri", "Veri sekmesine git", "database")))
      }
      if (!isTRUE(v$hazir)) {
        return(div(class = "alert alert-warning",
          p("Veri sekmesinde tarih ve talep sütunlarını kontrol edin."),
          gecis("veri", "Sütunları seç", "database")))
      }
      NULL
    })
  })
}
