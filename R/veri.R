veri_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Ortak veri"),
    p("Dosyayı bir kez yükleyin; seçilen sütunlar bütün modüllere aktarılır."),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Dosya ve sütunlar"),
        fileInput(ns("dosya"), "Excel veya CSV yükleyin",
                  accept = c(".xlsx", ".csv")),
        selectInput(ns("ayirici"), "CSV ayırıcı",
                    choices = c("Virgül" = ",", "Noktalı virgül" = ";",
                                "Sekme" = "\t")),
        selectInput(ns("ondalik"), "CSV ondalık işareti",
                    choices = c("Nokta" = ".", "Virgül" = ",")),
        p("CSV dosyası UTF-8; Excel için ilk çalışma sayfası kullanılır."),
        selectInput(ns("ornek_tipi"), "Örnek veri seç",
          c("Motor arızaları — kitap örnek 2.2–2.5 (8 dönem)" = "motor",
            "Mevsimsel — kitap örnek 2.7 (8 dönem)" = "mevsim",
            "Winters güncellemeleri — kitap örnek 2.8 (12 dönem)" = "winters",
            "Trend + mevsimsellik (48 aylık yapay veri)" = "aylik",
            "AirPassengers — kitap ARIMA vakası (144 ay)" = "airline")),
        actionButton(ns("ornek"), "Örnek veri ile dene",
                     icon = icon("flask"), class = "btn-outline-primary"),
        selectInput(ns("tarih"), "Tarih / dönem sütunu", choices = character()),
        selectInput(ns("talep"), "Talep sütunu", choices = character()),
        uiOutput(ns("bilgi"))
      ),
      card(card_header("İlk 10 satır"), DTOutput(ns("onizleme")))
    )
  )
}

veri_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    ham <- reactiveVal(NULL)
    ad <- reactiveVal(NULL)
    hata <- reactiveVal(NULL)

    yukle <- function(d, isim) {
      if (!is.data.frame(d) || nrow(d) == 0 || ncol(d) < 2) {
        stop("Dosyada en az iki sütun ve bir veri satırı bulunmalı.")
      }
      names(d) <- make.unique(ifelse(nzchar(names(d)), names(d), "sutun"))
      ham(d)
      ad(isim)
      hata(NULL)
      sayisal <- names(d)[vapply(d, is.numeric, logical(1))]
      talep <- if (length(sayisal)) tail(sayisal, 1) else names(d)[2]
      tarih <- setdiff(names(d), talep)[1]
      updateSelectInput(session, "tarih", choices = names(d), selected = tarih)
      updateSelectInput(session, "talep", choices = names(d), selected = talep)
    }

    observeEvent(input$dosya, {
      tryCatch({
        f <- input$dosya
        uzanti <- tolower(tools::file_ext(f$name))
        d <- switch(
          uzanti,
          xlsx = as.data.frame(readxl::read_excel(f$datapath)),
          csv = read.table(f$datapath, header = TRUE, sep = input$ayirici,
                           dec = input$ondalik, fileEncoding = "UTF-8-BOM",
                           stringsAsFactors = FALSE, check.names = FALSE,
                           comment.char = ""),
          stop("Yalnızca .xlsx ve .csv dosyaları desteklenir.")
        )
        yukle(d, f$name)
      }, error = function(e) {
        # Başarısız yüklemede eski verinin kullanılmasını engeller.
        ham(NULL)
        ad(NULL)
        hata(paste("Dosya okunamadı:", conditionMessage(e)))
      })
    })

    observeEvent(input$ornek, {
      tip <- input$ornek_tipi
      y <- switch(tip,
        motor = c(200,250,175,186,225,285,305,190),
        mevsim = c(10,20,26,17,12,23,30,22),
        winters = c(10,20,26,17,12,23,30,22,16,33,34,26),
        aylik = round((100+2*seq_len(48))*rep(c(.7,.75,.9,1,1.1,1.2,1.35,1.25,1.1,1,.85,.8),4)),
        airline = as.numeric(datasets::AirPassengers))
      tarih <- if (tip %in% c("aylik","airline"))
        seq(as.Date(if(tip=="airline") "1949-01-01" else "2023-01-01"),by="month",length.out=length(y)) else seq_along(y)
      yukle(data.frame(tarih=tarih,talep=y),paste0("ornek_",tip," (",length(y)," dönem)"))
    })

    # Veri sözleşmesi: ham, dosya, tarih, talep, hazir.
    secim <- reactive({
      d <- ham()
      if (is.null(d)) return(NULL)
      uygun <- length(input$tarih) == 1 && length(input$talep) == 1 &&
        input$tarih %in% names(d) && input$talep %in% names(d) &&
        input$tarih != input$talep
      hazir <- uygun && is.numeric(d[[input$talep]]) &&
        all(is.finite(d[[input$talep]])) &&
        !anyNA(d[[input$tarih]])
      list(ham = d, dosya = ad(), tarih = input$tarih,
           talep = input$talep, hazir = hazir)
    })

    output$bilgi <- renderUI({
      if (!is.null(hata())) {
        return(div(class = "alert alert-danger", role = "alert", hata()))
      }
      v <- secim()
      if (is.null(v)) return(p("Dosya yükleyin veya örnek veriyi kullanın."))
      if (!v$hazir) {
        return(div(class = "alert alert-warning", role = "alert",
          "Farklı iki sütun seçin. Talep sayısal ve eksiksiz olmalı;
           tarih / dönem sütununda boş değer bulunmamalı."))
      }
      div(class = "alert alert-success", "Veri bütün modüllerde kullanılabilir.")
    })

    output$onizleme <- renderDT({
      req(ham())
      datatable(head(ham(), 10), rownames = FALSE,
                options = list(pageLength = 10, dom = "t", scrollX = TRUE))
    })

    return(secim)
  })
}
