nedensel_ui <- function(id) {
  ns <- NS(id)
  tagList(h2("Nedensel regresyon"), uiOutput(ns("uyari")),
    layout_columns(col_widths = c(4,8),
      card(card_header("Açıklayıcı değişkenler"),
        selectInput(ns("x"), "Sayısal X sütunları", choices = character(), multiple = TRUE),
        numericInput(ns("gecikme"), "X gecikmesi (0: aynı dönem; 1: önceki dönem)", 0, min = 0, max = 12, step = 1),
        textAreaInput(ns("gelecek_x"), "Gelecekte kullanılacak X değerleri", "", rows = 7,
          placeholder = "Her satır bir gelecek dönem; seçilen X sırasıyla, virgülle ayrılmış değerler."),
        p("Y=a0+a1 X1+...+ak Xk. Gelecek X değerleri bilinmeden geleceğin Y değeri hesaplanamaz. Gecikmeli modelde de bu kutuya tahmin denkleminde kullanılacak X satırlarını yazın."),
        actionButton(ns("hesapla"), "Regresyonu hesapla", class = "btn-primary")),
      card(card_header("Model ve tahmin"), verbatimTextOutput(ns("ozet")),
        plotOutput(ns("grafik")), DTOutput(ns("katsayi")), DTOutput(ns("tahmin")), DTOutput(ns("olculer")),
        p("Geçmiş hata ölçüleri eğitim hatasıdır. Regresyon ilişkisi tek başına neden-sonuç kanıtı değildir. Aralıklar normal, bağımsız ve sabit varyanslı artık varsayımıyla %95 tahmin aralığıdır.")))
  )
}

nedensel_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    output$uyari <- renderUI(fc_uyari(veri))
    observeEvent(veri(), {
      v <- veri(); req(v)
      sec <- names(v$ham)[vapply(v$ham, is.numeric, logical(1))]
      updateSelectInput(session, "x", choices = setdiff(sec, c(v$talep, v$tarih)), selected = character())
    })
    r <- eventReactive(list(input$hesapla, veri()), {
      req(input$hesapla > 0)
      tryCatch({
        d <- fc_veri_al(veri)
        validate(need(length(input$x) > 0, "En az bir X sütunu seçin; dosyada ek açıklayıcı sütun bulunmalı."))
        lag <- fc_tamsayi(input$gecikme,0,12); n <- length(d$y)
        validate(need(n-lag > length(input$x)+1, "Regresyon için yeterli eşleşen gözlem yok."))
        yi <- (lag+1):n; xi <- seq_len(n-lag)
        x <- as.matrix(d$ham[xi,input$x,drop=FALSE])
        validate(need(all(is.finite(x)), "X sütunları sonlu sayısal değerler içermeli."))
        colnames(x) <- paste0("X",seq_len(ncol(x)))
        dat <- data.frame(Y = d$y[yi], x)
        model <- lm(Y ~ ., data = dat)
        validate(need(model$rank == ncol(dat), "X sütunları doğrusal bağımlı veya sabit; değişkenleri azaltın."))
        gelecek <- NULL
        if (nzchar(trimws(input$gelecek_x))) {
          g <- as.matrix(read.table(text = input$gelecek_x, sep = ",", header = FALSE))
          validate(need(is.numeric(g) && all(is.finite(g)) && ncol(g) == ncol(x), "Gelecek X tablosunda her satır seçilen X sayısı kadar sayısal değer içermeli."))
          colnames(g) <- colnames(x)
          gelecek <- as.data.frame(predict(model, newdata = as.data.frame(g), interval = "prediction", level = .95))
        }
        list(model = model, x = input$x, y = dat$Y, fitted = fitted(model), g = gelecek)
      }, error = function(e) validate(need(FALSE, conditionMessage(e))))
    }, ignoreNULL = FALSE)
    output$ozet <- renderPrint({
      cat("Değişken eşlemesi:\n"); print(setNames(r()$x, paste0("X",seq_along(r()$x))))
      print(summary(r()$model))
    })
    output$katsayi <- renderDT(fc_tablo(data.frame(terim = names(coef(r()$model)), katsayi = coef(r()$model))))
    output$tahmin <- renderDT({ validate(need(!is.null(r()$g), "Gelecek X değerleri girilmedi; yalnız model kuruldu.")); fc_tablo(r()$g) })
    output$olculer <- renderDT(fc_tablo(fc_olculer(r()$y,r()$fitted)))
    output$grafik <- renderPlot({
      plot(r()$y,type="o",ylab="Talep",xlab="Eşleşen dönem"); lines(r()$fitted,col="#426b65",lwd=2)
      legend("topleft",c("Gerçek","Regresyon uyumu"),col=c("black","#426b65"),lty=1,bty="n")
    })
  })
}
