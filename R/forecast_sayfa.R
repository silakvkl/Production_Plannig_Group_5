fc_parametre_ui <- function(ns) {
  # Karşılaştırma sayfasında tek bir yöntem seçimi yoktur; tüm parametreler gösterilir.
  alan <- function(yontemler, ...) {
    dizi <- paste(sprintf("'%s'", yontemler), collapse = ",")
    conditionalPanel(
      condition = paste0("typeof input.yontem === 'undefined' || input.yontem === null || [",dizi,"].includes(input.yontem)"),
      ns = ns, ...)
  }
  tags$details(open = NA,
    tags$summary("Yöntem parametreleri"),
    alan(c("ma","wma","ayristirma"),
      numericInput(ns("N"), "Hareketli ortalama penceresi N", 3, min = 1, step = 1)),
    alan("wma",
      textInput(ns("agirlik"), "Ağırlıklar (eskiden yeniye)", "0.2, 0.3, 0.5")),
    alan(c("ses","adaptif","holt","winters","ayristirma"),
      numericInput(ns("alpha"), "α — seviye / üstel düzeltme", .1, min = .001, max = .999, step = .05)),
    alan(c("adaptif","holt","winters","ayristirma"),
      numericInput(ns("beta"), "β — trend / adaptif hata düzeltme", .1, min = .001, max = .999, step = .05)),
    alan("winters",
      numericInput(ns("gamma"), "γ — Winters mevsim düzeltme", .1, min = .001, max = .999, step = .05)),
    alan(c("mevsim_basit","ayristirma","winters","arima"),
      numericInput(ns("m"), "Mevsim uzunluğu m (çeyrek=4, ay=12)", 4, min = 3, step = 1)),
    alan(c("ses","adaptif","holt","ayristirma"),
      selectInput(ns("baslangic"), "SES / Holt / adaptif başlangıcı",
        c("İlk gözlem, ilk trend=0" = "ilk", "İlk k gözlemle başlat" = "blok", "Manuel S0 ve G0" = "manuel")),
      conditionalPanel("input.baslangic === 'blok'", ns=ns,
        numericInput(ns("k"), "Başlangıç bloğu k", 5, min = 2, step = 1)),
      conditionalPanel("input.baslangic === 'manuel'", ns=ns,
        numericInput(ns("S0"), "Manuel S0 (SES için F1)", 200),
        numericInput(ns("G0"), "Manuel G0 (yalnız Holt)", 10))),
    alan("ayristirma",
      selectInput(ns("baz"), "Ayrıştırma sonrası baz yöntem",
        c("Doğrusal regresyon" = "regresyon", "Holt" = "holt", "Hareketli ortalama" = "ma", "Üstel düzeltme" = "ses"))),
    p("Yalnız seçilen yönteme ait parametreler kullanılır. Winters ilk iki mevsimle otomatik başlar."),
    alan("arima",
      tags$details(open=NA,
        tags$summary("ARIMA dereceleri"),
        numericInput(ns("p"), "p — AR", 1, min = 0, max = 3, step = 1),
        numericInput(ns("d"), "d — fark", 0, min = 0, max = 2, step = 1),
        numericInput(ns("q"), "q — MA", 0, min = 0, max = 3, step = 1),
        numericInput(ns("P"), "P — mevsimsel AR", 0, min = 0, max = 2, step = 1),
        numericInput(ns("D"), "D — mevsimsel fark", 0, min = 0, max = 1, step = 1),
        numericInput(ns("Q"), "Q — mevsimsel MA", 0, min = 0, max = 2, step = 1),
        checkboxInput(ns("log"), "Doğal log dönüşümü", FALSE),
        checkboxInput(ns("drift"), "Toplam fark derecesi 1 ise drift ekle", FALSE)))
  )
}

fc_parametre_al <- function(input) {
  list(N = input$N, agirlik = if (!is.null(input$yontem) && input$yontem == "wma") fc_sayilar(input$agirlik) else NULL,
    alpha = input$alpha, beta = input$beta, gamma = input$gamma, m = input$m,
    baslangic = input$baslangic, k = input$k, S0 = input$S0, G0 = input$G0,
    baz = input$baz, order = c(input$p, input$d, input$q),
    seasonal = c(input$P, input$D, input$Q), log = input$log, drift = input$drift)
}

fc_sayfa_ui <- function(id, baslik, yontemler) {
  ns <- NS(id)
  tagList(h2(baslik), uiOutput(ns("uyari")),
    layout_columns(col_widths = c(4, 8),
      card(card_header("Parametreler"),
        selectInput(ns("yontem"), "Yöntem", choices = setNames(yontemler, fc_adlar[yontemler])),
        numericInput(ns("h"), "Kaç dönem ileri?", 6, min = 1, max = 120, step = 1),
        fc_parametre_ui(ns),
        actionButton(ns("hesapla"), "Hesapla", icon = icon("calculator"), class = "btn-primary"),
        p("Parametre değişikliğinden sonra Hesapla'ya basın. Sonuçlar son hesaplamaya aittir.")),
      card(card_header("Tahmin ve inceleme"), uiOutput(ns("mesaj")),
        tabsetPanel(
          tabPanel("Grafik", plotOutput(ns("grafik"), height = "380px")),
          tabPanel("Gelecek", DTOutput(ns("gelecek")), downloadButton(ns("indir"), "Tahmin CSV indir")),
          tabPanel("Geçmiş ve hata", DTOutput(ns("gecmis")), DTOutput(ns("olculer")),
            p("MAPE yalnız sıfır olmayan gerçekleşmelerden hesaplanır; MAPE_n kullanılan sayıdır. sigma_MAD=1.25×MAD normal, yaklaşık sıfır ortalamalı hatalar için yaklaşımdır.")),
          tabPanel("Ara işlemler", DTOutput(ns("detay")), verbatimTextOutput(ns("ek"))),
          tabPanel("Artık tanısı", plotOutput(ns("artik_grafik")), verbatimTextOutput(ns("artik_test")),
            p("ARIMA'da tanı model ölçeğindeki yeniliklere uygulanır; log seçildiyse bu ölçek logdur. Diğer yöntemlerde F-D hataları kullanılır. Büyük p-değeri modelin doğru olduğunu kanıtlamaz.")),
          tabPanel("Hata izleme", numericInput(ns("iz_beta"), "İzleme β", .1, min = .001, max = .999),
            numericInput(ns("mad0"), "Başlangıç MAD", 0, min = 0),
            plotOutput(ns("hata_grafik")), plotOutput(ns("trigg_grafik"))),
          tabPanel("Formül / varsayımlar", verbatimTextOutput(ns("formul")))
        )
      )
    )
  )
}

fc_sayfa_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    output$uyari <- renderUI(fc_uyari(veri))
    sonuc <- eventReactive(list(input$hesapla, veri()), {
      req(input$hesapla > 0)
      d <- fc_veri_al(veri)
      tryCatch({
        z <- fc_tahmin(d$y, input$yontem, input$h, fc_parametre_al(input))
        z$gecmis$etiket <- d$etiket; z$yontem <- input$yontem
        list(ok = TRUE, z = z)
      }, error = function(e) list(ok = FALSE, hata = conditionMessage(e)))
    }, ignoreNULL = FALSE)
    z <- reactive({
      r <- sonuc()
      validate(need(r$ok, if (!isTRUE(r$ok)) r$hata else ""))
      r$z
    })
    output$mesaj <- renderUI({
      req(input$hesapla > 0)
      r <- sonuc()
      if (!r$ok) return(div(class = "alert alert-danger", r$hata))
      tagList(p(strong(paste("Hesaplanan yöntem:", fc_adlar[r$z$yontem]))),
        lapply(r$z$notlar, function(t) div(class = "alert alert-secondary", t)))
    })
    output$grafik <- renderPlot(fc_grafik(z()))
    output$gelecek <- renderDT(fc_tablo(z()$gelecek))
    output$olculer <- renderDT(fc_tablo(z()$olculer))
    iz <- reactive(fc_izleme(z()$gecmis$gercek, z()$gecmis$tahmin, input$iz_beta, input$mad0))
    output$gecmis <- renderDT(fc_tablo(cbind(z()$gecmis, iz())))
    output$detay <- renderDT({
      validate(need(!is.null(z()$detay), "Bu yöntem için ek işlem tablosu yok."))
      fc_tablo(z()$detay)
    })
    output$ek <- renderPrint({
      if (z()$yontem == "winters") print(z()$model)
      if (z()$yontem == "arima") cat("AIC:", z()$model$aic, "\nYenilik varyansı:", z()$model$sigma2)
    })
    artik <- reactive({
      r <- z()
      e <- if (r$yontem == "arima") as.numeric(residuals(r$model)) else r$gecmis$tahmin - r$gecmis$gercek
      e <- e[is.finite(e) & is.finite(r$gecmis$tahmin)]
      validate(need(length(e) >= 5 && sd(e) > 0, "Artık tanısı için en az 5 değişken hata gözlemi gerekli."))
      e
    })
    output$artik_grafik <- renderPlot({
      e <- artik(); par(mfrow = c(1,2))
      acf(e,lag.max = min(20,length(e)-1),main = "Artık ACF")
      qqnorm(e,main = "Normal Q-Q"); qqline(e,col = "#426b65")
    })
    output$artik_test <- renderPrint({
      e <- artik(); df <- 0
      if(z()$yontem == "arima") df <- sum(z()$model$arma[1:4])
      lag <- min(length(e)-1,max(df+3,min(12,floor(length(e)/4))))
      validate(need(lag > df, "Serbestlik derecesi için yeterli artık yok."))
      print(Box.test(e,lag = lag,type = "Ljung-Box",fitdf = df))
    })
    output$formul <- renderText(paste(fc_formuller[[input$yontem]], collapse = "\n\n"))
    output$hata_grafik <- renderPlot({
      e <- iz()$hata; validate(need(any(is.finite(e)), "Başlangıç dışında hata gözlemi yok."))
      par(mfrow = c(2,1), mar = c(3,4,2,1))
      plot(e, type = "o", col = "#426b65", ylab = "F - D", xlab = "Dönem", main = "Tahmin hatası")
      abline(h = 0, lty = 2)
      plot(iz()$kumulatif_hata, type = "l", col = "#ba7d36", ylab = "Σe", xlab = "Dönem", main = "Kümülatif hata")
      abline(h = 0, lty = 2)
    })
    output$trigg_grafik <- renderPlot({
      t <- iz()$Trigg; validate(need(any(is.finite(t)), "İzleme için hata gözlemi yok."))
      plot(t, type = "l", ylim = c(0,1), col = "#426b65", ylab = "|E| / M", xlab = "Dönem", main = "Trigg izleme sinyali")
      if (abs(input$iz_beta - .1) < 1e-8) abline(h = .51, lty = 2, col = "red")
      mtext(if (abs(input$iz_beta - .1) < 1e-8) "β=0.1 için kitaptaki referans eşik: 0.51" else "Bu β için 0.51 eşiği uygulanmaz.", cex = .8)
    })
    output$indir <- downloadHandler(filename = function() paste0(z()$yontem, "_tahmin.csv"),
      content = function(file) write.csv(z()$gelecek, file, row.names = FALSE, fileEncoding = "UTF-8"))
  })
}
