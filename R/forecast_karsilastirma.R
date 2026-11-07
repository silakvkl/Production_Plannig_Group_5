karsilastirma_ui <- function(id) {
  ns <- NS(id)
  tagList(h2("Yöntem karşılaştırma ve parametre tarama"), uiOutput(ns("uyari")),
    layout_columns(col_widths = c(4,8),
      card(card_header("Adil tahmin testi"),
        checkboxGroupInput(ns("yontemler"), "Karşılaştırılacak yöntemler",
          choices = setNames(setdiff(names(fc_adlar), "wma"), fc_adlar[setdiff(names(fc_adlar), "wma")]),
          selected = c("ma", "ses", "regresyon", "holt")),
        numericInput(ns("test"), "Son kaç gözlem test için ayrılsın?", 4, min = 1, max = 120),
        checkboxInput(ns("ortalama"), "Başarılı yöntemlerin ortalama tahminini de karşılaştır", TRUE),
        numericInput(ns("h"), "Tam veriyle gelecek tahmin ufku", 6, min=1, max=120),
        selectInput(ns("test_mod"), "Test biçimi", c("Tek noktadan çok dönem ileri" = "sabit", "Her dönem yeniden kur, bir adım ileri" = "ilerleyen")),
        fc_parametre_ui(ns),
        actionButton(ns("hesapla"), "Karşılaştır", class = "btn-primary"),
        p("İlerleyen test ARIMA için daha uzun sürebilir. Bütün yöntemlerde aynı test dönemleri kullanılır.")),
      card(card_header("Test sonuçları"),
        p("Küçük MAD / MSE / MAPE daha iyidir. Başarısız modeller sıralamaya katılmaz. Eğitim hataları burada kullanılmaz."),
        DTOutput(ns("ozet")), plotOutput(ns("grafik")), DTOutput(ns("detay")),
        h4("Tam veriyle yeniden kurulan gelecek tahminleri"), DTOutput(ns("gelecek")),
        p("Bu tablo test sonuçlarından farklıdır: bütün mevcut geçmiş kullanılır. Ortalama sütunu yalnız başarıyla yeniden kurulabilen yöntemleri birleştirir; kullanılan yöntemler diğer sütunlarda görünür."),
        downloadButton(ns("indir"), "Karşılaştırma CSV indir"))
    ),
    card(card_header("Ayrı doğrulama bloğunda parametre tarama"),
      p("Son test bloğu taramadan tamamen çıkarılır. Kalan geçmişin son doğrulama bloğunda aday parametreler denenir; seçilen parametrelerle son test ayrıca raporlanır."),
      selectInput(ns("tara_yontem"), "Taranacak yöntem", c("SES" = "ses", "Holt" = "holt", "Winters" = "winters", "Hareketli ortalama" = "ma")),
      numericInput(ns("dogrulama"), "Doğrulama dönemi sayısı", 4, min = 1, max = 120),
      selectInput(ns("hedef"), "En küçük yapılacak ölçü", c("MAD", "MSE", "MAPE")),
      actionButton(ns("tara"), "Taramayı çalıştır"),
      p("SES/Holt/Winters katsayı adayları: 0.1, 0.3, 0.5, 0.7, 0.9. MA adayları: N=2,...,12. Bu sınırlı ızgara küresel optimum garantilemez."),
      DTOutput(ns("tarama")), DTOutput(ns("son_test")))
  )
}

karsilastirma_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    output$uyari <- renderUI(fc_uyari(veri))
    r <- eventReactive(list(input$hesapla, veri()), {
      req(input$hesapla > 0, length(input$yontemler) > 0)
      d <- fc_veri_al(veri)
      tryCatch(fc_karsilastir(d$y, input$yontemler, input$test, fc_parametre_al(input), input$test_mod, input$ortalama),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    }, ignoreNULL = FALSE)
    output$ozet <- renderDT(fc_tablo(r()$ozet[order(r()$ozet$MAD, na.last = TRUE), ]))
    output$detay <- renderDT({ req(r()$detay); fc_tablo(r()$detay) })
    ileri <- eventReactive(list(input$hesapla,veri()),{
      req(input$hesapla>0,length(input$yontemler)>0)
      d<-fc_veri_al(veri);p<-fc_parametre_al(input);h<-fc_tamsayi(input$h,1,120)
      sonuc<-list()
      for(ad in input$yontemler) {
        f<-tryCatch(fc_tahmin(d$y,ad,h,p)$gelecek$tahmin,error=function(e)NULL)
        if(!is.null(f)) sonuc[[ad]]<-f
      }
      validate(need(length(sonuc)>0,"Gelecek için kurulabilen yöntem yok."))
      tablo<-as.data.frame(sonuc)
      if(isTRUE(input$ortalama)&&ncol(tablo)>=2) tablo$ortalama<-rowMeans(tablo)
      data.frame(donem=length(d$y)+seq_len(h),tablo)
    },ignoreNULL=FALSE)
    output$gelecek<-renderDT(fc_tablo(ileri()))
    output$grafik <- renderPlot({
      d <- r()$detay; validate(need(!is.null(d), "Başarılı yöntem yok."))
      adlar <- unique(d$yontem); renk <- grDevices::hcl.colors(length(adlar), "Dark 3")
      a <- d[d$yontem == adlar[1], ]
      plot(a$donem, a$gercek, type = "o", lwd = 2, ylim = range(c(d$gercek, d$tahmin)),
        xlab = "Test dönemi", ylab = "Talep")
      for (j in seq_along(adlar)) {
        b <- d[d$yontem == adlar[j], ]; lines(b$donem, b$tahmin, col = renk[j], lty = j)
      }
      legend("topleft", c("Gerçek", adlar), col = c("black", renk), lty = c(1, seq_along(adlar)), bty = "n", cex = .8)
    })
    output$indir <- downloadHandler(filename = function() "karsilastirma.csv",
      content = function(file) write.csv(r()$ozet, file, row.names = FALSE))
    tarama <- eventReactive(list(input$tara, veri()), {
      req(input$tara > 0)
      tryCatch({
        y <- fc_veri_al(veri)$y; n <- length(y)
        te <- fc_tamsayi(input$test, 1, min(120,n-3))
        va <- fc_tamsayi(input$dogrulama, 1, min(120,n-te-2))
        eg <- n - te - va; ad <- input$tara_yontem; p <- fc_parametre_al(input)
        grid <- switch(ad, ses = expand.grid(alpha = c(.1,.3,.5,.7,.9)),
          holt = expand.grid(alpha = c(.1,.3,.5,.7,.9), beta = c(.1,.3,.5,.7,.9)),
          winters = expand.grid(alpha = c(.1,.3,.5,.7,.9), beta = c(.1,.3,.5,.7,.9), gamma = c(.1,.3,.5,.7,.9)),
          ma = expand.grid(N = 2:12))
        scores <- vapply(seq_len(nrow(grid)), function(i) {
          pi <- modifyList(p, as.list(grid[i,,drop=FALSE]))
          tryCatch({
            f <- fc_tahmin(y[seq_len(eg)], ad, va, pi)$gelecek$tahmin
            fc_olculer(y[eg + seq_len(va)], f)[[input$hedef]]
          }, error = function(e) NA_real_)
        }, numeric(1))
        validate(need(any(is.finite(scores)), "Bu bölme ve başlangıç ayarlarıyla hiçbir aday kurulamadı; daha uzun veri veya daha küçük bloklar kullanın."))
        i <- which.min(replace(scores, !is.finite(scores), Inf))
        secilen <- modifyList(p, as.list(grid[i,,drop=FALSE]))
        f <- fc_tahmin(head(y, -te), ad, te, secilen)$gelecek$tahmin
        list(tablo = data.frame(grid, dogrulama_hatasi = scores),
          test = data.frame(yontem = ad, grid[i,,drop=FALSE], fc_olculer(tail(y,te), f)))
      }, error = function(e) validate(need(FALSE, conditionMessage(e))))
    }, ignoreNULL = FALSE)
    output$tarama <- renderDT(fc_tablo(tarama()$tablo[order(tarama()$tablo$dogrulama_hatasi),,drop=FALSE]))
    output$son_test <- renderDT(fc_tablo(tarama()$test))
  })
}
