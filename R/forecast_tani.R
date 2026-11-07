tani_ui <- function(id) {
  ns <- NS(id)
  tagList(h2("Seri tanısı, fark alma ve stokastik süreçler"), uiOutput(ns("uyari")),
    card(card_header("Dönüşüm ve korelogram"),
      checkboxInput(ns("log"), "Doğal log al", FALSE),
      numericInput(ns("d"), "Ardışık fark sayısı", 0, min = 0, max = 2, step = 1),
      numericInput(ns("m"), "Mevsim uzunluğu", 12, min = 3, step = 1),
      checkboxInput(ns("mevsim_fark"), "Bir kez mevsimsel fark al", FALSE),
      numericInput(ns("lag"), "ACF gecikme sayısı", 12, min = 1, max = 60, step = 1),
      p("r_k=Σ(D_t-D̄)(D_(t-k)-D̄) / Σ(D_t-D̄)². Önce log, sonra normal fark, sonra mevsimsel fark uygulanır."),
      plotOutput(ns("seri")), plotOutput(ns("acf")), DTOutput(ns("korelasyon")),
      verbatimTextOutput(ns("qtest")), DTOutput(ns("donusum")),
      p("Korelogram sınırları yaklaşık %95 beyaz gürültü referansıdır; durağanlık kanıtı değildir. Buradaki test dönüştürülmüş seri içindir; kurulan ARIMA modelinin artık testi ARIMA sayfasındadır.")),
    card(card_header("AR(1), MA(1), ARMA(1,1) simülasyonu"),
      selectInput(ns("tip"), "Süreç", c("AR(1)" = "ar", "MA(1)" = "ma", "ARMA(1,1)" = "arma")),
      numericInput(ns("a1"), "AR katsayısı a1 (|a1|<1)", .7, min = -.99, max = .99),
      numericInput(ns("b1"), "Kitabın MA katsayısı b1 (R'de theta=-b1)", .7, min = -.99, max = .99),
      numericInput(ns("mu"), "Durağan süreç ortalaması μ", 100),
      numericInput(ns("sigma"), "Yenilik standart sapması", 10, min = .01),
      numericInput(ns("n"), "Gözlem sayısı", 200, min = 30, max = 5000),
      numericInput(ns("seed"), "Rastgelelik tohumu", 42, min = 0),
      actionButton(ns("uret"), "Süreç üret"),
      p("AR sabiti a0=μ(1-a1). MA sabiti b0=μ. MA(1): ρ1=-b1/(1+b1²); AR(1): ρk=a1^k."),
      plotOutput(ns("sim")), plotOutput(ns("teori")))
  )
}

tani_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    output$uyari <- renderUI(fc_uyari(veri))
    seri <- reactive({
      d <- fc_veri_al(veri); y <- d$y
      if (isTRUE(input$log)) {
        validate(need(all(y > 0), "Log için pozitif veri gerekli.")); y <- log(y)
      }
      fark <- fc_tamsayi(input$d, 0, 2)
      if (fark > 0) y <- diff(y, differences = fark)
      if (isTRUE(input$mevsim_fark)) {
        m <- fc_tamsayi(input$m, 3)
        validate(need(length(y) > m + 2, "Mevsimsel fark için yeterli veri yok."))
        y <- diff(y, lag = m)
      }
      validate(need(length(y) >= 3 && sd(y) > 0, "Dönüştürülmüş seri en az 3 gözlem ve pozitif varyans içermeli."))
      y
    })
    output$seri <- renderPlot(plot(seri(), type = "l", col = "#426b65", xlab = "Dönüştürülmüş dönem", ylab = "Değer"))
    output$acf <- renderPlot({
      par(mfrow = c(1,2)); lag <- min(fc_tamsayi(input$lag,1,60), length(seri()) - 1)
      acf(seri(), lag.max = lag, main = "Örnek ACF")
      pacf(seri(), lag.max = lag, main = "Örnek PACF (ek tanı)")
    })
    output$korelasyon <- renderDT({
      y <- seri(); k <- min(fc_tamsayi(input$lag,1,60), length(y)-1)
      a <- as.numeric(acf(y, lag.max = k, plot = FALSE)$acf)[-1]
      fc_tablo(data.frame(gecikme = seq_len(k), r = a, alt_referans = -1.96/sqrt(length(y)), ust_referans = 1.96/sqrt(length(y))))
    })
    output$qtest <- renderPrint({
      y <- seri(); lag <- min(input$lag, length(y)-1)
      print(Box.test(y, lag = lag, type = "Ljung-Box"))
      cat("\nBu ek tanı testi otokorelasyon yokluğu hipotezini sınar; büyük p-değeri hipotezi kanıtlamaz.")
    })
    output$donusum <- renderDT(fc_tablo(data.frame(donem = seq_along(seri()), deger = seri())))
    sim <- eventReactive(input$uret, {
      a <- input$a1; b <- input$b1
      validate(need(is.finite(a) && abs(a) < 1 && is.finite(b) && abs(b) < 1, "Katsayıların mutlak değeri 1'den küçük olmalı."))
      n <- fc_tamsayi(input$n,30,5000)
      validate(need(is.finite(input$sigma) && input$sigma > 0, "Sigma pozitif olmalı."))
      model <- switch(input$tip, ar = list(ar = a), ma = list(ma = -b), arma = list(ar = a, ma = -b))
      set.seed(fc_tamsayi(input$seed,0))
      list(y = as.numeric(arima.sim(model, n = n, sd = input$sigma)) + input$mu,
        teori = ARMAacf(ar = model$ar, ma = model$ma, lag.max = 15))
    })
    output$sim <- renderPlot(plot(sim()$y, type = "l", xlab = "Dönem", ylab = "Simüle edilen değer"))
    output$teori <- renderPlot({
      plot(0:15, sim()$teori, type = "h", ylim = c(-1,1), xlab = "Gecikme", ylab = "Teorik ACF")
      abline(h = 0)
    })
  })
}
