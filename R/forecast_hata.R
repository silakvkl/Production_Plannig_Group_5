hata_ui <- function(id) {
  ns <- NS(id)
  tagList(h2("Hata ölçüleri ve MA–SES ilişkileri"), uiOutput(ns("uyari")),
    card(card_header("Dosyadaki hazır tahminleri değerlendir"),
      selectInput(ns("f"), "Tahmin sütunu (sayısal)", choices = character()),
      numericInput(ns("beta"), "Trigg / MAD düzeltme katsayısı β", .1, min = .001, max = .999),
      numericInput(ns("mad0"), "Başlangıç MAD", 0, min = 0),
      p("e=F-D; MAD=ortalama(|e|); MSE=ortalama(e²); MAPE=100×ortalama(|e/D|). MAPE'de D=0 satırları dışlanır."),
      DTOutput(ns("olcu")), DTOutput(ns("hatalar")), plotOutput(ns("grafik"))),
    card(card_header("Kitaptaki teorik ilişkiler ve duyarlılık"),
      numericInput(ns("N"), "MA pencere sayısı N", 3, min = 1),
      numericInput(ns("alpha"), "SES α", .1, min = .001, max = .999),
      numericInput(ns("sigma"), "Talep standart sapması σ", 10, min = 0),
      numericInput(ns("degisim"), "Geçmiş kayıttaki düzeltme ΔD (yeni-eski)", -200),
      numericInput(ns("yas"), "Düzeltilen kaydın gelecek tahmine göre yaşı", 4, min = 1),
      p("MA: ortalama yaş=(N+1)/2; SES: ortalama yaş=1/α; eşdeğer α=2/(N+1), N=2/α-1."),
      p("Var(e_MA)=σ²(N+1)/N; Var(e_SES)=σ²·2/(2-α). Bu ilişkiler bağımsız, durağan talep varsayar; SES için uzun geçmiş yaklaşımıdır."),
      verbatimTextOutput(ns("iliski")), plotOutput(ns("agirlik")),
      p("Aynı hata varyansı aynı tekil tahmin demek değildir. Stok güvenliği için yalnız talep σ'sı yerine tahmin hatası σ_e dikkate alınır (§2.13). Bu sayfa otomatik stok politikası belirlemez.")))
}

hata_server <- function(id, veri) {
  moduleServer(id, function(input, output, session) {
    output$uyari <- renderUI(fc_uyari(veri))
    observeEvent(veri(), {
      v <- veri(); req(v)
      sec <- names(v$ham)[vapply(v$ham,is.numeric,logical(1))]
      updateSelectInput(session,"f",choices=setdiff(sec,c(v$talep,v$tarih)))
    })
    f <- reactive({
      d <- fc_veri_al(veri)
      validate(need(length(input$f)==1 && input$f %in% names(d$ham), "Dosyanıza tahmin adlı sayısal bir sütun ekleyip yeniden yükleyebilirsiniz."))
      d$ham[[input$f]]
    })
    output$olcu <- renderDT(fc_tablo(fc_olculer(fc_veri_al(veri)$y,f())))
    output$hatalar <- renderDT(fc_tablo(fc_izleme(fc_veri_al(veri)$y,f(),input$beta,input$mad0)))
    output$grafik <- renderPlot({
      iz <- fc_izleme(fc_veri_al(veri)$y,f(),input$beta,input$mad0)
      validate(need(any(is.finite(iz$hata)), "Karşılaştırılabilir tahmin yok."))
      par(mfrow=c(2,1)); plot(iz$hata,type="o",ylab="F-D",xlab="Dönem"); abline(h=0,lty=2)
      plot(iz$Trigg,type="l",ylim=c(0,1),ylab="Trigg",xlab="Dönem")
      if(abs(input$beta-.1)<1e-8) abline(h=.51,col="red",lty=2)
    })
    output$iliski <- renderPrint({
      N <- fc_tamsayi(input$N,1); a <- fc_oran(input$alpha); yas <- fc_tamsayi(input$yas,1)
      validate(need(is.finite(input$sigma) && input$sigma>=0,"Sigma negatif olamaz."))
      cat("MA ortalama yaş:", (N+1)/2, "\nSES ortalama yaş:",1/a,
        "\nMA'ya eşdeğer α:",2/(N+1),"\nSES'e eşdeğer N (tam sayı olmayabilir):",2/a-1,
        "\nMA hata varyansı:",input$sigma^2*(N+1)/N,
        "\nSES hata varyansı:",input$sigma^2*2/(2-a),
        "\nMA tahminindeki düzeltme:",if(yas<=N) input$degisim/N else 0,
        "\nSES tahminindeki düzeltme:",a*(1-a)^(yas-1)*input$degisim)
    })
    output$agirlik <- renderPlot({
      N <- fc_tamsayi(input$N,1); a <- fc_oran(input$alpha)
      k <- seq_len(min(100,max(N,20)))
      plot(k,a*(1-a)^(k-1),type="h",col="#426b65",xlab="Veri yaşı",ylab="Ağırlık",ylim=c(0,max(a,1/N)))
      points(k,ifelse(k<=N,1/N,0),col="#ba7d36",pch=16)
      legend("topright",c("SES","MA"),col=c("#426b65","#ba7d36"),pch=c(NA,16),lty=c(1,NA),bty="n")
    })
  })
}
