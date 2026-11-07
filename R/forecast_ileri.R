ileri_ui <- function(id) {
  ns <- NS(id)
  tagList(h2("İleri konular: kayıp satış ve senaryolar"),
    p("§2.12 bu konuları genel olarak tanıtır; tam algoritma vermez. Aşağıdaki MLE ve Monte Carlo araçları, açık varsayımlı uygulama uzantılarıdır; kitaptaki özel algoritmanın birebir kopyası değildir."),
    uiOutput(ns("uyari")),
    card(card_header("Stok tükenmesinde sansürlü talep kestirimi"),
      p("Veri sekmesindeki talep sütunu bu araçta satış olarak yorumlanır. Her dönemin satışa sunulan toplam stok miktarını ayrı stok sütununa ekleyin. Yenileme/transfer içermeyen tek dönemli stok varsayılır."),
      selectInput(ns("stok"),"Stok sınırı sütunu",choices=character()),
      actionButton(ns("mle"),"Satış ve düzeltilmiş talebi karşılaştır"),
      DTOutput(ns("sonuc")),
      p("Varsayım: bağımsız, aynı normal talep dağılımı; satış=min(talep,stok). Satış=stok ise talep sağdan sansürlüdür. Olabilirlik: sansürsüzlerde normal yoğunluk, tükenenlerde P(D≥stok). Negatif talep olasılığı yüksekse normal model uygun olmayabilir.")),
    card(card_header("Monte Carlo talep senaryosu"),
      numericInput(ns("mu"),"Dönem başına varsayılan ortalama",100),
      numericInput(ns("sigma"),"Dönem başına standart sapma",30,min=.01),
      numericInput(ns("h"),"Ufuk",12,min=1,max=120),
      numericInput(ns("adet"),"Senaryo sayısı",2000,min=100,max=10000),
      numericInput(ns("seed"),"Tohum",42,min=0),
      actionButton(ns("simule"),"Senaryo üret"), plotOutput(ns("grafik")),DTOutput(ns("toplam")),
      p("D_t ~ Normal(μ,σ²), dönemler bağımsız. Negatif değerler otomatik kesilmez; böyle senaryolar varsa μ/σ varsayımını değiştirin. Grafik ilk en fazla 50 senaryoyu gösterir; toplam istatistikleri bütün senaryolardandır. Bu araç talebi geçmişten öğrenmez.")))
}

ileri_server <- function(id,veri) {
  moduleServer(id,function(input,output,session) {
    output$uyari <- renderUI(fc_uyari(veri))
    observeEvent(veri(),{
      v<-veri();req(v)
      sec<-names(v$ham)[vapply(v$ham,is.numeric,logical(1))]
      updateSelectInput(session,"stok",choices=setdiff(sec,c(v$talep,v$tarih)))
    })
    r<-eventReactive(list(input$mle,veri()),{
      req(input$mle>0);d<-fc_veri_al(veri)
      validate(need(length(input$stok)==1 && input$stok %in% names(d$ham),"Dosyada sayısal stok sütunu seçin."))
      tryCatch(fc_sansurlu(d$y,d$ham[[input$stok]]),error=function(e)validate(need(FALSE,conditionMessage(e))))
    },ignoreNULL=FALSE)
    output$sonuc<-renderDT(fc_tablo(r()))
    sim<-eventReactive(input$simule,{
      h<-fc_tamsayi(input$h,1,120);n<-fc_tamsayi(input$adet,100,10000)
      validate(need(is.finite(input$mu)&&is.finite(input$sigma)&&input$sigma>0,"Sonlu ortalama ve pozitif sigma gerekli."))
      set.seed(fc_tamsayi(input$seed,0))
      x <- matrix(rnorm(h*n,input$mu,input$sigma),nrow=h,ncol=n)
      attr(x,"mu") <- input$mu
      x
    })
    output$grafik<-renderPlot({
      x<-sim();matplot(x[,seq_len(min(ncol(x),50)),drop=FALSE],type="l",lty=1,
        col=adjustcolor("#426b65",.2),xlab="Gelecek dönem",ylab="Senaryo talebi")
      abline(h=attr(x,"mu"),lwd=2)
    })
    output$toplam<-renderDT({
      x<-sim();s<-colSums(x)
      fc_tablo(data.frame(ortalama_toplam=mean(s),sigma_toplam=sd(s),
        toplam_p025=unname(quantile(s,.025)),toplam_p975=unname(quantile(s,.975)),
        negatif_deger_orani=mean(x<0)))
    })
  })
}
