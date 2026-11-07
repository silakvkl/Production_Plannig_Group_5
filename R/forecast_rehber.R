rehber_ui <- function(id) {
  ns <- NS(id)
  tagList(h2("Forecasting rehberi ve kapsam"),
    card(card_header("Kitap ile uygulama eşlemesi"),
      tags$table(class="table table-striped",
        tags$thead(tags$tr(tags$th("Kitap"),tags$th("Uygulama"))),
        tags$tbody(
          tags$tr(tags$td("§2.1–2.2, s.55–56"),tags$td("Ufuk, belirsizlik ve toplulaştırma açıklamaları")),
          tags$tr(tags$td("§2.3, s.56–57"),tags$td("Nitel yöntemler ve uzman görüşü toplama")),
          tags$tr(tags$td("§2.4–2.5, s.57–61"),tags$td("Nedensel regresyon, seri tanısı ve ağırlıklar")),
          tags$tr(tags$td("§2.6, s.61–64"),tags$td("MAD, MSE, MAPE, hata grafiği, kümülatif hata")),
          tags$tr(tags$td("§2.7, s.64–74"),tags$td("MA, ağırlıklı MA, SES, çok adım ileri tahmin, α–N ilişkisi")),
          tags$tr(tags$td("§2.8, s.75–78"),tags$td("Doğrusal regresyon, Holt ve başlangıç seçenekleri")),
          tags$tr(tags$td("§2.9, s.79–89"),tags$td("Mevsim faktörleri, merkezli ortalama, arındırma/geri mevsimselleştirme, Winters")),
          tags$tr(tags$td("§2.10, s.89–105"),tags$td("ACF, AR/MA/ARMA simülasyonu, log/fark, ARIMA/SARIMA, artık tanısı")),
          tags$tr(tags$td("§2.11, s.105–107"),tags$td("Trigg sinyali, ortak test bloğu ve basit model karşılaştırması")),
          tags$tr(tags$td("§2.12, s.107–109"),tags$td("Açık varsayımlı Monte Carlo ve sansürlü normal MLE uzantıları")),
          tags$tr(tags$td("§2.13, s.110–112"),tags$td("Yumuşatılmış MAD, σ_e≈1.25 MAD, talep ve tahmin varyansı farkı")),
          tags$tr(tags$td("§2.14–2.15, s.112–113"),tags$td("Sınırlamalar ve rehber; yalnız adı geçen Kalman/spektral yöntemler ayrı algoritma olarak sunulmaz")),
          tags$tr(tags$td("Ek problemler ve Ek 2-A/2-B, s.113–122"),tags$td("Adaptif Trigg–Leach, parametre tarama, kayıt düzeltme etkisi, hata varyansı ve regresyon katsayıları"))
        ))),
    card(card_header("Nasıl ilerlemeli?"),
      tags$ol(
        tags$li("Veriyi eskiden yeniye, eşit aralıklı tek bir seri olarak yükleyin. Tarih ve talep sütununu seçin."),
        tags$li("Seri tanısında trend/mevsimselliği ve ACF'yi inceleyin."),
        tags$li("Durağan seride MA/SES; trendde regresyon/Holt; mevsimsellikte ayrıştırma/Winters deneyin."),
        tags$li("Uzun ve güvenilir geçmişte ARIMA kurup artıkların ACF'sini kontrol edin."),
        tags$li("Karşılaştırma sayfasında son dönemleri ayırın. Parametre seçimi için ayrı doğrulama bloğu kullanın."),
        tags$li("Hataları zaman içinde izleyin; kampanya veya kapasite değişimi gibi bilinen bilgileri ayrıca değerlendirin."))),
    card(card_header("Yorumlama kuralları"),
      p("Tahminler genellikle yanlıştır; daha uzun ufukta belirsizlik artar. Ürün grubu toplamları yüzde olarak daha kararlı olabilir; bağımlılık yapısı önemlidir."),
      p("Bir grafiğin trende benzemesi trendin devam edeceği anlamına gelmez. En düşük eğitim hatası en iyi gelecek tahmini garantilemez."),
      p("R'nin istatistiksel yenilikleri D-F, uygulamanın değerlendirme hatası F-D işaretindedir. MA(q) modelini basit hareketli ortalamayla karıştırmayın."),
      p("Kitap örnekleri ara adımlarda yuvarlar; uygulama tam hassasiyetle hesaplar. Winters her adımda normalleştirildiği için kitaptaki normalleştirmeyi atlayan yaklaşık örneklerden de ayrılabilir."),
      p("Kitap bütün ileri konuların algoritmasını vermez. Nitel yöntemler insan girdisi gerektirir. Kalman, Wiener, sinir ağları ve firmaya özgü simülasyonlar yalnız kavramsal olarak anılır; bunlar uygulanmış gibi etiketlenmemiştir.")))
}

rehber_server <- function(id,veri) {
  moduleServer(id,function(input,output,session) {})
}

nitel_ui <- function(id) {
  ns<-NS(id)
  tagList(h2("Nitel yöntemler ve uzman görüşleri"),
    card(card_header("Kitaptaki dört yaklaşım"),
      tags$ul(
        tags$li(strong("Satış ekibi bileşimi: "),"Çakışmayan bölge/ürün tahminlerini toplayın; kota kaynaklı düşük tahmin yanlılığına dikkat edin."),
        tags$li(strong("Müşteri anketi: "),"Temsili örneklem ve tarafsız soru tasarımı gerektirir; otomatik bir zaman serisi formülü değildir."),
        tags$li(strong("Yönetici/uzman görüşü: "),"Yeni üründe farklı işlevlerin beklentilerini gerekçeleriyle birleştirin."),
        tags$li(strong("Delphi: "),"Anonim görüş toplayın, dağılımı geri bildirin, yeni tur isteyin. Bu araç tek turu özetler; görüş toplamaz veya uzlaşma gerçekleştiğini iddia etmez."))),
    card(card_header("Bir tur görüşü özetle"),
      textAreaInput(ns("gorus"),"Her satır: birim/uzman,tahmin", "Bolge_A,120\nBolge_B,150\nBolge_C,130",rows=6),
      selectInput(ns("birlestir"),"Görüşlerin anlamı",c("Farklı bölgeler: toplam"="toplam","Aynı büyüklüğe uzman tahminleri: ortalama/medyan"="uzman")),
      actionButton(ns("hesapla"),"Görüşleri özetle"), DTOutput(ns("tablo")),verbatimTextOutput(ns("ozet")),
      p("Bu basit özetleme aracı uygulama desteğidir. Delphi'nin anonimlik ve çok turlu süreç yönetimi bu oturumda otomatik yürütülmez.")))
}

nitel_server<-function(id,veri) {
  moduleServer(id,function(input,output,session) {
    r<-eventReactive(input$hesapla,{
      tryCatch({
        x<-read.table(text=input$gorus,sep=",",header=FALSE,stringsAsFactors=FALSE)
        validate(need(ncol(x)==2 && is.numeric(x[[2]]) && all(is.finite(x[[2]])),"Her satırı isim,sayı biçiminde girin."))
        names(x)<-c("kaynak","tahmin");list(x=x,tip=input$birlestir)
      },error=function(e)validate(need(FALSE,conditionMessage(e))))
    })
    output$tablo<-renderDT(fc_tablo(r()$x))
    output$ozet<-renderPrint({
      y<-r()$x$tahmin
      if(r()$tip=="toplam") cat("Çakışmayan birimlerin toplam tahmini:",sum(y)) else {
        cat("Ortalama:",mean(y),"\nMedyan:",median(y),"\nEn düşük:",min(y),"\nEn yüksek:",max(y),"\nGörüş sayısı:",length(y))
      }
    })
  })
}
