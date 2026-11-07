fc_adlar <- c(
  ma = "Basit hareketli ortalama", wma = "Ağırlıklı hareketli ortalama",
  ses = "Basit üstel düzeltme", adaptif = "Trigg–Leach adaptif düzeltme",
  regresyon = "Doğrusal trend regresyonu", holt = "Holt",
  mevsim_basit = "Trendsiz mevsim faktörleri", ayristirma = "Merkezli ortalama ile ayrıştırma",
  winters = "Çarpımsal Winters", arima = "AR / MA / ARMA / ARIMA / SARIMA"
)

fc_formuller <- list(
  ma = c("F_t = (D_(t-1) + ... + D_(t-N)) / N",
    "F_(t+1) = F_t + (D_t - D_(t-N)) / N",
    "Durağan varsayım: F_(t,t+h) = son N gözlemin ortalaması; her h için aynı.",
    "Kaynak: §2.7, s.64-66. Hesaplamada ara yuvarlama yapılmaz."),
  wma = c("F_t = Σ w_j D_(t-N+j-1), j=1,...,N; Σw_j=1",
    "Ağırlıklar eskiden yeniye yazılır; toplam 1 değilse normalize edilir.",
    "Kaynak: §2.5 genel ağırlık gösterimi ve §2.6 problem 11; §2.7 bu yöntemi ayrıntılı işlemez."),
  ses = c("F_(t+1) = α D_t + (1-α) F_t = F_t - α e_t",
    "e_t = F_t - D_t; 0 < α < 1",
    "Ağırlık: α(1-α)^(j-1); ortalama veri yaşı = 1/α",
    "F_(t,t+h) = F_(t+1); durağan modelde bütün ufuklar aynı.",
    "Kaynak: §2.7, s.67-72."),
  adaptif = c("E_t = β e_t + (1-β)E_(t-1)",
    "M_t = β|e_t| + (1-β)M_(t-1)",
    "α_t = |E_t|/M_t; M_t=0 ise α_t=0",
    "F_(t+1) = α_t D_t + (1-α_t)F_t",
    "İlk gözlenen hata ile E ve M başlatılır. Gelecek ufuklarda düzey sabittir.",
    "Kaynak: Ek problemler 57, s.117. Kitap bu yöntemin her seride üstün olmadığını vurgular."),
  regresyon = c("D_hat(t) = a + b t",
    "Sxy = n Σ(t D_t) - Σt ΣD_t; Sxx = n Σ(t²) - (Σt)²",
    "b = Sxy/Sxx; a = D_ortalama - b t_ortalama",
    "F_(n,n+h) = a + b(n+h)",
    "Kaynak: §2.8 s.75-76 ve Ek 2-B s.120-122."),
  holt = c("S_t = α D_t + (1-α)(S_(t-1)+G_(t-1))",
    "G_t = β(S_t-S_(t-1)) + (1-β)G_(t-1)",
    "F_(t,t+h) = S_t + h G_t",
    "Blok başlangıcında regresyonun blok sonundaki değeri S_k, eğimi G_k alınır.",
    "Kaynak: §2.8 s.77-78."),
  mevsim_basit = c("c_j = aynı mevsim konumundaki D değerlerinin ortalaması / genel ortalama",
    "Σc_j=m; D_arindirilmis(t)=D_t/c_j; F_(n,n+h)=D_ortalama c_j",
    "En az iki tam mevsim; trendsiz ve çarpımsal mevsimsellik varsayılır.",
    "Kaynak: §2.9 s.79-80."),
  ayristirma = c("1. m dönemlik hareketli ortalamaları merkezle.",
    "2. m çiftse komşu iki ortalamayı tekrar ortala.",
    "3. Uçları kitaptaki gibi en yakın iki merkezli değerin ortalamasıyla tamamla.",
    "4. r_t=D_t/CMA_t; aynı konumdaki r değerlerini ortala.",
    "5. c_j=m r_ortalama(j)/Σr_ortalama(j); D_arindirilmis=D_t/c_j",
    "6. Arındırılmış seriyi tahmin et; F=F_arindirilmis × c_gelecek.",
    "Kaynak: §2.9 s.81-83; uç tamamlama, klasik yazılım varsayılanlarından farklıdır."),
  winters = c("S_t=α(D_t/c_(t-m))+(1-α)(S_(t-1)+G_(t-1))",
    "G_t=β(S_t-S_(t-1))+(1-β)G_(t-1)",
    "c_t=γ(D_t/S_t)+(1-γ)c_(t-m); son m faktörü toplamı m olacak şekilde normalize et.",
    "F_(t,t+h)=(S_t+h G_t) × uygun son mevsim faktörü",
    "İlk iki mevsim ortalamaları V1,V2: G0=(V2-V1)/m; S0=V2+G0(m-1)/2.",
    "Başlangıç faktörleri bu trend çizgisine oranlardan bulunur ve normalize edilir.",
    "Kaynak: §2.9 s.84-89. Başlangıç bloğuna geçmiş tahmin yazılmaz."),
  arima = c("AR(p): D_t = a0 + Σa_i D_(t-i) + ε_t",
    "Kitap MA(q): D_t=b0 - Σb_i ε_(t-i) + ε_t",
    "R MA(q): D_t=μ + Σθ_i ε_(t-i) + ε_t; θ_i=-b_i",
    "ARIMA: φ(B)(1-B)^d D_t = θ(B)ε_t",
    "Mevsimsel: Φ(B^m)(1-B^m)^D φ(B)(1-B)^d D_t = Θ(B^m)θ(B)ε_t",
    "p,d,q: normal; P,D,Q: mevsimsel dereceler. Drift yalnızca toplam fark derecesi 1 ise eklenir.",
    "Log dönüşümde exp(z_hat) medyan verir. Aralıklar özgün ölçeğe geri çevrilir.",
    "AR(1): ρ_k=a1^k; MA(1): ρ1=-b1/(1+b1²), sonraki ρ'lar sıfır.",
    "Kaynak: §2.10 s.89-105. Basit hareketli ortalama ile stokastik MA(q) farklıdır.")
)

fc_veri_al <- function(veri) {
  v <- veri()
  shiny::validate(shiny::need(!is.null(v), "Önce Veri sekmesinden dosya yükleyin."))
  shiny::validate(shiny::need(isTRUE(v$hazir), "Veri sekmesindeki sütun seçimlerini kontrol edin."))
  d <- v$ham
  etiket <- as.character(d[[v$tarih]])
  shiny::validate(shiny::need(all(nzchar(trimws(etiket))), "Boş tarih/dönem etiketi var."))
  shiny::validate(shiny::need(!anyDuplicated(etiket), "Tarih/dönem etiketleri tekil olmalı. Ürünleri önce ayrı serilere ayırın."))
  t <- d[[v$tarih]]
  if (is.numeric(t) || inherits(t,"Date") || inherits(t,"POSIXt") || all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",etiket)))
    shiny::validate(shiny::need(!is.unsorted(t), "Tarih/dönem sütununu eskiden yeniye sıralayıp dosyayı tekrar yükleyin."))
  list(y = fc_kontrol(d[[v$talep]]), etiket = etiket, ham = d)
}

fc_tablo <- function(x) {
  DT::datatable(x, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE),
    filter = "none")
}

fc_uyari <- function(veri) {
  v <- veri()
  if (is.null(v) || !isTRUE(v$hazir)) {
    div(class = "alert alert-warning", p("Önce Veri sekmesinden dosya yükleyin ve sütunları seçin."),
      gecis("veri", "Veri sekmesine git", "database"))
  } else div(class = "alert alert-info",
    "Her satır tek bir eşit aralıklı dönemi temsil etmeli. Satırlar eskiden yeniye sıralı olmalı; eksik dönemleri dosyada kontrol edin.")
}

fc_grafik <- function(z) {
  g <- z$gecmis; f <- z$gelecek
  aralik <- range(c(g$gercek, g$tahmin, f$tahmin, f$alt_95, f$ust_95), finite = TRUE)
  plot(g$donem, g$gercek, type = "o", pch = 16, col = "#253248",
    xlim = c(1, max(f$donem)), ylim = aralik, xlab = "Dönem (satır sırası)", ylab = "Talep")
  if (any(is.finite(f$alt_95))) polygon(c(f$donem, rev(f$donem)),
    c(f$alt_95, rev(f$ust_95)), col = adjustcolor("#426b65", .18), border = NA)
  lines(g$donem, g$tahmin, col = "#ba7d36", lwd = 2)
  lines(c(tail(g$donem,1), f$donem), c(tail(g$gercek,1), f$tahmin), col = "#426b65", lwd = 2, lty = 2)
  abline(v = nrow(g) + .5, lty = 3)
  legend("topleft", c("Gerçekleşen", "Geçmiş tahmin / uyum", "Gelecek tahmin"),
    col = c("#253248", "#ba7d36", "#426b65"), lty = c(1,1,2), bty = "n", cex = .85)
}
