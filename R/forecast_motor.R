# Nahmias, Bölüm 2: hesaplama motoru. Hata = tahmin - gerçekleşen.
# Bu dosya Shiny'den bağımsızdır; sayısal testlerde doğrudan kullanılabilir.
fc_kontrol <- function(x, en_az = 2L) {
  x <- as.numeric(x)
  if (length(x) < en_az || any(!is.finite(x)))
    stop(paste0("En az ", en_az, " sonlu sayısal gözlem gerekli."))
  x
}

fc_tamsayi <- function(x, alt = 1, ust = Inf) {
  if (length(x) != 1 || !is.finite(x) || x != floor(x) || x < alt || x > ust)
    stop(paste("Tam sayı gerekli; izin verilen aralık:", alt, "-", ust))
  as.integer(x)
}

fc_oran <- function(x) {
  if (length(x) != 1 || !is.finite(x) || x <= 0 || x >= 1)
    stop("Düzeltme katsayıları 0 ile 1 arasında olmalı (uçlar hariç).")
  x
}

fc_sayilar <- function(metin) {
  # Ondalık nokta; liste ayırıcı virgül, boşluk veya noktalı virgül.
  z <- strsplit(trimws(metin), "[,;[:space:]]+")[[1]]
  x <- suppressWarnings(as.numeric(z))
  if (!length(x) || any(!is.finite(x))) stop("Sayıları noktalı ondalıkla yazın: 0.1, 0.2, 0.7")
  x
}

fc_regresyon <- function(y, x = seq_along(y)) {
  y <- fc_kontrol(y); x <- fc_kontrol(x)
  if (length(x) != length(y) || var(x) == 0) stop("Regresyon için değişken X değerleri gerekli.")
  n <- length(y)
  sxx <- n * sum(x^2) - sum(x)^2
  sxy <- n * sum(x * y) - sum(x) * sum(y)
  b <- sxy / sxx
  a <- mean(y) - b * mean(x)
  list(a = a, b = b, Sxx = sxx, Sxy = sxy, uyum = a + b * x)
}

fc_olculer <- function(y, f) {
  ok <- is.finite(y) & is.finite(f)
  y <- y[ok]; f <- f[ok]; e <- f - y
  if (!length(e)) return(data.frame(n = 0, ME = NA_real_, MAD = NA_real_,
    MSE = NA_real_, RMSE = NA_real_, MAPE = NA_real_, MAPE_n = 0, sigma_MAD = NA_real_))
  nz <- y != 0
  data.frame(n = length(e), ME = mean(e), MAD = mean(abs(e)),
    MSE = mean(e^2), RMSE = sqrt(mean(e^2)),
    MAPE = if (any(nz)) 100 * mean(abs(e[nz] / y[nz])) else NA_real_,
    MAPE_n = sum(nz), sigma_MAD = 1.25 * mean(abs(e)))
}

fc_izleme <- function(y, f, beta = .1, mad0 = 0) {
  fc_oran(beta)
  if (!is.finite(mad0) || mad0 < 0) stop("Başlangıç MAD negatif olamaz.")
  e <- f - y; n <- length(e)
  E <- M <- T <- toplam <- rep(NA_real_, n)
  es <- 0; ms <- mad0; cs <- 0
  for (i in seq_len(n)) if (is.finite(e[i])) {
    es <- beta * e[i] + (1 - beta) * es
    ms <- beta * abs(e[i]) + (1 - beta) * ms
    cs <- cs + e[i]
    E[i] <- es; M[i] <- ms; T[i] <- if (ms > 0) abs(es) / ms else 0
    toplam[i] <- cs
  }
  data.frame(hata = e, mutlak_hata = abs(e), kare_hata = e^2,
    yuzde_hata = ifelse(y == 0, NA_real_, 100 * abs(e / y)),
    kumulatif_hata = toplam, yumusatilmis_hata = E,
    yumusatilmis_MAD = M, sigma_e = 1.25 * M, Trigg = T)
}

fc_mevsim <- function(y, m, tip = "merkezli") {
  y <- fc_kontrol(y); m <- fc_tamsayi(m, 3)
  n <- length(y)
  if (n < 2 * m) stop("Mevsimsellik için en az iki tam mevsim gerekli.")
  if (any(y <= 0)) stop("Çarpımsal mevsimsel yöntemlerde bütün talepler pozitif olmalı.")
  konum <- (seq_len(n) - 1) %% m + 1
  cma <- rep(NA_real_, n)
  if (tip == "basit") {
    if (n %% m != 0) stop("Basit faktör yöntemi için tam mevsim sayısı kullanın.")
    cma[] <- mean(y)
  } else {
    ma <- vapply(seq_len(n - m + 1), function(i) mean(y[i:(i + m - 1)]), numeric(1))
    if (m %% 2 == 0) {
      k <- (m / 2 + 1):(n - m / 2)
      cma[k] <- (head(ma, -1) + tail(ma, -1)) / 2
    } else {
      k <- ((m + 1) / 2):(n - (m - 1) / 2)
      cma[k] <- ma
    }
    # Kitap s.82: başlangıç/son uçları en yakın iki merkezli değerin ortalamasıyla doldur.
    ilk <- min(k); son <- max(k)
    cma[seq_len(ilk - 1)] <- mean(cma[k[seq_len(min(2, length(k))) ]])
    if (son < n) cma[(son + 1):n] <- mean(cma[tail(k, min(2, length(k)))])
  }
  oran <- y / cma
  ham <- vapply(seq_len(m), function(j) mean(oran[konum == j]), numeric(1))
  faktor <- m * ham / sum(ham)
  list(faktor = faktor, tablo = data.frame(donem = seq_len(n), talep = y,
    merkezli_ortalama = cma, oran = oran, mevsim_konumu = konum,
    faktor = faktor[konum], arindirilmis = y / faktor[konum]))
}

fc_winters_baslat <- function(y, m) {
  if (length(y) < 2 * m) stop("Winters başlangıcı için en az iki mevsim gerekli.")
  # Kitap s.86-87: ilk iki mevsim başlangıç bloğudur; sonraki veriler güncellenir.
  z <- y[seq_len(2 * m)]
  v1 <- mean(z[seq_len(m)]); v2 <- mean(z[(m + 1):(2 * m)])
  g <- (v2 - v1) / m
  s <- v2 + g * (m - 1) / 2
  taban <- s + (seq_len(2 * m) - 2 * m) * g
  if (any(taban <= 0)) stop("Winters başlangıç eğilimi pozitif taban üretmiyor.")
  ham <- z / taban
  c <- (ham[seq_len(m)] + ham[(m + 1):(2 * m)]) / 2
  list(S = s, G = g, c = m * c / sum(c))
}

fc_tahmin <- function(y, yontem, h = 6, p = list()) {
  y <- fc_kontrol(y)
  h <- fc_tamsayi(h, 1, 120)
  n <- length(y); fitted <- rep(NA_real_, n); detay <- NULL; model <- NULL
  notlar <- character(); alt <- ust <- rep(NA_real_, h)
  getp <- function(ad, varsayilan) if (is.null(p[[ad]])) varsayilan else p[[ad]]
  N <- getp("N", 3); a <- getp("alpha", .1); b <- getp("beta", .1)
  g <- getp("gamma", .1); m <- getp("m", 4)

  if (yontem %in% c("ma", "wma")) {
    N <- fc_tamsayi(N, 1, n)
    w <- if (yontem == "ma") rep(1/N, N) else fc_kontrol(getp("agirlik", rep(1/N, N)), 1)
    if (length(w) != N || any(w < 0) || sum(w) <= 0) stop("N adet negatif olmayan ağırlık girin.")
    w <- w / sum(w)
    if (n > N) for (i in (N + 1):n) fitted[i] <- sum(w * y[(i - N):(i - 1)])
    gelecek <- rep(sum(w * tail(y, N)), h)
    detay <- data.frame(yas = N:1, agirlik = w)
    notlar <- c(notlar, "Ağırlık sırası eskiden yeniye. Durağanlık varsayımıyla bütün ileri ufuklar aynı tahmini alır.")
  } else if (yontem %in% c("ses", "adaptif", "holt")) {
    fc_oran(a); fc_oran(b)
    baslangic <- getp("baslangic", "ilk")
    if (baslangic == "blok") {
      k <- fc_tamsayi(getp("k", 5), 2, n)
      if (yontem == "holt") {
        r <- fc_regresyon(y[seq_len(k)])
        s <- r$a + r$b * k; egim <- r$b
      } else { s <- mean(y[seq_len(k)]); egim <- 0 }
      ilk <- k + 1
    } else if (baslangic == "manuel") {
      s <- getp("S0", 200); egim <- getp("G0", 10); ilk <- 1
      if (!all(is.finite(c(s, egim)))) stop("Başlangıç değerleri sonlu olmalı.")
    } else {
      s <- y[1]; egim <- 0; ilk <- 2
    }
    S <- G <- A <- rep(NA_real_, n); es <- ms <- 0
    if (ilk > 1) { S[ilk - 1] <- s; G[ilk - 1] <- egim }
    if (ilk <= n) for (i in ilk:n) {
      fitted[i] <- s + if (yontem == "holt") egim else 0
      ai <- a
      if (yontem == "adaptif") {
        ei <- fitted[i] - y[i]
        if (i == ilk) { es <- ei; ms <- abs(ei) } else {
          es <- b * ei + (1 - b) * es; ms <- b * abs(ei) + (1 - b) * ms
        }
        ai <- if (ms > 0) abs(es) / ms else 0
      }
      eski <- s
      s <- ai * y[i] + (1 - ai) * fitted[i]
      if (yontem == "holt") egim <- b * (s - eski) + (1 - b) * egim
      S[i] <- s; G[i] <- egim; A[i] <- ai
    }
    gelecek <- s + if (yontem == "holt") seq_len(h) * egim else rep(0, h)
    detay <- data.frame(donem = seq_len(n), seviye = S, egim = G, alpha = A)
    notlar <- c(notlar, "Başlangıç bloğu hata hesabına katılmaz. Manuel S0/G0 verisi ilk gözlemden öncesine aittir; SES için S0 = F1.")
  } else if (yontem == "regresyon") {
    r <- fc_regresyon(y)
    fitted <- r$uyum; gelecek <- r$a + r$b * (n + seq_len(h))
    detay <- data.frame(a = r$a, b = r$b, Sxx = r$Sxx, Sxy = r$Sxy)
    notlar <- c(notlar, "Geçmiş çizgi bütün verilere uydurulmuştur; geçmiş hata ölçüleri eğitim hatasıdır. Gerçek tahmin başarısı için Karşılaştırma sayfasını kullanın.")
  } else if (yontem %in% c("mevsim_basit", "ayristirma")) {
    m <- fc_tamsayi(m, 3)
    r <- fc_mevsim(y, m, if (yontem == "mevsim_basit") "basit" else "merkezli")
    ind <- (n + seq_len(h) - 1) %% m + 1
    if (yontem == "mevsim_basit") {
      gelecek <- mean(y) * r$faktor[ind]
      fitted <- mean(y) * r$tablo$faktor
    } else {
      baz <- getp("baz", "regresyon")
      z <- fc_tahmin(r$tablo$arindirilmis, baz, h, p)
      gelecek <- z$gelecek$tahmin * r$faktor[ind]
      fitted <- z$gecmis$tahmin * r$tablo$faktor
      notlar <- c(notlar, paste("Arındırılmış seri için baz yöntem:", baz))
    }
    detay <- r$tablo
    notlar <- c(notlar, "Mevsim faktörleri bütün geçmişten hesaplanır; geçmiş hatalar eğitim/uyum hatasıdır. Testte faktörler yalnızca eğitim verisiyle hesaplanır.")
  } else if (yontem == "winters") {
    m <- fc_tamsayi(m, 3); fc_oran(a); fc_oran(b); fc_oran(g)
    if (any(y <= 0)) stop("Çarpımsal Winters pozitif talep gerektirir.")
    z <- fc_winters_baslat(y, m); s <- z$S; egim <- z$G; c <- z$c
    S <- G <- C <- rep(NA_real_, n)
    S[2*m] <- s; G[2*m] <- egim
    if (n > 2*m) for (i in (2*m + 1):n) {
      j <- (i - 1) %% m + 1
      fitted[i] <- (s + egim) * c[j]
      eski <- s
      s <- a * y[i] / c[j] + (1 - a) * (s + egim)
      if (s <= 0 || !is.finite(s)) stop("Winters seviyesi pozitif değil; parametreleri kontrol edin.")
      egim <- b * (s - eski) + (1 - b) * egim
      c[j] <- g * y[i] / s + (1 - g) * c[j]
      c <- m * c / sum(c)
      S[i] <- s; G[i] <- egim; C[i] <- c[j]
    }
    gelecek <- (s + seq_len(h) * egim) * c[(n + seq_len(h) - 1) %% m + 1]
    detay <- data.frame(donem = seq_len(n), seviye = S, egim = G, guncel_faktor = C)
    model <- list(baslangic = z, son_faktorler = c)
    notlar <- c(notlar, "İlk 2m gözlem başlangıç içindir; hata üretilmez. Her güncellemede m faktörünün toplamı m yapılır; kitaptaki yuvarlanmış örneklerden küçük fark olabilir.")
  } else if (yontem == "arima") {
    ord <- getp("order", c(1, 0, 0))
    sea <- getp("seasonal", c(0, 0, 0))
    if (length(ord) != 3 || length(sea) != 3 || any(!is.finite(c(ord,sea))) ||
        any(c(ord,sea) != floor(c(ord,sea))) || any(c(ord, sea) < 0) || any(c(ord, sea) > 3))
      stop("ARIMA dereceleri 0-3 arasında tam sayı olmalı.")
    m <- fc_tamsayi(m, 3)
    loglu <- isTRUE(getp("log", FALSE))
    if (loglu && any(y <= 0)) stop("Log dönüşümü pozitif veri gerektirir.")
    z <- if (loglu) log(y) else y
    if (n - ord[2] - sea[2] * m <= 5 + sum(ord[c(1,3)]) + sum(sea[c(1,3)]))
      stop("Seçilen ARIMA dereceleri için veri sayısı yetersiz.")
    xreg <- if (isTRUE(getp("drift", FALSE)) && ord[2] + sea[2] == 1) matrix(seq_len(n), ncol = 1) else NULL
    seas <- if (any(sea != 0)) list(order = sea, period = m) else list(order = c(0,0,0), period = NA)
    model <- stats::arima(z, order = ord, seasonal = seas, xreg = xreg,
      include.mean = ord[2] + sea[2] == 0, method = "ML")
    if (model$code != 0) stop("ARIMA optimizasyonu yakınsamadı; daha basit bir model deneyin.")
    pr <- stats::predict(model, n.ahead = h,
      newxreg = if (!is.null(xreg)) matrix(n + seq_len(h), ncol = 1) else NULL)
    fitted <- z - as.numeric(residuals(model))
    gelecek <- as.numeric(pr$pred)
    alt <- gelecek - qnorm(.975) * as.numeric(pr$se)
    ust <- gelecek + qnorm(.975) * as.numeric(pr$se)
    if (loglu) { fitted <- exp(fitted); gelecek <- exp(gelecek); alt <- exp(alt); ust <- exp(ust) }
    atla <- max(1, ord[2] + sea[2] * m, ord[1], ord[3], sea[1] * m, sea[3] * m)
    fitted[seq_len(min(n, atla))] <- NA_real_
    vc <- diag(model$var.coef)
    sorunlu <- !is.finite(vc) | vc < 0
    if (any(sorunlu)) {
      vc[sorunlu] <- NA_real_
      notlar <- c(notlar, "Bazı katsayı standart hataları kestirilemedi; model zayıf tanımlanmış olabilir. Daha basit dereceler deneyin. Bu katsayıların standart hatası NA gösterilir.")
    }
    detay <- data.frame(terim = names(coef(model)), katsayi = as.numeric(coef(model)),
      standart_hata = sqrt(vc))
    notlar <- c(notlar, "ARIMA geçmiş değerleri bütün veride kestirilen parametrelerle koşullu uyumdur; bağımsız test değildir.",
      "R: D_t = AR terimleri + epsilon_t + theta*epsilon_(t-1). Kitabın eksi MA katsayısı b için theta = -b; yenilik epsilon ile değerlendirme hatası F-D farklı işaretlidir.",
      "%95 tahmin aralığı koşullu normal varsayımına dayanır; parametre kestirim belirsizliğini içermez.")
    if (loglu) notlar <- c(notlar, "Log modelde exp(tahmin) özgün ölçekte medyandır; aritmetik ortalama değildir.")
    if (n < 72) notlar <- c(notlar, "Kitap ARIMA için en az 72 geçmiş gözlem önerir; mevcut seri daha kısa.")
  } else stop("Tanımsız yöntem.")

  if (any(!is.finite(gelecek))) stop("Tahmin sonlu değil; verileri ve parametreleri kontrol edin.")
  if (any(gelecek < 0)) notlar <- c(notlar, "Negatif talep tahmini oluştu. Sonuç otomatik olarak sıfıra kesilmedi; model uygunluğunu değerlendirin.")
  list(gecmis = data.frame(donem = seq_len(n), gercek = y, tahmin = fitted),
    gelecek = data.frame(donem = n + seq_len(h), ufuk = seq_len(h), tahmin = gelecek,
      alt_95 = alt, ust_95 = ust), detay = detay, model = model,
    olculer = fc_olculer(y, fitted), notlar = notlar)
}

fc_karsilastir <- function(y, yontemler, test, p = list(), mod = "sabit", ortalama = FALSE) {
  y <- fc_kontrol(y); test <- fc_tamsayi(test, 1, length(y) - 2)
  n <- length(y); egitim <- n - test; tablolar <- list(); ozet <- list()
  for (ad in yontemler) {
    hata <- NULL
    z <- tryCatch({
      if (mod == "sabit") fc_tahmin(y[seq_len(egitim)], ad, test, p)$gelecek$tahmin else {
        vapply((egitim + 1):n, function(i) {
          fc_tahmin(y[seq_len(i - 1)], ad, 1, p)$gelecek$tahmin[1]
        }, numeric(1))
      }
    }, error = function(e) { hata <<- conditionMessage(e); NULL })
    if (is.null(z)) {
      ozet[[ad]] <- data.frame(yontem = ad, fc_olculer(numeric(), numeric()), durum = hata)
    } else {
      gercek <- tail(y, test)
      ozet[[ad]] <- data.frame(yontem = ad, fc_olculer(gercek, z), durum = "Başarılı")
      tablolar[[ad]] <- data.frame(yontem = ad, donem = (egitim + 1):n, gercek = gercek, tahmin = z)
    }
  }
  if (isTRUE(ortalama) && length(tablolar) >= 2) {
    birlesik <- rowMeans(do.call(cbind,lapply(tablolar,function(t) t$tahmin)))
    gercek <- tail(y,test)
    ozet[["ortalama"]] <- data.frame(yontem="ortalama",fc_olculer(gercek,birlesik),
      durum=paste("Başarılı yöntemlerin ortalaması:",paste(names(tablolar),collapse=", ")))
    tablolar[["ortalama"]] <- data.frame(yontem="ortalama",donem=(egitim+1):n,gercek=gercek,tahmin=birlesik)
  }
  list(ozet = do.call(rbind, ozet), detay = if (length(tablolar)) do.call(rbind, tablolar) else NULL)
}

fc_sansurlu <- function(satis, stok) {
  satis <- fc_kontrol(satis, 5); stok <- fc_kontrol(stok, 5)
  if (length(satis) != length(stok) || any(stok <= 0) || any(satis < 0) || any(satis > stok))
    stop("Satış ve stok uzunlukları eşit olmalı; 0 <= satış <= stok ve stok > 0 gerekli.")
  sansur <- satis >= stok
  if (sum(!sansur) < 3) stop("En az üç sansürsüz gözlem gerekli; yalnızca tükenen stoklarla talep tanımlanamaz.")
  hedef <- function(par) {
    mu <- par[1]; sigma <- exp(par[2])
    if (!is.finite(sigma) || sigma < .Machine$double.eps) return(1e100)
    deger <- -sum(dnorm(satis[!sansur], mu, sigma, log = TRUE)) -
      sum(pnorm(stok[sansur], mu, sigma, lower.tail = FALSE, log.p = TRUE))
    if (is.finite(deger)) deger else 1e100
  }
  fit <- optim(c(mean(satis), log(max(sd(satis), .1))), hedef, method = "BFGS")
  if (fit$convergence != 0 || !all(is.finite(fit$par))) stop("Sansürlü normal model yakınsamadı.")
  data.frame(yontem = c("Satışları talep sayan", "Sansürlü normal MLE"),
    ortalama = c(mean(satis), fit$par[1]), sigma = c(sd(satis), exp(fit$par[2])),
    sansurlu_gozlem = sum(sansur))
}
