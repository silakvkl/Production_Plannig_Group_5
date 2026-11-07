# Paket kurulumu yalnız bir kez Console'da yapılır.
# install.packages(c("shiny", "bslib", "DT", "readxl"))
library(shiny)
library(bslib)
library(DT)
library(readxl)

# R klasöründeki tüm modüller otomatik yüklenir.
for (dosya in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(dosya, local = TRUE, encoding = "UTF-8")
}

# Menü sırası ve kategori sırası bu listeden belirlenir.
moduller <- list(
  list(id = "ana", baslik = "Ana sayfa", ikon = "house",
       kategori = NA_character_, ui = ana_sayfa_ui, server = ana_sayfa_server),
  list(id = "veri", baslik = "Veri", ikon = "database",
       kategori = NA_character_, ui = veri_ui, server = veri_server),
  list(id = "hareketli", baslik = "Hareketli Ortalama", ikon = "chart-line",
       kategori = "Tahmin (Forecasting)", ui = hareketli_ortalama_ui,
       server = hareketli_ortalama_server),
  list(id = "duragan", baslik = "Durağan Seri", ikon = "wave-square",
       kategori = "Tahmin (Forecasting)", ui = duragan_seri_ui,
       server = duragan_seri_server),
  list(id = "trend", baslik = "Trend", ikon = "arrow-trend-up",
       kategori = "Tahmin (Forecasting)", ui = trend_ui, server = trend_server),
  list(id = "rehber", baslik = "Forecasting Rehberi", ikon = "book-open",
       kategori = "Tahmin (Forecasting)", ui = rehber_ui, server = rehber_server),
  list(id = "mevsimsel", baslik = "Mevsimsel Yöntemler", ikon = "calendar",
       kategori = "Tahmin (Forecasting)", ui = mevsimsel_ui, server = mevsimsel_server),
  list(id = "arima", baslik = "Box–Jenkins / ARIMA", ikon = "chart-line",
       kategori = "Tahmin (Forecasting)", ui = arima_ui, server = arima_server),
  list(id = "tani", baslik = "Seri Tanısı / ACF", ikon = "magnifying-glass-chart",
       kategori = "Tahmin (Forecasting)", ui = tani_ui, server = tani_server),
  list(id = "karsilastirma", baslik = "Yöntem Karşılaştırma", ikon = "scale-balanced",
       kategori = "Tahmin (Forecasting)", ui = karsilastirma_ui, server = karsilastirma_server),
  list(id = "nedensel", baslik = "Nedensel Regresyon", ikon = "diagram-project",
       kategori = "Tahmin (Forecasting)", ui = nedensel_ui, server = nedensel_server),
  list(id = "hata", baslik = "Hata ve Teorik İlişkiler", ikon = "square-root-variable",
       kategori = "Tahmin (Forecasting)", ui = hata_ui, server = hata_server),
  list(id = "nitel", baslik = "Nitel Yöntemler", ikon = "users",
       kategori = "Tahmin (Forecasting)", ui = nitel_ui, server = nitel_server),
  list(id = "ileri", baslik = "Kayıp Satış / Senaryolar", ikon = "flask",
       kategori = "Tahmin (Forecasting)", ui = ileri_ui, server = ileri_server),
  list(id = "ogrenme", baslik = "Öğrenme Eğrisi", ikon = "graduation-cap",
       kategori = NA_character_, ui = ogrenme_egrisi_ui,
       server = ogrenme_egrisi_server),
  list(id = "yardim", baslik = "Yardım / Hakkında", ikon = "circle-question",
       kategori = NA_character_, ui = yardim_ui, server = yardim_server)
)

ids <- vapply(moduller, function(m) m$id, character(1))
stopifnot(!anyDuplicated(ids), all(grepl("^[a-z][a-z0-9_]*$", ids)),
          all(c("ana", "veri") %in% ids))

# Bu düğmeler menüde, kartlarda ve veri uyarısında aynı şekilde çalışır.
gecis <- function(id, metin, ikon = NULL, aktif = FALSE, alt = FALSE) {
  tags$button(
    type = "button", "data-sayfa" = id,
    class = paste("gecis", if (aktif) "aktif", if (alt) "alt"),
    "aria-current" = if (aktif) "page" else NULL,
    if (!is.null(ikon)) shiny::icon(ikon), tags$span(metin)
  )
}

ui <- page_sidebar(
  title = "Üretim Planlama | Karar Destek",
  theme = bs_theme(version = 5, bg = "#f6f7f9", fg = "#253248",
                   primary = "#426b65", base_font = "system-ui"),
  fillable = FALSE,
  sidebar = sidebar(
    title = "Çalışma alanı", width = 260, open = "desktop",
    uiOutput("menu")
  ),
  tags$head(
    tags$style(HTML("
      .gecis {display:flex;align-items:center;gap:12px;width:100%;
        border:0;border-radius:9px;padding:12px;margin:4px 0;
        background:transparent;color:#253248;text-align:left;font-weight:650}
      .gecis:hover {background:#e9efed}
      .gecis.aktif {background:#dcebe6;color:#24594c}
      .gecis.alt {padding-left:32px;font-weight:400}
      .gecis:focus-visible {outline:3px solid #426b65}
      .kategori {display:flex;gap:12px;font-weight:750;
        padding:18px 12px 6px;color:#48576a}
      .durum {padding:12px 18px;margin-bottom:24px;border:1px solid #dde4e9;
        border-radius:12px;background:white;color:#566477}
      .card {margin-bottom:20px}
      .bos-alan {padding:30px 8px;color:#687587}
      h2 {margin-bottom:20px}
    ")),
    tags$script(HTML("
      document.addEventListener('click', function(e) {
        var b = e.target.closest('button[data-sayfa]');
        if (b && window.Shiny) {
          Shiny.setInputValue('sayfa_sec', b.dataset.sayfa, {priority:'event'});
        }
      });
    "))
  ),
  div(class = "durum", textOutput("durum")),
  # Gizli sekmeler tüm modüllerin girişlerini sayfa değişiminde korur.
  do.call(tabsetPanel, c(
    list(id = "sayfalar", type = "hidden", selected = "ana"),
    lapply(moduller, function(m) {
      tabPanel(title = m$baslik, value = m$id, m$ui(m$id))
    })
  ))
)

server <- function(input, output, session) {
  aktif <- reactiveVal("ana")
  ortak_veri <- reactiveVal(NULL)

  observeEvent(input$sayfa_sec, {
    req(input$sayfa_sec %in% ids)
    aktif(input$sayfa_sec)
    updateTabsetPanel(session, "sayfalar", selected = input$sayfa_sec)
  })

  output$menu <- renderUI({
    sonuc <- list()
    kategoriler <- character()
    for (m in moduller) {
      alt <- !is.na(m$kategori) && nzchar(m$kategori)
      if (alt && !m$kategori %in% kategoriler) {
        sonuc <- append(sonuc, list(
          div(class = "kategori", icon("layer-group"), m$kategori)
        ))
        kategoriler <- c(kategoriler, m$kategori)
      }
      sonuc <- append(sonuc, list(
        gecis(m$id, m$baslik, m$ikon, aktif() == m$id, alt)
      ))
    }
    tagList(sonuc)
  })

  output$durum <- renderText({
    v <- ortak_veri()
    if (is.null(v)) return("Henüz veri yüklenmedi")
    paste0("Yüklü veri: ", v$dosya, " · ", nrow(v$ham),
           " dönem / satır · ",
           if (v$hazir) "Sütunlar seçildi" else "Sütun seçimini kontrol edin")
  })

  # Her modüle aynı reactive veri kaynağı gönderilir.
  for (m in moduller) {
    sonuc <- m$server(m$id, ortak_veri)
    if (m$id == "veri") {
      local({
        kaynak <- sonuc
        observe({ ortak_veri(kaynak()) })
      })
    }
  }

  outputOptions(output, "durum", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
