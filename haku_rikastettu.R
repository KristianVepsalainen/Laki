# Rikastettu metadatan haku: jokaisesta säädöksestä dateIssued, docTitle,
# pykälämäärä. Näistä johdetaan kuukausi, säädöstyyppi ja pituus.
#
# HUOM: hakee ~56 000 XML-dokumenttia. Aja yöksi.
# Keskeytyksenkestävä: tallentaa välituloksen ja jatkaa siitä.

library(httr)
library(xml2)
library(dplyr)
library(fst)

# Null-coalescing operaattori (varmuuden vuoksi, jos rlang ei lataudu)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

UA        <- "FinlexAnalyysi/1.0 (kristian.vepsalainen@proton.me)"
VALI_FILE <- "data/rikastettu_valitulos.rds"
LOPPU_FST <- "data/finlex_rikastettu.fst"

df <- read_fst("data/finlex_saadokset.fst")
cat("Käsiteltäviä säädöksiä:", nrow(df), "\n")

# Jatka keskeytyksestä
if (file.exists(VALI_FILE)) {
  tulokset <- readRDS(VALI_FILE)
  cat("Ladattu välitulos:", length(tulokset), "käsiteltyä\n")
} else {
  tulokset <- list()
}

# ── Poimi yhden säädöksen metadata ────────────────────────────────────────────
poimi_meta <- function(uri, max_yritykset = 3) {
  for (yritys in seq_len(max_yritykset)) {
    resp <- tryCatch(
      GET(uri, add_headers(`User-Agent` = UA, `Accept` = "application/xml"),
          timeout(30)),
      error = function(e) NULL
    )
    if (is.null(resp)) { Sys.sleep(5); next }
    
    code <- status_code(resp)
    if (code == 200) {
      d <- tryCatch(read_xml(rawToChar(resp$content)), error = function(e) NULL)
      if (is.null(d)) return(list(date_issued = NA, otsikko = NA, n_pykala = NA))
      ns <- xml_ns(d)
      
      date_issued <- xml_attr(
        xml_find_first(d, "//d1:FRBRWork/d1:FRBRdate[@name='dateIssued']", ns), "date")
      otsikko <- xml_text(xml_find_first(d, "//d1:docTitle", ns))
      # Pykälämäärä: section-elementtien lukumäärä
      n_pykala <- length(xml_find_all(d, "//d1:section", ns))
      
      return(list(
        date_issued = date_issued,
        otsikko     = if (length(otsikko) == 0) NA else otsikko,
        n_pykala    = n_pykala
      ))
    } else if (code == 429) {
      Sys.sleep(30)
    } else {
      return(list(date_issued = NA, otsikko = NA, n_pykala = NA))
    }
  }
  list(date_issued = NA, otsikko = NA, n_pykala = NA)
}

# ── Pääsilmukka ───────────────────────────────────────────────────────────────
for (i in seq_len(nrow(df))) {
  uri <- df$uri[i]
  if (!is.null(tulokset[[uri]])) next
  
  tulokset[[uri]] <- poimi_meta(uri)
  
  if (i %% 500 == 0) {
    saveRDS(tulokset, VALI_FILE)
    cat(sprintf("  %d / %d (%.1f %%)\n", i, nrow(df), i / nrow(df) * 100))
  }
  Sys.sleep(0.2)
}
saveRDS(tulokset, VALI_FILE)

# ── Muunna dataframeksi ───────────────────────────────────────────────────────
rikastettu <- df |>
  mutate(
    date_issued = sapply(uri, function(u) tulokset[[u]]$date_issued %||% NA),
    otsikko     = sapply(uri, function(u) tulokset[[u]]$otsikko     %||% NA),
    n_pykala    = sapply(uri, function(u) {
      v <- tulokset[[u]]$n_pykala; if (is.null(v)) NA_integer_ else as.integer(v)
    })
  )

# Johdetut muuttujat
rikastettu <- rikastettu |>
  mutate(
    pvm        = as.Date(date_issued),
    kuukausi   = as.integer(format(pvm, "%m")),
    viikonpaiva = weekdays(pvm),
    # Säädöstyyppi otsikon perusteella. Kattaa sekä ennen 2000 (päätös-muodot)
    # että jälkeen 2000 (asetus-muodot) sekä yhdyssanamuodot (esim. -laki, -asetus).
    saados_luokka = case_when(
      grepl("^Laki\\b",                          otsikko)            ~ "Laki",
      grepl("laki$",                             otsikko, ignore.case = TRUE) ~ "Laki",
      grepl("^Valtioneuvoston asetus",           otsikko)            ~ "Valtioneuvoston asetus",
      grepl("^Tasavallan presidentin asetus",    otsikko)            ~ "Tasavallan presidentin asetus",
      grepl("ministeriön asetus",                otsikko)            ~ "Ministeriön asetus",
      grepl("^Asetus\\b",                        otsikko)            ~ "Asetus",
      grepl("asetus$",                           otsikko, ignore.case = TRUE) ~ "Asetus",
      grepl("^Valtioneuvoston päätös",           otsikko)            ~ "Valtioneuvoston päätös",
      grepl("ministeriön päätös",                otsikko)            ~ "Ministeriön päätös",
      grepl("hallituksen päätös|hallituksen ohjesääntö|hallituksen ilmoitus",
            otsikko)            ~ "Viranomaispäätös",
      grepl("päätös$|päätös ",                   otsikko, ignore.case = TRUE) ~ "Muu päätös",
      TRUE                                                           ~ "Muu"
    )
  )

cat("\nValmis. Rivejä:", nrow(rikastettu), "\n")
cat("date_issued puuttuu:", sum(is.na(rikastettu$pvm)), "\n")
cat("otsikko puuttuu:", sum(is.na(rikastettu$otsikko)), "\n\n")
cat("Säädösluokat:\n"); print(table(rikastettu$saados_luokka))
cat("\nKuukausijakauma:\n"); print(table(rikastettu$kuukausi))

write_fst(rikastettu, LOPPU_FST)
cat("\nTallennettu:", LOPPU_FST, "\n")