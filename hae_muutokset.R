# Kohdelakien haku: affectedDocument jokaiselle muutos- ja kumoamissäädökselle
# Tämä mahdollistaa elinikä- ja muutostiheysanalyysin
#
# HUOM: tämä hakee ~35 000 XML-dokumenttia. Kestää tunteja.
# Skripti tallentaa välituloksia ja jatkaa keskeytyksestä.

library(httr)
library(xml2)
library(jsonlite)
library(dplyr)
library(fst)

UA        <- "FinlexAnalyysi/1.0 (kristian.vepsalainen@proton.me)"
VALI_FILE <- "data/affected_valitulos.rds"
LOPPU_FST <- "data/finlex_affected.fst"

# Lataa pääaineisto — käsitellään vain Amendment ja Repeal
df <- read_fst("data/finlex_saadokset.fst")
kohteet <- df |> filter(statute_type %in% c("Amendment", "Repeal"))
cat("Käsiteltäviä säädöksiä:", nrow(kohteet), "\n")

# Jatka keskeytyksestä jos välitulos on olemassa
if (file.exists(VALI_FILE)) {
  tulokset <- readRDS(VALI_FILE)
  cat("Ladattu välitulos:", length(tulokset), "käsiteltyä\n")
} else {
  tulokset <- list()
}

# ── Apufunktio: hae yhden säädöksen affectedDocument(it) ──────────────────────
hae_affected <- function(uri, max_yritykset = 3) {
  for (yritys in seq_len(max_yritykset)) {
    resp <- tryCatch(
      GET(uri, add_headers(`User-Agent` = UA, `Accept` = "application/xml"),
          timeout(30)),
      error = function(e) NULL
    )
    if (is.null(resp)) { Sys.sleep(5); next }
    if (status_code(resp) == 200) {
      d  <- tryCatch(read_xml(rawToChar(resp$content)), error = function(e) NULL)
      if (is.null(d)) return(NA_character_)
      ns <- xml_ns(d)
      af <- xml_find_all(d, "//d1:affectedDocument", ns)
      if (length(af) == 0) return(NA_character_)
      # Palauta kaikki hrefit (yleensä 1, mutta kokoomalaeissa useita)
      return(xml_attr(af, "href"))
    } else if (status_code(resp) == 429) {
      Sys.sleep(30)
    } else {
      return(NA_character_)
    }
  }
  NA_character_
}

# ── Pääsilmukka ───────────────────────────────────────────────────────────────
for (i in seq_len(nrow(kohteet))) {
  uri <- kohteet$uri[i]

  # Ohita jo käsitellyt
  if (!is.null(tulokset[[uri]])) next

  hrefit <- hae_affected(uri)
  tulokset[[uri]] <- hrefit

  # Tallenna välitulos 500 säädöksen välein
  if (i %% 500 == 0) {
    saveRDS(tulokset, VALI_FILE)
    cat(sprintf("  %d / %d käsitelty (%.1f %%)\n",
                i, nrow(kohteet), i / nrow(kohteet) * 100))
  }
  Sys.sleep(0.2)
}
saveRDS(tulokset, VALI_FILE)

# ── Muunna pitkäksi dataframeksi ──────────────────────────────────────────────
# Yksi rivi per (lähde, kohde) -pari
rivit <- list()
k <- 1
for (uri in names(tulokset)) {
  hrefit <- tulokset[[uri]]
  if (length(hrefit) == 1 && is.na(hrefit)) next
  for (h in hrefit) {
    # Poimi kohde-URI:sta vuosi ja numero: /akn/fi/act/statute/2015/1352
    kohde_vuosi  <- as.integer(sub(".*/statute/([0-9]{4})/([0-9]+).*", "\\1", h))
    kohde_numero <- as.integer(sub(".*/statute/([0-9]{4})/([0-9]+).*", "\\2", h))
    rivit[[k]] <- data.frame(
      lahde_uri    = uri,
      kohde_href   = h,
      kohde_vuosi  = kohde_vuosi,
      kohde_numero = kohde_numero,
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }
}
affected <- bind_rows(rivit)

# Liitä lähdesäädöksen tiedot
affected <- affected |>
  left_join(
    df |> select(lahde_uri = uri, lahde_vuosi = vuosi,
                 lahde_statute_type = statute_type),
    by = "lahde_uri"
  )

cat("\nValmis. Rivejä:", nrow(affected), "\n")
cat("Uniikkeja kohdelakeja:", n_distinct(affected$kohde_href), "\n")

write_fst(affected, LOPPU_FST)
cat("Tallennettu:", LOPPU_FST, "\n")

# Pikakatsaus: eniten muutetut lait
cat("\n=== 10 eniten muutettua/kumottua lakia ===\n")
affected |>
  count(kohde_numero, kohde_vuosi, sort = TRUE) |>
  head(10) |>
  print()