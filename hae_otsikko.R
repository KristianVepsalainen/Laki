# Finlex-otsikon haku — kestävä versio
# Käsittelee sekä uudet (fin@) että mahdolliset poikkeustapaukset.
#
# Rajapinnan opitut säännöt:
#  - statute/VUOSI/NUMERO/fin@  -> varsinainen AKN-dokumentti (d1-namespace)
#  - statute/VUOSI/NUMERO       -> AknXmlList-listaus; <Results/> tyhjä jos ei löydy
#  - Status 200 + tyhjä Results = säädöstä ei ole rajapinnassa
#  - Hyvin vanhoja säädöksiä (esim. 1800-luku) ei välttämättä ole saatavilla

library(httr)
library(xml2)

UA <- "FinlexAnalyysi/1.0 (kristian.vepsalainen@proton.me)"

# Palauttaa listan: $status ("ok" | "not_found" | "error"), $otsikko, $uri
hae_otsikko <- function(vuosi, numero, max_yritykset = 3) {
  uri <- sprintf(
    "https://opendata.finlex.fi/finlex/avoindata/v1/akn/fi/act/statute/%d/%d/fin@",
    vuosi, numero
  )
  
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
      if (is.null(d)) return(list(status = "error", otsikko = NA_character_, uri = uri))
      
      juuri <- xml_name(d)
      
      # Tyhjä listaus = säädöstä ei ole
      if (juuri == "AknXmlList") {
        return(list(status = "not_found", otsikko = NA_character_, uri = uri))
      }
      
      # Varsinainen AKN-dokumentti: poimi docTitle d1-namespacella
      ns <- xml_ns(d)
      otsikot <- tryCatch(
        xml_text(xml_find_all(d, "//d1:docTitle", ns)),
        error = function(e) character(0)
      )
      otsikot <- trimws(otsikot[nzchar(trimws(otsikot))])
      
      if (length(otsikot) == 0) {
        return(list(status = "ok", otsikko = NA_character_, uri = uri))
      }
      return(list(status = "ok", otsikko = otsikot[1], uri = uri))
      
    } else if (code == 404) {
      return(list(status = "not_found", otsikko = NA_character_, uri = uri))
    } else if (code == 429) {
      Sys.sleep(30)
    } else {
      Sys.sleep(5)
    }
  }
  
  list(status = "error", otsikko = NA_character_, uri = uri)
}

# ── Esimerkki ─────────────────────────────────────────────────────────────────
if (interactive()) {
  testit <- list(
    c(1535, 1992),  # Tuloverolaki
    c(39, 1889),    # Rikoslaki — ei rajapinnassa, palauttaa not_found
    c(1501, 1993)   # Arvonlisäverolaki
  )
  for (t in testit) {
    tulos <- hae_otsikko(t[1], t[2])
    cat(sprintf("%d/%d -> [%s] %s\n",
                t[1], t[2], tulos$status,
                ifelse(is.na(tulos$otsikko), "(ei otsikkoa)", tulos$otsikko)))
    Sys.sleep(0.3)
  }
}