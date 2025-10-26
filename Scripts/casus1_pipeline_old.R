
# Casus 1 – CPI kwartaalmutaties 
# ===============================================================

# 📦 Packages
install.packages("cbsodataR")
library(cbsodataR)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
install.packages("stringr")
install.packages("duckdb")

# ===============================================================
# 1 📥 Data ophalen van CBS (CPI 2015 = 100)
# ===============================================================
cpi_data <- cbs_get_data("83131NED")

# Bekijk de eerste paar rijen en kolomnamen
head(cpi_data)
colnames(cpi_data)

# ===============================================================
# 2 Metadata ophalen (productcategorieën)
# ===============================================================
meta <- cbs_get_meta("83131NED")

head(meta$Bestedingscategorieen)
colnames(meta$Bestedingscategorieen)


# ===============================================================
#  🔗 Metadata koppelen
# ===============================================================
cpi_full <- merge(cpi_data,
                  meta$Bestedingscategorieen[, c("Key", "Title")],
                  by.x = "Bestedingscategorieen",
                  by.y = "Key",
                  all.x = TRUE)
#controle:zie kolommen
colnames(cpi_full)
# Controle: bekijk eerste 2 rijen
head(cpi_full[, c("Title", "Perioden", "CPI_1")], 2)
colnames(cpi_full[, c("Title", "Perioden", "CPI_1")])



# ===============================================================
# Selecteer 2 producten:
# ===============================================================
producten <- c("011200 Vlees", "011700 Groenten")  # pas aan als de exacte titels anders zijn
df <- cpi_full %>%
  filter(Title %in% producten)
head(df)
colnames(df)

# 🧹️ DataCleaning
# ===============================================================

library(stringr)

df_clean <- df %>%
  # 1. Relevante kolommen houden
  select(Title, Perioden, CPI_1) %>%
  
  # 2. Titel opschonen (spaties, rare tekens)
  mutate(Title = str_squish(Title)) %>%
  
  # 3. Alleen maandregels (YYYYMM01 t/m YYYYMM12, geen MM00 of Kwartaalregels)
  filter(grepl("^\\d{4}MM(0[1-9]|1[0-2])$", Perioden)) %>%
  
  # 4. CPI omzetten naar numeric en NA’s verwijderen
  mutate(CPI_1 = suppressWarnings(as.numeric(CPI_1))) %>%
  filter(!is.na(CPI_1)) %>%
  
  # 5. Dubbele rijen verwijderen
  distinct(Title, Perioden, .keep_all = TRUE) %>%
  
  # 6. Jaar / maand / kwartaal toevoegen
  mutate(
    jaar   = as.integer(substr(Perioden, 1, 4)),
    maand  = as.integer(substr(Perioden, 7, 8)),
    kwartaal  = ((maand - 1) %/% 3) + 1,
    jaar_kwart = paste0(jaar, "Q", kwartaal)
  ) %>%
  
  # 7. Chronologisch sorteren
  arrange(Title, jaar, maand)


# ===============================================================
#  ⚙️ FUNCTIE: Kwartaalmutatie!!!
# ===============================================================

# 💡 Instelbare parameters
# ===============================================================
# Pas hieronder de verslagperiode aan om automatisch:
# - kwartaalmutaties te berekenen,
# - grafieken te genereren,
# - en resultaten op te slaan in de database.
#
# Gebruik het formaat "YYYYMM##" (bijv. "2022MM09" t/m "2024MM03")

start_periode <- "2022MM09"   # Begin van verslagperiode (aanpasbaar)
eind_periode  <- "2024MM03"   # Eind van verslagperiode (aanpasbaar)

# functie om kwartaalmutaties te berekenen binnen een verslagperiode
# ===============================================================

bereken_kwartaalmutatie <- function(df_clean, start_periode, eind_periode){
  library(dplyr)
  
  # 1️⃣ Bereken kwartaalgemiddelden en mutaties per product
  df_kwartaal <- df_clean %>%
    group_by(Title, jaar, kwartaal, jaar_kwart) %>%
    summarise(CPI_kwartaalgem = mean(CPI_1, na.rm = TRUE), .groups = "drop_last") %>%
    arrange(Title, jaar, kwartaal) %>%
    group_by(Title) %>%
    mutate(
      kwartaalmutatie = (CPI_kwartaalgem - lag(CPI_kwartaalgem)) / lag(CPI_kwartaalgem) * 100
    ) %>%
    ungroup()
  
  # 2️⃣ Hulpfunctie om periode naar jaar/kwartaal om te zetten
  parse_to_yq <- function(p) {
    if (grepl("^\\d{4}Q[1-4]$", p)) {
      c(as.integer(substr(p, 1, 4)), as.integer(substr(p, 6, 6)))
    } else if (grepl("^\\d{4}MM(0[1-9]|1[0-2])$", p)) {
      y <- as.integer(substr(p, 1, 4))
      m <- as.integer(substr(p, 7, 8))
      c(y, ((m - 1) %/% 3) + 1)
    } else {
      stop("⚠️ Ongeldig format. Gebruik 'YYYYQ#' of 'YYYYMM##' (01–12).")
    }
  }
  
  # Hulpfunctie om een index te maken (voor filtering)
  idx <- function(y, q) y * 10 + q
  
  # 3️⃣ Filteren op verslagperiode (indien meegegeven)
  if (!is.null(start_periode) && !is.null(eind_periode)) {
    s <- parse_to_yq(start_periode)
    e <- parse_to_yq(eind_periode)
    s_idx <- idx(s[1], s[2])
    e_idx <- idx(e[1], e[2])
    
    # Controle: als gebruiker omgekeerde volgorde heeft
    if (s_idx > e_idx) {
      warning("⚠️ Startperiode ligt na eindperiode — verwisseld voor correcte volgorde.")
      tmp <- s_idx; s_idx <- e_idx; e_idx <- tmp
    }
    
    df_kwartaal <- df_kwartaal %>%
      filter(idx(jaar, kwartaal) >= s_idx & idx(jaar, kwartaal) <= e_idx)
  }
  
  # 4️⃣ Resultaat teruggeven
  return(df_kwartaal)
}

# =============================================================

# Functie aanroepen met gekozen verslagperiode
resultaat <- bereken_kwartaalmutatie(df_clean, start_periode, eind_periode)

# Bekijk de eerste regels van de resultaten
head(resultaat)

# ===============================================================
# 📊 Grafieken genereren op basis van gekozen verslagperiode
# ===============================================================

library(ggplot2)

# Maak dynamische bestandsnamen
lijn_bestand  <- paste0("output/grafiek_lijn_kwartaalmutatie_", start_periode, "_", eind_periode, ".png")
staaf_bestand <- paste0("output/grafiek_staaf_CPI_per_kwartaal_", start_periode, "_", eind_periode, ".png")
db_bestand    <- paste0("output/CPI_resultaten_", start_periode, "_", eind_periode, ".db")

# 1️⃣ Lijngrafiek: kwartaalmutatie per product
grafiek_lijn <- ggplot(resultaat, aes(x = jaar_kwart, y = kwartaalmutatie, color = Title, group = Title)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = paste0("Kwartaalmutatie van de CPI per product (", start_periode, " – ", eind_periode, ")"),
       x = "Verslagperiode (Jaar-Kwartaal)",
       y = "Kwartaalmutatie (%)",
       color = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafiek_lijn

# Opslaan in output-map
ggsave(lijn_bestand, plot = grafiek_lijn, width = 8, height = 5, dpi = 300)


# 2️⃣ Staafgrafiek: CPI-niveau per product
grafiek_staaf <- ggplot(resultaat, aes(x = jaar_kwart, y = CPI_kwartaalgem, fill = Title)) +
  geom_col(position = "dodge") +
  labs(title = paste0("Gemiddelde CPI per kwartaal (", start_periode, " – ", eind_periode, ")"),
       x = "Verslagperiode (Jaar-Kwartaal)",
       y = "CPI (2015 = 100)",
       fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafiek_staaf

# Opslaan in output-map
ggsave(staaf_bestand, plot = grafiek_staaf, width = 8, height = 5, dpi = 300)


# ===============================================================
# 💾 Opslaan in lokale database (DuckDB)
# ===============================================================

library(DBI)
library(duckdb)

# Verbind met DuckDB database in de output-map
con <- dbConnect(duckdb::duckdb(), db_bestand)

# Sla de resultaten-tabel op (wordt overschreven bij dezelfde periode)
dbWriteTable(con, "kwartaalmutaties", resultaat, overwrite = TRUE)

# Controle: toon tabellen en eerste rijen
dbListTables(con)
dbReadTable(con, "kwartaalmutaties") |> head()

# Sluit de verbinding
dbDisconnect(con)

cat("\n✅ Pipeline succesvol uitgevoerd!\n",
    "Lijngrafiek opgeslagen als:", lijn_bestand, "\n",
    "Staafgrafiek opgeslagen als:", staaf_bestand, "\n",
    "Database opgeslagen als:", db_bestand, "\n")




