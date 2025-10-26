# ===============================================================
# 📊 Casus 1 – CPI kwartaalmutaties (volautomatische pipeline)
# ===============================================================

# 📦 PACKAGES -----------------------------------------------------
# Installeer ontbrekende pakketten éénmalig
packages <- c("cbsodataR", "dplyr", "lubridate", "stringr", "duckdb", "ggplot2", "DBI")

for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
}

# Laad alle pakketten
invisible(lapply(packages, library, character.only = TRUE))

# ===============================================================
# 💡 Instelbare parameters
# ===============================================================
# Pas hieronder de verslagperiode aan om automatisch:
# - kwartaalmutaties te berekenen,
# - grafieken te genereren,
# - en resultaten op te slaan in de database.
#
# Gebruik het formaat "YYYYMM##" (bijv. "2022MM09" t/m "2024MM03")

start_periode <- "2021MM07"   # Begin van verslagperiode (aanpasbaar)
eind_periode  <- "2024MM03"   # Eind van verslagperiode (aanpasbaar)

producten <- c("011200 Vlees", "011700 Groenten")  # Selecteer producten. In deze geval "011200 Vlees" en "011700 Groenten"



# ===============================================================
# 1 📥 Data ophalen van CBS (CPI 2015 = 100)
# ===============================================================
cpi_data <- cbs_get_data("83131NED")

# Metadata ophalen (productcategorieën)
meta <- cbs_get_meta("83131NED")

# 🔗 Metadata koppelen
cpi_full <- merge(
  cpi_data,
  meta$Bestedingscategorieen[, c("Key", "Title")],
  by.x = "Bestedingscategorieen",
  by.y = "Key",
  all.x = TRUE
)

# Filter op gekozen producten
df <- cpi_full %>%
  filter(Title %in% producten)

# ===============================================================
# 🧹 Data cleaning
# ===============================================================
df_clean <- df %>%
  select(Title, Perioden, CPI_1) %>%
  mutate(Title = str_squish(Title)) %>%
  filter(grepl("^\\d{4}MM(0[1-9]|1[0-2])$", Perioden)) %>%
  mutate(CPI_1 = suppressWarnings(as.numeric(CPI_1))) %>%
  filter(!is.na(CPI_1)) %>%
  distinct(Title, Perioden, .keep_all = TRUE) %>%
  mutate(
    jaar = as.integer(substr(Perioden, 1, 4)),
    maand = as.integer(substr(Perioden, 7, 8)),
    kwartaal = ((maand - 1) %/% 3) + 1,
    jaar_kwart = paste0(jaar, "Q", kwartaal)
  ) %>%
  arrange(Title, jaar, maand)

# ===============================================================
# ⚙️ FUNCTIE: Kwartaalmutatie berekenen
# ===============================================================

bereken_kwartaalmutatie <- function(df_clean, start_periode, eind_periode) {
  df_kwartaal <- df_clean %>%
    group_by(Title, jaar, kwartaal, jaar_kwart) %>%
    summarise(CPI_kwartaalgem = mean(CPI_1, na.rm = TRUE), .groups = "drop_last") %>%
    arrange(Title, jaar, kwartaal) %>%
    group_by(Title) %>%
    mutate(kwartaalmutatie = (CPI_kwartaalgem - lag(CPI_kwartaalgem)) / lag(CPI_kwartaalgem) * 100) %>%
    ungroup()
  
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
  
  idx <- function(y, q) y * 10 + q
  
  if (!is.null(start_periode) && !is.null(eind_periode)) {
    s <- parse_to_yq(start_periode)
    e <- parse_to_yq(eind_periode)
    s_idx <- idx(s[1], s[2])
    e_idx <- idx(e[1], e[2])
    if (s_idx > e_idx) {
      warning("⚠️ Startperiode ligt na eindperiode — verwisseld.")
      tmp <- s_idx; s_idx <- e_idx; e_idx <- tmp
    }
    df_kwartaal <- df_kwartaal %>%
      filter(idx(jaar, kwartaal) >= s_idx & idx(jaar, kwartaal) <= e_idx)
  }
  
  return(df_kwartaal)
}

# ===============================================================
# 🔄 Functie uitvoeren
# ===============================================================
resultaat <- bereken_kwartaalmutatie(df_clean, start_periode, eind_periode)

# ===============================================================
# 📊 Grafieken genereren
# ===============================================================
lijn_bestand  <- paste0("output/grafiek_lijn_kwartaalmutatie_", start_periode, "_", eind_periode, ".png")
staaf_bestand <- paste0("output/grafiek_staaf_CPI_per_kwartaal_", start_periode, "_", eind_periode, ".png")
db_bestand    <- paste0("output/CPI_resultaten_", start_periode, "_", eind_periode, ".db")

# Lijngrafiek
grafiek_lijn <- ggplot(resultaat, aes(x = jaar_kwart, y = kwartaalmutatie, color = Title, group = Title)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = paste0("Kwartaalmutatie van de CPI per product (", start_periode, " – ", eind_periode, ")"),
       x = "Verslagperiode (Jaar-Kwartaal)", y = "Kwartaalmutatie (%)", color = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(lijn_bestand, plot = grafiek_lijn, width = 8, height = 5, dpi = 300)

# Staafgrafiek
grafiek_staaf <- ggplot(resultaat, aes(x = jaar_kwart, y = CPI_kwartaalgem, fill = Title)) +
  geom_col(position = "dodge") +
  labs(title = paste0("Gemiddelde CPI per kwartaal (", start_periode, " – ", eind_periode, ")"),
       x = "Verslagperiode (Jaar-Kwartaal)", y = "CPI (2015 = 100)", fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(staaf_bestand, plot = grafiek_staaf, width = 8, height = 5, dpi = 300)

# ===============================================================
# 💾 Opslaan in lokale DuckDB database
# ===============================================================
con <- dbConnect(duckdb::duckdb(), db_bestand)
dbWriteTable(con, "kwartaalmutaties", resultaat, overwrite = TRUE)
dbDisconnect(con)

cat("\n✅ Pipeline succesvol uitgevoerd!\n",
    "Lijngrafiek opgeslagen als:", lijn_bestand, "\n",
    "Staafgrafiek opgeslagen als:", staaf_bestand, "\n",
    "Database opgeslagen als:", db_bestand, "\n")
