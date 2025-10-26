Casus 1 – R

📊 Doel 
In deze casus is een klein, geautomatiseerd proces ontwikkeld in R om de kwartaalmutatie van de consumentenprijsindex (CPI) te berekenen.  
De data wordt direct opgehaald uit CBS StatLine (open data API),zonder handmatige downloads of externe bestanden.

Het doel is om te laten zien hoe je een reproduceerbare dataverwerkingsstraat kan opzetten met duidelijke stappen:  
Input → Throughput → Output.


⚙️ Tooling
| Onderdeel | Beschrijving |
|------------|---------------|
| **R** | Hoofdtaal voor data-analyse en automatisering |
| **RStudio** | Ontwikkelomgeving voor R |
| **cbsodataR** | Ophalen van open data van CBS StatLine via de API |
| **dplyr** | Datamanipulatie en berekeningen |
| **ggplot2** | Visualisatie van kwartaalmutaties |
| **DuckDB** | Lokale database voor opslag van resultaten |
| **GitHub** | Versiebeheer en publicatie van code |


🧠 Procesoverzicht

1 Input
- Data wordt automatisch opgehaald van CBS StatLine  via de OData API:  
  Tabel 83131NED – Consumentenprijsindex (2015 = 100)  
- Metadata (zoals productcategorieën) wordt opgehaald met `cbs_get_meta()`.

2 Throughput
- De data wordt gecombineerd met productnamen.  
- Data wordt opgeschoond(Data cleaning)
- De functie (`bereken_kwartaalmutatie()`) berekent per product de kwartaalgemiddelden en kwartaalmutaties.  
- De gebruiker kiest zelf de producten (bijv. Vlees en Groenten) en geeft daarna in de functie de verslagperiode op.

3 Output
- Er worden automatisch twee visualisaties gegenereerd:
📈  Een lijngrafiek met de kwartaalmutatie van de CPI om trends in inflatie per product te tonen.
📊  Een verticale staafgrafiek met de gemiddelde CPI per kwartaal om prijsniveaus tussen producten te vergelijken.  
- De resultaten (CPI kwartaalmutaties) worden opgeslagen in een lokale DuckDB database (data/output/CPI_resultaten.db). 
  De database bevat één tabel: kwartaalmutaties.

4 Reproduceerbaarheid
  De pipeline is reproduceerbaar en parametriseerbaar: andere producten en verslagperioden kunnen eenvoudig opnieuw worden geanalyseerd met dezelfde methode.
  
5 Gebruik
  Open het R-script (Scripts/casus1_pipeline.R) in RStudio.
  Pas bovenin het script de waarden van start_periode en eind_periode aan. 
  Klik op Source ▶️ om automatisch:
        o	de kwartaalmutaties te berekenen,
        o	grafieken te genereren,
        o	en de resultaten op te slaan in de output/ map.


6 Projectstructuur
  De onderstaande mapstructuur laat zien hoe het project is opgebouwd volgens de logica Input → Throughput → Output.
  Hiermee is direct duidelijk waar data, scripts en resultaten zich bevinden.
```
CPI-kwartaalmutaties/
│
├── Scripts/                   # Bevat alle R-scripts (hoofdscript: casus1_pipeline.R)
│
├── data/                      # (optioneel) opslag van ruwe of tussenbestanden
│
├── output/                    # Automatisch gegenereerde resultaten:
│     ├── grafiek_lijn_kwartaalmutatie_[verslagperiode].png
│     ├── grafiek_staaf_CPI_per_kwartaal_[verslagperiode].png
│     └── CPI_resultaten_[verslagperiode].db
│
├── README.md                  # Beschrijving van project, proces en gebruik
│
└── CPI-kwartaalmutaties.Rproj # RStudio-projectbestand
```


