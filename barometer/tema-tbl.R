# Load package
library(data.table)

# Create data.table
folkehelse_dt <- data.table::data.table(
  indikator_nr = 1:34,
  indikator = c(
    "Andel i yrkesaktiv alder (16-66 år)",
    "Andel over 80 år",
    "Personer som bor alene, 45 år +",
    "VGS eller høyere utdanning, 30-39 år",
    "Vedvarende lavinntekt, alle aldre",
    "Inntektsulikhet, P90/P10",
    "Leier bolig, 45 år +",
    "Bor trangt, 0-17 år",
    "Sysselsatte, 20-66 år",
    "Står utenfor, 20-66 år",
    "Sykefravær, legemeldt, 20-66 år",
    "Mottakere av uføretrygd, 20-66 år",
    "Trives på skolen, 10. trinn",
    "Laveste mestringsnivå i lesing, 5. tr.",
    "Laveste mestringsnivå i regning, 5. tr.",
    "Gjennomføring i vdg. opplæring",
    "Høy tilfredshet, Ungdata 2025",
    "Skader, behandlet i sykehus",
    "Drikkevann, hygienisk og stabilt",
    "Luftforurensning, eksponert",
    "Med i fritidsorganisasjon, Ungd. 2025",
    "Valgdeltakelse",
    "Trener sjeldnere enn ukentlig, 17 år",
    "Skjermtid over 4 timer, Ungd. 2025",
    "Røyking, kvinner",
    "Forventet levealder, menn",
    "Forventet levealder, kvinner",
    "Utd.forskjeller i forventet levealder",
    "Fornøyd med helsa, Ungd. 2025",
    "Psykiske sympt./lidelser, 0-74 år",
    "Muskel og skjelett, 0-74 år",
    "Overvekt og fedme, 17 år",
    "Hudkreft, nye tilfeller",
    "Vaksinasjonsdekning, meslinger, 9 år"
  ),
  kommune = c(
    67.6, 3.8, 25.4, 86.1,
    9.0, 3.2, 11.7, 17.7,
    78.0, 17.1, 4.8, 7.5,
    82, 24, 23, 85, 54,
    12.2, 100.0, 81, 67, 82.0,
    26, 35, 1.8, 81.0, 84.4,
    6.0, 68, 180, 309, 18,
    116, 96.9
  ),
  fylke = c(
    65.9, 4.1, 24.0, 83.8,
    8.2, 2.8, 11.6, 13.8,
    78.2, 17.3, 5.1, 9.0,
    83, 28, 26, 84, 54,
    12.1, 91.3, 53, 65, 81.1,
    29, 35, 2.4, 80.9, 84.5,
    5.2, 67, 172, 322, 20,
    114, 97.5
  ),
  norge = c(
    65.9, 4.8, 27.0, 82.1,
    9.7, 2.8, 12.7, 18.3,
    76.9, 18.4, 5.9, 10.0,
    83, 27, 28, 82, 51,
    13.7, 89.1, 45, 61, 80.0,
    29, 41, 1.8, 80.7, 84.2,
    5.2, 66, 180, 315, 21,
    96, 97.1
  ),
  enhet = c(
    rep("prosent", 5),
    "-",
    rep("prosent", 11),
    "per 1000",
    "prosent",
    "prosent",
    "prosent",
    "prosent",
    "prosent",
    "prosent",
    "år",
    "år",
    "år",
    "prosent",
    "per 1000",
    "per 1000",
    "prosent",
    "per 100 000",
    "prosent"
  )
)

# View data
folkehelse_dt

# Export to csv if needed
data.table::fwrite(
  folkehelse_dt,
  file = "folkehelseindikatorer.csv",
  sep = ";"
)

# Legg til tema for hver indikator
folkehelse_dt[, tema := c(
  rep("Befolkning", 4),
  rep("Oppvekst og levekår", 13),
  rep("Miljø, skader og ulykker", 5),
  rep("Helserelatert atferd", 3),
  rep("Helsetilstand", 9)
)]

# Flytt tema-kolonnen foran indikatornavn
data.table::setcolorder(
  folkehelse_dt,
  c("indikator_nr", "tema", "indikator",
    "kommune", "fylke", "norge", "enhet")
)

folkehelse_dt
