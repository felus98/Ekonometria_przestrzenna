# 1. Instalacja i wczytanie pakietów
install.packages(c("readxl", "dplyr", "ggplot2", "psych", "moments", "corrplot", "sf", "spdep", "tmap"))
library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
library(moments)
library(corrplot)
library(sf)
library(spdep)
library(tmap)
library(sf)
library(spdep)
library(spatialreg)
library(dplyr)

# 2. Wczytanie i przygotowanie danych
dane <- read_excel("Data.xlsx")
names(dane) <- trimws(names(dane))
dane <- dane %>%
  select(ID, Rok, Wojewodztwo, `liczba mieszkan`, wynagrodzenia, `liczba bezrobotnych`) %>%
  rename(
    migracje = `liczba mieszkan`,      # zakładam że migracje to oddane mieszkania
    mieszkania = `liczba mieszkan`,
    bezrobocie = `liczba bezrobotnych`
  )

# 3. Identyfikacja braków danych
sapply(dane, function(x) sum(is.na(x)))

# 4. Wykrywanie obserwacji odstających (3×SD)
outliers <- dane %>% 
  summarise(
    mieszkania = sum(abs(mieszkania - mean(mieszkania)) > 3*sd(mieszkania)),
    wynagrodzenia = sum(abs(wynagrodzenia - mean(wynagrodzenia)) > 3*sd(wynagrodzenia)),
    bezrobocie = sum(abs(bezrobocie - mean(bezrobocie)) > 3*sd(bezrobocie))
  )

# 5. Statystyki opisowe
desc <- describe(dane[, c("mieszkania","wynagrodzenia","bezrobocie")])
desc$skewness <- sapply(dane[, c("mieszkania","wynagrodzenia","bezrobocie")], skewness)

# 6. Analiza korelacji
cor_mat <- cor(dane[, c("mieszkania","wynagrodzenia","bezrobocie")])
corrplot(cor_mat, method="ellipse", type="lower", tl.col="black", tl.srt=45)

# 7. Wykresy histogramów z nakładką gęstości
vars <- c("mieszkania","wynagrodzenia","bezrobocie")
for(v in vars) {
  p <- ggplot(dane, aes_string(x=v)) +
    geom_histogram(aes(y=..density..), bins=30, color="black", fill="lightblue") +
    geom_density(color="red", size=1) +
    labs(title=paste("Histogram i gęstość:", v), x=v, y="Gęstość")
  print(p)
}

# 8. Wykresy rozrzutu i gęstości par zmiennych
pairs(dane[, vars], main="Wykresy rozrzutu zmiennych")

map <- st_read("wojewodztwa.shp") # .shp file with voivodships boundaries
colnames(map)  # Checking what headers a map file has

nb <- poly2nb(map)

lw <- nb2listw(nb, style = "W", zero.policy=TRUE)

colnames(map)    # sprawdzenie nazw kolumn w pliku .shp 
print(map$JPT_KOD_JE)
all.equal(map$JPT_KOD_JE, dane$ID)