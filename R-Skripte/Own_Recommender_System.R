# Neuer Ansatz: Eigenes Recommender-System basierend auf den Vorschlägen vom 16.05.2021

# Autoren-Similarity basierend auf den Main-Topics berechnen.
# Ziel: Eine Matrix mit den Autoren in den Zeilen und den Main-Topics in den Spalten
# In den Zellen stehen dann die Anzahl der Bücher des jeweiligen Autoren im jeweiligen Genre
# Damit die Matrix nicht ausufert müssen wir vorher die Main-Topics "zusammenstampfen".
# Grund: Somit können wir die Autoren gruppieren und bspw. die Roman-Autoren von Kochbuch-Autoren trennen.
# Somit schränken wir auch sofort die Büchersuche auf diese Bereiche ein und sparen uns so Rechenzeit,
# die verloren gehen würde, wenn wir in "sinnbefreiten" Genre suchen würden.

# Packages laden

library(tidyverse)

# Datensatz laden

# Da wir ja bereits festegestellt haben, dass diverse Autoren häufiger unter einem anderen Namen vorkommen,
# verwenden wir die von Eric mittels Open Refine vorbehandelte Matrix. In dieser sind bestenfalls auch schon
# die Main Topics auf die ersten zwei, drei Buchstaben zusammengestampft.
items <- read.csv("./Data/items_bearbeitet4.csv")

# Matrix aufbauen, mit den Dimensionen entsprechend der Anzahl der Autoren und Genre
AuthMat <- items %>% 
  group_by(author, main.topic) %>% 
  dplyr::summarise(n = n()) %>% 
  spread(key = main.topic, value = n)

apply(AuthMat,2,class)

# komischerweise werden die Spalten als character dargestellt, deswegen müssen die Variablen
# in numerische Werte transformiert werden, damit die Korrelationen zwischen den Autoren berechnet
# werden können
AuthMat <- apply(AuthMat[2:ncol(AuthMat)],2,as.numeric)

# Überprüfung, ob Transformation funktioniert hat
apply(AuthMat,2,class)

AuthorSim <- cor(t(AuthMat), use = "pairwise.complete.obs", method = "pearson")
