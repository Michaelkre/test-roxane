# Lese den Titanic-Datensatz ein
titanic <- read.csv("titanic.csv")

# Installation des stringr Packages
install.packages("stringr")

# auflade das stringr Package
library(stringr)

# Mein List von Namen
names <- titanic$Name

#Pattern von Anrede 
pattern <- "\\b[A-Za-z]+\\."

# str_extract Funktion um Anrede zu finden
titles <- str_extract(names, pattern)

# Unique Funktion, um die einzigartigen Anrede
unique_titles <- unique(titles)

# Zeigen Sie einzigartige Anrede
print(unique_titles)

# Ordinale Variables als Factor 

#Survived Yes = 1, No = 0
Survived = factor(titanic$Survived)

#Sex: Geschlecht (male/female)
Sex = factor(titanic$Sex)

#Embarked: Zustiegshafen (C = Cherbourg; Q = Queenstown; S = Southampton)
Embarked = factor (titanic$Embarked)

# Umkehrung der Faktorslevel
# Klasse des Reisenden (ordinal mit 1 > 2 > 3)
Pclass= factor(titanic$Pclass, level = rev(c(1,2,3)) , ordered = TRUE)


# Titel zum Datenframe hinzufügen
titanic$Title <- unlist(titles)

# Berechnung des mean für jeden Titel
average_age_by_title <- aggregate(Age ~ Title, titanic, mean, na.rm = TRUE)

# Ersetzen der fehlenden Alter durch das mean für jeden Titel
titanic$Age <- ifelse(is.na(titanic$Age), 
                      ave(titanic$Age, titanic$Title, FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)), 
                      titanic$Age)


