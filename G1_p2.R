#G1_Katarzyna_Blitek_
# p-value mniej niz 5 % - odrzucamy H0
# wicej ni¿ 5% - nie ma podstaw do odrzucenia hipotezy zerowej


# SREDNIE
srednie <-as.data.frame(apply(Dane, 2, mean))
View(srednie)

# WARIANCJE
var(Dane)
wariancje<-as.data.frame(apply(Dane, 2, var))
View(wariancje)

#ODCHYLENIA STANDARDOWE
odchylenieStd<-as.data.frame(sqrt(wariancje))
View(odchylenieStd)

#podsumowanie danych
summary(Dane)

# WYKRESY PUDELKOWE DLA KAZDEJ KATEGORII
# ogolem
boxplot(Dane[,1], main="Wydatki w Polsce")

# towary i uslugi konsumpcyjne
boxplot(Dane[,2], main="Towary i uslugi konsumpcyjne")

#zywnosc i napoje bezalkoholowe
boxplot(Dane[,3], main="Zywnosc i napoje bezalkoholowe")

# napoje alkoholowe i wyroby tytoniowe
boxplot(Dane[,4], main="Napoje alkoholowe i wyroby tytoniowe")

# odziez i obuwie
boxplot(Dane[,5], main="Odziez i obuwie")

#uzytkowanie mieszkania lub domu i nosniki energii
boxplot(Dane[,6], main="Uzytkowanie mieszkania lub domu i nosniki energii")

#wyposazenie mieszkania i prowadzenie gospodarstwa domowego
boxplot(Dane[,7], main="Wyposazenie i prowadzenie domu")

# zdrowie
boxplot(Dane[,8], main="Zdrowie")

# transport
boxplot(Dane[,9], main="Transport")

# lacznosc
boxplot(Dane[,10], main="Lacznosc")

# rekreacja i kultura
boxplot(Dane[,11], main="Rekreacja i Kultura")

#edukacja
boxplot(Dane[,12], main="Edukacja")

# restauracje i hotele
boxplot(Dane[,13], main="Restauracje i Hotele")

# pozostale wydatki na towary i uslugi
boxplot(Dane[,14], main="Pozostale wydatki na towary i uslugi")


#TEST NORMALNOSCI ROZKLADOW Shapiro-Wilka w poszczegolnych podgrupach
# Hipoteza zerowa (H0) - podgrupa ma rozkad normalny
# Hipoteza alternatywna (H1) - podgrupa nie ma rozkadu normalnego

# ogolem
shapiro.test(Dane$ogolem)
# Nie ma podstaw do odrzucenia H0 - Podgrupa ma rozklad normalny

# towary i uslugi konsumpcyjne
shapiro.test(Dane$towary.i.uslugi.konsumpcyjne)
# Nie ma podstaw do odrzucenia H0 - Podgrupa ma rozklad normalny

# zywnosc i napoje bezalkoholowe
shapiro.test(Dane$zywnosc.i.napoje.bezalkoholowe)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# napoje alkoholowe i wyroby tytoniowe
shapiro.test(Dane$napoje.alkoholowe.i.wyroby.tytoniowe)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# odziez i obuwie
shapiro.test(Dane$odziez.i.obuwie)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

#uzytkowanie mieszkania lub domu i nosniki energii
shapiro.test(Dane$uzytkowanie.mieszkania.lub.domu.i.nosniki.energii)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

#wyposazenie mieszkania i prowadzenie gospodarstwa domowego
shapiro.test(Dane$wyposazenie.mieszkania.i.prowadzenie.gospodarstwa.domowego)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# zdrowie
shapiro.test(Dane$zdrowie)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# transport
shapiro.test(Dane$transport)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# lacznosc
shapiro.test(Dane$lacznosc)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# rekreacja i kultura
shapiro.test(Dane$rekreacja.i.kultura)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

#edukacja
shapiro.test(Dane$edukacja)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# restauracje i hotele
shapiro.test(Dane$restauracje.i.hotele)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

# pozostale wydatki na towary i uslugi
shapiro.test(Dane$pozostale.wydatki.na.towary.i.uslugi)
# Nie ma podstaw do odrzucenia H0 - Proba ma rozklad normalny

#PODGRUPY MAJA ROZKLAD NORMALNY

#zalozenie testow istotnosci- rozklad normalny - SPELNIONE




#HIPOTEZA PIERWSZA - Wariancje dla towarow i uslug konsumpcyjnych oraz zywnosci i napojow bezalkoholowych s¹ sobie równe.

#TEST ISTOTNOSCI DWOCH WARIANCJI
# Hipoteza zerowa (H0) - wariancje w obu grupach sa rowne
# Hipoteza alternatywna (H1) - wariancje w obu grupach sa istotnie rozne

testDwochWariancji<-var.test(Dane$towary.i.uslugi.konsumpcyjne,Dane$zywnosc.i.napoje.bezalkoholowe)
testDwochWariancji
#WNIOSEK - Nie ma podstaw do odrzucenia H0
#Mo¿na stwierdziæ, ¿e wariancje w obu grupach s¹ sobie równe



#HIPOTEZA DRUGA - Przecietne miesiecznie wydatki jednej osoby na kulture i rekreacje sa istotnie mniejsze niz 82 zl

#TEST ISTOTNOŒCI WARTOŒCI OCZEKIWANEJ
# H0 - Przecietny Polak wydaje miesiecznie 82 zl na kulture i rekreacje
# H1 - Przecietny Polak wydaje istotnie mniej niz 82 zl na kulture i rekreacje
testSrednieja<-t.test(Dane$rekreacja.i.kultura, mu=82, conf.level = 0.95, alternative = 'less')
testSrednieja
#WNIOSEK - Odrzucamy H0 na rzecz hipotezy alternatywnej H1
#Mozna stwierdzic, ze przecietny Polak wydaje istotnie mniej niz 82 zl na kulture i rekreacje



# HIPOTEZA TRZECIA - Przecietny Polak wydaje miesiecznie istotnie wiecej niz 117 zl na rachunki zwiazane z utrzymaniem mieszkania i energia

#TEST ISTOTNOŒCI WARTOŒCI OCZEKIWANEJ
# H0 - Przecietny Polak wydaje miesiecznie 117 zl na rachunki zwiazane z utrzymaniem mieszkania i energia
# H1 - Przecietny Polak wydaje istotnie wiecej niz 117 zl na rachunki zwiazane z utrzymaniem mieszkania i energia
testSredniejb<-t.test(Dane$uzytkowanie.mieszkania.lub.domu.i.nosniki.energii, mu=117, conf.level = 0.95, alternative = 'greater')
testSredniejb
#WNIOSEK - Odrzucamy H0 na rzecz hipotezy alternatywnej H1
#Mozna stwierdzic, ze przecietny Polak wydaje istotnie wiecej niz 117 zl na rachunki zwiazane z utrzymaniem mieszkania i energia



# HIPOTEZA CZWARTA - Srednie miesieczne wydatki jednej osoby na transport i lacznosc sa takie same.

#TEST ISTOTNOSCI DWOCH WARTOSCI OCZEKIWANYCH
# HO - Srednie miesieczne wydatki na te kategorie dobr i uslug sa sobie rowne
# H1 - Srednie miesieczne wydatki na te kategorie dobr i uslug istotnie sie roznia
testDwochSrednich<-t.test(Dane$transport,Dane$lacznosc,var.equal = F)
testDwochSrednich
#WNIOSEK - Nie ma podstaw do odrzucenia H0
#Mozna stwierdzic, ze srednie miesieczne wydatki na te kategorie dobr i uslug sa rowne



library(EnvStats)
#HIPOTEZA 5 - Wariancja dla wydatków miesiêcznych jednej osoby w kategorii „zdrowie” jest równa 24.
#TEST ISTOTNOCI WARIANCJI
testVariancji<-varTest(Dane$zdrowie,sigma.squared = 24,conf.level = 0.95)
testVariancji
# H0 - Wariancja dla kategorii zdrowie jest rowna 24
# H1 - Wariancja dla kategorii zdrowie jest rozna niz 24
#WNIOSEK - Odrzucamy H0 na rzecz hipotezy alternatywnej H1
#Mozna stwierdzic, ze wariancja dla kategorii zdrowie jest rozna niz 24



# do HIPOTEZY 6  - ANOVA: 
#Grupy brane pod uwage: towary i uslugi konsumpcyjne, odziez i obuwie, rekreacja i kultura 
# H0 - Srednie w grupach sa rowne (nieistotne roznice)
# H1 - Srednie w grupach sa rozne (istotne roznice)


y <- c(Dane$towary.i.uslugi.konsumpcyjne, Dane$odziez.i.obuwie, Dane$rekreacja.i.kultura)

group <- as.factor(c(rep(1, length(Dane$towary.i.uslugi.konsumpcyjne)), rep(2, length(Dane$odziez.i.obuwie)), rep(3, length(Dane$rekreacja.i.kultura))   ))


#TESTY WERYFIKUJ¥CE JEDNORODNOŒÆ WARIANCJI W WIÊCEJ NI¯ 2 GRUPACH 
#test Levene'a
library(car)
leveneTest(y, group)
# H0 - wariancje s¹ równe, 
# H1 - przynajmniej jedna jest istotnie ró¿na
#WNIOSEK - Odrzucamy H0 na rzecz hipotezy alternatywnej H1 

#NIESPELNIONE ZALOZENIE DOTYCZACE ROWNOSCI WARIANCJI W PODGRUPACH
#NIE MOZNA ZASTOSOWAC ANOVY



#KIEDY ZA£O¯ENIA ANOVY NIE S¥ SPE£NIONE STOSUJEMY JEJ NIEPARAMETRYCZNY ODPOWIEDNIK
#test Kruskala-Wallisa
kruskal.test(y, group)
#Hipotezy zatem takie jak w Anovie:
# H0 - Srednie w grupach sa rowne (wystepuja nieistotne roznice)
# H1 - Srednie w grupach sa rozne (wystepuja istotne roznice)
#WNIOSEK - Odrzucamy H0 na rzecz hipotezy alternatywnej
#Miedzy grupami: towary i uslugi konsumpcyjne, odziez i obuwie, rekreacja i kultura wystepuja istotne roznice pomiedzy ich srednimi

