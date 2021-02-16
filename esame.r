# COMINI VILLANUEVA
# ESAME R

# install.packages("tidyverse")
# library(tidyverse)

'1.Caricare il datasetheart.csve analizzarne dettagliatamente la struttura. 
2.Trasformare i dati in modo che siano tecnicamente corretti. 
3.Rinominare le colonne in maniera appropriata e descrivere il tipodi ogni attributo 
(nominale, ordinale, di intervallo o di rapporto). 
4.Rinominare i livellidei fattori in maniera appropriata, se necessario. 
5.Descrivere brevemente gli attributi.
1.1.Controllare se sono presenC valori NAe, nel caso, rimuoverli. 
2.1.Rimuovere le colonne ritenute non necessarie.'

# es 1
# Caricare il dataset heart.csv e analizzarne dettagliatamente la struttura.


dataset <- read.csv("heart.csv", header = TRUE, stringsAsFactors = FALSE)
str (dataset)
View(dataset)


# es 1.1
# Controllare se sono presenC valori NA e, nel caso, rimuoverli.
# Questo ciclo if fatto sopra mi stampa a video se ci sono o meno dei valori NULL nel dataset


controllo_isn <- function(dataset)
{
  count_isn <- sum(is.na(dataset))
  if (count_isn > 0 )
  {
    print("nel database sono presenti i valori NULL")
    count_isn
  }
  else
  {
    print("nel dataset non sono presenti valori NULL!")
  }
}

controllo_isn(dataset) 
dataset <- na.omit(dataset) 


# es 2.1 
# Rimuovere le colonne ritenute non necessarie.
# Elimino la colonna X perch� da me ritenuta inutile

dataset <- subset(dataset, select = - x)




# es 3
# Rinominare le colonne in maniera appropriata e descrivere, tramite un commento
# testuale, il tipo di ogni attributo (nominale, ordinale, di intervallo o di rapporto).


# Stampo inizialmente il mio dataset per controllare se vanno rinominate
# le colonne in modo appropriato
str(dataset)


# Quindi avendo trovato delle incongruenze con i nomi assegnati alle colonne le 
# ho rinominate secondo me nel modo pi� opportuno

names(dataset)[names(dataset) == "cp"] <- "chest_pain"
names(dataset)[names(dataset) == "trestbps"] <- "rest_bp"
names(dataset)[names(dataset) == "chol"] <- "cholesterol"
names(dataset)[names(dataset) == "thalach"] <- "max_hr"
names(dataset)[names(dataset) == "exang"] <- "exercise_angina"
names(dataset)[names(dataset) == "thal"] <- "thalassemia"
names(dataset)[names(dataset) == "target"] <- "heart_disease"
names(dataset)[names(dataset) == "ca"] <- "n_vessels"
names(dataset)[names(dataset) == "restecg"] <- "rest_ecg"

# E poi ho assegnato ad ogni attributo il suo tipo

# $ age            : int  63 37 41 56 57 57 56 44 52 57 ...           ORDINALE
# $ sex            : chr  "1" "1" "0" "1" ...                         NOMINALE
# $ chest_pain     : int  3 2 1 1 0 0 1 1 2 2 ...                     NOMINALE
# $ rest_bp        : int  145 130 130 120 120 140 140 120 51 150 ...  DI RAPPORTO
# $ cholesterol    : chr  "233" "250" "204" "236" ...                 DI INTERVALLO
# $ fbs            : int  1 0 0 0 0 0 0 0 1 0 ...                     DI RAPPORTO
# $ rest_ecg       : int  0 1 0 1 1 1 0 1 1 1 ...                     NOMINALE
# $ max_hr         : int  150 187 172 178 163 148 153 173 162 174 ... DI INTERVALLO
# $ exercise_angina: int  0 0 0 0 1 0 0 0 0 0 ...                     NOMINALE
# $ oldpeak        : num  2.3 3.5 1.4 0.8 0.6 0.4 1.3 0 0.5 1.6 ...   ORDINALE
# $ slope          : int  0 0 2 2 2 1 1 2 2 2 ...                     NOMINALE
# $ n_vessels      : int  0 0 0 0 0 0 0 0 0 0 ...                     ORDINALE
# $ thalassemia    : int  1 2 2 2 2 1 2 3 3 2 ...                     NOMINALE
# $ heart_disease  : int  1 1 1 1 1 1 1 1 1 1 ...                     NOMINALE




# es 2
# Trasformare i dati in modo che siano tecnicamente corretti.
# Ho trasformato i dati secondo me non capibili in una chiave pi� 
# leggibile 

# Ho trasformato, nella colonna sex, i semplici valori "0" e "1"  in 
# "F" per femmina e in "M" per maschio e poi creato un fattore 
dataset$sex[(dataset$sex == "0")] <- "F"
dataset$sex[(dataset$sex == "1")] <- "M" 
dataset$sex <- as.factor(dataset$sex)

 
# Ho trasformato il tipo di dato per la colonna chest pain 
# da int a factor quindi diviso in pi� livelli (0 - 1 - 2 - 3)
dataset$chest_pain <- as.factor(dataset$chest_pain)


# Per la colonna cholesterol ho trasformato in un primo luogo tutti i valori
# "undefined" nella mediana dei valori di tutta la mia colonna
# e in secondo luogo ho traformato il tipo di dato da char a integer
dataset$cholesterol[(dataset$cholesterol == "undefined")] <- median(dataset$cholesterol)
dataset$cholesterol <- as.integer(dataset$cholesterol)


# Ho trasformato il tipo di dato per la colonna fbs 
# da int a factor quindi diviso in pi� livelli (1 - 0)
dataset$fbs <- as.factor(dataset$fbs)


# Ho trasformato il tipo di dato per la colonna rest_ecg 
# da int a factor quindi diviso in pi� livelli (0 - 1 - 2)
dataset$rest_ecg <- as.factor(dataset$rest_ecg)


# Ho trasformato il tipo di dato per la colonna exercise_angina 
# da int a factor quindi diviso in pi� livelli (1 - 0)
dataset$exercise_angina <- as.factor(dataset$exercise_angina)


# Ho trasformato il tipo di dato per la colonna slope 
# da num a factor quindi diviso in pi� livelli (0 - 1 - 2)
dataset$slope <- as.factor(dataset$slope)


# Ho trasformato il tipo di dato per la colonna thalassemia 
# da int a factor quindi diviso in pi� livelli (0 - 1 - 2 - 3)
dataset$thalassemia <- as.factor(dataset$thalassemia)


# Ho trasformato il tipo di dato per la colonna heart_disease 
# da int a factor quindi diviso in pi� livelli (1 - 0)
dataset$heart_disease <- as.factor(dataset$heart_disease)


str(dataset)


# 4.Rinominare i livelli dei fattori in maniera appropriata, se necessario.


# levels for chest pain
levels(dataset$chest_pain)[levels(dataset$chest_pain)== 0 ] <- "asymptomatic"
levels(dataset$chest_pain)[levels(dataset$chest_pain)== 1 ] <- "nontypical_angina"
levels(dataset$chest_pain)[levels(dataset$chest_pain)== 2 ] <- "nonanginal_pain"
levels(dataset$chest_pain)[levels(dataset$chest_pain)== 3 ] <- "typical_angina"


# levels for fbs
levels(dataset$fbs)[levels(dataset$fbs)== 0 ] <- "False"
levels(dataset$fbs)[levels(dataset$fbs)== 1 ] <- "True"


# levels for rest_ecg
levels(dataset$rest_ecg)[levels(dataset$rest_ecg)== 0 ] <- "Ventricular_hypertrophy"
levels(dataset$rest_ecg)[levels(dataset$rest_ecg)== 1 ] <- "Normal"
levels(dataset$rest_ecg)[levels(dataset$rest_ecg)== 2 ] <- "Anomaly"


# levels for exer angina
levels(dataset$exercise_angina)[levels(dataset$exercise_angina)== 0 ] <- "No"
levels(dataset$exercise_angina)[levels(dataset$exercise_angina)== 1 ] <- "Yes"


# levels for slope
levels(dataset$slope)[levels(dataset$slope)== 0 ] <- "Descending"
levels(dataset$slope)[levels(dataset$slope)== 1 ] <- "Flat"
levels(dataset$slope)[levels(dataset$slope)== 2 ] <- "Ascending"


# levels for thalessimia
levels(dataset$thalassemia)[levels(dataset$thalassemia)== 0 ] <- "non_existent"
levels(dataset$thalassemia)[levels(dataset$thalassemia)== 1 ] <- "defect_corrected"
levels(dataset$thalassemia)[levels(dataset$thalassemia)== 2 ] <- "normal_blood"
levels(dataset$thalassemia)[levels(dataset$thalassemia)== 3 ] <- "reversible_defect"


# levels for heart disease
levels(dataset$heart_disease)[levels(dataset$heart_disease)==0] <- "Yes"
levels(dataset$heart_disease)[levels(dataset$heart_disease)==1] <- "No"


str(dataset)
# 5.Descrivere brevemente gli attributi.



# età: l'età della persona in anni
# sesso: il sesso della persona (1 = maschio, 0 = femmina)
# cp: tipo di dolore toracico - Valore 0: asintomatico - Valore 1: angina atipica - Valore 2: dolore non anginoso - Valore 3: angina tipica
# trestbps: la pressione sanguigna a riposo della persona (mm Hg al momento del ricovero in ospedale)
# chol: misura del colesterolo della persona in mg / dl
# fbs: glicemia a digiuno della persona (> 120 mg / dl, 1 = vero; 0 = falso)
# restecg: risultati elettrocardiografici a riposo - Valore 0: mostra probabile o definita ipertrofia ventricolare sinistra secondo i criteri di Estes - Valore 1: normale - Valore 2: con anomalia dell'onda ST-T (inversioni dell'onda T e / o elevazione o depressione ST> 0,05 mV )
# thalach: la frequenza cardiaca massima raggiunta dalla persona
# exang: angina indotta dall'esercizio (1 = si; 0 = no)
# oldpeak: depressione ST indotta dall'esercizio rispetto al riposo ("ST" si riferisce alle posizioni sul grafico ECG. Vedi di più qui)
# pendenza: la pendenza del segmento ST dell'esercizio di punta - 0: discesa; 1: piatto; 2: in salita 0: in discesa; 1: piatto; 2: rialzo
# ca: il numero di navi principali (0-3)
# tal: una malattia del sangue chiamata talassemia Valore 0: NULL (eliminato dal dataset precedente Valore 1: difetto corretto (nessun flusso sanguigno in alcune parti del cuore) Valore 2: flusso sanguigno normale Valore 3: difetto reversibile (si osserva un flusso sanguigno ma non è normale)
# target: malattie cardiache (1 = no, 0 = sì)


# 1.Trasformare i dati in modo che siano consistenti. Assumere, ad esempio, che la
# frequenza cardiaca massima non possa essere superiore a 222, sostituendo i valori
# maggiori di 222 con il valore medio della variabile.



hist(dataset$max_hr, col = "red")
dataset$max_hr[dataset$max_hr > 222] <- mean(dataset$max_hr)
hist(dataset$max_hr,  col= "green")





# 2.Trasformare i dati in modo che siano consistenti. Assumere come outlier, ad
# esempio, i valori relativi alla pressione sanguigna a riposo che non rispettano la
# 1.5xIQR Rule. Individuare e rimuovere tali valori.



boxplot(dataset$rest_bp, col = c("red"))
hist(dataset$rest_bp, col = "red")
Q3 <- quantile(dataset$rest_bp, 0.75)
Q1 <- quantile(dataset$rest_bp, 0.25)




IQR<-(Q3-Q1)
left<- (Q1-(1.5*IQR))
right<- (Q3+(1.5*IQR))



rest_bp_IQR <- dataset$rest_bp[dataset$rest_bp > left & dataset$rest_bp < right]



hist(rest_bp_IQR, col = "green")
boxplot(rest_bp_IQR, col = "green")





# 3.Trasformare i dati in modo che siano consistenti. Sono necessarie altre
# trasformazioni? Se si, quali?
View(dataset)
summary(dataset$age)


dataset$age <- as.integer(dataset$age)
dataset$age[dataset$age < 0 | dataset$age > 120] <- mean(dataset$age)


summary(dataset$age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.00   48.00   55.00   54.49   61.00   77.00

summary(dataset$rest_bp)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 94.0   120.0   130.0   127.9   138.0   150.0 

dataset$rest_bp[dataset$rest_bp < 70 | dataset$rest_bp > 150] <- mean(dataset$rest_bp)


summary(dataset$rest_bp)
View(dataset)

#   4.Visualizzare, prima e dopo le trasformazioni, i grafici ritenuti più opportuni.
#    5. analisi descrittiva con i grafici


# 1. Analizzare la relazione tra due variabili del dataset attraverso la regressione lineare semplice e determinare: 
# • il grafico del modello; 
# • il coefficiente angolare e l’intercetta (interpretabile) della retta di regressione; 
# • il tipo di relazione tramite re la bontà del modello tramite R!; 
# • l’analisi dei residui e la distribuzione in quantili, con i relativi grafici. 

summary(dataset$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.00   48.00   55.00   54.49   61.00   77.00

summary(dataset$rest_bp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 94.0   120.0   130.0   127.9   138.0   150.0 


# regressione lineare tra rest_bp e age
# importante l'unità di misura!!!
plot(dataset$age, dataset$rest_bp, xlab = "age (year)", ylab= "rest_bp (mm/Hg)")

# invertire l'ordine!


reg <- lm(dataset$rest_bp ~ dataset$age)

# per disegnare la retta di regressione lineare
abline (reg, col = "red")

# reg0 --> abbiamo visto che tra age e rest_bp non c'� correlazione
# Call:
#   lm(formula = dataset$rest_bp ~ dataset$age)
# 
# Coefficients:
#   (Intercept)  dataset$age  
# 114.3157       0.2495  


summary(dataset$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.00   48.00   55.00   54.49   61.00   77.00

summary(dataset$max_hr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 71.0   134.0   152.0   149.5   166.0   202.0 


# regressione lineare tra max_hr e age
# importante l'unità di misura!!!
plot(dataset$age, dataset$max_hr, xlab = "age (year)", ylab= "max_hr (Bpm)")

# invertire l'ordine!

reg0 <- lm(max_hr ~ age, data = dataset)

# per disegnare la retta di regressione lineare
abline (reg0, col = "red")


# per visualizzare i residui
# x di partenza, y di partenza
# x di arrivo, y di arrivo
# colore, tipo tratto

segments(dataset$age, fitted(reg0), dataset$age, dataset$max_hr, col = "blue", lty = 2)
title(main = "Regr.lin tra max_hr e age")

summary(reg0)

# Call:
#   lm(formula = dataset$max_hr ~ dataset$age)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -66.030 -11.919   4.046  16.010  45.036 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 203.6911     7.6023  26.793  < 2e-16 ***
#   dataset$age  -0.9949     0.1377  -7.226 4.27e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.16 on 295 degrees of freedom
# Multiple R-squared:  0.1504,	Adjusted R-squared:  0.1475 
# F-statistic: 52.21 on 1 and 295 DF,  p-value: 4.275e-12



# calcolo il coefficiente di correlazione lineare
r0 <- cor(dataset$max_hr, dataset$age)
r0
# [1] -0.3877871

# R^2 = 0.1503788 coefficiente di determinazione
rq0 <- r0^2
rq0
# [1] 0.1503788

r0 <- cov(dataset$max_hr, dataset$age) / (sd(dataset$max_hr) * sd(dataset$age))
r0
r0^2


# analisi dei residui


plot(reg0$fitted, reg0$residuals, main = "Residui")
abline(0,0)

# distribuzione in quantili confrontabile con
# quella di una normale

qqnorm(reg0$residuals)
qqline(reg0$residuals)


# 2. Creare un data frame contenente 10 osservazioni (non presenti nel dataset) ed effettuare delle previsioni.



dat <- read.csv("osservazioni.csv", header = TRUE, stringsAsFactors = FALSE)
str (dat)
View(dat)


predict(reg0, dat)

# 1         2         3         4         5         6         7         8         9        10        11        12        13 
# 113.17265 128.09188 120.13495 172.84956 137.04341 157.93033 103.22649 155.94110  99.24803  92.28572 160.91418 117.15111 163.89803 
# 14        15        16        17        18        19        20 
# 116.15649  88.30726 129.08649 174.83880 135.05418 153.95187 123.11880 


# � possibile ottenere gli IC per il valore medio predetto predict(reg, df_pred, interval = "confidence") 
predict(reg0, dat, interval = "confidence") 

# fit       lwr      upr
# 1  113.17265 102.98835 123.3569
# 2  128.09188 121.78282 134.4009
# 3  120.13495 111.78158 128.4883
# 4  172.84956 166.03791 179.6612
# 5  137.04341 132.88143 141.2054
# 6  157.93033 154.59214 161.2685
# 7  103.22649  90.39313 116.0599
# 8  155.94110 152.95103 158.9312
# 9   99.24803  85.34842 113.1476
# 10  92.28572  76.51432 108.0571
# 11 160.91418 156.97083 164.8575
# 12 117.15111 108.01637 126.2858
# 13 163.89803 159.28514 168.5109
# 14 116.15649 106.76010 125.5529
# 15  88.30726  71.46370 105.1508
# 16 129.08649 123.02691 135.1461
# 17 174.83880 167.51780 182.1598
# 18 135.05418 130.44011 139.6683
# 19 153.95187 151.24537 156.6584
# 20 123.11880 115.54012 130.6975






# 1. Applicare un modello di Machine Learning a scelta, misurandone l'accuratezza sul test set.
# seed = set.seed(2021)
# training_index <- createDataPartition(dataset$age, p = .80, list = FALSE)
# 
# training_set <- dataset[training_index, ]
# nrow(training_set)
# # use the remaining 20% of the data for test
# test_set <- dataset[-training_index, ]
# nrow(test_set)

control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"

## kNN
fit_knn <- train(dataset$age ~ dataset$max_hr, data = training_set, metric = metric, trControl = control, method = "knn")
## MLP
fit_mlp <- train(dataset$age ~ dataset$max_hr, data = training_set, metric = metric, trControl = control, method = "mlp")


# 2. Descrivere brevemente il funzionamento del modello scelto.



























