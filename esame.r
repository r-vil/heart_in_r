

# 1.Caricare il dataset heart.csv e analizzarne dettagliatamente la struttura.


dataset <- read.csv("heart.csv", header = TRUE, stringsAsFactors = FALSE)
str (dataset)
View(dataset)




# 1.1 Controllare se sono presenC valori NA e, nel caso, rimuoverli.


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
# questo ciclo if fatto sopra mi stampa a video se ci sono o meno dei valori NULL nel dataset





controllo_isn(dataset) 


# 2.1 Rimuovere le colonne ritenute non necessarie.



dataset <- na.omit(dataset) 




# elimino anche la colonna X perchè da me ritenuta inutile
dataset <- subset(dataset, select = - x)





# Rinominare le colonne in maniera appropriata e descrivere, tramite un commento
# testuale, il tipo di ogni attributo (nominale, ordinale, di intervallo o di rapporto).




# stampo inizialmente il mio dataset per controllare se vanno rinominate
# le colonne in modo appropriato
str(dataset)




# 3.Rinominare le colonne in maniera appropriata e descrivere il tipo di ogni attributo
# (nominale, ordinale, di intervallo o di rapporto).




names(dataset)[names(dataset) == "cp"] <- "chest_pain"


names(dataset)[names(dataset) == "trestbps"] <- "rest_bp"


names(dataset)[names(dataset) == "chol"] <- "cholesterol"


names(dataset)[names(dataset) == "thalach"] <- "max_hr"


names(dataset)[names(dataset) == "exang"] <- "exercise_angina"


names(dataset)[names(dataset) == "thal"] <- "thalassemia"


names(dataset)[names(dataset) == "target"] <- "heart_disease"


names(dataset)[names(dataset) == "ca"] <- "n_vessels"


names(dataset)[names(dataset) == "restecg"] <- "rest_ecg"



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





# 2.Trasformare i dati in modo che siano tecnicamente corretti.



#sex
dataset$sex[(dataset$sex == "0")] <- "F"
dataset$sex[(dataset$sex == "1")] <- "M" 
dataset$sex <- as.factor(dataset$sex)


# chest pain
dataset$chest_pain <- as.factor(dataset$chest_pain)


# colesterolo
dataset$cholesterol[(dataset$cholesterol == "undefined")] <- median(dataset$cholesterol)
dataset$cholesterol <- as.integer(dataset$cholesterol)


#fbs glicemia
dataset$fbs <- as.factor(dataset$fbs)


#rest electrocardiogramma
dataset$rest_ecg <- as.factor(dataset$rest_ecg)


#ex angina
dataset$exercise_angina <- as.factor(dataset$exercise_angina)


# slope
dataset$slope <- as.factor(dataset$slope)


# thalassemia
dataset$thalassemia <- as.factor(dataset$thalassemia)


# heart disease
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


summary(dataset$rest_bp)


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
reg0 <- lm(dataset$rest_bp ~ dataset$age)

# per disegnare la retta di regressione lineare
abline (reg0, col = "red")

# reg0 --> abbiamo visto che tra age e rest_bp non c'è correlazione
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
reg0 <- lm(dataset$max_hr ~ dataset$age)

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


altezza <- c(174, 166, 173, 171, 168, 167, 165,
             164, 181, 185)
summary(altezza)
## Min. 1st Qu. Median Mean 3rd Qu.
# Max.
## 164.0 166.2 169.5 171.4 173.8 185.0

peso <- c(75, 63, 70, 71, 68, 68, 64, 60, 86, 91)
summary(peso)




# 1. Applicare un modello di Machine Learning a scelta, misurandone l’accuratezza sul test set. 
# 2. Descrivere brevemente il funzionamento del modello scelto.



























