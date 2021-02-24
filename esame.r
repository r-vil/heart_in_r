# COMINI VILLANUEVA
# ESAME R

'
1 --> Caricare il datasetheart.csve analizzarne dettagliatamente la struttura. 
2 --> Trasformare i dati in modo che siano tecnicamente corretti. 
3 --> Rinominare le colonne in maniera appropriata e descrivere il tipodi ogni attributo 
      (nominale, ordinale, di intervallo o di rapporto). 
4 --> Rinominare i livellidei fattori in maniera appropriata, se necessario. 
5 --> Descrivere brevemente gli attributi.
1.1 --> Controllare se sono presenti valori NA e, nel caso, rimuoverli. 
2.1 --> Rimuovere le colonne ritenute non necessarie.
1.3 --> Trasformare i dati in modo che siano consistenti. Assumere, ad esempio, che la
        frequenza cardiaca massima non possa essere superiore a 222, sostituendo i valori
        maggiori di 222 con il valore medio della variabile.
2.3 --> Trasformare i dati in modo che siano consistenti. Assumere come outlier, ad
        esempio, i valori relativi alla pressione sanguigna a riposo che non rispettano la
        1.5xIQR Rule. Individuare e rimuovere tali valori.
3.1 --> Trasformare i dati in modo che siano consistenti. Sono necessarie altre
        trasformazioni? Se si, quali?
4.1 --> Ricordarsi di visualizzare, prima e dopo le trasformazioni, i grafici ritenuti piÃ¹ opportuni.
5.1 --> Ricordarsi di condurre una analisi descrittiva con i grafici
6 --> Analizzare la relazione tra due variabili del dataset attraverso la regressione lineare 
semplice e determinare: 
- il grafico del modello; 
- il coefficiente angolare e la intercetta (interpretabile) della retta di regressione; 
- il tipo di relazione tramite re la bontà del modello tramite R^2; 
- la analisi dei residui e la distribuzione in quantili, con i relativi grafici. 
2.4 --> Creare un data frame contenente 10 osservazioni (non presenti nel dataset) 
        ed effettuare delle previsioni.
2.5 --> Descrivere brevemente il funzionamento del modello scelto (eseguito sulla descrizione).
'

 #install.packages("tidyverse")
library(tidyverse)

 #install.packages("caret")
library(caret)

 #install.packages("ggthemes")
library(ggthemes)


# es 1
# Caricare il dataset heart.csv e analizzarne dettagliatamente la struttura.

dataset <- read.csv("heart.csv", header = TRUE, stringsAsFactors = FALSE) %>% as_tibble()
dataset

View(dataset)

# es 1.1
# Controllare se sono presenti valori NA e, nel caso, rimuoverli.
# Questo ciclo if fatto sopra mi stampa a video se ci sono o meno dei valori NULL nel dataset.

controllo_isn <- function(dataset)
{
  return(paste("c'è/ ci sono", sum(is.na(dataset)), "valore/i NA"))
}
controllo_isn(dataset) 
dataset <- na.omit(dataset) 

# es 2.1 
# Rimuovere le colonne ritenute non necessarie.
# Elimino la colonna X perchè da me ritenuta inutile

# ---- FUNZIONE SENZA LIBRERIA TIDYVERSE ------
# dataset <- subset(dataset, select = - x)

# es 3
# Rinominare le colonne in maniera appropriata e descrivere, tramite un commento
# testuale, il tipo di ogni attributo (nominale, ordinale, di intervallo o di rapporto).
# Stampo inizialmente il mio dataset per controllare se vanno rinominate
# le colonne in modo appropriato

str(dataset)
# Quindi, avendo trovato delle incongruenze con i nomi assegnati alle colonne, le 
# abbiamo  rinominate secondo noi nel modo più opportuno.

dataset <- dataset %>%  
  
  select(-one_of("x")) %>%
  rename(
    
    chest_pain = cp,
    rest_bp = trestbps,
    cholesterol = chol,
    max_hr= thalach,
    exercise_angina = exang,
    thalessemia = thal,
    heart_disease = target,
    n_vessels = ca,
    rest_ecg = restecg
    
  )

str(dataset)

# ---- FUNZIONE SENZA TIDYVERSE------
# names(dataset)[names(dataset) == "cp"] <- "chest_pain"
# names(dataset)[names(dataset) == "trestbps"] <- "rest_bp"
# names(dataset)[names(dataset) == "chol"] <- "cholesterol"
# names(dataset)[names(dataset) == "thalach"] <- "max_hr"
# names(dataset)[names(dataset) == "exang"] <- "exercise_angina"
# names(dataset)[names(dataset) == "thal"] <- "thalassemia"
# names(dataset)[names(dataset) == "target"] <- "heart_disease"
# names(dataset)[names(dataset) == "ca"] <- "n_vessels"
# names(dataset)[names(dataset) == "restecg"] <- "rest_ecg"

# E poi abbiamo assegnato ad ogni attributo il suo tipo

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
# Abbiamo trasformato i dati secondo noi non capibili in una chiave più 
# leggibile. 


dataset <- dataset %>% 
  
  mutate(
    
    age <- as.integer(age),
    sex = ifelse(sex == "1", "M", "F"),
    sex = as.factor(sex),
    chest_pain = as.factor(chest_pain),
    cholesterol = ifelse(cholesterol == "undefined", median(cholesterol), cholesterol),
    cholesterol = as.integer(cholesterol),
    fbs = as.factor(fbs),
    rest_ecg = as.factor(rest_ecg),
    exercise_angina = as.factor(exercise_angina),
    slope = as.factor(slope),
    thalessemia = as.factor(thalessemia),
    heart_disease = as.factor(heart_disease)
    
  )

str(dataset)

# ---- FUNZIONI SENZA TIDYVERSE------ 

# Abbiamo trasformato, nella colonna sex, i semplici valori "0" e "1"  in 
# "F" per femmina e in "M" per maschio e poi creato un fattore.
# dataset$sex[(dataset$sex == "0")] <- "F"
# dataset$sex[(dataset$sex == "1")] <- "M" 
# dataset$sex <- as.factor(dataset$sex)

# Abbiamo trasformato il tipo di dato per la colonna chest pain 
# da int a factor quindi diviso in più livelli (0 - 1 - 2 - 3)
# dataset$chest_pain <- as.factor(dataset$chest_pain)

# Per la colonna cholesterol Abbiamo trasformato in un primo luogo tutti i valori
# "undefined" nella mediana dei valori di tutta la mia colonna
# e in secondo luogo Abbiamo trasformato il tipo di dato da char a integer
# dataset$cholesterol[(dataset$cholesterol == "undefined")] <- median(dataset$cholesterol)
# dataset$cholesterol <- as.integer(dataset$cholesterol)

# Abbiamo trasformato il tipo di dato per la colonna fbs 
# da int a factor quindi diviso in più livelli (1 - 0)
# dataset$fbs <- as.factor(dataset$fbs)

# Abbiamo trasformato il tipo di dato per la colonna rest_ecg 
# da int a factor quindi diviso in più livelli (0 - 1 - 2)
# dataset$rest_ecg <- as.factor(dataset$rest_ecg)

# Abbiamo trasformato il tipo di dato per la colonna exercise_angina 
# da int a factor quindi diviso in più livelli (1 - 0)
# dataset$exercise_angina <- as.factor(dataset$exercise_angina)

# Abbiamo trasformato il tipo di dato per la colonna slope 
# da num a factor quindi diviso in più livelli (0 - 1 - 2)
# dataset$slope <- as.factor(dataset$slope)

# Abbiamo trasformato il tipo di dato per la colonna thalassemia 
# da int a factor quindi diviso in più livelli (0 - 1 - 2 - 3)
# dataset$thalassemia <- as.factor(dataset$thalassemia)

# Abbiamo trasformato il tipo di dato per la colonna heart_disease 
# da int a factor quindi diviso in più livelli (1 - 0)
# dataset$heart_disease <- as.factor(dataset$heart_disease)



# es 4
# Rinominare i livelli dei fattori in maniera appropriata, se necessario.

tmp <- recode_factor(dataset$chest_pain, "0" = "asymptomatic", "1" = "nontypical_angina", "2" = "nonangial_pain", "3" = "typical_angina")
dataset <- dataset %>%
  
  mutate(
    chest_pain =      recode_factor(chest_pain, 
                                  "0" = "asymptomatic", 
                                  "1" = "nontypical_angina", 
                                  "2" = "nonangial_pain", 
                                  "3" = "typical_angina"),
    fbs =             recode_factor(fbs,
                                  "0" = "False",
                                  "1" = "True"),
    rest_ecg =        recode_factor(rest_ecg,
                                  "0" = "Ventricular_hypertrophy",
                                  "1" = "Normal",
                                  "2" = "Anomaly"),
    exercise_angina = recode_factor(exercise_angina,
                                  "0" = "False",
                                  "1" = "True"),
    slope =           recode_factor(slope,
                                  "0" = "Descending",
                                  "1" = "Flat",
                                  "2" = "Ascending"),
    thalessemia =     recode_factor(thalessemia,
                                  "0" = "non_existent",
                                  "1" = "defect_corrected",
                                  "2" = "normal_blood",
                                  "3" = "reversible_defect"),
    heart_disease =   recode_factor(heart_disease,
                                  "0" = "Yes",
                                  "1" = "No")
  )


view(dataset)



#  ---- FUNZIONI SENZA TIDYVERSE------ 
# 
# levels for chest pain
# levels(dataset$chest_pain)[levels(dataset$chest_pain)== 0 ] <- "asymptomatic"
# evels(dataset$chest_pain)[levels(dataset$chest_pain)== 1 ] <- "nontypical_angina"
# levels(dataset$chest_pain)[levels(dataset$chest_pain)== 2 ] <- "nonanginal_pain"
# levels(dataset$chest_pain)[levels(dataset$chest_pain)== 3 ] <- "typical_angina"

# levels for fbs
# levels(dataset$fbs)[levels(dataset$fbs)== 0 ] <- "False"
# levels(dataset$fbs)[levels(dataset$fbs)== 1 ] <- "True"

# levels for rest_ecg
# levels(dataset$rest_ecg)[levels(dataset$rest_ecg)== 0 ] <- "Ventricular_hypertrophy"
# levels(dataset$rest_ecg)[levels(dataset$rest_ecg)== 1 ] <- "Normal"
# levels(dataset$rest_ecg)[levels(dataset$rest_ecg)== 2 ] <- "Anomaly"

# levels for exer angina
# levels(dataset$exercise_angina)[levels(dataset$exercise_angina)== 0 ] <- "No"
# levels(dataset$exercise_angina)[levels(dataset$exercise_angina)== 1 ] <- "Yes"

# lvels for exercise_angina
# levels for exer angina
# levels(dataset$exercise_angina)[levels(dataset$exercise_angina)== 0 ] <- "No"
# levels(dataset$exercise_angina)[levels(dataset$exercise_angina)== 1 ] <- "Yes"

# levels for slope
# levels(dataset$slope)[levels(dataset$slope)== 0 ] <- "Descending"
# levels(dataset$slope)[levels(dataset$slope)== 1 ] <- "Flat"
# levels(dataset$slope)[levels(dataset$slope)== 2 ] <- "Ascending"
 

# levels for thalessimia
# levels(dataset$thalassemia)[levels(dataset$thalassemia)== 0 ] <- "non_existent"
# levels(dataset$thalassemia)[levels(dataset$thalassemia)== 1 ] <- "defect_corrected"
# levels(dataset$thalassemia)[levels(dataset$thalassemia)== 2 ] <- "normal_blood"
# levels(dataset$thalassemia)[levels(dataset$thalassemia)== 3 ] <- "reversible_defect"

# levels for heart disease
# levels(dataset$heart_disease)[levels(dataset$heart_disease)==0] <- "Yes"
# levels(dataset$heart_disease)[levels(dataset$heart_disease)==1] <- "No"


# es 5
# -------- Descrivere brevemente gli attributi. --------


# età: l'età della persona in anni

# sesso: il sesso della persona (1 = maschio, 0 = femmina)

# cp: tipo di dolore toracico: 
# --> Valore 0: asintomatico 
# --> Valore 1: angina atipica 
# --> Valore 2: dolore non anginoso 
# --> Valore 3: angina tipica

# trestbps: la pressione sanguigna a riposo della persona (mm Hg al momento del ricovero in ospedale)

# chol: misura del colesterolo della persona in mg / dl

# fbs: glicemia a digiuno della persona (> 120 mg / dl, 1 = vero; 0 = falso)

# restecg: risultati elettrocardiografici a riposo 
# --> Valore 0: mostra probabile o definita ipertrofia ventricolare sinistra secondo i criteri di Estes - Valore 1: normale 
# --> Valore 2: con anomalia dell'onda ST-T (inversioni dell'onda T e / o elevazione o depressione ST> 0,05 mV )

# thalach: la frequenza cardiaca massima raggiunta dalla persona

# exang: angina indotta dall'esercizio (1 = si; 0 = no)

# oldpeak: depressione ST indotta dall'esercizio rispetto al riposo ("ST" si riferisce alle posizioni sul grafico ECG)

# pendenza: la pendenza del segmento ST dell'esercizio di punta 
# --> Valore 0: discesa
# --> Valore 1: piatto
# --> Valore 2: in salita/rialzo

# ca: il numero di navi principali (0-3)

# tal: una malattia del sangue chiamata talassemia 
# --> Valore 0: NULL (eliminato dal dataset precedente 
# --> Valore 1: difetto corretto (nessun flusso sanguigno in alcune parti del cuore) 
# --> Valore 2: flusso sanguigno normale 
# --> Valore 3: difetto reversibile (si osserva un flusso sanguigno ma non Ã¨ normale)

# target: malattie cardiache (1 = no, 0 = si)


# es 1.3
# Trasformare i dati in modo che siano consistenti. Assumere, ad esempio, che la
# frequenza cardiaca massima non possa essere superiore a 222, sostituendo i valori
# maggiori di 222 con il valore medio della variabile.
# La funzione generica hist calcola un istogramma dei valori di dati forniti.




hist_ggplot <- function(dataset, x, f, title, x_title){
  output <- dataset %>% 
    ggplot(aes(x, fill = f )) +
    geom_histogram() +
    labs(title = title,
         x = x_title,
         fill = "")+
    theme_fivethirtyeight()+
    theme(axis.title = element_text())
  
  
  
  return(output)
  
}

boxplot_ggplot <- function(dataset, y, f, title, y_title) {
  output <-dataset %>%
    ggplot(aes(x=f, y = y, fill = f)) +
    geom_boxplot() +
    labs(title = title,
         y = y_title,
         fill = "")+
    stat_boxplot(geom = "errorbar", width = 0.2)
  theme_fivethirtyeight()+
    theme(axis.title = element_text())
  
  return(output)
  
}

# hist(dataset$max_hr, col = "red")

hist_ggplot(dataset, dataset$max_hr, NULL, "Istogramma del massimo battito cardiato", "max heart-rate(BPS)")
dataset$max_hr[dataset$max_hr > 222] <- mean(dataset$max_hr)
hist_ggplot(dataset, dataset$max_hr, NULL, "Istogramma del massimo battito cardiato", "max heart-rate(BPS)")


# es 2.3
# Trasformare i dati in modo che siano consistenti. Assumere come outlier, ad
# esempio, i valori relativi alla pressione sanguigna a riposo che non rispettano la
# 1.5xIQR Rule. Individuare e rimuovere tali valori.



#  ---- FUNZIONI SENZA TIDYVERSE e GGPLOT------ 
# # hist(dataset$rest_bp, col = "red")
# boxplot(dataset$rest_bp, col = c("red"))



hist_ggplot(dataset, dataset$rest_bp ,dataset$sex ,"histogramma massimo del battito cardiaco a riposo diviso per il sesso", "heart-rate on rest (BPS)")
boxplot_ggplot(dataset, dataset$rest_bp ,dataset$sex ,"boxplot del massimo battito cardiaco a riposo diviso per il sesso", "heart-rateon rest (BPS)")

Q3 <- quantile(dataset$rest_bp, 0.75)
Q1 <- quantile(dataset$rest_bp, 0.25)
IQR<-(Q3-Q1)
left<- (Q1-(1.5*IQR))
right<- (Q3+(1.5*IQR))
rest_bp_IQR <- data.frame(rest_bp = dataset$rest_bp[dataset$rest_bp > left & dataset$rest_bp < right], sex = dataset$sex[dataset$rest_bp > left & dataset$rest_bp < right])




#  ---- FUNZIONI SENZA TIDYVERSE e GGPLOT------ 
# hist(rest_bp_IQR, col = "green")
# boxplot(rest_bp_IQR, col = "green")

hist_ggplot(rest_bp_IQR, rest_bp_IQR$rest_bp, rest_bp_IQR$sex, "histogramma massimo del battito cardiaco a riposo diviso per il sesso", "max heart-rate (BPM)")
boxplot_ggplot(rest_bp_IQR, rest_bp_IQR$rest_bp ,rest_bp_IQR$sex, "boxplot del massimo battito cardiaco a riposo diviso per il sesso", "max heart-rate (BPM)")


# es 3.1
# Trasformare i dati in modo che siano consistenti. Sono necessarie altre
# trasformazioni? Se si, quali?

View(dataset)

# Abbiamo impostato un filtro sui dati della colonna età 

summary(dataset$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -10.00   47.00   55.00   53.91   61.00   77.00 

dataset$age[dataset$age < 0 | dataset$age > 120] <- mean(dataset$age)

summary(dataset$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.00   48.00   55.00   54.49   61.00   77.00


# Abbiamo impostato un filtro sui dati della colonna rest_bp

summary(dataset$rest_bp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 51.0   120.0   130.0   131.3   140.0   200.0

dataset$rest_bp[dataset$rest_bp < 70 | dataset$rest_bp > 150] <- mean(dataset$rest_bp)


summary(dataset$rest_bp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 94.0   120.0   130.0   127.9   138.0   150.0 


# Abbiamo impostato un filtro sui dati della colonna cholesterol

summary(dataset$cholesterol)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 126.0   211.0   243.0   246.7   275.0   564.0 


dataset$cholesterol[dataset$cholesterol > 400] <- mean(dataset$cholesterol)

summary(dataset$cholesterol)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 126     211     243     244     273     394 



# es 4.1
# Ricordarsi di visualizzare, prima e dopo le trasformazioni, i grafici ritenuti piÃ¹ opportuni.
# es 5.1
# Ricordarsi di condurre un'analisi descrittiva con i grafici


# es 6
# Analizzare la relazione tra due variabili del dataset attraverso la regressione lineare 
# semplice e determinare: 
# - il grafico del modello; 
# - il coefficiente angolare e l'intercetta (interpretabile) della retta di regressione; 
# - il tipo di relazione tramite re la bontà del modello tramite R^2; 
# - l'analisi dei residui e la distribuzione in quantili, con i relativi grafici. 


summary(dataset$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.00   48.00   55.00   54.49   61.00   77.00
summary(dataset$rest_bp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 94.0   120.0   130.0   127.9   138.0   150.0 


# Regressione lineare tra rest_bp e age
# importante l'unità di misura!!!
# Plot è una funzione generica per la rappresentazione grafica di oggetti in R. 
# Funzione generica significa che si adatta a diversi tipi di oggetti, 
# dalle variabili alle tabelle agli output di funzioni complesse, producendo risultati diversi.

#  ---- FUNZIONI SENZA TIDYVERSE e GGPLOT------ 
# plot(dataset$age, dataset$rest_bp, xlab = "age (year)", ylab= "rest_bp (mm/Hg)")


dataset %>% 
  ggplot(aes(x = age, y = rest_bp)) +
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Scatterplot tra battiti cardiaci a riposo e età",
       y = "heart-rate in rest(mm/Hg)",
       x = "age (y)")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), plot.background = element_rect(fill = "#f75e25")) 

# Ricordarsi di invertire l'ordine!
reg <- lm(dataset$rest_bp ~ dataset$age)

# La funzione abline() disegna una retta di regressione attraverso i suoi parametri:
# a = intercetta, e b = coefficiente angolare.
# abline (reg, col = "red")

# reg0 --> abbiamo visto che tra age e rest_bp non c'è correlazione
# Call:
#   lm(formula = dataset$rest_bp ~ dataset$age)
# 
# Coefficients:
#   (Intercept)  dataset$age  
# 114.3157       0.2495  


# Abbiamo preso altre due variabili per poter continuare la regressione lineare


summary(dataset$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.00   48.00   55.00   54.49   61.00   77.00

summary(dataset$max_hr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 71.0   134.0   152.0   149.5   166.0   202.0 

# Regressione lineare tra max_hr e age
# Importante l'unità  di misura!!!

#  ---- FUNZIONI SENZA TIDYVERSE e GGPLOT------ 
# plot(dataset$age, dataset$max_hr, xlab = "age (year)", ylab= "max_hr (Bpm)")


# invertire l'ordine!

reg0 <- lm(max_hr ~ age, data = dataset)

#  ---- FUNZIONI SENZA TIDYVERSE e GGPLOT------ 
# abline (reg0, col = "red")


# Grafico regressione lineare con Ggplot
  
dataset %>% ggplot(aes(x=age, y=max_hr)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Regressione linare tra il massimo battito cardiaco a riposo e l'età",
       x = "age (y)",
       y = "max heart-rate (BPM)")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), plot.background = element_rect(fill = "#66ff00")) 


#  ---- FUNZIONI SENZA TIDYVERSE e GGPLOT------ 
# segments(dataset$age, fitted(reg0), dataset$age, dataset$max_hr, col = "blue", lty = 2)
# title(main = "Regr.lin tra max_hr e age")

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
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# Residual standard error: 21.16 on 295 degrees of freedom
# Multiple R-squared:  0.1504,	Adjusted R-squared:  0.1475 
# F-statistic: 52.21 on 1 and 295 DF,  p-value: 4.275e-12

# calcolo il coefficiente di correlazione lineare
r0 <- cor(dataset$max_hr, dataset$age)
r0
# potrebbe variare
# -0.3876143

# R^2 = 0.1502449 coefficiente di determinazione
rq0 <- r0^2
rq0
# 0.1502449

r0 <- cov(dataset$max_hr, dataset$age) / (sd(dataset$max_hr) * sd(dataset$age))
r0
r0^2

# Analisi dei residui
#  ---- FUNZIONI SENZA TIDYVERSE e GGPLOT------ 
# plot(reg0$fitted, reg0$residuals, main = "Residui")
# abline(0,0)

reg0 %>% ggplot(aes(x = reg0$fitted, y = reg0$residuals)) +
  geom_point()+ 
  geom_hline(yintercept = 0) +
  labs(title = "Analisi dei residui", 
       x = "regression fitted",
       y = "regression residuals")+
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.background = element_rect(fill = "white")) 


  

# Distribuzione in quantili confrontabile con
# quella di una normale
qqnorm(reg0$residuals)
qqline(reg0$residuals)




# Abbiamo provato a mettere in relazioni altre colonne del dataset
# e abbiamo notato che ci sono correlazioni oltre a quella da noi analizzata

# sembra che ci sia correlazione 
plot(dataset$age, dataset$cholesterol, xlab = "age (year)", ylab = "cholesterol (mg/dl)", main = "Scatterplot tra anni e colesterolo")


# sembra che ci sia correlazione 
plot(dataset$rest_bp, dataset$max_hr, xlab = "rest bp (mm/Hg)", ylab= "max_hr (Bpm)",  main = "Scatterplot tra rest bp e max hr")

# sembra che ci sia correlazione
plot(dataset$cholesterol, dataset$max_hr, xlab = "cholesterol (mg/dl)", ylab= "max_hr (Bpm)",  main = "Scatterplot tra colesterolo e max hr")

plot(dataset$cholesterol, dataset$oldpeak, xlab = "cholesterol (mm/Hg)",  main = "Scatterplot tra colesterolo e old peak")


# sembra che ci sia correlazione
plot(dataset$max_hr, dataset$oldpeak,xlab = "max hr(bpm)", xlab = "old peak",  main = "Scatterplot tra max hr e old peak")



# es 2.4
# Creare un data frame contenente 10 osservazioni (non presenti nel dataset) 
# ed effettuare delle previsioni.

dat <- read.csv("osservazioni.csv", header = TRUE, stringsAsFactors = FALSE) %>% as_tibble()
str (dat)
View(dat)


# Predizione per l'attributo età attravareso reg0

df_pred <- data.frame("age" = dat$age)
predict(reg0, df_pred)
# 1         2         3         4         5         6         7         8         9        10        11 
# 113.17265 128.09188 120.13495 172.84956 137.04341 157.93033 103.22649 155.94110  99.24803  92.28572 160.91418 
# 12        13        14        15        16        17        18        19        20 
# 117.15111 163.89803 116.15649  88.30726 129.08649 174.83880 135.05418 153.95187 123.11880 



# es 1.5 
# Applicare un modello di Machine Learning a scelta, misurandone 
# l'accuratezza sul test set.

# Il seed è un numero casuale per far andare in modo consistente i modelli di machine learning
seed = set.seed(2021)

control <- trainControl(method = "cv", number = 10, seed = seed)


# kNN
fit_knn <- train(age ~ ., data = dataset, method = "knn", trControl = control)
fit_knn

# k-Nearest Neighbors 
# 
# 297 samples
# 14 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 267, 267, 268, 267, 268, 267, ... 
# Resampling results across tuning parameters:
#   
#   k  RMSE      Rsquared   MAE     
# 5  5.104316  0.7183579  4.064807
# 7  5.355917  0.6923200  4.202551
# 9  5.544080  0.6751923  4.321310
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was k = 5.


## MLP
fit_mlp <- train(age ~ ., data = dataset, method = "mlp", trControl = control)
fit_mlp

# Multi-Layer Perceptron 
# 
# 297 samples
# 14 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 268, 267, 267, 267, 267, 267, ... 
# Resampling results across tuning parameters:
#   
#   size  RMSE      Rsquared    MAE      
# 1     18.49229  0.01782076  16.909140
# 3     10.40671         NaN   8.580055
# 5     11.87053  0.03447440   9.962101
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was size = 3.

# RF
fit_rf <- train(age ~ ., data = dataset, method = "rf", trControl = control)
fit_rf

# Random Forest 
# 
# 297 samples
# 14 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 267, 267, 268, 267, 267, 267, ... 
# Resampling results across tuning parameters:
#   
#   mtry  RMSE       Rsquared   MAE      
# 2    5.3067604  0.7894700  3.9510994
# 11    1.5597166  0.9688907  0.7587672
# 20    0.5005757  0.9955183  0.1507403
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was mtry = 20.



results <- resamples(list( knn = fit_knn, mlp = fit_mlp, rf = fit_rf))
summary(results)

# Call:
#   summary.resamples(object = results)
# 
# Models: knn, mlp, rf 
# Number of resamples: 10 
# 
# MAE 
# Min.    1st Qu.    Median      Mean   3rd Qu.       Max. NA's
# knn 3.04000000 3.82166667 4.2066667 4.0648072 4.3650644  4.5866667    0
# mlp 6.85196279 6.96845265 7.4404801 8.5800551 8.5834975 17.2310911    0
# rf  0.03735112 0.07204457 0.1167621 0.1507403 0.1674122  0.3828455    0
# 
# RMSE 
#           Min.   1st Qu.    Median       Mean    3rd Qu.      Max. NA's
# knn 3.97559220 4.6341279 5.2464467  5.1043156  5.6575846  5.742101    0
# mlp 8.26718581 8.4863063 9.6195265 10.4067052 10.4981054 19.041418    0
# rf  0.05771655 0.1706759 0.3428455  0.5005757  0.6794707  1.262975    0
# 
# Rsquared 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# knn 0.6069089 0.6388037 0.7133936 0.7183579 0.7913478 0.8608696    0
# mlp        NA        NA        NA       NaN        NA        NA   10
# rf  0.9816208 0.9943202 0.9985328 0.9955183 0.9996551 0.9999658    0
# 


# Grafico del risultato delle due metodologie di machine learning
dotplot(results, main = "Risultati dei modelli di machine learning")


# es 2.5
# Descrivere brevemente il funzionamento del modello scelto (eseguito sulla descrizione).


