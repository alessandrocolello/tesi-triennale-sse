############################################################################################################
## Alessandro Colello                                                                                     ##
############################################################################################################
# Script per l'analisi dell'accuratezza del macchinario, per il confronto con il righello di precisione e  #
# per l'individuazione delle possibili cause di inaccuratezza dellee misure del macchinario utilizzando la #
# ricetta E.                                                                                               #
############################################################################################################

# Installazione dei pacchetti
if (!"readxl"%in%installed.packages()) install.packages("readxl")
if (!"dplyr"%in%installed.packages()) install.packages("dplyr")
if (!"ggplot2"%in%installed.packages()) install.packages("ggplot2")
if (!"ggpubr"%in%installed.packages()) install.packages("ggpubr")
if (!"moments"%in%installed.packages()) install.packages("moments")
if (!"rootSolve"%in%installed.packages()) install.packages("rootSolve")
if (!"car"%in%installed.packages()) install.packages("car")

# Caricamento dei pacchetti utilizzati
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rootSolve)
library(car)

# Settaggio della working directory
setwd(dir = "") #inserire il percorso della cartella di lavoro


# Funzione per la visualizzazione della distribuzione di densità di un campione
vizDistrib <- function(x, xlabel = deparse(substitute(x)), bins = round(1+3.322*log(length(x)))) {
  label <- paste0("Test di Shapiro-Wilk:\n", 
                  "Statistica test=", round(shapiro.test(x)$statistic, 4), "\n",
                  "p-value=", round(shapiro.test(x)$p.value, 4))
  g1 <- ggplot(mapping = aes(x = x, y = ..density..)) +
    geom_histogram(bins = bins, color = "black", fill = "lightblue") +
    stat_density(alpha = 0.5, color = "black", fill = "lightblue") +
    annotate(geom = "text", label = label, size = 5, x = -Inf, y = Inf, hjust = -0.1, vjust = 1.3) + 
    xlab("") + ylab("Densità") +
    scale_x_continuous(n.breaks = 7) +
    scale_y_continuous(breaks = NULL) +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  g2 <- ggplot(mapping = aes(x = x)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot(fill = "lightblue") +
    scale_x_continuous(limits = layer_scales(g1)$x$range$range, n.breaks = 7) +
    scale_y_continuous(breaks = NULL) +
    xlab(xlabel) + ylab(" ") +
    theme_bw() + 
    theme(axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank(), 
          plot.margin = unit(c(-22.5,5.5,5.5,5.5), "pt"), legend.position = "none")
  ggarrange(g1, g2, nrow = 2, heights = 2:1, vjust = 0)
}
# Funzione di log-verosimiglianza di una popolazione
l <- function(mu, psi, x) {
  -length(x)/2*log(2*pi*(psi-mu^2))-sum((x-mu)^2)/(2*(psi-mu^2))
}
# Funzione di log-verosimiglianza congiunta
lc <- function(param, x1, x2) { #param = (mu1, mu2, psi1, psi2)
  l(mu = param[1], psi = param[3], x = x1)+l(mu = param[2], psi = param[4], x = x2)
}
# Funzione di log-verosimiglianza congiunta sotto H_0
lcH0 <- function(param, x1, x2) { #param = (mu1, mu2, psi)
  lc(param = rep(param, times = c(1,1,2)), x1 = x1, x2 = x2)
}
# Funzione di log-verosimiglianza congiunta sotto H_0 negativa
lcH0.neg <- function(param, x1, x2) { #param = (mu1, mu2, psi)   
  ifelse(param[3]-param[1]^2<=0 | param[3]-param[2]^2<=0 | param[3]<=0, 
         Inf, 
         -lcH0(param = param, x1 = x1, x2 = x2))
}
# Statistica del test del rapporto di verosimiglianza
W <- function(mu, psi, muH0, psiH0, dati) {
  2*(l(mu = mu, psi = psi, x = dati)-l(mu = muH0, psi = psiH0, x = dati))-qchisq(1-alpha, df = 1)
}
# Funzione per la visualizzazione degli strumenti di misura nello spazio della media e della varianza degli scarti
visStrumenti <- function(medie.M, varianze.M, media.R, varianza.R, n.M, n.R, livelli = seq(0.005, 0.6, 0.005)) {
  plot(x = c(medie.M, media.R), y = c(varianze.M*(n.M-1)/n.M, varianza.R*(n.R-1)/n.R), type = "n", xlab = "media", ylab = "varianza")
  l2 <- legend(-5, -5, legend = "", lty = 1, col = "grey", plot = FALSE)
  x <- seq(min(c(medie.M, media.R)), max(c(medie.M, media.R)), 0.001)
  y <- seq(min(c(varianze.M, varianza.R)), max(c(varianze.M, varianza.R)), 0.001)
  z <- outer(x^2, y, "+")
  contour(x, y, z, levels = livelli, add = T, col = "grey")
  text(x = medie.M, y = varianze.M, labels = ricette, col = "darkblue")
  text(x = media.R, y = varianza.R, labels = "R", col = "darkred")
  l1 <- legend("bottomright", legend = c("Righello", "Macchinario"), fill = c("darkred", "darkblue"))
  legend(l1$rect$left, l1$rect$top+l2$rect$h, legend = "Indicatore", lty = 1, col = "grey")
}


# Caricamento dei dati
scarti.macchinario <- read_xlsx(path = "DB Misure.xlsx", sheet = "Scarti Macchinario")
scarti.righello <- read_xlsx(path = "DB Misure.xlsx", sheet = "Scarti Righello") %>% pull(scarti)

# ANALISI DESCRITTIVA
# Sintesi degli scarti del macchinario
smr.M <- scarti.macchinario %>% 
  group_by(ricetta) %>% 
  summarise("minimo" = min(scarti), 
            "1o quartile" = quantile(scarti, 0.25),
            "media" = mean(scarti), 
            "mediana" = median(scarti),
            "3o quartile" = quantile(scarti, 0.75),
            "massimo" = max(scarti),
            "deviazione std" = sd(scarti))
View(smr.R)

# Sintesi degli scarti del righello
smr.R <- data.frame("minimo" = min(scarti.righello), 
                    "1o quartile" = quantile(scarti.righello, 0.25),
                    "media" = mean(scarti.righello), 
                    "mediana" = median(scarti.righello),
                    "3o quartile" = quantile(scarti.righello, 0.75),
                    "massimo" = max(scarti.righello),
                    "deviazione std" = sd(scarti.righello))
rownames(smr.R) <- "scarti righello"
View(smr.R)

ricette <- sort(unique(scarti.macchinario$ricetta)) #lista delle ricette 

# Istogrammi, boxplot e test di Shapiro-Wilk per la normalità degli scarti del macchinario per ogni ricetta
vizDistrib(scarti.macchinario %>% filter(ricetta=="A") %>% pull(scarti), xlabel = "Scarti del macchinario con ricetta A", bins = 10)
vizDistrib(scarti.macchinario %>% filter(ricetta=="B") %>% pull(scarti), xlabel = "Scarti del macchinario con ricetta B", bins = 12)
vizDistrib(scarti.macchinario %>% filter(ricetta=="C") %>% pull(scarti), xlabel = "Scarti del macchinario con ricetta C", bins = 8)
vizDistrib(scarti.macchinario %>% filter(ricetta=="D") %>% pull(scarti), xlabel = "Scarti del macchinario con ricetta D", bins = 5)
vizDistrib(scarti.macchinario %>% filter(ricetta=="E") %>% pull(scarti), xlabel = "Scarti del macchinario con ricetta E", bins = 12)
vizDistrib(scarti.macchinario %>% filter(ricetta=="F") %>% pull(scarti), xlabel = "Scarti del macchinario con ricetta F", bins = 13)

# Normal QQ-plot degli scarti dello strumento con ricetta C
qqnorm(scarti.macchinario %>% filter(ricetta=="C") %>% pull(scarti)); qqline(scarti.macchinario %>% filter(ricetta=="C") %>% pull(scarti))

# Normal QQ-plot degli scarti dello strumento con ricetta E
qqnorm(scarti.macchinario %>% filter(ricetta=="E") %>% pull(scarti)); qqline(scarti.macchinario %>% filter(ricetta=="E") %>% pull(scarti))

# Normal QQ-plot degli scarti dello strumento con ricetta F
qqnorm(scarti.macchinario %>% filter(ricetta=="F") %>% pull(scarti)); qqline(scarti.macchinario %>% filter(ricetta=="F") %>% pull(scarti))

# Istogrammi, boxplot e test di Shapiro-Wilk per la normalità degli scarti del righello
vizDistrib(x = scarti.righello, xlabel = "Scarti del righello", bins = 15)

# Normal Q-Q plot degli scarti del righello
qqnorm(scarti.righello, main = "Normal Q-Q plot per gli scarti del righello"); qqline(scarti.righello)



# INFERENZA
# Numerosità campionarie degli scarti del macchinario per ogni ricetta
n.M <- scarti.macchinario %>% 
  group_by(ricetta) %>% 
  summarise(n = n()) %>% 
  pull(var = n, name = ricetta)

# Numerosità campionaria degli scarti del righello
n.R <- length(scarti.righello)

# Stime puntuali
# Medie campionarie degli scarti del macchinario per ogni ricetta
medie.M <- scarti.macchinario %>% 
  group_by(ricetta) %>% 
  summarise(media = mean(scarti)) %>%
  pull(var = media, name = ricetta)

# Varianze campionarie corrette degli scarti del macchinario per ogni ricetta
varianze.M <- scarti.macchinario %>% 
  group_by(ricetta) %>% 
  summarise(varianza = var(scarti)) %>%
  pull(var = varianza, name = ricetta)

# Media campionaria degli scarti del righello
media.R <- mean(scarti.righello)

# Varianza campionaria corretta degli scarti del righello
varianza.R <- var(scarti.righello)


# Stime intervallari
alpha <- 0.05 #livello di significatività
# Intervalli di confidenza per le medie degli scarti del macchinario per ogni ricetta
ic.medie.M <- matrix(medie.M+c(qt(p = alpha/2, df = n.M-1), qt(p = 1-alpha/2, df = n.M-1))*sqrt(varianze.M/(n.M-1)), 
                     ncol = 2, byrow = FALSE)
colnames(ic.medie.M) <- c("L1", "L2")
rownames(ic.medie.M) <- ricette

# Intervalli di confidenza per le medie degli scarti del macchinario per ogni ricetta
ic.varianze.M <- matrix((n.M-1)*varianze.M/c(qchisq(1-alpha/2, n.M-1), qchisq(alpha/2, n.M-1)), 
                        ncol = 2, byrow = FALSE)
colnames(ic.varianze.M) <- c("L1", "L2")
rownames(ic.varianze.M) <- ricette


# Intervalli di confidenza per la media degli scarti del righello
ic.media.R <- matrix(media.R+c(qt(p = alpha/2, df = n.R-1), qt(p = 1-alpha/2, df = n.R-1))*sqrt(varianza.R/(n.R-1)),
                     nrow = 1, dimnames = list("R", c("L1", "L2")))
# Intervalli di confidenza per la varianza degli scarti del righello
ic.varianza.R <- (n.R-1)*varianza.R/c(qchisq(1-alpha/2, n.R-1), qchisq(alpha/2, n.R-1))


# Visualizzazione degli strumenti nello spazio della media e della varianza degli scarti
visStrumenti(medie.M, varianze.M, media.R, varianza.R, n.M, n.R)





# One sample T test per la verifica dell'ipotesi di media nulla degli scarti del righello e del 
# macchinario con ogni ricetta
scarti.macchinario %>% 
  group_by(ricetta) %>% 
  summarise("Statistica test" = t.test(scarti)$statistic, 
            "gdl" = t.test(scarti)$parameter, 
            "p-value" = t.test(scarti)$p.value)

data.frame("Statistica test" = t.test(scarti.righello)$statistic, 
           "gdl" = t.test(scarti.righello)$parameter, 
           "p-value" = t.test(scarti.righello)$p.value)

# F test per la verifica dell'ipotesi di uguaglianza tra la varianza degli scarti del righello e 
# la varianza degli scarti del macchinario per ogni ricetta
scarti.macchinario %>% 
  group_by(ricetta) %>% 
  summarise("Statistica test" = var.test(x = scarti, y = scarti.righello)$statistic,
            "gdl" = paste(var.test(x = scarti, y = scarti.righello)$parameter, collapse = ", "),
            "p-value" = var.test(x = scarti, y = scarti.righello, alternative = "less")$p.value)

# INDICATORI
# Stima puntuale
indicatori.M <- medie.M^2+varianze.M*(n.M-1)/n.M
indicatore.R <- media.R^2+varianza.R*(n.R-1)/n.R

# IC ALLA WALD
# Intervalli di confidenza per gli indicatori del macchinario per ogni ricetta
se.indicatori.M <- sqrt(2*(indicatori.M-medie.M^2)^2/n.M)
ic.w.indicatori.M <- indicatori.M+matrix(c(qnorm(alpha/2), qnorm(1-alpha/2)), 
                                       nrow = length(ricette), ncol = 2, byrow = T, dimnames = list(ricette, c("L1", "L2")))*se.indicatori.M
# Intervallo di confidenza per l'indicatore del righello
se.indicatore.R <- sqrt(2*(indicatore.R-media.R^2)^2/n.R)
ic.w.indicatore.R <- indicatore.R+c(qnorm(alpha/2), qnorm(1-alpha/2))*se.indicatore.R


# INTERVALLI DI CONFIDENZA BASATI SUL RAPPORTO DI VEROSIMIGLIANZA
# Intervalli di confidenza per gli indicatori del macchinario per ogni ricetta
param <- sapply(ricette, 
                function(r) nlminb(start = c(medie.M[r], indicatori.M[r]), 
                                   objective = function(param, x) -l(mu = param[1], psi = param[2], 
                                                                     x = scarti.macchinario %>% 
                                                                       filter(ricetta==r) %>% 
                                                                       pull(scarti)))$par)
rownames(param) <- c("media", "indicatore")

ic.lr.indicatori.M <- sapply(ricette, 
                          function(r) uniroot.all(f = function(mu, psi, muH0, x, dati) W(mu = mu, psi = psi, muH0 = muH0, psiH0 = x, dati = dati),
                                                  interval = c(0, 1), n = 10^5, tol = .Machine$double.eps, 
                                                  mu = medie.M[r], psi = indicatori.M[r], muH0 = param["media", r],
                                                  dati = scarti.macchinario %>% filter(ricetta==r) %>% pull(scarti))) %>% suppressWarnings() %>% t()
colnames(ic.lr.indicatori.M) <- c("L1", "L2")

# Intervallo di confidenza per l'indicatore degli scarti del righello
param <- nlminb(start = c(media.R, indicatore.R), 
              objective = function(param, x) -l(mu = param[1], psi = param[2], x = x),
              x = scarti.righello)$par %>% suppressWarnings()
ic.lr.indicatore.R <- uniroot.all(function(mu, psi, muH0, x, dati) W(mu = mu, psi = psi, muH0 = muH0, psiH0 = x, dati = dati),
                               interval = c(0, 1), n = 10^5, tol = .Machine$double.eps,
                               mu = media.R, psi = indicatore.R, muH0 = param[1], dati = scarti.righello)



# VERIFICA DI IPOTESI SULL'INDICATORE
# H_0:"uguaglianza tra gli indicatori del righello e del macchinario con ricetta considerata"
testConfrontoIndicatori <- function(mu1, #stima della media della prima popolazione
                                    mu2, #stima della media della seconda popolazione
                                    psi1, #stima dell'indicatore della prima popolazione
                                    psi2, #stima dell'indicatore della seconda popolazione
                                    x1, #vettore contenente il primo campione
                                    x2, #vettore contenente il secondo campione
                                    alpha = 0.05 #livello di significatività del test
) {
  if (is.null(mu1) | is.null(mu2) | is.null(psi1) | is.null(psi2) | is.null(x1) | is.null(x2)) return(cat("Inserire tutti i parametri"))
  if (length(x1)<2 | length(x2)<2) return(cat("I campioni devono avere numerosità >= 2"))
  # Test di Wald
  z <- (psi1-psi2)/sqrt(2*(psi1-mu1^2)^2/length(x1)+2*(psi2-mu2^2)^2/length(x2)) # valore osservato della statistica test
  p.W <- 2*(1-pnorm(abs(z))) #p-value
  #esito.W <- paste0(ifelse(p.W<alpha, "Test significativo al ", "Test non significativo al "), alpha*100, "%") #decisione
  # LR test
  param <- nlminb(start = c(mu1, mu2, max(psi1, psi2)), 
                objective = lcH0.neg, x1 = x1, x2 = x2, 
                lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, Inf))$par
  W <- 2*(lc(param = c(mu1, mu2, psi1, psi2), x1 = x1, x2 = x2)-lcH0(param = param, x1 = x1, x2 = x2)) #valore osservato della statistica test
  p.LR <- pchisq(W, df = 1, lower.tail = FALSE) #p-value
  return(list("test di Wald" = unname(p.W), "test LR" = unname(p.LR)))
}

confronto.test <- sapply(ricette, 
                         function(r) testConfrontoIndicatori(mu1 = medie.M[r], mu2 = media.R, 
                                                             psi1 = indicatori.M[r], psi2 = indicatore.R, 
                                                             x1 = scarti.macchinario %>% filter(ricetta==r) %>% pull(scarti),
                                                             x2 = scarti.righello))

# Confronto unilaterale degli indicatori degli strumenti
# H_0:"il righello è accurato almeno quanto il macchinario con specifica ricetta"
z <- (indicatori.M-indicatore.R)/sqrt(2*(indicatori.M-medie.M^2)^2/n.M+2*(indicatore.R-media.R^2)^2/n.R) #valore osservato della statistica test
p <- pnorm(z) #p-value
p<0.05




# STUDIO DELLE RELAZIONE DEGLI SCARTI DEL MACCHINARIO CON ALCUNE VARIABILI
# Caricamento del dataset
dataset <- read_xlsx(path = "DB Misure.xlsx", sheet = "Dataset") %>% 
  mutate(ricetta = factor(ricetta))

# MODELLO PER LA RICETTA "E"
# Creazione delle variabili
scarto <- dataset %>% filter(ricetta=="E") %>% pull(scarto)
altezza <- dataset %>% filter(ricetta=="E") %>% pull(altezza)
spessore <- dataset %>% filter(ricetta=="E") %>% pull(spessore)
fili.per.cm <- dataset %>% filter(ricetta=="E") %>% pull(fili.per.cm)
apertura.maglia <- dataset %>% filter(ricetta=="E") %>% pull(apertura.maglia)

# Matrice di correlazione
round(cor(cbind(scarto, altezza, spessore, fili.per.cm, apertura.maglia)), digits = 4)

# Scarti vs altezza
plot(formula = scarto~altezza, xlab = "Altezza", ylab = "Scarti")

# Scarti vs fili.per.cm
plot(formula = scarto~fili.per.cm, xlab = "Fili per cm", ylab = "Scarti")

# Stima del modello
mod <- lm(formula = scarto~altezza+fili.per.cm)
summary(mod)

# Grafici per la diagnostica di problemi nella validità delle assunzioni del modello
par(mfrow = c(2, 2)); plot(mod); par(mfrow = c(1, 1))

e <- mod$residuals #residui del modello d'interesse

# Scatterplot Varianza vs Altezza
plot(e^2~altezza)

# Scatterplot Varianza vs Fili.per.cm
plot(e^2~fili.per.cm)

# Trattamento dell'eteroschedasticità
# Stima del modello di Breusch-Pagan e test 
mod.bp <- lm(formula = e^2~altezza+fili.per.cm)
summary(mod.bp)
BP <- length(e)*summary(mod.bp)$r.squared
df.bp <- length(mod.bp$coefficients)-1
p.bp <- pchisq(BP, df = df.bp, lower.tail = FALSE)

# Stima del modello di Harvey e test 
mod.h <- lm(log(e^2)~altezza+fili.per.cm)
summary(mod.h)
H <- length(e)*summary(mod.h)$r.squared
df.h <- length(mod.h$coefficients)-1
p.h <- pchisq(H, df = length(mod.h$coefficients)-1, lower.tail = FALSE)

View(data.frame("Statistica test" = c(BP, H), 
                "Gradi di libertà" = c(df.bp, df.h), 
                "p-value" = c(p.bp, p.h), 
                row.names = c("Modello di Breusch-Pagan", "Modello di Harvey")))

# Interpretazione dei coefficienti del modello di Harvey
summary(mod.h)[[4]]
# Solo l'altezza delle tirelle misurate è significativa per spiegare la varianza degli scarti.
# All'aumentare dell'altezza della tirella misurata, aumenta la variabilità degli scarti.

# MODELLO TRASFORMATO secondo il modello di Harvey
sigma <- sqrt(exp(mod.h$fitted.values)) #valori fittati del modello di Harvey

# Trasformazione delle variabili
Scarto <- scarto/sigma
Intercetta <- 1/sigma
Altezza <- altezza/sigma
Fili.per.cm <- fili.per.cm/sigma

mod.t.h <- lm(formula = Scarto~0+Intercetta+Altezza+Fili.per.cm) #stima del modello trasformato
summary(mod.t.h)
par(mfrow=c(2,2)); plot(mod.t.h); par(mfrow=c(1,1)) #diagnostica

outlierTest(mod.t.h) #test per l'identificazione di outlier

mod.t.h <- lm(formula = Scarto~0+Intercetta+Altezza+Fili.per.cm, subset = -3) #ristima del modello trasformato
summary(mod.t.h)
par(mfrow=c(2,2)); plot(mod.t.h); par(mfrow=c(1,1)) #diagnostica

# Interpretazione dei coefficienti del modello trasformato
# Tutti i coefficienti perdono di significatività.
# Non c'è evidenza empirica che supporta l'ipotesi iniziale che altezza e numero di fili per cm influenzino
# in modo statisticamente significativo gli scarti relativi allo strumento digitale con ricetta E.
