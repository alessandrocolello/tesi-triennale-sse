###########################################################################################################
# Alessandro Colello                                                                                      #
###########################################################################################################
# Script per la manipolazione dei dati salvati sul workbook Dati.xlsx e la creazione dei worksheet degli  #
# scarti del macchinario e del righello e del dataset con possibili cause di inaccuratezzza delle misure. #
###########################################################################################################

# Installazione dei pacchetti
if (!"dplyr"%in%installed.packages()) install.packages("dplyr")
if (!"readxl"%in%installed.packages()) install.packages("readxl")
if (!"tidyr"%in%installed.packages()) install.packages("tidyr")
if (!"openxlsx"%in%installed.packages()) install.packages("openxlsx")

# Caricamento delle librerie utilizzate
library(dplyr)
library(readxl)
library(tidyr)
library(openxlsx)

setwd(dir = "") #inserire percorso della cartella di lavoro


# CREAZIONE WORKSHEET 'Scarti Macchinario'

# Importazione del worksheet 'Ordini'
ordini <- read_xlsx(path = "DB Misure.xlsx", sheet = "Ordini")

# Importazione delle misure del macchinario
misure.macchinario <- read_xlsx(path = "DB Misure.xlsx", sheet = "Misure Macchinario") %>%
  gather(key = "posizione", value = "mis.macc", inizio, fine) %>% 
  filter(complete.cases(mis.macc))

# Importazione delle misure del laboratorio
misure.laboratorio <- read_xlsx(path = "DB Misure.xlsx", sheet = "Misure Laboratorio") %>% 
  gather(key = "posizione", value = "mis.lab", inizio, fine) %>% 
  group_by(ordine, tirella, posizione) %>% 
  summarise(mis.lab = mean(mis.lab), .groups = "keep")

# Calcolo degli scarti del macchinario
scarti.macchinario <- misure.macchinario %>% 
  left_join(y = misure.laboratorio, 
            by = c("ordine", "tirella", "posizione")) %>% 
  transmute(ordine, scarti = mis.macc-mis.lab)

# Associazione della ricetta allo scarto
scarti <- scarti.macchinario %>% 
  left_join(y = ordini, by = "ordine") %>% 
  transmute(scarti, ricetta)

# Scrittura degli scarti nel worksheet
wb <- loadWorkbook(file = "DB Misure.xlsx")
if (!("Scarti Macchinario"%in%names(wb))) addWorksheet(wb = wb, sheetName = "Scarti Macchinario")
writeData(wb = wb, x = scarti, sheet = "Scarti Macchinario")
saveWorkbook(wb = wb, file = "DB Misure.xlsx", overwrite = TRUE)


# CREAZIONE WORKSHEET 'Scarti Righello'

# Importazione delle misure del righello
misure.righello <- read_xlsx(path = "DB Misure.xlsx", sheet = "Misure Righello") %>% 
  gather(key = "posizione", value = "mis.rig", inizio, fine) %>% 
  filter(complete.cases(mis.rig))

# Calcolo degli scarti del righello
scarti.righello <- misure.righello %>% 
  left_join(y = misure.laboratorio, 
            by = c("ordine", "tirella", "posizione")) %>% 
  transmute(scarti = mis.rig-mis.lab)

# Scrittura degli scarti nel worksheet
wb <- loadWorkbook(file = "DB Misure.xlsx")
if (!("Scarti Righello"%in%names(wb))) addWorksheet(wb = wb, sheetName = "Scarti Righello")
writeData(wb = wb, x = scarti.righello, sheet = "Scarti Righello")
saveWorkbook(wb = wb, file = "DB Misure.xlsx", overwrite = TRUE)


# CREAZIONE WORKSHEET 'dataset'

# Importazione del worksheet 'Ordine-Partita'
ordine.partita <- read_xlsx(path = "DB Misure.xlsx", sheet = "Ordine-Partita")

# Importazione del worksheet 'Partita-Spessore'
partita.spessore <- read_xlsx(path = "DB Misure.xlsx", sheet = "Partita-Spessore") 

# Join dei worksheet 'Ordini' e 'Partita-Spessore'
ordine.variabili <- left_join(x = partita.spessore, y = ordine.partita, by = "partita") %>% 
  group_by(ordine) %>% 
  summarise(spessore = unique(spessore)[which.max(tabulate(match(spessore, unique(spessore))))]) %>% 
  right_join(y = ordini, by = "ordine") %>% 
  transmute(ordine, altezza, spessore, fili.per.cm, apertura.maglia, ricetta)
  
# Creazione del dataset
dataset <- left_join(x = scarti.macchinario, y = ordine.variabili, by = "ordine") %>% 
  select(scarto = scarti, altezza, spessore, fili.per.cm, apertura.maglia, ricetta)

# Scrittura nel worksheet
if (!("Dataset"%in%names(wb))) addWorksheet(wb = wb, sheetName = "Dataset")
writeData(wb = wb, x = dataset, sheet = "Dataset")
saveWorkbook(wb, file = "DB Misure.xlsx", overwrite = TRUE)






