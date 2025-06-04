# Chargement de l'espace de travail
setwd("C:/Users/Alexandre_Dosset/Desktop/Antoptic")

#===============
# I - Importation des packages ===============
#===============

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(stringr)
library(data.table)
library(readxl)
library(xlsx)
library(bipartite)
library(vegan)
library(broom)
library(reshape2)
library(FactoMineR)
library(factoextra)

#===============
# II - Importation des donnees ===============
#===============

  # Table des probabilités d'intéractions entre proies et prédateurs (Next-generation sequencing)
tab_pij2 <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/tab_pij2.txt", header = TRUE, sep = "")
tab_pij2 <- tab_pij2 %>% dplyr::filter(ReadName != "Malacostraca_Potamidae_Longpotamon") # Retirer une espèce de crustacés qui est un artefact
    # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs spécifiques dans "Isp"
    tab_pij2_All_species <- tab_pij2
    # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs "PijComb" > O.O1
    tab_pij2_All_species_Seuil_1_percent <- tab_pij2_All_species %>% filter(PijComb > 0.01)
    tab_pij2_All_species_Seuil_1_percent$FS <- substr(tab_pij2_All_species_Seuil_1_percent$Parc, nchar(tab_pij2_All_species_Seuil_1_percent$Parc), nchar(tab_pij2_All_species_Seuil_1_percent$Parc))
    
  # Table des pratiques utilisées par parcelles et de description du paysage
Data_IFT <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/pratiques_paysages_2015.txt", header = TRUE, sep = "")
  
  # Table des occurences, fréquences et abondances des ravageurs par parcelles et de description du paysage
CR_Paysage <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/CR_Paysage.txt", header = TRUE, sep = "")

  # Table des pratiques utilisées par parcelles et de prédation des oeufs sur les cartes de prédation
Data_eggs <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/data_eggs.txt", header = TRUE, sep = "", fill = TRUE)


#===============
# III - Fonction pour calculer l'indice de chevauchement de Pianka ===============
# https://rdrr.io/cran/EcoSimR/man/pianka.html
#===============

pianka_index <- function(matrix) {
  species <- nrow(matrix)
  pianka_matrix <- matrix(NA, nrow = species, ncol = species)
  rownames(pianka_matrix) <- rownames(matrix)
  colnames(pianka_matrix) <- rownames(matrix)
  
  for (j in 1:species) {
    for (k in 1:species) {
      if (j != k) {
        pj <- matrix[j, ] / sum(matrix[j, ])
        pk <- matrix[k, ] / sum(matrix[k, ])
        
        numerator <- sum(pj * pk)
        denominator <- sqrt(sum(pj^2) * sum(pk^2))
        
        pianka_matrix[j, k] <- numerator / denominator
      }
    }
  }
  return(pianka_matrix)
}

#===============
# IV - Création de la matrice d'interactions et calcul des métriques du réseau ===============
#===============

library(dplyr)

# Obtenir toutes les valeurs uniques de la colonne Parc
parc_values <- unique(tab_pij2_All_species_Seuil_1_percent$Parc)

# Fonction pour filtrer la table en fonction du parc
filter_by_parc <- function(parc) {
  tab_pij2_filtered <- tab_pij2_All_species_Seuil_1_percent %>%
    filter(Parc == parc)
  
  return(tab_pij2_filtered)
}

# Créer des variables dynamiques pour chaque valeur de Parc
for (parc in parc_values) {
  # Créer un nom de variable dynamique
  var_name <- paste0("tab_pij2_filtered_", parc)
  
  # Assigner le tableau filtré à la variable dynamique
  assign(var_name, filter_by_parc(parc))
}



# Liste des tableaux d'interaction
tableaux_interaction <- list("1088B" = tab_pij2_filtered_1088B, 
                             "1088C" = tab_pij2_filtered_1088C,
                             "1435B" = tab_pij2_filtered_1435B,
                             "1435C" = tab_pij2_filtered_1435C,
                             "1650B" = tab_pij2_filtered_1650B,
                             "1650C" = tab_pij2_filtered_1650C,
                             "1662B" = tab_pij2_filtered_1662B,
                             "1662C" = tab_pij2_filtered_1662C,
                             "1868B" = tab_pij2_filtered_1868B,
                             "1868C" = tab_pij2_filtered_1868C)


# Fonction pour effectuer le bootstrap et calculer la moyenne de l'indice de Pianka
bootstrap_matrix <- function(data, n_bootstrap = 1000, sample_size = 20) {
  # Initialiser un vecteur pour stocker les valeurs de Pianka
  pianka_values <- numeric(n_bootstrap)
  
  for (b in 1:n_bootstrap) {
    # Rééchantillonnage bootstrap des données
    bootstrap_data <- data %>%
      group_by(Isp) %>%
      sample_n(size = min(sample_size, n()), replace = TRUE) %>%
      ungroup()
    
    # Créer la matrice d'interaction entre les prédateurs et les proies
    interaction_matrix <- dcast(bootstrap_data, Isp ~ ReadName, value.var = "PijComb", fun.aggregate = mean, fill = 0)
    
    # Convertir la matrice en un format adapté à pianka()
    interaction_matrix_data <- as.matrix(interaction_matrix[, -1])  # Retirer la colonne des noms de prédateurs (Isp)
    rownames(interaction_matrix_data) <- interaction_matrix$Isp  # Nommer les lignes par les prédateurs
    
    # Utiliser la fonction pianka() pour calculer l'indice de Pianka
    pianka_matrix <- EcoSimR::pianka(interaction_matrix_data)
    
    # Calculer la moyenne des indices de Pianka (uniquement la partie inférieure de la matrice)
    pianka_values[b] <- mean(pianka_matrix)
  }
  
  # Retourner la moyenne des valeurs de Pianka obtenues via le bootstrap
  return(mean(pianka_values))
}



# Liste pour stocker les résultats
pianka_results <- list()

# Boucle sur chaque tableau d'interaction
for (nom_tableau in names(tableaux_interaction)) {
  # Récupérer le tableau actuel
  tab_pij2_filtered <- tableaux_interaction[[nom_tableau]]
  
  # Calculer la moyenne de l'indice de Pianka avec bootstrap
  mean_pianka <- bootstrap_matrix(tab_pij2_filtered, n_bootstrap = 1000, sample_size = 20)
  
  # Stocker le résultat dans la liste avec le nom du tableau d'interaction
  pianka_results[[nom_tableau]] <- mean_pianka
}

# Convertir la liste des résultats en tableau (data frame)
pianka_df <- data.frame(Parc = names(pianka_results), 
                        Pianka_index = unlist(pianka_results))

# Afficher le tableau final avec les résultats de Pianka
print(pianka_df)


#===============
# V - Création du jeu de données pour l'ACP ===============
#===============

Data_IFT <- rename(Data_IFT, Parc = parc)
Data_IFT$IFT_Ins_Fon <- Data_IFT$IFTIns + Data_IFT$IFTFon

# Fusionner les deux tables par la colonne 'Parc'
CR_Paysage <- rename(CR_Paysage, Parc = parc)

data_merged <- merge(tab_pij2_All_species_Seuil_1_percent, CR_Paysage, by = "Parc")
data_merged <- merge(data_merged, pianka_df, by = "Parc")

data_ACP <- merge(data_merged[, c("Parc", "FS", "AC", "AB", "HSNtot", "Pianka_index")], 
                  Data_IFT[, c("Parc", "IFTHer", "IFTIns", "IFTFon", "IFT_Ins_Fon", "surf", "Int_ti")], by = c("Parc"))


#===============
# VI - ACP ===============
#===============

head(data_ACP)

#scale data

data_ACP_1 <- data_ACP %>%
  select(HSNtot, IFTHer, IFTIns, IFTFon, Int_ti, Pianka_index)

data_ACP_2 <- data_ACP_1
data_ACP_2$IFTTot <- data_ACP_2$IFTHer + data_ACP_2$IFTIns + data_ACP_2$IFTFon

data_ACP_2 <- data_ACP_2 %>%
  select(HSNtot, IFTTot, Int_ti)


dataPCAscale<-scale(data_ACP_2) 

resPCA<- PCA(dataPCAscale, scale.unit=TRUE)

fviz_eig(resPCA,add.labels=T)
#fviz_pca_ind(resPCA,repel=T,col.ind="cos2")

#extraction des deux axes : local land use intensity and landscape land use intensity

IFTLUI<-resPCA$ind$coord[,2]
LandscapeLUI<-resPCA$ind$coord[,1]


#add data LUIlocal and Landscape to data
data_ACP_2 <- data_ACP_2 %>%
  mutate(data_ACP_1, IFTLUI, LandscapeLUI)

data_ACP_2$Parc <- data_ACP$Parc

head(data_ACP_2)


data_ACP_2 <- data_ACP_2 %>%
  distinct(Parc, .keep_all = TRUE)

#===============
# VII - Graphs descriptifs (Shannon Div Pred, Predator Abundance, Shannon Div Prey) ===============
#===============

# Calcul de l'indice de diversité de Shannon des prédateurs

Tab_natural_enemies <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/SOLUTION_data_naturalenemy_landscape_2015.txt", header = TRUE, sep = "\t")

Tab_natural_enemies$Genus_species <- paste(Tab_natural_enemies$genus, Tab_natural_enemies$species, sep = "_")

Tab_natural_enemies <- rename(Tab_natural_enemies, Parc = vineyard)

Tab_natural_enemies <- Tab_natural_enemies %>%
  filter(sampling_date %in% c(2, 3))

#Regrouper les données pour avoir une matrice d'abondance
table_pred_abondance <- Tab_natural_enemies %>%
  group_by(Parc, Genus_species) %>%
  summarise(Pred_Abundance = sum(N), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Genus_species, values_from = Pred_Abundance, values_fill = 0)

#Calculer l'indice de Shannon pour chaque Parc
tab_pred_shannon_div <- table_pred_abondance %>%
  column_to_rownames(var = "Parc") %>%
  vegan::diversity(index = "shannon") %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Parc")
colnames(tab_pred_shannon_div)[2] <- "Predator_Shannon_Div"


tab_pred_shannon_div <- tab_pred_shannon_div %>%
  filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))

tab_RS <- table_pred_abondance
tab_RS <- tab_RS %>%
  column_to_rownames(var= "Parc")

#Calculer Species Richness pour chaque Parc
tab_pred_RS <- tab_RS %>%
  vegan::specnumber() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Parc")
colnames(tab_pred_RS)[2] <- "Predator_Species_Richness"

tab_pred_RS <- tab_pred_RS %>%
  filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))

data_ACP_2 <- data_ACP_2 %>%
  mutate(tab_pred_shannon_div, tab_pred_RS)

ggplot(data_ACP_2, aes(x = Predator_Shannon_Div, y = Predator_Species_Richness, color = Parc)) +
  geom_point(size = 3) +
  geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
  labs(x = "Predator_Shannon_Div", y = "Predator_Species_Richness")


# Somme abondances totale par parcelles

table_Abondances_totale <- table_pred_abondance %>%
  filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))

table_Abondances_totale <- table_Abondances_totale %>%
rowwise()%>%
  mutate(Abondance_tot = sum(c_across(where(is.numeric))))
          
table_Abondances_totale <- table_Abondances_totale %>%
  select(Parc, Abondance_tot)

data_ACP_2 <- data_ACP_2 %>%
  mutate(table_Abondances_totale)

ggplot(data_ACP_2, aes(x = Predator_Shannon_Div, y = Abondance_tot, color = Parc)) +
  geom_point(size = 3) +
  geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
  labs(x = "Predator_Shannon_Div", y = "Abondance_tot")


# Calcul de l'indice de diversité de Shannon des proies

tab_pij2_All_species_Seuil_1_percent$N <- 1
#Regrouper les données pour avoir une matrice d'abondance
table_prey_abondance <- tab_pij2_All_species_Seuil_1_percent %>%
  group_by(Parc, ReadName) %>%
  summarise(Prey_Abundance = sum(N), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = ReadName, values_from = Prey_Abundance, values_fill = 0)

#Calculer l'indice de Shannon pour chaque Parc
tab_prey_shannon_div <- table_prey_abondance %>%
  column_to_rownames(var = "Parc") %>%
  vegan::diversity(index = "shannon") %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Parc")
colnames(tab_prey_shannon_div)[2] <- "Prey_Shannon_Div"

data_ACP_2 <- data_ACP_2 %>%
  mutate(tab_prey_shannon_div)


#===============
# VIII - Réseaux par Parc et métriques de réseau ===============
#===============

# Réseaux trophiques par parcelle
# Extraire les valeurs uniques de la colonne 'Parc'
parc_values <- unique(tab_pij2_All_species_Seuil_1_percent$Parc)

# Générer une palette de couleurs, par exemple avec RColorBrewer (ou d'autres)
colors <- grDevices::rainbow(length(parc_values))  # Utiliser un ensemble de couleurs distinctes

# Initialiser une liste pour stocker les dataframes de metriques
list_metriques <- list()

# Boucle pour créer un tableau, une matrice et un graphique pour chaque Parc
for (i in seq_along(parc_values)) {
  
  # Filtrer les données pour un parc spécifique
  parc <- parc_values[i]
  subset_data <- tab_pij2_All_species_Seuil_1_percent[tab_pij2_All_species_Seuil_1_percent$Parc == parc, ]
  
  # Transformer le data frame en matrice d'interaction pour ce parc
  interaction_matrix_All_species <- reshape2::dcast(subset_data, ReadName ~ Isp, value.var = "PijComb", fun.aggregate = sum, fill = 0)
  
  # Mettre les noms de lignes et de colonnes
  rownames(interaction_matrix_All_species) <- interaction_matrix_All_species[, 1]
  interaction_matrix_All_species <- as.matrix(interaction_matrix_All_species[, -1])
  
  # Trier les lignes par ordre alphabétique des noms de lignes
  interaction_matrix_All_species <- interaction_matrix_All_species[order(rownames(interaction_matrix_All_species)), ]
  
  # Créer une variable dynamique avec le nom du parc
  var_name <- paste0("interaction_matrix_All_species_", parc)
  
  # Assigner la matrice à une variable avec ce nom
  assign(var_name, interaction_matrix_All_species)
  
  # Calculer les metriques de réseau
  metriques <- as.data.frame(networklevel(interaction_matrix_All_species))
  # Stocker les metriques dans la liste
  list_metriques[[parc]] <- metriques
  
  # Paramètres graphiques pour réduire la taille du graphique
  op <- par(mar = c(0.1, 2, 0.1, 2) + 0.1, cex = 0.8)
  
  # Générer le graphique plotweb avec des liens plus visibles (en utilisant une couleur différente pour chaque parc)
  plotweb(interaction_matrix_All_species, method = "normal",
          arrow = "down",
          text.rot = 90, col.low = "grey",
          bor.col.interaction = "black",  # Bordure autour des interactions
          col.interaction = colors[i])  # Couleur des liens pour chaque parc spécifique
  
  # Ajouter un titre spécifique pour chaque parc
  title(main = paste("Réseau trophique pour Parc", parc))
  
  # Réinitialiser les paramètres graphiques
  par(op)
}

# Combiner tous les dataframes de metriques en un seul dataframe si souhaité
final_metriques <- do.call(cbind, list_metriques)

# Renommer les colonnes avec le nom de la parcelle suivi d'un underscore
colnames(final_metriques) <- paste0(parc_values, "_", colnames(final_metriques))
final_metriques$metriques <- rownames(final_metriques)

writexl::write_xlsx(final_metriques, "final_metriques_par_parcelles.xlsx")



metriques_reseau_All_species_Parc <- final_metriques[rownames(final_metriques) %in% c("vulnerability.LL", "connectance", "nestedness", "generality.HL", "niche.overlap.HL", "niche.overlap.LL", "modularity Q", "H2"), ]

metriques_reseau_All_species_Parc <- rename(metriques_reseau_All_species_Parc, "1088B" = "1088B_networklevel(interaction_matrix_All_species)",
                                            "1088C" = "1088C_networklevel(interaction_matrix_All_species)",
                                            "1435B" = "1435B_networklevel(interaction_matrix_All_species)",
                                            "1435C" = "1435C_networklevel(interaction_matrix_All_species)",
                                            "1650B" = "1650B_networklevel(interaction_matrix_All_species)",
                                            "1650C" = "1650C_networklevel(interaction_matrix_All_species)",
                                            "1662B" = "1662B_networklevel(interaction_matrix_All_species)",
                                            "1662C" = "1662C_networklevel(interaction_matrix_All_species)",
                                            "1868B" = "1868B_networklevel(interaction_matrix_All_species)",
                                            "1868C" = "1868C_networklevel(interaction_matrix_All_species)",)


t_metrics_Parc <- t(metriques_reseau_All_species_Parc)

colnames(t_metrics_Parc) <- rownames(metriques_reseau_All_species_Parc)
rownames(t_metrics_Parc) <- colnames(metriques_reseau_All_species_Parc)

t_metrics_Parc <- t_metrics_Parc[-11,]
t_metrics_Parc <- as.data.frame(t_metrics_Parc)

t_metrics_Parc <- t_metrics_Parc %>%
  tibble::rownames_to_column(var = "Parc")

data_ACP_2 <- data_ACP_2 %>%
  mutate(t_metrics_Parc)


ggplot(data_ACP_2, aes(x = Pianka_index, y = niche.overlap.LL, color = Parc)) +
  geom_point(size = 3) +
  geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
  labs(x = "Pianka_index", y = "niche.overlap.LL")



# indices réseaux à l'échelle des proies d'intérêt

library(reshape2)
library(bipartite)

# Espèces ciblées
species_of_interest <- c(
  "INS_Phylloxeridae_Daktulosphaira",
  "INS_Cicadellidae_Empoasca",
  "INS_Cicadellidae_Scaphoideus",
  "INS_Tortricidae_Lobesia"
)

# Initialiser une liste pour stocker les résultats
results_list <- list()

for (parc in parc_values) {
  
  # Filtrer les données pour le parc
  subset_data <- tab_pij2_All_species_Seuil_1_percent[tab_pij2_All_species_Seuil_1_percent$Parc == parc, ]
  
  # Créer la matrice d'interaction
  interaction_matrix <- reshape2::dcast(
    subset_data,
    ReadName ~ Isp,
    value.var = "PijComb",
    fun.aggregate = sum,
    fill = 0
  )
  
  # Nettoyage : noms de lignes et suppression de la colonne ReadName
  rownames(interaction_matrix) <- interaction_matrix[, 1]
  interaction_matrix <- as.matrix(interaction_matrix[, -1])
  
  # Trier les lignes
  interaction_matrix <- interaction_matrix[order(rownames(interaction_matrix)), ]
  
  # Extraire les indices
  deg <- specieslevel(interaction_matrix, index = "degree", level = "lower")
  eff <- specieslevel(interaction_matrix, index = "effective partners", level = "lower")
  spe <- specieslevel(interaction_matrix, index = "species specificity", level = "lower")
  
  # Convertir en data frame
  df_deg <- as.data.frame(deg)
  df_eff <- as.data.frame(eff)
  df_spe <- as.data.frame(spe)
  
  # Garder uniquement les espèces d'intérêt
  deg_values <- df_deg[rownames(df_deg) %in% species_of_interest, , drop = FALSE]
  eff_values <- df_eff[rownames(df_eff) %in% species_of_interest, , drop = FALSE]
  spe_values <- df_spe[rownames(df_spe) %in% species_of_interest, , drop = FALSE]
  
  # Si certaines espèces sont absentes, les ajouter avec NA
  missing_species <- setdiff(species_of_interest, rownames(deg_values))
  for (sp in missing_species) {
    deg_values[sp, ] <- NA
    eff_values[sp, ] <- NA
    spe_values[sp, ] <- NA
  }
  
  # Ordonner les espèces comme dans species_of_interest
  deg_values <- deg_values[species_of_interest, , drop = FALSE]
  eff_values <- eff_values[species_of_interest, , drop = FALSE]
  spe_values <- spe_values[species_of_interest, , drop = FALSE]
  
  # Créer un data frame avec les valeurs et noms de colonnes explicites
  result_row <- data.frame(
    Parc = parc,
    t(deg_values),
    t(eff_values),
    t(spe_values)
  )
  
  # Renommer les colonnes
  colnames(result_row)[-1] <- c(
    paste0("degree_", species_of_interest),
    paste0("effective_partners_", species_of_interest),
    paste0("species_specificity_", species_of_interest)
    
  )
  
  # Calculer les moyennes des indices pour les espèces d'intérêt
  mean_degree <- mean(as.numeric(deg_values[, 1]), na.rm = TRUE)
  mean_effective_partners <- mean(as.numeric(eff_values[, 1]), na.rm = TRUE)
  mean_species_specificity <- mean(as.numeric(spe_values[, 1]), na.rm = TRUE)
  
  # Ajouter les moyennes comme colonnes supplémentaires
  result_row$mean_degree <- mean_degree
  result_row$mean_effective_partners <- mean_effective_partners
  result_row$mean_species_specificity <- mean_species_specificity
  
  
  # Stocker le résultat
  results_list[[parc]] <- result_row
}

# Combiner tous les résultats
species_level_df <- do.call(rbind, results_list)
rownames(species_level_df) <- NULL


data_ACP_2 <- data_ACP_2 %>%
  mutate(species_level_df)


#===============
# XIX - Création du tableau pour les modèles ===============
#===============

Tab_models <- pianka_df
Tab_models$cult <- substr(Tab_models$Parc, nchar(Tab_models$Parc), nchar(Tab_models$Parc))


#''''''''''''''''''''''''
#'  CR_Paysage pour abondances ravageurs 
#''''''''''''''''''''''''
#''''''''''''''''''''''''

CR_Paysage_Tab_modèles <- CR_Paysage[,c("Parc", "cult", "camp", "NBcica", "NBtord", "NBphyl")]

CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
  filter(camp %in% c(2, 3))

CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
  filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))

# Conserver la tableau pour calculer le taux d'accroissement camp 2 et 3
Tab_accroissement_2_3 <- CR_Paysage_Tab_modèles

CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
  select(-cult, -camp)

CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
  group_by(Parc) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))


CR_Paysage_Tab_modèles_scaled <- scale(CR_Paysage_Tab_modèles[, c("NBcica", "NBtord", "NBphyl")], center = TRUE, scale = TRUE)

CR_Paysage_Tab_modèles_scaled <- data.frame(Parc = CR_Paysage_Tab_modèles$Parc, NBAll_pests = rowMeans(CR_Paysage_Tab_modèles_scaled))


#''''''''''''''''''''''''
#'  CR_Paysage pour delta camp 2 et 3 pour Cica et Phyl 
#''''''''''''''''''''''''
#''''''''''''''''''''''''

Tab_accroissement_2_3 <- Tab_accroissement_2_3[,c("Parc", "camp", "NBcica", "NBphyl")] %>%
  pivot_wider(names_from = camp, values_from = c(NBcica, NBphyl), names_prefix = "camp") %>%
  mutate(Taux_accroiss_NBcica = log((1 + NBcica_camp3) / (1 + NBcica_camp2)),
         Taux_accroiss_NBphyl = log((1 + NBphyl_camp3) / (1 + NBphyl_camp2))
  ) %>%
  select(Parc, starts_with("Taux_accroiss_"))


Tab_accroissement_2_3_scaled <- scale(Tab_accroissement_2_3[, c("Taux_accroiss_NBcica", "Taux_accroiss_NBphyl")], center = TRUE, scale = TRUE)

Tab_accroissement_2_3_scaled <- data.frame(Parc = Tab_accroissement_2_3$Parc, Taux_accroiss_NBAll_pests = rowMeans(Tab_accroissement_2_3_scaled))


#''''''''''''''''''''''''
#'  CR_Paysage pour HSN
#''''''''''''''''''''''''
#''''''''''''''''''''''''

Tab_HSN <- CR_Paysage[,c("Parc","HSNtot")]
Tab_HSN <- Tab_HSN %>%
  distinct(Parc, .keep_all = TRUE)
Tab_HSN <- Tab_HSN %>%
  filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))


#''''''''''''''''''''''''
#'  Data_composite_2 pour IFT
#''''''''''''''''''''''''
#''''''''''''''''''''''''

# custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#normalise data using custom function
Data_composite <- data_ACP[,-2]

Data_composite$HSNtot_bin <- minMax(Data_composite$HSNtot)
Data_composite$HSNtot_bin_inv <- 1 - minMax(Data_composite$HSNtot)
Data_composite$IFT_Ins_Fon_bin <- minMax(Data_composite$IFT_Ins_Fon)
Data_composite$Int_ti_bin <- minMax(Data_composite$Int_ti)

Data_composite$Composite <- Data_composite$HSNtot_bin_inv + Data_composite$IFT_Ins_Fon_bin + Data_composite$Int_ti_bin

Data_composite_2 <- Data_composite %>%
  dplyr::group_by(Parc) %>%
  dplyr::summarize(across(everything(), mean, na.rm = TRUE))

Data_composite_2_Tab_modèles <- Data_composite_2[,c("Parc", "IFTHer", "IFTIns", "IFTFon")]
Data_composite_2_Tab_modèles$IFTTot <- Data_composite_2_Tab_modèles$IFTHer + Data_composite_2_Tab_modèles$IFTIns + Data_composite_2_Tab_modèles$IFTFon


pHSN <- CR_Paysage[, c("Parc", "HSNtot")] %>%
  distinct(Parc, HSNtot)

Data_modeles <- pianka_df

# Ajouter la colonne FS
Data_modeles$FS <- ifelse(
  substr(Data_modeles$Parc, nchar(Data_modeles$Parc), nchar(Data_modeles$Parc)) == "B",
  "Bio",
  "Conv"
)

Data_modeles <- Data_modeles %>%
  left_join(tab_pred_shannon_div, by = "Parc")

Data_modeles <- Data_modeles %>%
  left_join(tab_prey_shannon_div, by = "Parc")

Data_modeles <- Data_modeles %>%
  left_join(pHSN, by = "Parc")


Data_modeles_2 <- Data_modeles %>%
  left_join(Data_composite_2[,c("Parc", "Composite", "Int_ti", "IFT_Ins_Fon")], by = "Parc")


#''''''''''''''''''''''''
#'  Data_eggs_2 pour Npred
#''''''''''''''''''''''''
#''''''''''''''''''''''''

Data_eggs <- subset(Data_eggs, sampling_date == "3" & type == "egg")

Data_eggs <- rename(Data_eggs, Parc = vineyard)

Data_eggs_2 <- Data_eggs %>%
  dplyr::mutate(across(where(is.character), as.numeric, .names = "numeric_{.col}")) %>%
  dplyr::group_by(Parc) %>%
  dplyr::summarize(across(everything(), mean, na.rm = TRUE))

Data_eggs_2_Tab_modèles <- Data_eggs_2[,c("Parc", "Npred")]



#===============
# XX - Calcul des metriques de réseau par Parc (Nestedness, Connectance, vulnerability and specialization) ===============
#===============

metriques_reseau_All_species_Parc <- final_metriques[rownames(final_metriques) %in% c("vulnerability.LL", "connectance", "nestedness", "specialisation asymmetry"), ]

metriques_reseau_All_species_Parc <- rename(metriques_reseau_All_species_Parc, "1088B" = "1088B_networklevel(interaction_matrix_All_species)",
                                            "1088C" = "1088C_networklevel(interaction_matrix_All_species)",
                                            "1435B" = "1435B_networklevel(interaction_matrix_All_species)",
                                            "1435C" = "1435C_networklevel(interaction_matrix_All_species)",
                                            "1650B" = "1650B_networklevel(interaction_matrix_All_species)",
                                            "1650C" = "1650C_networklevel(interaction_matrix_All_species)",
                                            "1662B" = "1662B_networklevel(interaction_matrix_All_species)",
                                            "1662C" = "1662C_networklevel(interaction_matrix_All_species)",
                                            "1868B" = "1868B_networklevel(interaction_matrix_All_species)",
                                            "1868C" = "1868C_networklevel(interaction_matrix_All_species)",)

t_metrics_Parc <- t(metriques_reseau_All_species_Parc)
colnames(t_metrics_Parc) <- rownames(metriques_reseau_All_species_Parc)
rownames(t_metrics_Parc) <- colnames(metriques_reseau_All_species_Parc)

t_metrics_Parc <- t_metrics_Parc[-11,]
t_metrics_Parc <- as.data.frame(t_metrics_Parc)

t_metrics_Parc <- t_metrics_Parc %>%
  tibble::rownames_to_column(var = "Parc")

#===============
# XXI - Création du tableau pour les modèles ===============
#===============

Tab_models <- Tab_models %>%
  left_join(tab_pred_shannon_div, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(tab_prey_shannon_div, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(CR_Paysage_Tab_modèles, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(CR_Paysage_Tab_modèles_scaled, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(Tab_HSN, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(Data_composite_2_Tab_modèles, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(Data_eggs_2_Tab_modèles, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(Tab_accroissement_2_3, by = "Parc")

Tab_models <- Tab_models %>%
  left_join(Tab_accroissement_2_3_scaled, by = "Parc")


write.table(Tab_models, file = "Tab_models.txt", sep = "\t",
            row.names = FALSE)


#===============
# XXII - ACP sur les métriques réseaux ===============
#===============

#scale data
ACP_metriques_reseaux <- data_ACP_2 %>%
  #select(Pianka_index, connectance, `modularity Q`, nestedness, niche.overlap.HL, niche.overlap.LL, generality.HL, vulnerability.LL)
select(Predator_Shannon_Div, Prey_Shannon_Div, HSNtot, IFTTot, Int_ti, IFTLUI, LandscapeLUI,Pianka_index, connectance, `modularity Q`, nestedness, niche.overlap.HL, niche.overlap.LL, generality.HL, vulnerability.LL, H2, mean_degree, mean_effective_partners, mean_species_specificity
       #degree_INS_Phylloxeridae_Daktulosphaira,degree_INS_Cicadellidae_Empoasca, degree_INS_Cicadellidae_Scaphoideus, degree_INS_Tortricidae_Lobesia, effective_partners_INS_Phylloxeridae_Daktulosphaira, effective_partners_INS_Cicadellidae_Empoasca, effective_partners_INS_Cicadellidae_Scaphoideus, effective_partners_INS_Tortricidae_Lobesia
       )

ACP_metriques_reseaux <- as.data.frame(apply(ACP_metriques_reseaux, 2, as.numeric))

dataPCAscale_reseaux <-scale(ACP_metriques_reseaux) 

resPCA_reseaux <- PCA(dataPCAscale_reseaux, scale.unit=TRUE)

fviz_eig(resPCA_reseaux,add.labels=T)
#fviz_pca_ind(resPCA,repel=T,col.ind="cos2")

#extraction des deux axes : local land use intensity and landscape land use intensity

IFTLUI<-resPCA_reseaux$ind$coord[,2]
LandscapeLUI<-resPCA_reseaux$ind$coord[,1]

data_ACP_2 <- data_ACP_2 %>%
  mutate(Tab_models[,c("Parc", "Npred", "NBcica", "NBtord", "NBphyl", "NBAll_pests", "Taux_accroiss_NBcica", "Taux_accroiss_NBphyl", "Taux_accroiss_NBAll_pests")])
         
data_ACP_2 <- as.data.frame(apply(data_ACP_2, 2, as.numeric))


ggplot(data_ACP_2, aes(x = HSNtot, y = mean_species_specificity)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm")+
  labs(x = "HSNtot", y = "mean_species_specificity")


#===============
# XXIII - Matrice de corrélation ===============
#===============

tab_correlation_1 <- data_ACP_2 %>%
  select(-starts_with("degree"),
         -starts_with("effective"),
         -starts_with("species"),
         -starts_with("tab"),
         -Parc,
         -IFTHer,
         -IFTIns,
         -IFTFon,
         -nestedness)

tab_correlation_1 <- as.data.frame(apply(tab_correlation_1, 2, as.numeric))

# Calculer la matrice de corrélation
correlation_matrix <- cor(tab_correlation_1)

print(correlation_matrix)

# Convertir la matrice de corrélation en data frame
melted_correlation_matrix <- melt(correlation_matrix)

# Créer le heatmap
ggplot(melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3, color = "black") +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  theme_minimal() +
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))



# Calculer la distance (1 - corrélation)
dist_matrix <- as.dist(1 - correlation_matrix)

# Classification hiérarchique
hc <- hclust(dist_matrix, method = "ward.D2")  # méthode possible : "ward.D2", "complete", "average", etc.

# Affichage du dendrogramme
plot(hc, main = "Dendrogramme des variables", xlab = "", sub = "", cex = 0.8)


library(pheatmap)
pheatmap(
  correlation_matrix,
  clustering_distance_rows = "euclidean",  # ou "correlation" si tu veux personnaliser
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D2",           # ou "complete", "average", etc.
  color = colorRampPalette(c("red", "white", "green"))(50),
  display_numbers = TRUE,                  # ✅ pour afficher les valeurs dans la matrice
  fontsize_number = 6,
  fontsize_row = 6,
  fontsize_col = 6,
  main = "Dendrogramme + Matrice de corrélation"
)


#===============
# XXIV - Modules du réseau par Parc ===============
#===============

library(igraph)

# Liste pour stocker les modularités
modularites <- list()

# Boucle pour chaque parcelle
for (parc in parc_values) {
  
  # Charger la matrice d'interaction bipartite (proies en lignes, prédateurs en colonnes)
  interaction_matrix <- get(paste0("interaction_matrix_All_species_", parc))
  
  # Conversion en matrice si nécessaire
  mat_bipartite <- as.matrix(interaction_matrix)
  
  # Créer le graphe biparti
  g <- graph_from_incidence_matrix(mat_bipartite, weighted = TRUE)
  
  # Attribuer les noms d’espèces aux sommets
  prey_names <- rownames(mat_bipartite)
  predator_names <- colnames(mat_bipartite)
  V(g)$name <- c(prey_names, predator_names)
  
  # Détection des communautés avec Walktrap
  community <- cluster_walktrap(g)
  
  # Obtenir les modules (communautés détectées)
  modules <- membership(community)
  
  # Calculer la modularité du réseau
  modularite <- modularity(community)
  modularites[[parc]] <- modularite
  
  # Coloration des sommets selon les modules
  V(g)$color <- as.factor(modules)
  
  # Créer et afficher le dendrogramme pour chaque parcelle
  dend <- as.dendrogram(community)
  plot(dend, main = paste("Dendrogramme des communautés - Parcelle", parc))
  
  # Visualiser le réseau avec les modules colorés
  plot(g, vertex.size = 5, vertex.label = V(g)$name, main = paste("Réseau avec modules - Parcelle", parc))
}

# Afficher les résultats de modularité pour chaque parcelle
modularites


# Résumé des modularités
modularites_df <- data.frame(
  Parcelle = names(modularites),
  Modularite = unlist(modularites)
)

print(modularites_df)


#===============
# XXV - Nouveaux modèles 23_04_2025 ===============
#===============

data_ACP_2$Parc <- Tab_models$Parc
# On commence par enlever de df2 les colonnes qui sont déjà dans df1, sauf 'Parc'
df2_filtered <- data_ACP_2 %>% select(-any_of(setdiff(intersect(names(Tab_models), names(data_ACP_2)), "Parc")))


# Puis on fait un left_join pour ajouter les colonnes uniques de df2 à df1, en faisant correspondre 'Parc'
Tableau_model_final <- Tab_models %>% left_join(df2_filtered, by = "Parc")

Tableau_model_final$Site <- substr(Tableau_model_final$Parc, 1, nchar(Tableau_model_final$Parc) - 1)

Tableau_model_final$Site <- as.factor(Tableau_model_final$Site)



#M1a: Predator diversity ~ HSN * AB + (1|site)
#M1b: Predator diversity ~ HSN * IFTTot + (1|site)
#M2a:Prey diversity ~ HSN + AB + (1|site)
#M2b:Prey diversity ~ HSN + IFTTot + (1|site)


#===============
# Diversité des prédateurs en fonction de la diversité des habitats et des pratiques agricoles
#===============
# Mod1a


Mod1a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Predator_Shannon_Div)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Predator_Shannon_Div") +
  theme_minimal()        
Mod1a
ggsave(filename = "Mod1a.jpeg", plot = Mod1a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod1a <- lmer(Predator_Shannon_Div ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod1a)
summary(Mod1a)


# Mod1b


Mod1b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Predator_Shannon_Div)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Predator_Shannon_Div") +
  theme_minimal()        
Mod1b
ggsave(filename = "Mod1b.jpeg", plot = Mod1b, width = 11, height = 8)

Mod1b <- lmer(Predator_Shannon_Div ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod1b)
summary(Mod1b)


#===============
# Diversité des proies en fonction de la diversité des habitats et des pratiques agricoles
#===============
# Mod2a


Mod2a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Prey_Shannon_Div)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Prey_Shannon_Div") +
  theme_minimal()        
Mod2a
ggsave(filename = "Mod2a.jpeg", plot = Mod2a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod2a <- lmer(Prey_Shannon_Div ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod2a)
summary(Mod2a)


# Mod2b


Mod2b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Prey_Shannon_Div)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Prey_Shannon_Div") +
  theme_minimal()        
Mod2b
ggsave(filename = "Mod2b.jpeg", plot = Mod2b, width = 11, height = 8)

Mod2b <- lmer(Prey_Shannon_Div ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod2b)
summary(Mod2b)



#===============
# Chevauchement de niches des prédateurs selon l’environnement et les intrants
#===============
# Mod3a
   

Mod3a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = niche.overlap.HL)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "niche.overlap.HL") +
  theme_minimal()        
Mod3a
ggsave(filename = "Mod3a.jpeg", plot = Mod3a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod3a <- lmer(niche.overlap.HL ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod3a)
summary(Mod3a)


# Mod3b


Mod3b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = niche.overlap.HL)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "niche.overlap.HL") +
  theme_minimal()        
Mod3b
ggsave(filename = "Mod3b.jpeg", plot = Mod3b, width = 11, height = 8)

Mod3b <- lmer(niche.overlap.HL ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod3b)
summary(Mod3b)



# Mod4a
   

Mod4a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Pianka_index)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Pianka_index") +
  theme_minimal()        
Mod4a
ggsave(filename = "Mod4a.jpeg", plot = Mod4a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod4a <- lmer(Pianka_index ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod4a)
summary(Mod4a)


# Mod4b


Mod4b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Pianka_index)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Pianka_index") +
  theme_minimal()        
Mod4b
ggsave(filename = "Mod4b.jpeg", plot = Mod4b, width = 11, height = 8)

Mod4b <- lmer(Pianka_index ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod4b)
summary(Mod4b)



#===============
# Propriétés du réseau
#===============

# Vulnerabilité des proies
# Mod5a
   

Mod5a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = vulnerability.LL)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "vulnerability.LL") +
  theme_minimal()        
Mod5a
ggsave(filename = "Mod5a.jpeg", plot = Mod5a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod5a <- lmer(vulnerability.LL ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod5a)
summary(Mod5a)


# Mod5b


Mod5b <- ggplot(Tableau_model_final, aes(x = IFTTot, y = vulnerability.LL)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "IFTTot",
       y = "vulnerability.LL") +
  theme_minimal()        
Mod5b
ggsave(filename = "Mod5b.jpeg", plot = Mod5b, width = 11, height = 8)

Mod5b <- lmer(vulnerability.LL ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod5b)
summary(Mod5b)


# Généralité des prédateurs

# Mod6a
   

Mod6a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = generality.HL)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "generality.HL") +
  theme_minimal()        
Mod6a
ggsave(filename = "Mod6a.jpeg", plot = Mod6a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod6a <- lmer(generality.HL ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod6a)
summary(Mod6a)


# Mod6b


Mod6b <- ggplot(Tableau_model_final, aes(x = IFTTot, y = generality.HL)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "IFTTot",
       y = "generality.HL") +
  theme_minimal()        
Mod6b
ggsave(filename = "Mod6b.jpeg", plot = Mod6b, width = 11, height = 8)

Mod6b <- lmer(generality.HL ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod6b)
summary(Mod6b)


# Modularité du réseau

# Mod7a
   

Mod7a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = `modularity Q`)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "`modularity Q`") +
  theme_minimal()        
Mod7a
ggsave(filename = "Mod7a.jpeg", plot = Mod7a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod7a <- lmer(`modularity Q` ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod7a)
summary(Mod7a)


# Mod7b


Mod7b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = `modularity Q`)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "`modularity Q`") +
  theme_minimal()        
Mod7b
ggsave(filename = "Mod7b.jpeg", plot = Mod7b, width = 11, height = 8)

Mod7b <- lmer(`modularity Q` ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod7b)
summary(Mod7b)


# Specialisation
# Mod8a
   

Mod8a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = H2)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "H2") +
  theme_minimal()        
Mod8a
ggsave(filename = "Mod8a.jpeg", plot = Mod8a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod8a <- lmer(H2 ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod8a)
summary(Mod8a)


# Mod8b


Mod8b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = H2)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "H2") +
  theme_minimal()        
Mod8b
ggsave(filename = "Mod8b.jpeg", plot = Mod8b, width = 11, height = 8)

Mod8b <- lmer(H2 ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod8b)
summary(Mod8b)



# Mod9a
   

Mod9a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = mean_degree)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "mean_degree") +
  theme_minimal()        
Mod9a
ggsave(filename = "Mod9a.jpeg", plot = Mod9a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod9a <- lmer(mean_degree ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod9a)
summary(Mod9a)


# Mod9b


Mod9b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = mean_degree)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "mean_degree") +
  theme_minimal()        
Mod9b
ggsave(filename = "Mod9b.jpeg", plot = Mod9b, width = 11, height = 8)

Mod9b <- lmer(mean_degree ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod9b)
summary(Mod9b)



# Mod10a
   

Mod10a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = mean_species_specificity)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "mean_species_specificity") +
  theme_minimal()        
Mod10a
ggsave(filename = "Mod10a.jpeg", plot = Mod10a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod10a <- lmer(mean_species_specificity ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod10a)
summary(Mod10a)


# Mod10b


Mod10b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = mean_effective_partners)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "mean_effective_partners") +
  theme_minimal()        
Mod10b
ggsave(filename = "Mod10b.jpeg", plot = Mod10b, width = 11, height = 8)

Mod10b <- lmer(mean_effective_partners ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod10b)
summary(Mod10b)



# Mod11a
   

Mod11a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = mean_species_specificity)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "mean_species_specificity") +
  theme_minimal()        
Mod11a
ggsave(filename = "Mod11a.jpeg", plot = Mod11a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod11a <- lmer(mean_species_specificity ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod11a)
summary(Mod11a)


# Mod11b


Mod11b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = mean_species_specificity)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "mean_species_specificity") +
  theme_minimal()        
Mod11b
ggsave(filename = "Mod11b.jpeg", plot = Mod11b, width = 11, height = 8)

Mod11b <- lmer(mean_species_specificity ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod11b)
summary(Mod11b)


#===============
# Abondance des ravageurs (Cica, Phyl)
#===============
# Mod12a
   

Mod12a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "NBAll_pests") +
  theme_minimal()        
Mod12a
ggsave(filename = "Mod12a.jpeg", plot = Mod12a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod12a <- lmer(NBAll_pests ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod12a)
summary(Mod12a)


# Mod12b


Mod12b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = NBAll_pests)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "NBAll_pests") +
  theme_minimal()        
Mod12b
ggsave(filename = "Mod12b.jpeg", plot = Mod12b, width = 11, height = 8)

Mod12b <- lmer(NBAll_pests ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod12b)
summary(Mod12b)


# Mod13a
   

Mod13a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = NBcica)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "NBcica") +
  theme_minimal()        
Mod13a
ggsave(filename = "Mod13a.jpeg", plot = Mod13a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod13a <- lmer(NBcica ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod13a)
summary(Mod13a)


# Mod13b


Mod13b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = NBcica)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "NBcica") +
  theme_minimal()        
Mod13b
ggsave(filename = "Mod13b.jpeg", plot = Mod13b, width = 11, height = 8)

Mod13b <- lmer(NBcica ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod13b)
summary(Mod13b)



# Mod14a
   

Mod14a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = NBphyl)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "NBphyl") +
  theme_minimal()        
Mod14a
ggsave(filename = "Mod14a.jpeg", plot = Mod14a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod14a <- lmer(NBphyl ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod14a)
summary(Mod14a)


# Mod14b


Mod14b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = NBphyl)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "NBphyl") +
  theme_minimal()        
Mod14b
ggsave(filename = "Mod14b.jpeg", plot = Mod14b, width = 11, height = 8)

Mod14b <- lmer(NBphyl ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod14b)
summary(Mod14b)


#===============
# Taux d'accroissement des ravageurs (Cica, Tord, Phyl)
#===============
# Mod15a
   

Mod15a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod15a
ggsave(filename = "Mod15a.jpeg", plot = Mod15a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod15a <- lmer(Taux_accroiss_NBAll_pests ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod15a)
summary(Mod15a)


# Mod15b


Mod15b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod15b
ggsave(filename = "Mod15b.jpeg", plot = Mod15b, width = 11, height = 8)

Mod15b <- lmer(Taux_accroiss_NBAll_pests ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod15b)
summary(Mod15b)



# Mod16a
   

Mod16a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Taux_accroiss_NBcica)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Taux_accroiss_NBcica") +
  theme_minimal()        
Mod16a
ggsave(filename = "Mod16a.jpeg", plot = Mod16a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod16a <- lmer(Taux_accroiss_NBcica ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod16a)
summary(Mod16a)


# Mod16b


Mod16b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Taux_accroiss_NBcica)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Taux_accroiss_NBcica") +
  theme_minimal()        
Mod16b
ggsave(filename = "Mod16b.jpeg", plot = Mod16b, width = 11, height = 8)

Mod16b <- lmer(Taux_accroiss_NBcica ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod16b)
summary(Mod16b)


# Mod17a
   

Mod17a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Taux_accroiss_NBphyl)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Taux_accroiss_NBphyl") +
  theme_minimal()        
Mod17a
ggsave(filename = "Mod17a.jpeg", plot = Mod17a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod17a <- lmer(Taux_accroiss_NBphyl ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod17a)
summary(Mod17a)


# Mod17b


Mod17b <- ggplot(Tableau_model_final, aes(x = IFTTot, y = Taux_accroiss_NBphyl)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "IFTTot",
       y = "Taux_accroiss_NBphyl") +
  theme_minimal()        
Mod17b
ggsave(filename = "Mod17b.jpeg", plot = Mod17b, width = 11, height = 8)

Mod17b <- lmer(Taux_accroiss_NBphyl ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod17b)
summary(Mod17b)

#===============
### Relationships between communities and trophic network metrics
#===============
#===============
# Diversité des proies et structure du réseau
#===============
# Mod18a
   

Mod18a <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = niche.overlap.HL)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "niche.overlap.HL") +
  theme_minimal()        
Mod18a
ggsave(filename = "Mod18a.jpeg", plot = Mod18a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod18a <- lmer(niche.overlap.HL ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod18a)
summary(Mod18a)



# Mod18b
   

Mod18b <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = Pianka_index)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "Pianka_index") +
  theme_minimal()        
Mod18b
ggsave(filename = "Mod18b.jpeg", plot = Mod18b, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod18b <- lmer(Pianka_index ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod18b)
summary(Mod18b)




# Mod18c
   

Mod18c <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = vulnerability.LL)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "vulnerability.LL") +
  theme_minimal()        
Mod18c
ggsave(filename = "Mod18c.jpeg", plot = Mod18c, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod18c <- lmer(vulnerability.LL ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod18c)
summary(Mod18c)


# Mod18d
   

Mod18d <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = generality.HL)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "generality.HL") +
  theme_minimal()        
Mod18d
ggsave(filename = "Mod18d.jpeg", plot = Mod18d, width = 11, height = 8)

Mod18d <- lmer(generality.HL ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod18d)
summary(Mod18d)



# Mod19a
   

Mod19a <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = `modularity Q`)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "`modularity Q`") +
  theme_minimal()        
Mod19a
ggsave(filename = "Mod19a.jpeg", plot = Mod19a, width = 11, height = 8)

Mod19a <- lmer(`modularity Q` ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod19a)
summary(Mod19a)



# Mod19b
   

Mod19b <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = H2)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "H2") +
  theme_minimal()        
Mod19b
ggsave(filename = "Mod19b.jpeg", plot = Mod19b, width = 11, height = 8)

Mod19b <- lmer(H2 ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod19b)
summary(Mod19b)



# Mod20a
   

Mod20a <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = mean_degree)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "mean_degree") +
  theme_minimal()        
Mod20a
ggsave(filename = "Mod20a.jpeg", plot = Mod20a, width = 11, height = 8)

Mod20a <- lmer(mean_degree ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod20a)
summary(Mod20a)


# Mod20b
   

Mod20b <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = mean_effective_partners)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "mean_effective_partners") +
  theme_minimal()        
Mod20b
ggsave(filename = "Mod20b.jpeg", plot = Mod20b, width = 11, height = 8)

Mod20b <- lmer(mean_effective_partners ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod20b)
summary(Mod20b)



# Mod20c
   

Mod20c <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = mean_species_specificity)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "mean_species_specificity") +
  theme_minimal()        
Mod20c
ggsave(filename = "Mod20c.jpeg", plot = Mod20c, width = 11, height = 8)

Mod20c <- lmer(mean_species_specificity ~ Prey_Shannon_Div + Predator_Shannon_Div + (1|Site), data = Tableau_model_final)
print(Mod20c)
summary(Mod20c)


#===============
### Relationships between communities and trophic network metrics
#===============
#===============
# Ravageurs et structure du réseau
#===============
# Mod21a
   

Mod21a <- ggplot(Tableau_model_final, aes(x = niche.overlap.HL, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "niche.overlap.HL",
       y = "NBAll_pests") +
  theme_minimal()        
Mod21a
ggsave(filename = "Mod21a.jpeg", plot = Mod21a, width = 11, height = 8)

Mod21a <- lmer(NBAll_pests ~ niche.overlap.HL + (1|Site), data = Tableau_model_final)
print(Mod21a)
summary(Mod21a)



# Mod21b
   

Mod21b <- ggplot(Tableau_model_final, aes(x = niche.overlap.HL, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "niche.overlap.HL",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod21b
ggsave(filename = "Mod21b.jpeg", plot = Mod21b, width = 11, height = 8)

Mod21b <- lmer(Taux_accroiss_NBAll_pests ~ niche.overlap.HL + (1|Site), data = Tableau_model_final)
print(Mod21b)
summary(Mod21b)



# Mod22a
   

Mod22a <- ggplot(Tableau_model_final, aes(x = Pianka_index, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Pianka_index",
       y = "NBAll_pests") +
  theme_minimal()        
Mod22a
ggsave(filename = "Mod22a.jpeg", plot = Mod22a, width = 11, height = 8)

Mod22a <- lmer(NBAll_pests ~ Pianka_index + (1|Site), data = Tableau_model_final)
print(Mod22a)
summary(Mod22a)



# Mod22b
   

Mod22b <- ggplot(Tableau_model_final, aes(x = Pianka_index, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Pianka_index",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod22b
ggsave(filename = "Mod22b.jpeg", plot = Mod22b, width = 11, height = 8)

Mod22b <- lmer(Taux_accroiss_NBAll_pests ~ Pianka_index + (1|Site), data = Tableau_model_final)
print(Mod22b)
summary(Mod22b)



# Mod23a
   

Mod23a <- ggplot(Tableau_model_final, aes(x = vulnerability.LL, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "vulnerability.LL",
       y = "NBAll_pests") +
  theme_minimal()        
Mod23a
ggsave(filename = "Mod23a.jpeg", plot = Mod23a, width = 11, height = 8)

Mod23a <- lmer(NBAll_pests ~ vulnerability.LL + (1|Site), data = Tableau_model_final)
print(Mod23a)
summary(Mod23a)



# Mod23b
   

Mod23b <- ggplot(Tableau_model_final, aes(x = vulnerability.LL, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "vulnerability.LL",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod23b
ggsave(filename = "Mod23b.jpeg", plot = Mod23b, width = 11, height = 8)

Mod23b <- lmer(Taux_accroiss_NBAll_pests ~ vulnerability.LL + (1|Site), data = Tableau_model_final)
print(Mod23b)
summary(Mod23b)



# Mod24a
   

Mod24a <- ggplot(Tableau_model_final, aes(x = generality.HL, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "generality.HL",
       y = "NBAll_pests") +
  theme_minimal()        
Mod24a
ggsave(filename = "Mod24a.jpeg", plot = Mod24a, width = 11, height = 8)

Mod24a <- lmer(NBAll_pests ~ generality.HL + (1|Site), data = Tableau_model_final)
print(Mod24a)
summary(Mod24a)



# Mod24b
   

Mod24b <- ggplot(Tableau_model_final, aes(x = generality.HL, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "generality.HL",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod24b
ggsave(filename = "Mod24b.jpeg", plot = Mod24b, width = 11, height = 8)

Mod24b <- lmer(Taux_accroiss_NBAll_pests ~ generality.HL + (1|Site), data = Tableau_model_final)
print(Mod24b)
summary(Mod24b)



# Mod25a
   

Mod25a <- ggplot(Tableau_model_final, aes(x = `modularity Q`, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "`modularity Q`",
       y = "NBAll_pests") +
  theme_minimal()        
Mod25a
ggsave(filename = "Mod25a.jpeg", plot = Mod25a, width = 11, height = 8)

Mod25a <- lmer(NBAll_pests ~ `modularity Q` + (1|Site), data = Tableau_model_final)
print(Mod25a)
summary(Mod25a)



# Mod25b
   

Mod25b <- ggplot(Tableau_model_final, aes(x = `modularity Q`, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "`modularity Q`",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod25b
ggsave(filename = "Mod25b.jpeg", plot = Mod25b, width = 11, height = 8)

Mod25b <- lmer(Taux_accroiss_NBAll_pests ~ `modularity Q` + (1|Site), data = Tableau_model_final)
print(Mod25b)
summary(Mod25b)



# Mod26a
   

Mod26a <- ggplot(Tableau_model_final, aes(x = H2, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "H2",
       y = "NBAll_pests") +
  theme_minimal()        
Mod26a
ggsave(filename = "Mod26a.jpeg", plot = Mod26a, width = 11, height = 8)

Mod26a <- lmer(NBAll_pests ~ H2 + (1|Site), data = Tableau_model_final)
print(Mod26a)
summary(Mod26a)



# Mod26b
   

Mod26b <- ggplot(Tableau_model_final, aes(x = H2, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "H2",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod26b
ggsave(filename = "Mod26b.jpeg", plot = Mod26b, width = 11, height = 8)

Mod26b <- lmer(Taux_accroiss_NBAll_pests ~ H2 + (1|Site), data = Tableau_model_final)
print(Mod26b)
summary(Mod26b)


# Mod27a
   

Mod27a <- ggplot(Tableau_model_final, aes(x = mean_degree, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "mean_degree",
       y = "NBAll_pests") +
  theme_minimal()        
Mod27a
ggsave(filename = "Mod27a.jpeg", plot = Mod27a, width = 11, height = 8)

Mod27a <- lmer(NBAll_pests ~ mean_degree + (1|Site), data = Tableau_model_final)
print(Mod27a)
summary(Mod27a)



# Mod27b
   

Mod27b <- ggplot(Tableau_model_final, aes(x = mean_degree, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "mean_degree",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod27b
ggsave(filename = "Mod27b.jpeg", plot = Mod27b, width = 11, height = 8)

Mod27b <- lmer(Taux_accroiss_NBAll_pests ~ mean_degree + (1|Site), data = Tableau_model_final)
print(Mod27b)
summary(Mod27b)



# Mod28a
   

Mod28a <- ggplot(Tableau_model_final, aes(x = mean_effective_partners, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "mean_effective_partners",
       y = "NBAll_pests") +
  theme_minimal()        
Mod28a
ggsave(filename = "Mod28a.jpeg", plot = Mod28a, width = 11, height = 8)

Mod28a <- lmer(NBAll_pests ~ mean_effective_partners + (1|Site), data = Tableau_model_final)
print(Mod28a)
summary(Mod28a)



# Mod28b
   

Mod28b <- ggplot(Tableau_model_final, aes(x = mean_effective_partners, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "mean_effective_partners",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod28b
ggsave(filename = "Mod28b.jpeg", plot = Mod28b, width = 11, height = 8)

Mod28b <- lmer(Taux_accroiss_NBAll_pests ~ mean_effective_partners + (1|Site), data = Tableau_model_final)
print(Mod28b)
summary(Mod28b)



# Mod29a
   

Mod29a <- ggplot(Tableau_model_final, aes(x = mean_species_specificity, y = NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "mean_species_specificity",
       y = "NBAll_pests") +
  theme_minimal()        
Mod29a
ggsave(filename = "Mod29a.jpeg", plot = Mod29a, width = 11, height = 8)

Mod29a <- lmer(NBAll_pests ~ mean_species_specificity + (1|Site), data = Tableau_model_final)
print(Mod29a)
summary(Mod29a)



# Mod29b
   

Mod29b <- ggplot(Tableau_model_final, aes(x = mean_species_specificity, y = Taux_accroiss_NBAll_pests)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "mean_species_specificity",
       y = "Taux_accroiss_NBAll_pests") +
  theme_minimal()        
Mod29b
ggsave(filename = "Mod29b.jpeg", plot = Mod29b, width = 11, height = 8)

Mod29b <- lmer(Taux_accroiss_NBAll_pests ~ mean_species_specificity + (1|Site), data = Tableau_model_final)
print(Mod29b)
summary(Mod29b)

#' ---
#' title: "RDA_Output_Antoptic"
#' output: html_document
#' ---
#===============
# XXVI - Analyses multivariées ===============
#===============

library(vegan)

#' Load data

# Set up for the RDA
Tableau_model_final <- Tableau_model_final %>%
  rename(modularity_Q = 'modularity Q',
         niche_overlap_HL = 'niche.overlap.HL',
         niche_overlap_LL = 'niche.overlap.LL',
         generality_HL = 'generality.HL',
         vulnerability_LL = 'vulnerability.LL')

Tableau_model_final_1 <- Tableau_model_final
Tableau_model_final_1 <- Tableau_model_final_1[, colSums(is.na(Tableau_model_final_1)) == 0]


Tableau_var_a_expliquer <- Tableau_model_final_1[, c("Predator_Shannon_Div", "Prey_Shannon_Div", "niche_overlap_HL", "niche_overlap_LL", "Pianka_index",
                                                     "modularity_Q", "H2", "mean_degree", "vulnerability_LL", "generality_HL", "mean_effective_partners", "effective_partners_INS_Phylloxeridae_Daktulosphaira",
                                                     "mean_species_specificity", "species_specificity_INS_Phylloxeridae_Daktulosphaira", "NBAll_pests", "NBcica", "NBphyl", "NBtord", "Taux_accroiss_NBAll_pests", "Taux_accroiss_NBcica", "Taux_accroiss_NBphyl")]
rownames(Tableau_var_a_expliquer) <- Tableau_model_final_1$Parc

# Abundance variables must not be centered if we want to keep differences
vars_a_conserver <- c("NBcica", "NBphyl", "NBtord")
Y_a_conserver <- Tableau_var_a_expliquer[ , vars_a_conserver]
Y_a_transformer <- Tableau_var_a_expliquer[ , !(names(Tableau_var_a_expliquer) %in% vars_a_conserver) & sapply(Tableau_var_a_expliquer, is.numeric)]
Y_transformed <- as.data.frame(scale(Y_a_transformer))
Y_non_num <- Tableau_var_a_expliquer[ , !sapply(Tableau_var_a_expliquer, is.numeric)]

Y_centered_red <- cbind(Y_non_num, Y_a_conserver, Y_transformed)



Tableau_var_explicatives <- Tableau_model_final_1[, c("HSNtot", "cult", "IFTTot", "Int_ti")]
rownames(Tableau_var_explicatives) <- Tableau_model_final_1$Parc
X_num <- Tableau_var_explicatives[sapply(Tableau_var_explicatives, is.numeric)]
X_num_centered_red <- scale(X_num)
X_non_num <- Tableau_var_explicatives[!sapply(Tableau_var_explicatives, is.numeric)]
X_centered_red <- cbind(X_non_num, as.data.frame(X_num_centered_red))
X_centered_red <- as.data.frame(X_centered_red)


Tableau_var_condition <- Tableau_model_final_1[, "Site", drop = FALSE]
rownames(Tableau_var_condition) <- Tableau_model_final_1$Parc
Z <- Tableau_var_condition


df_all <- data.frame(X_centered_red, Z)
formule_rda <- as.formula(paste(
  "Y_centered_red ~",
  paste(colnames(X_centered_red), collapse = " + "),
  "+ Condition(Site)"
))

#' Perform RDA

RDA_1_formula <- vegan::rda(formule_rda, data = df_all)

#' Summary of RDA

summary(RDA_1_formula) # First axis explains the majority of the variance => the one to look at for interpretation
  #NBphyl is strongly related to RDA1 (explanatory variables)
  #Int_ti strongly positively correlated to RDA1
  #IFTtot negatively correlated to RDA1
  #cult has a strong effect on RDA1
  #Parc 1650B is positive in RDA1 => positively influenced by Int_ti (tillage intensity)
  #Parc 1650C is the opposite because it is the opposite FS
  

#' ANOVA tests

# Significance tests
anova.cca(RDA_1_formula)
  #Residual Df very low => low statistical power
anova.cca(RDA_1_formula, by = "axis") # by axis
anova.cca(RDA_1_formula, by = "terms") # by variables

anova.cca(RDA_1_formula, permutations = 999, by = "term")
anova.cca(RDA_1_formula, permutations = 999, by = "margin")

# Colinearity
sqrt(vif.cca(RDA_1_formula)) # if >2 multicollinearity is considered high.
# Extreme colinearity between sites
vif.cca(RDA_1_formula) # Les variables sont très colinéaires entre elles

# On a peut-être trop peu d'observations par rapport au nombre de sites

#' Adjusted R²

# R2
R2 <- RsquareAdj(RDA_1_formula)
R2$r.squared
R2$adj.r.squared

#' Scores

# Scores
scores(RDA_1_formula, display = "sites") # Sites scores
scores(RDA_1_formula, display = "species") # Y variables scores
scores(RDA_1_formula, display = "bp") # Explanatory variables scores (biplot arrows)

#' Eigen values

# Eigen values
eigenvals(RDA_1_formula)
RDA_1_formula$CCA$eig

#' Graphs

# Graphs
plot(RDA_1_formula, scaling = 1) # Scaling 1 shows similarities between objects in the response matrix.
plot(RDA_1_formula, scaling = 2) # Scaling 2 shows the effects of explanatory variables.

vegan::ordilabel(RDA_1_formula, display = "species", cex = 0.7)

vegan::ordiplot(RDA_1_formula, display = "sites", type = "text", scaling = 2)

#' Complementary analysis of variance partitionning

# Complementary analysis of variance partitionning
varpart(Y_centered_red, X_centered_red, Z)

#' Remarks

# Remarks :
# - Few residual degrees of freedom => low statistical power
# - High multicollinearity possible among explanatory variables
# - Possibly too few observations compared to the number of sites ?

knitr::spin("your_script.R", knit = TRUE)


#===============
# XXVII - Diagramme du meta-réseau ===============
#===============
library(migest)
library(circlize)
library(scales)

circos.clear()

prey_family_df <- tribble(
  ~prey,                               ~Prey_Family,
  "COL_Entomobryidae_Willowsia",       "Entomobryidae",
  "COL_Paronellidae_sp",               "Paronellidae",
  "COL_Sminthuridae_Sminthurus",       "Sminthuridae",
  "INS_Acrididae_sp",                  "Acrididae",
  "INS_Aeolothripidae_Aeolothrips",    "Aeolothripidae",
  "INS_Caeciliusidae_Valenzuela",      "Caeciliusidae",
  "INS_Chrysididae_sp",                "Chrysididae",
  "INS_Cicadellidae_Empoasca",         "Cicadellidae",
  "INS_Cicadellidae_Scaphoideus",      "Cicadellidae",
  "INS_Drosophilidae_Scaptomyza",      "Drosophilidae",
  "INS_Gryllidae_sp",                  "Gryllidae",
  "INS_Lygaeidae_Beosus",              "Lygaeidae",
  "INS_Phoridae_Megaselia",            "Phoridae",
  "INS_Phylloxeridae_Daktulosphaira",  "Phylloxeridae",
  "INS_Platygastridae_sp",             "Platygastridae",
  "INS_Tephritidae_Dioxyna",           "Tephritidae",
  "INS_Trichopsocidae_sp",             "Trichopsocidae",
  "INS_Aphelinidae_sp",                "Aphelinidae",
  "INS_Ectopsocidae_sp",               "Ectopsocidae",
  "INS_Thripidae_Drepanothrips",       "Thripidae",
  "INS_Miridae_Trigonotylus",          "Miridae",
  "INS_Thripidae_sp",                  "Thripidae",
  "INS_Aphididae_Acyrthosiphon",       "Aphididae",
  "INS_Asteiidae_sp",                  "Asteiidae",
  "INS_Brentidae_sp",                  "Brentidae",
  "INS_Elateridae_sp",                 "Elateridae",
  "INS_Latridiidae_Cortinicara",       "Latridiidae",
  "INS_Miridae_Lygus",                 "Miridae",
  "INS_Culicidae_Culex",               "Culicidae",
  "INS_Pentatomidae_Aelia",            "Pentatomidae",
  "INS_Pteromalidae_sp",               "Pteromalidae",
  "INS_Stenopsocidae_sp",              "Stenopsocidae",
  "INS_Apidae_Apis",                   "Apidae",
  "COL_Entomobryidae_sp",              "Entomobryidae",
  "INS_Aphididae_Anoecia",             "Aphididae",
  "INS_Aphididae_Aphis",               "Aphididae",
  "INS_Anthocoridae_Orius",            "Anthocoridae",
  "INS_Braconidae_Lysiphlebus",        "Braconidae",
  "INS_Crambidae_Pyrausta",            "Crambidae",
  "INS_Lygaeidae_Nysius",              "Lygaeidae",
  "ARA_Anystidae_Anystis",             "Anystidae",
  "INS_Aphididae_Protaphis",           "Aphididae",
  "INS_Noctuidae_Agrotis",             "Noctuidae",
  "INS_Chrysomelidae_Longitarsus",     "Chrysomelidae",
  "INS_Tortricidae_Lobesia",           "Tortricidae",
  "INS_Noctuidae_Spodoptera",          "Noctuidae",
  "INS_Anthomyiidae_Delia",            "Anthomyiidae",
  "INS_Crambidae_Agriphila",           "Crambidae",
  "INS_Thripidae_Thrips",              "Thripidae",
  "INS_Crambidae_Udea",                "Crambidae",
  "INS_Geometridae_Idaea",             "Geometridae"
)



Tab_reseau_All_species <- tab_pij2_All_species_Seuil_1_percent %>%
  select(ReadName, Isp, PijComb)

colnames(Tab_reseau_All_species) <- c("prey", "predator", "Interaction_probability")

Tab_reseau_All_species <- Tab_reseau_All_species %>%
  left_join(prey_family_df, by = "prey")



Tab_reseau_All_species$Predator_Class <- substr(Tab_reseau_All_species$predator, 1, 3)
Tab_reseau_All_species$Prey_Class <- substr(Tab_reseau_All_species$prey, 1, 3)

family_df <- tribble(
  ~predator,                          ~Predator_Family,
  "INS_Harpalus_smaragdinus",      "Carabidae",
  "INS_Pseudoophonus_rufipes",     "Carabidae",
  "INS_Chrysopidae_sp",            "Chrysopidae",
  "INS_Myrmica_sp",                "Formicidae",
  "ARA_Salticus_sp",               "Salticidae",
  "ARA_Pardosa_sp",                "Lycosidae",
  "INS_Calathus_fuscipes",         "Carabidae",
  "INS_Lasius_niger",              "Formicidae",
  "ARA_Pardosa_hortensis",         "Lycosidae",
  "INS_Harpalus_honestus",         "Carabidae",
  "INS_Harpalus_affinis",          "Carabidae",
  "INS_Ocypus_olens",              "Staphylinidae",
  "ARA_Oxyopes_lineatus",          "Oxyopidae",
  "INS_Ophonus_sabulicola",        "Carabidae",
  "INS_Ophonus_ardosiacus",        "Carabidae",
  "ARA_Clubiona_sp",               "Clubionidae",
  "INS_Harpalus_dimidiatus",       "Carabidae",
  "ARA_Phalangium_opilio",         "Phalangiidae",
  "INS_Drusilla_canaliculata",     "Staphylinidae",
  "ARA_Leptorchestes_sp",          "Salticidae",
  "ARA_Araneidae_sp",              "Araneidae",
  "ARA_Nuctenea_umbratica",        "Araneidae",
  "INS_Harpalus_rubripes",         "Carabidae",
  "ARA_Cheiracanthium_sp",         "Cheiracanthiidae",
  "ARA_Xysticus_sp",               "Thomisidae",
  "ARA_Pardosa_proxima",           "Lycosidae",
  "ARA_Linyphiidae_sp",            "Linyphiidae",
  "ARA_Tenuiphantes_tenuis",       "Linyphiidae",
  "INS_Carabus_problematicus",     "Carabidae",
  "INS_Formica_rufibarbis",        "Formicidae",
  "INS_Tetramorium_sp",            "Formicidae",
  "ARA_Salticidae_sp",             "Salticidae",
  "INS_Formica_sp",                "Formicidae",
  "ARA_Oxyopes_sp",                "Oxyopidae",
  "INS_Ophonus_azureus",           "Carabidae",
  "ARA_Oedothorax_sp",             "Linyphiidae",
  "ARA_Pseudeuophrys_sp",          "Salticidae",
  "ARA_Pseudeuophrys_erratica",    "Salticidae",
  "ARA_Argiope_bruennichi",        "Araneidae",
  "ARA_Trochosa_sp",               "Lycosidae",
  "ARA_Philodromus_sp",            "Philodromidae",
  "INS_Carabus_violaceus_purpurascens", "Carabidae",
  "ARA_Salticus_scenicus",         "Salticidae",
  "INS_Ophonus_grpuncticeps",      "Carabidae",
  "ARA_Evarcha_arcuata",           "Salticidae",
  "ARA_Enoplognatha_latimana",     "Theridiidae",
  "ARA_Pisaura_mirabilis",         "Pisauridae",
  "ARA_Anyphaena_accentuata",      "Anyphaenidae",
  "ARA_Araneus_diadematus",        "Araneidae",
  "INS_Camponotus_vagus",          "Formicidae",
  "INS_Crematogaster_scutellaris", "Formicidae",
  "INS_Formica_cunicularia",       "Formicidae"
)

Tab_reseau_All_species <- Tab_reseau_All_species %>%
  left_join(family_df, by = "predator")

Tab_reseau_All_species$prey <- sapply(Tab_reseau_All_species$prey, function(x) {
  parts <- unlist(strsplit(x, "_"))
  if (tolower(tail(parts, 1)) == "sp") {
    name <- parts[length(parts) - 1]
  } else {
    name <- tail(parts, 1)
  }
  name_clean <- paste0(toupper(substring(name, 1, 1)), tolower(substring(name, 2)))
  paste(name_clean, "sp")
})


# Remove Class indicator before species and clean species name
Tab_reseau_All_species <- Tab_reseau_All_species %>%
  mutate(
      # Replace "_" with a blank space
    predator = predator %>%
      str_remove("^[A-Z]{3}_") %>%
      str_replace_all("_", " ")
  )


Tab_reseau_All_species$prey <- str_to_sentence(Tab_reseau_All_species$prey)

predator_order_df <- Tab_reseau_All_species %>%
  select(predator, Predator_Class, Predator_Family) %>%
  distinct() %>%
  arrange(Predator_Class, Predator_Family, predator)

ordered_predators <- predator_order_df$predator


prey_order_df <- Tab_reseau_All_species %>%
  select(prey, Prey_Family, Prey_Class) %>%
  distinct() %>%
  arrange(desc(Prey_Family))

ordered_prey <- prey_order_df$prey

group_colors <- c(
  "Lobesia sp" = adjustcolor("red", alpha.f = 0.7),
  "Empoasca sp" = adjustcolor("green4", alpha.f = 0.7),
  "Scaphoideus sp" = adjustcolor("#4DAF4A", alpha.f = 0.7),
  "Daktulosphaira sp" = adjustcolor("#984EA3", alpha.f = 0.7)
)

group_to_cluster <- intersect(names(group_colors), unique(Tab_reseau_All_species$prey))
remaining_prey <- setdiff(unique(Tab_reseau_All_species$prey), group_to_cluster)


#ordered_sectors <- c(group_to_cluster, remaining_prey, ordered_predators)
ordered_sectors <- c(ordered_prey, ordered_predators)

prey_family_groups <- prey_order_df %>%
  filter(!is.na(Prey_Family)) %>%
  group_by(Prey_Family) %>%
  summarise(sectors = list(prey), .groups = "drop")



# Unique families list
pred_family_df <- unique(Tab_reseau_All_species[, c("predator", "Predator_Family")])

familles_unique <- unique(pred_family_df$Predator_Family)
family_colors <- c(
  "Anyphaenidae" = "#88BBD6",
  "Araneidae" = "#1B3B6F",
  "Cheiracanthiidae" = "#88BBD6",
  "Clubionidae" = "#1B3B6F",
  "Linyphiidae" = "#88BBD6",
  "Lycosidae" = "#1B3B6F",
  "Oxyopidae" = "#88BBD6",
  "Phalangiidae" = "#1B3B6F",
  "Philodromidae" = "#88BBD6",
  "Pisauridae" = "#1B3B6F",
  "Salticidae" = "#88BBD6",
  "Theridiidae" = "#1B3B6F",
  "Thomisidae" = "#88BBD6",
  "Carabidae" = "#C97D02",
  "Chrysopidae" = "#B6452C",
  "Formicidae" = "#7D2C45",
  "Staphylinidae" = "#C97D40"
)

family_colors <- sapply(family_colors, adjustcolor, alpha.f = 0.7)

pred_color_by_family <- setNames(family_colors[pred_family_df$Predator_Family], pred_family_df$predator)


unique_prey <- unique(Tab_reseau_All_species$prey)


# All names to color (predators + preys)
all_names <- unique(c(Tab_reseau_All_species$prey, Tab_reseau_All_species$predator))

# Grey as default color
grid.col <- setNames(rep("grey80", length(all_names)), all_names)

# Only replace predators by their family color
grid.col[names(pred_color_by_family)] <- pred_color_by_family

grid.col[names(group_colors)] <- group_colors


pdf("chord_diagram.pdf", width = 14, height = 14)

chordDiagram(
  Tab_reseau_All_species,
  grid.col = grid.col,
  annotationTrack = "grid",
  preAllocateTracks = list(
    list(track.height = max(strwidth(unlist(dimnames(Tab_reseau_All_species))))),
    list(track.height = 0.04),
    list(track.height = 0.02),
    list(track.height = 0.02)
  ),
  order = c(ordered_prey, ordered_predators),
  transparency = 0.50
)

# Prey/predator labels
circos.track(
  track.index = 4,
  panel.fun = function(x, y) {
    sector <- CELL_META$sector.index
    circos.text(
      x = CELL_META$xcenter,
      y = CELL_META$ylim[1],
      labels = parse(text = paste0("italic('", sector, "')")),
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.6
    ) },
  bg.border = NA
)

# Families
predator_family_df <- predator_order_df %>%
  filter(!is.na(Predator_Family)) %>%
  select(predator, Predator_Family)

family_groups <- predator_family_df %>%
  group_by(Predator_Family) %>%
  summarise(sectors = list(predator), .groups = "drop")

for (i in seq_len(nrow(family_groups))) {
  sectors <- family_groups$sectors[[i]]
  fam_name <- family_groups$Predator_Family[i]
  
  
  highlight.sector(
    sector.index = sectors,
    track.index = 1,
    col = NA,
    lwd = 1.2,
    text = fam_name,
    cex = 1.0,
    facing = "outside",
    niceFacing = TRUE,
    text.vjust = 6,
    text.track.height = 0.05
  )
}

for (i in seq_len(nrow(prey_family_groups))) {
  sectors <- prey_family_groups$sectors[[i]]
  fam_name <- prey_family_groups$Prey_Family[i]
  
  highlight.sector(
    sector.index = sectors,
    track.index = 1,
    col = NA,
    lwd = 1.2,
    text = fam_name,
    cex = 1.0,
    facing = "outside",
    niceFacing = TRUE,
    text.vjust = -1,
    text.track.height = 0.05
  )
}

dev.off()



# Pests network only

pdf("chord_diagram_pests.pdf", width = 14, height = 14)

Tab_reseau_All_species_Pests <- Tab_reseau_All_species %>%
  filter(prey %in% group_to_cluster)

chordDiagram(
  Tab_reseau_All_species_Pests,
  grid.col = grid.col,
  annotationTrack = "grid",
  preAllocateTracks = list(
    list(track.height = max(strwidth(unlist(dimnames(Tab_reseau_All_species_Pests))))),
    list(track.height = 0.04),
    list(track.height = 0.02),
    list(track.height = 0.02)
  ),
  order = ordered_sectors,
  transparency = 0.50
)

# Prey/predator labels
circos.track(
  track.index = 4,
  panel.fun = function(x, y) {
    circos.text(
      x = CELL_META$xcenter,
      y = CELL_META$ylim[1],
      labels = CELL_META$sector.index,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.8
    )
  },
  bg.border = NA
)

# Families
predator_family_df <- predator_order_df %>%
  filter(!is.na(Predator_Family)) %>%
  select(predator, Predator_Family)

family_groups <- predator_family_df %>%
  group_by(Predator_Family) %>%
  summarise(sectors = list(predator), .groups = "drop")

present_sectors <- get.all.sector.index()

for (i in seq_len(nrow(family_groups))) {
  sectors <- family_groups$sectors[[i]]
  
  sectors_to_highlight <- intersect(sectors, present_sectors)
  
  if (length(sectors_to_highlight) == 0) next
  
  fam_name <- family_groups$Predator_Family[i]
  
  highlight.sector(
    sector.index = sectors_to_highlight,
    track.index = 1,
    col = NA,
    lwd = 1.2,
    text = fam_name,
    cex = 1.0,
    facing = "outside",
    niceFacing = TRUE,
    text.vjust = 6
  )
}

dev.off()




# Identify predator species for pests (common_species) and missing predators for pests (missing_species)
# Species present in Tab_reseau_All_species
All_species <- unique(sort(Tab_reseau_All_species$predator))

# Species present in Tab_reseau_All_species_Pests
All_species_Pests <- unique(sort(Tab_reseau_All_species_Pests$predator))


missing_species <- setdiff(All_species, All_species_Pests) # 3 of 52 predator species are not predators for our pests
common_species <- intersect(All_species, All_species_Pests)


#===============
# XXVIII - Diagrammes des réseaux par Parc ===============
#===============

# Create Folder
dir.create("ChordDiagrams_byParc", showWarnings = FALSE)

# Parc list
liste_parcelles <- unique(tab_pij2_All_species_Seuil_1_percent$Parc)


for (parcelle in liste_parcelles) {

  # Filter data for each Parc
  Tab_reseau_All_species_Parc <- tab_pij2_All_species_Seuil_1_percent %>%
    filter(Parc == parcelle) %>%
    select(ReadName, Isp, PijComb) %>%
    rename(prey = ReadName, predator = Isp, Interaction_probability = PijComb)
  
  # Join families
  Tab_reseau_All_species_Parc <- Tab_reseau_All_species_Parc %>%
    left_join(prey_family_df, by = "prey") %>%
    left_join(family_df, by = "predator")
  
  # Label cleaning
  Tab_reseau_All_species_Parc$Predator_Class <- substr(Tab_reseau_All_species_Parc$predator, 1, 3)
  Tab_reseau_All_species_Parc$Prey_Class <- substr(Tab_reseau_All_species_Parc$prey, 1, 3)
  
  Tab_reseau_All_species_Parc$prey <- sapply(Tab_reseau_All_species_Parc$prey, function(x) {
    parts <- unlist(strsplit(x, "_"))
    if (tolower(tail(parts, 1)) == "sp") {
      name <- parts[length(parts) - 1]
    } else {
      name <- tail(parts, 1)
    }
    name_clean <- paste0(toupper(substring(name, 1, 1)), tolower(substring(name, 2)))
    paste(name_clean, "sp")
  })
  
  Tab_reseau_All_species_Parc <- Tab_reseau_All_species_Parc %>%
    mutate(predator = predator %>%
             str_remove("^[A-Z]{3}_") %>%
             str_replace_all("_", " "),
           prey = str_to_sentence(prey))
  
  # Taxas order
  predator_order_df <- Tab_reseau_All_species_Parc %>%
    select(predator, Predator_Class, Predator_Family) %>%
    distinct() %>%
    arrange(Predator_Class, Predator_Family, predator)
  
  ordered_predators <- predator_order_df$predator
  
  prey_order_df <- Tab_reseau_All_species_Parc %>%
    select(prey, Prey_Family, Prey_Class) %>%
    distinct() %>%
    arrange(desc(Prey_Family))
  
  ordered_prey <- prey_order_df$prey
  
  # Colors
  group_colors <- c(
    "Lobesia sp" = adjustcolor("red", alpha.f = 0.7),
    "Empoasca sp" = adjustcolor("green4", alpha.f = 0.7),
    "Scaphoideus sp" = adjustcolor("#4DAF4A", alpha.f = 0.7),
    "Daktulosphaira sp" = adjustcolor("#984EA3", alpha.f = 0.7)
  )
  remaining_prey <- setdiff(unique(Tab_reseau_All_species_Parc$prey), group_to_cluster)
  ordered_sectors <- c(ordered_prey, ordered_predators)
  
  prey_family_groups <- prey_order_df %>%
    filter(!is.na(Prey_Family)) %>%
    group_by(Prey_Family) %>%
    summarise(sectors = list(prey), .groups = "drop")
  
  pred_family_df <- unique(Tab_reseau_All_species_Parc[, c("predator", "Predator_Family")])
  
  family_colors <- c(
    "Anyphaenidae" = "#88BBD6",
    "Araneidae" = "#1B3B6A",
    "Cheiracanthiidae" = "#5FA6C7",
    "Clubionidae" = "#23497F",
    "Linyphiidae" = "#A2CBE4",
    "Lycosidae" = "#305B8A",
    "Oxyopidae" = "#76AED1",
    "Phalangiidae" = "#305B8A",
    "Philodromidae" = "#99C8DF",
    "Pisauridae" = "#193F6E",
    "Salticidae" = "#B1D4E8",
    "Theridiidae" = "#14406F",
    "Thomisidae" = "#7FBEDC",
    "Carabidae" = "#C97D02",
    "Chrysopidae" = "#B6452C",
    "Formicidae" = "#7D2C45",
    "Staphylinidae" = "#C97D40"
  )
  
  family_colors <- sapply(family_colors, adjustcolor, alpha.f = 0.7)
  
  pred_color_by_family <- setNames(family_colors[pred_family_df$Predator_Family], pred_family_df$predator)
  
  pred_color_by_family <- setNames(family_colors[pred_family_df$Predator_Family], pred_family_df$predator)
  
  all_names <- unique(c(Tab_reseau_All_species_Parc$prey, Tab_reseau_All_species_Parc$predator))
  grid.col <- setNames(rep("grey80", length(all_names)), all_names)
  grid.col[names(pred_color_by_family)] <- pred_color_by_family
  grid.col[names(group_colors)] <- group_colors
  
  
  Tab_reseau_All_species_Parc <- Tab_reseau_All_species_Parc %>%
    mutate(link_color = ifelse(prey %in% group_to_cluster, group_colors[prey], adjustcolor("grey80", alpha.f = 0.5)))
  
  
  # PDF creation
  pdf(paste0("ChordDiagrams_byParc/chord_diagram_", parcelle, ".pdf"), width = 14, height = 14)
  
  chordDiagram(
    Tab_reseau_All_species_Parc,
    grid.col = grid.col,
    annotationTrack = "grid",
    preAllocateTracks = list(
      list(track.height = max(strwidth(unlist(dimnames(Tab_reseau_All_species_Parc))))),
      list(track.height = 0.04),
      list(track.height = 0.02),
      list(track.height = 0.02)
    ),
    order = ordered_sectors,
    transparency = 0.5
  )
  
  circos.track(
    track.index = 4,
    panel.fun = function(x, y) {
      sector <- CELL_META$sector.index
      circos.text(
        x = CELL_META$xcenter,
        y = CELL_META$ylim[1],
        labels = parse(text = paste0("italic('", sector, "')")),
        facing = "clockwise",
        niceFacing = TRUE,
        adj = c(0, 0.5),
        cex = 0.6
      )
    },
    bg.border = NA
  )
  
  # Highlight predator families
  family_groups <- predator_order_df %>%
    filter(!is.na(Predator_Family)) %>%
    group_by(Predator_Family) %>%
    summarise(sectors = list(predator), .groups = "drop")
  
  for (i in seq_len(nrow(family_groups))) {
    highlight.sector(
      sector.index = family_groups$sectors[[i]],
      track.index = 1,
      col = NA,
      lwd = 1.2,
      text = family_groups$Predator_Family[i],
      cex = 1.0,
      facing = "outside",
      niceFacing = TRUE,
      text.vjust = 6,
      text.track.height = 0.05
    )
  }
  
  # Highlight prey families
  for (i in seq_len(nrow(prey_family_groups))) {
    highlight.sector(
      sector.index = prey_family_groups$sectors[[i]],
      track.index = 1,
      col = NA,
      lwd = 1.2,
      text = prey_family_groups$Prey_Family[i],
      cex = 1.0,
      facing = "outside",
      niceFacing = TRUE,
      text.vjust = -1,
      text.track.height = 0.05
    )
  }
  
  dev.off()
}


#===============
# Pests per Parc


for (parcelle in liste_parcelles) {
  
  Tab_reseau_All_species_Parc <- tab_pij2_All_species_Seuil_1_percent %>%
    filter(Parc == parcelle) %>%
    select(ReadName, Isp, PijComb) %>%
    rename(prey = ReadName, predator = Isp, Interaction_probability = PijComb) %>%
    left_join(prey_family_df, by = "prey") %>%
    left_join(family_df, by = "predator")
  
  # Label cleaning
  Tab_reseau_All_species_Parc$Predator_Class <- substr(Tab_reseau_All_species_Parc$predator, 1, 3)
  Tab_reseau_All_species_Parc$Prey_Class <- substr(Tab_reseau_All_species_Parc$prey, 1, 3)
  
  Tab_reseau_All_species_Parc$prey <- sapply(Tab_reseau_All_species_Parc$prey, function(x) {
    parts <- unlist(strsplit(x, "_"))
    if (tolower(tail(parts, 1)) == "sp") {
      name <- parts[length(parts) - 1]
    } else {
      name <- tail(parts, 1)
    }
    name_clean <- paste0(toupper(substring(name, 1, 1)), tolower(substring(name, 2)))
    paste(name_clean, "sp")
  })
  
  Tab_reseau_All_species_Parc <- Tab_reseau_All_species_Parc %>%
    mutate(predator = predator %>%
             str_remove("^[A-Z]{3}_") %>%
             str_replace_all("_", " "),
           prey = str_to_sentence(prey))
  
  # Filter pests only
  Tab_reseau_pests_Parc <- Tab_reseau_All_species_Parc %>%
    filter(prey %in% group_to_cluster)

  
  # Taxa order
  predator_order_df <- Tab_reseau_pests_Parc %>%
    select(predator, Predator_Class, Predator_Family) %>%
    distinct() %>%
    arrange(Predator_Class, Predator_Family, predator)
  
  ordered_predators <- predator_order_df$predator
  ordered_prey <- unique(Tab_reseau_pests_Parc$prey)
  ordered_sectors <- c(ordered_prey, ordered_predators)
  
  
  
  # Colors
  
  # All names to color (predators + preys)
  all_names_Parc <- unique(c(Tab_reseau_pests_Parc$prey, Tab_reseau_pests_Parc$predator))
  
  # Grey as default color
  grid.col <- setNames(rep("grey80", length(all_names_Parc)), all_names_Parc)
  
  # Unique families list
  pred_family_df <- unique(Tab_reseau_pests_Parc[, c("predator", "Predator_Family")])
  
  familles_unique <- unique(pred_family_df$Predator_Family)
  family_colors <- c(
    "Anyphaenidae" = "#88BBD6",
    "Araneidae" = "#1B3B6A",
    "Cheiracanthiidae" = "#5FA6C7",
    "Clubionidae" = "#23497F",
    "Linyphiidae" = "#A2CBE4",
    "Lycosidae" = "#305B8A",
    "Oxyopidae" = "#76AED1",
    "Phalangiidae" = "#305B8A",
    "Philodromidae" = "#99C8DF",
    "Pisauridae" = "#193F6E",
    "Salticidae" = "#B1D4E8",
    "Theridiidae" = "#14406F",
    "Thomisidae" = "#7FBEDC",
    "Carabidae" = "#C97D02",
    "Chrysopidae" = "#B6452C",
    "Formicidae" = "#7D2C45",
    "Staphylinidae" = "#C97D40"
  )
  
  family_colors <- sapply(family_colors, adjustcolor, alpha.f = 0.7)
  
  pred_color_by_family <- setNames(family_colors[pred_family_df$Predator_Family], pred_family_df$predator)
  
  
  # Only replace predators by their family color
  grid.col[names(pred_color_by_family)] <- pred_color_by_family
  
  grid.col[names(group_colors)] <- group_colors
  
  grid.col[names(pred_color_by_family)] <- pred_color_by_family
  
  
  # Binds color (color if pest, grey if not)
  Tab_reseau_pests_Parc <- Tab_reseau_pests_Parc %>%
    mutate(link_color = ifelse(
      prey %in% names(group_colors),
      group_colors[prey],
      adjustcolor("grey80", alpha.f = 0.5)
    ))
  
  # PDF
  pdf(paste0("ChordDiagrams_byParc/chord_diagram_pests_", parcelle, ".pdf"), width = 14, height = 14)
  
  circos.clear()
  chordDiagram(
    Tab_reseau_pests_Parc,
    grid.col = grid.col,
    col = Tab_reseau_pests_Parc$link_color,
    order = ordered_sectors,
    annotationTrack = "grid",
    preAllocateTracks = list(
      list(track.height = max(strwidth(unlist(dimnames(Tab_reseau_pests_Parc))))),
      list(track.height = 0.04),
      list(track.height = 0.02),
      list(track.height = 0.02)
    ),
    transparency = 0.5
  )
  
  # Names in italic
  circos.track(
    track.index = 4,
    panel.fun = function(x, y) {
      sector <- CELL_META$sector.index
      circos.text(
        x = CELL_META$xcenter,
        y = CELL_META$ylim[1],
        labels = parse(text = paste0("italic('", sector, "')")),
        facing = "clockwise",
        niceFacing = TRUE,
        adj = c(0, 0.5),
        cex = 0.6
      )
    },
    bg.border = NA
  )
  
  # Highlight families
  family_groups <- predator_order_df %>%
    filter(!is.na(Predator_Family)) %>%
    group_by(Predator_Family) %>%
    summarise(sectors = list(predator), .groups = "drop")
  
  for (i in seq_len(nrow(family_groups))) {
    highlight.sector(
      sector.index = family_groups$sectors[[i]],
      track.index = 1,
      col = NA,
      lwd = 1.2,
      text = family_groups$Predator_Family[i],
      cex = 1.0,
      facing = "outside",
      niceFacing = TRUE,
      text.vjust = 6,
      text.track.height = 0.05
    )
  }
  
  dev.off()
}

#===============
# XXIX - Common and missing species for each Parc ===============
#===============

# Identify predator species for pests (common_species) and missing predators for pests (missing_species)
# Per Parc values

group_colors_bis <- c(
  "INS_Tortricidae_Lobesia" = adjustcolor("red", alpha.f = 0.7),
  "INS_Cicadellidae_Empoasca" = adjustcolor("green4", alpha.f = 0.7),
  "INS_Cicadellidae_Scaphoideus" = adjustcolor("#4DAF4A", alpha.f = 0.7),
  "INS_Phylloxeridae_Daktulosphaira" = adjustcolor("#984EA3", alpha.f = 0.7)
)


# Tab for common and missing species for each Parc

Tab_reseau_All_species_bis <- tab_pij2_All_species_Seuil_1_percent %>%
  select(ReadName, Isp, PijComb, Parc)

colnames(Tab_reseau_All_species_bis) <- c("prey", "predator", "Interaction_probability", "Parc")

group_to_cluster_bis <- intersect(names(group_colors_bis), unique(Tab_reseau_All_species_bis$prey))

Tab_reseau_All_species_Pests_bis <- Tab_reseau_All_species_bis %>%
  filter(prey %in% group_to_cluster_bis)


parcelles <- unique(Tab_reseau_All_species_bis$Parc)

missing_species_list <- list()
common_species_list <- list()

summary_list <- list()

for (parc in parcelles) {
  data_all <- Tab_reseau_All_species_bis[Tab_reseau_All_species_bis$Parc == parc, ]
  data_pests <- Tab_reseau_All_species_Pests_bis[Tab_reseau_All_species_Pests_bis$Parc == parc, ]
  
  # Present species
  All_species <- unique(sort(data_all$predator))
  All_species_Pests <- unique(sort(data_pests$predator))
  
  # Missing and common species
  missing_species <- setdiff(All_species, All_species_Pests)
  common_species <- intersect(All_species, All_species_Pests)
  
  # Storage
  missing_species_list[[parc]] <- missing_species
  common_species_list[[parc]] <- common_species
  
  summary_list[[parc]] <- data.frame(
    Parc = parc,
    n_missing = length(missing_species),
    n_common = length(common_species),
    n_total = length(All_species)
  )
}

Common_and_missing_predator_species_per_Parc_df <- do.call(rbind, summary_list)


write.table(Common_and_missing_predator_species_per_Parc_df, file = "Nb of missing and common species in chord diagrams per Parc.txt", sep = "\t", row.names = FALSE)
