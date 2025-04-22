setwd("C:/Users/Alexandre_Dosset/Desktop/Antoptic")

#####Importation des packages#####
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

#####Importation des donn?es#####

TAB <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/TAB.txt", header = TRUE, sep = "")
tab_pij2 <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/tab_pij2.txt", header = TRUE, sep = "")
tab_pij2 <- tab_pij2 %>% dplyr::filter(ReadName != "Malacostraca_Potamidae_Longpotamon") # Retirer une espèce de crustacés qui est un artefact
#################################

# I - Préparation des données descriptives =======================================================================
# ===== I-1 - Créer un data frame pour chaque parcelle -----------------------------------------------------

unique(tab_pij2$Parc)

# Diviser le tableau initial en plusieurs tableaux par la colonne "Parc"
list_of_dataframes <- split(tab_pij2, tab_pij2$Parc)

# Afficher les noms des tableaux créés
names(list_of_dataframes)


# Utiliser une boucle pour assigner chaque tableau à une variable distincte
for (parc_name in names(list_of_dataframes)) {
  assign(parc_name, list_of_dataframes[[parc_name]])
}


# ===== I-2 - Calcul du Ratio proies détectées (pijComb<0,01)/ proies non-détectées (pijComb>0,01) ---------

# Compter le nombre de valeurs dans la colonne pijComb inférieures à 0,01
nb_inf_0_01 <- sum(tab_pij2$PijComb < 0.01, na.rm = TRUE)
# Compter le nombre de valeurs dans la colonne pijComb supérieures à 0,01
nb_sup_0_01 <- sum(tab_pij2$PijComb > 0.01, na.rm = TRUE)

# Calculer le ratio
Ratio_detected_not_detected <- nb_inf_0_01 / nb_sup_0_01
# Afficher le ratio
print(Ratio_detected_not_detected)

# ===== I-3 - Calcul du Ratio proies détectées (pijComb<0,01)/ proies totales ------------------------------

# Compter le nombre de valeurs dans la colonne pijComb
nb_tot <- length(tab_pij2$PijComb)

# Calculer le ratio
Ratio_detected_total <- nb_inf_0_01 / nb_tot
# Afficher le ratio
print(Ratio_detected_total)

# ===== I-4 - Calcul des Ratios pour chaque valeur de "Parc" -----------------------------------------------

ratios_by_parc <- tab_pij2 %>%
  group_by(Parc) %>%
  summarise(
    nb_inf_0_01 = sum(PijComb < 0.01, na.rm = TRUE),
    nb_sup_0_01 = sum(PijComb > 0.01, na.rm = TRUE),
    nb_tot = n()
  ) %>%
  mutate(
    Ratio_detected_not_detected = nb_inf_0_01 / nb_sup_0_01,
    Ratio_detected_total = nb_inf_0_01 / nb_tot
  )

# Afficher les résultats
print(ratios_by_parc)


# II - Préparation des données de travail ========================================================================
# ===== II-1 - Prendre un sous-échantillon des espèces présentes sur les 10 sites (totalité) ---------------

tab_pij2_unique <- tab_pij2 %>%
  distinct(Isp, Parc)

# Ajouter une colonne pour indiquer la présence
tab_pij2_unique <- tab_pij2_unique %>%
  mutate(presence = 1)

# Transformer les valeurs de "Parc" en colonnes
tab_pij2_unique_wide <- tab_pij2_unique %>%
  pivot_wider(names_from = Parc, values_from = presence, values_fill = list(presence = 0))

# Transformer les valeurs de "Parc" en colonnes
tab_pij2_nb_in_parc <- tab_pij2_unique_wide
tab_pij2_nb_in_parc$nb_in_parc <- rowSums(tab_pij2_unique_wide[, -1])

# Supprimer les lignes qui contiennent une valeur de 0 dans les colonnes sauf la première
tab_pij2_present_in_all_parc<- tab_pij2_unique_wide %>%
  filter(if_all(-1, ~ . != 0))

# Affichage du tableau filtré et modifié
print(tab_pij2_present_in_all_parc)


#############
#### PCA on environmental data to examine domensions of land use intensity
############

library(FactoMineR)
library(factoextra)

head(data_ACP)

#scale data

dataPCA_Adrien <- data_ACP %>%
  select(HSNtot, IFTHer, IFTIns, IFTFon, Int_ti)

dataPCA_Adrien_2 <- dataPCA_Adrien
dataPCA_Adrien_2$IFTTot <- dataPCA_Adrien_2$IFTHer + dataPCA_Adrien_2$IFTIns + dataPCA_Adrien_2$IFTFon

dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  select(HSNtot, IFTTot, Int_ti)


dataPCAscale<-scale(dataPCA_Adrien_2) 

resPCA<- PCA(dataPCAscale, scale.unit=TRUE)

fviz_eig(resPCA,add.labels=T)
#fviz_pca_ind(resPCA,repel=T,col.ind="cos2")

#extraction des deux axes : local land use intensity and landscape land use intensity

IFTLUI<-resPCA$ind$coord[,2]
LandscapeLUI<-resPCA$ind$coord[,1]

#add data LUIlocal and Landscape to data
dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(dataPCA_Adrien, IFTLUI, LandscapeLUI)

dataPCA_Adrien_2$Parc <- data_ACP$Parc

head(dataPCA_Adrien_2)


dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  distinct(Parc, .keep_all = TRUE)

################

# Shannon Div Predators

################

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

dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(tab_pred_shannon_div, tab_pred_RS)

ggplot(dataPCA_Adrien_2, aes(x = Predator_Shannon_Div, y = Predator_Species_Richness, color = Parc)) +
  geom_point(size = 3) +
  geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
  labs(x = "Predator_Shannon_Div", y = "Predator_Species_Richness")


########
# Somme abondances totale par parcelles
########

table_Abondances_totale <- table_pred_abondance %>%
  filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))

table_Abondances_totale <- table_Abondances_totale %>%
rowwise()%>%
  mutate(Abondance_tot = sum(c_across(where(is.numeric))))
          
table_Abondances_totale <- table_Abondances_totale %>%
  select(Parc, Abondance_tot)

dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(table_Abondances_totale)

ggplot(dataPCA_Adrien_2, aes(x = Predator_Shannon_Div, y = Abondance_tot, color = Parc)) +
  geom_point(size = 3) +
  geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
  labs(x = "Predator_Shannon_Div", y = "Abondance_tot")

########
# Diversité proies (NGS)
########

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

dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(tab_prey_shannon_div)

################
# pianka_df
################

dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(pianka_df)

################
# metriques reseaux par Parc
################

metriques_reseau_All_species_Parc <- final_métriques[rownames(final_métriques) %in% c("vulnerability.LL", "connectance", "nestedness", "generality.HL", "niche.overlap.HL", "niche.overlap.LL", "modularity Q"), ]

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


t_metrics_Parc <- transpose(metriques_reseau_All_species_Parc)
colnames(t_metrics_Parc) <- rownames(metriques_reseau_All_species_Parc)
rownames(t_metrics_Parc) <- colnames(metriques_reseau_All_species_Parc)

t_metrics_Parc <- t_metrics_Parc[-11,]

t_metrics_Parc <- t_metrics_Parc %>%
  tibble::rownames_to_column(var = "Parc")

dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(t_metrics_Parc)


ggplot(dataPCA_Adrien_2, aes(x = Pianka_index, y = niche.overlap.LL, color = Parc)) +
  geom_point(size = 3) +
  geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
  labs(x = "Pianka_index", y = "niche.overlap.LL")


##############
# indices réseaux à l'échelle des proies d'intérêt
##############

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
  
  # Convertir en data frame
  df_deg <- as.data.frame(deg)
  df_eff <- as.data.frame(eff)
  
  # Garder uniquement les espèces d'intérêt
  deg_values <- df_deg[rownames(df_deg) %in% species_of_interest, , drop = FALSE]
  eff_values <- df_eff[rownames(df_eff) %in% species_of_interest, , drop = FALSE]
  
  # Si certaines espèces sont absentes, les ajouter avec NA
  missing_species <- setdiff(species_of_interest, rownames(deg_values))
  for (sp in missing_species) {
    deg_values[sp, ] <- NA
    eff_values[sp, ] <- NA
  }
  
  # Ordonner les espèces comme dans species_of_interest
  deg_values <- deg_values[species_of_interest, , drop = FALSE]
  eff_values <- eff_values[species_of_interest, , drop = FALSE]
  
  # Créer un data frame avec les valeurs et noms de colonnes explicites
  result_row <- data.frame(
    Parc = parc,
    t(deg_values),
    t(eff_values)
  )
  
  # Renommer les colonnes
  colnames(result_row)[-1] <- c(
    paste0("degree_", species_of_interest),
    paste0("effective_partners_", species_of_interest)
  )
  
  # Stocker le résultat
  results_list[[parc]] <- result_row
}

# Combiner tous les résultats
final_df <- do.call(rbind, results_list)
rownames(final_df) <- NULL


dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(final_df)


##########
# ACP métriques réseaux
##########

#scale data
ACP_metriques_reseaux <- dataPCA_Adrien_2 %>%
  #select(Pianka_index, connectance, `modularity Q`, nestedness, niche.overlap.HL, niche.overlap.LL, generality.HL, vulnerability.LL)
select(Predator_Shannon_Div, HSNtot, IFTTot, Int_ti, IFTLUI, LandscapeLUI,Pianka_index, connectance, `modularity Q`, nestedness, niche.overlap.HL, niche.overlap.LL, generality.HL, vulnerability.LL,
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


dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(Tab_models[,c("Parc", "Npred")])
         
dataPCA_Adrien_2 <- as.data.frame(apply(dataPCA_Adrien_2, 2, as.numeric))


dataPCA_Adrien_2$vulnerability.LL
ggplot(dataPCA_Adrien_2, aes(x = vulnerability.LL, y = Npred)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm")+
  labs(x = "vulnerability.LL", y = "Npred")
