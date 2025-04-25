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

metriques_reseau_All_species_Parc <- final_métriques[rownames(final_métriques) %in% c("vulnerability.LL", "connectance", "nestedness", "generality.HL", "niche.overlap.HL", "niche.overlap.LL", "modularity Q", "H2"), ]

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


dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(species_level_df)





##########
# ACP métriques réseaux
##########

#scale data
ACP_metriques_reseaux <- dataPCA_Adrien_2 %>%
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

dataPCA_Adrien_2 <- dataPCA_Adrien_2 %>%
  mutate(Tab_models[,c("Parc", "Npred", "NBcica", "NBtord", "NBphyl", "NBAll_pests", "Taux_accroiss_NBcica", "Taux_accroiss_NBphyl", "Taux_accroiss_NBAll_pests")])
         
dataPCA_Adrien_2 <- as.data.frame(apply(dataPCA_Adrien_2, 2, as.numeric))


ggplot(dataPCA_Adrien_2, aes(x = HSNtot, y = mean_species_specificity)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm")+
  labs(x = "HSNtot", y = "mean_species_specificity")


tab_correlation_1 <- dataPCA_Adrien_2 %>%
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

#############
# Modules du réseau par Parc
#############

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



#####################

# Nouveaux modèles 23_04_2025

#####################

library(dplyr)

dataPCA_Adrien_2$Parc <- Tab_models$Parc
# On commence par enlever de df2 les colonnes qui sont déjà dans df1, sauf 'Parc'
df2_filtered <- dataPCA_Adrien_2 %>% select(-any_of(setdiff(intersect(names(Tab_models), names(dataPCA_Adrien_2)), "Parc")))


# Puis on fait un left_join pour ajouter les colonnes uniques de df2 à df1, en faisant correspondre 'Parc'
Tableau_model_final <- Tab_models %>% left_join(df2_filtered, by = "Parc")

Tableau_model_final$Site <- substr(Tableau_model_final$Parc, 1, nchar(Tableau_model_final$Parc) - 1)

Tableau_model_final$Site <- as.factor(Tableau_model_final$Site)


########
# Modèles
########

#M1a: Predator diversity ~ HSN * AB + (1|site)
#M1b: Predator diversity ~ HSN * IFTTot + (1|site)
#M2a:Prey diversity ~ HSN + AB + (1|site)
#M2b:Prey diversity ~ HSN + IFTTot + (1|site)


#####################
# Mod1a
#####################    

Mod1a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Pred_Shannon_diversity)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Pred_Shannon_diversity") +
  theme_minimal()        
Mod1a
ggsave(filename = "Mod1a.jpeg", plot = Mod1a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod1a <- lmer(Pred_Shannon_diversity ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod1a)
summary(Mod1a)

#####################
# Mod1b
##################### 

Mod1b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = Pred_Shannon_diversity)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "Pred_Shannon_diversity") +
  theme_minimal()        
Mod1b
ggsave(filename = "Mod1b.jpeg", plot = Mod1b, width = 11, height = 8)

Mod1b <- lmer(Pred_Shannon_diversity ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod1b)
summary(Mod1b)


#####################
# Mod2a
#####################    

Mod2a <- ggplot(Tableau_model_final, aes(x = HSNtot, y = prey_shannon_diversity)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "prey_shannon_diversity") +
  theme_minimal()        
Mod2a
ggsave(filename = "Mod2a.jpeg", plot = Mod2a, width = 11, height = 8)

library(lme4)
library(lmerTest)
Mod2a <- lmer(prey_shannon_diversity ~ HSNtot * cult + (1|Site), data = Tableau_model_final)
print(Mod2a)
summary(Mod2a)

#####################
# Mod2b
##################### 

Mod2b <- ggplot(Tableau_model_final, aes(x = HSNtot, y = prey_shannon_diversity)) +
  geom_point(aes(color = IFTTot)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "HSNtot",
       y = "prey_shannon_diversity") +
  theme_minimal()        
Mod2b
ggsave(filename = "Mod2b.jpeg", plot = Mod2b, width = 11, height = 8)

Mod2b <- lmer(prey_shannon_diversity ~ HSNtot + IFTTot + Int_ti + (1|Site), data = Tableau_model_final)
print(Mod2b)
summary(Mod2b)



#####################
# Mod3a
#####################    

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

#####################
# Mod3b
##################### 

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


#####################
# Mod4a
#####################    

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

#####################
# Mod4b
##################### 

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



#####################
# Mod5a
#####################    

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

#####################
# Mod5b
##################### 

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


#####################
# Mod6a
#####################    

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

#####################
# Mod6b
##################### 

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


#####################
# Mod7a
#####################    

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

#####################
# Mod7b
##################### 

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


#####################
# Mod8a
#####################    

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

#####################
# Mod8b
##################### 

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


#####################
# Mod9a
#####################    

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

#####################
# Mod9b
##################### 

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


#####################
# Mod10a
#####################    

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

#####################
# Mod10b
##################### 

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


#####################
# Mod11a
#####################    

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

#####################
# Mod11b
##################### 

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


#####################
# Mod12a
#####################    

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

#####################
# Mod12b
##################### 

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


#####################
# Mod13a
#####################    

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

#####################
# Mod13b
##################### 

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


#####################
# Mod14a
#####################    

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

#####################
# Mod14b
##################### 

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


#####################
# Mod15a
#####################    

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

#####################
# Mod15b
##################### 

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


#####################
# Mod16a
#####################    

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

#####################
# Mod16b
##################### 

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

#####################
# Mod17a
#####################    

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

#####################
# Mod17b
##################### 

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


#####################
# Mod18a
#####################    

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
Mod18a <- lmer(niche.overlap.HL ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod18a)
summary(Mod18a)


#####################
# Mod18b
#####################    

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
Mod18b <- lmer(Pianka_index ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod18b)
summary(Mod18b)



#####################
# Mod18c
#####################    

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
Mod18c <- lmer(vulnerability.LL ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod18c)
summary(Mod18c)

#####################
# Mod18d
#####################    

Mod18d <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = generality.HL)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "generality.HL") +
  theme_minimal()        
Mod18d
ggsave(filename = "Mod18d.jpeg", plot = Mod18d, width = 11, height = 8)

Mod18d <- lmer(generality.HL ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod18d)
summary(Mod18d)


#####################
# Mod19a
#####################    

Mod19a <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = `modularity Q`)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "`modularity Q`") +
  theme_minimal()        
Mod19a
ggsave(filename = "Mod19a.jpeg", plot = Mod19a, width = 11, height = 8)

Mod19a <- lmer(`modularity Q` ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod19a)
summary(Mod19a)


#####################
# Mod19b
#####################    

Mod19b <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = H2)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "H2") +
  theme_minimal()        
Mod19b
ggsave(filename = "Mod19b.jpeg", plot = Mod19b, width = 11, height = 8)

Mod19b <- lmer(H2 ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod19b)
summary(Mod19b)


#####################
# Mod20a
#####################    

Mod20a <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = mean_degree)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "mean_degree") +
  theme_minimal()        
Mod20a
ggsave(filename = "Mod20a.jpeg", plot = Mod20a, width = 11, height = 8)

Mod20a <- lmer(mean_degree ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod20a)
summary(Mod20a)

#####################
# Mod20b
#####################    

Mod20b <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = mean_effective_partners)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "mean_effective_partners") +
  theme_minimal()        
Mod20b
ggsave(filename = "Mod20b.jpeg", plot = Mod20b, width = 11, height = 8)

Mod20b <- lmer(mean_effective_partners ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod20b)
summary(Mod20b)


#####################
# Mod20c
#####################    

Mod20c <- ggplot(Tableau_model_final, aes(x = Prey_Shannon_Div, y = mean_species_specificity)) +
  geom_point(aes(color = cult)) +
  geom_text_repel(aes(label = Parc), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Droite du modèle global
  labs(x = "Prey_Shannon_Div",
       y = "mean_species_specificity") +
  theme_minimal()        
Mod20c
ggsave(filename = "Mod20c.jpeg", plot = Mod20c, width = 11, height = 8)

Mod20c <- lmer(mean_species_specificity ~ Prey_Shannon_Div + Pred_Shannon_diversity + (1|Site), data = Tableau_model_final)
print(Mod20c)
summary(Mod20c)


#####################
# Mod21a
#####################    

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


#####################
# Mod21b
#####################    

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


#####################
# Mod22a
#####################    

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


#####################
# Mod22b
#####################    

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


#####################
# Mod23a
#####################    

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


#####################
# Mod23b
#####################    

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


#####################
# Mod24a
#####################    

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


#####################
# Mod24b
#####################    

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


#####################
# Mod25a
#####################    

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


#####################
# Mod25b
#####################    

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


#####################
# Mod26a
#####################    

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


#####################
# Mod26b
#####################    

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

#####################
# Mod27a
#####################    

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


#####################
# Mod27b
#####################    

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


#####################
# Mod28a
#####################    

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


#####################
# Mod28b
#####################    

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


#####################
# Mod29a
#####################    

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


#####################
# Mod29b
#####################    

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


######
# Analyses multivariées
######
vegan::rda()
ade4::