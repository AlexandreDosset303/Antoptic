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

          #[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
          #[[[[[[[[[[[[[[[ Il y a 3 espèces présentes dans tous les sites ]]]]]]]]]]]]]]]
          #[[[[[[[[[[[ INS_Chrysopidae_sp, ARA_Pardosa_sp et INS_Lasius_niger ]]]]]]]]]]]
          #[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
        
        
# III - Liens entre abondance de prédateurs et diversité des proies (proies détectées) ===========================
      # ===== III-1 - Préparation des données --------------------------------------------------------------------        
            # ===== III-1-a - Pour les prédateurs INS_Chrysopidae_sp, ARA_Pardosa_sp et INS_Lasius_niger ---------

                # Liste des valeurs à conserver dans la colonne "Isp"
                Species_in_all_Parc <- c("INS_Chrysopidae_sp", "ARA_Pardosa_sp", "INS_Lasius_niger")
                
                # Filtrer les lignes où la colonne "to_keep" a la valeur "oui"
                TAB_Kept <- TAB %>%
                  filter(to_keep == "oui")
                
                # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs spécifiques dans "Isp"
                TAB_Species_in_all_Parc <- subset(TAB_Kept, Isp %in% Species_in_all_Parc)
                # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs spécifiques dans "Isp"
                tab_pij2_Species_in_all_Parc <- subset(tab_pij2, Isp %in% Species_in_all_Parc)
                
                # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs "PijComb" > O.O1
                tab_pij2_Species_in_all_Parc_Seuil_1_percent <- tab_pij2_Species_in_all_Parc %>% filter(PijComb > 0.01)
                
                # Compter le nombre de valeurs uniques dans la colonne "ind" (Abondance totale des 3 espèces)
                TAB_nombre_valeurs_uniques <- length(unique(TAB_Species_in_all_Parc$ind))
                
                # Compter le nombre de valeurs uniques dans la colonne "ind" pour chaque valeur unique de "Isp"
                Abundance_sp_in_all_Parc <- aggregate(ind ~ Isp, data = TAB_Species_in_all_Parc, FUN = function(x) length(unique(x)))
                Abundance_sp_in_all_Parc <- rename(Abundance_sp_in_all_Parc, Abundance = ind)
        
            # ===== III-1-b - Pour chacun des trois prédateurs ---------------------------------------------------        
                #[[[[[[[[[[[]]]]]]]]]]]
                # INS_Chrysopidae_sp
                #[[[[[[[[[[[]]]]]]]]]]]
                # Filtrage du dataframe pour conserver uniquement les lignes avec INS_Chrysopidae_sp
                TAB_INS_Chrysopidae_sp <- subset(TAB, Isp == "INS_Chrysopidae_sp")
                
                #[[[[[[[[[[[]]]]]]]]]]]
                # ARA_Pardosa_sp
                #[[[[[[[[[[[]]]]]]]]]]]
                # Filtrage du dataframe pour conserver uniquement les lignes avec ARA_Pardosa_sp
                TAB_ARA_Pardosa_sp <- subset(TAB_Kept, Isp == "ARA_Pardosa_sp")
                
                #[[[[[[[[[[[]]]]]]]]]]]
                # INS_Lasius_niger
                #[[[[[[[[[[[]]]]]]]]]]]
                # Filtrage du dataframe pour conserver uniquement les lignes avec INS_Lasius_niger
                TAB_INS_Lasius_niger <- subset(TAB_Kept, Isp == "INS_Lasius_niger")

      # ===== III-2 - Diversité de proies = f(Abondance prédateurs) ----------------------------------------------
            # ===== III-2-a - Pour chacun des trois prédateurs ---------------------------------------------------
                # Il faut transformer le tableau en matrice contenant les proies en colonne et les prédateurs en ligne.
                # Les valeurs de la matrice indiquent la présence ou l'absence de la relation prédateur-proie.
                
                # Garder l'information sur la parcelle
                tab_pij2_Species_in_all_Parc_Seuil_1_percent$ParcIsp <- paste(tab_pij2_Species_in_all_Parc_Seuil_1_percent$Parc, tab_pij2_Species_in_all_Parc_Seuil_1_percent$Isp, sep = "_")
                tab_pij2_Species_in_all_Parc_Seuil_1_percent$ParcReadName <- paste(tab_pij2_Species_in_all_Parc_Seuil_1_percent$Parc, tab_pij2_Species_in_all_Parc_Seuil_1_percent$ReadName, sep = "_")
    
                # Ajouter une colonne 'Presence' avec des valeurs 1 pour indiquer la présence d'une interaction
                tab_pij2_presence <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                mutate(Presence = 1)
                
                # Créer la matrice de PijComb
                tab_pij2_PijComb <- tab_pij2_Species_in_all_Parc_Seuil_1_percent[,c("PijComb", "ParcIsp", "ParcReadName")] %>%
                pivot_wider(names_from = ParcIsp, values_from = PijComb)

            # ===== III-2-b - Calculer le nombre total de prédateurs pour chaque parcelle ------------------------

                predator_abundance <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  group_by(Parc) %>%
                  summarise(predator_abundance = sum(!is.na(Isp)))  # Nombre total de prédateurs par parcelle
                
                INS_Chrysopidae_sp_abundance <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  filter(Isp == "INS_Chrysopidae_sp") %>%
                  group_by(Parc) %>%
                  summarise(INS_Chrysopidae_sp_abundance = sum(!is.na(Isp)))  # Nombre total de prédateurs par parcelle
                
                INS_Lasius_niger_abundance <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  filter(Isp == "INS_Lasius_niger") %>%
                  group_by(Parc) %>%
                  summarise(INS_Lasius_niger_abundance = sum(!is.na(Isp)))  # Nombre total de prédateurs par parcelle
                
                ARA_Pardosa_sp_abundance <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  filter(Isp == "ARA_Pardosa_sp") %>%
                  group_by(Parc) %>%
                  summarise(ARA_Pardosa_sp_abundance = sum(!is.na(Isp)))  # Nombre total de prédateurs par parcelle
                
            # ===== III-2-c - Calculer la fréquence de chaque proie pour chaque parcelle -------------------------

                prey_freq <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  group_by(Parc, ReadName) %>%
                  summarise(freq = n()) %>%
                  ungroup()
                
                INS_Chrysopidae_sp_prey_freq <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  filter(Isp == "INS_Chrysopidae_sp") %>%
                  group_by(Parc, ReadName) %>%
                  summarise(freq = n()) %>%
                  ungroup()
                
                INS_Lasius_niger_prey_freq <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  filter(Isp == "INS_Lasius_niger") %>%
                  group_by(Parc, ReadName) %>%
                  summarise(freq = n()) %>%
                  ungroup()
                
                ARA_Pardosa_sp_prey_freq <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
                  filter(Isp == "ARA_Pardosa_sp") %>%
                  group_by(Parc, ReadName) %>%
                  summarise(freq = n()) %>%
                  ungroup()
  
            # ===== III-2-d - Calculer la diversité de Shannon des proies pour chaque parcelle ---------------------

                prey_shannon_diversity <- prey_freq %>%
                  group_by(Parc) %>%
                  summarise(prey_shannon_diversity = -sum(freq / sum(freq) * log(freq / sum(freq)))) %>%
                  ungroup()
                
                INS_Chrysopidae_sp_prey_shannon_diversity <- INS_Chrysopidae_sp_prey_freq %>%
                  group_by(Parc) %>%
                  summarise(INS_Chrysopidae_sp_prey_shannon_diversity = -sum(freq / sum(freq) * log(freq / sum(freq)))) %>%
                  ungroup()
                
                INS_Lasius_niger_prey_shannon_diversity <- INS_Lasius_niger_prey_freq %>%
                  group_by(Parc) %>%
                  summarise(INS_Lasius_niger_prey_shannon_diversity = -sum(freq / sum(freq) * log(freq / sum(freq)))) %>%
                  ungroup()
                
                ARA_Pardosa_sp_prey_shannon_diversity <- ARA_Pardosa_sp_prey_freq %>%
                  group_by(Parc) %>%
                  summarise(ARA_Pardosa_sp_prey_shannon_diversity = -sum(freq / sum(freq) * log(freq / sum(freq)))) %>%
                  ungroup()
  
            # ===== III-2-e - Joindre les données sur la diversité de Shannon et le nombre total de prédateurs par parcelle ----
                              
                Ratio_prey_shannon_diversity_predator_abundance <- inner_join(predator_abundance, prey_shannon_diversity, by = "Parc")
                
                INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance <- inner_join(INS_Chrysopidae_sp_abundance, INS_Chrysopidae_sp_prey_shannon_diversity, by = "Parc")
                
                INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance <- inner_join(INS_Lasius_niger_abundance, INS_Lasius_niger_prey_shannon_diversity, by = "Parc")
                
                ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance <- inner_join(ARA_Pardosa_sp_abundance, ARA_Pardosa_sp_prey_shannon_diversity, by = "Parc")
                
            # ===== III-2-f - Standardiser les colonnes predator_abundance et prey_shannon_diversity -------------
              
                Ratio_prey_shannon_diversity_predator_abundance_scaled <- Ratio_prey_shannon_diversity_predator_abundance %>%
                  mutate(predator_abundance = scale(predator_abundance),
                         prey_shannon_diversity = scale(prey_shannon_diversity))
                
                INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance$INS_Chrysopidae_sp_abundance <- as.numeric(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance$INS_Chrysopidae_sp_abundance)
                INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance$INS_Chrysopidae_sp_prey_shannon_diversity <- as.numeric(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance$INS_Chrysopidae_sp_prey_shannon_diversity)
                INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled <- INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance %>%
                  mutate(predator_abundance = scale(INS_Chrysopidae_sp_abundance),
                         prey_shannon_diversity = scale(INS_Chrysopidae_sp_prey_shannon_diversity))
                
                INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance$INS_Lasius_niger_abundance <- as.numeric(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance$INS_Lasius_niger_abundance)
                INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance$INS_Lasius_niger_prey_shannon_diversity <- as.numeric(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance$INS_Lasius_niger_prey_shannon_diversity)
                INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled <- INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance %>%
                  mutate(predator_abundance = scale(INS_Lasius_niger_abundance),
                         prey_shannon_diversity = scale(INS_Lasius_niger_prey_shannon_diversity))
                
                ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance$ARA_Pardosa_sp_abundance <- as.numeric(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance$ARA_Pardosa_sp_abundance)
                ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance$ARA_Pardosa_sp_prey_shannon_diversity <- as.numeric(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance$ARA_Pardosa_sp_prey_shannon_diversity)
                ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled <- ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance %>%
                  mutate(predator_abundance = scale(ARA_Pardosa_sp_abundance),
                         prey_shannon_diversity = scale(ARA_Pardosa_sp_prey_shannon_diversity))
  
            # ===== III-2-g - Calculer le ratio de diversité de Shannon par rapport à l'abondance totale de prédateurs pour chaque parcelle -------------

                Ratio_prey_shannon_diversity_predator_abundance_scaled$Ratio <- Ratio_prey_shannon_diversity_predator_abundance_scaled$prey_shannon_diversity / Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance
                Ratio_prey_shannon_diversity_predator_abundance_scaled$FS <- substr(Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc, nchar(Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc), nchar(Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc))
              
                correlation_value <- cor(Ratio_prey_shannon_diversity_predator_abundance_scaled$prey_shannon_diversity, Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance)
                
                
                INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Ratio <- INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Chrysopidae_sp_prey_shannon_diversity / INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Chrysopidae_sp_abundance
                INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$FS <- substr(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc, nchar(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc), nchar(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc))
                
                INS_Chrysopidae_sp_correlation_value <- cor(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Chrysopidae_sp_prey_shannon_diversity, INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Chrysopidae_sp_abundance)
                
                
                INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$Ratio <- INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Lasius_niger_prey_shannon_diversity / INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Lasius_niger_abundance
                INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$FS <- substr(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc, nchar(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc), nchar(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc))
                
                INS_Lasius_niger_correlation_value <- cor(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Lasius_niger_prey_shannon_diversity, INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$INS_Lasius_niger_abundance)
                
                
                ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Ratio <- ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$ARA_Pardosa_sp_prey_shannon_diversity / ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$ARA_Pardosa_sp_abundance
                ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$FS <- substr(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc, nchar(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc), nchar(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Parc))
                
                ARA_Pardosa_sp_correlation_value <- cor(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$ARA_Pardosa_sp_prey_shannon_diversity, ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$ARA_Pardosa_sp_abundance)
                
      # ===== III-3 - Tracer les graphiques ----------------------------------------------------------------------
            # ===== III-3-a - Pour chacun des trois prédateurs ensembles -----------------------------------------
                
                    ggplot(Ratio_prey_shannon_diversity_predator_abundance_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = Parc)) +
                      geom_point(size = 3) +
                      geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
                      labs(x = "Nombre total de prédateurs (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "Parcelle") +
                      theme_minimal() +
                      ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs par parcelle") +
                      annotate("text", x = Inf, y = Inf, 
                               label = paste("Corrélation =", round(correlation_value, 3)), 
                               hjust = 1.7, vjust = 1.3, size = 5, color = "green4")
                
                #--- Pour chaque farming system ---#
                
                # Calculer la corrélation pour chaque valeur de FS
                correlations <- Ratio_prey_shannon_diversity_predator_abundance_scaled %>%
                  group_by(FS) %>%
                  summarise(correlation = cor(predator_abundance, prey_shannon_diversity)) %>%
                  ungroup()
                
                # Ajouter une colonne pour les positions y des annotations
                correlations <- correlations %>%
                  mutate(y_position = max(Ratio_prey_shannon_diversity_predator_abundance_scaled$prey_shannon_diversity) - (row_number() * 0.2))
                
                
                    ggplot(Ratio_prey_shannon_diversity_predator_abundance_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = factor(FS))) +
                      geom_point(size = 3) +
                      geom_text(aes(label=Parc), vjust = -1, hjust = 0.5, size = 3) +
                      labs(x = "Nombre total de prédateurs (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "FS") +
                      scale_color_manual(values = c("blue", "red")) +  # Définir les couleurs manuellement
                      theme_minimal() +
                      ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs par FS") +
                      # Ajouter les annotations de corrélation pour chaque groupe de FS
                      geom_text(data = correlations, aes(x = min(Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance) * 1.1, 
                                                         y = y_position,
                                                         label = paste("FS:", FS, "Corrélation:", round(correlation, 3)), 
                                                         color = factor(FS)), 
                                hjust = 0, size = 4, inherit.aes = FALSE)
  
            # ===== III-3-b - Pour chacun des trois prédateurs séparement ----------------------------------------
                
                INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Isp <- "INS_Chrysopidae_sp"
                INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$Isp <- "INS_Lasius_niger"
                ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$Isp <- "ARA_Pardosa_sp"
                
                    # ===== III-3-b1 - ARA_Pardosa_sp ------------------------------------------------------------
  
                        # Calculer la corrélation pour chaque valeur de FS
                        ARA_Pardosa_sp_correlations <- ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled %>%
                          group_by(FS) %>%
                          summarise(correlation = cor(predator_abundance, prey_shannon_diversity)) %>%
                          ungroup()
                        
                        # Ajouter une colonne pour les positions y des annotations
                        ARA_Pardosa_sp_correlations <- ARA_Pardosa_sp_correlations %>%
                          mutate(y_position = max(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$prey_shannon_diversity) - (row_number() * 0.2))
                        
                        # Faire graph avec les 3 espèces de prédateurs
                        ggplot(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = factor(FS))) +
                          geom_point(size = 3) +
                          geom_text_repel(aes(label = Parc), size = 3) +
                          labs(x = "Nombre total de prédateurs ARA_Pardosa_sp (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "FS") +
                          scale_color_manual(values = c("blue", "red")) +  # Définir les couleurs manuellement
                          theme_minimal() +
                          ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs ARA_Pardosa_sp par FS") +
                          # Ajouter les annotations de corrélation pour chaque groupe de FS
                          geom_text(data = ARA_Pardosa_sp_correlations, aes(x = min(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance) * 1.1, 
                                                                            y = y_position,
                                                                            label = paste("FS:", FS, "Corrélation:", round(correlation, 3)), 
                                                                            color = factor(FS)), 
                                    hjust = 0, size = 4, inherit.aes = FALSE)
                        
                    # ===== III-3-b2 - INS_Chrysopidae_sp --------------------------------------------------------
  
                        # Calculer la corrélation pour chaque valeur de FS
                        INS_Chrysopidae_sp_correlations <- INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled %>%
                          group_by(FS) %>%
                          summarise(correlation = cor(predator_abundance, prey_shannon_diversity)) %>%
                          ungroup()
                        
                        # Ajouter une colonne pour les positions y des annotations
                        INS_Chrysopidae_sp_correlations <- INS_Chrysopidae_sp_correlations %>%
                          mutate(y_position = max(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$prey_shannon_diversity) - (row_number() * 0.2))
                        
                        ggplot(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = factor(FS))) +
                          geom_point(size = 3) +
                          geom_text_repel(aes(label = Parc), size = 3) +
                          labs(x = "Nombre total de prédateurs ARA_Pardosa_sp (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "FS") +
                          scale_color_manual(values = c("blue", "red")) +  # Définir les couleurs manuellement
                          theme_minimal() +
                          ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs ARA_Pardosa_sp par FS") +
                          # Ajouter les annotations de corrélation pour chaque groupe de FS
                          geom_text(data = INS_Chrysopidae_sp_correlations, aes(x = min(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance) * 1.1, 
                                                                            y = y_position,
                                                                            label = paste("FS:", FS, "Corrélation:", round(correlation, 3)), 
                                                                            color = factor(FS)), 
                                    hjust = 0, size = 4, inherit.aes = FALSE)
                        
                    # ===== III-3-b3 - INS_Lasius_niger ----------------------------------------------------------
  
                        # Calculer la corrélation pour chaque valeur de FS
                        INS_Lasius_niger_correlations <- INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled %>%
                          group_by(FS) %>%
                          summarise(correlation = cor(predator_abundance, prey_shannon_diversity)) %>%
                          ungroup()
                        
                        # Ajouter une colonne pour les positions y des annotations
                        INS_Lasius_niger_correlations <- INS_Lasius_niger_correlations %>%
                          mutate(y_position = max(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$prey_shannon_diversity) - (row_number() * 0.2))
                        
                        # Ajouter une colonne pour les positions y des annotations
                        ggplot(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = factor(FS))) +
                          geom_point(size = 3) +
                          geom_text_repel(aes(label = Parc), size = 3) +
                          labs(x = "Nombre total de prédateurs INS_Lasius_niger (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "FS") +
                          scale_color_manual(values = c("blue", "red")) +  # Définir les couleurs manuellement
                          theme_minimal() +
                          ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs ARA_Pardosa_sp par FS") +
                          # Ajouter les annotations de corrélation pour chaque groupe de FS
                          geom_text(data = INS_Lasius_niger_correlations, aes(x = min(INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance) * 1.1, 
                                                                            y = y_position,
                                                                            label = paste("FS:", FS, "Corrélation:", round(correlation, 3)), 
                                                                            color = factor(FS)), 
                                    hjust = 0, size = 4, inherit.aes = FALSE)

            # ===== III-3-c - Pour chacun des trois prédateurs sur le même graph ---------------------------------

                    # Faire graph avec les 3 espèces de prédateurs
                    Full_Ratio_prey_shannon_diversity_predator_abundance_scaled <- rbind(INS_Chrysopidae_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled[,c("Isp","predator_abundance","prey_shannon_diversity", "Ratio", "FS", "Parc")],
                                                                                         INS_Lasius_niger_Ratio_prey_shannon_diversity_predator_abundance_scaled[,c("Isp","predator_abundance","prey_shannon_diversity", "Ratio", "FS", "Parc")],
                                                                                         ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled[,c("Isp","predator_abundance","prey_shannon_diversity", "Ratio", "FS", "Parc")])

                    # Calculer la corrélation pour chaque valeur de FS
                    Full_correlations <- Full_Ratio_prey_shannon_diversity_predator_abundance_scaled %>%
                      group_by(FS) %>%
                      summarise(correlation = cor(predator_abundance, prey_shannon_diversity)) %>%
                      ungroup()
                    
                    # Ajouter une colonne pour les positions y des annotations
                    Full_correlations <- Full_correlations %>%
                      mutate(y_position = max(Ratio_prey_shannon_diversity_predator_abundance_scaled$prey_shannon_diversity) - (row_number() * 0.2))
  
                    ggplot(Full_Ratio_prey_shannon_diversity_predator_abundance_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = factor(Isp), shape = FS)) +
                      geom_point(size = 5, alpha = 0.5) + # Modifier l'alpha en fonction de la forme
                      geom_text_repel(aes(label = Parc), size = 3) +
                      labs(x = "Nombre total de prédateurs (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "Isp") +
                      scale_color_manual(values = c("cornflowerblue", "coral", "darkseagreen")) +  # Définir les couleurs manuellement
                      theme_minimal() +
                      ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs par FS") +
                      # Ajouter les annotations de corrélation pour chaque groupe de FS
                      geom_text(data = Full_correlations, aes(x = min(ARA_Pardosa_sp_Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance) * 1.1, 
                                                              y = y_position,
                                                              label = paste("FS:", FS, "Corrélation:", round(correlation, 3))), 
                                hjust = 0, size = 4, inherit.aes = FALSE)
                    
            # ===== III-3-d - Courbe de raréfaction ---------------------------------
                    
                    # Filtrer les lignes où la colonne "to_keep" a la valeur "oui"
                    TAB_Kept <- TAB %>%
                      filter(to_keep == "oui")
                    
                    # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs spécifiques dans "Isp"
                    TAB_All_species <- TAB_Kept
                    # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs spécifiques dans "Isp"
                    tab_pij2_All_species <- tab_pij2
                    
                    # Filtrage du dataframe pour conserver uniquement les lignes avec les valeurs "PijComb" > O.O1
                    tab_pij2_All_species_Seuil_1_percent <- tab_pij2_All_species %>% filter(PijComb > 0.01)
                    
                    # Compter les individus de proies pour chaque combinaison Parc et Isp
                    count_df <- tab_pij2_All_species_Seuil_1_percent %>%
                      group_by(Parc, Isp) %>%
                      summarise(Nb_proies = n()) %>%
                      ungroup()
                    
                    # Transformer les données pour avoir une colonne par espèce de prédateur
                    result_df <- count_df %>%
                      pivot_wider(names_from = Isp, values_from = Nb_proies, values_fill = list(Nb_proies = 0))
                    
                    # Extraire les données nécessaires pour la raréfaction
                    rarefaction_data <- result_df %>% select(-Parc)
                    
                    # Calculer la taille de l'échantillon commune
                    raremax <- min(rowSums(rarefaction_data))
                    # raremax = 82
                    # Calculer la richesse raréfiée
                    rarefied <- rarefy(rarefaction_data, sample = raremax)
                    
                    # Générer les couleurs pour chaque parc
                    colors <- rainbow(nrow(result_df))
                    
                    # Effectuer la raréfaction et tracer les courbes
                    rarecurve(rarefaction_data, step = 1, sample = raremax, col = colors, cex = 0.6,
                              main = "Courbe de raréfaction", ylab = "Nombre de proies")
                    
                    # Ajouter des labels aux courbes
                    ordilabel(cbind(rowSums(rarefaction_data), specnumber(rarefaction_data)), labels=result_df$Parc, col = colors)
                    
                    abline(v = raremax, col = "red", lty = 2) 
                    
# IV - Liens entre abondance prédateurs avec seulement les prédateurs avec proie détectée et diversité des proies =====
      # ===== IV-1 - Préparation des données --------------------------------------------------------------------        
            # ===== IV-1-a - Pour les prédateurs INS_Chrysopidae_sp, ARA_Pardosa_sp et INS_Lasius_niger ---------

            # Compter le nombre de valeurs uniques dans la colonne "ind" (Abondance totale des 3 espèces)
            TAB_nombre_valeurs_uniques <- length(unique(TAB_Species_in_all_Parc$ind))
            
            # Compter le nombre de valeurs uniques dans la colonne "ind" pour chaque valeur unique de "Isp"
            Abundance_All_sp <- aggregate(ind ~ Isp, data = TAB_All_species, FUN = function(x) length(unique(x)))
            Abundance_All_sp <- rename(Abundance_All_sp, Abundance = ind)
            
      # ===== IV-2 - Diversité de proies = f(Abondance prédateurs) ----------------------------------------------
            # ===== IV-2-a - Pour chacun des trois prédateurs ---------------------------------------------------
            # Il faut transformer le tableau en matrice contenant les proies en colonne et les prédateurs en ligne.
            # Les valeurs de la matrice indiquent la présence ou l'absence de la relation prédateur-proie.
            
            # Garder l'information sur la parcelle
            tab_pij2_All_species_Seuil_1_percent$ParcIsp <- paste(tab_pij2_All_species_Seuil_1_percent$Parc, tab_pij2_All_species_Seuil_1_percent$Isp, sep = "_")
            tab_pij2_All_species_Seuil_1_percent$ParcReadName <- paste(tab_pij2_All_species_Seuil_1_percent$Parc, tab_pij2_All_species_Seuil_1_percent$ReadName, sep = "_")
            
            # Ajouter une colonne 'Presence' avec des valeurs 1 pour indiquer la présence d'une interaction
            tab_pij2_All_species_presence <- tab_pij2_All_species_Seuil_1_percent %>%
              mutate(Presence = 1)
            
            # Créer la matrice de PijComb
            tab_pij2_All_species_PijComb <- tab_pij2_All_species_Seuil_1_percent[,c("PijComb", "ParcIsp", "ParcReadName")] %>%
              pivot_wider(names_from = ParcIsp, values_from = PijComb)
            
            # ===== IV-2-b - Calculer le nombre total de prédateurs pour chaque parcelle ------------------------
    
            predator_abundance_All_species <- tab_pij2_All_species_Seuil_1_percent %>%
              group_by(Parc) %>%
              summarise(predator_abundance = sum(!is.na(Isp)))  # Nombre total de prédateurs par parcelle
            
            # ===== IV-2-c - Calculer la fréquence de chaque proie pour chaque parcelle -------------------------
    
            prey_freq_All_species <- tab_pij2_All_species_Seuil_1_percent %>%
              group_by(Parc, ReadName) %>%
              summarise(freq = n()) %>%
              ungroup()
            
            # ===== IV-2-d - Calculer la diversité de Shannon des proies pour chaque parcelle ---------------------
    
            prey_shannon_diversity_All_species <- prey_freq_All_species %>%
              group_by(Parc) %>%
              summarise(prey_shannon_diversity = -sum(freq / sum(freq) * log(freq / sum(freq)))) %>%
              ungroup()
    
            # ===== IV-2-e - Joindre les données sur la diversité de Shannon des proies et le nombre total de prédateurs par parcelle ----
    
            Ratio_prey_shannon_diversity_predator_abundance_All_species <- inner_join(predator_abundance_All_species, prey_shannon_diversity_All_species, by = "Parc")

            # ===== IV-2-f - Standardiser les colonnes predator_abundance et prey_shannon_diversity -------------
    
            Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled <- Ratio_prey_shannon_diversity_predator_abundance_All_species %>%
              mutate(predator_abundance = scale(predator_abundance),
                     prey_shannon_diversity = scale(prey_shannon_diversity))

            # ===== IV-2-g - Calculer le ratio de diversité de Shannon par rapport à l'abondance totale de prédateurs pour chaque parcelle -------------
    
            Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$Ratio <- Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$prey_shannon_diversity / Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$predator_abundance
            Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$FS <- substr(Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$Parc, nchar(Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$Parc), nchar(Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$Parc))
            
            correlation_value <- cor(Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$prey_shannon_diversity, Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$predator_abundance)
            
      # ===== IV-3 - Rarefaction predateurs -----
      
      # Calculer la richesse raréfiée pour chaque échantillon à une taille d'échantillon commune
      Ratio_prey_shannon_diversity_predator_abundance_All_species[, c(2,3)] <- round(Ratio_prey_shannon_diversity_predator_abundance_All_species[, c(2,3)])
      
      rarefied <- rarefy(Ratio_prey_shannon_diversity_predator_abundance_All_species[, c(2,3)], sample = raremax)
      
      # Calculer la taille de l'échantillon commune
      raremax <- min(rowSums(Ratio_prey_shannon_diversity_predator_abundance_All_species[, c("predator_abundance", "prey_shannon_diversity")]))
      # raremax est la taille de l'échantillon la plus petite parmi tous les échantillons.
      # En d'autres termes, c'est le nombre d'individus du plus petit échantillon.
      # Lors de la raréfaction, tous les échantillons sont réduits à cette taille pour permettre une comparaison équitable de la richesse spécifique.
      
      
      colors <- rainbow(nrow(Ratio_prey_shannon_diversity_predator_abundance_All_species))
      
      # Effectuer la raréfaction
      rarecurve(Ratio_prey_shannon_diversity_predator_abundance_All_species[, c("predator_abundance", "prey_shannon_diversity")], step = 1, sample = raremax, col = colors, cex = 0.6,
                main = "Courbe de raréfaction", ylab = "Diversité de Shannon des proies", plot = FALSE)
      
      
      # La courbe de raréfaction montre comment la richesse spécifique augmente avec le nombre d'individus échantillonnés.
      # cela suggère que davantage d'échantillons pourraient révéler encore plus de diversité.
      ordilabel(cbind(rowSums(Ratio_prey_shannon_diversity_predator_abundance_All_species[, c(2,3)]), specnumber(Ratio_prey_shannon_diversity_predator_abundance_All_species[, c(2,3)])), labels=Ratio_prey_shannon_diversity_predator_abundance_All_species$Parc, col = colors)

      abline(v = raremax, col = "red", lty = 2) 
      
      # ===== IV-4 - Rarefaction proies -----
            # ===== IV-4-a - Calculer le nombre total de proies pour chaque parcelle ------------------------
            
            prey_abundance_Parc_All_species <- tab_pij2_All_species_Seuil_1_percent %>%
              group_by(Parc) %>%
              summarise(prey_abundance = sum(!is.na(ReadName)))  # Nombre total de proies par parcelle
            
            # ===== IV-4-b - Calculer la fréquence de chaque proie pour chaque parcelle -------------------------
            
            prey_freq_All_species <- tab_pij2_All_species_Seuil_1_percent %>%
              group_by(Parc, ReadName) %>%
              summarise(prey_abundance = n()) %>%
              ungroup()
            
            # ===== IV-4-c - Calculer la diversité de Shannon des proies pour chaque parcelle ---------------------
            
            prey_shannon_diversity_All_species <- prey_freq_All_species %>%
              group_by(Parc) %>%
              summarise(prey_shannon_diversity = -sum(prey_abundance / sum(prey_abundance) * log(prey_abundance / sum(prey_abundance)))) %>%
              ungroup()
            
            # ===== IV-4-d - Joindre les données sur la diversité de Shannon et le nombre total de prédateurs par parcelle ----
            
            Ratio_prey_shannon_diversity_prey_abundance_All_species <- inner_join(prey_abundance_Parc_All_species, prey_shannon_diversity_All_species, by = "Parc")
            
            # ===== IV-4-e - Standardiser les colonnes predator_abundance et prey_shannon_diversity -------------
            
            Ratio_prey_shannon_diversity_prey_abundance_All_species_scaled <- Ratio_prey_shannon_diversity_prey_abundance_All_species %>%
              mutate(prey_abundance = scale(prey_abundance),
                     prey_shannon_diversity = scale(prey_shannon_diversity))
            
            # ===== IV-4-f - Calculer le ratio de diversité de Shannon par rapport à l'abondance totale de prédateurs pour chaque parcelle -------------
            
            Ratio_prey_shannon_diversity_prey_abundance_All_species$Ratio <- Ratio_prey_shannon_diversity_prey_abundance_All_species$prey_shannon_diversity / Ratio_prey_shannon_diversity_prey_abundance_All_species$prey_abundance
            Ratio_prey_shannon_diversity_prey_abundance_All_species$FS <- substr(Ratio_prey_shannon_diversity_prey_abundance_All_species$Parc, nchar(Ratio_prey_shannon_diversity_prey_abundance_All_species$Parc), nchar(Ratio_prey_shannon_diversity_prey_abundance_All_species$Parc))
            
            correlation_value <- cor(Ratio_prey_shannon_diversity_prey_abundance_All_species$prey_shannon_diversity, Ratio_prey_shannon_diversity_prey_abundance_All_species$prey_abundance)
            
            # Calculer la richesse raréfiée pour chaque échantillon à une taille d'échantillon commune
            Ratio_prey_shannon_diversity_prey_abundance_All_species[, c(2,3)] <- round(Ratio_prey_shannon_diversity_prey_abundance_All_species[, c(2,3)])
            
            rarefied <- rarefy(Ratio_prey_shannon_diversity_prey_abundance_All_species[, c(2,3)], sample = raremax)
            
            # Calculer la taille de l'échantillon commune
            raremax <- min(rowSums(Ratio_prey_shannon_diversity_prey_abundance_All_species[, c("prey_abundance", "prey_shannon_diversity")]))
            # raremax est la taille de l'échantillon la plus petite parmi tous les échantillons.
            # En d'autres termes, c'est le nombre d'individus du plus petit échantillon.
            # Lors de la raréfaction, tous les échantillons sont réduits à cette taille pour permettre une comparaison équitable de la richesse spécifique.
            
            
            colors <- rainbow(nrow(Ratio_prey_shannon_diversity_prey_abundance_All_species))
            
            # Effectuer la raréfaction
            rarecurve(Ratio_prey_shannon_diversity_prey_abundance_All_species[, c("prey_abundance", "prey_shannon_diversity")], step = 1, sample = raremax, col = colors, cex = 0.6,
                      main = "Courbe de raréfaction", ylab = "Diversité de Shannon des proies", plot = FALSE)
            
            rare_results <- rarecurve(Ratio_prey_shannon_diversity_prey_abundance_All_species[, c("prey_abundance", "prey_shannon_diversity")], step = 1, sample = raremax, col = colors, cex = 0.6, plot = FALSE)
            
            rare_results[[1]]
            head(rare_results[[1]]) 
            # La courbe de raréfaction montre comment la richesse spécifique augmente avec le nombre d'individus échantillonnés.
            # cela suggère que davantage d'échantillons pourraient révéler encore plus de diversité.
            ordilabel(cbind(rowSums(Ratio_prey_shannon_diversity_prey_abundance_All_species[, c(2,3)]), specnumber(Ratio_prey_shannon_diversity_prey_abundance_All_species[, c(2,3)])), labels=Ratio_prey_shannon_diversity_prey_abundance_All_species$Parc, col = colors)
            
            abline(v = raremax, col = "red", lty = 2) 
            
      # ===== IV-5 - Tracer les graphiques ----------------------------------------------------------------------
            # ===== IV-5-a - Pour chacun des prédateurs ensembles -----------------------------------------
                    
                    ggplot(Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = Parc)) +
                      geom_point(size = 3) +
                      geom_text_repel(aes(label = Parc), size = 3) +
                      labs(x = "Nombre total de prédateurs (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "Parcelle") +
                      theme_minimal() +
                      ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs par parcelle") +
                      annotate("text", x = Inf, y = Inf, 
                               label = paste("Corrélation =", round(correlation_value, 3)), 
                               hjust = 1.7, vjust = 1.3, size = 5, color = "green4")
                    
                    #--- Pour chaque farming system ---#
                    
                    # Calculer la corrélation pour chaque valeur de FS
                    correlations <- Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled %>%
                      group_by(FS) %>%
                      summarise(correlation = cor(predator_abundance, prey_shannon_diversity)) %>%
                      ungroup()
                    
                    # Ajouter une colonne pour les positions y des annotations
                    correlations <- correlations %>%
                      mutate(y_position = max(Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled$prey_shannon_diversity) - (row_number() * 0.2))
                    
                    
                    ggplot(Ratio_prey_shannon_diversity_predator_abundance_All_species_scaled, aes(x = predator_abundance, y = prey_shannon_diversity, color = factor(FS))) +
                      geom_point(size = 3) +
                      geom_text_repel(aes(label = Parc), size = 3) +
                      labs(x = "Nombre total de prédateurs (scaled)", y = "Diversité de Shannon des proies (scaled)", color = "FS") +
                      scale_color_manual(values = c("blue", "red")) +  # Définir les couleurs manuellement
                      theme_minimal() +
                      ggtitle("(scaled) Relation entre diversité de Shannon des proies et abondance de prédateurs par FS") +
                      # Ajouter les annotations de corrélation pour chaque groupe de FS
                      geom_text(data = correlations, aes(x = min(Ratio_prey_shannon_diversity_predator_abundance_scaled$predator_abundance) * 1.1, 
                                                         y = y_position,
                                                         label = paste("FS:", FS, "Corrélation:", round(correlation, 3)), 
                                                         color = factor(FS)), 
                                hjust = 0, size = 4, inherit.aes = FALSE)

# V - Liens entre abondance de prédateurs et équitabilité des proies ==================================================
      # ===== V-1 - All_Species (Nb Reads) ---------------------------------------------------------------------------------        
      # Calculer l'abondance des prédateurs
      Abundance_pred <- tab_pij2_All_species_Seuil_1_percent %>%
        group_by(Parc, Isp) %>%
        summarise(abundance = n(), .groups = 'drop')
      
      Occurrence_prey <- tab_pij2_All_species_Seuil_1_percent %>%
        group_by(Parc, Isp, ReadName) %>%
        summarise(occurrences = n(), .groups = 'drop')
      
      Diversity_prey_occurrence <- Occurrence_prey %>%
        group_by(Isp, Parc) %>%
        summarise(H = vegan::diversity(occurrences, index = "shannon"),
                  S = n_distinct(ReadName),
                  H_max = log(n_distinct(ReadName)),
                  J = H / H_max, .groups = 'drop')
      
      
      # Fusionner les deux jeux de données
      result_nb_reads <- merge(Abundance_pred, Diversity_prey_occurrence, by = c("Parc", "Isp"))
      result_nb_reads$FS <- substr(result_nb_reads$Parc, nchar(result_nb_reads$Parc), nchar(result_nb_reads$Parc))
      
      # Standardiser les variables
      result_scaled_nb_reads_equitability <- result_nb_reads %>%
        mutate(predator_abundance_scaled = scale(abundance),
               prey_equitability_scaled = scale(J))
      
      result_scaled1 <- result_scaled_nb_reads_equitability %>% filter(Isp %in% unique(Isp)[1:8])
      result_scaled2 <- result_scaled_nb_reads_equitability %>% filter(Isp %in% unique(Isp)[9:16])
      result_scaled3 <- result_scaled_nb_reads_equitability %>% filter(Isp %in% unique(Isp)[17:24])
      result_scaled4 <- result_scaled_nb_reads_equitability %>% filter(Isp %in% unique(Isp)[25:32])
      result_scaled5 <- result_scaled_nb_reads_equitability %>% filter(Isp %in% unique(Isp)[33:40])
      result_scaled6 <- result_scaled_nb_reads_equitability %>% filter(Isp %in% unique(Isp)[41:48])
      result_scaled7 <- result_scaled_nb_reads_equitability %>% filter(Isp %in% unique(Isp)[49:52])
      
      # Visualiser la relation
      ggplot(result_scaled1, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled2, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled3, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled4, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled5, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled6, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled7, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 2, nrow = 2)
    
      
    # Test de la significativité

        # Ajuster le modèle de régression linéaire
        model <- lm(prey_equitability_scaled ~ predator_abundance_scaled, data = result_scaled_nb_reads_equitability)
        
        # Obtenir un résumé du modèle et ajouter l'espèce aux résultats
        model_summary <- tidy(model)
        
        # Afficher les résultats
        print(model_summary)

   # La relation entre l'abondance des prédateurs et l'équitabilité des proies n'est pas significative.
        

      # ===== V-2 - All_Species (PijComb) ---------------------------------------------------------------------------------        
        # Calculer l'abondance des prédateurs
        Abundance_pred <- tab_pij2_All_species_Seuil_1_percent %>%
          group_by(Parc, Isp) %>%
          summarise(abundance = n(), .groups = 'drop')
        
        # Calculer l'équitabilité des proies
        Diversity_prey <- tab_pij2_All_species_Seuil_1_percent %>%
          group_by(Parc, Isp, ReadName) %>%
          summarise(Interaction_probability = sum(PijComb), .groups = 'drop')
        
        Diversity_prey_summary <- Diversity_prey %>%
          group_by(Parc, Isp) %>%
          summarise(H = vegan::diversity(Interaction_probability, index = "shannon"),
                    S = n_distinct(ReadName),
                    H_max = log(n_distinct(ReadName)),
                    J = H / H_max, .groups = 'drop')
        
        # Fusionner les deux jeux de données
        result_PijComb <- merge(Abundance_pred, Diversity_prey_summary, by = c("Parc", "Isp"))
        result_PijComb$FS <- substr(result_PijComb$Parc, nchar(result_PijComb$Parc), nchar(result_PijComb$Parc))
        
        # Standardiser les variables
        result_scaled_PijComb_equitability <- result_PijComb %>%
          mutate(predator_abundance_scaled = scale(abundance),
                 prey_equitability_scaled = scale(J))
        
        result_scaled1 <- result_scaled_PijComb_equitability %>% filter(Isp %in% unique(Isp)[1:8])
        result_scaled2 <- result_scaled_PijComb_equitability %>% filter(Isp %in% unique(Isp)[9:16])
        result_scaled3 <- result_scaled_PijComb_equitability %>% filter(Isp %in% unique(Isp)[17:24])
        result_scaled4 <- result_scaled_PijComb_equitability %>% filter(Isp %in% unique(Isp)[25:32])
        result_scaled5 <- result_scaled_PijComb_equitability %>% filter(Isp %in% unique(Isp)[33:40])
        result_scaled6 <- result_scaled_PijComb_equitability %>% filter(Isp %in% unique(Isp)[41:48])
        result_scaled7 <- result_scaled_PijComb_equitability %>% filter(Isp %in% unique(Isp)[49:52])
        
        # Visualiser la relation
        ggplot(result_scaled1, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
          geom_point() +
          geom_text_repel(aes(label = Parc), size = 3) +
          labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
          theme_minimal() +
          facet_wrap(~ Isp, ncol = 4, nrow = 2)
        
        # Visualiser la relation
        ggplot(result_scaled2, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
          geom_point() +
          geom_text_repel(aes(label = Parc), size = 3) +
          labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
          theme_minimal() +
          facet_wrap(~ Isp, ncol = 4, nrow = 2)
        
        # Visualiser la relation
        ggplot(result_scaled3, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
          geom_point() +
          geom_text_repel(aes(label = Parc), size = 3) +
          labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
          theme_minimal() +
          facet_wrap(~ Isp, ncol = 4, nrow = 2)
        
        # Visualiser la relation
        ggplot(result_scaled4, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
          geom_point() +
          geom_text_repel(aes(label = Parc), size = 3) +
          labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
          theme_minimal() +
          facet_wrap(~ Isp, ncol = 4, nrow = 2)
        
        # Visualiser la relation
        ggplot(result_scaled5, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
          geom_point() +
          geom_text_repel(aes(label = Parc), size = 3) +
          labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
          theme_minimal() +
          facet_wrap(~ Isp, ncol = 4, nrow = 2)
        
        # Visualiser la relation
        ggplot(result_scaled6, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
          geom_point() +
          geom_text_repel(aes(label = Parc), size = 3) +
          labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
          theme_minimal() +
          facet_wrap(~ Isp, ncol = 4, nrow = 2)
        
        # Visualiser la relation
        ggplot(result_scaled7, aes(x = predator_abundance_scaled, y = prey_equitability_scaled, color = Parc)) +
          geom_point() +
          geom_text_repel(aes(label = Parc), size = 3) +
          labs(x = "Abondance des prédateurs", y = "Équitabilité des proies (Indice de Pielou)") +
          theme_minimal() +
          facet_wrap(~ Isp, ncol = 2, nrow = 2)
        
        
        # Test de la significativité
        
        # Ajuster le modèle de régression linéaire
        model <- lm(prey_equitability_scaled ~ predator_abundance_scaled, data = result_scaled_PijComb_equitability)

        # Obtenir un résumé du modèle et ajouter l'espèce aux résultats
        model_summary <- tidy(model)
        
        # Afficher les résultats
        print(model_summary)
        
        # La relation entre l'abondance des prédateurs et l'équitabilité des proies n'est pas significative.
        
        
        
# VI - Liens entre abondance prédateurs et diversité de Shannon des proies ============================================
      # ===== VI-1 - All_Species (Nb Reads) --------------------------------------------------------------------------------        

        # Standardiser les variables
        result_scaled_nb_reads_diversity <- result_nb_reads %>%
          mutate(predator_abundance_scaled = scale(abundance),
                 prey_diversity_scaled = scale(H))
        
      result_scaled1 <- result_scaled_nb_reads_diversity %>% filter(Isp %in% unique(Isp)[1:8])
      result_scaled2 <- result_scaled_nb_reads_diversity %>% filter(Isp %in% unique(Isp)[9:16])
      result_scaled3 <- result_scaled_nb_reads_diversity %>% filter(Isp %in% unique(Isp)[17:24])
      result_scaled4 <- result_scaled_nb_reads_diversity %>% filter(Isp %in% unique(Isp)[25:32])
      result_scaled5 <- result_scaled_nb_reads_diversity %>% filter(Isp %in% unique(Isp)[33:40])
      result_scaled6 <- result_scaled_nb_reads_diversity %>% filter(Isp %in% unique(Isp)[41:48])
      result_scaled7 <- result_scaled_nb_reads_diversity %>% filter(Isp %in% unique(Isp)[49:52])
      
      # Visualiser la relation
      ggplot(result_scaled1, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled2, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled3, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled4, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled5, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled6, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled7, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 2, nrow = 2) 

      
      # Test de la significativité
      
      # Ajuster le modèle de régression linéaire
      model <- lm(prey_diversity_scaled ~ predator_abundance_scaled, data = result_scaled_nb_reads_diversity)
      
      # Obtenir un résumé du modèle et ajouter l'espèce aux résultats
      model_summary <- tidy(model)
      
      # Afficher les résultats
      print(model_summary)
      
      
      # ===== VI-2 - All_Species (PijComb) --------------------------------------------------------------------------------        

      # Standardiser les variables
      result_scaled_PijComb_diversity <- result_PijComb %>%
        mutate(predator_abundance_scaled = scale(abundance),
               prey_diversity_scaled = scale(H))
      
      result_scaled1 <- result_scaled_PijComb_diversity %>% filter(Isp %in% unique(Isp)[1:8])
      result_scaled2 <- result_scaled_PijComb_diversity %>% filter(Isp %in% unique(Isp)[9:16])
      result_scaled3 <- result_scaled_PijComb_diversity %>% filter(Isp %in% unique(Isp)[17:24])
      result_scaled4 <- result_scaled_PijComb_diversity %>% filter(Isp %in% unique(Isp)[25:32])
      result_scaled5 <- result_scaled_PijComb_diversity %>% filter(Isp %in% unique(Isp)[33:40])
      result_scaled6 <- result_scaled_PijComb_diversity %>% filter(Isp %in% unique(Isp)[41:48])
      result_scaled7 <- result_scaled_PijComb_diversity %>% filter(Isp %in% unique(Isp)[49:52])
      
      # Visualiser la relation
      ggplot(result_scaled1, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled2, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled3, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled4, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled5, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled6, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 4, nrow = 2)
      
      # Visualiser la relation
      ggplot(result_scaled7, aes(x = predator_abundance_scaled, y = prey_diversity_scaled, color = Parc)) +
        geom_point() +
        geom_text_repel(aes(label = Parc), size = 3) +
        labs(x = "Abondance des prédateurs", y = "Diversité des proies (Indice de Shannon)") +
        theme_minimal() +
        facet_wrap(~ Isp, ncol = 2, nrow = 2) 
      
      
      # Test de la significativité
      
      # Ajuster le modèle de régression linéaire
      model <- lm(prey_diversity_scaled ~ predator_abundance_scaled, data = result_scaled_PijComb_diversity)
      
      # Obtenir un résumé du modèle et ajouter l'espèce aux résultats
      model_summary <- tidy(model)
      
      # Afficher les résultats
      print(model_summary)
      
# VII - Réseau trophique ==============================================================================================                    
      # ===== VII-1 - All_Species --------------------------------------------------------------------------------        
    
      # Transformer le data frame en matrice
          interaction_matrix_All_species <- reshape2::dcast(tab_pij2_All_species_Seuil_1_percent, ReadName ~ Isp, value.var = "PijComb", fun.aggregate = sum, fill = 0)

      # Mettre les noms de lignes et de colonnes
          rownames(interaction_matrix_All_species) <- interaction_matrix_All_species[, 1]
          interaction_matrix_All_species <- as.matrix(interaction_matrix_All_species[, -1])                    
          
      # Trier les lignes par ordre alphabétique des noms de lignes
          interaction_matrix_All_species <- interaction_matrix_All_species[order(rownames(interaction_matrix_All_species)), ]
          

          specieslevel <- specieslevel(interaction_matrix_All_species)
          
          specieslevelHL <- as.data.frame(specieslevel$`higher level`)

          
      # Paramètres graphiques pour réduire la taille du graphique
          op <- par(mar = c(0.1, 2, 0.1, 2) + 0.1, cex = 0.8)

      # Utiliser plotweb avec rotation des labels et taille réduite
          plotweb(interaction_matrix_All_species, method = "normal", 
                  low.lablength = 30, arrow = "down", high.lablength = 30,
                  text.rot = 90, col.low="grey", ybig = 1, y.width.high = 0.03, y.width.low = 0.03, bor.col.interaction = NA) # Rotation du texte des labels de 90 degrés

      
          métriques <- as.data.frame(networklevel(interaction_matrix_All_species))
          print(métriques)         
      
      # La connectance d'un réseau trophique bipartite est une mesure qui indique le nombre réel de connexions existantes par rapport au nombre total possible de connexions entre les deux ensembles d'espèces dans le réseau trophique. 
      # La valeur de la connectance est : 0.66666667.
      
          métriques$métriques <- rownames(métriques)
          
      # Accéder à la connectance dans les métriques du réseau
      # La connectance est définie comme le ratio du nombre réel de connexions dans le réseau au nombre maximal de connexions possibles.
      # Une connectance qui tend vers 1 signifie que chaque espèce du premier groupe interagit avec chaque espèce du deuxième groupe.
      # Cela peut suggérer une forte redondance fonctionnelle, où plusieurs espèces peuvent remplir des rôles similaires.
          connectance_value <- métriques[métriques$métriques == "connectance", "networklevel(interaction_matrix)"]
          cat("La valeur de la connectance est :", connectance_value, "\n")
          
      # Basse connectance : Une connectance faible (proche de 0) indique que les interactions sont rares et que chaque espèce a des relations spécialisées avec peu d'autres espèces. Cela peut indiquer une forte spécialisation des interactions et une structure plus fragile aux perturbations.
      # Les réseaux avec une connectance intermédiaire tendent à être plus stables et résilients aux perturbations, car ils ont un équilibre entre redondance (qui offre des sauvegardes contre la perte de certaines espèces) et spécialisation (qui permet une utilisation efficace des ressources).
      # Cela suggère un réseau relativement dense en termes de relations trophiques, avec une bonne probabilité que les consommateurs puissent trouver des ressources alternatives si certaines interactions disparaissent.
      
      
      # Calcul manuel de la connectance
          num_rows <- nrow(interaction_matrix_All_species)
          num_cols <- ncol(interaction_matrix_All_species)
          total_possible_connections <- num_rows * num_cols
          actual_connections <- sum(interaction_matrix_All_species > 0)
          connectance <- actual_connections / total_possible_connections
          
          cat("Nombre total de connexions possibles :", total_possible_connections, "\n")
          cat("Nombre de connexions réelles :", actual_connections, "\n")
          
          
          
          métriques_Dormann_2009 <- c("connectance", "web asymmetry", "links per species", 
                                      "number of compartments", "cluster coefficient", "extinction.slope.HL", 
                                      "extinction.slope.LL", "mean.number.of.shared.partners.HL", 
                                      "mean.number.of.shared.partners.LL", "togetherness.HL", "togetherness.LL", 
                                      "C.score.HL", "C.score.LL", "V.ratio.HL", "V.ratio.LL", "nestedness", 
                                      "weighted nestedness", "generality.HL", "vulnerability.LL", 
                                      "linkage density", "interaction evenness", "interaction strength asymmetry", 
                                      "niche.overlap.HL", "niche.overlap.LL")
          table_metriques_Dormann_2009 <- métriques[métriques_Dormann_2009, ]
          
          
          writexl::write_xlsx(table_metriques_Dormann_2009, "metriques_Dormann_2009.xlsx")

      # ===== VII-2 - Species_in_all_Parc -----------------------------------------------------------------------        
                  
      # Transformer le data frame en matrice
      interaction_matrix <- reshape2::dcast(tab_pij2_Species_in_all_Parc_Seuil_1_percent, ReadName ~ Isp, value.var = "PijComb", fun.aggregate = sum, fill = 0)
      
      # Mettre les noms de lignes et de colonnes
      rownames(interaction_matrix) <- interaction_matrix[, 1]
      interaction_matrix <- as.matrix(interaction_matrix[, -1])                    
      
      # Trier les lignes par ordre alphabétique des noms de lignes
      interaction_matrix <- interaction_matrix[order(rownames(interaction_matrix)), ]
      
      bipartite::networklevel(interaction_matrix)
      
      # Paramètres graphiques pour réduire la taille du graphique
      op <- par(mar = c(0.1, 2, 0.1, 2) + 0.1, cex = 0.8)
      
      # Définir un vecteur de couleurs correspondant aux interactions
      interaction_colors <- c("deepskyblue2", "deeppink3", "darkgoldenrod2")
      
      
      # Utiliser plotweb avec rotation des labels et taille réduite
      plotweb(interaction_matrix, method = "normal", 
              low.lablength = 30, arrow = "down", col.interaction = interaction_colors, high.lablength = 30,
              text.rot = 90, col.low="grey", ybig = 1, y.width.high = 0.03, y.width.low = 0.03, bor.col.interaction = NA) # Rotation du texte des labels de 90 degrés
      
      
      métriques <- as.data.frame(networklevel(interaction_matrix))
      print(métriques)         
      
      # La connectance d'un réseau trophique bipartite est une mesure qui indique le nombre réel de connexions existantes par rapport au nombre total possible de connexions entre les deux ensembles d'espèces dans le réseau trophique. 
      # La valeur de la connectance est : 0.66666667.
      
      métriques$métriques <- rownames(métriques)
      
      # Accéder à la connectance dans les métriques du réseau
      # La connectance est définie comme le ratio du nombre réel de connexions dans le réseau au nombre maximal de connexions possibles.
      # Une connectance qui tend vers 1 signifie que chaque espèce du premier groupe interagit avec chaque espèce du deuxième groupe.
      # Cela peut suggérer une forte redondance fonctionnelle, où plusieurs espèces peuvent remplir des rôles similaires.
      connectance_value <- métriques[métriques$métriques == "connectance", "networklevel(interaction_matrix)"]
      cat("La valeur de la connectance est :", connectance_value, "\n")
      
      # Basse connectance : Une connectance faible (proche de 0) indique que les interactions sont rares et que chaque espèce a des relations spécialisées avec peu d'autres espèces. Cela peut indiquer une forte spécialisation des interactions et une structure plus fragile aux perturbations.
      # Les réseaux avec une connectance intermédiaire tendent à être plus stables et résilients aux perturbations, car ils ont un équilibre entre redondance (qui offre des sauvegardes contre la perte de certaines espèces) et spécialisation (qui permet une utilisation efficace des ressources).
      # Cela suggère un réseau relativement dense en termes de relations trophiques, avec une bonne probabilité que les consommateurs puissent trouver des ressources alternatives si certaines interactions disparaissent.
      
      
      # Calcul manuel de la connectance
      num_rows <- nrow(interaction_matrix)
      num_cols <- ncol(interaction_matrix)
      total_possible_connections <- num_rows * num_cols
      actual_connections <- sum(interaction_matrix > 0)
      connectance <- actual_connections / total_possible_connections
      
      cat("Nombre total de connexions possibles :", total_possible_connections, "\n")
      cat("Nombre de connexions réelles :", actual_connections, "\n")
      
      
      
      métriques_Dormann_2009 <- c("connectance", "web asymmetry", "links per species", 
                                  "number of compartments", "cluster coefficient", "extinction.slope.HL", 
                                  "extinction.slope.LL", "mean.number.of.shared.partners.HL", 
                                  "mean.number.of.shared.partners.LL", "togetherness.HL", "togetherness.LL", 
                                  "C.score.HL", "C.score.LL", "V.ratio.HL", "V.ratio.LL", "nestedness", 
                                  "weighted nestedness", "generality.HL", "vulnerability.LL", 
                                  "linkage density", "interaction evenness", "interaction strength asymmetry", 
                                  "niche.overlap.HL", "niche.overlap.LL")
      table_metriques_Dormann_2009 <- métriques[métriques_Dormann_2009, ]
      
      
      writexl::write_xlsx(table_metriques_Dormann_2009, "metriques_Dormann_2009.xlsx")

# VIII - Visualisation des probabilités d'interaction par type de parcelle =============================================                 
      # ===== VIII-1 - All_species -----------------------------------------------------------------------        
      tab_pij2_All_species_Seuil_1_percent$FS <- substr(tab_pij2_All_species_Seuil_1_percent$Parc, nchar(tab_pij2_All_species_Seuil_1_percent$Parc), nchar(tab_pij2_All_species_Seuil_1_percent$Parc))
      
      ggplot(tab_pij2_All_species_Seuil_1_percent, aes(x=Parc, y=PijComb, fill = FS)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title="Distribution des probabilités d'interaction par type de parcelle",
             x="Type de parcelle",
             y="Probabilité d'interaction")
      
      
      library(lme4)
      library(ggplot2)
      # Modèle linéaire mixte généralisé
      # Le modèle inclut 'parcelle' comme facteur fixe et 'Prédateur' et 'proie' comme effets aléatoires
      glmm <- glmer(PijComb ~ Parc + (1|Isp) + (1|ReadName), data=tab_pij2_All_species_Seuil_1_percent, family=binomial)
      
      # Résumé du modèle
      summary(glmm)
      
      # Analyse de variance (ANOVA) pour tester l'effet de 'parcelle'
      anova(glmm)
      
      # Vérification des résidus
      plot(resid(glmm))
      unique(tab_pij2_All_species_Seuil_1_percent$Parc)
      # Analyse de sensibilité si nécessaire
      # Exemple de modèle ajoutant une variable de sensibilité
      # sensibilite_var pourrait être une colonne supplémentaire dans votre tableau de données
      # sens_model <- glmer(proba ~ parcelle + sensibilite_var + (1|Prédateur) + (1|proie), data=data, family=binomial)
      # summary(sens_model)

      
      
      modele <- glmer(PijComb ~ FS + (1|Isp) + (1|ReadName), data = tab_pij2_All_species_Seuil_1_percent, family = binomial)
      summary(modele)
      
      # Visualiser les différences de probabilités d'interaction entre les types de parcelles
      ggplot(tab_pij2_All_species_Seuil_1_percent, aes(x = FS, y = PijComb, fill = FS)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Interaction probability between predators and preys amongst farming system",
             x = "Farming system",
             y = "Interaction probability")

      # Effectuer un test t de Student
      t_test <- t.test(PijComb ~ FS, data = tab_pij2_All_species_Seuil_1_percent)
      
      # Afficher les résultats du test
      print(t_test)
      
      # La différence observée est que le groupe C a une moyenne légèrement plus élevée que le groupe B.
      
      # ===== VIII-2 - Species_in_all_Parc -----------------------------------------------------------------------        
      tab_pij2_Species_in_all_Parc_Seuil_1_percent$FS <- substr(tab_pij2_Species_in_all_Parc_Seuil_1_percent$Parc, nchar(tab_pij2_Species_in_all_Parc_Seuil_1_percent$Parc), nchar(tab_pij2_Species_in_all_Parc_Seuil_1_percent$Parc))
      
      modele_species_in_all_Parc <- glmer(PijComb ~ FS + (1|Isp) + (1|ReadName), data = tab_pij2_Species_in_all_Parc_Seuil_1_percent, family = binomial)
      summary(modele_species_in_all_Parc)
      
      # Visualiser les différences de probabilités d'interaction entre les types de parcelles
      ggplot(tab_pij2_Species_in_all_Parc_Seuil_1_percent, aes(x = FS, y = PijComb, fill = FS)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "(Species in all parc) Interaction probability between predators and preys amongst farming system",
             x = "Farming system",
             y = "Interaction probability")
      
      # Effectuer un test t de Student
      t_test <- t.test(PijComb ~ FS, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent)
      
      # Afficher les résultats du test
      print(t_test)
      
      # Create the boxplot and facet by species (Isp)
      ggplot(tab_pij2_Species_in_all_Parc_Seuil_1_percent, aes(x = FS, y = PijComb, fill = FS)) +
        geom_boxplot() +
        facet_wrap(~ Isp) +
        theme_minimal() +
        labs(title = "(Species in all parc) Interaction probability between predators and preys amongst farming systems",
             x = "Farming system",
             y = "Interaction probability")


      tab_pij2_Species_in_all_Parc_Seuil_1_percent_Pardosa <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
        filter(Isp == "ARA_Pardosa_sp")
      
      tab_pij2_Species_in_all_Parc_Seuil_1_percent_Chrysopidae <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
        filter(Isp == "INS_Chrysopidae_sp")
      
      tab_pij2_Species_in_all_Parc_Seuil_1_percent_Lasius <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
        filter(Isp == "INS_Lasius_niger")
      
      
      # Effectuer un test t de Student
      t_test <- t.test(PijComb ~ FS, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Pardosa)
      # Afficher les résultats du test
      print(t_test)
      
      # Effectuer un test t de Student
      t_test <- t.test(PijComb ~ FS, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Chrysopidae)
      # Afficher les résultats du test
      print(t_test)
      
      # Effectuer un test t de Student
      t_test <- t.test(PijComb ~ FS, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Lasius)
      # Afficher les résultats du test
      print(t_test)
      
# IX - Probabilité moyenne et médiane pour chaque proie d’être prédatée en fonction du nombre de prédateur par parcelle ===============================                    
      # ===== IX-1 - Species_in_all_Parc -----------------------------------------------------------------------        
      # Visualiser les différences de probabilités d'interaction entre les types de parcelles
      # Créez le graphique
      ggplot(tab_pij2_Species_in_all_Parc_Seuil_1_percent, aes(x = Isp, y = PijComb, fill = ReadName)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", position = position_dodge(0.75), shape=20, size=3, color="red", show.legend = FALSE) +
        #geom_text(aes(label = ReadName), size = 3, position = position_dodge(width = 0.75), vjust = -1.5) +
        theme_minimal() +
        labs(title = "(Species in all parc) Interaction probability between predators and preys",
             x = "Predator species",
             y = "Interaction probability") +
        scale_fill_discrete(name = "Prey species")
      
      
      tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red <- tab_pij2_Species_in_all_Parc_Seuil_1_percent[,c('Isp','Parc','ReadName','PijComb')]
      
      # /!\ Aller chercher dans TAB_Species_in_all_Parc
      Pred_Nb <- aggregate(Isp ~ Parc + ReadName, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, FUN = length)
      colnames(Pred_Nb) <- c("Parc", "ReadName", "Pred_Nb")
      
      # Moyenne de PijComb
      Mean_PijComb <- aggregate(PijComb ~ Parc + ReadName, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, FUN = mean)
      colnames(Mean_PijComb) <- c("Parc", "ReadName", "Mean_PijComb")
      
      # Médiane de PijComb
      Median_PijComb <- aggregate(PijComb ~ Parc + ReadName, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, FUN = median)
      colnames(Median_PijComb) <- c("Parc", "ReadName", "Median_PijComb")
      
      result <- merge(Pred_Nb, Mean_PijComb, by = c("Parc", "ReadName"), all = TRUE)
      result <- merge(result, Median_PijComb, by = c("Parc", "ReadName"), all = TRUE)
      
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Mean_PijComb, color = ReadName, label = ReadName)) +
        geom_point() +
        geom_text(vjust = -0.5, hjust = 0.5, size = 2) +
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité moyenne d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité moyenne d'interaction") +
        scale_color_discrete() +
        facet_wrap(~Parc) +
        scale_fill_discrete(name = "Prey species")
      
      TAB_Species_in_all_Parc_Red <- TAB_Species_in_all_Parc[,c('Isp','Parc','ReadName')]
      
      # /!\ Aller chercher dans TAB_Species_in_all_Parc
      Pred_Nb <- aggregate(Isp ~ Parc + ReadName, data = TAB_Species_in_all_Parc_Red, FUN = length)
      colnames(Pred_Nb) <- c("Parc", "ReadName", "Pred_Nb")
      
      
      # Moyenne de PijComb
      Mean_PijComb <- aggregate(PijComb ~ Parc + ReadName, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, FUN = mean)
      colnames(Mean_PijComb) <- c("Parc", "ReadName", "Mean_PijComb")
      
      # Médiane de PijComb
      Median_PijComb <- aggregate(PijComb ~ Parc + ReadName, data = tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, FUN = median)
      colnames(Median_PijComb) <- c("Parc", "ReadName", "Median_PijComb")
      
      result <- merge(Mean_PijComb,Pred_Nb, by=c("Parc", "ReadName"),all.x=TRUE)
      result <- merge(result, Median_PijComb, by = c("Parc", "ReadName"), all = TRUE)
      
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Mean_PijComb, color = ReadName, label = ReadName)) +
        geom_point() +
        geom_text(vjust = -0.5, hjust = 0.5, size = 2) +  # Ajouter les noms des points avec une taille de texte réduite
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité moyenne d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité moyenne d'interaction") +
        scale_color_discrete() +
        facet_wrap(~Parc) +
        scale_fill_discrete(name = "Prey species")
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Median_PijComb, color = ReadName, label = ReadName)) +
        geom_point() +
        geom_text(vjust = -0.5, hjust = 0.5, size = 2) +  # Ajouter les noms des points avec une taille de texte réduite
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité médiane d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité médiane d'interaction") +
        scale_color_discrete() +
        facet_wrap(~Parc) +
        scale_fill_discrete(name = "Prey species")
      
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Mean_PijComb, color = Parc, label = Parc)) +
        geom_point() +
        geom_text(vjust = -0.5, hjust = 0.5, size = 2) +  # Ajouter les noms des points avec une taille de texte réduite
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité moyenne d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité moyenne d'interaction") +
        scale_color_discrete() +
        facet_wrap(~ReadName) +
        scale_fill_discrete(name = "Prey species")
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Median_PijComb, color = Parc, label = Parc)) +
        geom_point() +
        geom_text(vjust = -0.5, hjust = 0.5, size = 2) +  # Ajouter les noms des points avec une taille de texte réduite
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité médiane d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité médiane d'interaction") +
        scale_color_discrete() +
        facet_wrap(~ReadName) +
        scale_fill_discrete(name = "Prey species")

# X - Diet composition of predators ==================================================================================                 
      # ===== X-1 - Species_in_all_Parc -----------------------------------------------------------------------        
      # % proies détectée pour chaque predateurs => diagramme en barres et diagramme circulaire
            # ===== X-1-a - INS_Chrysopidae_sp -----------------------------------------
            # INS_Chrysopidae_sp
            Chrysopidae_data <- subset(tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, Isp == 'INS_Chrysopidae_sp')
            
            # Calculer la somme totale des probabilités d'interaction pour "INS_Chrysopidae_sp"
            total_pijcomb <- sum(Chrysopidae_data$PijComb)
            
            percentage_data <- Chrysopidae_data %>%
              group_by(ReadName) %>%
              summarise(SumPijComb = sum(PijComb),
                        Predator = "INS_Chrysopidae_data_sp") %>%
              mutate(Percentage = (SumPijComb / total_pijcomb) * 100) %>%
              mutate(Genus = str_extract(ReadName, "(?<=_)[^_]+(?=_)"))
            
            percentage_data_aggregated <- percentage_data %>%
              group_by(Genus) %>%
              summarise(Percentage = sum(Percentage, na.rm = TRUE))
            
            percentage_data_aggregated_desc <- percentage_data_aggregated %>%
              arrange(desc(Percentage))
            
            percentage_data_aggregated_desc$Genus <- factor(percentage_data_aggregated_desc$Genus, levels = percentage_data_aggregated$Genus[order(percentage_data_aggregated$Percentage, decreasing = TRUE)])
            
            
            # Création du graphique ggplot avec spécification d'une palette de couleurs
            ggplot(percentage_data_aggregated_desc, aes(x = Genus, y = Percentage, fill = Genus)) +
              geom_bar(stat = "identity") +
              geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                        vjust = -0.5, size = 3.5) +
              theme_minimal() +
              labs(title = "Pourcentage du régime alimentaire de INS_Chrysopidae_sp par catégorie de proie",
                   x = "Proie",
                   y = "Pourcentage") +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
            
            
            percentage_data_aggregated <- percentage_data_aggregated %>%
              mutate(Percentage = round(Percentage, 1),  # Arrondir les pourcentages au dixième
                     Label = paste(Genus, "(", Percentage, "%)", sep = ""))
            
            # Réorganiser les niveaux du facteur Label par ordre croissant de pourcentage
            percentage_data_aggregated <- percentage_data_aggregated %>%
              arrange(desc(Percentage)) %>%
              mutate(Label = factor(Label, levels = Label))
            
            ggplot(percentage_data_aggregated, aes(x = "", y = Percentage, fill = Label)) +
              geom_bar(width = 1, stat = "identity", color = "black") +
              geom_text(aes(label = paste0(Percentage, "%")),
                        position = position_stack(vjust = 0.5), size = 3) + 
              coord_polar(theta = "y", start = 0) +
              theme_void() +
              labs(title = "Diagramme circulaire du régime alimentaire de INS_Chrysopidae_sp par catégorie de proie") +
              scale_fill_discrete(name = "Proie")
            
            # ===== X-1-b - INS_Lasius_niger -----------------------------------------
            # INS_Lasius_niger
            Lasius_data <- subset(tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, Isp == 'INS_Lasius_niger')
            
            # Calculer la somme totale des probabilités d'interaction pour "INS_Lasius_niger"
            total_pijcomb <- sum(Lasius_data$PijComb)
            
            percentage_data <- Lasius_data %>%
              group_by(ReadName) %>%
              summarise(SumPijComb = sum(PijComb),
                        Predator = "INS_Lasius_data_sp") %>%
              mutate(Percentage = (SumPijComb / total_pijcomb) * 100) %>%
              mutate(Genus = str_extract(ReadName, "(?<=_)[^_]+(?=_)"))
            
            percentage_data_aggregated <- percentage_data %>%
              group_by(Genus) %>%
              summarise(Percentage = sum(Percentage, na.rm = TRUE))
            
            percentage_data_aggregated_desc <- percentage_data_aggregated %>%
              arrange(desc(Percentage))
            
            percentage_data_aggregated_desc$Genus <- factor(percentage_data_aggregated_desc$Genus, levels = percentage_data_aggregated$Genus[order(percentage_data_aggregated$Percentage, decreasing = TRUE)])
            
            
            # Création du graphique ggplot avec spécification d'une palette de couleurs
            ggplot(percentage_data_aggregated_desc, aes(x = Genus, y = Percentage, fill = Genus)) +
              geom_bar(stat = "identity") +
              geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                        vjust = -0.5, size = 3.5) +
              theme_minimal() +
              labs(title = "Pourcentage du régime alimentaire de INS_Lasius_niger par catégorie de proie",
                   x = "Proie",
                   y = "Pourcentage") +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
            
            
            percentage_data_aggregated <- percentage_data_aggregated %>%
              mutate(Percentage = round(Percentage, 1),  # Arrondir les pourcentages au dixième
                     Label = paste(Genus, "(", Percentage, "%)", sep = ""))
            
            # Réorganiser les niveaux du facteur Label par ordre croissant de pourcentage
            percentage_data_aggregated <- percentage_data_aggregated %>%
              arrange(desc(Percentage)) %>%
              mutate(Label = factor(Label, levels = Label))
            
            ggplot(percentage_data_aggregated, aes(x = "", y = Percentage, fill = Label)) +
              geom_bar(width = 1, stat = "identity", color = "black") +
              geom_text(aes(label = paste0(Percentage, "%")),
                        position = position_stack(vjust = 0.5), size = 3) + 
              coord_polar(theta = "y", start = 0) +
              theme_void() +
              labs(title = "Diagramme circulaire du régime alimentaire de INS_Lasius_niger par catégorie de proie") +
              scale_fill_discrete(name = "Proie")
            
            # ===== X-1-c - ARA_Pardosa_sp -----------------------------------------
            # ARA_Pardosa_sp
            Pardosa_data <- subset(tab_pij2_Species_in_all_Parc_Seuil_1_percent_Red, Isp == 'ARA_Pardosa_sp')
            
            # Calculer la somme totale des probabilités d'interaction pour "ARA_Pardosa_sp"
            total_pijcomb <- sum(Pardosa_data$PijComb)
            
            percentage_data <- Pardosa_data %>%
              group_by(ReadName) %>%
              summarise(SumPijComb = sum(PijComb),
                        Predator = "ARA_Pardosa_data_sp") %>%
              mutate(Percentage = (SumPijComb / total_pijcomb) * 100) %>%
              mutate(Genus = str_extract(ReadName, "(?<=_)[^_]+(?=_)"))
            
            percentage_data_aggregated <- percentage_data %>%
              group_by(Genus) %>%
              summarise(Percentage = sum(Percentage, na.rm = TRUE))
            
            percentage_data_aggregated_desc <- percentage_data_aggregated %>%
              arrange(desc(Percentage))
            
            percentage_data_aggregated_desc$Genus <- factor(percentage_data_aggregated_desc$Genus, levels = percentage_data_aggregated$Genus[order(percentage_data_aggregated$Percentage, decreasing = TRUE)])
            
            
            # Création du graphique ggplot avec spécification d'une palette de couleurs
            ggplot(percentage_data_aggregated_desc, aes(x = Genus, y = Percentage, fill = Genus)) +
              geom_bar(stat = "identity") +
              geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                        vjust = -0.5, size = 3.5) +
              theme_minimal() +
              labs(title = "Pourcentage du régime alimentaire de ARA_Pardosa_sp par catégorie de proie",
                   x = "Proie",
                   y = "Pourcentage") +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
            
            
            percentage_data_aggregated <- percentage_data_aggregated %>%
              mutate(Percentage = round(Percentage, 1),  # Arrondir les pourcentages au dixième
                     Label = paste(Genus, "(", Percentage, "%)", sep = ""))
            
            # Réorganiser les niveaux du facteur Label par ordre croissant de pourcentage
            percentage_data_aggregated <- percentage_data_aggregated %>%
              arrange(desc(Percentage)) %>%
              mutate(Label = factor(Label, levels = Label))
            
            ggplot(percentage_data_aggregated, aes(x = "", y = Percentage, fill = Label)) +
              geom_bar(width = 1, stat = "identity", color = "black") +
              geom_text(aes(label = paste0(Percentage, "%")),
                        position = position_stack(vjust = 0.5), size = 3) + 
              coord_polar(theta = "y", start = 0) +
              theme_void() +
              labs(title = "Diagramme circulaire du régime alimentaire de ARA_Pardosa_sp par catégorie de proie") +
              scale_fill_discrete(name = "Proie")

      # ===== X-2 - All_species -----------------------------------------
            setwd("C:/Users/Alexandre_Dosset/Desktop/Antoptic/Diet_composition_graphs")
            
            # Liste des valeurs uniques de Isp
            unique_isps <- unique(tab_pij2_All_species_Seuil_1_percent$Isp)
            
            # Boucle sur chaque valeur unique de Isp
            for (isp_value in unique_isps) {
              
              # Filtrer les données pour la valeur actuelle de Isp
              data_filtered <- subset(tab_pij2_All_species_Seuil_1_percent, Isp == isp_value)
            
              # Calculer la somme totale des probabilités d'interaction pour l'espèce actuelle
              total_pijcomb <- sum(data_filtered$PijComb)
              
              # Calculer les pourcentages d'interaction pour chaque "ReadName"
              percentage_data <- data_filtered %>%
                group_by(ReadName) %>%
                summarise(SumPijComb = sum(PijComb)) %>%
                mutate(Percentage = (SumPijComb / total_pijcomb) * 100,
                       Genus = str_extract(ReadName, "(?<=_)[^_]+(?=_)"))
            
              # Agréger les données par genre
              percentage_data_aggregated <- percentage_data %>%
                group_by(Genus) %>%
                summarise(Percentage = sum(Percentage, na.rm = TRUE)) %>%
                arrange(desc(Percentage))
            
              # Convertir le facteur Genus pour une visualisation ordonnée
              percentage_data_aggregated$Genus <- factor(percentage_data_aggregated$Genus, 
                                                         levels = percentage_data_aggregated$Genus[order(percentage_data_aggregated$Percentage, decreasing = TRUE)])
              
              # Création du graphique en barres avec ggplot2
              bar_plot <- ggplot(percentage_data_aggregated, aes(x = Genus, y = Percentage, fill = Genus)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 3.5) +
                theme_minimal() +
                labs(title = paste("Pourcentage du régime alimentaire de", isp_value, "par catégorie de proie"),
                     x = "Proie",
                     y = "Pourcentage") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
            
              # Sauvegarde du graphique en barres
              ggsave(filename = paste0("bar_plot_", isp_value, ".png"), plot = bar_plot, width = 11, height = 8)
              
              # Préparation des données pour le graphique circulaire
              percentage_data_aggregated <- percentage_data_aggregated %>%
                mutate(Percentage = round(Percentage, 1),
                       Label = paste(Genus, "(", Percentage, "%)", sep = "")) %>%
                arrange(desc(Percentage)) %>%
                mutate(Label = factor(Label, levels = Label))
              
              # Création du diagramme circulaire
              pie_chart <- ggplot(percentage_data_aggregated, aes(x = "", y = Percentage, fill = Label)) +
                geom_bar(width = 1, stat = "identity", color = "black") +
                geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 3) + 
                coord_polar(theta = "y", start = 0) +
                theme_void() +
                labs(title = paste("Diagramme circulaire du régime alimentaire de", isp_value, "par catégorie de proie")) +
                scale_fill_discrete(name = "Proie")
              
              # Sauvegarde du diagramme circulaire
              ggsave(filename = paste0("pie_chart_", isp_value, ".png"), plot = pie_chart, width = 11, height = 8)
            }
            
            setwd("C:/Users/Alexandre_Dosset/Desktop/Antoptic")
            
# XI - Distribution de la richesse spécifique de proies par espèce de prédateur ========================================                 
      # ===== XI-1 - Species_in_all_Parc -----------------------------------------------------------------------        
      richesse_par_observation <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
        group_by(Parc, Isp) %>%
        summarise(RichesseSpecifique_proie = n_distinct(ReadName))
      
      # Visualisation de la richesse spécifique de proies par parcelle
      ggplot(richesse_par_observation, aes(x=Parc, y=RichesseSpecifique_proie)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title="Distribution de la richesse spécifique de proies par type de parcelle",
             x="Type de parcelle",
             y="Richesse spécifique de proies")
      
      
      richesse_par_observation <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
        group_by(Isp, Parc) %>%
        summarise(RichesseSpecifique_proie = n_distinct(ReadName), .groups = 'drop')
      
      # Calculer les statistiques nécessaires pour chaque espèce de prédateur
      stats_par_predateur <- richesse_par_observation %>%
        group_by(Isp) %>%
        summarise(
          Mean = mean(RichesseSpecifique_proie),
          SD = sd(RichesseSpecifique_proie),
          N = n(),
          SE = ifelse(N > 1, SD / sqrt(N), NA),
          CI_Lower = ifelse(N > 1, Mean - qt(0.975, df=N-1) * SE, NA),
          CI_Upper = ifelse(N > 1, Mean + qt(0.975, df=N-1) * SE, NA),
          .groups = 'drop'
        )
      
      
      richesse_par_observation <- left_join(richesse_par_observation, stats_par_predateur, by = "Isp")
      
      # Créer le boxplot et ajouter les moyennes et les intervalles de confiance
      ggplot(richesse_par_observation, aes(x=Isp, y=RichesseSpecifique_proie)) +
        geom_boxplot() +
        geom_point(data=richesse_par_observation, aes(x=Isp, y=Mean), color="red", size=3) +
        geom_jitter(height = 0, width = 0.2, color = "black", size = 1) +
        theme_minimal() +
        labs(title="Distribution de la richesse spécifique de proies par espèce de prédateur",
             x="Espèce de prédateur",
             y="Richesse spécifique de proies") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  
      # ===== XI-2 - All_species -----------------------------------------------------------------------        
      richesse_par_observation <- tab_pij2_All_species_Seuil_1_percent %>%
        group_by(Parc, Isp) %>%
        summarise(RichesseSpecifique_proie = n_distinct(ReadName))
      
      # Visualisation de la richesse spécifique de proies par parcelle
      ggplot(richesse_par_observation, aes(x=Parc, y=RichesseSpecifique_proie)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title="Distribution de la richesse spécifique de proies par type de parcelle",
             x="Type de parcelle",
             y="Richesse spécifique de proies")
  
  
  richesse_par_observation <- tab_pij2_All_species_Seuil_1_percent %>%
  group_by(Isp, Parc) %>%
  summarise(RichesseSpecifique_proie = n_distinct(ReadName), .groups = 'drop')
  
  # Calculer les statistiques nécessaires pour chaque espèce de prédateur
  stats_par_predateur <- richesse_par_observation %>%
  group_by(Isp) %>%
  summarise(
  Mean = mean(RichesseSpecifique_proie),
  SD = sd(RichesseSpecifique_proie),
  N = n(),
  SE = ifelse(N > 1, SD / sqrt(N), NA),
  CI_Lower = ifelse(N > 1, Mean - qt(0.975, df=N-1) * SE, NA),
  CI_Upper = ifelse(N > 1, Mean + qt(0.975, df=N-1) * SE, NA),
  .groups = 'drop'
  )
  
  
  richesse_par_observation <- left_join(richesse_par_observation, stats_par_predateur, by = "Isp")
  
  # Créer le boxplot et ajouter les moyennes et les intervalles de confiance
  ggplot(richesse_par_observation, aes(x=Isp, y=RichesseSpecifique_proie)) +
  geom_boxplot() +
  geom_point(data=richesse_par_observation, aes(x=Isp, y=Mean), color="red", size=3) +
  geom_jitter(height = 0, width = 0.2, color = "black", size = 1) +
  theme_minimal() +
  labs(title="Distribution de la richesse spécifique de proies par espèce de prédateur",
  x="Espèce de prédateur",
  y="Richesse spécifique de proies") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  

# XII - Fonction pour calculer l'indice de chevauchement de Pianka ===============
# https://rdrr.io/cran/EcoSimR/man/pianka.html
  
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
  
  # Calcul de l'indice de Pianka
  pianka_predators  <- pianka_index(interaction_matrix_All_species)
  #pianka_predators  <- as.data.frame(pianka_predators )

  pianka_sites <- pianka_index(t(interaction_matrix_All_species))
  #pianka_sites <- as.data.frame(pianka_sites)

  # Convertir les matrices en vecteurs en enlevant les valeurs NA
  predator_values <- as.vector(pianka_predators[lower.tri(pianka_predators)])
  site_values <- as.vector(pianka_sites[lower.tri(pianka_sites)])
  
  # Enlever les valeurs NA
  valid_indices <- !is.na(predator_values) & !is.na(site_values)
  predator_values <- predator_values[valid_indices]
  site_values <- site_values[valid_indices]
  
  # Vérifier que toutes les valeurs sont finies
  valid_indices <- is.finite(predator_values) & is.finite(site_values)
  predator_values <- predator_values[valid_indices]
  site_values <- site_values[valid_indices]
  
  # Créer un graphique de dispersion
  plot(predator_values, site_values, 
       xlab = "Indice de Pianka des prédateurs", 
       ylab = "Indice de Pianka des sites",
       main = "Chevauchement de niche entre les prédateurs et les sites",
       pch = 19, col = "blue")
  
  # Ajuster un modèle de régression linéaire
  model <- lm(site_values ~ predator_values)
  abline(model, col = "red", lwd = 2)
  
  summary(model)
  
  # Ajouter les intervalles de confiance à 95%
  pred_vals <- seq(min(predator_values), max(predator_values), length.out = 100)
  pred_data <- data.frame(predator_values = pred_vals)
  conf_interval <- predict(model, newdata = pred_data, interval = "confidence")
  
  # Ajouter la zone colorée entre les intervalles de confiance au graphique
  polygon(c(pred_vals, rev(pred_vals)), 
          c(conf_interval[, "lwr"], rev(conf_interval[, "upr"])), 
          col = rgb(1, 0, 0, 0.2), border = NA)
  
  # Ajouter les intervalles de confiance au graphique
  lines(pred_vals, conf_interval[, "fit"], col = "red", lwd = 2)
  lines(pred_vals, conf_interval[, "lwr"], col = "red", lwd = 1, lty = 2)
  lines(pred_vals, conf_interval[, "upr"], col = "red", lwd = 1, lty = 2)
  
  # Afficher le résumé du modèle pour voir le R²
  model_summary <- summary(model)
  print(model_summary)
  
  # Extraire et afficher le R²
  r_squared <- model_summary$r.squared
  cat("Le coefficient de détermination R² est:", r_squared, "\n")
  
  correlation <- cor(predator_values, site_values, method = "pearson")

# XIII - Modularité ---------------------------------------
  
  modules <- computeModules(interaction_matrix_All_species)
  plotModuleWeb(modules) 
  
  module_assignment <- modules@moduleWeb  # Une matrice avec les modules
  
  module_table <- data.table::as.data.table(module_assignment, keep.rownames = TRUE)
  
  
# XIV - Centrality ----------------------------------------

  library(igraph)
  interaction_matrix <- interaction_matrix_All_species
  graph <- graph_from_incidence_matrix(interaction_matrix)
  
  # Calcul des différentes mesures de centralité
  degree_centrality <- degree(graph, mode="all")
  betweenness_centrality <- betweenness(graph, directed=FALSE)
  closeness_centrality <- closeness(graph, mode="all")
  eigenvector_centrality <- eigen_centrality(graph)$vector  

  # Affichage des résultats
  centralities <- data.frame(
    Degree = degree_centrality,
    Betweenness = betweenness_centrality,
    Closeness = closeness_centrality,
    Eigenvector = eigenvector_centrality
  )
  
  print(centralities)  
  
  
  closeness_w(interaction_matrix, directed=NULL, gconly=TRUE, precomp.dist=NULL, alpha=1)
  
  
# Autre -----------  
  
  
  ####
  # Raréfier
  ####

# Compter les individus de proies pour chaque combinaison Parc et Isp
  count_df <- tab_pij2_Species_in_all_Parc_Seuil_1_percent %>%
    group_by(Parc, Isp) %>%
    summarise(Nb_proies = n()) %>%
    ungroup()
  
  # Transformer les données pour avoir une colonne par espèce de prédateur
  result_df <- count_df %>%
    pivot_wider(names_from = Isp, values_from = Nb_proies, values_fill = list(Nb_proies = 0))
  
  # Extraire les données nécessaires pour la raréfaction
  rarefaction_data <- result_df %>% select(-Parc)
  
  # Calculer la taille de l'échantillon commune
  raremax <- min(rowSums(rarefaction_data))
  
  # Calculer la richesse raréfiée
  rarefied <- rarefy(rarefaction_data, sample = raremax)
  
  # Générer les couleurs pour chaque parc
  colors <- rainbow(nrow(result_df))
  
  # Effectuer la raréfaction et tracer les courbes
  rarecurve(rarefaction_data, step = 1, sample = raremax, col = colors, cex = 0.6,
            main = "Courbe de raréfaction", ylab = "Nombre de proies")
  
  # Ajouter des labels aux courbes
  ordilabel(cbind(rowSums(rarefaction_data), specnumber(rarefaction_data)), labels=result_df$Parc, col = colors)
  
  abline(v = raremax, col = "red", lty = 2) 
  
  
  # Correlation niveau d'infestation avec contenus stomacaux -----------  
  
  # Importation de la feuille CR_Paysage
  
  CR_Paysage <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/CR_Paysage.txt", header = TRUE, sep = "")
  CR_Paysage <- rename(CR_Paysage, Parc = parc)
  
  #Niveau d'infestation = CR_Paysage$Occurences esp
  #Contenus stomacaux = PijComb moyen sur l'ensemble des prédateurs pour le bioagresseur donné tab_pij2_All_species_Seuil_1_percent
  
  #Phylloxera
      #CR_Paysage$Ophyl
      #tab_pij2_All_species_Seuil_1_percent$ReadName == INS_Phylloxeridae_daktulosphera + moyenne PijComb
  Ophyl <- CR_Paysage[, c("Parc", "Ophyl")]
  Ophyl <- na.omit(Ophyl)
  Ophyl$Ophyl_scaled <- scale(Ophyl$Ophyl, center = TRUE, scale = TRUE)
  Ophyl <- Ophyl %>%
    group_by(Parc) %>%
    summarise(
      Mean_Ophyl_scaled = mean(Ophyl_scaled),
    )
  
  Pij_Ophyl <- subset(tab_pij2_All_species_Seuil_1_percent, ReadName == "INS_Phylloxeridae_Daktulosphaira")
  Pij_Ophyl <- Pij_Ophyl[, c("Isp", "Parc", "ReadName", "PijComb")]
  Pij_Ophyl <- na.omit(Pij_Ophyl)
  Pij_Ophyl$PijComb_scaled <- scale(Pij_Ophyl$PijComb, center = TRUE, scale = TRUE)
  Pij_Ophyl <- Pij_Ophyl %>%
    group_by(Parc) %>%
    summarise(
      PijComb_scaled = mean(PijComb_scaled),
    )
  
  Graph_Phylloxera <- Ophyl
  Graph_Phylloxera <- merge(Graph_Phylloxera, Pij_Ophyl[, c("Parc", "PijComb_scaled")], by = "Parc", all.x = TRUE)
  Graph_Phylloxera <- na.omit(Graph_Phylloxera)
  
  
  Graphique_Phylloxera <- ggplot(Graph_Phylloxera, aes(x = Mean_Ophyl_scaled, y = PijComb_scaled)) +
    geom_point(color = "blue", size = 3) +  # Ajouter les points
    geom_text(aes(label = Parc), 
              vjust = -0.5,         # Décalage vertical vers le haut
              hjust = -0.2,
              color = "black",       # Couleur des labels
              angle = 70,          # Inclinaison des labels
              size = 3) +          # Taille du texte
    labs(x = "Occurence Phylloxera", 
         y = "Mean_PijComb_Ophyl", 
         title = "Graphique de Mean_PijComb_Ophyl en fonction de l'occurence de Phylloxera") +
    theme_minimal()  # Utilisation d'un thème minimaliste
  
  Graphique_Phylloxera
  
  Graphique_Phylloxera <- Graphique_Phylloxera + geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Ajouter la droite de régression linéaire
    labs(x = "Occurrence Phylloxera", 
         y = "Mean_PijComb_Ophyl", 
         title = "Graphique de Mean_PijComb_Ophyl en fonction de l'occurence de Phylloxera") +
    theme_minimal()  # Utilisation d'un thème minimaliste
  
  Graphique_Phylloxera
  
  correlation_Phylloxera <- cor(Graph_Phylloxera$Mean_Ophyl_scaled, Graph_Phylloxera$PijComb_scaled, use = "complete.obs")
  print(correlation_Phylloxera)
  
  
  #Tordeuse
      #CR_Paysage$Otord
      #tab_pij2_All_species_Seuil_1_percent$ReadName == INS_Tortricidae_Lobesia + moyenne PijComb
  Otord <- CR_Paysage[, c("Parc", "Otord")]
  Otord <- na.omit(Otord)
  Otord$Otord_scaled <- scale(Otord$Otord, center = TRUE, scale = TRUE)
  Otord <- Otord %>%
    group_by(Parc) %>%
    summarise(
      Mean_Otord_scaled = mean(Otord_scaled),
    )
  
  Pij_Otord <- subset(tab_pij2_All_species_Seuil_1_percent, ReadName == "INS_Tortricidae_Lobesia")
  Pij_Otord <- Pij_Otord[, c("Isp", "Parc", "ReadName", "PijComb")]
  Pij_Otord <- na.omit(Pij_Otord)
  Pij_Otord$PijComb_scaled <- scale(Pij_Otord$PijComb, center = TRUE, scale = TRUE)
  Pij_Otord <- Pij_Otord %>%
    group_by(Parc) %>%
    summarise(
      PijComb_scaled = mean(PijComb_scaled),
    )
  
  Graph_Tordeuse <- Otord
  Graph_Tordeuse <- merge(Graph_Tordeuse, Pij_Otord[, c("Parc", "PijComb_scaled")], by = "Parc", all.x = TRUE)
  Graph_Tordeuse <- na.omit(Graph_Tordeuse)
  
  
  Graphique_Tordeuse <- ggplot(Graph_Tordeuse, aes(x = Mean_Otord_scaled, y = PijComb_scaled)) +
    geom_point(color = "red", size = 3) +  # Ajouter les points
    geom_text(aes(label = Parc), 
              vjust = -0.5,         # Décalage vertical vers le haut
              hjust = -0.2,
              color = "black",       # Couleur des labels
              angle = 70,          # Inclinaison des labels
              size = 3) +          # Taille du texte
    labs(x = "Occurence Tordeuse", 
         y = "Mean_PijComb_Otord", 
         title = "Graphique de Mean_PijComb_Otord en fonction de l'occurence de Tordeuse") +
    theme_minimal()  # Utilisation d'un thème minimaliste
  
  Graphique_Tordeuse
  
  
  #Cicadellidae
      #CR_Paysage$Ocica
      #tab_pij2_All_species_Seuil_1_percent$ReadName == INS_Cicadellidae_Empoasca + INS_Cicadellidae_Scaphoideus + moyenne PijComb
  Ocica <- CR_Paysage[, c("Parc", "cult", "site", "Ocica")]
  Ocica <- na.omit(Ocica)
  Ocica$Ocica_scaled <- scale(Ocica$Ocica, center = TRUE, scale = TRUE)
  Ocica <- Ocica %>%
    group_by(Parc) %>%
    summarise(
      Mean_Ocica_scaled = mean(Ocica_scaled),
    )
  
  Pij_Ocica <- subset(tab_pij2_All_species_Seuil_1_percent, ReadName %in% c("INS_Cicadellidae_Empoasca", "INS_Cicadellidae_Scaphoideus"))
  Pij_Ocica <- Pij_Ocica[, c("Isp", "Parc", "ReadName", "PijComb")]
  Pij_Ocica <- na.omit(Pij_Ocica)
  Pij_Ocica$PijComb_scaled <- scale(Pij_Ocica$PijComb, center = TRUE, scale = TRUE)
  Pij_Ocica <- Pij_Ocica %>%
    group_by(Parc) %>%
    summarise(
      PijComb_scaled = mean(PijComb_scaled),
    )
  
  Graph_Cicadellidae <- Ocica
  Graph_Cicadellidae <- merge(Graph_Cicadellidae, Pij_Ocica[, c("Parc", "PijComb_scaled")], by = "Parc", all.x = TRUE)
  Graph_Cicadellidae <- na.omit(Graph_Cicadellidae)
  
  Graphique_Cicadellidae <- ggplot(Graph_Cicadellidae, aes(x = Mean_Ocica_scaled, y = PijComb_scaled)) +
    geom_point(color = "green", size = 3) +  # Ajouter les points
    geom_text(aes(label = Parc), 
              vjust = -0.5,         # Décalage vertical vers le haut
              hjust = -0.2,
              color = "black",       # Couleur des labels
              angle = 70,          # Inclinaison des labels
              size = 3) +          # Taille du texte
    labs(x = "Occurence Cicadellidae", 
         y = "Mean_PijComb_Ocica", 
         title = "Graphique de Mean_PijComb_Ocica en fonction de l'occurence de Cicadellidae") +
    theme_minimal()  # Utilisation d'un thème minimaliste
  
  Graphique_Cicadellidae
  
  Graphique_Cicadellidae <- Graphique_Cicadellidae + geom_smooth(method = "lm", color = "green", se = FALSE) +  # Ajouter la droite de régression linéaire
    labs(x = "Occurrence Cicadellidae", 
         y = "Mean_PijComb_Ocica", 
         title = "Graphique de Mean_PijComb_tord en fonction de l'occurence de Cicadellidae") +
    theme_minimal()  # Utilisation d'un thème minimaliste
  
  Graphique_Cicadellidae
  
  correlation_Cicadellidae <- cor(Graph_Cicadellidae$Mean_Ocica_scaled, Graph_Cicadellidae$PijComb_scaled, use = "complete.obs")
  print(correlation_Cicadellidae)
  
  
  library(patchwork)
  combined_plot <- Graphique_Phylloxera / Graphique_Tordeuse / Graphique_Cicadellidae
  print(combined_plot)
  
  
  # Occurence = Observation
  # Fréquence = Effectif ramené à l'effectif total
  
  # Agréger les données par parcelle pour obtenir la fréquence d'observation des couples proies-prédateurs
  freq_interactions_par_parc <- tab_pij2_All_species_Seuil_1_percent %>%
    group_by(Parc) %>%
    summarise(Frequence_interactions = n()) %>%
    ungroup()
  
  # Joindre ReadName également
  freq_interactions_par_parc_2 <- tab_pij2_All_species_Seuil_1_percent %>%
    group_by(Parc, ReadName) %>%
    summarise(Frequence_interactions = n()) %>%
    ungroup()
  # INS_Totricidae_Lobesia, INS_Phyllaxeridae_daktulosphora, INS_Cicadellidae_Empoasca, INS_Cicadellidae_Scaphoideus
  freq_interactions_par_parc_2 <- freq_interactions_par_parc_2 %>%
    filter(ReadName %in% c("INS_Tortricidae_Lobesia", 
                           "INS_Phylloxeridae_Daktulosphaira", 
                           "INS_Cicadellidae_Empoasca", 
                           "INS_Cicadellidae_Scaphoideus"))
  
  CR_Paysage_tord <- CR_Paysage[, c("Parc", "Otord")]  
  CR_Paysage_tord <- na.omit(CR_Paysage_tord)
  CR_Paysage_tord <- CR_Paysage_tord %>%
    group_by(Parc) %>%
    summarize(Otord = sum(Otord))
  
  # Joindre les deux tableaux
  freq_interactions_par_parc_3 <- CR_Paysage_tord %>%
    left_join(freq_interactions_par_parc_2, by = "Parc")
  freq_interactions_par_parc_3 <- na.omit(freq_interactions_par_parc_3)
  
  freq_Tordeuse <- freq_interactions_par_parc_3 %>%
    filter(ReadName %in% c("INS_Tortricidae_Lobesia"))
  # Calculer la corrélation entre le niveau d'infestation et la fréquence d'observation
  correlation_Tordeuse <- cor(freq_Tordeuse$Otord, freq_Tordeuse$Frequence_interactions, use = "complete.obs")
  print(correlation_Tordeuse)
  
  # CR_Paysage_Ophyl à faire 
  CR_Paysage_phyl <- CR_Paysage[, c("Parc", "Ophyl")]  
  CR_Paysage_phyl <- na.omit(CR_Paysage_phyl)
  CR_Paysage_phyl <- CR_Paysage_phyl %>%
    group_by(Parc) %>%
    summarize(Ophyl = sum(Ophyl))
  
  # Joindre les deux tableaux
  freq_interactions_par_parc_4 <- CR_Paysage_phyl %>%
    left_join(freq_interactions_par_parc_2, by = "Parc")
  freq_interactions_par_parc_4 <- na.omit(freq_interactions_par_parc_4)
  
  freq_Phylloxera <- freq_interactions_par_parc_4 %>%
    filter(ReadName %in% c("INS_Phylloxeridae_Daktulosphaira"))
  # Calculer la corrélation entre le niveau d'infestation et la fréquence d'observation
  correlation_Phylloxera <- cor(freq_Phylloxera$Ophyl, freq_Phylloxera$Frequence_interactions, use = "complete.obs")
  print(correlation_Phylloxera)
  
  
  # CR_Paysage_Ophyl à faire 
  CR_Paysage_cica <- CR_Paysage[, c("Parc", "Ocica")]  
  CR_Paysage_cica <- na.omit(CR_Paysage_cica)
  CR_Paysage_cica <- CR_Paysage_cica %>%
    group_by(Parc) %>%
    summarize(Ocica = sum(Ocica))
  
  # Joindre les deux tableaux
  freq_interactions_par_parc_5 <- CR_Paysage_cica %>%
    left_join(freq_interactions_par_parc_2, by = "Parc")
  freq_interactions_par_parc_5 <- na.omit(freq_interactions_par_parc_5)
  
  freq_Cicadellidae <- freq_interactions_par_parc_5 %>%
    filter(ReadName %in% c("INS_Cicadellidae_Empoasca", "INS_Cicadellidae_Scaphoideus"))
  # Calculer la corrélation entre le niveau d'infestation et la fréquence d'observation
  correlation_Cicadellidae <- cor(freq_Cicadellidae$Ocica, freq_Cicadellidae$Frequence_interactions, use = "complete.obs")
  print(correlation_Cicadellidae)
  
  
  # Joindre les deux tableaux
  tab_infest_inter <- CR_Paysage %>%
    left_join(freq_interactions_par_parc, by = "Parc")
  
  # Calculer la corrélation entre le niveau d'infestation et la fréquence d'observation
  correlation <- cor(tab_infest_inter$Ococh, tab_infest_inter$Frequence_interactions, use = "complete.obs")
  print(correlation)
  
  
  
  
  
  
  #Interprétation Finale
  #Performance Générale : Le modèle semble avoir une erreur relativement élevée (RMSE) et une capacité d'explication de la variance très faible (Rsquared). Ces indicateurs suggèrent que le modèle n'explique pas bien les données.
  #Stabilité : Les faibles écarts types pour RMSE, Rsquared, et MAE montrent que la performance du modèle est relativement stable à travers les différents folds, bien que la performance moyenne ne soit pas très bonne.
  
  # Chercher si la diversité des régimes alimentaires des prédateurs (?) est liée à une composition différente ou une disponibilité de proies   
  # Dans un paysage en fonction de %conv, %bio et %HSN  
  
  # Comment décorréler la composition du paysage avec la composition de la communauté de prédateurs
  
  
  ###################################
  # Antoptic 20-09-2024
  ###################################
  
  # Remarque 4 - ACP (IFT, Paysage, Surfaces travaillées => intensité d'usage du sol)
  # Indice qui regroupe Pesticides, couvert végétal et Forêt avec 0 et 1 pour chaque valeur, donc somme allant de 0 à 3.
  
  
  Data_IFT <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/pratiques_paysages_2015.txt", header = TRUE, sep = "")
  
  Data_IFT <- rename(Data_IFT, Parc = parc)
  Data_IFT$IFT_Ins_Fon <- Data_IFT$IFTIns + Data_IFT$IFTFon
  
  # Fusionner les deux tables par la colonne 'Parc'
  data_merged <- merge(tab_pij2_All_species_Seuil_1_percent, CR_Paysage, by = "Parc")
  data_ACP <- merge(data_merged[, c("Parc", "FS", "AC", "AB", "HSNtot")], 
                       Data_IFT[, c("Parc", "IFTHer", "IFTIns", "IFTFon", "IFT_Ins_Fon", "surf", "Int_ti")], by = c("Parc"))
  
  # Sélection des colonnes de composition du paysage pour l'ACP
  paysage_vars <- data_ACP %>% select(AC, AB, HSNtot, IFTHer, IFTIns, IFTFon, IFT_Ins_Fon, surf, Int_ti)
  
  # Effectuer l'ACP
  pca_paysage <- prcomp(paysage_vars, center = TRUE,scale. = TRUE)
  
  # Valeurs propres (variance expliquée par chaque composante)
  pca_paysage$sdev^2

  # Contributions des variables aux composantes principales
  pca_paysage$rotation
  
  # Afficher un résumé des résultats de l'ACP
  summary(pca_paysage)
  
  # Extraire les scores des composantes principales
  pca_scores <- pca_paysage$x
  
  library(FactoMineR)
  library(factoextra)

  # Graphique des individus
  fviz_pca_ind(pca_paysage)
  
  # Graphique des variables
  fviz_pca_var(pca_paysage)
 
  # Biplot (individus + variables)
  fviz_pca_biplot(pca_paysage)

  
  fviz_eig(pca_paysage, addlabels = TRUE)
  
  fviz_pca_var(pca_paysage, col.var = "black", repel = TRUE)
  # All the variables that are grouped together are positively correlated to each other ().
  # The higher the distance between the variable and the origin, the better represented that variable is.
  # Variables that are negatively correlated are displayed to the opposite sides of the biplot’s origin.
  
  
  fviz_cos2(pca_paysage, choice = "var", axes = 1:2)
  
  
  fviz_pca_var(pca_paysage, col.var = "cos2",
               gradient.cols = c("black", "orange", "green"),
               repel = TRUE)
  
  # https://www.datacamp.com/tutorial/pca-analysis-r

  library(ggrepel)
  
  # Créer le biplot avec ellipses pour 'cult'
  p <- fviz_pca_biplot(pca_paysage,
                       habillage = data_ACP$Parc,  # Colorer les points selon 'cult'
                       addEllipses = TRUE # Ajouter des ellipses autour des groupes
  )
  
  p
  
  
  # Ajouter les étiquettes basées sur 'Parc'
  p + geom_text(aes(label = data_ACP$Parc), 
                vjust = -0.5,    # Ajuster la position verticale des étiquettes
                hjust = 1,       # Ajuster la position horizontale des étiquettes
                size = 3,        # Taille des étiquettes
                color = "black") # Couleur des étiquettes

  
  # Créer le biplot avec ellipses pour 'cult'
  p <- fviz_pca_biplot(pca_paysage,
                       habillage = data_ACP$FS,  # Colorer les points selon 'cult'
                       addEllipses = TRUE # Ajouter des ellipses autour des groupes
  )
  
  p
  
  # Ajouter les étiquettes basées sur 'Parc'
  p + geom_text(aes(label = data_ACP$Parc), 
                vjust = -0.5,    # Ajuster la position verticale des étiquettes
                hjust = 1,       # Ajuster la position horizontale des étiquettes
                size = 3,        # Taille des étiquettes
                color = "black") # Couleur des étiquettes
  
  
  
### Boucle sur les réseaux trophiques pour chaque prédateur ###

  library(reshape2)
  library(vegan)  # Assurez-vous que 'vegan' est installé pour utiliser 'plotweb'
  
  # Transformer le data frame en matrice
  interaction_matrix_All_species <- reshape2::dcast(tab_pij2_All_species_Seuil_1_percent, ReadName ~ Isp, value.var = "PijComb", fun.aggregate = sum, fill = 0)
  
  # Mettre les noms de lignes et de colonnes
  rownames(interaction_matrix_All_species) <- interaction_matrix_All_species[, 1]
  interaction_matrix_All_species <- as.matrix(interaction_matrix_All_species[, -1])                    
  
  # Trier les lignes par ordre alphabétique des noms de lignes
  interaction_matrix_All_species <- interaction_matrix_All_species[order(rownames(interaction_matrix_All_species)), ]
  
  # Obtenir les noms des colonnes
  col_names <- colnames(interaction_matrix_All_species)
  
  # Boucle sur chaque colonne
  n_col <- ncol(interaction_matrix_All_species)
  for (i in seq_len(n_col)) {
    # Sélectionner la colonne actuelle
    interaction_matrix_subset <- interaction_matrix_All_species[, i, drop = FALSE]
    
    # Vérifier si la matrice sélectionnée contient des données
    if (all(rowSums(interaction_matrix_subset) == 0)) {
      next  # Passer cette itération si toutes les valeurs sont à 0
    }
    
    # Définir le nom du fichier PNG
    file_name <- paste0("graph_", col_names[i], ".png")
    
    # Créer un fichier PNG avec une résolution plus élevée
    #png(filename = file_name, width = 3400, height = 2000, res = 300)
    
    # Paramètres graphiques pour réduire la taille du graphique
    op <- par(mar = c(5, 4, 4, 2) + 0.1, cex = 1)
    
    # Créer le graphique
    plotweb(interaction_matrix_subset, method = "normal", 
            low.lablength = 30, arrow = "down", high.lablength = 30,
            text.rot = 0, col.low = "grey", ybig = 1, 
            y.width.high = 0.03, y.width.low = 0.03, bor.col.interaction = NA)
    
    # Ajouter un titre avec le nom de la colonne
    title(main = paste("Graph for column:", col_names[i]))
    
    # Fermer le fichier PNG pour enregistrer le plot
    dev.off()
    
    # Confirmation de sauvegarde
    #cat("Saved", file_name, "\n")
  }
  
  
  

### Le régime alimentaire est-il différent entre bio et conv (en virant les effets confondants (Nb indiv capturés, ...), Analyses de sensibilité)
  interaction_counts <- tab_pij2_All_species_Seuil_1_percent %>%
    group_by(Parc) %>%
    summarize(NbInteractions = n())
  
  Test <- merge(tab_pij2_All_species_Seuil_1_percent, interaction_counts, by = "Parc")
  
  library(lme4)
  
  # Modèle mixte avec effet fixe pour la méthode de culture et effet aléatoire pour le parc
  model <- lmer(PijComb ~ FS + NbInteractions + (1 | Parc), data = Test)
  summary(model)
  
  # Il y a un effet significatif du nombre d’interactions sur PijComb.
  # Plus le nombre d’individus capturés augmente, plus les probabilités d'intéraction sont faibles.
  
  # ---
  
  tab_pij2_All_species_Seuil_1_percent$Parc_Red <- str_extract(tab_pij2_All_species_Seuil_1_percent$Parc, "^\\d+")
  
  # Visualiser la probabilité d'interaction par type de parcelle
  ggplot(tab_pij2_All_species_Seuil_1_percent, aes(x = Parc_Red, y = PijComb, color = FS)) +
    geom_boxplot() +
    labs(title = "Distribution des Probabilités d'Interaction par Type de Parcelle")

  ggplot(tab_pij2_All_species_Seuil_1_percent, aes(x = PijComb, fill = FS)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~Parc_Red) +
    labs(title = "Distribution des Probabilités d'Interaction par Type de Parcelle")
  
  
  test_diff_per_pair <- function(data) {
    # Liste des parcelles uniques
    Parc <- unique(tab_pij2_All_species_Seuil_1_percent$Parc_Red)
    
    # Liste pour stocker les résultats
    results <- data.frame(Parc = character(), p_value = numeric(), stringsAsFactors = FALSE)
    
    # Tester chaque parcelle
    for (Parc in Parc) {
      # Filtrer les données pour la parcelle actuelle
      subset_data <- tab_pij2_All_species_Seuil_1_percent[tab_pij2_All_species_Seuil_1_percent$Parc_Red == Parc, ]
      
      # Vérifier qu'il y a exactement 2 niveaux de FS pour la parcelle
      if(length(unique(subset_data$FS)) == 2) {
        # Effectuer un test de Wilcoxon-Mann-Whitney (non paramétrique)
        test_result <- wilcox.test(PijComb ~ FS, data = subset_data)
        
        # Stocker le résultat
        results <- rbind(results, data.frame(Parc = Parc, p_value = test_result$p.value))
      }
    }
    
    return(results)
  }
  
  # Appliquer la fonction aux données
  test_results <- test_diff_per_pair(tab_pij2_All_species_Seuil_1_percent)
  print(test_results)
  
#   Parc      p_value
#   1088 9.152002e-03
#   1435 5.946060e-07
#   1650 5.410627e-01
#   1662 4.526226e-01
#   1868 2.157376e-0
  
  
  library(caret)
  
  # Créer un objet de contrôle pour la validation croisée k-fold
  control <- trainControl(method = "cv", number = 10)
  
  # Entraîner un modèle en utilisant la validation croisée
  model <- train(PijComb ~ FS + Parc_Red, data = tab_pij2_All_species_Seuil_1_percent,
                 method = "glm", # ou un autre modèle comme "rf" pour Random Forest
                 trControl = control)
  
  # Résultats de la validation croisée
  print(model)
  
  # Exemple de données (simulées)
  set.seed(123)
  tab_pij2_All_species_Seuil_1_percent_Test <- data.frame(
    PijComb = rnorm(1000),
    FS = rep(c("B", "C"), each = 500),
    Parc_Red = sample(1:10, 1000, replace = TRUE)
  )
  # Modèle de régression de base
  base_model <- lm(PijComb ~ FS + Parc_Red, data = tab_pij2_All_species_Seuil_1_percent)
  summary(base_model)
  
  # Fonction pour ajuster le modèle avec différents seuils
  test_thresholds <- function(data, thresholds) {
    results <- data.frame(Threshold = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
    
    for (thresh in thresholds) {
      # Filtrer les données selon le seuil
      filtered_data <- data %>% filter(PijComb >= quantile(PijComb, probs = thresh))
      
      # Ajuster le modèle sur les données filtrées
      model <- lm(PijComb ~ FS + Parc_Red, data = filtered_data)
      
      # Obtenir la valeur p pour l'effet du type de parcelle
      p_value <- summary(model)$coefficients["FSC", "Pr(>|t|)"]
      
      # Stocker les résultats
      results <- rbind(results, data.frame(Threshold = thresh, p_value = p_value))
    }
    
    return(results)
  }
  
  # Testez avec plusieurs seuils
  thresholds <- seq(0.01, 0.10, by = 0.01)
  sensitivity_results <- test_thresholds(tab_pij2_All_species_Seuil_1_percent, thresholds)
  print(sensitivity_results)
  
  
  # Fonction pour comparer les méthodes statistiques
  compare_methods <- function(data) {
    results <- data.frame(Method = character(), p_value = numeric(), stringsAsFactors = FALSE)
    
    # Régression linéaire
    model_lm <- lm(PijComb ~ FS + Parc_Red, data = data)
    p_value_lm <- summary(model_lm)$coefficients["FSC", "Pr(>|t|)"]
    
    # Test de Wilcoxon-Mann-Whitney
    test_wilcox <- wilcox.test(PijComb ~ FS, data = data)
    p_value_wilcox <- test_wilcox$p.value
    
    # Stocker les résultats
    results <- rbind(results, data.frame(Method = "Linear Model", p_value = p_value_lm))
    results <- rbind(results, data.frame(Method = "Wilcoxon", p_value = p_value_wilcox))
    
    return(results)
  }
  
  # Comparer les méthodes
  method_comparison <- compare_methods(tab_pij2_All_species_Seuil_1_percent)
  print(method_comparison)
  
  
  # Fonction pour analyser les sous-groupes
  analyze_subgroups <- function(data, subgroups) {
    results <- data.frame(Subgroup = character(), p_value = numeric(), stringsAsFactors = FALSE)
    
    for (sub in subgroups) {
      # Filtrer les données selon le sous-groupe
      subgroup_data <- data %>% filter(Parc_Red == sub)
      
      # Ajuster le modèle
      model <- lm(PijComb ~ FS, data = subgroup_data)
      p_value <- summary(model)$coefficients["FSC", "Pr(>|t|)"]
      
      # Stocker les résultats
      results <- rbind(results, data.frame(Subgroup = sub, p_value = p_value))
    }
    
    return(results)
  }
  
  # Analyser différents sous-groupes
  subgroups <- unique(tab_pij2_All_species_Seuil_1_percent$Parc_Red)
  subgroup_results <- analyze_subgroups(tab_pij2_All_species_Seuil_1_percent, subgroups)
  print(subgroup_results)
  
  
  # Visualiser les résultats de la sensibilité au seuil
  ggplot(sensitivity_results, aes(x = Threshold, y = p_value)) +
    geom_line() +
    geom_point() +
    labs(title = "Sensibilité aux Seuils de Détection",
         x = "Seuil de Détection",
         y = "Valeur p")
  
  # Visualiser les résultats de la comparaison des méthodes
  ggplot(method_comparison, aes(x = Method, y = p_value)) +
    geom_bar(stat = "identity") +
    labs(title = "Comparaison des Méthodes Statistiques",
         x = "Méthode",
         y = "Valeur p")
  
  # Visualiser les résultats des sous-groupes
  ggplot(subgroup_results, aes(x = Subgroup, y = p_value)) +
    geom_bar(stat = "identity") +
    labs(title = "Sensibilité aux Sous-Groupes",
         x = "Sous-Groupe",
         y = "Valeur p")
  
  
  
  
  # Charger les bibliothèques nécessaires
  library(caret)
  library(ggplot2)
  
  # Simuler un jeu de données
  set.seed(123)
  data <- data.frame(
    PijComb = rnorm(1000),
    FS = factor(rep(c("B", "C"), each = 500)),
    Parc_Red = sample(1:10, 1000, replace = TRUE)
  )
  
  # Créer un objet de contrôle pour la validation croisée k-fold
  control <- trainControl(method = "cv", number = 10)
  
  # Entraîner le modèle de régression linéaire avec validation croisée
  model <- train(PijComb ~ FS + Parc_Red, data = data,
                 method = "lm",                # Méthode de régression linéaire
                 trControl = control)          # Contrôle de validation croisée
  
  # Afficher les résultats du modèle
  results <- model$results
  print(results)
  
  # Visualiser la performance moyenne (RMSE) avec écart type
  ggplot(results, aes(x = "", y = RMSE)) +
    geom_col(fill = "skyblue") +
    geom_errorbar(aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD), width = 0.2) +
    labs(title = "Erreur Quadratique Moyenne (RMSE) avec Écart Type",
         x = "Modèle",
         y = "Erreur Quadratique Moyenne (RMSE)") +
    theme_minimal()

  
  #  --- Calcul de la diversité de proies pour chaque prédateur par parcelle --------  #
library(vegan)
  # Calcul de la diversité des proies par prédateur et par parcelle
  diversity_data <- tab_pij2_All_species_Seuil_1_percent %>%
    group_by(Parc, Isp) %>%
    summarise(Diversity_Shannon = vegan::diversity(table(ReadName), index = "shannon"))
  
  # Afficher les résultats
  print(diversity_data)
  
  
  
  # Calculer les proportions des espèces de proies pour chaque prédateur dans chaque parcelle
  composition_data <- tab_pij2_All_species_Seuil_1_percent %>%
    group_by(Parc, Isp, ReadName) %>%
    summarise(Freq = n()) %>%  # Calcul de la fréquence de chaque proie
    mutate(Proportion = Freq / sum(Freq)) %>%  # Calcul de la proportion
    ungroup()
  
  # Afficher les résultats
  print(composition_data)

  
  # Fusionner les données de diversité des prédateurs et la composition des proies
  merged_data <- merge(diversity_data[,c("Parc","Isp", "Diversity_Shannon")], composition_data[,c("Parc","Isp", "Proportion")], by = c("Parc","Isp"))
  
  # Calculer la corrélation entre la diversité des prédateurs et la diversité des proies
  correlation_result <- cor.test(merged_data$Diversity_Shannon, merged_data$Proportion)
  print(correlation_result)  
  
  
  # Préparer les données pour la RDA
  # Fusionner les données de diversité des prédateurs et la composition des proies
  merged_data <- merge(diversity_data[, c("Parc", "Isp", "Diversity_Shannon")], 
                       composition_data[, c("Parc", "Isp", "Proportion")], by = c("Parc", "Isp"))
  
  # Calculer la corrélation entre la diversité des prédateurs et la composition des proies
  correlation_result <- cor.test(merged_data$Diversity_Shannon, merged_data$Proportion)
  print(correlation_result)
  
  # Préparer les données pour la RDA
  # Table de diversité des prédateurs (explanatory variables)
  X <- as.data.frame(diversity_data[, c("Parc","Diversity_Shannon"), drop = FALSE])
  str(X)
  class(X)
  # Table de composition des proies (response variables)
  Y <- as.data.frame.matrix(table(tab_pij2_All_species_Seuil_1_percent$Parc, 
                                  tab_pij2_All_species_Seuil_1_percent$ReadName))
  
  X_aggregated <- aggregate(. ~ Parc, data = X, FUN = mean)

  Y$Parc <- rownames(Y)
  Y <- as.matrix(Y)
  
  
  data_combined <- merge(Y, X_aggregated, by = "Parc")
  data_combined %>%
    relocate("Parc")
  
  # Vérifiez la structure du data frame pour identifier les types de colonnes
  str(data_combined[, -1])
  
  # Filtrer seulement les colonnes numériques
  data_combined_numeric <- data_combined[, sapply(data_combined[, -1], is.numeric)]
  
  # Vérifiez les colonnes conservées
  str(data_combined_numeric)
  
  data_combined_numeric <- data_combined[, -which(names(data_combined) == "Parc")]
  data_combined_numeric <- as.data.frame(data_combined_numeric)
  sapply(data_combined_numeric, class)
  # Convertir les colonnes en numériques si possible
  data_combined_numeric[] <- lapply(data_combined_numeric, function(x) as.numeric(as.character(x)))
  
  # Retirer les colonnes non numériques
  data_combined_numeric <- data_combined_numeric[, sapply(data_combined_numeric, is.numeric)]
  dim(Y)
  dim(data_combined_numeric)
  
  Y_matrix <- as.matrix(Y)
  dim(Y_matrix)
  
  # Appliquer la RDA avec les colonnes numériques uniquement
  #rda_result <- rda(Y_matrix ~ ., data = data_combined_numeric)
  
  # Résultats de la RDA
  #summary(rda_result)
  #plot(rda_result)
  
  # Modèle linéaire mixte
  library(lme4)
  lmm_model <- lmer(Diversity_Shannon ~ Proportion + (1|Parc), data = merged_data)
  
  # Résultats du modèle
  summary(lmm_model)
  
#Interprétation Générale
#Impact de la Proportion : La proportion semble avoir un effet très significatif et négatif sur la diversité de Shannon. Cela signifie qu'une augmentation de la proportion est associée à une réduction de la diversité dans les données de votre modèle.
#Variabilité entre les Parcelles : La variabilité entre les parcelles est faible, suggérant que la principale source de variabilité dans la diversité de Shannon est expliquée par la proportion plutôt que par les différences entre les parcelles.
  

# XV - Centrality ----------------------------------------

# Regarder la stabilité de la probabilité d’interaction avec l’ensemble des proies (graphiquement) = plot probabilité d’interaction par espèces de proies par parcelles

# plot probabilité d’interaction par espèces de proies par parcelles  

library(RColorBrewer)

  # ===== XV - Pardosa_sp -----------------------------------------------------------------------        
  p <- ggplot(Pardosa_data, aes(x = ReadName, y = PijComb, fill = Parc)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    scale_x_discrete(drop = FALSE) +
    labs(
      x = "Espèce de proie",
      y = "Probabilité d'interaction (PijComb)",
      title = "Probabilité d'interaction par espèce de proie et parcelles pour ARA_Pardosa_sp"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p
  
  #ggsave(filename = "Probabilite_Interaction_ARA_Pardosa_sp.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  
  
  p <- ggplot(Pardosa_data, aes(x = Parc, y = PijComb, fill = ReadName)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    scale_x_discrete(drop = FALSE) +
    labs(
      x = "Espèce de proie",
      y = "Probabilité d'interaction (PijComb)",
      title = "Probabilité d'interaction par parcelles et par espèce de proie pour ARA_Pardosa_sp"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p
  
  #ggsave(filename = "Probabilite_Interaction_ARA_Pardosa_sp_2.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  
  
  # Comparer les probabilités d'interaction pour chaque proie
  # Calculer les moyennes de PijComb par ReadName
  Pardosa_mean_pijcomb <- aggregate(PijComb ~ ReadName, data = Pardosa_data, FUN = mean)
  print(Pardosa_mean_pijcomb)
  
  # Analyse statistique : ANOVA pour tester si les moyennes de PijComb diffèrent significativement entre les différentes proies.
  # ANOVA à un facteur : déterminer s'il existe une préférence significative pour certaines proies.
  # ANOVA pour tester si les moyennes de PijComb diffèrent significativement entre les ReadName
  anova_result <- aov(PijComb ~ ReadName, data = Pardosa_data)
  summary(anova_result)
  
  # L'ANOVA montre une différence significative, on utilise un test post-hoc (Tukey HSD) pour identifier quelles paires de proies diffèrent significativement en termes de probabilité d'interaction.
  # Test post-hoc pour voir quelles proies sont préférées
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)  
  
  # Boxplot pour visualiser la distribution des PijComb pour chaque ReadName
  library(ggplot2)
  
  ggplot(Pardosa_data, aes(x = ReadName, y = PijComb)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution des probabilités d'interaction (PijComb) par proie",
         x = "Proie (ReadName)",
         y = "Probabilité d'interaction (PijComb)")  
  
  # Calculer la moyenne globale de PijComb
  mean_pijcomb_overall <- mean(Pardosa_data$PijComb)
  
  # Pour chaque proie, calculer un rapport de préférence :
  # Rapport de préférence = Moyenne de PijComb pour une proie / Moyenne de PijComb pour toutes les proies
  # Un rapport supérieur à 1 indique une préférence pour cette proie, tandis qu'un rapport inférieur à 1 indique une préférence moindre.
  
  # Ajouter une colonne avec le rapport de préférence
  Pardosa_data$PreferenceRatio <- Pardosa_data$PijComb / mean_pijcomb_overall
  
  # Visualiser les rapports de préférence
  print(aggregate(PreferenceRatio ~ ReadName, data = Pardosa_data, FUN = mean))
  
  # Classer les données par ordre décroissant de PreferenceRatio
  Pardosa_data_sorted <- aggregate(PreferenceRatio ~ ReadName, data = Pardosa_data, FUN = mean)
  Pardosa_data_sorted <- Pardosa_data_sorted[order(-Pardosa_data_sorted$PreferenceRatio), ]
  
  # Ajouter une colonne de couleurs en fonction du rapport de préférence
  Pardosa_data_sorted$Color <- factor(cut(Pardosa_data_sorted$PreferenceRatio,
                                          breaks = c(-Inf, 0.9, 1.1, Inf),
                                          labels = c("Proie moins privilégiée", 
                                                     "Proie moyennement privilégiée", 
                                                     "Proie privilégiée")),
                                      levels = c("Proie privilégiée", 
                                                 "Proie moyennement privilégiée", 
                                                 "Proie moins privilégiée"))
  
  # Créer un barplot avec les ReadName classés par ordre décroissant de PreferenceRatio et couleurs personnalisées
  p <- ggplot(Pardosa_data_sorted, aes(x = reorder(ReadName, -PreferenceRatio), y = PreferenceRatio, fill = Color)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Proie privilégiée" = "darkgreen",
                                 "Proie moyennement privilégiée" = "lightgreen", 
                                 "Proie moins privilégiée" = "lightgrey"),
                      labels = c("Proie privilégiée", 
                                 "Proie moyennement privilégiée", 
                                 "Proie moins privilégiée")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Rapport de préférence des proies pour ARA_Pardosa_sp",
         x = "Proie (ReadName)",
         y = "Rapport de Préférence (moyenne)",
         fill = "Catégorie de Préférence")  # Ajouter une légende pour les couleurs
  
  p
  
  ggsave(filename = "Rapport de préférence des proies pour ARA_Pardosa_sp.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  
  # ===== XV - INS_Chrysopidae_sp -----------------------------------------------------------------------        
  p <- ggplot(Chrysopidae_data, aes(x = ReadName, y = PijComb, fill = Parc)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    scale_x_discrete(drop = FALSE) +
    labs(
      x = "Espèce de proie",
      y = "Probabilité d'interaction (PijComb)",
      title = "Probabilité d'interaction par espèce de proie et parcelles pour INS_Chrysopidae_sp"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p
  
  #ggsave(filename = "Probabilite_Interaction_INS_Chrysopidae_sp.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  
  p <- ggplot(Chrysopidae_data, aes(x = Parc, y = PijComb, fill = ReadName)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    scale_x_discrete(drop = FALSE) +
    labs(
      x = "Espèce de proie",
      y = "Probabilité d'interaction (PijComb)",
      title = "Probabilité d'interaction par parcelles et par espèce de proie pour INS_Chrysopidae_sp"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p
  
  #ggsave(filename = "Probabilite_Interaction_INS_Chrysopidae_sp_2.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  
  
  # Comparer les probabilités d'interaction pour chaque proie
  # Calculer les moyennes de PijComb par ReadName
  Chrysopidae_mean_pijcomb <- aggregate(PijComb ~ ReadName, data = Chrysopidae_data, FUN = mean)
  print(Chrysopidae_mean_pijcomb)
  
  # Analyse statistique : ANOVA pour tester si les moyennes de PijComb diffèrent significativement entre les différentes proies.
  # ANOVA à un facteur : déterminer s'il existe une préférence significative pour certaines proies.
  # ANOVA pour tester si les moyennes de PijComb diffèrent significativement entre les ReadName
  anova_result <- aov(PijComb ~ ReadName, data = Chrysopidae_data)
  summary(anova_result)
  
  # L'ANOVA montre une différence significative, on utilise un test post-hoc (Tukey HSD) pour identifier quelles paires de proies diffèrent significativement en termes de probabilité d'interaction.
  # Test post-hoc pour voir quelles proies sont préférées
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)  
  
  # Boxplot pour visualiser la distribution des PijComb pour chaque ReadName
  library(ggplot2)
  
  ggplot(Chrysopidae_data, aes(x = ReadName, y = PijComb)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution des probabilités d'interaction (PijComb) par proie",
         x = "Proie (ReadName)",
         y = "Probabilité d'interaction (PijComb)")  
  
  # Calculer la moyenne globale de PijComb
  mean_pijcomb_overall <- mean(Chrysopidae_data$PijComb)
  
  
  # Pour chaque proie, calculer un rapport de préférence :
  # Rapport de préférence = Moyenne de PijComb pour une proie / Moyenne de PijComb pour toutes les proies
  # Un rapport supérieur à 1 indique une préférence pour cette proie, tandis qu'un rapport inférieur à 1 indique une préférence moindre.
  
  # Ajouter une colonne avec le rapport de préférence
  Chrysopidae_data$PreferenceRatio <- Chrysopidae_data$PijComb / mean_pijcomb_overall
  
  # Visualiser les rapports de préférence
  print(aggregate(PreferenceRatio ~ ReadName, data = Chrysopidae_data, FUN = mean))
  
  # Classer les données par ordre décroissant de PreferenceRatio
  Chrysopidae_data_sorted <- aggregate(PreferenceRatio ~ ReadName, data = Chrysopidae_data, FUN = mean)
  Chrysopidae_data_sorted <- Chrysopidae_data_sorted[order(-Chrysopidae_data_sorted$PreferenceRatio), ]
  
  # Ajouter une colonne de couleurs en fonction du rapport de préférence
  Chrysopidae_data_sorted$Color <- factor(cut(Chrysopidae_data_sorted$PreferenceRatio,
                                          breaks = c(-Inf, 0.9, 1.1, Inf),
                                          labels = c("Proie moins privilégiée", 
                                                     "Proie moyennement privilégiée", 
                                                     "Proie privilégiée")),
                                      levels = c("Proie privilégiée", 
                                                 "Proie moyennement privilégiée", 
                                                 "Proie moins privilégiée"))
  
  # Créer un barplot avec les ReadName classés par ordre décroissant de PreferenceRatio et couleurs personnalisées
  p <- ggplot(Chrysopidae_data_sorted, aes(x = reorder(ReadName, -PreferenceRatio), y = PreferenceRatio, fill = Color)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Proie privilégiée" = "darkgreen",
                                 "Proie moyennement privilégiée" = "lightgreen", 
                                 "Proie moins privilégiée" = "lightgrey"),
                      labels = c("Proie privilégiée", 
                                 "Proie moyennement privilégiée", 
                                 "Proie moins privilégiée")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Rapport de préférence des proies pour INS_Chrysopidae_sp",
         x = "Proie (ReadName)",
         y = "Rapport de Préférence (moyenne)",
         fill = "Catégorie de Préférence")  # Ajouter une légende pour les couleurs
  
  p
  
  ggsave(filename = "Rapport de préférence des proies pour INS_Chrysopidae_sp.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  

# ===== XV - INS_Lasius_niger -----------------------------------------------------------------------        
  p <- ggplot(Lasius_data, aes(x = ReadName, y = PijComb, fill = Parc)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    scale_x_discrete(drop = FALSE) +
    labs(
      x = "Espèce de proie",
      y = "Probabilité d'interaction (PijComb)",
      title = "Probabilité d'interaction par espèce de proie et parcelles pour INS_Lasius_niger"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p
  
  #ggsave(filename = "Probabilite_Interaction_INS_Lasius_niger.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  

  p <- ggplot(Lasius_data, aes(x = Parc, y = PijComb, fill = ReadName)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    scale_x_discrete(drop = FALSE) +
    labs(
      x = "Espèce de proie",
      y = "Probabilité d'interaction (PijComb)",
      title = "Probabilité d'interaction par parcelles et par espèce de proie pour INS_Lasius_niger"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p
  
  #ggsave(filename = "Probabilite_Interaction_INS_Lasius_niger_2.jpeg", plot = p, width = 10, height = 7, dpi = 300)
  
  
# Comparer les probabilités d'interaction pour chaque proie
  
  # Calculer les moyennes de PijComb par ReadName
  Lasius_mean_pijcomb <- aggregate(PijComb ~ ReadName, data = Lasius_data, FUN = mean)
  print(Lasius_mean_pijcomb)

# Analyse statistique : ANOVA pour tester si les moyennes de PijComb diffèrent significativement entre les différentes proies.
  # ANOVA à un facteur : déterminer s'il existe une préférence significative pour certaines proies.
  
  # Tester si les moyennes de PijComb diffèrent significativement entre les ReadName
  anova_result <- aov(PijComb ~ ReadName, data = Lasius_data)
  summary(anova_result)
  
  # L'ANOVA montre une différence significative, on utilise un test post-hoc (Tukey HSD) pour identifier quelles paires de proies diffèrent significativement en termes de probabilité d'interaction.
  # Test post-hoc pour voir quelles proies sont préférées
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)  
  
  # Boxplot pour visualiser la distribution des PijComb pour chaque ReadName
  library(ggplot2)
  
  ggplot(Lasius_data, aes(x = ReadName, y = PijComb)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution des probabilités d'interaction (PijComb) par proie",
         x = "Proie (ReadName)",
         y = "Probabilité d'interaction (PijComb)")  
  
  # Pour chaque proie, calculer un rapport de préférence :
  # Rapport de préférence = Moyenne de PijComb pour une proie / Moyenne de PijComb pour toutes les proies
  # Un rapport supérieur à 1 indique une préférence pour cette proie, tandis qu'un rapport inférieur à 1 indique une préférence moindre.
  # Calculer la moyenne globale de PijComb
  mean_pijcomb_overall <- mean(Lasius_data$PijComb)
  
  # Ajouter une colonne avec le rapport de préférence
  Lasius_data$PreferenceRatio <- Lasius_data$PijComb / mean_pijcomb_overall
  
  # Visualiser les rapports de préférence
  print(aggregate(PreferenceRatio ~ ReadName, data = Lasius_data, FUN = mean))
  
  # Classer les données par ordre décroissant de PreferenceRatio
  Lasius_data_sorted <- aggregate(PreferenceRatio ~ ReadName, data = Lasius_data, FUN = mean)
  Lasius_data_sorted <- Lasius_data_sorted[order(-Lasius_data_sorted$PreferenceRatio), ]
  
  # Ajouter une colonne de couleurs en fonction du rapport de préférence
  Lasius_data_sorted$Color <- factor(cut(Lasius_data_sorted$PreferenceRatio,
                                              breaks = c(-Inf, 0.9, 1.1, Inf),
                                              labels = c("Proie moins privilégiée", 
                                                         "Proie moyennement privilégiée", 
                                                         "Proie privilégiée")),
                                          levels = c("Proie privilégiée", 
                                                     "Proie moyennement privilégiée", 
                                                     "Proie moins privilégiée"))
  
  # Créer un barplot avec les ReadName classés par ordre décroissant de PreferenceRatio et couleurs personnalisées
  p <- ggplot(Lasius_data_sorted, aes(x = reorder(ReadName, -PreferenceRatio), y = PreferenceRatio, fill = Color)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Proie privilégiée" = "darkgreen",
                                 "Proie moyennement privilégiée" = "lightgreen", 
                                 "Proie moins privilégiée" = "lightgrey"),
                      labels = c("Proie privilégiée", 
                                 "Proie moyennement privilégiée", 
                                 "Proie moins privilégiée")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Rapport de préférence des proies pour INS_Lasius_niger",
         x = "Proie (ReadName)",
         y = "Rapport de Préférence (moyenne)",
         fill = "Catégorie de Préférence")  # Ajouter une légende pour les couleurs
  
  p
  
  ggsave(filename = "Rapport de préférence des proies pour INS_Lasius_niger.jpeg", plot = p, width = 10, height = 7, dpi = 300)

  
  
# ===== XVI - Point du 03-09-2024 avec Adrien -----------------------------------------------------------------------        
  # Uiterwaal 2023
  
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
  
  
  #------------------------------------------------------------------------
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


#============ Pianka juste pour Cica, Phyl et Tort
  
  # Filtrer les lignes où ReadName contient "Cica", "Phyl" ou "Tort"
  tab_pij2_All_species_Seuil_1_percent_preys <- tab_pij2_All_species_Seuil_1_percent %>%
    filter(grepl("Cica|Phyl|Tort", ReadName))
  
  
  # Obtenir toutes les valeurs uniques de la colonne Parc
  parc_values <- unique(tab_pij2_All_species_Seuil_1_percent_preys$Parc)
  
  
  # Fonction pour filtrer la table en fonction du parc
  filter_by_parc <- function(parc) {
    tab_pij2_filtered <- tab_pij2_All_species_Seuil_1_percent_preys %>%
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
  pianka_df_preys <- data.frame(Parc = names(pianka_results), 
                                Pianka_index_preys = unlist(pianka_results))
  
  # Afficher le tableau final avec les résultats de Pianka
  print(pianka_df_preys)
  
  
#=== Calcul des indices de diversité de Shannon (prédateurs et proies) par parcelles

  # Compter la fréquence des prédateurs dans chaque parc
  df_counts_Isp <- tab_pij2_All_species_Seuil_1_percent %>%
    count(Parc, Isp) %>%
    group_by(Parc) %>%
    mutate(Proportion = n / sum(n))
  
  # Calculer la diversité de Shannon pour chaque parc
  Shannon_Isp <- df_counts_Isp %>%
    group_by(Parc) %>%
    summarise(Predator_Shannon_Diversity = vegan::diversity(Proportion, index = "shannon"))
  
  
  # Compter la fréquence des proies dans chaque parc
  df_counts_ReadName <- tab_pij2_All_species_Seuil_1_percent %>%
    count(Parc, ReadName) %>%
    group_by(Parc) %>%
    mutate(Proportion = n / sum(n))
  
  # Calculer la diversité de Shannon pour chaque parc
  Shannon_ReadName <- df_counts_ReadName %>%
    group_by(Parc) %>%
    summarise(Prey_Shannon_Diversity = vegan::diversity(Proportion, index = "shannon"))
  
  
#=== Rassemblement des données

#Données de paysage

# Récupérer les valeurs uniques de HSNtot pour chaque Parc
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
  left_join(Shannon_Isp, by = "Parc")
    
Data_modeles <- Data_modeles %>%
  left_join(Shannon_ReadName, by = "Parc")

Data_modeles <- Data_modeles %>%
  left_join(pHSN, by = "Parc")


#=== Matrice de corrélation des 4 variables explicatrices du modèle

# Calculer la matrice de corrélation
correlation_matrix <- cor(Data_modeles[,-c(1,3)])

print(correlation_matrix)


# Convertir la matrice de corrélation en data frame
melted_correlation_matrix <- melt(correlation_matrix)

# Créer le heatmap
ggplot(melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#=== Modèles
  
# Centrer et réduire les données
#Data_modeles$Pianka_index_scaled <- scale(Data_modeles$Pianka_index)
#Data_modeles$Predator_Shannon_Diversity_scaled <- scale(Data_modeles$Predator_Shannon_Diversity)
#Data_modeles$Prey_Shannon_Diversity_scaled <- scale(Data_modeles$Prey_Shannon_Diversity)
#Data_modeles$HSNtot_scaled <- scale(Data_modeles$HSNtot)


# a)	Pianka ~ Shannon Isp + Shannon ReadName
    #	Si les deux Shannon ne sont pas significatifs, on passe à la b)
    # Si un seul Shannon est significatif, on passe à la c)
    # Si les deux Shannon sont significatifs, on passe à la d)

model_a <- lm(Pianka_index ~ Predator_Shannon_Diversity + Prey_Shannon_Diversity, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_a_summary <- tidy(model_a)
# Afficher les résultats
print(model_a_summary)


#model_a_scaled <- lm(Pianka_index_scaled ~ Predator_Shannon_Diversity_scaled + Prey_Shannon_Diversity_scaled, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
#model_a_summary_scaled <- tidy(model_a_scaled)
# Afficher les résultats
#print(model_a_summary_scaled)

# Prey_Shannon_Diversity est significatif donc c)

# b)	Pianka ~ FS + pHSN

model_b <- lm(Pianka_index ~ FS + HSNtot, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)

#model_b_scaled <- lm(Pianka_index_scaled ~ FS + HSNtot_scaled, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
#model_b_summary_scaled <- tidy(model_b_scaled)
# Afficher les résultats
#print(model_b_summary_scaled)

# c)	Pianka ~ FS + pHSN + Shannon (Isp ou ReadName)

model_c <- lm(Pianka_index ~ FS + HSNtot + Prey_Shannon_Diversity, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)

#model_c_scaled <- lm(Pianka_index_scaled ~ FS + HSNtot_scaled + Prey_Shannon_Diversity_scaled, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
#model_c_summary_scaled <- tidy(model_c_scaled)
# Afficher les résultats
#print(model_c_summary_scaled)

# d)	Pianka ~ FS + pHSN + Shannon Isp + Shannon ReadName

model_d <- lm(Pianka_index ~ FS + HSNtot + Predator_Shannon_Diversity + Prey_Shannon_Diversity, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_d_summary <- tidy(model_d)
# Afficher les résultats
print(model_d_summary)

#model_d_scaled <- lm(Pianka_index_scaled ~ FS + HSNtot_scaled + Predator_Shannon_Diversity_scaled + Prey_Shannon_Diversity_scaled, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
#model_d_summary_scaled <- tidy(model_d_scaled)
# Afficher les résultats
#print(model_d_summary_scaled)


#=== Graphiques des relations entre variables

ggplot(data = Data_modeles, aes(x = Prey_Shannon_Diversity, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Prey_Shannon_Diversity", y = "Pianka_index")

ggplot(data = Data_modeles, aes(x = Prey_Shannon_Diversity, y = Pianka_index, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Prey_Shannon_Diversity", y = "Pianka_index")



ggplot(data = Data_modeles, aes(x = Predator_Shannon_Diversity, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Predator_Shannon_Diversity", y = "Pianka_index")

ggplot(data = Data_modeles, aes(x = Predator_Shannon_Diversity, y = Pianka_index, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Predator_Shannon_Diversity", y = "Pianka_index")



ggplot(data = Data_modeles, aes(x = HSNtot, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "HSNtot", y = "Pianka_index")

ggplot(data = Data_modeles, aes(x = HSNtot, y = Pianka_index, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "HSNtot", y = "Pianka_index")


ggplot(data = Data_modeles, aes(x = FS, y = Pianka_index, fill = FS)) +
  geom_boxplot() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "HSNtot", y = "Pianka_index")



library(spaa)
library(reshape2)
library(dplyr)

# Fonction pour calculer les chevauchements de niche
calculate_niche_overlap <- function(interaction_matrix) {
  # Calculer les chevauchements de niche
  overlap <- niche.overlap(interaction_matrix, method = "levins")

  # Retourner les moyennes des chevauchements
  return(c(overlap = mean(overlap, na.rm = TRUE)))
}

# Fonction pour effectuer le bootstrap et calculer les chevauchements de niche
bootstrap_matrix <- function(data, n_bootstrap = 1000, sample_size = 20) {
  # Initialiser des vecteurs pour stocker les valeurs de niche overlap
  overlap_values <- numeric(n_bootstrap)

  for (b in 1:n_bootstrap) {
    # Rééchantillonnage bootstrap des données
    bootstrap_data <- data %>%
      group_by(Isp) %>%
      sample_n(size = min(sample_size, n()), replace = TRUE) %>%
      ungroup()
    
    # Créer la matrice d'interaction entre les prédateurs et les proies
    interaction_matrix <- dcast(bootstrap_data, Isp ~ ReadName, value.var = "PijComb", fun.aggregate = mean, fill = 0)
    
    # Convertir la matrice en un format adapté pour l'analyse de chevauchement
    interaction_matrix_data <- as.matrix(interaction_matrix[, -1])  # Retirer la colonne des noms de prédateurs (Isp)
    rownames(interaction_matrix_data) <- interaction_matrix$Isp  # Nommer les lignes par les prédateurs
    
    # Calculer les chevauchements de niche
    niche_overlap <- calculate_niche_overlap(interaction_matrix_data)
    
    # Stocker les valeurs de niche overlap
    overlap_values[b] <- niche_overlap["overlap"]
  }
  
  # Retourner les moyennes des valeurs de niche overlap obtenues via le bootstrap
  return(list(mean_niche_overlap = mean(overlap_values)))
}

# Liste pour stocker les résultats
niche_overlap_results <- list()

# Boucle sur chaque tableau d'interaction
for (nom_tableau in names(tableaux_interaction)) {
  # Récupérer le tableau actuel
  tab_pij2_filtered <- tableaux_interaction[[nom_tableau]]
  
  # Calculer la moyenne des indices de chevauchement de niche avec bootstrap
  mean_niche_overlap <- bootstrap_matrix(tab_pij2_filtered, n_bootstrap = 1000, sample_size = 20)
  
  # Stocker les résultats dans la liste avec le nom du tableau d'interaction
  niche_overlap_results[[nom_tableau]] <- mean_niche_overlap
}

# Convertir la liste des résultats en tableau (data frame)
niche_overlap_df <- do.call(rbind, lapply(names(niche_overlap_results), function(name) {
  result <- niche_overlap_results[[name]]
  data.frame(Parc = name, 
             Niche_Overlap = result$mean_niche_overlap)
}))

# Afficher le tableau final avec les résultats de niche overlap
print(niche_overlap_df)



# -----
Data_modeles_1 <- Data_modeles %>%
  left_join(niche_overlap_df, by = "Parc")


#=== Matrice de corrélation des 4 variables explicatrices du modèle

# Calculer la matrice de corrélation
correlation_matrix <- cor(Data_modeles_1[,-c(1,3)])

print(correlation_matrix)


# Convertir la matrice de corrélation en data frame
melted_correlation_matrix <- melt(correlation_matrix)

# Créer le heatmap
ggplot(melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#=== Graphiques des relations entre variables

ggplot(data = Data_modeles_1, aes(x = Prey_Shannon_Diversity, y = Niche_Overlap)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Prey_Shannon_Diversity", y = "Niche_Overlap")


model_e <- lm(Niche_Overlap ~ FS + HSNtot + Predator_Shannon_Diversity + Prey_Shannon_Diversity, data = Data_modeles_1)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_e_summary <- tidy(model_e)
# Afficher les résultats
print(model_e_summary)

model_e <- lm(Niche_Overlap ~ FS + HSNtot + Prey_Shannon_Diversity, data = Data_modeles_1)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_e_summary <- tidy(model_e)
# Afficher les résultats
print(model_e_summary)



# NMDS

# Charger les packages nécessaires
library(vegan)
library(ggplot2)
library(ggrepel)  # Pour éviter la superposition des labels


# Vecteur des parcelles
interaction_df <- tab_pij2_All_species_Seuil_1_percent[,c("Parc", "PijComb", "Isp", "ReadName")]

# Reformater les données en une matrice d'interaction (prédateurs en lignes, proies en colonnes)
interaction_matrix <- dcast(interaction_df, Isp ~ ReadName, value.var = "PijComb", fun.aggregate = sum, fill = 0)

# Enlever la colonne des prédateurs pour la NMDS
interaction_matrix_data <- as.matrix(interaction_matrix[,-1])

# Réaliser l'analyse NMDS
dist_matrix <- vegdist(interaction_matrix_data, method = "bray")
nmds_result <- metaMDS(dist_matrix, k = 2, trymax = 100)

# Créer un dataframe pour visualiser avec ggplot
nmds_df <- as.data.frame(nmds_result$points)
nmds_df$predateur <- interaction_matrix$Isp
nmds_df$parcelle <- interaction_df$Parc[match(nmds_df$predateur, interaction_df$Isp)]  # Associer les parcelles aux prédateurs
nmds_df$FS <- substr(nmds_df$parcelle, nchar(nmds_df$parcelle), nchar(nmds_df$parcelle))

# Visualiser les résultats NMDS avec ggplot2
ggplot(nmds_df, aes(x = MDS1, y = MDS2, color = FS, label = predateur)) +
  geom_point(size = 4) +
  geom_text_repel() +
  stat_ellipse(aes(fill = FS), alpha = 0.2, level = 0.95) +
  labs(title = "NMDS des Régimes Alimentaires des Prédateurs en Fonction des Parcelles",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

# Visualiser les résultats NMDS avec ggplot2
ggplot(nmds_df, aes(x = MDS1, y = MDS2, color = parcelle, label = predateur)) +
  geom_point(size = 4) +
  geom_text_repel() +
  stat_ellipse(aes(fill = FS), alpha = 0.2, level = 0.95) +
  labs(title = "NMDS des Régimes Alimentaires des Prédateurs en Fonction des Parcelles",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()


# Regrouper par parcelle et prédateur, et sommer les interactions
df_grouped <- interaction_df %>%
  group_by(Parc, Isp, ReadName) %>%
  summarise(PijComb = sum(PijComb)) %>%
  ungroup()

# Reformater les données en une matrice (prédateurs * proies) pour chaque parcelle
interaction_matrix <- dcast(df_grouped, Isp + Parc ~ ReadName, value.var = "PijComb", fun.aggregate = sum, fill = 0)

# Séparer les colonnes de parcelles pour les utiliser plus tard
parcelles <- interaction_matrix$Parc
interaction_matrix <- as.matrix(interaction_matrix[,-c(1,2)]) # Enlever les colonnes Isp et Parc

# Réaliser l'analyse NMDS
dist_matrix <- vegdist(interaction_matrix, method = "bray")
nmds_result <- metaMDS(dist_matrix, k = 2, trymax = 100)

# Créer un dataframe pour visualiser avec ggplot
nmds_df <- as.data.frame(nmds_result$points)
nmds_df$predateur <- df_grouped$Isp[match(row.names(nmds_df), df_grouped$Isp)]  # Associer prédateurs
nmds_df$parcelle <- parcelles  # Associer parcelles

# Visualiser les résultats NMDS avec ggplot2 et ajouter des labels sur les points
ggplot(nmds_df, aes(x = MDS1, y = MDS2, color = parcelle)) +
  geom_point(size = 2) +  # Points pour chaque prédateur
  geom_text_repel(aes(label = predateur), size = 2, max.overlaps = Inf) +  # Labels des prédateurs
  labs(title = "NMDS des régimes alimentaires des prédateurs en fonction des parcelles",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()


#########################

###################################
# Antoptic 20-09-2024
###################################

# Remarque 2 - Pour pest_abundance => il faut le niveau d'infestation
# et faire 3 modèles : mod1:Fcica, mod2:Fphyll et mod3:Ftord


# I - Relation Niche.Overlap ~ abondance de certains taxons (pests)

Pest_abundance <- CR_Paysage[,c("Parc", "Fcica", "Fphyl", "Ftord")]

Pest_abundance <- Pest_abundance %>%
  dplyr::group_by(Parc) %>%
  dplyr::summarise(across(c(Fcica, Fphyl, Ftord), ~ mean(.x, na.rm = TRUE)))


Data_modeles_pests <- Data_modeles_1[,c("Parc", "Pianka_index", "Niche_Overlap")] %>%
  left_join(Pest_abundance, by = "Parc")


#=== Graphiques des relations entre variables avec Pianka_index

# mod1:Fcica
# Pianka_index
ggplot(data = Data_modeles_pests, aes(x = Fcica, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Fcica", y = "Pianka_index")


model_f_cica <- lm(Pianka_index ~ Fcica, data = Data_modeles_pests)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_f_cica_summary <- tidy(model_f_cica)
# Afficher les résultats
print(model_f_cica_summary)


# mod2:Fphyl
# Pianka_index
ggplot(data = Data_modeles_pests, aes(x = Fphyl, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Fphyl", y = "Pianka_index")


model_f_phyl <- lm(Pianka_index ~ Fphyl, data = Data_modeles_pests)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_f_phyl_summary <- tidy(model_f_phyl)
# Afficher les résultats
print(model_f_phyl_summary)


# mod3:Ftord
# Pianka_index
ggplot(data = Data_modeles_pests, aes(x = Ftord, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Ftord", y = "Pianka_index")


model_f_tord <- lm(Pianka_index ~ Ftord, data = Data_modeles_pests)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_f_tord_summary <- tidy(model_f_tord)
# Afficher les résultats
print(model_f_tord_summary)



#=== Graphiques des relations entre variables avec Niche_Overlap

# mod1:Fcica
# Niche_Overlap
ggplot(data = Data_modeles_pests, aes(x = Fcica, y = Niche_Overlap)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Fcica", y = "Niche_Overlap")


model_g_cica <- lm(Niche_Overlap ~ Fcica, data = Data_modeles_pests)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_g_cica_summary <- tidy(model_g_cica)
# Afficher les résultats
print(model_g_cica_summary)


# mod2:Fphyl
# Niche_Overlap
ggplot(data = Data_modeles_pests, aes(x = Fphyl, y = Niche_Overlap)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Fphyl", y = "Niche_Overlap")


model_g_phyl <- lm(Niche_Overlap ~ Fphyl, data = Data_modeles_pests)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_g_phyl_summary <- tidy(model_g_phyl)
# Afficher les résultats
print(model_g_phyl_summary)


# mod3:Ftord
# Niche_Overlap
ggplot(data = Data_modeles_pests, aes(x = Ftord, y = Niche_Overlap)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Ftord", y = "Niche_Overlap")


model_g_tord <- lm(Niche_Overlap ~ Ftord, data = Data_modeles_pests)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_g_tord_summary <- tidy(model_g_tord)
# Afficher les résultats
print(model_g_tord_summary)



#########################

# II - # Relation Niche.Overlap ~ Régulation ?




#########################

# III - # Relation Niche.Overlap ~ Prey_Shannon expliqué par IFT ?

Data_modeles_pests_IFT <- Data_modeles_pests %>%
  left_join(Data_IFT, by = "Parc")

Data_modeles_pests_IFT <- Data_modeles_pests_IFT %>%
  left_join(Data_modeles[,c("Parc", "Prey_Shannon_Diversity")], by = "Parc")

Data_modeles_pests_IFT$IFT_Ins_Fon <- Data_modeles_pests_IFT$IFTIns + Data_modeles_pests_IFT$IFTFon


#=== Graphiques des relations entre variables

ggplot(Data_modeles_pests_IFT, aes(x = Prey_Shannon_Diversity, y = Pianka_index, color = IFT_Ins_Fon)) +
  geom_point() +  # Points de données
  geom_smooth(method = "lm", aes(group = 1), col = "blue") +  # Régression linéaire globale
  labs(title = "Relation entre Pianka_index et Prey_Shannon_Diversity avec IFT_Ins_Fon",
       x = "Prey_Shannon_Diversity",
       y = "Pianka_index") +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "darkblue")  # Gradient de couleur basé sur IFT

ggplot(Data_modeles_pests_IFT, aes(x = Prey_Shannon_Diversity, y = Pianka_index, size = IFT_Ins_Fon)) +
  geom_point(alpha = 0.6) +  # Taille des points varie avec IFT
  geom_smooth(method = "lm", col = "blue") +  # Régression linéaire
  labs(title = "Relation entre Pianka_index et Prey_Shannon_Diversity avec IFT_Ins_Fon",
       x = "Prey_Shannon_Diversity",
       y = "Pianka_index") +
  theme_minimal()



ggplot(Data_modeles_pests_IFT, aes(x = Prey_Shannon_Diversity, y = Pianka_index, color = factor(IFT_Ins_Fon), size = IFT_Ins_Fon)) +
  geom_point() + 
  geom_smooth(method = "lm", aes(group = IFT_Ins_Fon), se = FALSE) +  # Régression linéaire par groupe IFT_Ins_Fon
  geom_smooth(method = "lm", col = "red") +
  geom_text(aes(label = Parc), vjust = -1, hjust = 0.5) +  # Ajouter les labels Parc au-dessus des points
  scale_size_continuous(range = c(2, 10)) +  # Ajuster l'échelle de taille des points
  labs(title = "Relation entre Pianka_index et Prey_Shannon_Diversity par IFT_Ins_Fon",
       x = "Prey_Shannon_Diversity",
       y = "Pianka_index") +
  theme_minimal()


########

model <- lm(Pianka_index ~ Prey_Shannon_Diversity + IFT_Ins_Fon, data = Data_modeles_pests_IFT)

# Résumé du modèle
summary(model)



######################

# Réseaux trophiques par parcelle
# Extraire les valeurs uniques de la colonne 'Parc'
parc_values <- unique(tab_pij2_All_species_Seuil_1_percent$Parc)

# Générer une palette de couleurs, par exemple avec RColorBrewer (ou d'autres)
colors <- grDevices::rainbow(length(parc_values))  # Utiliser un ensemble de couleurs distinctes

# Initialiser une liste pour stocker les dataframes de métriques
list_métriques <- list()

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
  
  # Calculer les métriques de réseau
  métriques <- as.data.frame(networklevel(interaction_matrix_All_species))
  # Stocker les métriques dans la liste
  list_métriques[[parc]] <- métriques
  
  # Paramètres graphiques pour réduire la taille du graphique
  op <- par(mar = c(0.1, 2, 0.1, 2) + 0.1, cex = 0.8)
  
  # Générer le graphique plotweb avec des liens plus visibles (en utilisant une couleur différente pour chaque parc)
  plotweb(interaction_matrix_All_species, method = "normal", 
          low.lablength = 30, arrow = "down", high.lablength = 30,
          text.rot = 90, col.low = "grey", ybig = 1, 
          y.width.high = 0.03, y.width.low = 0.03, 
          bor.col.interaction = "black",  # Bordure autour des interactions
          col.interaction = colors[i])  # Couleur des liens pour chaque parc spécifique
  
  # Ajouter un titre spécifique pour chaque parc
  title(main = paste("Réseau trophique pour Parc", parc))
  
  # Réinitialiser les paramètres graphiques
  par(op)
}

# Combiner tous les dataframes de métriques en un seul dataframe si souhaité
final_métriques <- do.call(cbind, list_métriques)

# Renommer les colonnes avec le nom de la parcelle suivi d'un underscore
colnames(final_métriques) <- paste0(parc_values, "_", colnames(final_métriques))
final_métriques$métriques <- rownames(final_métriques)

writexl::write_xlsx(final_métriques, "final_métriques_par_parcelles.xlsx")


###################################
# Antoptic 20-09-2024
###################################

# Remarque 1 - Recalculer les indicateurs per capita, Shannon raréfié en fonction du nb de prédateurs capturés (voir discussion 09-2024)
library(iNEXT)

# Créer un tableau de contingence des prédicteurs (Isp) par site (Parc)
table_data <- table(tab_pij2_All_species_Seuil_1_percent$ReadName, tab_pij2_All_species_Seuil_1_percent$Parc)

# Convertir le tableau en data.frame
abundance_data <- as.data.frame.matrix(table_data)

Plot_inext <- iNEXT::iNEXT(abundance_data, q = 1, datatype = "abundance")

ggiNEXT(Plot_inext, type=1, se=FALSE, grey=FALSE)



# Créer un tableau de contingence des prédicteurs (Isp) par site (Parc)
table_data <- table(tab_pij2_All_species_Seuil_1_percent$Isp, tab_pij2_All_species_Seuil_1_percent$Parc)

# Convertir le tableau en data.frame
abundance_data <- as.data.frame.matrix(table_data)

Plot_inext <- iNEXT::iNEXT(abundance_data, q = 1, datatype = "abundance")

ggiNEXT(Plot_inext, type=1, se=FALSE, grey=FALSE)




# Créer un tableau de contingence des prédicteurs (Isp) par site (Parc)
table_data_Isp <- table(tab_pij2_All_species_Seuil_1_percent$Isp, tab_pij2_All_species_Seuil_1_percent$Parc)

# Convertir le tableau en data.frame
abundance_data_Isp <- as.data.frame.matrix(table_data_Isp)

shannon_estimate_Isp <- iNEXT::ChaoShannon(abundance_data_Isp, datatype = "abundance")
shannon_estimate_Isp$Parc <- rownames(shannon_estimate_Isp)
shannon_estimate_Isp <- shannon_estimate_Isp %>%
  rename(shannon_rarefied_Isp = Estimator)


# Créer un tableau de contingence des prédicteurs (Isp) par site (Parc)
table_data_ReadName <- table(tab_pij2_All_species_Seuil_1_percent$ReadName, tab_pij2_All_species_Seuil_1_percent$Parc)

# Convertir le tableau en data.frame
abundance_data_ReadName <- as.data.frame.matrix(table_data_ReadName)

shannon_estimate_ReadName <- iNEXT::ChaoShannon(abundance_data_ReadName, datatype = "abundance")
shannon_estimate_ReadName$Parc <- rownames(shannon_estimate_ReadName)
shannon_estimate_ReadName <- shannon_estimate_ReadName %>%
  rename(shannon_rarefied_ReadName = Estimator)


#=== Rassemblement des données

#Données de paysage

Data_modeles <- pianka_df

# Ajouter la colonne FS
Data_modeles$FS <- ifelse(
  substr(Data_modeles$Parc, nchar(Data_modeles$Parc), nchar(Data_modeles$Parc)) == "B",
  "Bio",
  "Conv"
)

Data_modeles <- Data_modeles %>%
  left_join(shannon_estimate_Isp[,c("Parc", "shannon_rarefied_Isp")], by = "Parc")

Data_modeles <- Data_modeles %>%
  left_join(shannon_estimate_ReadName[,c("Parc", "shannon_rarefied_ReadName")], by = "Parc")

Data_modeles <- Data_modeles %>%
  left_join(pHSN, by = "Parc")


#=== Matrice de corrélation des 4 variables explicatrices du modèle

# Calculer la matrice de corrélation
correlation_matrix <- cor(Data_modeles[,-c(1,3)])

print(correlation_matrix)


# Convertir la matrice de corrélation en data frame
melted_correlation_matrix <- melt(correlation_matrix)

# Créer le heatmap
ggplot(melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#=== Modèles

# a)	Pianka ~ Shannon Isp + Shannon ReadName
#	Si les deux Shannon ne sont pas significatifs, on passe à la b)
# Si un seul Shannon est significatif, on passe à la c)
# Si les deux Shannon sont significatifs, on passe à la d)

model_a <- lm(Pianka_index ~ shannon_rarefied_Isp + shannon_rarefied_ReadName, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_a_summary <- tidy(model_a)
# Afficher les résultats
print(model_a_summary)


# Prey_Shannon_Diversity est significatif donc c)

# c)	Pianka ~ shannon_rarefied_ReadName + FS + pHSN

model_c <- lm(Pianka_index ~ shannon_rarefied_ReadName + FS + HSNtot, data = Data_modeles)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)



#=== Graphiques des relations entre variables

ggplot(data = Data_modeles, aes(x = shannon_rarefied_ReadName, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "shannon_rarefied_ReadName", y = "Pianka_index")

ggplot(data = Data_modeles, aes(x = shannon_rarefied_ReadName, y = Pianka_index, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "shannon_rarefied_ReadName", y = "Pianka_index")

# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles, aes(x = shannon_rarefied_ReadName, y = Pianka_index)) +
  geom_point(aes(color = FS, size = HSNtot)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Shannon rarefié (ReadName)",
       y = "Pianka Index")

# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles, aes(x = shannon_rarefied_ReadName, y = Pianka_index, fill = FS)) +
  geom_point(aes(color = FS, size = HSNtot)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Shannon rarefié (ReadName)",
       y = "Pianka Index")



ggplot(data = Data_modeles, aes(x = shannon_rarefied_Isp, y = Pianka_index)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "shannon_rarefied_Isp", y = "Pianka_index")

ggplot(data = Data_modeles, aes(x = shannon_rarefied_Isp, y = Pianka_index, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "shannon_rarefied_Isp", y = "Pianka_index")


# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles, aes(x = shannon_rarefied_Isp, y = Pianka_index)) +
  geom_point(aes(color = FS, size = HSNtot)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Shannon rarefié (Isp)",
       y = "Pianka Index")


# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles, aes(x = shannon_rarefied_Isp, y = Pianka_index, fill = FS)) +
  geom_point(aes(color = FS, size = HSNtot)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Shannon rarefié (Isp)",
       y = "Pianka Index")



###################################
# Antoptic 20-09-2024
###################################

# Remarque 4 : indice composite (entre 0 et 3) : IFT_Ins_Fon (entre 0 et 1), Int_ti(entre 0 et 1), HSNtot (entre 0 et 1)

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

Data_modeles_2 <- Data_modeles %>%
  left_join(Data_composite_2[,c("Parc", "Composite", "Int_ti", "IFT_Ins_Fon")], by = "Parc")



#=== Matrice de corrélation des 4 variables explicatrices du modèle

# Calculer la matrice de corrélation
correlation_matrix <- cor(Data_modeles_2[,-c(1,3)])

print(correlation_matrix)


# Convertir la matrice de corrélation en data frame
melted_correlation_matrix <- melt(correlation_matrix)

# Créer le heatmap
ggplot(melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles_2, aes(x = Pianka_index, y = shannon_rarefied_Isp, fill = FS)) +
  geom_point(aes(color = FS)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Composite (HSNtot_inv + Int_ti + IFT_Ins_Fon)",
       y = "Pianka Index")

model_c <- lm(Pianka_index ~ shannon_rarefied_ReadName + FS + Composite, data = Data_modeles_2)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)


# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles_2, aes(x = Int_ti, y = Pianka_index, fill = FS)) +
  geom_point(aes(color = FS, size = HSNtot)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Int_ti",
       y = "Pianka Index")

model_c <- lm(Pianka_index ~ Int_ti, data = Data_modeles_2)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)


# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles_2, aes(x = Composite, y = shannon_rarefied_Isp, fill = FS)) +
  geom_point(aes(color = FS)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Composite (HSNtot + Int_ti + IFT_Ins_Fon)",
       y = "Shannon rarefié (Isp)")

model_c <- lm(shannon_rarefied_Isp ~ Composite + FS, data = Data_modeles_2)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)




###################################
# Antoptic 20-09-2024
###################################

# Remarque 3 - mod4:Pianka_index ~ niveau de régulation des oeufs
# Degré de spécialisation (Pianka sur la communauté de proies qu'on sélectionne (les 3 groupes de la remarque 2))


Data_eggs <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/data_eggs.txt", header = TRUE, sep = "", fill = TRUE)
Data_eggs <- subset(Data_eggs, sampling_date == "3" & type == "egg")

Data_eggs <- rename(Data_eggs, Parc = vineyard)

Data_eggs_2 <- Data_eggs %>%
  dplyr::group_by(Parc) %>%
  dplyr::summarize(across(everything(), mean, na.rm = TRUE))

Data_modeles_4 <- Data_modeles_2 %>%
  left_join(Data_eggs_2[,c("Parc", "Nrest", "Npred")], by = "Parc")

Data_modeles_4 <- Data_modeles_4 %>%
  left_join(pianka_df_preys[,c("Parc", "Pianka_index_preys")], by = "Parc")



# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles_4, aes(x = Npred, y = Pianka_index, fill = FS)) +
  geom_point(aes(color = FS)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Npred",
       y = "Pianka Index")

model_c <- lm(Pianka_index ~ Npred + FS, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)




# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles_4, aes(x = Nrest, y = Pianka_index, fill = FS)) +
  geom_point(aes(color = FS)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Nrest",
       y = "Pianka Index")

model_c <- lm(Pianka_index ~ Nrest + FS, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)



#============================


#=== Matrice de corrélation des 4 variables explicatrices du modèle

# Calculer la matrice de corrélation
correlation_matrix <- cor(Data_modeles_4[,-c(1,3,6,8,9)])

print(correlation_matrix)


# Convertir la matrice de corrélation en data frame
melted_correlation_matrix <- melt(correlation_matrix)

# Créer le heatmap
ggplot(melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#=== Modèles

# a)	Pianka ~ Shannon Isp + Shannon ReadName
#	Si les deux Shannon ne sont pas significatifs, on passe à la b)
# Si un seul Shannon est significatif, on passe à la c)
# Si les deux Shannon sont significatifs, on passe à la d)

model_a <- lm(Pianka_index_preys ~ shannon_rarefied_Isp + shannon_rarefied_ReadName, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_a_summary <- tidy(model_a)
# Afficher les résultats
print(model_a_summary)


# Prey_Shannon_Diversity est significatif donc c)

# b)	Pianka ~ FS + pHSN

model_b <- lm(Pianka_index_preys ~ FS + HSNtot, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)


# c)	Pianka ~ FS + pHSN + Shannon (Isp ou ReadName)

model_c <- lm(Pianka_index_preys ~ FS + HSNtot + shannon_rarefied_ReadName, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)


# d)	Pianka ~ FS + pHSN + Shannon Isp + Shannon ReadName

model_d <- lm(Pianka_index_preys ~ FS + HSNtot + shannon_rarefied_Isp + shannon_rarefied_ReadName, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_d_summary <- tidy(model_d)
# Afficher les résultats
print(model_d_summary)



#=== Graphiques des relations entre variables

ggplot(data = Data_modeles_4, aes(x = shannon_rarefied_ReadName, y = Pianka_index_preys)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Prey_Shannon_Diversity", y = "Pianka_index_preys")

model_b <- lm(Pianka_index_preys ~ shannon_rarefied_ReadName, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)


ggplot(data = Data_modeles_4, aes(x = shannon_rarefied_ReadName, y = Pianka_index_preys, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Prey_Shannon_Diversity", y = "Pianka_index_preys")

model_b <- lm(Pianka_index_preys ~ shannon_rarefied_ReadName + FS, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)



ggplot(data = Data_modeles_4, aes(x = shannon_rarefied_Isp, y = Pianka_index_preys)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Predator_Shannon_Diversity", y = "Pianka_index_preys")

model_b <- lm(Pianka_index_preys ~ shannon_rarefied_Isp, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)


ggplot(data = Data_modeles_4, aes(x = shannon_rarefied_Isp, y = Pianka_index_preys, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Predator_Shannon_Diversity", y = "Pianka_index_preys")

model_b <- lm(Pianka_index_preys ~ shannon_rarefied_Isp + FS, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)



ggplot(data = Data_modeles_4, aes(x = Composite, y = Pianka_index_preys)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Composite", y = "Pianka_index_preys")

model_b <- lm(Pianka_index_preys ~ Composite, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)


ggplot(data = Data_modeles_4, aes(x = Composite, y = Pianka_index_preys, fill = FS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Graphique de corrélation avec régression linéaire", x = "Composite", y = "Pianka_index_preys")

model_b <- lm(Pianka_index_preys ~ Composite + FS, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_b_summary <- tidy(model_b)
# Afficher les résultats
print(model_b_summary)



# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles_4, aes(x = Npred, y = Pianka_index_preys, fill = FS)) +
  geom_point(aes(color = FS)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Npred",
       y = "Pianka Index_preys")

model_c <- lm(Pianka_index_preys ~ Npred + FS, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)




# Visualiser les relations avec plusieurs prédicteurs
ggplot(Data_modeles_4, aes(x = Nrest, y = Pianka_index_preys, fill = FS)) +
  geom_point(aes(color = FS)) +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Nrest",
       y = "Pianka Index_preys")

model_c <- lm(Pianka_index_preys ~ Nrest + FS, data = Data_modeles_4)
# Obtenir un résumé du modèle et ajouter l'espèce aux résultats
model_c_summary <- tidy(model_c)
# Afficher les résultats
print(model_c_summary)



model <- lm(Pianka_index_preys ~ shannon_rarefied_ReadName + Composite + Npred + shannon_rarefied_Isp, data = Data_modeles_4)
summary(model)


#--------------- Corrélation Pianka_index / Corrélation Pianka_index_preys ---------------------------------

cor(Data_modeles_4$Pianka_index, Data_modeles_4$Pianka_index_preys, method = "pearson")
cor(Data_modeles_4$Pianka_index, Data_modeles_4$Pianka_index_preys, method = "spearman")

ggplot(Data_modeles_4, aes(x = Pianka_index, y = Pianka_index_preys)) +
  geom_point() +  # Point coloré selon FS et taille selon HSNtot
  geom_smooth(method = "lm", col = "red") +  # Ajoute la droite de régression
  theme_minimal() +
  labs(title = "Effet des prédicteurs sur Pianka Index",
       x = "Pianka_index",
       y = "Pianka Index_preys")


#----- Mail Adrien - 03-10-2024 -----#
# 1a- Comment nos changements environnementaux affectent le degré de chevauchement de niches entre prédateurs d’un même site ?
      
      Data_modeles_5 <- niche_overlap_df %>%
        left_join(Data_composite_2, by = "Parc")
      
      # Modèle de régression linéaire multiple
      modele <- lm(Niche_Overlap ~ Composite, data = Data_modeles_5)
      
      # Résumé du modèle
      summary(modele)
      
      ggplot(Data_modeles_5, aes(x = Composite, y = Niche_Overlap)) +
        geom_point() +
        geom_smooth(method = "lm", col = "blue") +
        geom_text(aes(label = Parc), vjust = -0.5, hjust = 0.5) +
        theme_minimal() +
        labs(title = "Relation entre Composite et chevauchement de niches",
             x = "Composite",
             y = "Chevauchement de niches")


# 1b- Comment ce degré de chevauchement affecte la régulation des insectes phytophages à deux niveaux (l’ensemble des proies, ou certaines espèces qui sont ravageurs) ?
#Insectes phytophages ?
      
    Data_modeles_5b <- niche_overlap_df %>%
    left_join(Data_modeles_4, by = "Parc")

    # Modèle de régression linéaire multiple
    modele <- lm(Niche_Overlap ~ Npred, data = Data_modeles_5b)
    
    # Résumé du modèle
    summary(modele)
    
    ggplot(Data_modeles_5b, aes(x = Npred, y = Niche_Overlap)) +
      geom_point() +
      geom_smooth(method = "lm", col = "blue") +
      geom_text(aes(label = Parc), vjust = -0.5, hjust = 0.5) +
      theme_minimal() +
      labs(title = "Relation entre Npred et chevauchement de niches",
           x = "Npred",
           y = "Chevauchement de niches")
    
    # A POURSUIVRE
      
# 2- On pense globalement que des environnements moins perturbés (AB / bcp d’habitats semi-naturels) favorisent :
     #(i)	la diversité et l’abondance des proies ce qui entraine du partitionnement de niches et donc une diminution du niche overlap moyen de prédateurs (bottom-up hypothesis);
     #(ii)	la diversité et l’abondance des prédateurs ce qui entrainerait de la compétition et de la redondance fonctionnelle (?) et donc augmenter le niche overlap moyen des prédateurs (top-down hypothesis)
     #(iii)	à la fois la diversité des prédateurs et des proies et donc ne change rien au niche overlap (ou les effets + / -  se compensent) (neutral hypothesis)
        # J’aime bien l’idée ! En gros, est ce qu’un « gros réseaux » avec bcp de nœud, c’est juste un « gonflement proportionnel » des liens d’un petit réseau ?
    
  #Ça nous fait un jeu d’hypothèses à tester et on a déjà des éléments de réponses j’ai l’impression…on a juste regardé le niche overlap mais on pourrait rajouter abondance / diversité des prédateurs (peut être proie aussi ?) explicitement...
    # Tu penses que les Shannon raréfiés déjà calculés ce n’est pas suffisants ? Ou tu penses à autre chose ?

# 3- On pense que diminuer le niche overlap devrait favoriser la régulation globale des proies de phytophages alors que l’augmenter devrait diminuer la régulation globale des proies (car plus de compétitions, interférences etc) mais ça se discute car ça dépend de comment est structuré l’overlap dans l’espace et dans le temps (et ça on ne sait pas…) (Je pose délibérément une hypothèse à l’échelle de la communauté de proies) ….
      # Je m’attendrai à ce que le niche overlap augmente la régulation des proies, dasn l’idée que ca intensifie la pression exercée sur les proies et les proba de capture
    #=> ici je ne sais pas bien comment tester cette hypothèse : regarder les abondances poolées de certaines proies ? 
    # On peut calculer un indice d’infestation comme on l’a fait pour le paysage et les pratiques avec les proies : tortricidae, phylloxeridae et cicadellidae (ca fait un peu gros sabot peut-être)
    
# 4- On peut revisiter les hypothèses ci-dessous (2- et 3-) mais ce coup-ci en se focalisant sur les communautés de prédateurs impliquées dans la régulation de quelques proies dont on a les abondances (eg, Lobesia, Scaphoïdeus…) 

# 5- A ce stade on simplifie bcp notre histoire en regardant comment la richesse spécifique des prédateurs ou des proies affectent le niche overlap, mais ça me semble assez plat comme raisonnement.

  #J’ai envie de regarder des histoires de compositions entre niveaux trophiques car on peut supposer que c’est ça qui va plus déterminer le niche overlap (car ça doit embarquer des aspects non trophiques etc…). On pourrait donc tester les hypothèses 2- en regardant comment la composition des prédateurs et/ou des proies affectent l’overlap ; pas hyper clair pour moi comment on fait pr résumer les compositions de communautés avec quelques vecteurs… (NMDS ?) mais on doit pouvoir trouver…
    # On peut peut-être partir sur des indicateurs mesurant les niveaux de dissimilarité fonctionnelle ?



# Calcul Niveau d'infestation tot (valeurs pour Tort, Phyll et Cica entre 0 et 1 respectivement puis somme ente 0 et 3)
    Data_infest <- CR_Paysage
    
    Data_infest <- Data_infest %>%
      group_by(Parc) %>%
      summarise(Fcica = sum(Fcica, na.rm = TRUE),
                Fphyl = sum(Fphyl, na.rm = TRUE),
                Ftord = sum(Ftord, na.rm = TRUE))
    
    
    Data_infest$Fcica_bin <- minMax(Data_infest$Fcica)
    Data_infest$Fphyl_bin <- minMax(Data_infest$Fphyl)
    Data_infest$Ftord_bin <- minMax(Data_infest$Ftord)
    
    Data_infest$Infest_tot <- Data_infest$Fcica_bin + Data_infest$Fphyl_bin + Data_infest$Ftord_bin
    
    
    Data_modeles_6 <- niche_overlap_df %>%
      left_join(Data_infest, by = "Parc")
    
    # Modèle de régression linéaire multiple
    modele <- lm(Niche_Overlap ~ Infest_tot, data = Data_modeles_6)
    summary(modele)

    ggplot(Data_modeles_6, aes(x = Infest_tot, y = Niche_Overlap)) +
      geom_point() +
      geom_smooth(method = "lm", col = "blue") +
      geom_text(aes(label = Parc), vjust = -0.5, hjust = 0.5) +
      theme_minimal() +
      labs(title = "Relation entre Npred et chevauchement de niches",
           x = "Infest_tot",
           y = "Chevauchement de niches")
    
    

    ############################
# Calculs pour papier Phytoma ou autres
    ############################
    
# Décrire les espèces qui consomment les Phylloxeridae, Tortricidae et Cicadellidae
  # Les 5 espèces qui sont le plus impliquées dans le contrôle
    # Voir dans d'autres données si Conv et Bio sont semblables
    
    # Filtrer les lignes où ReadName contient "Cica", "Phyl" ou "Tort"
    tab_pij2_All_species_Seuil_1_percent_pests <- tab_pij2_All_species_Seuil_1_percent %>%
      filter(grepl("Cica|Phyl|Tort", ReadName))

    subset_data_1 <- tab_pij2_All_species_Seuil_1_percent_pests
    
    # Transformer le data frame en matrice d'interaction pour ce parc
    interaction_matrix_All_species_pests <- reshape2::dcast(subset_data_1, ReadName ~ Isp, value.var = "PijComb", fun.aggregate = sum, fill = 0)
    
    # Mettre les noms de lignes et de colonnes
    rownames(interaction_matrix_All_species_pests) <- interaction_matrix_All_species_pests[, 1]
    interaction_matrix_All_species_pests <- as.matrix(interaction_matrix_All_species_pests[, -1])
    
    # Trier les lignes par ordre alphabétique des noms de lignes
    interaction_matrix_All_species_pests <- interaction_matrix_All_species_pests[order(rownames(interaction_matrix_All_species_pests)), ]
    


    # Tracer le réseau avec les couleurs personnalisées
    plotweb(interaction_matrix_All_species_pests, method = "normal", 
            low.lablength = 30, arrow = "down", high.lablength = 30,
            text.rot = 90, ybig = 1, y.width.high = 0.03, y.width.low = 0.03,
            bor.col.interaction = NA)  # Applique la couleur aux labels des proies
    
    # Calculer les métriques de réseau
    métriques_pests_all <- as.data.frame(networklevel(interaction_matrix_All_species_pests))
    
    métriques_pests_all$métriques <- rownames(métriques_pests_all)
    
    
    writexl::write_xlsx(métriques_pests_all, "métriques_all (pests).xlsx")
    
    
    library(dplyr)
    library(tidyr)

    # Isoler les 5 prédateurs qui consomment le plus chaque proie
    top_consumers <- tab_pij2_All_species_Seuil_1_percent_pests %>%
      group_by(ReadName) %>%
      slice_max(order_by = PijComb, n = 5) %>%
      ungroup()
    
    INS_Cicadellidae_Empoasca <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    # Afficher les résultats
    print(top_consumers)
  
    writexl::write_xlsx(top_consumers, "top_consumers (pests).xlsx")
    writexl::write_xlsx(tab_pij2_All_species_Seuil_1_percent_pests, "tab_pij2_All_species_Seuil_1_percent_pests.xlsx")
    writexl::write_xlsx(INS_Cicadellidae_Empoasca, "INS_Cicadellidae_Empoasca.xlsx")
    writexl::write_xlsx(INS_Cicadellidae_Scaphoideus, "INS_Cicadellidae_Scaphoideus.xlsx")
    writexl::write_xlsx(INS_Phylloxeridae_Daktulosphaira, "INS_Phylloxeridae_Daktulosphaira.xlsx")
    writexl::write_xlsx(INS_Tortricidae_Lobesia, "INS_Tortricidae_Lobesia.xlsx")
    
    
    
    
    ############################
    # Calculs par parcelles
    ############################
    
    
    # Réseaux trophiques par parcelle
    # Extraire les valeurs uniques de la colonne 'Parc'
    parc_values <- unique(tab_pij2_All_species_Seuil_1_percent_pests$Parc)
    
    # Générer une palette de couleurs, par exemple avec RColorBrewer (ou d'autres)
    colors <- grDevices::rainbow(length(parc_values))  # Utiliser un ensemble de couleurs distinctes
    
    # Initialiser une liste pour stocker les dataframes de métriques
    list_métriques_pests <- list()
    
    # Boucle pour créer un tableau, une matrice et un graphique pour chaque Parc
    for (i in seq_along(parc_values)) {
      
      # Filtrer les données pour un parc spécifique
      parc <- parc_values[i]
      subset_data_pests <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == parc, ]
      
      # Transformer le data frame en matrice d'interaction pour ce parc
      interaction_matrix_All_species_pests <- reshape2::dcast(subset_data_pests, ReadName ~ Isp, value.var = "PijComb", fun.aggregate = sum, fill = 0)
      
      # Mettre les noms de lignes et de colonnes
      rownames(interaction_matrix_All_species_pests) <- interaction_matrix_All_species_pests[, 1]
      interaction_matrix_All_species_pests <- as.matrix(interaction_matrix_All_species_pests[, -1])
      
      # Trier les lignes par ordre alphabétique des noms de lignes
      interaction_matrix_All_species_pests <- interaction_matrix_All_species_pests[order(rownames(interaction_matrix_All_species_pests)), ]
      
      # Calculer les métriques de réseau
      métriques_pests <- as.data.frame(networklevel(interaction_matrix_All_species_pests))
      # Stocker les métriques dans la liste
      list_métriques_pests[[parc]] <- métriques_pests
      
      # Paramètres graphiques pour réduire la taille du graphique
      op <- par(mar = c(0.1, 2, 0.1, 2) + 0.1, cex = 0.8)
      
      # Générer le graphique plotweb avec des liens plus visibles (en utilisant une couleur différente pour chaque parc)
      plotweb(interaction_matrix_All_species_pests, method = "normal", 
              low.lablength = 30, arrow = "down", high.lablength = 30,
              text.rot = 90, col.low = "grey", ybig = 1, 
              y.width.high = 0.03, y.width.low = 0.03, 
              bor.col.interaction = "black",  # Bordure autour des interactions
              col.interaction = colors[i])  # Couleur des liens pour chaque parc spécifique
      
      # Ajouter un titre spécifique pour chaque parc
      title(main = paste("Réseau trophique pour Parc (pests)", parc))
      
      # Réinitialiser les paramètres graphiques
      par(op)
    }
  
    
    
    # Combiner tous les dataframes de métriques en un seul dataframe si souhaité
    final_métriques_pests <- do.call(cbind, list_métriques_pests)
    
    # Renommer les colonnes avec le nom de la parcelle suivi d'un underscore
    colnames(final_métriques_pests) <- paste0(parc_values, "_", colnames(final_métriques_pests))
    final_métriques_pests$métriques <- rownames(final_métriques_pests)
    
    writexl::write_xlsx(final_métriques_pests, "final_métriques_par_parcelles (pests Parc).xlsx")
    
    

    
    library(dplyr)
    library(tidyr)
    
    interaction_long_pests <- tab_pij2_All_species_Seuil_1_percent %>%
      filter(grepl("Cica|Phyl|Tort", ReadName)) %>%  # Filtre les proies d'intérêt
      select(ReadName, Isp, PijComb, Parc) %>%       # Sélectionne les colonnes nécessaires, y compris Parc
      filter(PijComb > 0)   
    
    top_consumers_by_parc_pests <- interaction_long_pests %>%
      group_by(Parc, ReadName) %>%
      slice_max(order_by = PijComb, n = 5, with_ties = FALSE) %>%
      ungroup()
    
    # Afficher les résultats pour vérification
    print(top_consumers_by_parc_pests)

    writexl::write_xlsx(top_consumers_by_parc_pests, "top_consumers_by_parc_pests (pests).xlsx")
    
    
    library(reshape2)
    
    # Créer une matrice d'occurrence binaire avec `dcast`
    occurrence_matrix <- dcast(top_consumers_by_parc_pests, Parc ~ Isp, fun.aggregate = length)
    
    # Transformer en binaire (remplace toutes valeurs >1 par 1)
    occurrence_matrix[, -1] <- ifelse(occurrence_matrix[, -1] > 0, 1, 0)
    
    # Afficher la matrice
    print(occurrence_matrix)
    
    
    # Supprimer la colonne Isp pour obtenir seulement la matrice binaire
    rownames(occurrence_matrix) <- occurrence_matrix$Parc
    binary_matrix <- as.data.frame(occurrence_matrix[, -1])

    library(vegan)
    
    # Calculer l'indice de Sørensen entre parcelles
    sorensen_similarity <- vegdist(binary_matrix, method = "bray", binary = TRUE)
    
    # Convertir en matrice pour faciliter la visualisation
    sorensen_matrix <- as.matrix(1 - sorensen_similarity) # 1 - distance de Bray-Curtis pour obtenir la similarité
    
    colnames(sorensen_matrix) <- rownames(sorensen_matrix)
    
    # Afficher la matrice de similarité de Sørensen
    print(sorensen_matrix)
    
    library(pheatmap)
    # Créer la heatmap de similarité
    pheatmap(sorensen_matrix,
             cluster_rows = TRUE,         # Clusteriser les lignes pour regrouper les parcelles similaires
             cluster_cols = TRUE,         # Clusteriser les colonnes pour la même raison
             display_numbers = TRUE,      # Afficher les valeurs de similarité dans les cellules
             color = colorRampPalette(c("white", "lightblue", "salmon", "red"))(100),  # Palette de couleurs pour les valeurs de similarité
             main = "Similarité de Sørensen entre les parcelles",
             fontsize_number = 8)         # Ajuster la taille des valeurs affichées
    
    
    
    # Charger le package
    library(vegan)
    
    # Supposons que 'clusters' soit le vecteur des clusters pour chaque parcelle
    clusters <- cutree(hclust(dist(sorensen_matrix)), k = 2)  # Ajustez k selon vos besoins
    cluster_df <- data.frame(Parcelle = rownames(sorensen_matrix), cluster = as.factor(clusters))
    
    # Créer une matrice de dissimilarité
    sorensen_dist <- as.dist(1 - sorensen_matrix)  # Convertir la similarité en dissimilarité
    
    # Déterminer le nombre d'échantillons dans chaque cluster
    n1 <- sum(clusters == 1)  # Nombre d'échantillons dans Cluster1
    n2 <- sum(clusters == 2)  # Nombre d'échantillons dans Cluster2
    
    # Créer un facteur pour les groupes
    group <- factor(clusters)  # Créez un facteur basé sur le vecteur 'clusters'
    
    # Effectuer le test de permutation
    adonis_result <- adonis2(sorensen_dist ~ group, permutations = 999)
    print(adonis_result)
    

    
    # Transformer la matrice de similarité en format long
    sorensen_long <- melt(sorensen_matrix)
    colnames(sorensen_long) <- c("Parcelle1", "Parcelle2", "Freq")
    sorensen_long <- merge(sorensen_long, cluster_df, by.x = "Parcelle1", by.y = "Parcelle")
    
    
    library(ggplot2)
    # Boxplot des similarités de Sørensen par cluster
    ggplot(sorensen_long, aes(x = factor(cluster), y = Freq)) +
      geom_boxplot(fill = c("lightblue", "salmon")) +
      labs(x = "Cluster", y = "Similarité de Sørensen") +
      theme_minimal() +
      theme(legend.position = "none")
    
    
    ################################
    # Top consumers par Parc
    ################################
    
    tab_pij2_pests_1088B <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1088B",]
    
    INS_Cicadellidae_Empoasca_1088B <- tab_pij2_pests_1088B[tab_pij2_pests_1088B$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1088B <- tab_pij2_pests_1088B[tab_pij2_pests_1088B$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1088B <- tab_pij2_pests_1088B[tab_pij2_pests_1088B$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1088B <- tab_pij2_pests_1088B[tab_pij2_pests_1088B$ReadName == "INS_Tortricidae_Lobesia",]
    

    tab_pij2_pests_1088C <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1088C",]
    
    INS_Cicadellidae_Empoasca_1088C <- tab_pij2_pests_1088C[tab_pij2_pests_1088C$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1088C <- tab_pij2_pests_1088C[tab_pij2_pests_1088C$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1088C <- tab_pij2_pests_1088C[tab_pij2_pests_1088C$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1088C <- tab_pij2_pests_1088C[tab_pij2_pests_1088C$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    tab_pij2_pests_1435B <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1435B",]
    
    INS_Cicadellidae_Empoasca_1435B <- tab_pij2_pests_1435B[tab_pij2_pests_1435B$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1435B <- tab_pij2_pests_1435B[tab_pij2_pests_1435B$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1435B <- tab_pij2_pests_1435B[tab_pij2_pests_1435B$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1435B <- tab_pij2_pests_1435B[tab_pij2_pests_1435B$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    tab_pij2_pests_1435C <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1435C",]
    
    INS_Cicadellidae_Empoasca_1435C <- tab_pij2_pests_1435C[tab_pij2_pests_1435C$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1435C <- tab_pij2_pests_1435C[tab_pij2_pests_1435C$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1435C <- tab_pij2_pests_1435C[tab_pij2_pests_1435C$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1435C <- tab_pij2_pests_1435C[tab_pij2_pests_1435C$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    
    tab_pij2_pests_1650B <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1650B",]
    
    INS_Cicadellidae_Empoasca_1650B <- tab_pij2_pests_1650B[tab_pij2_pests_1650B$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1650B <- tab_pij2_pests_1650B[tab_pij2_pests_1650B$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1650B <- tab_pij2_pests_1650B[tab_pij2_pests_1650B$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1650B <- tab_pij2_pests_1650B[tab_pij2_pests_1650B$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    tab_pij2_pests_1650C <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1650C",]
    
    INS_Cicadellidae_Empoasca_1650C <- tab_pij2_pests_1650C[tab_pij2_pests_1650C$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1650C <- tab_pij2_pests_1650C[tab_pij2_pests_1650C$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1650C <- tab_pij2_pests_1650C[tab_pij2_pests_1650C$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1650C <- tab_pij2_pests_1650C[tab_pij2_pests_1650C$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    tab_pij2_pests_1662B <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1662B",]
    
    INS_Cicadellidae_Empoasca_1662B <- tab_pij2_pests_1662B[tab_pij2_pests_1662B$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1662B <- tab_pij2_pests_1662B[tab_pij2_pests_1662B$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1662B <- tab_pij2_pests_1662B[tab_pij2_pests_1662B$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1662B <- tab_pij2_pests_1662B[tab_pij2_pests_1662B$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    tab_pij2_pests_1662C <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1662C",]
    
    INS_Cicadellidae_Empoasca_1662C <- tab_pij2_pests_1662C[tab_pij2_pests_1662C$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1662C <- tab_pij2_pests_1662C[tab_pij2_pests_1662C$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1662C <- tab_pij2_pests_1662C[tab_pij2_pests_1662C$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1662C <- tab_pij2_pests_1662C[tab_pij2_pests_1662C$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    tab_pij2_pests_1868B <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1868B",]
    
    INS_Cicadellidae_Empoasca_1868B <- tab_pij2_pests_1868B[tab_pij2_pests_1868B$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1868B <- tab_pij2_pests_1868B[tab_pij2_pests_1868B$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1868B <- tab_pij2_pests_1868B[tab_pij2_pests_1868B$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1868B <- tab_pij2_pests_1868B[tab_pij2_pests_1868B$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    tab_pij2_pests_1868C <- tab_pij2_All_species_Seuil_1_percent_pests[tab_pij2_All_species_Seuil_1_percent_pests$Parc == "1868C",]
    
    INS_Cicadellidae_Empoasca_1868C <- tab_pij2_pests_1868C[tab_pij2_pests_1868C$ReadName == "INS_Cicadellidae_Empoasca",]
    INS_Cicadellidae_Scaphoideus_1868C <- tab_pij2_pests_1868C[tab_pij2_pests_1868C$ReadName == "INS_Cicadellidae_Scaphoideus",]
    INS_Phylloxeridae_Daktulosphaira_1868C <- tab_pij2_pests_1868C[tab_pij2_pests_1868C$ReadName == "INS_Phylloxeridae_Daktulosphaira",]
    INS_Tortricidae_Lobesia_1868C <- tab_pij2_pests_1868C[tab_pij2_pests_1868C$ReadName == "INS_Tortricidae_Lobesia",]
    
    
    
    
  ###############
    # Tableau pour modèles antoptic Bordeaux
  ###############
    
    ###############
    # Tableau modèles
    ###############
    
    Tab_models <- pianka_df
    Tab_models$cult <- substr(Tab_models$Parc, nchar(Tab_models$Parc), nchar(Tab_models$Parc))
    
        #''''''''''''''''''''''''
        #'  Div Pred (independant data)
        #''''''''''''''''''''''''
        #''''''''''''''''''''''''
    
    Tab_natural_enemies <- read.table("C:/Users/Alexandre_Dosset/Desktop/Antoptic/SOLUTION_data_naturalenemy_landscape_2015.txt", header = TRUE, sep = "\t")

    Tab_natural_enemies$Genus_species <- paste(Tab_natural_enemies$genus, Tab_natural_enemies$species, sep = "_")
    
    Tab_natural_enemies <- rename(Tab_natural_enemies, Parc = vineyard)
    
    #Tab_natural_enemies <- Tab_natural_enemies %>%
    #  filter(sampling_date %in% c(2, 3))
    
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
    
    # Renommer la colonne
    colnames(tab_pred_shannon_div)[2] <- "Pred_Shannon_diversity"
    
    
    tab_pred_shannon_div <- tab_pred_shannon_div %>%
      filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))

    
        #''''''''''''''''''''''''
        #'  CR_Paysage pour abondances ravageurs 
        #''''''''''''''''''''''''
        #''''''''''''''''''''''''
    
    CR_Paysage_Tab_modèles <- CR_Paysage[,c("Parc", "cult", "camp", "NBcica", "NBtord", "NBphyl")]
    
    CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
      filter(camp %in% c(2, 3))
    
    CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
      filter(Parc %in% c("1088B", "1088C", "1435B", "1435C", "1650B", "1650C", "1662B", "1662C", "1868B", "1868C"))
    
    # Conserver la tableau pour calculer le delta camp 2 et 3
    Tab_delta_2_3 <- CR_Paysage_Tab_modèles
    
    CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
      select(-cult, -camp)
    
    CR_Paysage_Tab_modèles <- CR_Paysage_Tab_modèles %>%
      group_by(Parc) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE))
    
    
        #''''''''''''''''''''''''
        #'  CR_Paysage pour delta camp 2 et 3 pour Cica et Phyl 
        #''''''''''''''''''''''''
        #''''''''''''''''''''''''
    
    Tab_delta_2_3 <- Tab_delta_2_3[,c("Parc", "camp", "NBcica", "NBphyl")] %>%
      pivot_wider(names_from = camp, values_from = c(NBcica, NBphyl), names_prefix = "camp") %>%
      mutate(delta_NBcica = NBcica_camp3 - NBcica_camp2,
             delta_NBphyl = NBphyl_camp3 - NBphyl_camp2
      ) %>%
      select(Parc, starts_with("delta_"))
   
    
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
    
    Data_composite_2_Tab_modèles <- Data_composite_2[,c("Parc", "IFTHer", "IFTIns", "IFTFon", "IFT_Ins_Fon")]
    
        #''''''''''''''''''''''''
        #'  Data_eggs_2 pour Npred
        #''''''''''''''''''''''''
        #''''''''''''''''''''''''
        
    Data_eggs_2_Tab_modèles <- Data_eggs_2[,c("Parc", "Npred")]
    

    
    ###############
    # Ajouter au tableau modèles
    ###############
    
    Tab_models <- Tab_models %>%
      left_join(tab_pred_shannon_div, by = "Parc")
    
    Tab_models <- Tab_models %>%
      left_join(prey_shannon_diversity_All_species, by = "Parc")
    
    Tab_models <- Tab_models %>%
      left_join(CR_Paysage_Tab_modèles, by = "Parc")
    
    Tab_models <- Tab_models %>%
      left_join(Data_composite_2_Tab_modèles, by = "Parc")
    
    Tab_models <- Tab_models %>%
      left_join(Data_eggs_2_Tab_modèles, by = "Parc")
    
    Tab_models <- Tab_models %>%
      left_join(Tab_delta_2_3, by = "Parc")
    
    
    write.table(Tab_models, file = "Tab_models.txt", sep = "\t",
                row.names = FALSE)
    
    ###############
    # Métriques de réseau All_species (Nestedness, Connectance et vulnerability)
    ###############
    
    metriques_reseau_All_species <- table_metriques_Dormann_2009[rownames(table_metriques_Dormann_2009) %in% c("vulnerability.LL", "connectance", "nestedness"), ]
    
    modularity_Tab_modèles <- metriques_reseau_All_species
    
    
    write.table(modularity_Tab_modèles, file = "Network_metrics.txt", sep = "\t",
                row.names = FALSE)
    
    # Specialization ?
    
    
    
    #''''''''''''''''''''''''
    #'  Calcul de l'indice de redondance fonctionnelle de Benjamin Feit
    #''''''''''''''''''''''''
    #''''''''''''''''''''''''
    
    # ln io (Insecta) 21.97205 => i0 Insecta = 3486101848
    # ln io (Arachnida) 24.581475 => i0 Arachnida = 47380424732
    
    # Il faut intégrer le fait que i0 varie selon les taxons
    
    
    #'  Création des fonctions
    
    Metabolic_rate <- function(Mi, I0 = 1) {
      return(I0 * Mi^(3/4))
    }
    # Avec I0 = taxon-specific normalisation constant (Ehnes et al. 2011)
    # et M the average dry body mass of predator i.
    # Predator abundance was calculated from activity density in wet pitfall traps
    
    Risk_of_predation <- function(pij, qi, Ii) {
      return(pij * qi * Ii)
    }
    # Avec prey group j,  predator i.
    # pij is the probability of predator i feeding on prey j
    # qi is the activity density of predators belonging to species i.
    
    
    Feit_functional_redundancy_index <- function(Rij_vector) {
      p_prime <- Rij_vector / sum(Rij_vector, na.rm = TRUE)
      
      # Calcul de l'entropie de Shannon
      H <- -sum(p_prime * log(p_prime), na.rm = TRUE)
      
      # Redondance fonctionnelle
      eH <- exp(H)
      
      return(eH)
    }
    
    #####################
    #'  Calcul de l'indice de redondance fonctionnelle de Benjamin Feit
    
    # Bootstrap pour l’indice de redondance fonctionnelle
    bootstrap_redundancy_index <- function(data, n_bootstrap = 1000, sample_size = 20) {
      eH_values <- numeric(n_bootstrap)
      
      for (b in 1:n_bootstrap) {
        # Rééchantillonnage bootstrap des prédateurs
        bootstrap_data <- data %>%
          group_by(Isp) %>%
          sample_n(size = min(sample_size, n()), replace = TRUE) %>%
          ungroup()
        
        # Calcul du taux métabolique
        bootstrap_data <- bootstrap_data %>%
          mutate(Ii = Metabolic_rate(Mi))
        
        # Calcul du risque de prédation Rij
        bootstrap_data <- bootstrap_data %>%
          mutate(Rij = Risk_of_predation(pij = PijComb, qi = qi, Ii = Ii))
        
        # Calcul de eH pour chaque proie
        eH_per_prey <- bootstrap_data %>%
          group_by(ReadName) %>%
          summarise(eH = Feit_functional_redundancy_index(Rij), .groups = "drop")
        
        # Moyenne des indices de redondance fonctionnelle sur toutes les proies
        eH_values[b] <- mean(eH_per_prey$eH, na.rm = TRUE)
      }
      
      # Retourner la moyenne des valeurs de redondance obtenues via bootstrap
      return(mean(eH_values))
    }
    
    
    redundance_results <- list()
    
    for (nom_tableau in names(tableaux_interaction)) {
      tab_pij2_filtered <- tableaux_interaction[[nom_tableau]]
      
      mean_eH <- bootstrap_redundancy_index(tab_pij2_filtered, n_bootstrap = 1000, sample_size = 20)
      
      redundance_results[[nom_tableau]] <- mean_eH
    }
    
    # Résultat final en data frame
    redundance_df <- data.frame(Parc = names(redundance_results),
                                Feit_redundance_index = unlist(redundance_results))
    
    print(redundance_df)
    
    