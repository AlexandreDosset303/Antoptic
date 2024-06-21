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
            TAB_nombre_valeurs_uniques <- length(unique(tab_pij2_All_species$ind))
            
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
      # ===== V-1 - All_Species ---------------------------------------------------------------------------------        
      # Calculer l'abondance des prédateurs
      Abundance_pred <- tab_pij2_All_species_Seuil_1_percent %>%
        group_by(Parc, Isp) %>%
        summarise(abundance = n(), .groups = 'drop')
      
      Richesse_proie <- tab_pij2_All_species_Seuil_1_percent %>%
        group_by(Parc, Isp) %>%
        summarise(SR_prey = n_distinct(ReadName), .groups = 'drop')
      
      Occurrence_prey <- tab_pij2_All_species_Seuil_1_percent %>%
        group_by(Parc, Isp, ReadName) %>%
        summarise(occurrences = n(), .groups = 'drop')
      
      Diversity_prey_occurrence <- Occurrence_prey %>%
        group_by(Isp, Parc) %>%
        summarise(H = vegan::diversity(occurrences, index = "shannon"),
                  S = n_distinct(ReadName),
                  H_max = log(n_distinct(ReadName)),
                  J = H / H_max, .groups = 'drop')
      
      #Probabilités d'interaction
      # Calculer l'équitabilité des proies
      Diversity_prey <- tab_pij2_All_species_Seuil_1_percent %>%
        group_by(Parc, Isp, ReadName) %>%
        summarise(Interaction_probability = sum(PijComb), .groups = 'drop')
      
      Diversity_prey_summary <- Diversity_prey %>%
        group_by(Parc, Isp) %>%
        summarise(H = diversity(Interaction_probability, index = "shannon"),
                  S = n_distinct(ReadName),
                  H_max = log(n_distinct(ReadName)),
                  J = H / H_max, .groups = 'drop')
      
      # Fusionner les deux jeux de données
      result <- merge(Abundance_pred, Diversity_prey_occurrence, by = c("Parc", "Isp"))
      result$FS <- substr(result$Parc, nchar(result$Parc), nchar(result$Parc))
      
      # Standardiser les variables
      result_scaled <- result %>%
        mutate(predator_abundance_scaled = scale(abundance),
               prey_equitability_scaled = scale(J))
      
      result_scaled1 <- result_scaled %>% filter(Isp %in% unique(Isp)[1:8])
      result_scaled2 <- result_scaled %>% filter(Isp %in% unique(Isp)[9:16])
      result_scaled3 <- result_scaled %>% filter(Isp %in% unique(Isp)[17:24])
      result_scaled4 <- result_scaled %>% filter(Isp %in% unique(Isp)[25:32])
      result_scaled5 <- result_scaled %>% filter(Isp %in% unique(Isp)[33:40])
      result_scaled6 <- result_scaled %>% filter(Isp %in% unique(Isp)[41:48])
      result_scaled7 <- result_scaled %>% filter(Isp %in% unique(Isp)[49:52])
      
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
        model <- lm(prey_equitability_scaled ~ predator_abundance_scaled, data = result_scaled)
        
        # Obtenir un résumé du modèle et ajouter l'espèce aux résultats
        model_summary <- tidy(model)
        
        # Afficher les résultats
        print(model_summary)

   # La relation entre l'abondance des prédateurs et l'équitabilité des proies n'est pas significative.
        
      
# VI - Liens entre abondance prédateurs et diversité de Shannon des proies ============================================
      # ===== VI-1 - All_Species --------------------------------------------------------------------------------        
      ###
      #Abondance des prédateurs et diversité de Shannon des proies
      ###
      
      # Fusionner les deux jeux de données
      result <- merge(Abundance_pred, Diversity_prey_occurrence, by = c("Parc", "Isp"))
      result$FS <- substr(result$Parc, nchar(result$Parc), nchar(result$Parc))
      
      # Standardiser les variables
      result_scaled_diversity <- result %>%
        mutate(predator_abundance_scaled = scale(abundance),
               prey_diversity_scaled = scale(H))
      unique(result_scaled_diversity$Isp)
      result_scaled1 <- result_scaled_diversity %>% filter(Isp %in% unique(Isp)[1:8])
      result_scaled2 <- result_scaled_diversity %>% filter(Isp %in% unique(Isp)[9:16])
      result_scaled3 <- result_scaled_diversity %>% filter(Isp %in% unique(Isp)[17:24])
      result_scaled4 <- result_scaled_diversity %>% filter(Isp %in% unique(Isp)[25:32])
      result_scaled5 <- result_scaled_diversity %>% filter(Isp %in% unique(Isp)[33:40])
      result_scaled6 <- result_scaled_diversity %>% filter(Isp %in% unique(Isp)[41:48])
      result_scaled7 <- result_scaled_diversity %>% filter(Isp %in% unique(Isp)[49:52])
      
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
      model <- lm(prey_diversity_scaled ~ predator_abundance_scaled, data = result_scaled_diversity)
      
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
      ggplot(tab_pij2_All_species_Seuil_1_percent, aes(x=Parc, y=PijComb)) +
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

      
      tab_pij2_All_species_Seuil_1_percent$FS <- substr(tab_pij2_All_species_Seuil_1_percent$Parc, nchar(tab_pij2_All_species_Seuil_1_percent$Parc), nchar(tab_pij2_All_species_Seuil_1_percent$Parc))
      
      modele <- glmer(PijComb ~ FS + (1|Isp) + (1|ReadName), data = tab_pij2_All_species_Seuil_1_percent, family = binomial)
      summary(modele)
      
      # Visualiser les différences de probabilités d'interaction entre les types de parcelles
      ggplot(tab_pij2_All_species_Seuil_1_percent, aes(x = FS, y = PijComb, fill = FS)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Interaction probability between predators and preys amongst farming system",
             x = "Farming system",
             y = "Interaction probability")

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
      
      # Create the boxplot and facet by species (Isp)
      ggplot(tab_pij2_Species_in_all_Parc_Seuil_1_percent, aes(x = FS, y = PijComb, fill = FS)) +
        geom_boxplot() +
        facet_wrap(~ Isp) +
        theme_minimal() +
        labs(title = "(Species in all parc) Interaction probability between predators and preys amongst farming systems",
             x = "Farming system",
             y = "Interaction probability")


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
      ggplot(result, aes(x = Pred_Nb, y = Mean_PijComb, color = ReadName)) +
        geom_point() +
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité d'interaction entre prédateurs et proies selon la richesse spécifique de prédateurs",
             x = "Richesse spécifique de prédateurs",
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
      ggplot(result, aes(x = Pred_Nb, y = Mean_PijComb, color = ReadName)) +
        geom_point() +
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité moyenne d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité moyenne d'interaction") +
        scale_color_discrete() +
        facet_wrap(~Parc) +
        scale_fill_discrete(name = "Prey species")
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Median_PijComb, color = ReadName)) +
        geom_point() +
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité médiane d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité médiane d'interaction") +
        scale_color_discrete() +
        facet_wrap(~Parc) +
        scale_fill_discrete(name = "Prey species")
      
      
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Mean_PijComb, color = Parc)) +
        geom_point() +
        theme_minimal() +
        labs(title = "(Espèces dans tous les parcs) Probabilité moyenne d'interaction entre prédateurs et proies selon l'abondance de prédateurs",
             x = "Abondance de prédateurs",
             y = "Probabilité moyenne d'interaction") +
        scale_color_discrete() +
        facet_wrap(~ReadName) +
        scale_fill_discrete(name = "Prey species")
      
      # Créez le graphique
      ggplot(result, aes(x = Pred_Nb, y = Median_PijComb, color = Parc)) +
        geom_point() +
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
  
