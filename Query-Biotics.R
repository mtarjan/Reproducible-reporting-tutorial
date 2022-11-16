## Query data from central Biotics
## M Tarjan
## Nov 16, 2022

## Load packages
library(tidyverse)
library(RODBC)

## Connect to central biotics to pull out most recent data on sss
con<-odbcConnect("centralbiotics", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))

## Select primary global subset (used for data content standards)
qry <- "SELECT DISTINCT egt.element_global_id, gname.scientific_name, egt.g_primary_common_name, nc.name_category_desc, egt.rounded_g_rank
FROM  element_global egt
LEFT JOIN scientific_name gname
  ON egt.gname_id = gname.scientific_name_id
LEFT JOIN d_name_category nc
  ON gname.d_name_category_id = nc.d_name_category_id
WHERE
/* criteria that applies to all records - active, regular and confident in US or Canada */ 
  egt.inactive_ind = 'N' 
  and egt.element_global_id in ( 
    (SELECT ent.element_global_id 
      FROM element_national ent 
      where ent.nation_id in (38,225) 
       and ent.element_national_id in  
       (select tnd.element_national_id from taxon_natl_dist tnd 
        where tnd.d_regularity_id = 1 /* Regularly occurring */ and tnd.d_dist_confidence_id = 1 /* confident */))) 
  and  
  ( 
 -- animal criteria - full species, standard classification, standard taxonomic groups with complete distribution, exclude pops and hybrids 
  egt.element_global_id in  
   (select egta.element_global_id 
     from element_global egta, scientific_name sna,taxon_global tga 
     where egta.gname_id = sna.scientific_name_id 
     and egta.element_global_id = tga.element_global_id 
     and sna.d_classification_level_id = 7  /* full animal species only */ 
     and egta.d_classification_status_id = 1 /*standard */ 
     and sna.scientific_name not like '% pop. %' 
     and tga.g_hybrid_ind = 'N' 
     and standard_taxonomic_groups(egta.element_global_id) is not null 
     and standard_taxonomic_groups(egta.element_global_id) not in ('Notodontid Moths (G1G3)','Giant Silkworm and Royal Moths (G1G3)','Tiger Moths (G1G3)') )  
 -- plant criteria - vascular plants, standard classification, exclude pops and hybrids 
  or  
  egt.element_global_id in  
   (select egtp.element_global_id 
     from element_global egtp, scientific_name snp, taxon_global tgp 
     where egtp.gname_id = snp.scientific_name_id  
     and egtp.element_global_id = tgp.element_global_id 
     and snp.d_name_category_id = 4 /*Vascular Plant */ 
     and egtp.d_classification_status_id = 1 /*standard */ 
     and snp.scientific_name not like '% pop. %' 
     and tgp.g_hybrid_ind = 'N') 
 -- USESA criteria - include all except Delisted 
  or  
  egt.element_global_id in 
    (select esa_tg.element_global_id 
     from taxon_global esa_tg 
     where esa_tg.d_usesa_id is not null 
     and esa_tg.d_usesa_id != 39)  /** exclude Delisted only  **/ 
  or 
 -- COSEWIC status (actual, not interpreted, specific values) 
  egt.element_global_id in 
     (select cosewic_ent.element_global_id 
     from element_national cosewic_ent, 
     taxon_national cosewic_tn 
     where cosewic_ent.element_national_id = cosewic_tn.element_national_id 
     and cosewic_ent.nation_id = 38 
     and cosewic_tn.d_cosewic_id in (1, 2, 3, 4, 5)) 
  or 
 -- SARA status 
  egt.element_global_id in 
     (select sara_ent.element_global_id 
     from element_national sara_ent, 
      el_natl_agency_status sara_nas 
     where sara_ent.element_national_id = sara_nas.element_national_id 
     and sara_ent.nation_id = 38 
     and sara_nas.agency_name like 'SARA%') 
  ) 
"

dat<-sqlQuery(con, qry); head(dat) ##import the queried table

## When finished, close the connection
odbcClose(con)

##Group G and T ranks
dat <- dat %>% dplyr::mutate(G_RANK = dplyr::case_when(
  ROUNDED_G_RANK %in% c("G1", "T1") ~ "G1/T1",
  ROUNDED_G_RANK %in% c("G2", "T2") ~ "G2/T2",
  ROUNDED_G_RANK %in% c("G3", "T3") ~ "G3/T3",
  ROUNDED_G_RANK %in% c("G4", "T4") ~ "G4/T4",
  ROUNDED_G_RANK %in% c("G5", "T5") ~ "G5/T5",
  ROUNDED_G_RANK %in% c("GH", "TH") ~ "GH/TH",
  ROUNDED_G_RANK %in% c("GX", "TX") ~ "GX/TX",
  ROUNDED_G_RANK %in% c("GNA", "TNA") ~ "GNA/TNA",
  ROUNDED_G_RANK %in% c("GNR", "TNR") ~ "GNR/TNR",
  ROUNDED_G_RANK %in% c("GU", "TU") ~ "GU/TU"
))

## Write out data as csv
write.csv(dat, paste0("data/primary-subset-global-", Sys.Date(), ".csv"), row.names = F)
