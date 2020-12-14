library(tidyverse)
load("~/Such_kursach/Egorushka/meta_Dota.rda")
load("~/Such_kursach/Egorushka/edgelist_Dota.rda")
load("~/Such_kursach/Egorushka/meta_K2.rda")
load("~/Such_kursach/Egorushka/edgelist_K2.rda")
edgelist_Dota = edgelist_Dota %>% as.data.frame()
edgelist_K2 = edgelist_K2 %>% as.data.frame()


ego_meta = data.frame(.EGOID = c("Dota2","K2"))

edges_ids = edgelist_Dota %>% 
  dplyr::rename(.SRCID = V1,.TGTID = V2) %>% 
  mutate(.EGOID = "Dota2")


alter_meta = meta_Dota %>%
  dplyr::rename(.ALTERID = appid) %>% 
  mutate(.EGOID = "Dota2",.ALTERID = as.factor(.ALTERID))


e1 = egor(alters = alter_meta,
          egos = ego_meta,
          aaties = edges_ids,
          ID.vars = list(
            ego = ".EGOID",
            alter = ".ALTERID",
            source = ".SRCID",
            target = ".TGTID"))

ei_d1 = EI(e1, "cluster") %>%
  .[1,] %>%
  gather("cluster","value",1:13) %>% 
  na.omit()

ei_d1 = left_join(ei_d1,dplyr::count(meta_Dota,cluster))




ego_meta = data.frame(.EGOID = c("Dota2","K2"))

edges_ids = edgelist_K2 %>% 
  dplyr::rename(.SRCID = V1,.TGTID = V2) %>% 
  mutate(.EGOID = "K2")

alter_meta = meta_K2 %>%
  dplyr::rename(.ALTERID = appid) %>% 
  mutate(.EGOID = "K2",.ALTERID = as.factor(.ALTERID))


e2 = egor(alters = alter_meta,
          egos = ego_meta,
          aaties = edges_ids,
          ID.vars = list(
            ego = ".EGOID",
            alter = ".ALTERID",
            source = ".SRCID",
            target = ".TGTID"))


ei_k2 = EI(e2, "cluster")  %>%
  .[2,] %>%
  gather("cluster","value",1:20) %>% 
  na.omit()


ei_k2 = left_join(ei_k2,dplyr::count(meta_K2,cluster))


### интерпретация
### https://www.slideshare.net/plotti/social-network-analysis-part-ii слайд 21
### http://revista-redes.rediris.es/recerca/Egoredes/research/E-I%20Index%20April%2026.pdf слайд 13


### Что дальше
### я бы убрал кластеры с маленьким размером, у них ie index сильно искажен

library(igraph)

kek = edge_density(g)

YQ_gamers = YQ_dota

edge_mem = edge.betweenness.community(YQ_gamers)


clust_den = sapply(unique(membership(edge_mem)), function(i) {
  subg1 = induced.subgraph(YQ_gamers, which(membership(edge_mem)==i)) #membership id differs for each cluster
  return(edge_density(subg1))
})


clust_den_df = data.frame(cluster = 1:length(clust_den),cluster_den = clust_den)

kek = ei(YQ_gamers,membership(edge.betweenness.community(YQ_gamers)))
