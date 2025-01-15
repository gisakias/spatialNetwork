'
library(remotes)
library(devtools)
remotes::install_version("rgeos", version = "0.6-4")
remotes::install_version("simplevis", version = "7.1.0")
devtools::install_github("dimitrisk/goal", force=T)
'
#1
library(remotes)
library(devtools)
library(sf)
library(tidygraph)
library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(units)
library(tmap)
library(osmdata)
library(link2GI)
library(nabor)
library(units)
library(sfnetworks)
library(dplyr)
library(goal)

# Δεδομένα δικτύου  
q = c(23.9639, 35.4894, 24.0611, 35.5322) # Bounding Box πόλης Χανίων
net2 = goal::osm.getRoads(q, withBB=TRUE, outcrs=4326)
  
poly = goal::osm.bb_2_pol(q, outcrs =  4326) # bbox σε spatial polygon
net3 = goal::osm.ClipSFnetwork_with_poly(net2, poly) # clip network by spatial polygon

plot(net3,col="grey", main="Clipped sfnetwork of Chania")
plot(poly,add=T)

# Create a random point
#gps = sfheaders::sf_point(data.frame(y = 26.55257, x = 39.10575, ID=-1))  %>% st_set_crs(4326)

# nearest edge (road) to the point. The network must have edges activated.
#near_edge = st_nearest_feature(gps, net3 %>% st_as_sf())

#near_edge
#st_as_sf(net3)[near_edge,]

#p3 = ggplot() +
#geom_sf(data = st_as_sf(net3), color = 'black') +
#geom_sf(data = gps, color = 'red') +
#geom_sf(data = st_as_sf(net3)[near_edge,], color = 'orange')
#p3
#plot(net3)
#net3
  
net3 %>%  sfnetworks::activate("nodes") %>% dplyr::filter(!tidygraph::node_is_isolated())
  
net3 %>% sfnetworks::activate("nodes") %>% dplyr::filter(tidygraph::node_is_isolated()) %>% st_as_sf() %>% nrow()
  
net3 %>% sfnetworks::activate("edges") %>% dplyr::filter(tidygraph::edge_is_loop()) %>% st_as_sf() %>% nrow()
  
net3 %>% sfnetworks::activate("edges") %>% dplyr::filter(tidygraph::edge_is_multiple()) %>% st_as_sf() %>% nrow()

net3b = net3 %>% activate("edges") %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())
  
# Ενσωμάτωση των σημείων  
  
net4 = tidygraph::convert(net3b, to_spatial_subdivision)
net4 = tidygraph::convert(net4, to_spatial_smooth)
net4 = tidygraph::convert(net4, to_spatial_simple)
  
n_nodes = net4 %>% activate(nodes) %>% st_as_sf()%>% nrow()
  
net4 = net4 %>% activate(edges) %>%
  mutate(length = edge_length())%>% st_set_crs(4326)
  
net4 = net4 %>% activate(nodes) %>%
  mutate(ID = 1:n_nodes)%>%
  mutate(ishouse = 1)%>% st_set_crs(4326)
  
gps1 = sfheaders::sf_point(data.frame(y =23.976000, x =35.506798))  %>% st_set_crs(4326)
gps2 = sfheaders::sf_point(data.frame(y =23.995920, x =35.499531 ))  %>% st_set_crs(4326)
gps3 = sfheaders::sf_point(data.frame(y =24.016876, x =35.509592 ))  %>% st_set_crs(4326)
gps4 = sfheaders::sf_point(data.frame(y =24.046765, x =35.511828))  %>% st_set_crs(4326)

mycol = c("blue","green",'red',"purple")
evac_points = rbind(gps1, gps2, gps3,gps4) #%>% rowid_to_column() # Νέα στήλη 'rowid'
  
blended = st_network_blend(net4, evac_points  )
  
blended = blended %>% activate(nodes) %>%
  mutate(isevac = ifelse(is.na(ishouse), 1, 0) ) %>%
  mutate(ishouse = ifelse(is.na(ishouse), 0, 1) )
  
blended %>%  activate("nodes") %>% st_as_sf() %>% as.data.frame() %>% tail(10)
  
table(components(blended)$membership)
  
blended = blended %>% activate('nodes')%>%
  filter(components(blended)$membership == 1)
  
# Ανάθεση  

ggplot() +
  geom_sf(data = st_as_sf(net4%>%  activate("edges")), color = 'grey') +
  geom_sf(data = st_as_sf(net4%>%  activate("nodes")), color = 'grey')+
  geom_sf(data = evac_points, color = mycol, cex=3, pch=17)

rowids_evac = blended %>%  activate("nodes") %>% as.data.frame() %>%  rowid_to_column() %>% filter(isevac == 1) %>% pull(rowid)
tail(rowids_evac)
  
rowids_houses = blended %>%  activate("nodes") %>% as.data.frame() %>%  rowid_to_column() %>% filter(ishouse == 1) %>% pull(rowid)
tail(rowids_houses)
  
evac_sf = blended %>%  activate("nodes") %>% st_as_sf() %>% filter(isevac==1)
houses_sf = blended %>%  activate("nodes") %>% st_as_sf() %>% filter(ishouse==1)   #%>% rowid_to_column()
  
n_edges =  blended %>% activate(edges)  %>% st_as_sf()%>%  nrow()
  
blended = blended %>% activate(edges) %>% mutate(IDedge = 1:n_edges)
  
dm = st_network_cost(blended, from =rowids_houses , to =rowids_evac ,  direction="all")
head(dm)
  
houses_sf$closest_index = apply(dm, 1, function(x) which(x == min(x))[1])
houses_sf$closest_index_dist = apply(dm, 1, function(x)  min(x)[1])
  
plot(blended, col="grey")
plot(st_geometry(houses_sf), cex=1.5, col=mycol[houses_sf$closest_index], pch=21, add=T)
plot(st_geometry(evac_sf) , cex=2, pch=17, add=T, col=mycol)
plot(poly, add=T)
  
## Στατιστικά ανάθεσης  

table(houses_sf$closest_index)
  
houses_sf %>% as.data.frame()%>% group_by(closest_index) %>%
  summarise( min_dist=min(closest_index_dist),
             max_dist=max(closest_index_dist),
             mean_dist=mean(closest_index_dist) )
  
# Περιορισμός απόστασης  

apostasi = 800

dm2 = dm
dm2 = units::drop_units(dm2) #  Αφαίρεση μονάδων μέτρησης από το Distance Matrix
dm2[dm2>=apostasi] = NA

houses_sf$closest_index2 = apply(dm2, 1, function(x) which(x == min(x, na.rm=T))[1])
houses_sf$closest_index_dist2 = apply(dm2, 1, function(x)  min(x, na.rm=T)[1])
  
plot(blended, col="grey", main = sprintf("Με περιορισμό απόστασης %sμ", apostasi))
plot(st_geometry(houses_sf), cex=1.5, col=mycol[houses_sf$closest_index2],  pch=21, add=T)
plot(st_geometry(evac_sf) , cex=2, pch=17, add=T, col=mycol)
plot(poly, add=T)
plot(st_geometry(houses_sf),cex=0.5, add=T, col="grey", pch=20)
  
table(houses_sf$closest_index2,  useNA="ifany")
  
# Περιορισμός πλήθους  

dm3 = dm

df = as.data.frame(dm3)
df$whichMin = apply(dm3, 1, which.min)
df$minDistance = apply(dm3, 1, FUN=min, na.rm=T)
  
library(dplyr)
df3 = df %>%
  group_by(whichMin) %>%
  mutate(my_ranks = order(order(minDistance, decreasing=F)))


df3
  
df3$whichMin2 = NA
df3[df3$whichMin==1 & df3$my_ranks %in% c(1:55),]$whichMin2 = 1
df3[df3$whichMin==2 & df3$my_ranks %in% c(1:55),]$whichMin2 = 2
df3[df3$whichMin==3 & df3$my_ranks %in% c(1:55),]$whichMin2 = 3
df3[df3$whichMin==4 & df3$my_ranks %in% c(1:55),]$whichMin2 = 4

houses_sf$closest_index3 = df3$whichMin2
  
plot(blended, col="grey", main = sprintf("Με περιορισμό πλήθους %s ατόμων", 55))
plot(st_geometry(houses_sf), cex=1.5, col=mycol[houses_sf$closest_index3],  pch=21, add=T)
plot(st_geometry(evac_sf) , cex=2, pch=17, add=T, col=mycol)
plot(poly, add=T)
plot(st_geometry(houses_sf),cex=0.5, add=T, col="grey", pch=20)
  
houses_sf %>% as.data.frame()%>% group_by(closest_index2) %>%
  summarise( min_dist=min(closest_index_dist2),
             max_dist=max(closest_index_dist2),
             mean_dist=mean(closest_index_dist2) )