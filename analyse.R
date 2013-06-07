
######################################################################
#### Préparation
######################################################################

library(questionr)
library(flashClust)

require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("reshape2")
gpclibPermit()

source("fonctions.R")


## Répertoire de sortie des graphiques
out.path <- "~/projets/data.nozav.org/images/bureaux_lyon"

## Lecture fichier shapefile pour ggplot2
## Voir : https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
lyon <- readOGR(dsn="data/", layer="vdl_vie_citoyenne.contour_de_bureau_de_vote")
lyon@data$id <- rownames(lyon@data)
lyon.points <- fortify(lyon, region="id")

## Chargement shapefile arrondissements de Lyon
lyon.arr <- readOGR(dsn="data/", layer="arrondissements")
arr <- fortify(lyon.arr, region="nom")

## Chargement données résultat et fusion sur données geo
votes <- read.csv("data/votes.csv")
lyon.data <- merge(lyon@data, subset(votes, select=-id), by="num_bureau",all.x=TRUE, all.y=TRUE)
lyon.geo <- join(lyon.points, lyon.data, by="id")



######################################################################
#### Cartes
######################################################################



##### Carte de comparaison des principaux candidats
######################################################################


## Carte de comparaison des principaux candidats. On n'a gardé que les
## candidats ayant obtenu plus de 5% dans un bureau de vote.

vars <- c("num_bureau", "id", "sarkozy", "hollande", "melenchon", "lepen", "bayrou", "joly")

## Passage au format long
tmp <- lyon.data[,vars]
names(tmp) <- c("num_bureau", "id", "Sarkozy", "Hollande", "Mélenchon", "Le Pen", "Bayrou", "Joly")
lyon.data.long <- melt(tmp)
## Fusion sur données geo
lyon.geo.long <- join(lyon.points, lyon.data.long, by="id")

## Carte
ggplot(lyon.geo.long) + 
    aes(long,lat,group=group,fill=value,map_id=id) +
    geom_map(map=lyon.geo.long, colour="black", size=0.2) +
    geom_polygon(data=arr, aes(x=long, y=lat), fill=NA, colour="black", size=1) +
    coord_map() +
    scale_fill_gradient(limits=c(0,63), low="white", high="#AA0000", space="rgb") +
    theme_bw() +
    theme(axis.ticks = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.y =  element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x =  element_blank()) +
    labs(title="Pourcentages obtenus au premier tour par les principaux candidats", fill="%") +
    facet_wrap(~variable)



##### Cartes par candidat
######################################################################


## Fonction de production de la carte
carte <- function(varname, color, title="") { 
    ggplot(lyon.geo) + 
        aes_string("long","lat",group="group",fill=varname,map_id="id") +
        geom_map(map=lyon.geo, colour="black", size=0.2) +
        geom_polygon(data=arr, aes(x=long, y=lat), fill=NA, colour="black", size=1) +
        coord_map() +
        scale_fill_gradient(limits=c(0,max(lyon.geo[,varname])),low="white", high=color, space="rgb") +
        theme_bw() +
        theme(axis.ticks = element_blank(), 
              axis.title.y = element_blank(), 
              axis.text.y =  element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x =  element_blank()) +
        labs(title=title, fill="%")
}

carte("sarkozy","#000077", "Nicolas Sarkozy")
carte("hollande","dark magenta", "François Hollande")
carte("melenchon","#BB0000","Jean-Luc Mélenchon")
carte("lepen","saddle brown", "Marine Le Pen")
carte("bayrou","DarkOrange2", "François Bayrou")
carte("joly","#007700", "Éva Joly")
carte("dupont.aignan","#004477", "Nicolas Dupont-Aignan")
carte("arthaud","#990000", "Nathalie Arthaud")
carte("poutou","blue violet", "Philippe Poutou")
carte("cheminade","dark cyan", "Jacques Cheminade")
carte("blancs","#777777", "Votes blancs ou nuls")
carte("abst","olive drab", "Abstention")



######################################################################
#### Classification
######################################################################


##### Calcul
######################################################################

vars <- c("sarkozy", "hollande", "melenchon", "lepen", "bayrou", "joly", "dupont.aignan","arthaud","poutou","cheminade","blancs","abst")

## Calcul de la CAH
tmp <- lyon.data[,vars]
library(flashClust)
dist <- dist(tmp)
hc <- hclust(dist,method="ward")

## Dendrogramme
plclust(hc,labels=FALSE,hang=0)

## On choisit de conserver 8 classes
nb.classes <- 8

## Calcul des groupes
groupes <- factor(cutree(hc, k=nb.classes))
freq(groupes)


##### Description des classes
######################################################################


vars <- c("lepen","sarkozy","dupont.aignan","bayrou","hollande",
          "joly","melenchon","poutou","arthaud","cheminade",
          "blancs","abst")
labels <- c("Le Pen","Sarkozy","Dupont-Aignan","Bayrou","Hollande",
            "Joly","Mélenchon","Poutou","Arthaud","Cheminade",
            "Blancs","Abstention")

## Diffmatrix de la classification
m <- diffmatrix(vars, groupes, seuil=0.001, levels=vars, labels=labels)

png(file=paste0(out.path,"diffmatrix.png",sep=""),width=800,height=600)
diffmatrix.plot(m, seuil.diff=0.5,
                levels=vars, labels=labels,
                title="Description des groupes")
dev.off()

lyon.data <- cbind(lyon.data,groupes)
lyon.geo <- join(lyon.points, lyon.data, by="id")

ggplot(lyon.geo) + 
    aes(long,lat,group=group,fill=groupes,map_id=id) +
    geom_map(map=lyon.geo, colour=NA, size=0.2) +
    geom_map(map=lyon.geo, fill=NA, colour="black", size=0.2, show_guide=FALSE) +
    geom_polygon(data=arr, aes(x=long, y=lat), fill=NA, colour="black", size=1, show_guide=FALSE) +
    coord_map() +
    scale_fill_brewer(palette="Accent") +
    theme_bw() +
    theme(axis.ticks = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.y =  element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x =  element_blank()) +
    labs(title="Répartition des groupes", fill="Groupe")

## Calcul des variables centrées (on leur soustrait
## leur moyenne globale)
tmp <- lyon.data[,vars]
moyennes <- apply(tmp, 2, mean)
tmp <- sweep(tmp, 2, moyennes)
tmp$groupes <- groupes
## Passage en format "long"
tmp.l <- melt(tmp)

## Export d'un graphe de comparaison de densités pour chaque groupe
for (groupe in 1:nb.classes.brut) {
  png(file=paste0(out.path,"densites_groupe",groupe,".png",sep=""),width=400,height=400)
  print(groupes.density(tmp.l, groupe=groupe))
  dev.off()
}


######################################################################
#### Export KML
######################################################################

library(RColorBrewer)
palette <- brewer.pal(nb.classes, "Accent")
cols <- paste(palette,sep="")
lyon.data$couleurs <- cols[groupes]

vars <- c("num_bureau", "lepen","sarkozy","dupont.aignan","bayrou","hollande",
          "joly","melenchon","poutou","arthaud","cheminade",
          "blancs","abst","groupes","couleurs")

lyon.json <- lyon
lyon.json@data <- merge(lyon.json@data, lyon.data, by="num_bureau")
#lyon.json@data$couleurs <- c("#FDC086","#00FF00","#FF0000")
writeOGR(lyon.json, "/var/www/openlayers/lyon.groupes.json", "lyon.groupes", "GeoJSON")

class(lyon.json@data$couleurs)
lyon.json@data$couleurs
