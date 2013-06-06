## Lecture fichier shapefile pour ggplot2
## Voir : https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles


require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
gpclibPermit()

lyon <- readOGR(dsn="data/", layer="vdl_vie_citoyenne.contour_de_bureau_de_vote")
lyon@data$id <- rownames(lyon@data)
lyon.points <- fortify(lyon, region="id")

lyon.arr <- readOGR(dsn="data/", layer="arrondissements")
arr <- fortify(lyon.arr, region="nom")

votes <- read.csv("data/votes.csv")
tmp <- merge(lyon@data, subset(votes, select=-id), by="num_bureau",all.x=TRUE, all.y=TRUE)
lyon.df <- join(lyon.points, tmp, by="id")

carte <- function(varname, color) { 
    g <- ggplot(lyon.df) + 
               aes_string("long","lat",group="group",fill=varname,map_id="id") +
               geom_map(map=lyon.points, colour="black", size=0.2) +
               geom_polygon(data=arr, aes(x=long, y=lat), fill=NA, colour="black", size=1) +
               coord_map() +
               scale_fill_gradient(low="white", high=color) +
               theme_bw()
    print(g)
}

carte("joly","#009900")
carte("sarkozy","#000099")
carte("lepen","#000099")
carte("melenchon","#990000")
carte("hollande","#990000")
carte("arthaud","#990000")
carte("bayrou","#990000")
