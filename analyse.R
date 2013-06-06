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

load("data/lyon_com.rda") ## lyon.com.sp
lyon.arr <- fortify(lyon.com.sp, region="DepCom")

votes <- read.csv("data/votes.csv")
tmp <- merge(lyon@data, subset(votes, select=-id), by="num_bureau",all.x=TRUE, all.y=TRUE)
lyon.df <- join(lyon.points, tmp, by="id")


ggplot(lyon.df) + 
    aes(long,lat,group=group,fill=joly,map_id=id) +
    geom_map(map=lyon.points, colour="black", size=0.2) +
    geom_polygon(data=lyon.arr, aes(x=long, y=lat), fill=NA, colour="black", size=1) +
    coord_map() +
    scale_fill_gradient(low="white", high="red") +
    theme_bw()
