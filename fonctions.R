######################################################################
#### Fonctions
######################################################################

## Pour chaque groupe, calcul l'écart entre la moyenne de var
## dans ce groupe et la moyenne globale de var, fait un test t
## et conserve le résultat seulement si sa significativité est
## inférieure à seuil

difftests <- function(var, groupes, seuil=0.05) {
  v <- sapply(levels(groupes), function(i) {
    var.values <- var[groupes==i]
    diff.mean <- round(mean(var.values)-mean(var),2)
    if (length(var.values)<2) p.mean <- 0
    else p.mean <- t.test(var ~ groupes==i)$p.value
    if (p.mean > seuil) diff.mean <- NA
    return(diff.mean) })        
  names(v) <- levels(groupes)
  v
}

## Construit une matrice en agrégeant les difftests pour chaque
## variable de vars

diffmatrix <- function(vars, groupes, seuil=0.05, levels, labels) {
  m <- sapply(vars, function(name) {
    difftests(lyon.data[,name], groupes, seuil=seuil)
    })
  t(m)
}

## Représentation graphique d'une matrice calculée avec diffmatrix

diffmatrix.plot <- function(m, seuil.diff=0, levels, labels, title=NULL) {
  mm <- melt(m)
  mm$Var1 <- factor(mm$Var1, levels=vars, labels=labels)
  mm$value[abs(mm$value) < seuil.diff] <- NA
  ggplot(data=subset(mm, !is.na(value))) +
    geom_tile(aes(x=Var1, y=Var2,fill=value), colour="white") +
    scale_fill_gradient2(low="blue", mid="white", high="red") +
    geom_text(aes(x=Var1, y=Var2,label=value), size=5) +
    scale_y_continuous(breaks=1:max(mm$Var2)) +
    theme_gray(base_size = 14) +
    theme(legend.position="none",
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=15)) +
    xlab("") + ylab("Groupe") + labs(title=title)
}

## Affiche la répartition de la densité pour chaque variable
## pour un groupe donné

groupes.density <- function(df, groupe) {
  tmp.density <- df[df$groupes==groupe,]
  tmp.density$variable <- factor(tmp.density$variable, levels=vars, labels=labels)
  ggplot(data=tmp.density) +
    geom_density(aes(x=value, fill=variable, color=variable, alpha=0.6)) +
    geom_vline(xintercept=0, linetype=2) +
    facet_grid(variable~., scales="free") +
    scale_x_continuous(limits=c(-30,30), name="Écart à la moyenne globale") +
    scale_y_continuous(breaks=NULL, name="") +
    theme(legend.position="none", strip.text.y=element_text(size=11,angle=0)) +
    labs(title="") +
    theme(plot.margin=unit(c(2, 1, 2, 0), "lines")) +
    theme(axis.title.x=element_text(vjust=-1,size=12), plot.title=element_text(vjust=2, size=20))
}

## Affiche une carte de répartition des bureaux d'un groupe
## donné

groupes.carte <- function(df, id.groupe) {
    tmp.geo <- df
    tmp.geo <- tmp.geo[tmp.geo$groupes != id.groupe,]
    ggplot(df) + 
        aes(long,lat,group=group,fill=groupes,map_id=id) +
        geom_map(map=df, colour=NA, size=0.2, show_guide=FALSE) +
        geom_map(map=tmp.geo, fill="white", size=0.2, show_guide=FALSE) +
        geom_map(map=df, fill=NA, colour="black", size=0.2, show_guide=FALSE) +
        geom_polygon(data=arr, aes(x=long, y=lat), fill=NA, colour="black", size=1, show_guide=FALSE) +
        coord_map() +
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(axis.ticks = element_blank(), 
              axis.title.y = element_blank(), 
              axis.text.y =  element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x =  element_blank()) +
        labs(title=paste0("Bureaux du groupe ",id.groupe), fill="Groupe")
}
