## Graficas finales
library(phytools)

arbol <- read.nexus("~/MEGAsync/evolution/data/arbol_resuelto.nex")
datos <- read.csv("~/MEGAsync/evolution/data/character.csv", sep = ",")
plot.phylo(arbol, use.edge.length = F)
## Convirtiendo 0 en 0.00001
for( i in 1:length(arbol$edge.length)){
  if(arbol$edge.length[i] == 0){
    arbol$edge.length[i] <- 0.00001
  }
}
write.nexus(arbol, file = "~/MEGAsync/evolution/data/ff.nex")
edgelabels(arbol$edge.length)
edgelabels()
x11()
plot.phylo(arbol, use.edge.length = F, label.offset = 0.55 )
edgelabels(round(arbol$edge.length, 5),frame = "n", adj = c(0.3,1), cex=0.8)
###Caracter corolla_size
x0 <- as.numeric(as.vector(datos$corola_size))
x0
## reconstrucción
reco0 <- ace(x0, arbol, type = "continuous", method = "ML", model = "BM")
names(x0) <- arbol$tip.label
plot.phylo(arbol)
nodelabels(format(round(reco0$ace, 2), nsmall = 2), frame = "n", adj = 0)
plot.phylo(arbol, use.edge.length = F)
nodelabels(format(round(reco0$ace, 2), nsmall = 2), frame = "n", adj = 0)
?contMap
contMap(arbol, x0, lwd = 3.5)
nodelabels(format(round(reco0$ace, 2), nsmall = 2), frame = "n", adj = c(0,0.7))
dev.off()
phenogram(arbol, x0, spread.labels=TRUE)


###Caracter corolla_dissection
x1 <- as.numeric(as.vector(datos$corola_disection))
## reconstrucción
reco1 <- ace(x1, arbol, type = "continuous", method = "ML", model = "BM")
names(x1) <- arbol$tip.label
plot.phylo(arbol)
nodelabels(format(round(reco1$ace, 2), nsmall = 2), frame = "n", adj = 0)
plot.phylo(arbol, use.edge.length = F)
nodelabels(format(round(reco1$ace, 2), nsmall = 2), frame = "n", adj = 0)
contMap(arbol, x1, lwd = 3.5)
nodelabels(format(round(reco1$ace, 2), nsmall = 2), frame = "n", adj = c(0,0.7))
dev.off()
phenogram(arbol, x1, spread.labels=TRUE)



###Caracter lower_lip
x2 <- as.numeric(as.vector(datos$lower_lip))

## reconstrucción
reco2 <- ace(x2, arbol, type = "continuous", method = "ML", model = "BM")

names(x2) <- arbol$tip.label
plot.phylo(arbol)
nodelabels(format(round(reco2$ace, 2), nsmall = 2), frame = "n", adj = 0)
plot.phylo(arbol, use.edge.length = F)
nodelabels(format(round(reco2$ace, 2), nsmall = 2), frame = "n", adj = 0)
contMap(arbol, x2, lwd = 3.5)

nodelabels(format(round(reco2$ace, 2), nsmall = 2), frame = "n", adj = 0)
dev.off()
phenogram(arbol, x2)

###Caracter tube_lenght
x3 <- as.numeric(as.vector(datos$tube_lenght))
## reconstrucción
reco3 <- ace(x3, arbol, type = "continuous", method = "ML", model = "BM")
reco3
names(x3) <- arbol$tip.label
plot.phylo(arbol)
nodelabels(format(round(reco3$ace, 2), nsmall = 2), frame = "n", adj = 0)
plot.phylo(arbol, use.edge.length = F)
nodelabels(format(round(reco3$ace, 2), nsmall = 2), frame = "n", adj = 0)
contMap(arbol, x3, lwd = 3.5)
nodelabels(format(round(reco3$ace, 2), nsmall = 2), frame = "n", adj = c(-0.2,1))
dev.off()
phenogram(arbol, x3)

## Caracter corolla_color 
x4 <- as.factor(as.vector(datos$corola_color))
x4
## reconstrucción
reco4 <- ace(x4, arbol, type = "discrete")
reco4$lik.anc
reco4
names(x4) <- arbol$tip.label
plot.phylo(arbol, use.edge.length = T, label.offset = 0.0005)
nodelabels(node =13:23, pie = reco4$lik.anc, 
           piecol = c("pink", "red", "white"), cex = 0.6)
tiplabels(pie = to.matrix(x4, sort(unique(x4))), piecol = c("pink", "red", "white"), 
          cex = 0.3)
legend("bottomleft", legend = c("Rosado", "Rojo", "Blanco"),pch = 22, lwd = 5,   col = c("pink", "red", "white"), bty = "o", yjust = 1, y.intersp = 0.7, x.intersp =0.5, xjust = 1, bg = "gray")

## Caracter Syndrome
x5 <- as.factor(as.vector(datos$Syndrome))

reco5 <- ace(x5, arbol, type = "discrete")
reco5$lik.anc
reco5
names(x5) <- arbol$tip.label

plot.phylo(arbol, use.edge.length = T, label.offset =0.0003, direction = "l")
nodelabels(node =13:23, pie = reco5$lik.anc, 
           piecol = c("yellow", "blue", "red"), cex = 0.6)
tiplabels(pie = to.matrix(x5, sort(unique(x5))), piecol = c("yellow", "blue", "red"), 
          cex = 0.3)
par("usr")
legend(x =0.024, y = 1.8 , legend = c("Abeja", "Colibri", "Polilla"),pch = 22, lwd = 5,   col = c("yellow", "blue", "red"), bty = "n", yjust = 0.7, y.intersp = 0.7, x.intersp = 0.7)

?legend()
## Caracter Petal Reflection

x6 <- as.factor(as.vector(datos$petal_reflection))

reco6 <- ace(x6, arbol, type = "discrete")
reco6$lik.anc
reco6
names(x6) <- arbol$tip.label
plot.phylo(arbol, use.edge.length = T, label.offset =0.0003)
nodelabels(node =13:23, pie = reco6$lik.anc, 
           piecol = c("black", "gray"), cex = 0.6)
tiplabels(pie = to.matrix(x6, sort(unique(x6))), piecol = c("black", "gray"), 
          cex = 0.3)
legend("bottomleft", legend = c("Sin Reflexión", "Reflexión"),pch = 22, lwd = 5,   col = c("black", "gray"), bty = "n", yjust = 0.7, y.intersp = 0.7, x.intersp = 0.7)
par("usr")
## Caracter Explosive pollen discharge 

x7 <- as.factor(as.vector(datos$pollen_discharge))

reco7 <- ace(x7, arbol, type = "discrete")
reco7$lik.anc
reco7
names(x7) <- arbol$tip.label
plot.phylo(arbol, use.edge.length = T, label.offset =0.0003)
nodelabels(node =13:23, pie = reco7$lik.anc, 
           piecol = c("black", "white"), cex = 0.6)
tiplabels(pie = to.matrix(x7, sort(unique(x7))), piecol = c("black", "white"), 
          cex = 0.3)
legend("bottomleft", legend = c("Presente", "Ausente"),pch = 22, lwd = 5,   col = c("white", "black"), bty = "o", yjust = 0.7, y.intersp = 0.7, x.intersp = 0.7, bg = "gray")
par("usr")


## Correlaciones
# bee vs petall reflection
bee <- as.factor(as.vector(datos$Hummingbird))
names(bee) <- arbol$tip.label
fitPagel(arbol, bee, x6, method="ace")
# bee vs Explosive pollen discharge
bee <- as.factor(as.vector(datos$Hummingbird))
names(bee) <- arbol$tip.label
fitPagel(arbol, bee, x7, method="ace")

# bee vs Red
red <- as.factor(as.vector(datos$Red))
names(red) <- arbol$tip.label
red
fitPagel(arbol, red, bee, method="ace")
?fitPagel

# bee vs pink
pink <- as.factor(as.vector(datos$Pink))
names(pink) <- arbol$tip.label
fitPagel(arbol,bee, pink, method = "ace")

# bee vs white
white <- as.factor(as.vector(datos$White))
names(white) <- arbol$tip.label
fitPagel(arbol, bee, white, method ="ace")
?fitPagel
