#* ----------------------------------------------------------------------------
#* "THE BEER-WARE LICENSE" (Revision 42):
#* <dpabon@openmailbox.org> wrote this file.  As long as you retain this notice you
#* can do whatever you want with this stuff. If we meet some day, and you think
#* this stuff is worth it, you can buy me a beer in return.
#*                                                              Daniel Pabón
#* ----------------------------------------------------------------------------
library(ape)
library(phytools)
## Reconstrucción de estados ancestrales 
## arbol
arbol <- read.nexus("~/data/arbol_final.tre")
arbol
plot(arbol)
nodelabels()
## Enraizando
arbol <- root(arbol, interactive = T, resolve.root = F)
plot.phylo(arbol)
edgelabels(arbol$edge.length)
is.binary.tree(arbol)
class(arbol)
arbol
## Resolviendo politomia
is.binary.tree(arbol)
arbol$tip.label
arbol.resuelto <- multi2di(arbol)
plot.phylo(arbol.resuelto, use.edge.length = F)
edgelabels(arbol.resuelto$edge.length)
arbol.resuelto <- read.nexus("~/MEGAsync/evolution_project/data/arbol_resuelto.nex")
arbol.resuelto$edge.length
plot.phylo(arbol.resuelto,use.edge.length = T)
plot.phylo(arbol.resuelto, use.edge.length = F)
is.binary.tree(arbol.resuelto)
#write.nexus(arbol.resuelto,file = "~/MEGAsync/evolution_project/data/arbol_resuelto.nex")