library(igraph)
setwd("data")
combined<-read.csv("combined.edgelist.10.confidence.csv",header=FALSE)





g=graph.empty()+vertices(1:25)
g<-g+edges(as.vector(as.matrix(t(combined))))
plot(g)
combined_sigs<-read.csv("combined_signature_counts.csv", header=FALSE) 
plot(g,vertex.size=as.vector(t(combined_sigs))/3000)
