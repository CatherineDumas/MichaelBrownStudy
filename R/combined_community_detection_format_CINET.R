setwd("../data")

edges = read.csv("combined_g06.nx", header=F, sep="\t")
ids = unique(as.vector(as.matrix(edges)))

edges$nodeint1 = match(edges$V1, ids)
edges$nodeint2 = match(edges$V2, ids)
write.table(edges[, c("nodeint1", "nodeint2")], "combined.g06.nx", sep= " ", col.names=F, row.names=F) 