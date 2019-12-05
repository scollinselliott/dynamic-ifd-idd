# IDD
#####

g1.q.mc <- c()
g1.z.mc <- c()
g2.q.mc <- c()
g2.z.mc <- c()

p <- ggplot()

for ( i in 1:length(gazetteers) )  {
   g <<- gazetteers[[i]][[10]]
   g1 <- g[which(g$despotic == 1), ]
   g2 <- g[which(g$despotic == 2), ]
   g1o <- g1[order(-g1$size),]
   g2o <- g2[order(-g2$size),]
   g1o$rank <- 1:nrow(g1o)
   g2o$rank <- 1:nrow(g2o)
   
   if (nrow(g1o) > 1) & (nrow(g2o) > 1)) {
      g1.zmfit <- zmfit(g1o$rank, g1o$size)
      g2.zmfit <- zmfit(g2o$rank, g2o$size)
      
      g1.q.mc <- append(g1.q.mc, g1.zmfit$q)
      g1.z.mc <- append(g1.z.mc, g1.zmfit$z)

      g2.q.mc <- append(g2.q.mc, g2.zmfit$q)
      g2.z.mc <- append(g2.z.mc, g2.zmfit$z)

    }

}


# IFD
#####

g.q.mc <- c()
g.z.mc <- c()

for ( i in 1:length(gazetteers) )  {
  g <- gazetteers[[i]][[50]]
  
  if (nrow(g) > 1) {
    go <- g[order(-g$size),]
    go$rank <- 1:nrow(go)

    g.zmfit <- zmfit(go$rank, go$size)

    g.q.mc <- append(g.q.mc, g.zmfit$q)
    g.z.mc <- append(g.z.mc, g.zmfit$z)
  }


}
