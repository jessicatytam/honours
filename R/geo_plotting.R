library(letsR)
library(geosphere)
library(readr)
library(dplyr)
sbs <- read_csv(file = "intermediate_data/gbif_processed.csv")


lets.midpoint.fixed<-function (pam, planar = FALSE, method = "PC") 
{
  if (!(method %in% c("PC", "CMD", "MCC", "GM"))) {
    stop("method not found. The chosen method should be PC, CMD, MCC or GM.")
  }
  if (class(pam) == "PresenceAbsence") {
    n <- ncol(pam[[1]])
    species <- pam[[3]]
    pam2 <- pam[[1]]
  }
  else {
    n <- ncol(pam)
    species <- colnames(pam)[-(1:2)]
    pam2 <- pam
  }
  xm <- numeric((n - 2))
  ym <- xm
  if (method == "PC") {
    if (class(pam) != "PresenceAbsence") {
      stop("This method only works for PresenceAbsence objects")
    }
    for (i in 3:n) {
      pos <- which(pam2[, i] == 1)
      if (length(pos) == 1) {
        dis2 <- pam2[pos, 1:2, drop = FALSE]
      }
      else {
        ptemp <- pam[[2]]
        pos2 <- raster::extract(ptemp, pam2[pos, 1:2, 
                                            drop = FALSE], cellnumbers = TRUE)[, 1]
        values(ptemp)[-pos2] <- NA
        values(ptemp)[pos2] <- 1
        p <- rasterToPolygons(ptemp, dissolve = TRUE)
        if (!planar) {
          dis2 <- centroid(p)
        }
        else {
          dis2 <- gCentroid(p)@coords
        }
      }
      xm[(i - 2)] <- dis2[1, 1]
      ym[(i - 2)] <- dis2[1, 2]
    }
  }
  if (method == "GM") {
    for (i in 3:n) {
      pos <- which(pam2[, i] == 1)
      if (length(pos) == 1) {
        dis2 <- pam2[pos, 1:2, drop = FALSE]
        xm[(i - 2)] <- dis2[1, 1]
        ym[(i - 2)] <- dis2[1, 2]
      }
      else {
        a <- max(pam2[pos, 1])
        b <- min(pam2[pos, 1])
        c <- max(pam2[pos, 2])
        d <- min(pam2[pos, 2])
        if (!planar) {
          if (length(pos) == 2) {
            dis2 <- midPoint(pam2[pos[1], 1:2], pam2[pos[2], 
                                                     1:2])
          }
          else {
            if (a == b | c == d) {
              if (a == b) {
                dis2 <- midPoint(c(a, c), c(a, d))
              }
              else {
                dis2 <- midPoint(c(a, c), c(b, c))
              }
            }
            pol <- matrix(c(a, a, b, b, c, d, d, c), 
                          ncol = 2)
            dis2 <- centroid(pol)
          }
          xm[(i - 2)] <- dis2[1, 1]
          ym[(i - 2)] <- dis2[1, 2]
        }
        if (planar) {
          xm[(i - 2)] <- mean(c(a, b))
          ym[(i - 2)] <- mean(c(c, d))
        }
      }
    }
  }
  if (method == "CMD") {
    for (i in 3:n) {
      loc <- pam2[, i] == 1
      if (sum(loc) == 0) {
        ym[i - 2] <- xm[i - 2] <- NA
      }
      if (sum(loc) == 1) {
        xm[i - 2] <- pam2[loc, 1]
        ym[i - 2] <- pam2[loc, 2]
      }
      if (sum(loc) > 1) {
        if (!planar) {
          dist.mat <- lets.distmat(pam2[loc, 1:2], asdist = FALSE)
        }
        if (planar) {
          dist.mat <- as.matrix(dist(pam2[loc, 1:2]))
        }
        summed.dist <- apply(dist.mat, 2, sum)
        mid.p <- pam2[loc, 1:2][which.min(summed.dist)[1], 
        ]
        xm[i - 2] <- mid.p[1]
        ym[i - 2] <- mid.p[2]
      }
    }
  }
  if (method == "MCC") {
    for (i in 3:n) {
      pos <- which(pam2[, i] == 1)
      lpos <- length(pos)
      if (lpos == 1) {
        dis2 <- pam2[pos, 1:2, drop = FALSE]
      }
      else {
        if (lpos == 2) {
          if (!planar) {
            dis2 <- midPoint(pam2[pos[1], 1:2], pam2[pos[2], 
                                                     1:2])
          }
          else {
            dis2 <- matrix(colMeans(pam2[pos, 1:2]), 
                           ncol = 2)
          }
        }
        else {
          pam3 <- pam2[pos, 1:2]
          hp <- chull(pam2[pos, 1], pam2[pos, 2])
          hp <- c(hp, hp[1])
          p <- SpatialPolygons(list(Polygons(list(Polygon(pam3[hp, 
                                                               1:2])), 1)))
          if (!planar) {
            dis2 <- geosphere:::centroid(p)
          }
          else {
            dis2 <- gCentroid(p)@coords
          }
        }
      }
      xm[(i - 2)] <- dis2[1, 1]
      ym[(i - 2)] <- dis2[1, 2]
    }
  }
  resu <- as.data.frame(cbind(species, xm, ym))
  colnames(resu) <- c("Species", "x", "y")
  #resu[, 2] <- as.numeric(levels(resu[, 2]))[resu[, 2]]
  #resu[, 3] <- as.numeric(levels(resu[, 3]))[resu[, 3]]
  return(resu)
}



names(sbs)

table(sbs$order)
#test<-filter(sbs,order=="Primates")

PAM <- lets.presab.points(cbind(sbs$decimalLongitude,sbs$decimalLatitude), sbs$species, xmn = -180, xmx = 180, 
                          ymn = -90, ymx = 90,resol = 2)

summary(PAM)
plot(PAM)
mid <- lets.midpoint.fixed(PAM)
mid$x<-as.numeric(mid$x)
mid$y<-as.numeric(mid$y)
mid$order<-sbs$order[match(mid$Species,sbs$species)]


library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")


ggplot(data = world) +
  geom_sf()+geom_point(data=mid,aes(x=x,y=y,col=order),alpha=0.6,size=0.5)+theme_void()
