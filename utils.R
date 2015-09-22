# convert spatstat objects to sp classes
# auteur : Adrian Baddeley
# source : https://stat.ethz.ch/pipermail/r-sig-geo/2009-May/005781.html

owin2Polygons <- function(x, id="1") {
  stopifnot(is.owin(x))
  x <- as.polygonal(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords=closering(cbind(p$x,p$y)),
                             hole=is.hole.xypolygon(p))  })
  z <- Polygons(pieces, id)
  return(z)
}

tess2SP <- function(x, projection=CRS(as.character(NA))) {
  stopifnot(is.tess(x))
  y <- tiles(x)
  nam <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nam[i])
  return(SpatialPolygons(z,projection)) # modification pour inclure la projection
}

owin2SP <- function(x, projection=CRS(as.character(NA))) {
  stopifnot(is.owin(x))
  y <- owin2Polygons(x)
  z <- SpatialPolygons(Srl = list(y),proj4string = projection) 
  return(z)
}

# SpatialPolygon2SP
# source : aucune
