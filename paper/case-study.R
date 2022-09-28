# (i) load package
library(wdpar)

# (ii) download data
d <- wdpa_fetch("Malta")

# (iii) clean data
d <- wdpa_clean(d, verbose = TRUE)

# (iv) report number of protected areas in each IUCN category
table(d$IUCN_CAT)

# (v) create a map showing the protected areas
# N.B., colors denote different IUCN categories
pal <- c("#FDE725FF", "#8FD744FF", "#35B779FF", "#21908CFF",
         "#31688EFF", "#443A83FF", "#440154FF")
plot(d[, "IUCN_CAT"], main = NA, pal = pal, graticule = TRUE,
     axes = TRUE, key.pos = 1L, key.length = 0.9)
