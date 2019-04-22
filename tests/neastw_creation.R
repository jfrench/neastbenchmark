library(neastbenchmark)
library(spdep)
nb2 = poly2nb(neastpoly)
plot(nb2, coord = neastdata[,c('easting', 'northing')],
     xlim = c(60000, 100000),
     ylim = c(3e+04, 5e+04))
axis(1)
axis(2)

# Connect Dukes Island
nb2[[16]] = 13L
nb2[[13]] = c(nb2[[13]], 16L)

# Connect Nantucket island
nb2[[22]] = c(13L, 16L)
nb2[[13]] = c(nb2[[13]], 22L)
nb2[[16]] = c(nb2[[16]], 22L)

plot(nb2, coord = neastdata[,c('easting', 'northing')],
     xlim = c(50000, 80000),
     ylim = c(2e+04, 4e+04))
axis(1)
axis(2)
# identify(neastdata[,c('easting', 'northing')])

# Connect NYC (Manhatten) to 4 other borroughs and Hudson County
v = c(85L, 100L, 121L, 138L, 140L)
neastdata$id[v]

nb2[[128]] = v
for (i in v) {
  nb2[[i]] = c(nb2[[i]], 128L)
}


plot(nb2, coord = neastdata[,c('easting', 'northing')],
     xlim = c(50000, 80000),
     ylim = c(2e+04, 4e+04))
axis(1)
axis(2)

v = c(100L, 127L, 138L, 149L)
neastdata$id[v]

# Nassau County borders the following counties: [127]
#
# Bronx County — northwest [100]
# Fairfield County, Connecticut — north [1]
# Queens County — west [138]
# Suffolk County — east [149]
# Westchester County — northwest [157]

v2 = c(100L, 1L, 157L)
nb2[[127]] = c(nb2[[127]], v2)
for (i in v2) {
  nb2[[i]] = c(nb2[[i]], 127L)
}

# Nassau County borders the following counties: [149]
# Nassau County, New York - west [127]
# Fairfield County, Connecticut - northwest [1]
# New Haven County, Connecticut - north [5]
# Middlesex County, Connecticut - north [4]
# New London County, Connecticut - north [6]
# Washington County, Rhode Island - northeast [231]
v3 = c(1L, 4L, 5L, 6L, 231L)
nb2[[149]] = c(nb2[[149]], v3)
for (i in v3) {
  nb2[[i]] = c(nb2[[i]], 149L)
}

#  Richmond county adjacent  [140]
# Union County, New Jersey [96]
# Hudson County, New Jersey	[85]
# New York County (Manhattan) [128]
# Kings County (Brooklyn) [121]
# # Middlesex County, New Jersey [88]
# Monmouth County, New Jersey [89]
v4 = c(89L, 121L )
nb2[[140]] = c(nb2[[140]], v4)
for (i in v4) {
  nb2[[i]] = c(nb2[[i]], 140L)
}

plot(nb2, coord = neastdata[,c('easting', 'northing')],
     xlim = c(50000, 80000),
     ylim = c(2e+04, 4e+04))
axis(1)
axis(2)

w = nb2mat(nb2, style = "B")
wl = w[lower.tri(w)]
wu = w[upper.tri(w)]

rid = attr(nb2, "region.id")

for (i in seq_along(nb2)) {
  tv = nb2[[i]]
  for (j in tv) {
    if (!is.element(i, nb2[[j]])) {
      print (i)
      message(rid[j], "not connected to", rid[j])
    }
  }
}

w = nb2mat(nb2, style = "B")
for (i in 1:nrow(w)) {
  tvi = which(w[i, ] == 1)
  for (j in tvi) {
    tvj = which(w[j, ] == 1)
    if (!is.element(i, tvj)) {
     #  print (i)
      message(rid[i], " not connected to ", rid[j])
    }
  }
}

# isSymmetric(unname(w))
# 
# neastw_old = neastw
# save(neastw_old, file = "neastw_old.rda", compress = "bzip2")

neastw = w
save(neastw, file = "neastw.rda", compress = "bzip2")
