#====================================================================
library(geomorph)
#--------------------------------------------------------------------
#Function that calculates Procrustes distance between two
#sets of coordinates
procrustesDist = function(c1, c2)
{
  sum(sqrt((c1[,1]-c2[,1])^2 + (c1[,2]-c2[,2])^2))
}
#--------------------------------------------------------------------
#reading the data file
dat = read.table(file = "data_aegla_SD.txt", header = TRUE, as.is=TRUE)

#creating numeric identifiers for species and sex 
#(it will make analysis easier)
dat$sp.number = as.numeric(factor(dat$label))
#mixed - 1
#pure - 2

dat$sex.number = as.numeric(factor(dat$sex))
#female - 1
#male - 2

#reading the shape files
aeglas = readland.tps("aeglas.TPS", specID = "ID")
slid.aeglas = read.table("sliders.txt", head=TRUE)
gpa.aegla = gpagen(aeglas, curves = slid.aeglas, ProcD = TRUE)

#recovering centroid size for each individual
dat$csize = gpa.aegla$dat$Csize

#discovering the positions of the females in the data.frame
fpure = which(dados$sex == "female" & dados$label == "pure")
fmixed = which(dados$sex == "female" & dados$label == "mixed")

#calculating the mean female shape for each species
pure.cshape = mshape(gpa.aegla$coords[1:17,1:2,fpure])
mixed.cshape = mshape(gpa.aegla$coords[1:17,1:2,fmixed])

cshapes = list(pure = pure.cshape,
               mixed = mixed.cshape)

#calculating procrustes distance from the shape of each
#individual to the mean female shape
dat$shape = 0 #adding a new column

for(i in 1:nrow(dat))
{
  #coordinates of individual i
  coords.i = gpa.aegla$coords[,,i]
  
  #coordinates of the mean female of its species
  consensus.i = cshapes[[dat$label[i]]]
  
  #calculates the procrustes distance
  dat$shape[i] = procrustesDist(coords.i, consensus.i)
}

#scaling and/or transforming data
dat$scsize = as.vector(scale(dat$csize)) #centroid size (of the claw)
dat$scc = as.vector(scale(dat$cc)) #body size
dat$sqapodeme = sqrt(dat$apodeme) #strength

#====================================================================
