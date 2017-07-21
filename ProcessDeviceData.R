library("plot3D")

T1 <- rbind(read.table("IMU/data-20-57.txt"),read.table("IMU/data-20-59.txt"))
T2 <- rbind(read.table("LIDAR/data-20-57.txt"),read.table("LIDAR/data-20-59.txt"))

ax <- T1$V1
ay <- T1$V2
atime <- T1$V3

D <- T2$V1
dtime <- T2$V2

options(digits=3)

atime <- (60*60*(as.double(substring(atime,1,2)))) + 60*(as.double(substring(atime,4,5))) + (as.double(substring(atime,7,12)))
atime <- atime - atime[1]

dtime <- (60*60*(as.double(substring(dtime,1,2)))) + 60*(as.double(substring(dtime,4,5))) + (as.double(substring(dtime,7,12)))
dtime <- dtime - dtime[1]

#### FIT VECTORS TO MINIMUM SIZE ####

nax <- vector(mode = "numeric", length = length(dtime))
nay <- vector(mode = "numeric", length = length(dtime))

i <- 1
while (i <= length(dtime))
{
  actual <- dtime[i]
  temp <- abs(atime - actual)
  index <- which(temp == min(temp))
  
  nax[i] <- ax[index]
  nay[i] <- ay[index]
  
  i <- i + 1
}

#### CONVERT CLOUD FROM ANGLES AND DISTANCE TO POINTS ####

x <- vector(mode = "numeric", length = 0L)
y <- vector(mode = "numeric", length = 0L)
z <- vector(mode = "numeric", length = 0L)

i <- 1
while (i <= length(dtime))
{
  if (D[i] <= 500)
  {
    x <- c(x,D[i]*cos(nax[i]*pi/180)*cos(nay[i]*pi/180))
    y <- c(y,D[i]*cos(nax[i]*pi/180)*sin(nay[i]*pi/180))
    z <- c(z,D[i]*sin(nax[i]*pi/180))
  }
  i <- i + 1
}

plot3D(x, y, z, col="blue", type ="p")
