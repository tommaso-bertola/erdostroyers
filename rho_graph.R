df = read.table("network_rho.csv")

x = unlist(df[1,])
df = df[-1,]

y=c()
s=c()

for(i in 1:length(x)) {
 y = c(y, mean(df[,i], na.rm=T))
 s = c(s, sd(df[,i], na.rm=T))
}

plot(x, y, ylim=c(-0.1, 1.1))
arrows(x, y+s, x, y-s, code=3, len=0.05, angle=90)
