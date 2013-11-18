# plots.R - DESC
# plots.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

library(FLAdvice)
library(ggplotFL)
library(plyr)
library(maps)
library(mapproj)

data(ple4)

ple4sr <- fmle(as.FLSR(ple4,model="bevholt"))

ple4br <- FLBRP(ple4, sr=ple4sr)

### create the future
ple4 <- window(ple4, end=2018, FLBRP=ple4br)

f <- FLQuant(rlnorm(1000,log(.35),.2),dimnames=list(year=2009:2018,iter=1:100))

### project
ple4 <- fwd(ple4, f=f, sr=ple4sr)
dat <- ssb(ple4)

# 1. {{{
p <- ggplot(data = as.data.frame(window(dat, end=2008)),
  mapping = aes (x = year, y = data)) +
  ylab("SSB") + xlab("") +
  stat_summary (fun.y = mean, geom="line", mapping = aes (group = 1), size=1) +
  theme_bw() + opts(legend.position="none") + 
  scale_x_continuous(limits = c(1957, 2020)) + scale_y_continuous(limits = c(0, 5e5))
cairo_ps(file="obj_1.eps")
print(p)
dev.off()
# }}}

# 2. {{{
p <- p + geom_hline(aes(yintercept = 350000), linetype=2, colour="blue")
cairo_ps(file="obj_2.eps")
print(p)
dev.off()
# }}}

# 3. {{{
p <- p + geom_hline(aes(yintercept = 150000), linetype=3, colour="red")
cairo_ps(file="obj_3.eps")
print(p)
dev.off()
# }}}

# 4. {{{
p <- ggplot(data = as.data.frame(dat),
  mapping = aes (x = year, y = data)) +
  ylab("SSB") + xlab("") +
  stat_summary (fun.y = mean, geom="line", mapping = aes (group = 1), size=1) +
  theme_bw() + opts(legend.position="none") + 
  geom_hline(aes(yintercept = 350000), linetype=2, colour="blue") +
  geom_hline(aes(yintercept = 150000), linetype=3, colour="red") +
  scale_x_continuous(limits = c(1957, 2020)) + scale_y_continuous(limits = c(0, 5e5))

cairo_ps(file="obj_4.eps")
print(p)
dev.off()
# }}}

# 5. {{{
qu1 <- adply(dat, 2, quantile, probs=c(0.05,0.5,0.95))
names(qu1) <- c("year","a","b","c")
qu2 <- adply(dat, 2, quantile, probs=c(0.25,0.5,0.75))
names(qu2) <- c("year","a","b","c")
qu3 <- adply(dat, 2, quantile, probs=c(0.40,0.5,0.60))
names(qu3) <- c("year","a","b","c")

p <- p + 
  geom_ribbon(aes(x=year, ymin=qu1$a, ymax=qu1$c), fill="red", alpha=0.1) +
  geom_ribbon(aes(x=year, ymin=qu2$a, ymax=qu2$c), fill="red", alpha=0.15) + 
  geom_ribbon(aes(x=year, ymin=qu3$a, ymax=qu3$c), fill="red", alpha=0.20) +
  stat_summary (fun.y = min, geom="line", mapping = aes (group = 1),
    color="red",size=0.1) +
  stat_summary (fun.y = max, geom="line", mapping = aes (group = 1),
    color="red", size=0.1)

cairo_ps(file="obj_5.eps")
print(p)
dev.off()
# }}}

# 6. TODO IO TACs {{{
dat <- as.data.frame(FLQuants(Yield=catch(ple4), F=harvest(ple4), B=ssb(ple4)))

p <- ggplot(data = dat, mapping = aes (x = year, y = data)) +
  ylab("") + xlab("") +
  stat_summary (fun.y = mean, geom="line", mapping = aes (group = 1), size=1) +
  theme_bw() + opts(legend.position="none") + 
  facet_grid(qname ~ ., scales="free")

# 
obj <- data.frame(z = c(150000, 0.35, 350000), qname = c("Yield", "F", "B"))
p <- p + geom_hline(aes(yintercept = z), obj,  linetype=2, colour="blue")

#
lim <- data.frame(z = c(25000, 0.6, 150000), qname = c("Yield", "F", "B"))
p <- p + geom_hline(aes(yintercept = z), lim,  linetype=3, colour="red")

cairo_ps(file="obj_6.eps")
print(p)
dev.off()
# }}}

# Closed area {{{
lon <- c(30, 130)
lat <- c(-35, 35)

map("world", interior=FALSE, xlim=lon, ylim=lat)

polygon(c(40, 60, 60, 40), c(0, 0, 10, 10), col='lightblue')

map.grid(c(lon, lat), labels=FALSE, col='gray', lty=1)

map("world", interior=FALSE, xlim=lon, ylim=lat, fill=TRUE, col=c('gray'), add=TRUE)

map.axes()

cairo_ps(file="map_IO_closedarea.eps")

dev.off()
# }}}

# 7. MSE dashboard {{{
nc <- read.csv('/home/imosqueira/Life/Library/Data/IOTC/NCDB_20110822.csv')

yft <- tapply(as.numeric(nc$YFT), list(gear=nc$GearGroup, year=nc$Year), FUN=sum)
yft <- as.data.frame(t(yft))
yft$NC <- apply(yft, 1, sum)
yft$Year <- as.numeric(rownames(yft))

p <- ggplot(data = yft, aes (x = Year, y = NC)) +
  geom_line() + ylab("SSB") + xlab("") +
  theme_bw() + opts(legend.position="none")

Layout <- grid.layout(nrow = 5, ncol = 10,
     widths = unit(c(2, 2, 2), c("null", "null",
         "null")), heights = unit(c(1, 1, 1,
         1, 1.5), c("null", "null", "null")))
grid.show.layout(Layout)
 
vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}
subplot <- function(x, y) viewport(layout.pos.row = x,
  layout.pos.col = y)

foo <- function() {
vplayout()

print(p, vp = subplot(1, 1:2))
print(p, vp = subplot(1, 3:4))
}

foo()

# }}}
