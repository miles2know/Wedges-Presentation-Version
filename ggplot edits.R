
wedgesR <- sGhg[Res == 0 & Com == 0 & Ind == 0 & Trn == 0 & Elc == 0,]

projNghgM <- read.excel()

nghgD <- data.table(Sector = "Non-CO2", Year = projNghgM$Year, Ghg = projNghgM$ghgN)

datList <- list(a = wedgesR[, 6:8, with = FALSE ], b = nghgD)

wedgesR1 <- rbindlist(datList)

#secOrd <- c("Non-CO2","Commercial","Electricity","Industrial","Residential","Transportation")
#expDat <- wedgesR1[order(Year,match(Sector, secOrd)),]



expDat <- as.data.frame(wedgesR1)
expDat$Sector <- factor(expDat$Sector, levels(expDat$Sector)[c(6:1)])

expDat$Sector <- factor(expDat$Sector, levels = c("Non-CO2","Commercial","Electricity","Industrial","Residential","Transportation"))

df$letters = factor(df$letters,levels(df$letters)[c(4:1)])



gg <- ggplot(expDat, aes(x = Year, y = Ghg, group = Sector)) 
gg <- gg + stat_smooth(mapping = aes(fill = Sector), geom="area", position="stack", method="gam", formula = y~s(x)) 
gg <- gg + scale_fill_manual(values = colors1) 
gg <- gg + geom_line(data = lines1, aes(x = x, y = y),color = "red", size = 1.5)
gg <- gg + geom_hline(yintercept=127, color = "black", size = 1, linetype = 5 )
gg <- gg + scale_x_continuous(breaks=c(2013,2015,2020,2025,2030))
gg <- gg + scale_y_continuous(breaks=c(14,38,49,61,92,165))

gg <- gg + labs(x=NULL,y="Million Metric Tons", title = "GHG By Sector")
gg <- gg + theme_bw() + theme(legend.position="bottom") + guides(fill=guide_legend(title=NULL))

gg <- gg + theme(plot.title=element_text(face="bold", hjust=0))
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())

gg

colors <- c("#6EC400", "#5089E0", "#FF9C01", "#FFE78C", "#657383")

colors1 <- c("#6EC400", "#5089E0", "#FF9C01", "#FFE78C", "#657383", "#0000FF")


# how lines was originally created
lines1 <- ggplot_build(gg)$data[[1]] %>% filter(group == 6) %>% select(x,y) 
lines1$Sector <- factor("Total")

yScale <- ggplot_build(gg)$data[[1]] %>% filter(x == 2013) %>% select(y) %>% round(0)


####

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,as.is=TRUE,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}














