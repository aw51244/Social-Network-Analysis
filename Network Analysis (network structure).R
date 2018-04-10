require(data.table)
require(igraph)
# load data
data = rbind(fread(file="Funding_events_7.14.csv", header = T), 
             fread(file="Funding_events_7.14_page2.csv", header = T))

# get rid of confusion words
data <- data[,c(1,4,11)]
data$Investors = gsub("Inc.", "", data$Investors)
data$Investors = gsub("Inc", "", data$Investors)
data$Investors = gsub("Ltd.", "", data$Investors)
data$Investors = gsub("Ltd", "", data$Investors)
data$Investors = gsub("LTD", "", data$Investors)
data$Investors = gsub("LLC", "", data$Investors)

# get date
data$`Deal Date` = as.Date(data$`Deal Date`, format="%m/%d/%y")

# clean data
names(data) <- c("PCN", "Date", "Inverstors")
data[data$Inverstors==""] <- NA
data = na.omit(data)

#get edge list
library(stringr)
data <- cbind(data[,1:2],str_split_fixed(data$Inverstors, ", ", n=Inf))
data <- melt.data.table(data, id.vars = c("Date","PCN"))
data[,variable := NULL]

# clean data
data[data$value==""] <- NA
data[data$value==" "] <- NA
data[data$value=="  "] <- NA
data = na.omit(data)
dim(data)

# get rid of single nodes
data <- data[,.(.N,value),.(Date,PCN)]
single <- data[N==1,]

data <- data[N!=1, as.data.table(t(combn(value, 2))), .(Date,PCN)]
dim(data)
singlenodes <- setdiff(unique(single$value),union(unique(data$V1),unique(data$V2)))
single <-single[value %in% singlenodes,]

# get a graph contains all egdes
g_all <- graph.edgelist(as.matrix(data[,3:4]),directed = F)

# set time as attributes
set_edge_attr(g_all, "time", index = E(g_all), value = data$Date)

# Question 1 -------------------------------------
# get July 2014 graph
g_q1 <- graph.edgelist(as.matrix(data[data$Date<"2014-08-01",2:3]),directed = F)
g_q1 <- simplify(g_q1, remove.multiple = TRUE, remove.loops = TRUE)

# find degree
summary(degree(g_q1)) #Mean =4.418;Median =2
which.max(degree(g_q1)) 
# Intel Capital is the center of the network as of July 2014

# find the average shortest path length between all firms
mean_distance(g_q1) 
# 5.731115  I think uess the companies are not really well connected by then.


# Question 2---------------------------------------
# this function takes a date, and return the mean coreness of thar month.
month_g <- function(ym){
  edge_l <-data[Date<=ym,.(V1,V2)]
  maxk <- mean(coreness(graph.edgelist(as.matrix(unique(edge_l)),directed = F)))
  return(maxk)
}

# find a time list
tdata <- data[,.(Date,V1,V2,Year = year(Date),Month = month(Date))]
tdata <- tdata[, .(stamp = as.Date(min(Date))), by =.(Year, Month)][order(Year,Month)]

# find the K-core value
avg_coreness <- sapply(X=tdata$stamp,FUN = month_g)

plot(y = avg_coreness, x =tdata$stamp, type = "l")

# Question 3---------------------------------------------
# define network to include all 10-year relationships
month_g10 <- function(ym){
  edge_l <-data[(Date>(ym-3650)&(Date<ym)),.(V1,V2)]
  maxk <- mean(coreness(graph.edgelist(as.matrix(unique(edge_l)),directed = F)))
  return(maxk)
}

# find the K-core value
avg_coreness10 <- sapply(X=tdata$stamp,FUN = month_g10)
plot(y = avg_coreness10, x =tdata$stamp, type = "l")
# The two plots show similiar trend, expect the far right side, in recent yaers, 
# if we consider the fade ofrelationship, the coreness will not keep growing forever.
# ----------------------------------

# get adjacency matrix from graph
g_q4 <- graph.edgelist(as.matrix(unique(data[data$Date<"1991-07-01",3:4])),
                       directed = F)
g_q4 <- simplify(g_q4, remove.multiple = TRUE, remove.loops = TRUE)
adj_q4 <- as.matrix(get.adjacency(g_q4))

# iterative correlation clustering method
concor = list()
concor[[1]] = adj_q4

for(i in 2:9){
  concor[[i]] = cor(concor[[i - 1]])
}
concor[[9]][concor[[9]] < 0] = 0
concor_net = graph.adjacency(as.matrix.network(network(concor[[9]])), "undirected")
plot(g_q4)
plot(concor_net)

# Question 5 -------------------------------------------
# From the plot of Question 4, we can tell that the network between 
# companies started from a clustered/component structure, but then by the 
# average-coreness by month plot we can tell that the coreness really 
# starts to pick up around 2000, by which the network must had begun to 
# grow toward a a core-periphery structure.
