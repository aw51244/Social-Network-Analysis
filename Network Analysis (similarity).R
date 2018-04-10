library(igraph)
library(data.table)
require(gdata)

pd_kw_mx <- fread("production_keyword_matrix_1985.csv", header = TRUE)
dim(pd_kw_mx)

#  Question 1---------------------------------------------------
# find out all cliques in the network, 
# find out the largest clique who are most similar to each other
# find all posible pairs:
pairs <- as.data.table(t(combn(2:ncol(pd_kw_mx),2)))

## Jcard similarity
jaccard <- function(x){
un = sum(pd_kw_mx[,.SD, .SDcols=pairs$V1[x]]+pd_kw_mx[,.SD, .SDcols=pairs$V2[x]]!=0)
inter = sum(pd_kw_mx[,.SD, .SDcols=pairs$V1[x]]!=0 & pd_kw_mx[,.SD, .SDcols=pairs$V2[x]]!=0)
jcd = inter/un
return(jcd)
}

jcd_dst <- sapply(1:nrow(pairs), FUN = jaccard)
hist(jcd_dst,nclass = 100)
summary(jcd_dst)
jcd_max <- largest.cliques(graph.edgelist(as.matrix(pairs[which(jcd_dst==1),]),directed = F))
jcd_max <- as.numeric(jcd_max[[1]])
names(pd_kw_mx)[jcd_max]

## cosine similarity
cosine <- function(x){
numorater = sum(pd_kw_mx[,.SD, .SDcols=pairs$V1[x]]*pd_kw_mx[,.SD, .SDcols=pairs$V2[x]])
denominator = (sum((pd_kw_mx[,.SD, .SDcols=pairs$V1[x]])^2)^0.5)*(sum((pd_kw_mx[,.SD, .SDcols=pairs$V2[x]])^2)^0.5)
cos = numorater/denominator
return(cos)
}

cos_dst <-  sapply(1:nrow(pairs), FUN = cosine)
hist(cos_dst,nclass = 100)
summary(cos_dst)
cos_max <- max_cliques(graph.edgelist(as.matrix(pairs[which(cos_dst==1),]),directed = F))
cos_max <- as.numeric(cos_max[[1]])
names(pd_kw_mx)[cos_max]==names(pd_kw_mx)[jcd_max]
# the number of nodes with a jaccard similarity 1 and that of cosine similarity 1 are not same;
# The two distance consider different attribute, the cosine similarity takes quantitative measure
# into consideration.
# Nevertheless, the largest clique who are most similar to each other are the same.

# find total box for both films, and join with distance measures
dis_3y <- fread("film_distance_3year_window.csv", header = TRUE)
setkey(dis_3y,pindex1) 
Boff <- fread("box_office_films.csv", header = TRUE)
Boff1 <- Boff[,.(pindex1=pindex,box1=total_box)]
setkey(Boff1,pindex1) 
dis_3y <- dis_3y[!(pindex1%in%out|pindex2%in%out),]
join1 <- dis_3y[Boff1, nomatch=0]
Boff2 <- Boff[,.(pindex2=pindex,box2=total_box)]
setkey(Boff2,pindex2) 
setkey(join1,pindex2) 
join2 <- join1[Boff2, nomatch=0]

dis_dif <- join2[,.(dis=distance, dif=abs(box1-box2))]
# get correlation and plot
cor(dis_dif)
plot(dis_dif)
# It looks like as the distance inncrease, the difference between boxes also increase, 
# the correlation is not high, though.

# find top 250 keywords by total_box
film_pd <- fread("films_and_production_companies.csv", header = TRUE)
setkey(film_pd,pindex)
Boff3 <- Boff[,.(pindex,total_box)]
setkey(Boff3,pindex)
film_pd_box <- film_pd[Boff3, nomatch=0]
summary(film_pd_box$year)
film_box_10yr <- film_pd_box[year>max(year)-11,.(title=project, total_box)]
film_box_10yr <- film_box_10yr[,.(box=unique(total_box)), by = .(title)]
setkey(film_box_10yr,title)
film_kw <- fread("keywords_film.csv", header = TRUE)
setkey(film_kw,title)
kw_box <- film_kw[film_box_10yr, nomatch=0]
kw <- kw_box[,.(sum_box=sum(box)),by =keyword]
setorder(kw, -sum_box)
kw <- kw[1:250,keyword]

film_kw_10 <- film_kw[keyword %in% kw,]  # match kw to films
#film_pd_box_10 <- film_pd_box[year>max(year)-11 & project%in%film_kw_10$title,]

## label films with "only small/only big/colaborate"
lc1 <- film_pd_box[,.(sum_box=sum(total_box)), by = .(prod_company,year)]
lc2 <- lc1[,quantile(sum_box, .75), by  = year]
setkey(lc2,year)
setkey(lc1,year)
lc <- lc1[lc2]
lc[, big := (sum_box>=V1)]
lc[,V1 :=NULL]
fc <- film_pd_box[,.(title=project, prod_company,year)]
setkeyv(fc, c("prod_company","year"))
setkeyv(lc, c("prod_company","year"))
lc <- lc[fc]
lc <- lc[,.(perBig =sum(big)/.N), by = title]
lc[perBig==0, label := 1] # only small
lc[perBig>0 &perBig<1, label := 2] # colaborate
lc[perBig==1, label := 3] # only big

# label keywords with "mainly large/mainly small/mainly collaborate"
setkey(film_kw_10,title)
setkey(lc,title)
film_kw_10[lc, nomatch=0]
film_kw_lb <- film_kw_10[lc, nomatch=0][,perBig:=NULL] #########################
film_kw_lb[,maintype := which.max(c(sum(label==1),sum(label==2),sum(label==3))), by = keyword]
film_kw_lb[,unique(keyword),by =maintype][,.N,by =maintype]
# among 250, 210 appeared primarily in films made by large companies
# 7 by collaborations between the two
# 33 by small companies

# make a graph

#convert to edge list
film_kw_lb[,N:=.N, by = title]
elist <- film_kw_lb[N!=1, as.data.table(t(combn(keyword, 2))), by=.(title)]

#get graph
g <- graph.edgelist(as.matrix(elist[,2:3]),directed = F)
V(g)
film_kw_lb[,.N, by =.(keyword,maintype)]$maintype
types <-unique(film_kw_lb[,.(keyword,maintype)])$maintype
# set main type as vertex attribute
V(g)$types <- film_kw_lb[,.N, by =.(keyword,maintype)]$maintype
V(g)$N <- film_kw_lb[,.N, by =.(keyword,maintype)]$N

# set appear more often to weight
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = c(weight = "sum"))

colors = V(g)$types
colors[colors == "1"] = "yellow"
colors[colors == "2"] = "green"
colors[colors == "3"] = "blue"
plot(g, vertex.label=NA, 
     vertex.color=colors,
     vertex.size=V(g)$N/100,
     edge.width=E(g)$N/(10^9))
# in the plot, blue is mainly large companies and yellow is mainly small companies.
# edge width is sum of egdes, and vertex size is times of keyword appearing.
