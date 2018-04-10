rm(list = ls(all = TRUE))
library(sna)
library(data.table)
library(ergm)

# load the data
edges <- fread("startup_rounds_and_participants.csv",header = T)
# exclude duplicated records
edges <- unique(edges)

# get 1:n id for network
id <- unique(funion(edges[,.(id=participant_id)],edges[,.(id=startup_id)]))
r_id = seq(1,nrow(id),1)
nodes = cbind(r_id, id)

edges1 <- merge(edges,nodes,by.x = "participant_id", by.y = "id")
edges2 <- merge(edges1,nodes,by.x = "startup_id", by.y = "id")

# map the types of nodes to node list
id_type <- edges2[,.(p_type=unique(participant_type)), by = .(r_id =r_id.x)]
setkey(nodes,r_id)
setkey(id_type,r_id)
nodes <- id_type[nodes]
nodes[is.na(p_type), p_type:="Startup"] #if a node only exit in startup list, it is a startup

# convert to network
net_all=network(edges2[,7:8], multiple=TRUE) 
# the relation is participant--invest in-->startup


# assign edge-level attributes
set.edge.attribute(net_all, "round", edges2[,3])
set.edge.attribute(net_all, "date", edges2[,5])
set.edge.attribute(net_all, "traction", edges2[,6])

# assign node-level attributes
net_all %v% "type" = nodes[,2]
net_all %v% "ori_id" = nodes[,3]

# build first model (on all nodes)
m1=ergm(net_all ~ edges+mutual,burnin=15000,MCMCsamplesize=30000,verbose=T)
summary(m1)

# baseline of the probability of a tie existing
plogis(coef(m1)[['edges']]) #0.01862633

#given an i -> j tie, the probability of a j -> i tie
plogis(coef(m1)[['edges']] + coef(m1)[['mutual']]) #0.02699744 higher than baseline

# build the second model (on only startups)
net_start <- net_all
net_start <- delete.vertices(net_start,nodes[p_type=="User",r_id])
m2=ergm(net_start ~ edges+mutual,burnin=15000,MCMCsamplesize=30000,verbose=FALSE)
summary(m2)

# baseline of the probability of a tie existing
plogis(coef(m2)[['edges']]) #0.02904437 higher than base of m1

#given an i -> j tie, the probability of a j -> i tie
plogis(coef(m2)[['edges']] + coef(m2)[['mutual']]) 
#0.05419926 much higher than baseline

# I think the difference comes from the eliminating of "user" nodes, 
# because type "user" can not have mutural relationship in this network.

# This question asks how the outdegree (invest more) influence the probability of a relationship exists.
# So we need to add the term "gwodegree" in ERGM model.
m3=ergm(net_start ~ edges + mutual + gwodegree(decay = 1),burnin=15000, MCMCsamplesize=30000,verbose=T)
summary(m3)
# The coefficient of "gwodegree" is -0.7192, means that more investment actually
# leads to less probability of a relationship exists.

# compute the time # of invest
edge_13 <- edges[, year:=substr(round_record_date,1,4)][year>2012]
edge_12 <-edges[year==2012,.(startup_id,participant_id,marker=year)] #leave year as marker

# Which edges in 2013 had happened in 2012
setkey(edge_13,startup_id,participant_id)
setkey(edge_12,startup_id,participant_id)
edge_13 <- edge_12[edge_13, nomatch=NA]
edge_13[is.na(marker), happened:=0]
edge_13[!is.na(marker), happened:=1]
edge_13[,marker:=NULL]

# get 1:n id for network
id <- unique(funion(edge_13[,.(id=participant_id)],edge_13[,.(id=startup_id)]))
r_id = seq(1,nrow(id),1)
nodes = cbind(r_id, id)

edge1_13 <- merge(edge_13,nodes,by.x = "participant_id", by.y = "id")
edge2_13 <- merge(edge1_13,nodes,by.x = "startup_id", by.y = "id")

# convert to network
net_13=network(edge2_13[,9:10], multiple=TRUE)
# the relation is participant--invest in-->startup

# build the happend network 
happened<-as.network.matrix(edge2_13[happened==1,9:10])

# build the model (on all nodes)
m4=ergm(net_13 ~ edges+mutual+edgecov(happened),burnin=15000,MCMCsamplesize=30000,verbose=F)
summary(m4)
# Warning: The following terms have infinite coefficient estimates:
# edgecov.happened 

# Well, at least we get a coefficient greater than 1 and very big, 
# I think that means that an investor is more likely to participate
# in a startup’s round if it has participated in in one of its rounds 
# in the previous year

# delet User type of participants
edge_13 <- edge_13[participant_type=="Startup"]
edge_13

# get 1:n id for network
id <- unique(funion(edge_13[,.(id=participant_id)],edge_13[,.(id=startup_id)]))
r_id = seq(1,nrow(id),1)
nodes = cbind(r_id, id)
setkey(nodes,id)

edge1_13 <- merge(edge_13,nodes,by.x = "participant_id", by.y = "id")
edge2_13 <- merge(edge1_13,nodes,by.x = "startup_id", by.y = "id")

# compute total traction for startups, this should include traction from both startups and users
edges[year==2014, year:=2013] # treat 2014 as 2013
total12 <- edges[year==2012,.(total12=sum(traction)),by = startup_id]
total13 <- edges[year==2013,.(total13=sum(traction)),by = startup_id]
setkey(total12,startup_id)
setkey(total13,startup_id)

# map the total traction of nodes to node list
nodes <- total12[nodes]
setkey(nodes,startup_id)
nodes <- total13[nodes]
# set NA to 0
nodes[is.na(total12),total12:=0]
nodes[is.na(total13),total13:=0]

# convert to network
net_13=network(edge2_13[,9:10], multiple=TRUE)
# the relation is participant--invest in-->startup

# assign node-level attributes
net_13 %v% "total12" = nodes[,3]
net_13 %v% "total13" = nodes[,2]

# build the previous-year model (on all nodes)
m5=ergm(net_13 ~ edges+mutual+absdiff("total12"),burnin=15000,MCMCsamplesize=30000,verbose=F)
summary(m5)
# the coefficient of absdiff.total12 is -1.624e-11, which indicates that
# a startup is slightly more likely to have participants in 2013 rounds 
# that have gained similar levels of traction in 2012

# build the current-year model (on all nodes)
m6=ergm(net_13 ~ edges+mutual+absdiff("total13"),burnin=15000,MCMCsamplesize=30000,verbose=F)
summary(m6)
# the coefficient of absdiff.total13 is -9.968e-14, which indicates that
# a startup is slightlymore likely to have participants in 2013 rounds 
# that have gained similar levels of traction in the same year.

# diagnose model performance
mcmc.diagnostics(m1)
mcmc.diagnostics(m2)
mcmc.diagnostics(m3)
mcmc.diagnostics(m4)
mcmc.diagnostics(m5)
mcmc.diagnostics(m6)
# Based on the mcmc.diagnostics, m2 has the best performance. 
# the chains thoroughly explore the parameter space and 
# don’t wander over the course of the simulation.

#----- for below parts, only interesting plots are saved.
#Try simulate on m1
sim_nets = simulate(m1, nsim = 4)
lapply(sim_nets, plot)
plot(net_all)

#Try simulate on m2
sim_nets = simulate(m2, nsim = 4)
lapply(sim_nets, plot)
plot(net_start)
# This 4 network look similar, but they look somewhat different from the original plot.

#Try goodness-of-fit on m1
m1_gof = gof(m1)
plot(m1_gof)

#Try goodness-of-fit on m2
m2_gof = gof(m2)
par(mfrow = c(2, 3))
plot(m2_gof)


#Try goodness-of-fit on m3
m3_gof = gof(m3)
par(mfrow = c(2, 3))
plot(m3_gof)

#Try goodness-of-fit on m4
m4_gof = gof(m4)
par(mfrow = c(2, 3))
plot(m4_gof) # the gof goes infinity for “happened”

#Try goodness-of-fit on m5
m5_gof = gof(m5)
par(mfrow = c(2, 3))
plot(m5_gof)

#Try goodness-of-fit on m6
m6_gof = gof(m6)
par(mfrow = c(2, 3))
plot(m6_gof)
