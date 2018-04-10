# import all libraries
library(igraph)
library(data.table)
library(stats)
mydata = fread(file="sample_generated_network.csv", header = TRUE)
# Load in the data.
colnames(mydata) = gsub("To begin, please select your name.", "student_name", colnames(mydata))

# Divide the data into trust data and advice data.
TrustDT <- mydata[,1:(ncol(mydata) - 5)]
AdviceDT <- mydata[,-(2:(ncol(mydata) - 5))]

# Choose those participated in survey.
complete <- intersect(colnames(TrustDT), TrustDT[['student_name']])
TrustDT <- TrustDT[,.SD, .SDcols = c("student_name",paste0(complete))]

# Delete the students with incomplete records.
out <- c()
for (i in 1:nrow(TrustDT)){
  for (j in 1:ncol(TrustDT)){
  if ((TrustDT[i,.SD, .SDcol=j] == "") & !(names(TrustDT[,.SD, .SDcol=j]) %in% out))
    out <- append(out, names(TrustDT[,.SD, .SDcol=j]))
      }}

TrustDT[,(out) := NULL]
TrustDT <- TrustDT[student_name != out][order(student_name)]
DistrustDT <- TrustDT[student_name != out][order(student_name)]


## 1: Build the trust network.----------------------------------------------
# Make the choice data numeric.
column_wise_replace = function(DT, x, y) {
  for(i in seq_along(x)){
    for (j in seq_len(ncol(DT))){
      set(DT,which(DT[[j]] == x[i]),j,y[i])
    }
  }
}

scale_trust = cbind(c("Extremely Distrust  1",
                "Distrust  2",
                "Slightly Distrust  3",
                "Neither Distrust Nor Trust  4", 
                "Slightly Trust  5",
                "Trust  6", 
                "Extremely Trust  7", 
                "I don't know this person.", 
                "This is my own name."), 
              c(0, 0, 0, 0, 1, 2, 3, 0, 0))

column_wise_replace(TrustDT, scale_trust[,1], scale_trust[,2])

# make adjacency matrix
trusting = as.data.frame(TrustDT)
rownames(trusting) = trusting[,1]
trusting = trusting[,-1]

trusting = trusting[sort(rownames(trusting)),sort(colnames(trusting))]

# make the graph
trusting = graph.adjacency(as.matrix(trusting), "directed", weighted = TRUE)

plot.igraph(trusting,vertex.label=NA,
            layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color="black",
            edge.width=E(trusting)$weight, vertex.size =6, 
            edge.arrow.size=.3,edge.curved=FALSE)


## 2: Build the distrust network.----------------------------------------------
# Make the choice data numeric.
scale_distrust = cbind(c("Extremely Distrust  1",
                      "Distrust  2",
                      "Slightly Distrust  3",
                      "Neither Distrust Nor Trust  4", 
                      "Slightly Trust  5",
                      "Trust  6", 
                      "Extremely Trust  7", 
                      "I don't know this person.", 
                      "This is my own name."), 
                    c(3, 2, 1, 0, 0, 0, 0, 0, 0))

column_wise_replace(DistrustDT, scale_distrust[,1], scale_distrust[,2])

# make adjacency matrix
distrusting = as.data.frame(DistrustDT)
rownames(distrusting) = distrusting[,1]
distrusting = distrusting[,-1]

distrusting = distrusting[sort(rownames(distrusting)),sort(colnames(distrusting))]

# Cumpute the degree of distrusting
distrust_degree = colSums(sapply(distrusting,as.numeric))
rn <- max(distrust_degree)+1
distrust_degree = as.data.frame(distrust_degree)

# make the graph
distrusting = graph.adjacency(as.matrix(distrusting), "directed", weighted = TRUE)

for (i in  seq_along(rownames(distrust_degree))){
  V(distrusting)$color[i] = rainbow(rn, start = (1/2), end = (2/3))[distrust_degree[i,]+1]}
plot.igraph(distrusting,vertex.label=NA,
            layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color="black",
            edge.width=E(distrusting)$weight/4,vertex.size = 4, 
            edge.arrow.size=.1,edge.curved=FALSE)

## 3: Build the advice network.---------------------------------------------------
# prepare the data
AdviceDT = AdviceDT[order(student_name)]
advice_edges = rbind(AdviceDT[,.(student_name, advice=V38)],
                     AdviceDT[,.(student_name, advice=V39)],
                     AdviceDT[,.(student_name, advice=V40)],
                     AdviceDT[,.(student_name, advice=V41)],
                     AdviceDT[,.(student_name, advice=V42)])

# find out the most adviced student
trusted <- which.max(table(advice_edges$advice))

# make the graph
advice_seeking = graph.data.frame(advice_edges, directed = TRUE)

V(advice_seeking)$color = "light blue"
V(advice_seeking)$color[which.max(table(advice_edges$advice))] = "red"

plot.igraph(advice_seeking,vertex.label=NA,
            layout=layout.fruchterman.reingold, 
            vertex.label.color="black",
            edge.color="black",
            vertex.size = 6, 
            edge.arrow.size=.2,
            edge.curved=FALSE)

## 4: the number of reciprocated relationships.----------------------
# Build matrix
trusting_matrix = as.matrix(as_adjacency_matrix(trusting))
distrusting_matrix = as.matrix(as_adjacency_matrix(distrusting))
advice_matrix = as.matrix(as_adjacency_matrix(advice_seeking))

# count reciprocated trusting relationships
reciprocated = 0

for (i in 1:nrow(trusting_matrix)){
  for (j in 1:ncol(trusting_matrix)){
    if (trusting_matrix[i,j]*trusting_matrix[j,i]==1)
    reciprocated = reciprocated +1
  }
}
(reciprocated_trust = reciprocated/2)

# count reciprocated distrusting relationships
reciprocated = 0

for (i in 1:nrow(distrusting_matrix)){
  for (j in 1:ncol(distrusting_matrix)){
    if (distrusting_matrix[i,j]*distrusting_matrix[j,i]==1)
      reciprocated = reciprocated +1
  }
}
(reciprocated_distrust = reciprocated/2)

# count reciprocated advice relationships
reciprocated = 0

for (i in 1:nrow(advice_matrix)){
  for (j in 1:ncol(advice_matrix)){
    if (advice_matrix[i,j]==1 & advice_matrix[j,i]==1)
      reciprocated = reciprocated +1
  }
}
(reciprocated_advice = reciprocated/2)

## 5: Count overlap relationships-----------------------
# Delect imcomplete records from advice matrix:
advice_matrix = as.data.frame(advice_matrix)
advice_matrix <- advice_matrix[which(names(advice_matrix) %in% paste0(complete)),
                               which(names(advice_matrix) %in% paste0(complete))]
advice_matrix <- advice_matrix[-which(names(advice_matrix) %in% paste0(out)),
                               -which(names(advice_matrix) %in% paste0(out))]

# sort all matrix in same order
advice_matrix = advice_matrix[sort(rownames(advice_matrix)),
                              sort(colnames(advice_matrix))]
trusting_matrix = trusting_matrix[sort(rownames(trusting_matrix)),
                              sort(colnames(trusting_matrix))]
distrusting_matrix = distrusting_matrix[sort(rownames(distrusting_matrix)),
                              sort(colnames(distrusting_matrix))]

# find overlap between advice and trust
advice_trust =0
for (i in c(1:30)){
  for (j in c(1:30)){
    if (trusting_matrix[i,j]==1 & advice_matrix[i,j]==1)
      advice_trust = advice_trust +1
  }
}
advice_trust

# find overlap between advice and distrust
advice_distrust =0
for (i in c(1:30)){
  for (j in c(1:30)){
    if (distrusting_matrix[i,j]==1 & advice_matrix[i,j]==1)
      advice_distrust = advice_distrust +1
  }
}
advice_distrust
