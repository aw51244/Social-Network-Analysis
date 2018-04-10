# Social-Network-Analysis

This repository contains R codes for my student project for the course Social Network Analysis.  
There are **five** main projects:  
  
**Build Network:**  
--| Data: MBA student survey about trusting and advice    
--| Tasks:    
----| extract conjuction matrix of a trusting network, a distrusting network, and a advice-seeking network.  
----| build and plot networks based on conjuction matrix.  
----| find overlap between different networks.  
----| find reciprocated relations between vertices.  

**Network Analysis (vertices and deges):**   
--| Data: teamwork and social interaction data of high-shcool students  
--| Tasks:   
----| comput centrality measures (indegree, outdegree, undirected closeness, betweenness, and eigenvector centrality) for each individual vertice.  
----| merge two networks, find out correlations between different statistics for each individual vertice between different networks.  
----| Define strong/weak ties between vertices based on interation frequency, calculate the edge-level betweeness.  

**Network Analysis (network structure):**   
--| Data: Funding Events between VCs and Start-ups  
--| Tasks:  
----| clease data, extract an edge list from funding events records, build the network from it.    
----| based on centrality measures, find out which VC is the center of the network as of July 2014.  
----| filter the data based on the time of relationships(edges) starting.  
----| measure the structure of the network based on its K-core value, visualize its core-periphery structure.  
----| Find out investment growth trend based on average K-core value.  
  
**Network Analysis (similarity):**   
--| Data: Movie, box, producers, and key words    
--| Tasks:  
----| clease data, build the network of movies based on shared producers and key words.     
----| Find out the most similar movies with different similarity measures (Jaccard, cosine).   
----| Find out corralation between movie box and the similarity measures.  
----| Find out most popualr key-words by total box.  
----| Segment movie based on different egdes, explore collaboration trend between big and small movie companies.  
  
**Prediction Models based on Network:**    
--| Data: Funding Events between VCs and Start-ups     
--| Tasks:  
----| Predict the probability of a VC invest in a start-up.     
----| Find out factors influencing the investment decision.  
----| Tuning model based on goodness-of-fit.   
