source("RandomNetworksFuns2.r")
#Random nxn matrix for timing
n=30
A=mat.or.vec(n, n)
numNETS = 20;

Results=gibbsSamplerVerbose(3000,100,100,A);#The target coefficients and clustering are Hardcoded!
options(scipen=999)
Y=t(Results$Res)
plot(Y[,3])#Hamiltonian
hist(Y[,1])#Mean Degree
hist(Y[,2])#Clustering coefficient.

#E X P E R I M E N T O:
source("RandomNetworksFuns2.r")

n=30
#We start from a disjoint set of cliques of size k
clusterList = homogClusters(8,n); #We want clusters of size 10 in 200 vertices
A=initializeClusterGraph(clusterList, n); #Corresponding graph
Results=gibbsSamplerVerbose(100,100,100,A);
Results=gibbsSamplerVerbose(100,500,1000,Results$A);
Results=gibbsSamplerVerbose(100,500,10000,Results$A);#Vale la pena ir viendo como va el clustering coefficient (cuando llege a 0.25 se puede poner el segundo coeficiente muy muy alto y eso lo fija)
computeDegNC(A)

#Para grabar los resultados.
source("RandomNetworksFuns2.r")

#################################################
#We start from a disjoint set of cliques of size k
#################################################
#homogClusters=function(k,n)
##################################################

clusterList = homogClusters(8,n); #We want clusters of size 10 in 200 vertices

A=initializeClusterGraph(clusterList, n); #Corresponding graph

##################################################
#gibbsSampler=function(iterations,beta1,beta2,A)
##################################################

Results=gibbsSamplerVerbose(100,20,100,A);
Results=gibbsSamplerVerbose(100,20,1000,Results$A);
Results=gibbsSamplerVerbose(100,20,10000,Results$A);#Vale la pena ir viendo como va el clustering coefficient (cuando llege a 0.25 se puede poner el segundo coeficiente muy muy alto y eso lo fija)
computeDegNC(A)

#Para grabar los resultados.
#a number of "numNETS" networks are created. Networks are represented as matrices. L [[X]] represents the "X" network stored in "L"
L=c();
for (k in seq(1:numNETS)) {
	print(k)
	Results=gibbsSamplerVerbose(300,5,20000,Results$A);
	L=c(L,list(Results$A))	
}
save(L,file="Matrices30_test.RData")


