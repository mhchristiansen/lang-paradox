######################################################
meanDegree = 0.677 #hard coded
clusteringCoefficient = 0.25 #hard coded

######################################################

#Produces a list of neighbors of a vertex.
neighbors=function(i,A){
	Res=c();
	for (j in seq(1,ncol(A))) {
		if(A[i,j]==1){Res=c(Res,j);};
	};
	return(Res)
}

#Computes the average degree and the clustering coefficient of a given network
computeDegNC=function(A){
	n=nrow(A);
	deg=mat.or.vec(n,1);
	NC=mat.or.vec(n,1);
	for (i in seq(1,nrow(A))){
		for(j in seq(1,ncol(A))) {
			deg[i]=deg[i]+A[i,j];
		}
	}
	for (i in seq(1,nrow(A))) {
		NCValue=0;
		NB=neighbors(i,A);
		for (k1 in NB) {
			for(k2 in NB) {
				if (A[k1,k2]==1){ NCValue=NCValue+1;}
			}
			NC[i]=(NCValue/2.0);
			if (NC[i]!=0) { NC[i]=(2.0*NC[i])/(deg[i]*(deg[i]-1))}							
		}		
	}
	return(list(deg=mean(deg),NC=mean(NC)))
}

#Given a list computes the degree and clustering coefficient in the networks
params=function(L) {
	MeanNodalDegs=c()
	CC=c()
	for(l in L){
		ResP=computeDegNC(l);
		MeanNodalDegs=c(MeanNodalDegs,ResP$deg);	
		CC=c(CC,ResP$NC);
	}
	return(data.frame(MeanNodalDegs,CC))
}



#Computes the hamiltonian (i.e. the probability is proportional to e-((beta)*hamiltonian(A))
hamiltonian = function(beta1,beta2,A){
	n=nrow(A);
	TargetMeanDegree=n^(meanDegree);#Hard-coded
	TargetClusteringCoeff=clusteringCoefficient;#Hard-coded
	S=computeDegNC(A);
	return(beta1*(S$deg-TargetMeanDegree)^2+beta2*(S$NC-TargetClusteringCoeff)^2)
}

hamiltonianVerbose = function(beta1,beta2,A){
	n=nrow(A);
	TargetMeanDegree=n^(meanDegree);#Hard-coded
	#TargetMeanDegree=2;#Hard-coded
	TargetClusteringCoeff=clusteringCoefficient;#Hard-coded
	S=computeDegNC(A);
	return(list(deg=S$deg,NC=S$NC, ham=beta1*(S$deg-TargetMeanDegree)^2+beta2*(S$NC-TargetClusteringCoeff)^2))
}

#GibbsSampler
gibbsSampler=function(iterations,beta1,beta2,A) {
	n=nrow(A);	
	U=runif(iterations, min=0, max=1);
	E1=sample(1:n,iterations,replace=T); #sources
	E2=sample(1:n,iterations,replace=T);
	for(ctr in seq(1:iterations)) {
		Choice = 0; #No decision has been made in this round
		H0=hamiltonian(beta1, beta2, A); #Current Hamiltonian
		u=U[ctr];
		sv=E1[ctr];
		tv=E2[ctr];
		if(sv!=tv){		
		if(A[sv,tv]==1 && Choice==0) {
			Choice=1;
			A[sv,tv]=0;
			A[tv,sv]=0;
		    H1=hamiltonian(beta1, beta2, A);	
			if(log(u)>H0-H1) { 			
				#if Rejection then go back to the previous status
				#print("Rejection\n")					
				A[sv,tv]=1;
				A[tv,sv]=1;
				} ;
		}
		if(A[sv,tv]==0 && Choice==0) {
			Choice=1;
			A[sv,tv]=1;
			A[tv,sv]=1;
		    H1=hamiltonian(beta1, beta2, A);	
			if(log(u)>H0-H1) { 			
				#if Rejection then go back to the previous status
				#print("Rejection\n")					
				A[sv,tv]=0;
				A[tv,sv]=0;
				} ;
		}
		}
	}
	return(A)
}

#GibbsSamplerVerbose returns matrix whose columns are mean degree, clustering coeff, value of hamiltonian
gibbsSamplerVerbose=function(iterations,beta1,beta2,A) {
	Res=c();
	n=nrow(A);	
	U=runif(iterations, min=0, max=1);
	E1=sample(1:n,iterations,replace=T); #sources of edges to modify
	E2=sample(1:n,iterations,replace=T); #targets of edges to modify
	for(ctr in seq(1:iterations)) {
		if(ctr%%10==0) { print(ctr);};
		Choice = 0; #No decision has been made in this round
		H0=hamiltonianVerbose(beta1, beta2, A); #Current Hamiltonian
		Res=cbind(Res, c(H0$deg,H0$NC,H0$ham));
		H0=H0$ham;
		u=U[ctr];
		sv=E1[ctr];
		tv=E2[ctr];
		if(sv!=tv){		
		if(A[sv,tv]==1 && Choice==0) {
			Choice=1;
			A[sv,tv]=0;
			A[tv,sv]=0;
		    H1=hamiltonian(beta1, beta2, A);	
			if(log(u)>H0-H1) { 			
				#if Rejection then go back to the previous status
				#print("Rejection\n")					
				A[sv,tv]=1;
				A[tv,sv]=1;
				} ;
		}
		if(A[sv,tv]==0 && Choice==0) {
			Choice=1;
			A[sv,tv]=1;
			A[tv,sv]=1;
		    H1=hamiltonian(beta1, beta2, A);	
			if(log(u)>H0-H1) { 			
				#if Rejection then go back to the previous status
				#print("Rejection\n")					
				A[sv,tv]=0;
				A[tv,sv]=0;
				} ;
		}
		}
		}
	return(list(Res=Res,A=A));
}

#Initialization routine. We begin with a partition of the n vertices into cliques of sizes specified by a list.
#We use the R commands rep(sequence, times) and seq(from, to, by=)
initializeClusterGraph=function(partitionSequence, n) {
	A=mat.or.vec(n, n)
	StV=1;
	EnV=1;
	for(part in partitionSequence){
		EnV=(StV+part)-1;
		for(j1 in seq(StV,EnV)){
			for(j2 in seq(StV,EnV)) {
				if(j1!=j2) { A[j1,j2]=1;};
			};
		};
		StV=EnV+1;	
	}
	return(A)	
}
#Construction of the initial cluster size list
homogClusters=function(k,n){
	return(rep(k,floor(n/k)));
}

