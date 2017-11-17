###################################################
# this is the Exec file for horizontal transmission
###################################################

load ("Matrices500.rdata"); #loading the network for population size X in MatricesX.rdata

#----------------------------------------------------------------
#parameter setting
#----------------------------------------------------------------

nnumber= 31; #network number in networks¥ file L (in file MatricesX.rdata)

ag=500; #population size (number of agents)

thresEasy = 3; #times an agent needs to encounter an easy convention to learn it

thresHard = 4; #times an agent needs to encounter a hard convention to learn it

iterations =1000; #number of iterations
 
parameterActive = (0.1)*ag; # 10% of population size

doff=500; #for each of the M convention tokens stored, there is a  probability p = 1/doff that this token is 'forgotten.'

numSeed =5; # random seed

set.seed(numSeed);

#----------------------------------------------------------------
# definitions
#---------------------------------------------------------------

conAg= matrix(0,ag,ag); #matrix of agent connectivity

neigMat= matrix(0,ag,ag); #matrix of agent¥s neighbors

mesa= c(); #vector for conventions [i]. Dynamically growing list
 
mesaIndice= c(); # the hard/easy index of each convention [i]
		
wUttered= mat.or.vec(ag,1);#number of tokens(costumers) for agent¥s active conventions

visionMesa=c(); #matrix (columns: number of agents (j). Rows (i) grow dynamically as number of conventions grow. Value (i,j) = 1 if convention i is understood by agent j and 0 if it is not.

wHeardReservoir=c(); #number of times that an agent has been exposed to convention [i] in the past

forSample=c();# a list of lists for sample uniformelly where the first index is the agent and the second index is each convention been uttered by that agent. List grows dynamically

propC = c(); #matrix [i,j] proportion of neighbors sharing convention i per agent j

activeTables=c(); #conventions shared by 10% of population

nC=c(); #succesful conventions

tempL = c(); #provisional list

#---------------------------------------------------------
# inicializing
#---------------------------------------------------------


iniMesa = 0; #defines how HARD/EASY index are initialized to new conventions. If iniMesa=0, then HARD/EASY index is randomly assigned to newborn conventions. If iniMesa >0 , then newborn conventions are initialized to iniMesa value.

numEffectiveTables = 0; #number of successful conventions, that is, conventions undestood by at least one neighbor

distEffectiveTables = c(); # this is the distribition of convention use, that is: the vector "mesa" filtered by successful conventions

iter= iterations*ag; #iterations times the number of agents

k=ag;


#----------------------------------------------------------
# network connectivity
#----------------------------------------------------------

conAg = L[[nnumber]];

agentNeig = list(mat.or.vec(ag,1));

cont=1;

for (i in seq(ag))
	{
	for (j in seq(ag))
	{
		if ((conAg[i,j]==1))
		{
			neigMat[i,cont]=j;
			cont=cont+1;
		}
	}

agentNeig[i] = cont-1;
cont=1;
	}

#---------------------------------------------------------------
#initializing
#---------------------------------------------------------------
for(s in seq(ag))
{

#---------------------------------------------------------------
#Initializing forSample
#---------------------------------------------------------------

forSample=c(forSample,list(s));

#---------------------------------------
#inicialize visionMesa   
#-------------------------------------
visionMesa = c(visionMesa, list(mat.or.vec(ag,1)));
visionMesa[[s]][s]=1;
#---------------------------------------
#inicialize propC
#-------------------------------------
propC = c(propC, list(mat.or.vec(ag,1)));
propC[[s]][s]=0;
nC = c(nC, list(mat.or.vec(ag,1)));
nC[[s]][s]=0;
#---------------------------------------
#inicialize wWordReservoir
#-------------------------------------
wHeardReservoir = c(wHeardReservoir, list(mat.or.vec(ag,1)));
wHeardReservoir[[s]][s]=1;
#---------------------------------------
#inicialize mesa y mesaIndice
#-------------------------------------

mesa = c(mesa, list(mat.or.vec(1,1)));
mesa[[s]][1]=1;

if(iniMesa>0)
{
mesaIndice = c(mesaIndice, list(mat.or.vec(1,1)));
mesaIndice[[s]][1]= iniMesa;
}
if(iniMesa==0)
{
randomnum= sample(1:2, 1);
mesaIndice = c(mesaIndice, list(mat.or.vec(1,1)));
mesaIndice[[s]][1]= randomnum;
}

#---------------------------------------
#inicialize  wUttered
#-------------------------------------

wUttered[s] = wUttered[s]+1;

#---------------------------------------
#inicialize DistEffectiveTables   
#-------------------------------------

distEffectiveTables = c(distEffectiveTables, list(mat.or.vec(1,1)));
distEffectiveTables[[s]][1]=0;


}


#------------------------------------------------------------
# generation LOOP
#-------------------------------------------------------------

for (n in seq(iter))
{

#sampling an agent
	nagent=((n-1)%%ag) +1 ; #asigna el n˙mero de agente que corresponda.

#samplig a convention	
stopper=0;
while (stopper==0)
{
	u <- runif(1,1,wUttered[nagent]+2);	
	u=floor(u);

     #sampling a convention
  if (u < (wUttered[nagent]+1))
{
nword=forSample[[nagent]][[u]];
vision=visionMesa[[nword]][nagent];
if(vision==1)
{stopper=1;}
}
if (u==(wUttered[nagent]+1))
{
	k=k+1;
	nword= k;
stopper=1;}

}

# sample neighbor for communication
xxx= agentNeig[[nagent]];
neig <- runif(1,1,xxx+1);	
neig=floor(neig);

neighbor = neigMat[nagent,neig];

FixUttered = wUttered[nagent]+1; #this list stores the state of convention at the moment of sampling a neighbor

forgottenCount = 0; #counts the number of forgotten convetions for the sampled agent

	for (nn in 1:wUttered[nagent])
     {
	uuu=sample(1:doff, 1)	
     if (uuu==1)
	{
     forgottenCount = forgottenCount +1;
    }
}

forgottenCountNEIG = 0; #counts the number of forgotten convetions for the sampled neighbor

	for (nn in 1:wUttered[neighbor])
     {
	uuu2 <- runif(1,1,doff+1);
	uuu2=floor(uuu2);
     if (uuu2==1)
	{
     forgottenCountNEIG = forgottenCountNEIG +1;
      }
    
}


#################################################################
#If convention sampled is NOT new 
#################################################################

	if (u < FixUttered)
	{

#--------------------------------------------
#for agents
#--------------------------------------------

	#updates forSample for agent
	nword=forSample[[nagent]][[u]];
	mesa[[nword]][1]= mesa[[nword]][1]+1;
	wHeardReservoir[[nword]][nagent]= wHeardReservoir[[nword]][nagent]+1;

	if (forgottenCount == 0) #if no token is forgotten
	{
	tempL= forSample[[nagent]];
	tempL= c(tempL,nword);
	forSample[[nagent]]=tempL;
	tempL=c();

     #updates agent¥s heard/known conventions
	wUttered[nagent]= wUttered[nagent]+1;
	}

if (forgottenCount > 0) # Poisson forgetting.
	{
	uu <- runif(1,1,wUttered[nagent]+1);	
	uu=floor(uu);
      nwordOld= forSample[[nagent]][[uu]];	
	forSample[[nagent]][[uu]]=nword;
  
	temporal= wHeardReservoir[[nwordOld]][nagent];
	if (temporal>0)
   {wHeardReservoir[[nwordOld]][nagent]= wHeardReservoir[[nwordOld]][nagent]-1;}
	
   forgottenCount = forgottenCount-1;

while(forgottenCount > 0)
{
uuuu=wUttered[nagent];

uu <- runif(1,1,wUttered[nagent]+1);	
	uu=floor(uu);
      nwordOld= forSample[[nagent]][[uu]];	
	forSample[[nagent]][[uu]]=forSample[[nagent]][[uuuu]];
  
	temporal= wHeardReservoir[[nwordOld]][nagent];
	if (temporal>0)
{wHeardReservoir[[nwordOld]][nagent]= wHeardReservoir[[nwordOld]][nagent]-1;}

wUttered[nagent]=wUttered[nagent]-1;
forgottenCount = forgottenCount-1;
forSample[[nagent]]<-forSample[[nagent]][-uuuu];
}

}

#----------------------------------------------------------------
#for neighbors
#----------------------------------------------------------------

	if (forgottenCountNEIG ==0)
	{
	tempL= forSample[[neighbor]];
	tempL= c(tempL,nword);
	forSample[[neighbor]]=tempL;
	tempL=c();
      
	#updates neighbor¥s heard/known conventions
	wUttered[neighbor]= wUttered[neighbor]+1;
	
	}
	if (forgottenCountNEIG > 0)
	{
	uu <- runif(1,1,wUttered[neighbor]+1);	
	uu=floor(uu);
      nwordOld= forSample[[neighbor]][[uu]];	
	forSample[[neighbor]][[uu]]=nword;
   
	temporal= wHeardReservoir[[nwordOld]][neighbor];
	if (temporal>0)
   {wHeardReservoir[[nwordOld]][neighbor]= wHeardReservoir[[nwordOld]][neighbor]-1;}
      
    forgottenCountNEIG = forgottenCountNEIG-1;

while(forgottenCountNEIG > 0)
{
uuuu=wUttered[neighbor];

uu <- runif(1,1,wUttered[neighbor]+1);	
	uu=floor(uu);
      nwordOld= forSample[[neighbor]][[uu]];	
	forSample[[neighbor]][[uu]]=forSample[[neighbor]][[uuuu]];
  
	temporal= wHeardReservoir[[nwordOld]][neighbor];
	if (temporal>0)
{wHeardReservoir[[nwordOld]][neighbor]= wHeardReservoir[[nwordOld]][neighbor]-1;}

wUttered[neighbor]=wUttered[neighbor]-1;
forgottenCountNEIG = forgottenCountNEIG-1;
forSample[[neighbor]]<-forSample[[neighbor]][-uuuu];
}
     }

wHeardReservoir[[nword]][neighbor]= wHeardReservoir[[nword]][neighbor]+1;
temp = wHeardReservoir[[nword]][neighbor];

if(mesaIndice[[nword]][1]==1)
{	if(temp==thresEasy)
	{
	visionMesa[[nword]][neighbor]=1;
	}
}
if(mesaIndice[[nword]][1]==2)
{	if(temp==thresHard)
	{
	visionMesa[[nword]][neighbor]=1;
	}
}

#---------------------------------------------------------------
# updates successful active conventions in the population
#---------------------------------------------------------------
	x1= visionMesa[[nword]][nagent];
	x2= visionMesa[[nword]][neighbor];
	if((x1==1)&&(x2==1))
	{
	distEffectiveTables[[nword]][1]= distEffectiveTables[[nword]][1]+1;
	}


} # "if convention is not new"

#################################################################
#If convention is NEW
#################################################################

     if (u == FixUttered)
	{

	#updates conventions
	mesa = c(mesa, list(mat.or.vec(1,1)));
	mesa[[nword]][1]=1;

	mesaIndice = c(mesaIndice, list(mat.or.vec(1,1)));
	randomnum= sample(1:2, 1);
	mesaIndice[[nword]][1]= randomnum;

      distEffectiveTables = c(distEffectiveTables, list(mat.or.vec(1,1)));
	distEffectiveTables[[nword]][1]=0;
     

#-----------------------------------------------------------
#for agent
#-----------------------------------------------------------

	if (forgottenCount == 0)#if no token is forgotten by agent
	{
	tempL= forSample[[nagent]];
	tempL= c(tempL,nword);
	forSample[[nagent]]=tempL;
	tempL=c();


     #updates agent¥s heard/known conventions
	wUttered[nagent]= wUttered[nagent]+1;
	wHeardReservoir = c(wHeardReservoir, list(mat.or.vec(ag,1)));
	wHeardReservoir[[nword]][nagent]=1;

	} #if forgottenCount == 0


	if (forgottenCount > 0)#if convention tokens are forgotten
	{
	uu <- runif(1,1,wUttered[nagent]+1);	
	uu=floor(uu);
	nwordOld= forSample[[nagent]][[uu]];
	forSample[[nagent]][[uu]]=nword;

      temporal= wHeardReservoir[[nwordOld]][nagent];
	if (temporal>0)
    {wHeardReservoir[[nwordOld]][nagent]= wHeardReservoir[[nwordOld]][nagent]-1;}

	wHeardReservoir = c(wHeardReservoir, list(mat.or.vec(ag,1)));
	wHeardReservoir[[nword]][nagent]=1;

      forgottenCount = forgottenCount-1;

while(forgottenCount > 0)
{
uuuu=wUttered[nagent];

uu <- runif(1,1,wUttered[nagent]+1);	
	uu=floor(uu);
      nwordOld= forSample[[nagent]][[uu]];	
	forSample[[nagent]][[uu]]=forSample[[nagent]][[uuuu]];
  
	temporal= wHeardReservoir[[nwordOld]][nagent];
	if (temporal>0)
{wHeardReservoir[[nwordOld]][nagent]= wHeardReservoir[[nwordOld]][nagent]-1;}

wUttered[nagent]=wUttered[nagent]-1;
forgottenCount = forgottenCount-1;
forSample[[nagent]]<-forSample[[nagent]][-uuuu];
}

} #if forgottenCount > 0

	
forgottenCount ==0
      
	visionMesa = c(visionMesa, list(mat.or.vec(ag,1)));
	visionMesa[[nword]][nagent]=1;
	
	propC = c(propC, list(mat.or.vec(ag,1)));
	nC = c(nC, list(mat.or.vec(ag,1)));
	
#----------------------------------------------------------------
#for neighbor
#---------------------------------------------------------------

	if (forgottenCountNEIG ==0) #if no token is forgotten by neighbor
	{
	tempL= forSample[[neighbor]];
	tempL= c(tempL,nword);
	forSample[[neighbor]]=tempL;
	tempL=c();


     #updates neighbor¥s heard/known conventions
	wUttered[neighbor]= wUttered[neighbor]+1;
	wHeardReservoir[[nword]][neighbor]=1;

	} #for if (forgottenNEIG == 0)

	if (forgottenCountNEIG >0)#if tokens are forgotten by neighbor
	{
	uu <- runif(1,1,wUttered[neighbor]+1);	
	uu=floor(uu);
	nwordOld= forSample[[neighbor]][[uu]];
	forSample[[neighbor]][[uu]]=nword;

	wHeardReservoir[[nword]][neighbor]=1;
   
	temporal= wHeardReservoir[[nwordOld]][neighbor];
	if (temporal>0)
    {wHeardReservoir[[nwordOld]][neighbor]= wHeardReservoir[[nwordOld]][neighbor]-1;}
      
     forgottenCountNEIG = forgottenCountNEIG -1;

while(forgottenCountNEIG > 0)
{
uuuu=wUttered[neighbor];

uu <- runif(1,1,wUttered[neighbor]+1);	
	uu=floor(uu);
      nwordOld= forSample[[neighbor]][[uu]];	
	forSample[[neighbor]][[uu]]=forSample[[neighbor]][[uuuu]];
  
	temporal= wHeardReservoir[[nwordOld]][neighbor];
	if (temporal>0)
{wHeardReservoir[[nwordOld]][neighbor]= wHeardReservoir[[nwordOld]][neighbor]-1;}

wUttered[neighbor]=wUttered[neighbor]-1;
forgottenCountNEIG = forgottenCountNEIG-1;
forSample[[neighbor]]<-forSample[[neighbor]][-uuuu];
}
	} # if forgottenNEIG >0

	} # for (u==FixUttered)

} # END of generation LOOP


###################################################
#MEASURES                  
###################################################

#------------------------------------------------------------------------------------------------------------------------------
# The following calculates "numEffectiveTables", which is the raw number of of active conventions
#------------------------------------------------------------------------------------------------------------------------------

numEffectiveTables=0;

xx = length(distEffectiveTables)

for (i in seq(xx))
	{
	xxx= distEffectiveTables[[i]][1]
    	if(xxx>0)
     	{numEffectiveTables= numEffectiveTables +1;}
     	}
write.table(distEffectiveTables, "file.txt", sep="\n");


#--------------------------------------------------------------------------------------------------------------------------------
# Calculates the number of active conventions shared by 10% of population in iMesaEfinal
#-----------------------------------------------------------------------------------------------------------------------------------

counter2= c();
counter3= c();
iMesaEfinal= c();

for(s in seq(length(mesa)))
{

counter2=c(counter2, list(mat.or.vec(1,1)));
counter2[[s]][1]=0;

counter3=c(counter3, list(mat.or.vec(1,1)));
counter3[[s]][1]=0;

iMesaEfinal=c(iMesaEfinal, list(mat.or.vec(1,1)));
iMesaEfinal[[s]][1]=0;

}


for (i in seq(ag))
{

longit= length(forSample[[i]])

for (j in seq(longit))
{
      xx1= forSample[[i]][j]; #the convention at place j in agent i
      counter3[[xx1]][1]=0;
}

for (j in seq(longit))
{
	xx1= forSample[[i]][j]; #the convention at place j in agent i
	xx2= visionMesa[[xx1]][i]; #whether the convention xx1 is visible for agent i

                 if((xx2>0)&&(counter3[[xx1]][1]==0))
	{
                  counter2[[xx1]]=counter2[[xx1]]+1; # it increments in one each time an agent has active the convention xx1
	counter3[[xx1]][1]=1; # this is for counting only once each convention for each agent
	}

	
} # for j for Sample
} # for i for agent


yyy = length(counter2) #the number of conventions

for (i in seq(yyy))
{
 
     xxx= counter2[[i]][1]
     if(xxx > (parameterActive-1))
     {iMesaEfinal[[i]][1]=1;}

}


##############################################################################################
# Calculates the number of active conventions shared by 10% of population for HARD and EASY conventions (numIEEA and numIEH)
##############################################################################################

iMesaEEA= c();
iMesaEH = c();

xx = length(iMesaEfinal)

for (j in seq(xx))
{
	xx1=mesaIndice[[j]][1];

	if(xx1==2)
	{
	iMesaEH[[j]]=iMesaEfinal[[j]];
	iMesaEEA[[j]]=0;
	}

	if(xx1==1)
	{
	iMesaEEA[[j]]=iMesaEfinal[[j]];
	iMesaEH[[j]]=0;
	}



} #for j



numIEH=0;
xx = length(iMesaEH)

for (i in seq(xx))
{
xxx= iMesaEH[[i]][1]
    if(xxx>0)
     {numIEH= numIEH+1;}
}

numIEEA=0;
xx = length(iMesaEEA)

for (i in seq(xx))
{
xxx= iMesaEEA[[i]][1]
   
if(xxx>0)
     {numIEEA= numIEEA+1;}

}

#------------------------------------------------------------
#other measures
#------------------------------------------------------------

for (i in seq(ag))
{
longitCon = length(forSample[[i]]);

for (j in seq(longitCon))
{
	xx1= forSample[[i]][j]; #the convention at place j in agent i
	xx2= visionMesa[[xx1]][i]; #whether the convention xx1 is visible


xxx= agentNeig[[i]]; 
xx3=0;

for (kk in seq(xxx))
{
neighbor = neigMat[i,kk];
xx4= visionMesa[[xx1]][neighbor];
  if(xx4>0){xx3= xx3 + 1;}
}
      if((xx2>0)&(xx3>0))
	{
      tempL=c();
	tempL= SuccConPerAg[[i]];
	tempL= c(tempL,1);
	SuccConPerAg[[i]]=tempL;
	tempL=c();
	}
      if ((xx2==0)||(xx3==0))
	{
     tempL=c();
	tempL= SuccConPerAg[[i]];
	tempL= c(tempL,0);
	SuccConPerAg[[i]]=tempL;
	tempL=c();
	}

}#for j

}#for i

SuccConPerAg = c(); #Successful conventions per agent
for(s in seq(ag))
{
SuccConPerAg = c(SuccConPerAg, list(mat.or.vec(1,1)));
SuccConPerAg[[s]][1]=0;
}

for (i in seq(ag))
{
longitCon = length(forSample[[i]]);

for (j in seq(longitCon))
{
	xx1= forSample[[i]][j]; #convention at place j in agent i
	xx2= visionMesa[[xx1]][i]; #convention xx1 is visible


xxx= agentNeig[[i]]; 
xx3=0;

for (kk in seq(xxx))
{
neighbor = neigMat[i,kk];
xx4= visionMesa[[xx1]][neighbor];
  if(xx4>0){xx3= xx3 + 1;}
}

propC[[xx1]][i]=xx3/xxx; #proportion of successful conventions
nC[[xx1]][i]=xx3; #number of succesful conventions

      if((xx2>0)&&(xx3>0))
	{
     tempL=c();
	tempL= SuccConPerAg[[i]];
	tempL= c(tempL,1);
	SuccConPerAg[[i]]=tempL;
	tempL=c();
	}
      if ((xx2==0)||(xx3==0))
	{
                  tempL=c();
	tempL= SuccConPerAg[[i]];
	tempL= c(tempL,0);
	SuccConPerAg[[i]]=tempL;
	tempL=c();
	}

}#for j

}#for i

EHPerAg= mat.or.vec(ag,1);
EEAPerAg= mat.or.vec(ag,1);

longMesa= length(mesa);
contadorCon= mat.or.vec(longMesa,1);


for (j in seq(longMesa))
{
contadorCon[j]=0;
}

contadorEASY=0;
contadorHARD=0;
contadorPROPe=0;
contadorPROPh=0;

for (i in seq(ag))
{
for (j in seq(longMesa))
{
contadorCon[j]=0;
}

longitCon = length(forSample[[i]]);

for (j in seq(longitCon))
{
	xx1= forSample[[i]][j]; #convention j in agent i
     xx2= SuccConPerAg[[i]][j+1]; #whether convention xx1 is successful
      xx3= mesaIndice[[xx1]][1];
	 xx4= contadorCon[xx1];
      xx5 = propC[[xx1]][i];
      xx6= visionMesa[[xx1]][i]; #whether convention xx1 is visible
	xx7=nC[[xx1]][i];

if((xx6>0)&&(xx3==1)&&(xx4==0))
{
contadorEASY=contadorEASY+1;
contadorPROPe=contadorPROPe+xx5;
}

if((xx6>0)&&(xx3==2)&&(xx4==0))
{
contadorHARD=contadorHARD+1;
contadorPROPh=contadorPROPh+xx5;
}

if((xx2>0)&&(xx3==1)&&(xx4==0))
{
EEAPerAg[i] = EEAPerAg[i]+1;
}
if((xx2>0)&&(xx3==2)&&(xx4==0))
{
EHPerAg[i] = EHPerAg[i]+1;
}
   contadorCon[xx1]=1;

} #for i
} #for j

#--------------------------------------------------------------
mEHA=mean(EHPerAg);      #average number of "successful" conventions per agent (succesful= understood by at least 1 neighbor) (ABSOLUTE NUMBER) HARD

mEEAA=mean(EEAPerAg);    #average number of "successful" conventions per agent (succesful= understood by at least 1 neighbor) (ABSOLUTE NUMBER) EASY

#---------------------------------------------------------------

pe=contadorPROPe/contadorEASY; # mean proportion of neigbors that share an agent¥s convention, averaged across all convention-agents (EASY)

ph=contadorPROPh/contadorHARD; # mean proportion of neigbors that share an agent¥s convention, averaged across all convention-agents (HARD)
#---------------------------------------------------------------

numIEEA
numIEH
mEEAA
mEHA
pe
ph

#----------------------------------------------------------------

