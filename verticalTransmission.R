###################################################
# this is the Exec file for vertical transmission
###################################################

load ("Matrices30.rdata"); #loading the network for population size X in MatricesX.rdata

#----------------------------------------------------------------
#parameter setting
#----------------------------------------------------------------

nnumber= 1; #network number in networks� file L (in file MatricesX.rdata)

ag=30; #population size (number of agents)

thresEasy = 1; #times an agent needs to encounter an easy convention to learn it

thresHard = 2; #times an agent needs to encounter a hard convention to learn it

iterations =1000; #number of iterations
 
parameterActive = (0.1)*ag; # 10% of population size

doff=200; #for each of the M convention tokens stored, there is a  probability p = 1/doff that this token is 'forgotten.'

numSeed =13; # random seed

set.seed(numSeed);

#----------------------------------------------------------------
# definitions 
#---------------------------------------------------------------

conAg= matrix(0,ag,ag); #matrix of agent connectivity

neigMat= matrix(0,ag,ag); #matrix of agent큦 neighbors

mesa= c(); #vector for conventions [i]. Dynamically growing list
 
mesaIndice= c(); # the hard/easy index of each convention [i] 
		 
wUttered= mat.or.vec(ag,1);#number of tokens(costumers) for agent큦 active conventions

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
	nagent=((n-1)%%ag) +1 ; #chooses an agent

# sampling a convention 
stopper=0;

while (stopper==0)
{
	u <- runif(1,1,wUttered[nagent]+2);	
	u=floor(u);
  
	if (u < (wUttered[nagent]+1))
	{

     #sampling a convention
	nword=forSample[[nagent]][[u]];

	vision=visionMesa[[nword]][nagent];

	if(vision==1)
	{stopper=1;}
	}

	if (u ==(wUttered[nagent]+1))
	{
	k=k+1;
	nword= k;
	stopper=1;
	}
}

# sample neighbor for communication
xxx= agentNeig[[nagent]];
neig <- runif(1,1,xxx+1);	
neig=floor(neig); 

neighbor = neigMat[nagent,neig]; 

#################################################################
#If convention sampled is NOT new ################################################################

if (u < (wUttered[nagent]+1))
{
	nword=forSample[[nagent]][[u]]; 
     
#--------------------------------------------
#for agents
#--------------------------------------------

	#updates forSample for agent
     	tempL= forSample[[nagent]];
	tempL= c(tempL,nword);
	forSample[[nagent]]=tempL;
	tempL=c();
      
      #updates number of agent큦 uttered conventions
      wUttered[nagent]= wUttered[nagent]+1;
      
      #updates convention list 
      mesa[[nword]][1]= mesa[[nword]][1]+1;

      #updates reservoir of heard conventions
     	 wHeardReservoir[[nword]][nagent]= wHeardReservoir[[nword]][nagent]+1;

#----------------------------------------------------------------
#for neighbors
#----------------------------------------------------------------

	#updates uttered conventions
	wUttered[neighbor]= wUttered[neighbor]+1;

     #updates forSample
	tempL= forSample[[neighbor]];
	tempL= c(tempL,nword);
	forSample[[neighbor]]=tempL;
	tempL=c();

	#updates reservoir of heard conventions
	wHeardReservoir[[nword]][neighbor]= wHeardReservoir[[nword]][neighbor]+1;

temp = wHeardReservoir[[nword]][neighbor];
if(mesaIndice[[nword]][1]==1)
{	if(temp==thresEasy)
	{ 
	visionMesa[[nword]][neighbor]=1;
	}
}
if(mesaIndice[[nword]][1]==2)
{	if(temp>=thresHard)
	{ 
	visionMesa[[nword]][neighbor]=1;
	}
}

} #if (u<wUttered)... "if convention is not new"


#################################################################
#If convention is NEW
#################################################################

if (u==(wUttered[nagent]+1)) 
{


#----------------------------------------------------------------
#for agent
#----------------------------------------------------------------

	#updates convention list
	mesa = c(mesa, list(mat.or.vec(1,1)));
	mesa[[nword]][1]=1;

     #HARD/EASY index is assigned randomly to new convention
	mesaIndice = c(mesaIndice, list(mat.or.vec(1,1)));
	randomnum= sample(1:2, 1);
	mesaIndice[[nword]][1]= randomnum;

	#updates forSample for agent
	tempL=c();
	tempL= forSample[[nagent]];
	tempL= c(tempL,nword);
	forSample[[nagent]]=tempL;
	tempL=c();

     #updates agent큦 uttered conventions
	wUttered[nagent]= wUttered[nagent]+1;

	#updates reservoir of heard conventions
	wHeardReservoir = c(wHeardReservoir, list(mat.or.vec(ag,1)));
      wHeardReservoir[[nword]][nagent]=1;

	#updates visionMesa for agent
	visionMesa = c(visionMesa, list(mat.or.vec(ag,1)));
	visionMesa[[nword]][nagent]=1;

	propC = c(propC, list(mat.or.vec(ag,1)));
	nC = c(nC, list(mat.or.vec(ag,1)));
	
#----------------------------------------------------------------
#for neighbor
#----------------------------------------------------------------

	wUttered[neighbor]= wUttered[neighbor]+1;
	wHeardReservoir[[nword]][neighbor]=1;

	#updates forSample
	tempL= forSample[[neighbor]];
	tempL= c(tempL,nword);
	forSample[[neighbor]]=tempL;
	tempL=c();

     #updates neighbor큦 conventions
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

} # for (u==(wUttered[nagent]+1)) "if convention is NEW"

################################################################
#DIE OFF sampling
################################################################

randomnum= sample(1:doff, 1);

if((randomnum==1)&&(n>1)) #if die off
{
wUttered[nagent]=1;
	
longForSample= length(forSample[[nagent]]);

 for(ii in seq(longForSample))
 {
  salida= longForSample-(ii-2);
  forSample[[nagent]]<-forSample[[nagent]][-salida];
  }

xxx= agentNeig[[nagent]];
neig <- runif(1,1,xxx+1);	
neig=floor(neig); 

neighbor = neigMat[nagent,neig]; 

longSAMPneig= length(forSample[[neighbor]])

bornWordIndex= sample(1:longSAMPneig, 1);

bornWord=forSample[[neighbor]][bornWordIndex];

forSample[[nagent]][1]=bornWord;

	#update of wHeardReservoir id die off 
for(ii in seq(k))
{
	wHeardReservoir[[ii]][nagent]=0;
	visionMesa[[ii]][nagent]=0;
}

wHeardReservoir[[bornWord]][nagent]=1;
visionMesa[[bornWord]][nagent]=1;

} #if rumnumber "if die off happens"


} # END of generation LOOP



###################################################
#MEASURES                  
###################################################

numWords=length(mesa);

for (j in seq(numWords)){

activeTables = c(activeTables, list(mat.or.vec(1,1)));
activeTables[[j]][1]=0;

}

for (j in seq(numWords)){

contadorActive=0;
	
for(i in seq(ag))
{ 
contadorActive= contadorActive+visionMesa[[j]][i];
}
	if(contadorActive>(parameterActive-1))
	{
	activeTables[[j]][1]=1;
	}

}

#---------------------------------------------------------------
# successful active conventions in the population
#---------------------------------------------------------------

numEffectiveTables=0;
xx = length(distEffectiveTables)
for (i in seq(xx)){
xxx= distEffectiveTables[[i]][1]
    if(xxx>0)
     {numEffectiveTables= numEffectiveTables +1;}
     	}
write.table(distEffectiveTables, "file.txt", sep="\n");



################################################################
# relevant output measures
################################################################

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


#---------------------------------------------------------------
# number of active/successful conventions #---------------------------------------------------------------

numIEH=0;

xx = length(iMesaEH)

for (i in seq(xx)){
xxx= iMesaEH[[i]][1]
    if(xxx>0)
     {numIEH= numIEH+1;}
     	}

numIEEA=0;

xx = length(iMesaEEA)

for (i in seq(xx)){
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

pe=contadorPROPe/contadorEASY; # mean proportion of neigbors that share an agent큦 convention, averaged across all convention-agents (EASY)

ph=contadorPROPh/contadorHARD; # mean proportion of neigbors that share an agent큦 convention, averaged across all convention-agents (HARD)

#---------------------------------------------------------------




