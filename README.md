# lang-paradox
Code and data for simulating the effects of population size on grammar and vocabulary complexity

Article: Reali, F., Chater, N. & Christiansen, M.H. (2018). Simpler grammar, larger vocabulary: How population size affects language. Proceedings of the Royal Society B: Biological Sciences, 285, 20172586. https://doi.org/10.1098/rspb.2017.2586 

Ben Falandays discovered a minor error in the original code, which has been corrected in the file "horizontalTransmission_corrected.R". The error only resulted in minor changes (differences of 4 percentage-points or less) and does not impact the overall results or interpretations thereof in the paper (see panels comparison.xlsx for details). We have therefore not changedd the results files or Fig. 1. However, any further work within this framework should use the corrected code. We thank Ben Falandays for informing us of the error in our code. 

The data in results.xlxs are the simulation results plotted in Figure 1.
The R codes used for simulations are the following:

For network constructions: 
RandomNetworksExec2.R
RandomNetworksFunc2.R

For vertical and horizontal transmission simulation:
verticalTransmission.R
horizontalTransmission_corrected.R

-------------------------------------------------------------------------------------------------------------------------
PARAMETERS 
-------------------------------------------------------------------------------------------------------------------------
  notation in text       
1. population size: n = 30 50 100 200 500
2. type of transmision: vertical; horizontal
3. Probability of Poisson forgetting: p= 1/200; 1/500

  notation in results file (results.xlsx)
1. n (in column A)
2. in tab labels: "horiz" and "vert" 
3. f= 1/p 

   notation in code (horizontalTransmission_corrected.R and verticalTransmission.R)
1. n = "ag"
2. different code files "verticalTransmission.R" and "horizontalTransmission_corrected.R"  
3. f = "doff"


--------------------------------------------------------------------------------------------------------------------------
MAIN OUTPUTS NOTATION
--------------------------------------------------------------------------------------------------------------------------

in text:

1.  average number of successful conventions per agent (succesful= understood by at least 1 neighbor) (ABSOLUTE NUMBER)
1¥. average number of successful conventions per agent (succesful= understood by at least 1 neighbor) (RELATIVE PROPORTION)
2.  average number of conventions shared by at least 10% population (ABSOLUTE NUMBER)
2¥. average number of conventions shared by at least 10% population (RELATIVE PROPORTION)
3.  mean proportion of neigbors that share an agent¥s convention, averaged across all convention-agents


in results.xlxS file:

1. mean#EASY_CONVperAg  (for easy conventions) and mean#EASY_CONVperAg (for hard conventions)
1¥.mean_PROP_EASY_CONV_perAg (for easy conventions) and mean_PROP_EASY_CONV_perAg (for hard conventions)
2. EASY_SUCCESSFUL (for easy conventions) and HARD_SUCCESSFUL (for hard conventions)
2¥. %EASY_SUCCESSFUL (for easy conventions) and %HARD_SUCCESSFUL (for hard conventions)
3. propNeigh_Easy (for easy conventions) and propNeigh_Hard (for hard conventions)

in code (horizontalTransmission_corrected.R and verticalTransmission.R)

1. mEEAA (for easy conventions)  mEHA (for hard conventions)
2. numIEEA (for easy conventions) and numIEH (for hard conventions)
3. pe and ph (for easy and hard conventions, respectively)

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
*****************************
CODE
*****************************
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
To create networks
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Code Files: 
RandomNetworksExec2.R
RandomNetworksFunc2.R

The Exec File "RandomNetworksExec2.R" calls "RandomNetworksFunc2.R" as source

The population size "n" and the number of networks to be created are set using the parameters "n" and "numNETS" in 
"RandomNetworksExec2.R"

The output is "L" is a list of a number="numNETS" of connectivity matrices. Each matrix i, defined by L[[i]] is a network 
corresponding to "n" agents that can be used for simulation.

The list L is saved in a file Matrices[*].rdata where [*] is the population size

----------------------------------
To visualize networks¥ parameters:
----------------------------------
in R console:

source("RandomNetworksFuns2.r")
load ("Matrices[*].rdata")
params(L)

The function params(L) returns a table where rows correspond to each network in L. The first column corresponds to the
network index, the second column corresponds to networks¥ Mean Nodal Degree k (where Mean ﬂ in k = nﬂ-1), and the 
third column corresponds to network¥s clustering coefficients.


------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
Vertical and horizontal transmission
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------


1. EXECUTION and NETWORK LOADING
---------------------------------

Execution files: "VerticalTransmission.R" and "horizontalTransmission_corrected.R" 

Network connectivity data: 
Load Matrices[*].rdata, set the loading command manually at the beginning of "VerticalTransmission.txt" and "horizontalTransmission_corrected.R", where [*] corresponds to population size. 

One of the networks in Matrices[*].rdata should be chosen for a given simulation. 
Please choose the network number "x" by setting the parameter nnumber=x

2. PARAMETER SETTING
--------------------------------
Parameter setting is done at the initial lines of "VerticalTransmission.R" and "horizontalTransmission_corrected.R"

ag=X; population size (number of agents)

thresEasy = X; times an agent needs to encounter an easy convention to learn it

thresHard = X; times an agent needs to encounter a hard convention to learn it

iterations =X; number of generations
 
doff=X; 1/p, where p is the probability of dying off

3. EXECUTION
--------------------------------
load or paste the text "VerticalTransmission.R" and "horizontalTransmission_corrected.R" in R console

4. OUTPUTS
--------------------------------
retrieve

"pe" 
proportion of neighbors sharing agent's easy conventions averaged across agents. (in Fig 1= "mean proportion of neighbors that share an agent's convention, averaged across all convention-agents")

"ph" 
proportion of neighbors sharing agent's hard conventions averaged across agents. (in Fig 1= "mean proportion of neighbors that share an agent's convention, averaged across all convention-agents")


"numIEEA" 
average number of easy conventions shared by at least 10% population


"numIEH" 
average number of hard conventions shared by at least 10% population

"mEEAA"
average active easy conventions per agent (successful conventions, understood by at least 1 neighbor)

"mEHA"
average active hard conventions per agent (successful conventions, understood by at least 1 neighbor)

-----------------------------------------------------------------------------------------------------------------------


