************************************************************************************
************************************************************************************
****                                                                       	****
****        TYPE 3260  : Version 1.0, December, 12th, 2016                      ****
****                                                                       	****  
****        PCM storage tank  model for TRNSYS 16                               ****
****                                                                       	****
****        AUTHORS:                                                        	****
****        Dr. Katherine D'Avignon, Prof. Michaël Kummert                      ****
****        Polytechnique Montreal                                  		****
****        Montreal, Quebec, Canada                        			****
****        Tel.: +1 514 340 4711, Fax : +1 514 340 5170                 	****
****        email: katherine.davignon@polymtl.ca, michael.kummert@polymtl.ca 	****
****                                                                       	****
************************************************************************************
************************************************************************************


-------------------------------------------------------------------------------
|   (1)  I N S T A L L A T I O N                                              |
-------------------------------------------------------------------------------

      (a)  copy the files in the "dll" directory of the installation package  
           into your TRNSYS "Trnsys16\UserLib" directory.
      (b)  copy the files in the "Proforma" directory of the installation   
           package into the your TRNSYS "Trnsys16\Studio\Proformas\MyComponents"
           directory.
      (c)  When you start TRNSYS 16 the new type 3260 is available from the
           "MyComponents" directory.
           
-------------------------------------------------------------------------------
|   (2)  T E S T   P R O J E C T                                              |
-------------------------------------------------------------------------------
 
        "Example_Inter_H&C_10C_030.tpf"
        Comparison of the simulation with measured data from a series of 
        interrupted heating & cooling cycles. Information on the experimental 
	data can be found in chapter 6 of Dre. D'Avignon's thesis, in the 
	documentation section.
           
-------------------------------------------------------------------------------
|   (3)  C R E D I T                                                          |
-------------------------------------------------------------------------------
 
Please give appropriate credit in any publications when you use our Type for 
your simulations. Please cite the following publication when refering to this 
model :

Katherine D'Avignon & Michaël Kummert (2018) Modeling horizontal storage tanks 
with encapsulated phase change materials for building performance simulation, 
Science and Technology for the Built Environment, 24:4, 327-342, 
DOI: 10.1080/23744731.2018.1438012
           