! TRNSYS Type 3260: Encapsulated PCM storage tank  
! ----------------------------------------------------------------------------------------------------------------------
!
!
! This routines models a PCM storage tank with rectangular bricks of PCM. 
!
!
! Inputs 
! ----------------------------------------------------------------------------------------------------------------------
! Nb | Variable      | Description                                                    | Input  Units   | Internal Units 
! ---|---------------|----------------------------------------------------------------|----------------|----------------
!  1 | Tin           | Fluid inlet temperature                                        | °C             | °C
!  2 | mDotFlTot     | Total fluid inlet mass flowrate                                | kg/h           | kg/s
!  3 | Tenv          | Tank environment temperature                                   | °C             | °C
!  
! Parameters
! ----------------------------------------------------------------------------------------------------------------------
! Nb | Variable      | Description                                                    | Param. Units   | Internal Units 
! ---|---------------|----------------------------------------------------------------|----------------|----------------
!  1 | mode          | 0 = First version                                              | -              | -
!  2 | nCapD         | Number of PCM capsules over the height of the PCM tank         | -              | -
!  3 | nCapL         | Number of PCM capsules over the length of the PCM tank         | -              | -
!  4 | nCapW         | Number of PCM capsules over the width of the PCM tank          | -              | -
!  5 | lCap          | Length of PCM capsule                                          | m              | m
!  6 | wCap          | Width of PCM capsule                                           | m              | m
!  7 | dCap          | Height of PCM capsule                                          | m              | m
!  8 | dFl           | Height of vertical space between two PCM capsules              | m              | m
!  9 | rhoFl         | Fluid density                                                  | kg/m^3         | kg/m^3 
! 10 | cpFl          | Fluid specific heat capacity                                   | kJ/kg-K        | J/kg-K
! 11 | kFl           | Fluid thermal conductivity                                     | kJ/h-m-K       | J/s-m-K
! 12 | muFl          | Fluid dynamic viscosity                                        | kg/m-h         | kg/m-s
! 13 | beta 	     | Fluid coefficient of volumetric thermal expansion 	          | 1/K 	       | 1/K
! 14 | rhoSolPcm     | PCM solid phase density                                        | kg/m^3         | kg/m^3
! 15 | kSolPcm       | PCM solid phase thermal conductivity                           | kJ/h-m-K       | J/s-m-K
! 16 | rhoLiqPcm     | PCM liquid phase density                                       | kg/m^3         | kg/m^3
! 17 | kLiqPcm       | PCM liquid phase thermal conductivity                          | kJ/h-m-K       | J/s-m-K
! 18 | nNodesFl      | Number of nodes over the length of the PCM tank                | -              | -
! 19 | Tinit         | Initial PCM tank temperature                                   | °C             | °C
! 20 | Uenv          | Convective heat transfer coefficient: surroundings to PCM tank | kJ/h-K-m^2     | J/s-K-m^2 
! 21 | nNodePCM      | Number of nodes over the half-height of the PCM capsule        | -              | -
! 22 | mCap          | PCM mass inside one PCM capsule		                          | kg             | kg	 	
! 23 | HxCoef        | Heat exchange coef. (% of wCap where heat transfer occurs)     | %              | %  	 	
! 24 | Rcap          | Equivalent thermal resistance for the capsule wall and void    | m^2-°C/W       | m^2-°C-s/J
! 25 | VolInTot      | Total entry length volume                                      | m^3            | m^3
! 26 | VolOutTot     | Total exit length volume                                       | m^3            | m^3
! 27 | BP_f          | By-pass factor                                                 | %              | -
! 28 | Abp           | Area through which HTF circulates to by-pass PCM capsules      | m^2            | m^2    
! 29 | H0            | Enthalpy point 0, common both cooling and heating curve        | kJ/kg          | kJ/kg
! 30 | T0            | Temperature point 0, common both cooling and heating curve     | degC           | degC
! 31 | HC1           | Enthalpy point 1, cooling curve                                | kJ/kg          | kJ/kg    
! 32 | TC1           | Temperature point 1, cooling curve                             | degC           | degC   
! 33 | HC2           | Enthalpy point 2, cooling curve                                | kJ/kg          | kJ/kg    
! 34 | TC2           | Temperature point 2, cooling curve                             | degC           | degC  
! 35 | HC3           | Enthalpy point 3, cooling curve                                | kJ/kg          | kJ/kg    
! 36 | TC3           | Temperature point 3, cooling curve                             | degC           | degC  
! 37 | HC4           | Enthalpy point 4, cooling curve                                | kJ/kg          | kJ/kg    
! 38 | TC4           | Temperature point 4, cooling curve                             | degC           | degC  
! 39 | H5            | Enthalpy point 5, common both cooling and heating curve        | kJ/kg          | kJ/kg    
! 40 | T5            | Temperature point 5, common both cooling and heating curve     | degC           | degC     
! 41 | HH1           | Enthalpy point 1, heating curve                                | kJ/kg          | kJ/kg
! 42 | TH1           | Temperature point 1 , heating curve                            | degC           | degC
! 43 | HH2           | Enthalpy point 2, heating curve                                | kJ/kg          | kJ/kg    
! 44 | TH2           | Temperature point 2, heating curve                             | degC           | degC   
! 45 | HH3           | Enthalpy point 3, heating curve                                | kJ/kg          | kJ/kg    
! 46 | TH3           | Temperature point 3, heating curve                             | degC           | degC  
! 47 | HH4           | Enthalpy point 4, heating curve                                | kJ/kg          | kJ/kg    
! 48 | TH4           | Temperature point 4, heating curve                             | degC           | degC  
! 49 | Cinit         | Initial PCM curve, either liquid or heating curve              | -              | -           
! 50 | FlowS         | Flow restriction coef. (% of wCap over which HTF circulates)   | %              | %        
!
! Outputs 
! ----------------------------------------------------------------------------------------------------------------------
! Nb | Variable         | Description                                               | Output  Units  | Internal Units 
! ---|------------------|-----------------------------------------------------------|----------------|----------------
!  1 | Tout             | Fluid outlet temperature                                  | °C             | °C
!  2 | mdotFlTot        | Fluid total mass flow rate                                | kg/h           | kg/s
!  3 | HeatStorageRate  | Rate of heat storage                                      | kJ/h           | kJ/h
!  4 | SOC              | PCM tank State Of Charge                                  | %              | % 
!
! SubPrograms
! ----------------------------------------------------------------------------------------------------------------------
! 
! None
!
!
! Required external librairies
! ----------------------------------------------------------------------------------------------------------------------
!
! None
!
!
! Revision history
! ---------------------------------------------------------------------------------------------------------------------
!
! This type was originally written by Katherine D'Avignon and Michaël Kummert at Ecole Polytechnique Montreal 
! in March 2012.
!
! Modifications:
!
! 2012-03-05 - MKu - Version 1.0 
! 2012-07-11 - KD - Version 2.0
! 2012-09-11 - KD - Version 2.1     Notes: Multiple corrections to get the same result as Type5001. The use of Tw in the program is changed.
! 2012-09-17 - KD - Version 2.2     Notes: Added conduction between fluid nodes during no flow conditions, use of equivalent heat transfer 
!                                          coefficient (heq) for PCM/fluid heat exchange, Tw is removed, added heat losses to the surroundings. 
! 2013-03-15 - KD - Version 3.0     Notes: Major modification is that many nodes are included over the half-height of the PCM capsule. 
!                                          Introducing a special node type for the node at the capsule wall and for the node adjacent to the 
!                                          capsule symmetry line.
! 2013-05-15 - KD - Version 3.0	    Notes: Correct SOC to account for the fact that wall nodes are half the size of other PCM nodes.
!			                               Add info from nIter into debug mode.
! 2013-09-13 - KD - Version 3.0.1   Notes: Direct input of PCM mass rather than being calculated from density and PCM capsule volume	 	
!                                          *Compare capsule volume to PCM volume (calculated from density and mass) to evaluate the 	 	
!                                          capsule filling ratio. The void in the capsule changes overall PCM height and control volume 	 	
!                                          distribution. Additional thermal resistance due to presence of void.
!                                          *Set Pe limit to 2 rather than 100
!                                          *Change kPcm calculation for c.v. face from arithmetic mean to harmonic mean of adjacent node temperatures
! 2014-02-24 - KD - Version 3.0.1   Notes: Correction of errors in the code regarding heq and dy for cases where nNodePcm = 1.
! 2014-03-18 - KD - Version 3.4     Notes: Added tank bypass, entry and exit lengths water volumes and thermal losses.
! 2014-06-24 - KD - Version 3.5     Notes: Input HvsT information as curve rather than Cp_s, Cp_l ect
! 2015-07-27 - KD - Version 3.5.1   Notes: Added hysterisis between heating and cooling curves
! 2015-10-06 - KD - Version 3.5.2   Notes: Version used in article submitted for thesis and to IBPSA Journal.    
! 2016-10-24 - KD - Version 3.5.3   Notes: Revised version from after the article was rejected by IBPSA Journal. Many corrections.   
! 2016-12-12 - KD - Version 3.5.4   Notes: Preparation for diffusion. Changed Type #, removed excess parameters, added stability criteria and internal time step. 
!
! Local variables
! ----------------------------------------------------------------------------------------------------------------------
!
! a 		    : Variable used in calculating each fluid node's temperature [-]  
! APcm          : Surface area of convective heat transfer between the fluid and the PCM [m^2] 
! AvgCp         : Average slope of hvsT curve (cp) for liquid and solid phases [J/kg-°C]
! Ax            : Surface area perpendicular to fluid flow [m^2]
! b 		    : Variable used in calculating each fluid node's temperature [-]
! beta		    : Coefficent of volumetric thermal expansion [1/K]
! c 		    : Variable used in calculating each fluid node's temperature [-]
! Cinit         : Initial PCM curve, either liquid or heating curve 
! converged 	: Binary value of convergence, 1 if convergence is attained for the specific node considered [-]
! cpFl  	    : Specific heat of heat transfer fluid [J/kg-°C]
! cpPcm         : Specific heat of PCM used to verify the stability criteria [J/kg-°C]    
! Curvei        : Matrix of status of whether the heating or cooling hvsT curve is used [-]
! Curvef        : Matrix of status of whether the heating or cooling hvsT curve is used [-]    
! cvFlow 	    : Volume of fluid entering a control volume over one time step [m^3]
! cvV 		    : Volume of fluid in a control volume [m^3]
! d 		    : Variable used in calculating each fluid node's temperature [-]
! dFl        	: Height of fluide passage between PCM capsules [m]
! dCap    	    : Height of PCM capsule [m]
! dPcm          : Half height of PCM volume [m]
! dt      	    : Time step [s]
! dW 		    : Capsule wall thickness [m]
! dy            : Height of one PCM node [m]
! filR          : Filling ratio [-]
! fLPCm 	    : Matrix of liquid fraction for each PCM control volume [%]
! FlowS         : Flow restriction coef. (% of wCap over which HTF really circulates) [-]
! FoI           : Fourier number for interior PCM control volume [-]
! FoW           : Fourier number for PCM control volume at the capsule wall [-]    
! Gr 		    : Grashof number of the heat transfer fluid [-]
! hcPcm 	    : Heat transfer coefficient for convection from the heat transfer fluid to the PCM capsule [J/s-°C-m^2]
! heq 		    : Equivalent heat transfer coefficient for convection from the heat transfer fluid to the PCM capsule [J/s-°C-m^2]
! hfPcm  	    : Matrix of final specific enthalpy of each PCM control volume [J/kg]
! hfPcmOld 	    : Matrix of final specific enthalpy of each PCM control volume from the previous time step [J/kg]
! hiPcm 	    : Matrix of initial specific enthalpy of each PCM control volume [J/kg]
! hL2sPcm 	    : Specific enthalpy at the begining of the solidification process (liquid to solid) [J/kg]
! hS2lPcm 	    : Specific enthalpy at the begining of the melting process (solid to liquid) [J/kg]
! HxCoef        : Heat exchange coef. (% of wCap where heat transfer occurs) [-]
! H0            : Enthalpy point 0, common to both heating and cooling curves [kJ/kg] 
! H5            : Enthalpy point 5, common to both heating and cooling curves [kJ/kg]
! HC1           : Enthalpy point 1, cooling curve [kJ/kg]
! HC2           : Enthalpy point 2, cooling curve [kJ/kg]
! HC3           : Enthalpy point 3, cooling curve [kJ/kg]
! HC4           : Enthalpy point 4, cooling curve [kJ/kg] 
! HH1           : Enthalpy point 1, heating curve [kJ/kg]
! HH2           : Enthalpy point 2, heating curve [kJ/kg]
! HH3           : Enthalpy point 3, heating curve [kJ/kg]
! HH4           : Enthalpy point 4, heating curve [kJ/kg]      
! i  		    : Variable used in DO loop initializing variables for the present time step [-]
! j		        : Variable used in DO loop calculating each node [-]
! kFl		    : Thermal conductivity of the heat transfer fluid [J/s-m-°C]
! kLiqPcm	    : Thermal conductivity of the liquid PCM [J/s-m-°C]
! kPcm		    : Matrix of thermal conductivity of each PCM control volume for the present time step [J/s-m-°C]
! kSolPcm	    : Thermal conductivity of the solid PCM [J/s-m-°C]
! kW		    : Thermal conductivity of the PCM capsule wall [J/s-m-°C]
! L		        : Length of one PCM control volume [m]
! Lbp           : Half length of by-pass volume [m]    
! lCap		    : Length of PCM capsule [m]
! LCp           : Liquid specific heat [kJ/kg-K]    
! Lin           : Half lenght of entry volume [m]  
! Lout          : Half lenght of exit volume [m]    
! lCap		    : Length of PCM capsule [m]
! maxError	    : Maximum error
! maxErrorTFl	: Maximum difference between the last iteration fluid temperature and this iteration's fluid temperature 
! maxErrorTPcm	: Maximum difference between the last iteration PCM temperature and this iteration's PCM temperature 
! mCap		    : PCM mass inside one PCM capsule [kg]
! mDotBP        : Mass flow rate entering the by-pass [kg/s]
! mDotFl        : Mass flow rate in each fluid control volume (half height between PCM capsules) [kh/s]  
! mf            : Multiplication factor of capsule and HTF area to total tank area including by-pass [-]    
! mPcm          : Mass of PCM of one PCM control volume [kg]
! muFl          : Heat transfer fluid dynamic viscosity [kg/m-s]
! nCapD         : Number of PCM capsules over the height of the tank [-]
! nCapL         : Number of PCM capsules over the length of the tank [-]
! nCapW         : Number of PCM capsules over the width of the tank [-]
! nIter         : Number of iterations performed on the specific node evaluated [-]
! nIerBP        : Number of iterations performed on the by-pass node [-]
! nIerVol       : Number of iterations performed on the entry and exit nodes [-]
! nMaxIter      : Maximum number of iterations before convergence is considered impossible and the program automatically exits [-]
! nNodePCM      : Number of nodes over the half-height of the PCM capsule [-]
! nNodesPcmMax  : Maximum number of nodes over the half-height of the PCM capsule [-] (only for pre-allocating space for the matrices)
! nNodesFl      : Number of nodes over the length of the PCM tank [-]
! nNodesFlMax   : Maximum number of nodes over the length of the PCM tank [-] (only for pre-allocating space for the vectors)
! Nu 		    : Nusselt number for the heat trasnfer fluid [-]
! Pe		    : Peclet number for the heat transfer fluid [-]
! Pr		    : Prandtl number of the heat transfer fluid [-]
! Ra 		    : Rayliehg number for the heat transfer fluid [-]
! Rcap          : Equivalent thermal resistance for the capsule wall and void: Rcap = dW/kW + dVoid/kVoid [m^2-°C-s/J]
! rhoFl         : Heat transfer fluid density [kg/m^3]
! rhoLiqPcm     : Liquid PCM density [kg/m^3]
! rhoSolPcm     : Solid PCM density [kg/m^3]
! Sbp           : Surface area of the by-pass volume exchanging heat with the ambiant air [m^2]   
! SCp           : Solid specific heat [kJ/kg-K]
! Sin           : Surface area of the entry volume in contact with the environment surrounding it [m^2]    
! SOC           : PCM tank state of charge (aka liquid fraction of entire PCM volume) [-]
! Sout          : Surface area of the exit volume in contact with the environment surrounding it [m^2]      
! TaFl          : Vector of the average HTF temperature over the present time step [°C]
! TaPcm         : Vector of the average PCM temperature over the present time step [°C]
! Tenv          : Environment temperature surrounding the PCM tank [°C]
! TiBP          : Initial by-pass temperature for the present time step [°C] 
! TiFl          : Vector of initial HTF temperature for the present time step [°C] 
! TinFl         : HTF temperature entering the PCM tank at the present time step [°C]
! Tinit         : Initial temperature of the PCM tank (applicable to both PCM and HTF nodes) [°C]
! TiPcm         : Matrix of initial PCM temperature for the present time step [°C]
! TiVolIn       : Initial temperature of the entry length HTF volume for the present time step [°C]
! TiVolOut      : Initial temperature of the exit length HTF volume for the present time step [°C]
! TfBP          : Final by-pass temperatyre for the present time step [°C] 
! TfBPOld       : Final by-pass temperature from previous iteration, for convergence check [°C]   
! TfFl          : Matrix of final HTF temperature for the present time step and iteration [°C]
! TfFlOld       : Matrix of final HTF temperature from previous iteration, for convergence check [°C]
! TfPcm         : Matrix of final PCM temperature for the present time step and iteration [°C]
! TfPcmOld      : Vector of final PCM temperature from previous iteration, for convergence check [°C]
! TfVolIn       : Final temperature of the entry length HTF volume for the present time step [°C]
! TfVolInOld    : Final temperature of the entry length HTF volume from previous iteration, for convergence check [°C]
! TfVolOut      : Final temperature of the exit length HTF volume for the present time step [°C]
! TfVolOutOld   : Final temperature of the exit length HTF volume from previous iteration, for convergence check [°C] 
! TL2sPcm       : Temperature of the PCM at the beginning of the melting process (liquid to solid) [°C]
! Tpcm          : Temperature of the PCM calculated in transition from heating to cooling or vice versa [°C]
! TS2lPcm       : Temperature of the PCM at the beginning of the solification process (solid to liquid) [°C]
! T0            : Temperature point 0, common to both heating and cooling curves [°C] 
! T5            : Temperature point 5, common to both heating and cooling curves [°C] 
! TC1           : Temperature point 1, cooling curve [°C] 
! TC2           : Temperature point 2, cooling curve [°C] 
! TC3           : Temperature point 3, cooling curve [°C] 
! TC4           : Temperature point 4, cooling curve [°C]   
! TH1           : Temperature point 1, heating curve [°C] 
! TH2           : Temperature point 2, heating curve [°C] 
! TH3           : Temperature point 3, heating curve [°C] 
! TH4           : Temperature point 4, heating curve [°C]       
! Uenv          : Overall heat transfer coefficient from the heat transfer fluid to the tank environment [J/s-°C-m^2]
! Vbp           : By-pass volume [m^3]  
! VolIn         : Entry length volume [m^3]
! VolOut        : Exit length volume [m^3]
! wCap          : Width of PCM capsule [m]
! xstar 	    : Adimensionnal length [-]
!
! ----------------------------------------------------------------------------------------------------------------------
! Copyright © 2012. All rights reserved.

subroutine Type3260

! Export this subroutine for its use in external DLLs
!dec$attributes dllexport :: type3260

use TrnsysConstants
use TrnsysFunctions

implicit none

! Constants
integer, parameter :: nNodesFlMax = 100, nMaxIter = 100, nNodesPcmMax = 15
real(8) :: maxError = 0.001

! Inputs
real(8) :: TinFl, mDotFlTot, Tenv

! Parameters
real(8) :: mode
integer :: nCapW = 1
integer :: nCapL = 1
integer :: nCapD = 1
real(8) :: lCap, wCap, dCap, dFl
real(8) :: rhoFl, cpFl, kFl, muFl, beta
real(8) :: rhoSolPcm, kSolPcm, rhoLiqPcm, kLiqPcm
integer :: nNodesFL = nNodesFlMax
real(8) :: Tinit
real(8) :: Uenv
integer :: nNodePCM = nNodesPcmMax
real(8) :: mCap, HxCoef, Rcap, FlowS
real(8) :: VolInTot, VolOutTot, BP_f, Abp
real(8) :: H0, H5, HC1, HC2, HC3, HC4, HH1, HH2, HH3, HH4
real(8) :: T0, T5, TC1, TC2, TC3, TC4, TH1, TH2, TH3, TH4
integer :: Cinit = 0

! Outputs
real(8) :: ToutFl

! Local variables
real(8) :: time, mDotFl
real(8) :: Ax, APcm, L, dt, mPcm, dy
real(8) :: TiFl(nNodesFlMax), TaFl(nNodesFlMax), TfFl(nNodesFlMax), TfFlOld(nNodesFlMax)
real(8) :: TiPcm(nNodesPcmMax,nNodesFlMax), TaPcm(nNodesPcmMax,nNodesFlMax), TfPcm(nNodesPcmMax,nNodesFlMax), TfPcmOld(nNodesPcmMax,nNodesFlMax)
real(8) :: hiPcm(nNodesPcmMax,nNodesFlMax), haPcm(nNodesPcmMax,nNodesFlMax), hfPcm(nNodesPcmMax,nNodesFlMax), hfPcmOld(nNodesPcmMax,nNodesFlMax)
real(8) :: Sin, Sout, hcPcm, heq, kW, dW
real(8) :: fLPcm(nNodesPcmMax,nNodesFlMax), kPcm(nNodesPcmMax,nNodesFlMax)
real(8) :: nIter(1,nNodesFlMax), nIterVol(2), nIterBP
real(8) :: a, b, c, d
real(8) :: SOC
real(8) :: xstar, Pe, Ra, Gr, Pr, Nu
integer :: converged, i, j
real(8) :: ErrorTFl, ErrorTPcm, ErrorVol
character (len=maxMessageLength) :: aString
real(8) :: filR, dPcm
real(8) :: TiVolIn, TfVolIn, TIVolOut, TfVolOut, Lin, Lout, TfVolInOld, TfVolOutOld, VolIn, VolOut
real(8) :: Vbp, Lbp, Sbp, TiBP, TfBP, TfBPOld, mDotBP, mf
!Hysteresis
real(8) :: AvgCp, LCp, SCp 
integer :: Curvei(nNodesPcmMax,nNodesFlMax), Curvef(nNodesPcmMax,nNodesFlMax)
!Stability criteria
real(8) :: FoI, FoW, cpPcm

! Debugging
logical :: isDebug, isIoError
integer :: ioError, luDbgTaFl, luDbgTaPcm, luDbgfLPcm, luDbgTfFl, luDbgTfPcm, luDbgnIter
character(len=maxPathLength) :: dckFileName, dbgFileNameTaFl, dbgFileNameTaPcm, dbgFileNamefLPcm, dbgFileNameTfFl, dbgFileNameTfPcm, dbgFileNamenIter
character (len=maxFileWidth) :: fmtStr

! Validate inputs
real(8) :: cvV, cvFlow

! --- Version signing call: set the version number for this type -------------------------------------------------------

if ( GetIsVersionSigningTime() ) then

    call SetTypeVersion(17)
    return

endif


! --- First call of the simulation -------------------------------------------------------------------------------------

if ( GetIsFirstCallofSimulation() ) then

    call SetNumberofParameters(50)           
    call ReadParameters
    call SetNumberofInputs(3) 
    call SetNumberofDerivatives(0)          
    call SetNumberofOutputs(4)           
    call SetIterationMode(1)           
    call SetNumberStoredVariables(1,(2*nNodePCM+1)*nNodesFl+3)
    if ( isDebug ) then
        if ( GetIsReReadParameters() ) then
            ! If we are here there are several instances of this Type. Debugging will not work because extra outputs are not stored properly and there would be a file conflict
            call Messages(-1,'Only one instance of this Type is allowed in debugging mode','fatal',GetCurrentUnit(),GetCurrentType())
            return
	    endif
        ! Open output files for debugging info. Debugging file names = deck file name trimmed before the last '.' -Type3260Dbg-VarName.dat
        isIoError = .false.
        dckFileName = getDeckFileName()
        dckFileName = dckFileName(1:index(dckFileName,'.',back=.true.)-1)
        dbgFileNameTaFl = trim(dckFileName) // "-Type3260Dbg-TaFl.dat"
        open(newunit = luDbgTaFl, file = dbgFileNameTaFl, status = 'unknown', iostat = ioError)
        if ( ioError /= 0 ) isIoError = .true.
        dbgFileNameTaPcm = trim(dckFileName) // "-Type3260Dbg-TaPcm.dat"
        open(newunit = luDbgTaPcm, file = dbgFileNameTaPcm, status = 'unknown', iostat = ioError)
        if ( ioError /= 0 ) isIoError = .true.             
        dbgFileNamefLPcm = trim(dckFileName) // "-Type3260Dbg-fLPcm.dat"
        open(newunit = luDbgfLPcm, file = dbgFileNamefLPcm, status = 'unknown', iostat = ioError)
        if ( ioError /= 0 ) isIoError = .true.
        dbgFileNameTfFl = trim(dckFileName) // "-Type3260Dbg-TfFl.dat"
        open(newunit = luDbgTfFl, file = dbgFileNameTfFl, status = 'unknown', iostat = ioError)
        if ( ioError /= 0 ) isIoError = .true.
        dbgFileNameTfPcm = trim(dckFileName) // "-Type3260Dbg-TfPcm.dat"
        open(newunit = luDbgTfPcm, file = dbgFileNameTfPcm, status = 'unknown', iostat = ioError)
        if ( ioError /= 0 ) isIoError = .true.
        dbgFileNamenIter = trim(dckFileName) // "-Type3260Dbg-nIter.dat"
        open(newunit = luDbgnIter, file = dbgFileNamenIter, status = 'unknown', iostat = ioError)
        if ( ioError /= 0 ) isIoError = .true.
        if ( isIoError ) then
            call Messages(-1,'At least one of the debugging output file could not be opened','fatal',GetCurrentUnit(),GetCurrentType())
            return
        endif
        write(fmtStr,'(a,i,a,a)') '("time             TiVolIn  ",',nNodesFl,'("TaFl",i3.3,"  ")','("TiBP  TiVolOut"))'
        write(luDbgTaFl,fmtStr) (i, i = 1, nNodesFl)
        write(fmtStr,'(a,i,a,a)') '("time             TfVolIn  ",',nNodesFl,'("TfFl",i3.3,"  ")','("TfBP  TfVolOut"))'
        write(luDbgTfFl,fmtStr)  (i, i = 1, nNodesFl)
        write(luDbgTfPcm,'(a)', advance="no") 'time          '
        write(luDbgTaPcm,'(a)', advance="no") 'time          '
        write(luDbgfLPcm,'(a)', advance="no") 'time          '
        do i = 1,nNodesFl
            do j = 1,nNodePcm
                if ( (i==nNodesFl) .and. (j==nNodePcm) ) then
                    write(luDbgTfPcm, '(a,i3.3,a,i2.2,a)') 'TfPcm', i,'-',j,' '
                    write(luDbgTaPcm, '(a,i3.3,a,i2.2,a)') 'TaPcm', i,'-',j,' '
                    write(luDbgfLPcm, '(a,i3.3,a,i2.2,a)') 'lFPcm', i,'-',j,' '
                else
                    write(luDbgTfPcm, '(a,i3.3,a,i2.2,a)', advance="no") 'TfPcm', i,'-',j,' '
                    write(luDbgTaPcm, '(a,i3.3,a,i2.2,a)', advance="no") 'TaPcm', i,'-',j,' '
                    write(luDbgfLPcm, '(a,i3.3,a,i2.2,a)', advance="no") 'lFPcm', i,'-',j,' '
                    
                endif
            enddo    
        enddo
        write(fmtStr,'(a,i,a)') '(" time          ",',nNodesFl,'("nIterFlNode",i4.4,"  "))'
        write(luDbgnIter,fmtStr) (i, i = 1, nNodesFl)
    endif     
    return

endif


! --- Simulation start time --------------------------------------------------------------------------------------------

if ( GetIsStartTime() ) then
   
    call ReadParameters
    call ReadInputs
    call StabilityCriteria()
    
    do j = 1, nNodesFl
        call SetDynamicArrayValueThisIteration(j,Tinit)         !Initialize the vector of fluid control volume temperature to the intial tank temperature, Tinit.
        do i = 1, nNodePCM
            call SetDynamicArrayValueThisIteration(nNodesFl*i+j,Tinit)  !Initialize the matrix of PCM control volume temperature to the intial tank temperature, Tinit.
            hiPcm(i,j) = PcmEnthalpy(Tinit,Cinit)
            call SetDynamicArrayValueThisIteration((nNodePCM+i)*nNodesFl+3+j,hiPcm(i,j))  !Initialize the matrix of PCM curve parameters to either the cooling or heating curve.
        enddo
    enddo
    call SetDynamicArrayValueThisIteration((nNodePCM+1)*nNodesFl+1,Tinit)         !Initialize the entry volume temperature to the intial tank temperature, Tinit.
    call SetDynamicArrayValueThisIteration((nNodePCM+1)*nNodesFl+2,Tinit)         !Initialize the exit volume temperature to the intial tank temperature, Tinit.
    call SetDynamicArrayValueThisIteration((nNodePCM+1)*nNodesFl+3,Tinit)         !Initialize the by-pass volume temperature to the intial tank temperature, Tinit.
    
    call SetStaticArrayValue(1,1)
    call SetOutputValue(1,Tinit)            !Initialize tank output temperature to the intial tank temperature, Tinit. [degC]              
    call SetOutputValue(2,mdotFlTot*3600.0) !Initialize the output total mass flow rate to input total mass flow rate [kg/h]
    call SetOutputValue(3,0.0)              !Initialize heat storage rate for the present time step to zero [kJ/h]
    call SetOutputValue(4,0.0)              !Initialize the tank state of charge to zero [%]
    return
    
endif


! --- Very last call of the simulation ---------------------------------------------------------------------------------

if ( GetIsLastCallofSimulation() ) then
    
    if ( isDebug ) then
        close(luDbgTaFl)
        close(luDbgTaPcm)
        close(luDbgfLPcm)
        close(luDbgTfFl)
        close(luDbgTfPcm)
        close(luDbgnIter)
    endif
    return

endif


! --- End of a regular timestep ----------------------------------------------------------------------------------------

if ( GetIsEndOfTimestep() ) then

    time = GetSimulationTime() 
    if ( isDebug ) then
        write(fmtStr,'(a,i,a)') '(f13.6,',nNodesFl+3,'(f9.3," "))'
        write(luDbgTaFl,fmtStr) time, TiVolIn, (TaFl(i), i = 1, nNodesFl), TiBP, TiVolOut
        write(luDbgTfFl,fmtStr) time, TfVolIn, (TfFl(i), i = 1, nNodesFl), TfBP, TfVolOut
        write(luDbgnIter,fmtStr) time, (nIter(1,i), i = 1, nNodesFl)
        write(luDbgTaPcm,'(f13.6," ")', advance="no") time
        write(fmtStr,'(a,i,a)') '(',nNodePcm,'(f11.3," "))'
        do i = 1,nNodesFl
            if (i==nNodesFl) then
                write(luDbgTaPcm, fmtStr) (TaPcm(j,i), j = 1, nNodePCM)
            else
                write(luDbgTaPcm, fmtStr,advance="no") (TaPcm(j,i), j = 1, nNodePCM)                
            endif
        enddo    
        write(luDbgTfPcm,'(f13.6," ")', advance="no") time
        write(fmtStr,'(a,i,a)') '(',nNodePcm,'(f11.3," "))'
        do i = 1,nNodesFl
            if (i==nNodesFl) then
                write(luDbgTfPcm, fmtStr) (TfPcm(j,i), j = 1, nNodePCM)
            else
                write(luDbgTfPcm, fmtStr,advance="no") (TfPcm(j,i), j = 1, nNodePCM)                
            endif
        enddo  
        write(luDbgfLPcm,'(f13.6," ")', advance="no") time
        write(fmtStr,'(a,i,a)') '(',nNodePcm,'(f11.3," "))'
        do i = 1,nNodesFl
            if (i==nNodesFl) then
                write(luDbgfLPcm, fmtStr) (fLPcm(j,i), j = 1, nNodePCM)                
            else
                write(luDbgfLPcm, fmtStr,advance="no") (fLPcm(j,i), j = 1, nNodePCM)                
            endif
        enddo
    endif    
    
    converged = nint(GetStaticArrayValue(1))
    
    if (converged == 0) then
        write(aString,'("Internal iterations have not converged. The maximum number of iterations is currently set to ", g, " and can be changed in the source code (nMaxIter)")') nMaxIter
        call Messages(-1,aString,'warning',GetCurrentUnit(),GetCurrentType())
    endif

    return

endif


! --- regular iterative call -------------------------------------------------------------------------------------------

if ( GetIsReReadParameters() ) then
    call ReadParameters
endif

call ReadInputs

if (0.75 < GetSimulationTime() ) then
    continue
endif    


do j = 1, nNodesFl
    TiFl(j) = GetDynamicArrayValueLastTimestep(j)  
    do i = 1, nNodePCM
        TiPcm(i,j) = GetDynamicArrayValueLastTimestep(nNodesFl*i+j)
        hiPcm(i,j) = GetDynamicArrayValueLastTimestep((nNodePCM+i)*nNodesFl+3+j)
        Curvei(i,j) = PcmCurve(TiPcm(i,j),hiPcm(i,j))
        !hiPcm(i,j) = PcmEnthalpy(TiPcm(i,j),Curvei(i,j))
    enddo    
enddo
TiVolIn = GetDynamicArrayValueLastTimestep((nNodePCM+1)*nNodesFl+1)
TiVolOut = GetDynamicArrayValueLastTimestep((nNodePCM+1)*nNodesFl+2)  
TiBP = GetDynamicArrayValueLastTimestep((nNodePCM+1)*nNodesFl+3)

TaFl = TiFl                     ! Guess value 
TaPcm = TiPcm                   ! Guess value 
haPcm = hiPcm                   ! Guess value
TfFlOld = TiFl                  ! Guess value for convergence check
TfPcmOld = TiPcm                ! Guess value for convergence check
TfVolIn = TiVolIn               ! Guess value 
TfVolInOld = TiVolIn            ! Guess value for convergence check
TfVolOut = TiVolOut             ! Guess value 
TfVolOutOld = TiVolOut          ! Guess value for convergence check
TfBP = TiBP                     ! Guess value
TfBPOld = TiBP                  ! Guess value for convergence check

do j = 1, nNodesFl 
    do i = 1, nNodePCM
        if (nNodePcm==1) then
            kPcm(i,j) = PcmThermalConductivity( sum(haPcm)/dble(1*nNodesFl) )    ! Guess value based on average PCM enthalpy
        else    
            kPcm(i,j) = PcmThermalConductivity( sum(haPcm(2:nNodePcm,:))/dble((nNodePCM-1)*nNodesFl) + 0.5*sum(haPcm(1,:))/dble(1*nNodesFl) )    ! Guess value based on average PCM enthalpy
        endif
    enddo    
enddo

! Calculate constant coeficients
a = rhoFl*cpFl*Ax*L/dt
b = mDotFl*cpFl
c = kFl*Ax/L

! Evaluate the Peclet number value from : ( Reynolds number )*( Prandtl's number )
Pr = muFl*cpFl/kFl
Pe = (4.0*mDotFl/muFl/wCap/FlowS)*Pr
xstar = (dble(nCapL)*lCap) / (2.0*dFl*Pe)  

if ( Pe < 2.0 ) then    
    ! We consider the contribution of the fluid flow negligible vs the diffusion of heat amongst the fluid
    if (mDotFl > 0.0) then
        mDotFl = 0.0
        mDotFlTot = 0.0
        mDotBP = 0.0
        write(aString,'("The flowrate input into the model is very low and leads to unrealistic flow conditions. Please verify the input. In order for the simulation to continue, it has been forced to a value of 0 kg/s.")') 
        call Messages(-1,aString,'warning',GetCurrentUnit(),GetCurrentType())
    endif
        
    ! Loop through the whole tank and then check for convergence over the whole tank.

    converged = 0
    nIter = 0
    nIterVol = 0
    nIterBP = 0	

    ! Heat transfer coefficient for natural convection between fluid and PCM capsule wall 
    Gr = ( 9.81 * beta * (abs(sum(TaPcm(1,:))-sum(TaFl))/dble(nNodesFl)) * (lCap*dble(nCapL))**3.0 * rhoFl**2.0 )/muFl**2.0
    Ra = Gr*Pr

    ! Below is the correlation used for the IBPSA World article. It may not be the most appropriate.
    Nu = 0.5*1.0 + 0.5*0.069*sign(1.0d0,Ra)*abs(Ra)**(1.0/3.0)*Pr**(1.0/3.0) 
    
    hcPcm = Nu*kFl/(2.0*dFl)
    
    ! Overall heat transfer coefficient for heat transfer between the fluide and PCM
    ! Equivalent thermal resistance for the capsule wall and void: Rcap = dW/kW + dVoid/kVoid
    if (nNodePcm==1) then
        heq = 1 / ( 1/hcPcm + dPcm/(2.0*sum(kPcm)/dble(nNodesFl)) + Rcap)
    else
        heq = 1 / ( 1/hcPcm +Rcap )
    endif
    !Calculate variable coefficients	
    d = heq*Apcm*HxCoef
    
    ! Checking stability criteria for PCM control volume at the capsule wall (depends on convection coefficient)
    FoW = ( max(kLiqPcm, kSolPcm)*dt/(min(rhoLiqPcm, rhoSolPcm)*cpPcm*dy**2) )*( 1.0 + heq*dy/min(kLiqPcm, kSolPcm) )
    if (FoI < 1.0) then
        continue
    else
        call Messages(-1,'The stability criteria for the PCM control volume at the capsule wall is not respected. Either reduce the time step or the number of PCM control volumes.','fatal',GetCurrentUnit(),GetCurrentType())
    endif    

    do while ( (converged == 0) .and. (nIter(1,j) < nMaxIter) )   
        nIter = nIter + 1;
        TfVolIn = ( TiVolIn*( rhoFl*cpFl*VolIn/dt - 0.5*kFl*Ax/(Lin+0.5*L) -0.5*Uenv*Sin/(2.0*nCapD*nCapW) - 0.5*kFl*Abp/((Lbp+Lin)*2.0*dble(nCapW)*dble(nCapD)) ) + (0.5*TfFl(1)+0.5*TiFl(1))*kFl*Ax/(Lin+0.5*L) + Uenv*Tenv*Sin/(2.0*nCapD*nCapW) + (0.5*TfBP+0.5*TiBP)*kFl*Abp/((Lbp+Lin)*2.0*dble(nCapW)*dble(nCapD)) )/((rhoFl*cpFl*VolIn/dt) + 0.5*kFl*Ax/(Lin+0.5*L) + 0.5*Uenv*Sin/(2.0*nCapD*nCapW) + 0.5*kFl*Abp/((Lbp+Lin)*2.0*dble(nCapW)*dble(nCapD)))
        TfBP = ( TiBP*(rhoFl*cpFl*Vbp/dt - 0.5*kFl*Abp/(Lbp+Lout) - 0.5*kFl*Abp/(Lbp+Lin) - 0.5*Uenv*Sbp) + (0.5*TfVolOut+0.5*TiVolOut)*kFl*Abp/(Lbp+Lout) + (0.5*TfVolIn+0.5*TiVolIn)*kFl*Abp/(Lbp+Lin) + Uenv*Tenv*Sbp )/(rhoFl*cpFl*Vbp/dt + 0.5*kFl*Abp/(Lbp+Lout) + 0.5*kFl*Abp/(Lbp+Lin) + 0.5*Uenv*Sbp) 
        
        do j = 1, nNodesFl
		
            if (j == 1) then    ! node j-1 is VolIn
				TfFl(j) = ( TiFl(j)*(a-0.5*c-0.5*kFl*Ax/(Lin+0.5*L) -0.5*d) + (0.5*TfFl(j+1)+0.5*TiFl(j+1))*c + (0.5*TfVolIn+0.5*TiVolIn)*kFl*Ax/(Lin+0.5*L) + TaPcm(1,j)*d )/(a + 0.5*c + 0.5*kFl*Ax/(Lin+0.5*L) + 0.5*d)
            else if (j == nNodesFl) then ! node j+1 is VolOut
				TfFl(j) = ( TiFl(j)*(a-0.5*c-0.5*kFl*Ax/(Lout+0.5*L)-0.5*d) + (0.5*TfVolOut+0.5*TiVolOut)*kFl*Ax/(Lout+0.5*L) + (0.5*TfFl(j-1)+0.5*TiFl(j-1))*c + TaPcm(1,j)*d )/(a + 0.5*c + 0.5*kFl*Ax/(Lout+0.5*L) + 0.5*d)
            else
				TfFl(j) = ( TiFl(j)*(a-c-0.5*d) + (0.5*TfFl(j+1)+0.5*TiFl(j+1))*c + (0.5*TfFl(j-1)+0.5*TiFl(j-1))*c + TaPcm(1,j)*d )/(a + c + 0.5*d)
            endif

            ! Solve for average fluid temperature over the time step
            TaFl(j) = 0.5*( TiFl(j) + TfFl(j) )
            
            do i = 1, nNodePCM
                
                if (nNodePCM == 1) then
                    hfPcm(i,j) = hiPcm(i,j) + ( d*(TaFl(j)-TaPcm(i,j)) )*(dt/mPcm)
                else if (i == 1) then !Solve for node at PCM capsule wall
                    hfPcm(i,j) = hiPcm(i,j) + ( d*(TaFl(j)-TaPcm(i,j)) + (APcm/dy)*(2.0*kPcm(i+1,j)*kPcm(i,j)/(kPcm(i+1,j)+kPcm(i,j)))*(TaPcm(i+1,j)-TaPcm(i,j)) )*(2.0*dt/mPcm)
                else if (i == nNodePCM) then
                    hfPcm(i,j) = hiPcm(i,j) + ( (APcm/dy)*(2.0*kPcm(i-1,j)*kPcm(i,j)/(kPcm(i-1,j)+kPcm(i,j)))*(TaPcm(i-1,j)-TaPcm(i,j)) )*(dt/mPcm)
                else
                    hfPcm(i,j) = hiPcm(i,j) + ( (APcm/dy)*(2.0*kPcm(i+1,j)*kPcm(i,j)/(kPcm(i+1,j)+kPcm(i,j)))*(TaPcm(i+1,j)-TaPcm(i,j)) + (APcm/dy)*(2.0*kPcm(i-1,j)*kPcm(i,j)/(kPcm(i-1,j)+kPcm(i,j)))*(TaPcm(i-1,j)-TaPcm(i,j)) )*(dt/mPcm)
                endif    
            
                ! PCM temperature from enthalpy
                call PcmTemperature()
                ! PCM average data	
                TaPcm(i,j) = (TiPcm(i,j) + TfPcm(i,j)) / 2.0
                haPcm(i,j)= (hiPcm(i,j) + hfPcm(i,j)) /2.0
                ! PCM Liquid fraction and conductivity
                fLPcm(i,j) = PcmLiquidFraction(haPcm(i,j))
                kPcm(i,j) = PcmThermalConductivity(haPcm(i,j))
            
            enddo   ! i = 1,nNodePCM
        
        enddo  ! j = 1, nNodesFl
        
        TfVolOut = ( TiVolOut*(rhoFl*cpFl*VolOut/dt - 0.5*kFl*Ax/(Lout+0.5*L) - 0.5*Uenv*Sout/(2.0*nCapD*nCapW) - 0.5*kFl*Abp/((Lbp+Lout)*2.0*dble(nCapW)*dble(nCapD)) ) + (0.5*TfFl(nNodesFl)+0.5*TiFl(nNodesFl))*kFl*Ax/(Lout+0.5*L) + Tenv*Uenv*Sout/(2.0*nCapD*nCapW) + (0.5*TfBP+0.5*TiBP)*kFl*Abp/((Lbp+Lout)*2.0*dble(nCapW)*dble(nCapD)) )/(rhoFl*cpFl*VolOut/dt + 0.5*kFl*Ax/(Lout+0.5*L) + 0.5*Uenv*Sout/(2.0*nCapD*nCapW) + 0.5*kFl*Abp/((Lbp+Lout)*2.0*dble(nCapW)*dble(nCapD)))
 
        ! Convergence test
        ErrorTFl = maxval ( abs (TfFl - TfFlOld) )
        ErrorTPcm = maxval ( abs (TfPcm - TfPcmOld) )
        ErrorVol = max ( abs(TfVolIn - TfVolInOld), abs(TfVolOut - TfVolOutOld), abs(TfBP - TfBPOld) )
    
        if ( ( ErrorTFl <= maxError ) .and. ( ErrorTPcm <= maxError ) .and. ( ErrorVol <= maxError ) ) then
            converged = 1
        endif

        TfFlOld = TfFl
        TfPcmOld = TfPcm
        TfVolInOld = TfVolIn
        TfVolOutOld = TfVolOut
        TfBPOld = TfBP
    
    enddo  ! while

else  ! We consider the contribution of the diffusion of heat amongst the fluid negligible vs the contribution due to fluid flow
    
    ! Calculate each fluid transfer node and verify convergence before moving on the the next fluid node

    ! Heat transfer coefficient for forced convection between fluid and PCM capsule wall
    Nu = ( ( 1.849*(xstar**(-1.0/3.0)) )**3.5 + (7.541**3.5) )**(1.0/3.5)
    hcPcm = Nu*kFl/(2.0*dFl)

    ! Overall heat transfer coefficient for heat transfer between the fluide and PCM
    ! Equivalent thermal resistance for the capsule wall and void: Rcap = dW/kW + dVoid/kVoid 
    if (nNodePcm==1) then
        heq = 1 / ( 1/hcPcm + dPcm/(2.0*sum(kPcm)/dble(nNodesFl)) + Rcap)
    else
        heq = 1 / ( 1/hcPcm + Rcap )
    endif
    
    !Calculate variable coefficients	
    d = heq * APcm * HxCoef
    
    ! Checking stability criteria for PCM control volume at the capsule wall (depends on convection coefficient)
    FoW = ( max(kLiqPcm, kSolPcm)*dt/(min(rhoLiqPcm, rhoSolPcm)*cpPcm*dy**2) )*( 1.0 + heq*dy/min(kLiqPcm, kSolPcm) )
    if (FoI < 1.0) then
        continue
    else
        call Messages(-1,'The stability criteria for the PCM control volume at the capsule wall is not respected. Either reduce the time step or the number of PCM control volumes.','fatal',GetCurrentUnit(),GetCurrentType())
    endif    
    
    !Entry volume
    converged = 0
    nIterVol(1) = 0
    do while ( (converged == 0) .and. (nIterVol(1) < nMaxIter) )
        nIterVol(1) = nIterVol(1) + 1
        TfVolIn = ( TiVolIn*(rhoFl*cpFl*VolIn/dt -0.5*mDotFl*cpFl - 0.5*Uenv*Sin/(2.0*nCapD*nCapW)) + TinFl*mDotFl*cpFl + Tenv*Uenv*Sin/(2.0*nCapD*nCapW) ) / ( rhoFl*cpFl*VolIn/dt + 0.5*mDotFl*cpFl + 0.5*Uenv*Sin/(2.0*nCapD*nCapW) )
        !Convergence test
        ErrorVol = abs(TfVolIn - TfVolInOld)
        if ( ErrorVol <= maxError ) then
            converged = 1
        endif
        TfVolInOld = TfVolIn
    enddo
    
    !By-pass volume
    converged = 0
    nIterBP = 0
    do while ( (converged == 0) .and. (nIterBP < nMaxIter) )
        nIterBP = nIterBP + 1
        TfBP = ( TiBP*(rhoFl*cpFl*Vbp/dt - 0.5*BP_f*mDotFlTot*cpFl - 0.5*Uenv*Sbp) + (0.5*TfVolIn+0.5*TiVolIn)*BP_f*mDotFlTot*cpFl + Tenv*Uenv*Sbp ) / ( rhoFl*cpFl*Vbp/dt + 0.5*BP_f*mDotFlTot*cpFl + 0.5*Uenv*Sbp )
        !Convergence test
        ErrorVol = abs(TfBP - TfBPOld)
        if ( ErrorVol <= maxError ) then
            converged = 1
        endif
        TfBPOld = TfBP
    enddo
    
    do j = 1, nNodesFl
    
        converged = 0
        nIter(1,j) = 0 
        
        do i = 1, nNodePCM
            !First estimate of PCM Liquid fraction and conductivity
            fLPcm(i,j) = PcmLiquidFraction(hiPcm(i,j))
            kPcm(i,j) = PcmThermalConductivity(hiPcm(i,j)) 
        enddo    

        do while ( (converged == 0) .and. (nIter(1,j) < nMaxIter) )

            nIter(1,j) = nIter(1,j) + 1
            
            ! Solve fluid temperature    
            if (j == 1) then    ! node j-1 is VolIn         
                TfFl(j) = ( (a - 0.5*((1.0-BP_f)*b + d) )*TiFl(j) + (1.0-BP_f)*b*(0.5*TfVolIn+0.5*TiVolIn) + d*TaPcm(1,j) ) / (a + 0.5*((1.0-BP_f)*b+d))
            else
                TfFl(j) = ( (a - 0.5*((1.0-BP_f)*b + d) )*TiFl(j) + (1.0-BP_f)*b*(0.5*TiFl(j-1) + 0.5*TfFl(j-1)) + d*TaPcm(1,j) ) / (a + 0.5*((1.0-BP_f)*b+d))
            endif         
            TaFl(j) = 0.5*( TiFl(j) + TfFl(j) )
            
            do i = 1, nNodePCM
                
                if (nNodePCM == 1) then     !Solve for single PCM node 
                    hfPcm(i,j) = hiPcm(i,j) + ( d*(TaFl(j)-TaPcm(i,j)) )*(dt/mPcm)                
                else if (i == 1) then   !Solve for node at PCM capsule wall
                    hfPcm(i,j) = hiPcm(i,j) + (d*(TaFl(j)-TaPcm(i,j)) + (APcm/dy)*(2.0*kPcm(i+1,j)*kPcm(i,j)/(kPcm(i+1,j)+kPcm(i,j)))*(TaPcm(i+1,j)-TaPcm(i,j)))*(2.0*dt/mPcm)
                else if (i == nNodePCM) then        !Solve for PCM node adjacent to the capsule center line
                    hfPcm(i,j) = hiPcm(i,j) + ( (APcm/dy)*(2.0*kPcm(i-1,j)*kPcm(i,j)/(kPcm(i-1,j)+kPcm(i,j)))*(TaPcm(i-1,j)-TaPcm(i,j)) )*(dt/mPcm)
                else
                    hfPcm(i,j) = hiPcm(i,j) + ( (APcm/dy)*(2.0*kPcm(i+1,j)*kPcm(i,j)/(kPcm(i+1,j)+kPcm(i,j)))*(TaPcm(i+1,j)-TaPcm(i,j)) + (APcm/dy)*(2.0*kPcm(i-1,j)*kPcm(i,j)/(kPcm(i-1,j)+kPcm(i,j)))*(TaPcm(i-1,j)-TaPcm(i,j)) )*(dt/mPcm)
                endif    
            
                ! PCM heat balance
                call PcmTemperature()
                TaPcm(i,j) = (TiPcm(i,j) + TfPcm(i,j)) / 2.0
            
                ! PCM Liquid fraction and conductivity
                haPcm(i,j)= (hiPcm(i,j) + hfPcm(i,j)) /2.0
                fLPcm(i,j) = PcmLiquidFraction(haPcm(i,j))
                kPcm(i,j) = PcmThermalConductivity(haPcm(i,j))
            
            enddo   ! i = 1,nNodePCM
        
            ! Convergence test
            ErrorTFl = abs (TfFl(j) - TfFlOld(j)) 
            ErrorTPcm = maxval( abs(TfPcm(:,j) - TfPcmOld(:,j)) )
            
            if ( ( ErrorTFl <= maxError ) .and. ( ErrorTPcm <= maxError ) ) then
                converged = 1
            endif

            TfFlOld(j) = TfFl(j)
            TfPcmOld(1:nNodePCM,j) = TfPcm(1:nNodePCM,j)
        
        enddo  ! while

    enddo  ! j = 1, nNodesFl 
    
    !Exit volume
    converged = 0
    nIterVol(2) = 0
    do while ( (converged == 0) .and. (nIterVol(2) < nMaxIter) )
        nIterVol(2) = nIterVol(2) + 1
        TfVolOut = ( TiVolOut*(rhoFl*cpFl*VolOut/dt -0.5*(1.0-BP_f)*mDotFl*cpFl -0.5*BP_f*mDotFl*cpFl -0.5*Uenv*Sout/(2.0*nCapD*nCapW)) + (0.5*TfFl(nNodesFl)+0.5*TiFl(nNodesFl))*(1.0-BP_f)*mDotFl*cpFl + (0.5*TfBP+0.5*TiBP)*BP_f*mDotFl*cpFl + Tenv*Uenv*Sout/(2.0*nCapD*nCapW) ) / ( rhoFl*cpFl*VolOut/dt + 0.5*(1.0-BP_f)*mDotFl*cpFl + 0.5*BP_f*mDotFl*cpFl + 0.5*Uenv*Sout/(2.0*nCapD*nCapW) )
        !Convergence test
        ErrorVol = abs(TfVolOut - TfVolOutOld)
        if ( ErrorVol <= maxError ) then
            converged = 1
        endif
        TfVolOutOld = TfVolOut
    enddo
    
    

endif

call SetStaticArrayValue(1,dble(converged)) ! Store convergence status to trigger warning at end of time step call if required

do j = 1, nNodesFl
    call SetDynamicArrayValueThisIteration(j,TfFl(j))
    do i = 1,nNodePCM
        call SetDynamicArrayValueThisIteration(nNodesFl*i+j,TfPcm(i,j))         
        call SetDynamicArrayValueThisIteration((nNodePCM+i)*nNodesFl+3+j,hfPcm(i,j)) 
    enddo    
enddo
call SetDynamicArrayValueThisIteration((nNodePCM+1)*nNodesFl+1,TfVolIn)
call SetDynamicArrayValueThisIteration((nNodePCM+1)*nNodesFl+2,TfVolOut)
call SetDynamicArrayValueThisIteration((nNodePCM+1)*nNodesFl+3,TfBP)

if (nNodePcm == 1) then
    SOC = sum(fLPcm*mPcm)/dble(nNodesFl*mPcm)       !fLPcm is a row array 1 x nNodesFl so we make the sum over the whole array
else
    SOC = ( sum(fLPcm(2:nNodePcm,:)*mPcm) + sum(fLPcm(1,:)*mPcm*0.5) )  / ( (dble(nNodePCM)-1.0)*dble(nNodesFl)*mPcm + dble(nNodesFl)*mPcm*0.5 )     !fLPcm is a matrix of nNodePcm x nNodesFl
endif  

call SetOutputValue(1,0.5*TiVolOut+0.5*TfVolOut)
call SetOutputValue(2,mdotFlTot*3600.0)             !Internal units [kg/s] into TRNSYS units [kg/h]
call SetOutputValue(3,-mDotFlTot*3600.0*cpFl/1000.0*(0.5*TiVolOut+0.5*TfVolOut-TinFl))
call SetOutputValue(4,SOC)

return


!***********************************************************************************************************************

! ----------------------------------------------------------------------------------------------------------------------
! --- Internal functions and subroutines -------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
! The functions and subroutines here under are contained within the Type. They have access to all of its local variables
! through host association


contains

! ReadInputs: Subroutine to read all inputs 
! ----------------------------------------------------------------------------------------------------------------------

subroutine ReadInputs()

TinFl = GetInputValue(1)
mDotFlTot = GetInputValue(2)/3600.0                     !Total mass flow rate entering the PCM tank [kg/h], transformed into [kg/s]
mDotFl = mDotFlTot / (2.0*dble(nCapW) * dble(nCapD))    !Mass flow rate in each fluid control volume (half height between PCM capsules) [kg/s]
Tenv = GetInputValue(3)



return

end subroutine ReadInputs


! ReadParameters: Subroutine to read all parameters
! ----------------------------------------------------------------------------------------------------------------------


subroutine ReadParameters()

integer :: i

i = 0
! Mode
i = i+1; mode = getParameterValue(i)
! Tank parameters
i = i+1; nCapD = nint(getParameterValue(i))
i = i+1; nCapL = nint(getParameterValue(i))
i = i+1; nCapW = nint(getParameterValue(i))
i = i+1; lCap = getParameterValue(i)
i = i+1; wCap = getParameterValue(i)
i = i+1; dCap = getParameterValue(i)
i = i+1; dFl = getParameterValue(i)
! Fluid parameters
i = i+1; rhoFl = getParameterValue(i)               ![kg/m^3]
i = i+1; cpFl = getParameterValue(i) * 1000.0       ![kJ/kg-K] converted into [J/kg-K]
i = i+1; kFl = getParameterValue(i) / 3.6           ![kJ/h-m-K] converted into [J/s-m-K]
i = i+1; muFl = getParameterValue(i) / 3600.0       ![kg/m-h] converted into [kg/m-s]
i = i+1; beta = getParameterValue(i) 	            ![1/K]
! PCM parameters
i = i+1; rhoSolPcm = getParameterValue(i)           ![kg/m^3]
i = i+1; kSolPcm = getParameterValue(i) / 3.6       ![kJ/h-m-K] converted into [J/s-m-K]
i = i+1; rhoLiqPcm = getParameterValue(i)           ![kg/m^3]    
i = i+1; kLiqPcm = getParameterValue(i) / 3.6       ![kJ/h-m-K] converted into [J/s-m-K]
! Other modeling parameters
i = i+1; nNodesFl = nint(getParameterValue(i))      ![-]
i = i+1; Tinit = getParameterValue(i)               ![°C]
! Parameters for heat losses to the surroundings
i = i+1; Uenv = getParameterValue(i) / 3.6          ![kJ/h-K-m^2] converted into [J/s-K-m^2]
! Other modeling parameters
i = i+1; nNodePCM = nint(getParameterValue(i))      ![-]
i = i+1; mCap = getParameterValue(i)                ![kg]	 	
i = i+1; HxCoef = getParameterValue(i)              ![-]	 	
i = i+1; Rcap = getParameterValue(i)                ![m^2-°C/W]  
! Entry and exit length parameters
i = i+1; VolInTot = getParameterValue(i)            ![m^3] 
i = i+1; VolOutTot = getParameterValue(i)           ![m^3] 
! By-pass factor
i = i + 1; BP_f = getParameterValue(i)/100.0        ![%] converted to fraction between 0 and 1
i = i +1; Abp = getParameterValue(i)                ![m^2]
! Enthalpy-temperature curve points
i = i+1; H0 = getParameterValue(i) * 1000.0        ![kJ/kg] converted to [J/kg] 
i = i+1; T0 = getParameterValue(i)                 ![°C] 
i = i+1; HC1 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg] 
i = i+1; TC1 = getParameterValue(i)                ![°C] 
i = i+1; HC2 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg]   
i = i+1; TC2 = getParameterValue(i)                ![°C] 
i = i+1; HC3 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg] 
i = i+1; TC3 = getParameterValue(i)                ![°C] 
i = i+1; HC4 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg]   
i = i+1; TC4 = getParameterValue(i)                ![°C] 
i = i+1; H5 = getParameterValue(i) * 1000.0        ![kJ/kg] converted to [J/kg]  
i = i+1; T5 = getParameterValue(i)                 ![°C] 
i = i+1; HH1 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg] 
i = i+1; TH1 = getParameterValue(i)                ![°C] 
i = i+1; HH2 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg]   
i = i+1; TH2 = getParameterValue(i)                ![°C] 
i = i+1; HH3 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg] 
i = i+1; TH3 = getParameterValue(i)                ![°C] 
i = i+1; HH4 = getParameterValue(i) * 1000.0       ![kJ/kg] converted to [J/kg]   
i = i+1; TH4 = getParameterValue(i)                ![°C] 
i = i+1; Cinit = nint(getParameterValue(i))        ![-]   
i = i+1; FlowS = getParameterValue(i)              ![-] 
    
! Debug mode
if ( mode < 0 ) then
    isDebug = .true.
else
    isDebug = .false.
endif

! Assesment of the filling ratio of the capsules	 	
filR = ( mCap/(rhoLiqPcm*0.5 + rhoSolPcm*0.5) )/( lCap*wCap*dCap )	 	
if ( filR < 0.99 ) then	 	
    dPcm = ( mCap/(rhoLiqPcm*0.5 + rhoSolPcm*0.5) )/( lCap*wCap*2.0 )	 	
else	 	
    dPcm = dCap/2.0	 	
endif  

! Entry and exit length properties
VolIn = VolInTot/(2.0*dble(nCapW)*dble(nCapD))
VolOut = VolOutTot/(2.0*dble(nCapW)*dble(nCapD))
Lin = VolIn/(wCap*(0.5*dCap+0.5*dFl)*2.0)       ! Distance between entry volume node and entry volume surface
Lout = VolOut/(wCap*(0.5*dCap+0.5*dFl)*2.0)     ! Distance between exit volume node and exit volume surface
Sin = Abp + dble(nCapW)*wCap*dble(nCapD)*(dCap+dFl) + 2.0*(Lin*2.0*dble(nCapD)*(dCap+dFl)) + 2.0*(Lin*2.0*dble(nCapW)*wCap)     ! Entry volume surface area 
Sout = Abp + dble(nCapW)*wCap*dble(nCapD)*(dCap+dFl) + 2.0*(Lout*2.0*dble(nCapD)*(dCap+dFl)) + 2.0*(Lout*2.0*dble(nCapW)*wCap)  ! Exit volume surface area 

! By-pass node properties
Vbp = Abp*lCap*dble(nCapL)
Lbp = 0.5*lCap*dble(nCapL)
mf = sqrt(Abp/(dble(nCapW)*wCap*dble(nCapD)*(dCap+dFl))+1)   ! Multiplication factor between area of capsules and HTF flow and total tank area including by-pass
Sbp = (dble(nCapW)*wCap*mf*2.0+dble(nCapD)*(dCap+dFl)*mf*2.0)*(lCap*dble(nCapL))    ! Surface of heat exchanger between by-pass volume and ambiant air


! Fluid node parameters
Ax = wCap * dFl / 2.0
L = lCap*dble(nCapL) / dble(nNodesFl)
APcm = wCap * L
if (nNodePcm == 1) then
    dy = dPcm         ! dPcm is the half height of the PCM inside the capsule, dy is the height of the PCM node which is the full PCM half height
else    
    dy = 2.0*dPcm / (dble(nNodePCM)*2.0-1.0)      ! dPcm is the half height of the PCM inside the capsule, dy is the height of a full PCM nodes
endif

!Enthalpy-temperature curve parameters
AvgCp = ( 0.5*(HH1-HC1)/(TH1-TC1) + 0.5*(HH4-HC4)/(TH4-TC4) );
LCp = ( (HH4-HC4)/(TH4-TC4) );
SCp = ( (HH1-HC1)/(TH1-TC1) );

mPcm = (mCap*dble(nCapL))/(dble(nNodesFl)*(2.0*dble(nNodePcm)-1))

!Verification that initial state is on the heating or cooling curve
if ( (Cinit .NE. 0) .AND. (Cinit .NE. 1) ) then
    call Messages(-1,'Initial conditions must be either on the heating or cooling curve','fatal',GetCurrentUnit(),GetCurrentType())
endif 

dt = GetSimulationTimeStep() * 3600.0                 ![h] converted into [s]


return

end subroutine ReadParameters


! StabilityCriteria: Subroutine to verify stability criterias 
! ----------------------------------------------------------------------------------------------------------------------

subroutine StabilityCriteria()

! Checking Courant-Friedrichs-Lewy condition (i.e. control volumes will not be flushed within 1 time step)
cvV = wCap*(dFl/2)*(lCap*dble(nCapL)/dble(nNodesFl))
cvFlow = mDotFl*dt/rhoFl
if (cvFlow < cvV) then
    continue
else
    call Messages(-1,'The fluid control volume will be completly replaced within one time step, reduce either the time step or the number of nodes','fatal',GetCurrentUnit(),GetCurrentType())
endif 

! Checking stability criteria for interior PCM control volume 
cpPcm = min( (H5-HH4)/(T5-TH4), (HC1-H0)/(TC1-T0))  !PCM cp in liquid and solid states are compared
FoI = max(kLiqPcm, kSolPcm)*dt/(min(rhoLiqPcm, rhoSolPcm)*cpPcm*(dy/2.0)**2)
if (FoI < 1.0) then
    continue
else
    call Messages(-1,'The stability criteria for the interior PCM control volume is not respected. Either reduce the time step or the number of PCM control volumes.','fatal',GetCurrentUnit(),GetCurrentType())
endif

return

end subroutine StabilityCriteria


! PcmEnthalpy: PCM enthalpy for a given temperature
! ----------------------------------------------------------------------------------------------------------------------


real(8) function PcmEnthalpy(Tini, Cini)

real(8) :: Tini
integer :: Cini

select case (Cini)
    case (0)      !Heating
    
        if (Tini < TH1) then 
            PcmEnthalpy = ( (HH1-H0)/(TH1-T0) )*(Tini - T0) + H0
        elseif (Tini < TH2) then                              
            PcmEnthalpy = ( (HH2-HH1)/(TH2-TH1) )*(Tini - TH1) + HH1
        elseif (Tini < TH3) then
            PcmEnthalpy = ( (HH3-HH2)/(TH3-TH2) )*(Tini - TH2) + HH2
        elseif (Tini < TH4) then
            PcmEnthalpy = ( (HH4-HH3)/(TH4-TH3) )*(Tini - TH3) + HH3
        else 
            PcmEnthalpy = ( (H5-HH4)/(T5-TH4) )*(Tini - TH4) + HH4
        endif

    case (1)    !Cooling
            
        if (Tini < TC1) then   
            PcmEnthalpy = ( (HC1-H0)/(TC1-T0) )*(Tini - T0) + H0
        elseif (Tini < TC2) then                              
            PcmEnthalpy = ( (HC2-HC1)/(TC2-TC1) )*(Tini - TC1) + HC1
        elseif (Tini < TC3) then
            PcmEnthalpy = ( (HC3-HC2)/(TC3-TC2) )*(Tini - TC2) + HC2
        elseif (Tini < TC4) then
            PcmEnthalpy = ( (HC4-HC3)/(TC4-TC3) )*(Tini - TC3) + HC3
        else 
            PcmEnthalpy = ( (H5-HC4)/(T5-TC4) )*(Tini - TC4) + HC4
        endif 
        
    case (2)    !Transition
            
        call Messages(-1,'Error : The PCM must start the simulation either on the heating or freezing enthalpy-temperature curve.','fatal',GetCurrentUnit(),GetCurrentType())
        
end select
    
return

end function PcmEnthalpy


! Pcmtemperature: Subroutine to calculate PCM temperature for a given enthalpy
! ----------------------------------------------------------------------------------------------------------------------


subroutine PcmTemperature()          ![°C]

real(8) :: Tpcm, TH, TC

if ( (hiPcm(i,j) <= hfPcm(i,j) ) .and. (Curvei(i,j) == 0) ) then !Heating

    Curvef(i,j) = 0
    if ( hfPcm(i,j) < HH1 ) then
        TfPcm(i,j) = ( (TH1-T0)/(HH1-H0) )*(hfPcm(i,j) - H0) + T0
    elseif ( hfPcm(i,j) < HH2 ) then
        TfPcm(i,j) = ( (TH2-TH1)/(HH2-HH1) )*(hfPcm(i,j) - HH1) + TH1
    elseif ( hfPcm(i,j) < HH3 ) then
        TfPcm(i,j) = ( (TH3-TH2)/(HH3-HH2) )*(hfPcm(i,j) - HH2) + TH2
    elseif ( hfPcm(i,j) < HH4 ) then
        TfPcm(i,j) = ( (TH4-TH3)/(HH4-HH3) )*(hfPcm(i,j) - HH3) + TH3
    else 
        TfPcm(i,j) = ( (T5-TH4)/(H5-HH4) )*(hfPcm(i,j) - HH4) + TH4
    endif

elseif ( (hfPcm(i,j) <= hiPcm(i,j) ) .and. (Curvei(i,j) == 1) ) then !Cooling
   
    Curvef(i,j) = 1
    if ( hfPcm(i,j) < HC1 ) then
        TfPcm(i,j) = ( (TC1-T0)/(HC1-H0) )*(hfPcm(i,j) - H0) + T0
    elseif ( hfPcm(i,j) < HC2 ) then
        TfPcm(i,j) = ( (TC2-TC1)/(HC2-HC1) )*(hfPcm(i,j) - HC1) + TC1
    elseif ( hfPcm(i,j) < HC3 ) then
        TfPcm(i,j) = ( (TC3-TC2)/(HC3-HC2) )*(hfPcm(i,j) - HC2) + TC2
    elseif ( hfPcm(i,j) < HC4 ) then
        TfPcm(i,j) = ( (TC4-TC3)/(HC4-HC3) )*(hfPcm(i,j) - HC3) + TC3
    else 
        TfPcm(i,j) = ( (T5-TC4)/(H5-HC4) )*(hfPcm(i,j) - HC4) + TC4
    endif

elseif ( hiPcm(i,j) <= hfPcm(i,j) ) then !Cooling to heating curve

    if ( hfPcm(i,j) < HC1 ) then  !Both curves are the same, transition is instanteneous
        TfPcm(i,j) = ( (TC1-T0)/(HC1-H0) )*(hfPcm(i,j) - H0) + T0
        Curvef(i,j) = 0  
    elseif ( hfPcm(i,j) > HH4 ) then  !Both curves are the same, transition is instanteneous
        TfPcm(i,j) = ( (T5-TH4)/(H5-HH4) )*(hfPcm(i,j) - HH4) + TH4
        Curvef(i,j) = 0     
    elseif ( hfPcm(i,j) > HC4 ) then  !Slope during transition cannot be greater than liquid cp
        Tpcm = (hfPcm(i,j) - hiPcm(i,j))/min(LCp,AvgCp) + TiPcm(i,j)
        TH = ( (TH4-TH3)/(HH4-HH3) )*(hfPcm(i,j) - HH3) + TH3;
        if (TH >= TH4) then
            TfPcm(i,j) = ( (T5-TH4)/(H5-HH4) )*(hfPcm(i,j) - HH4) + TH4
            Curvef(i,j) = 0  
        elseif (Tpcm >= TH) then
            TfPcm(i,j) = TH
            Curvef(i,j) = 0
        else    
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2 
        endif    
    elseif ( (AvgCp < (HH1-hiPcm(i,j))/(TH1-TiPcm(i,j))) .and. (hiPcm(i,j) < HH1) ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TH = ( (TH1-T0)/(HH1-H0) )*(hfPcm(i,j) - H0) + T0
	    if (Tpcm >= TH) then
		    TfPcm(i,j) = TH
            Curvef(i,j) = 0
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
	    endif
    elseif ( (AvgCp < (HH2-hiPcm(i,j))/(TH2-TiPcm(i,j))) .and. (hiPcm(i,j) < HH2) ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TH = ( (TH2-TH1)/(HH2-HH1) )*(hfPcm(i,j) - HH1) + TH1
	    if (Tpcm >= TH) then
		    TfPcm(i,j) = TH
            Curvef(i,j) = 0
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
	    endif
    elseif ( (AvgCp < (HH3-hiPcm(i,j))/(TH3-TiPcm(i,j))) .and. (hiPcm(i,j) < HH3) ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TH = ( (TH3-TH2)/(HH3-HH2) )*(hfPcm(i,j) - HH2) + TH2
	    if (Tpcm >= TH) then
		    TfPcm(i,j) = TH
            Curvef(i,j) = 0
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
	    endif
    elseif ( (AvgCp < (HH4-hiPcm(i,j))/(TH4-TiPcm(i,j))) .and. (hiPcm(i,j) < HH4) ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TH = ( (TH4-TH3)/(HH4-HH3) )*(hfPcm(i,j) - HH3) + TH3
	    if (Tpcm >= TH) then
		    TfPcm(i,j) = TH
            Curvef(i,j) = 0
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
	    endif
    elseif (AvgCp < (HH4-hiPcm(i,j))/(TH4-TiPcm(i,j))) then
        TfPcm(i,j) = ( (TH4-TC4)/(HH4-HC4) )*(hfPcm(i,j) - HC4) + TC4
        Curvef(i,j) = 2
    else    
        call Messages(-1,'Error in calculating position on hvsT curve.','fatal',GetCurrentUnit(),GetCurrentType())
    endif             
    
elseif ( hfPcm(i,j) < hiPcm(i,j) ) then !Heating to cooling curve    
    
    if ( hfPcm(i,j) < HC1 ) then !Both curves are the same, transition is instanteneous
        TfPcm(i,j) = ( (TC1-T0)/(HC1-H0) )*(hfPcm(i,j) - H0) + T0
        Curvef(i,j) = 1
    elseif ( hfPcm(i,j) > HH4 ) then !Both curves are the same, transition is instanteneous
        TfPcm(i,j) = ( (T5-TH4)/(H5-HH4) )*(hfPcm(i,j) - HH4) + TH4
        Curvef(i,j) = 1
    elseif ( hfPcm(i,j) < HH1 ) then  !Slope during transition cannot be greater than solid cp   
        Tpcm = (hfPcm(i,j) - hiPcm(i,j))/min(AvgCp,SCp) + TiPcm(i,j)
        TC = ( (TC2-TC1)/(HC2-HC1) )*(hfPcm(i,j) - HC1) + TC1;
        if (TC <= TC1) then
            TfPcm(i,j) = ( (TC1-T0)/(HC1-H0) )*(hfPcm(i,j) - H0) + T0
            Curvef(i,j) = 1            
        elseif (Tpcm <= TC) then
            TfPcm(i,j) = TC
            Curvef(i,j) = 1
        else    
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2 
        endif
    elseif ( (AvgCp < (hiPcm(i,j)-HC4)/(TiPcm(i,j)-TC4)) .and. (hiPcm(i,j) > HC4)  ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TC = ( (TH4-TC4)/(HH4-HC4) )*(hfPcm(i,j) - HC4) + TC4
	    if (Tpcm <= TC) then
		    TfPcm(i,j) = TC
            Curvef(i,j) = 1
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
	    endif
    elseif ( (AvgCp < (hiPcm(i,j)-HC3)/(TiPcm(i,j)-TC3)) .and. (hiPcm(i,j) > HC3)  ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TC = ( (TC4-TC3)/(HC4-HC3) )*(hfPcm(i,j) - HC3) + TC3
	    if (Tpcm <= TC) then
		    TfPcm(i,j) = TC
            Curvef(i,j) = 1
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
	    endif
    elseif ( (AvgCp < (hiPcm(i,j)-HC2)/(TiPcm(i,j)-TC2)) .and. (hiPcm(i,j) > HC2)  ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TC = ( (TC3-TC2)/(HC3-HC2) )*(hfPcm(i,j) - HC2) + TC2
	    if (Tpcm <= TC) then
		    TfPcm(i,j) = TC
            Curvef(i,j) = 1
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
	    endif
    elseif ( (AvgCp < (hiPcm(i,j)-HC1)/(TiPcm(i,j)-TC1)) .and. (hiPcm(i,j) > HC1) ) then
	    Tpcm = (hfPcm(i,j) - hiPcm(i,j))/AvgCp + TiPcm(i,j)
        TC = ( (TC2-TC1)/(HC2-HC1) )*(hfPcm(i,j) - HC1) + TC1
	    if (Tpcm <= TC) then
		    TfPcm(i,j) = TC
            Curvef(i,j) = 1
       	else 
            TfPcm(i,j) = Tpcm
            Curvef(i,j) = 2
        endif
    elseif (AvgCp > (hiPcm(i,j)-HC1)/(TiPcm(i,j)-TC1)) then
        TfPcm(i,j) = ( (TH1-TC1)/(HH1-HC1) )*(hfPcm(i,j) - HC1) + TC1
        Curvef(i,j) = 2
    else
        call Messages(-1,'Error in calculating position on hvsT curve.','fatal',GetCurrentUnit(),GetCurrentType())
    endif    
else    
    call Messages(-1,'Error in calculating position on hvsT curve.','fatal',GetCurrentUnit(),GetCurrentType())
endif    

return

end subroutine PcmTemperature

! PcmCurve: PCM curve for a given temperature and enthalpy
! ----------------------------------------------------------------------------------------------------------------------


real(8) function PcmCurve(Tini, hini)

real(8) :: Tini, hini, hc, hh

if (Tini < TC1) then   
    hc = ( (HC1-H0)/(TC1-T0) )*(Tini - T0) + H0
elseif (Tini < TC2) then                              
    hc = ( (HC2-HC1)/(TC2-TC1) )*(Tini - TC1) + HC1
elseif (Tini < TC3) then
    hc = ( (HC3-HC2)/(TC3-TC2) )*(Tini - TC2) + HC2
elseif (Tini < TC4) then
    hc = ( (HC4-HC3)/(TC4-TC3) )*(Tini - TC3) + HC3
else 
    hc = ( (H5-HC4)/(T5-TC4) )*(Tini - TC4) + HC4
endif  

if (Tini < TH1) then 
    hh = ( (HH1-H0)/(TH1-T0) )*(Tini - T0) + H0
elseif (Tini < TH2) then                              
    hh = ( (HH2-HH1)/(TH2-TH1) )*(Tini - TH1) + HH1
elseif (Tini < TH3) then
    hh = ( (HH3-HH2)/(TH3-TH2) )*(Tini - TH2) + HH2
elseif (Tini < TH4) then
    hh = ( (HH4-HH3)/(TH4-TH3) )*(Tini - TH3) + HH3
else 
    hh = ( (H5-HH4)/(T5-TH4) )*(Tini - TH4) + HH4
endif

if ( abs(hini-hh) < 0.001 ) then
    PcmCurve = 0    !Heating
elseif ( abs(hini-hc) < 0.001) then
    PcmCurve = 1    !Cooling
else               
    PcmCurve = 2    !Transition
endif    


return

end function PcmCurve


! PcmLiquidFraction: PCM liquid fraction for a given temperature
! ----------------------------------------------------------------------------------------------------------------------


real(8) function PcmLiquidFraction(h)

real(8) :: h

if (h < HC1) then            !Solid 
    PcmLiquidFraction = 0.0  
elseif (h < HH4) then        !Liquid 
    PcmLiquidFraction = (h - HC1)/(HH4-HC1)
else                         !Mushy zone and transition
    PcmLiquidFraction = 1.0          
endif

return

end function PcmLiquidFraction


! PcmThermalConductivity: PCM thermal conductivity for a given temperature
! ----------------------------------------------------------------------------------------------------------------------


real(8) function PcmThermalConductivity(h)

real(8) :: h

if (h < HC1) then           !Solid
    PcmThermalConductivity = kSolPcm
elseif (h < HH4) then       !Mushy
    PcmThermalConductivity = kSolPcm + (h - HC1)*(kLiqPcm - kSolPcm) / (HH4 - HC1)
else                        !Liquid
    PcmThermalConductivity = kLiqPcm
endif

return

end function PcmThermalConductivity



end subroutine Type3260