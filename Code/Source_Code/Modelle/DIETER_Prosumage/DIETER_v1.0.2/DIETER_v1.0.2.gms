
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.0.2, January 2016.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
This version constitutes a minor revision of the model documented in Zerrahn, A., Schill, W.-P. (2015): A greenfield model to evaluate long-run power storage requirements for high shares of renewables. DIW Discussion Paper 1457. http://www.diw.de/documents/publikationen/73/diw_01.c.498475.de/dp1457.pdf
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************

**************************
***** GLOBAL OPTIONS *****
**************************

* Set star to skip Excel upload and load data from gdx
$setglobal skip_Excel ""

* Set star to write Excel output file
$setglobal write_to_excel "*"

* Set star to activate options
$setglobal DSM "*"
$setglobal reserves "*"

* Set star to select run-of-river options either as a simple exogenous parameter or as an endogenous variable including reserve provision:
* if nothing is selected, ROR capacity will be set to zero
* if parameter option is selected, set appropriate values in fix.gmx
* if variable option is selected, set appropriate bound in data_input excel
$setglobal ror_parameter ""
$setglobal ror_variable ""

* Set star to determine loops
$setglobal renewable_share "*"

* Set star to run test variant with each second hour
$setglobal second_hour ""

********************************************************************************

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal res_share ""
$setglobal em_share ""
$setglobal sec_hour "1"

%renewable_share%$ontext
$setglobal res_share ",loop_res_share"
$ontext
$offtext

%second_hour%$ontext
$setglobal sec_hour "8760/2208"
$ontext
$offtext

$if "%ror_parameter%" == "*" $if "%ror_variable%" == "*" $abort Choose appropriate ROR option! ;

********************************************************************************

**************************
***** SOLVER OPTIONS *****
**************************

options
optcr = 0.00
reslim = 10000000
lp = cplex
;

option
dispwidth = 15,
limrow = 0,
limcol = 0,
solprint = off,
sysout = off ,
threads = 2 ;

********************************************************************************

Sets
year      yearly time data                       /2011, 2012, 2013, 2013_windonsmooth/
ct        Conventional Technologies              /ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
res       Renewable technologies                 /Wind_on, Wind_off, Solar/
sto       Storage technolgies                    /Sto1*Sto7/
dsm_shift DSM shifting technologies              /DSM_shift1*DSM_shift5/
dsm_curt  Set of load curtailment technologies   /DSM_curt1*DSM_curt3/
reserves  Set of reserve qualities               /PR_up, PR_do, SR_up, SR_do, MR_up, MR_do/
%second_hour%h  hour                             /h1*h8760/
%second_hour%$ontext
$include second_hour.gms
$ontext
$offtext

loop_res_share    Solution loop for different shares of renewables       /res_share_0, res_share_60, res_share_70, res_share_80, res_share_90, res_share_100/
loop_em_share     Solution loop for different shares of renewables       /conv_only, em_share_0, em_share_20, em_share_40, em_share_60, em_share_80, em_share_100/
;

Alias (h,hh) ;
alias (res,resres) ;
alias (reserves,reservesreserves) ;

********************************************************************************

$include dataload.gms
*$stop

********************************************************************************

Variables
Z                Value objective function
;

Positive Variables
G_L(ct,h)        Generation level in hour h in MWh
G_UP(ct,h)       Generation upshift in hour h in MWh
G_DO(ct,h)       Generation downshift in hour h in MWh

G_RES(res,h)     Generation renewables type res in hour h in MWh
CU(res,h)        Renewables curtailment technology res in hour h in MWh

STO_IN(sto,h)    Storage inflow technology sto hour h in MWh
STO_OUT(sto,h)   Storage outflow technology sto hour h in MWh
STO_L(sto,h)     Storage level technology sto hour h in MWh

N_CON(ct)        Conventional technology ct built in MW
N_RES(res)       Renewable technology built in MW
N_STO_E(sto)     Storage technology built - Energy in MWh
N_STO_P(sto)     Storage loading and discharging capacity built - Capacity in MW

DSM_CU(dsm_curt,h)       DSM: Load curtailment hour h in MWh
DSM_UP(dsm_shift,h)      DSM: Load shifting up hour h technology dsm in MWh
DSM_DO(dsm_shift,h,hh)   DSM: Load shifting down in hour hh to account for upshifts in hour h technology dsm in MWh

DSM_UP_DEMAND(dsm_shift,h)   DSM: Load shifting up active for wholesale demand in hour h of technology dsm in MWh
DSM_DO_DEMAND(dsm_shift,h)   DSM: Load shifting down active for wholesale demand in hour h of technology dsm in MWh

N_DSM_CU(dsm_curt)        DSM: Load curtailment capacity in MW
N_DSM_SHIFT(dsm_shift)    DSM: Load shifting capacity in MWh

RP_CON(reserves,ct,h)                     Reserve provision by conventionals in hour h in MW
RP_RES(reserves,res,h)                    Reserve provision by renewables in hour h in MW
RP_STO_IN(reserves,sto,h)                 Reserve provision by storage in in hour h in MW
RP_STO_OUT(reserves,sto,h)                Reserve provision by storage out in hour h in MW
RP_DSM_CU(reserves,dsm_curt,h)            Reserve provision by DSM load curtailment in hour h in MW
RP_DSM_SHIFT(reserves,dsm_shift,h)        Reserve provision by DSM load shifting in hour h in MW
;

********************************************************************************


Equations
* Objective
obj                      Objective cost minimization

* Energy balance
con1a_bal                Supply Demand Balance in case of cost minimization

* Load change costs
con2a_loadlevel          Load change costs: Level
con2b_loadlevelstart     Load change costs: Level for first period

* Capacity contraints and flexibility constraints
con3a_maxprod_conv       Capacity Constraint conventionals
con3b_minprod_conv       Minimum production conventionals if reserves contracted

con3c_flex_PR_up        Flexibility of conventionals - provision PR up
con3d_flex_PR_do        Flexibility of conventionals - provision PR do
con3e_flex_SR_up        Flexibility of conventionals - provision SR up
con3f_flex_SR_do        Flexibility of conventionals - provision SR do
con3g_flex_MR_up        Flexibility of conventionals - provision MR up
con3h_flex_MR_do        Flexibility of conventionals - provision MR do

con3i_maxprod_ror        Capacity constraint Run-of-river
con3j_minprod_ror        Minimum production RoR if reserves contracted

con3k_maxprod_res        Capacity constraints renewables
con3l_minprod_res        Minimum production RES if reserves contracted

* Storage constraints
con4a_stolev_start        Storage Level Dynamics Initial Condition
con4b_stolev              Storage Level Dynamics

con4c_stolev_max          Storage Power Capacity
con4d_maxin_sto           Storage maximum inflow
con4e_maxout_sto          Storage maximum outflow
con4f_resrv_sto           Constraint on reserves (up)
con4g_resrv_sto           Constraint on reserves (down)

con4h_maxout_lev          Maximum storage outflow - no more than level of last period
con4i_maxin_lev           Maximum storage inflow - no more than ebergy capacity minus level of last period
con4j_ending              End level equal to initial level
con4k_PHS_EtoP            Maximum E to P ratio for PHS

* Minimum restrictions for renewables and biomass
con5a_minRES             Minimum yearly renewables requirement
con5b_maxBIO             Maximum yearly biomass energy

* DSM conditions: Load curtailment
con6a_DSMcurt_duration_max       Maximum curtailment energy budget per time
con6b_DSMcurt_max                Maximum curtailment per period

* DSM conditions: Load shifting
con7a_DSMshift_upanddown         Equalization of upshifts and downshifts in due time
con7b_DSMshift_granular_max      Maximum shifting in either direction per period
con7c_DSM_distrib_up             Distribution of upshifts between wholesale and reserves
con7d_DSM_distrib_do             Distribution of downshifts between wholesale and reserves
con7e_DSMshift_recovery          Recovery times

* Maximum installation conditions
con8a_max_I_con                 Maximum installable capacity: Conventionals
con8b_max_I_res                 Maximum installable capacity: Renewables
con8c_max_I_sto_e               Maximum installable energy: Storage in MWh
con8d_max_I_sto_p               Maximum installable capacity: Storage inflow-outflow in MW
con8e_max_I_dsm_cu              Maximum installable capacity: DSM load curtailment
con8f_max_I_dsm_shift_pos       Maximum installable capacity: DSM load shifting

* Reserves
con10a_reserve_prov             Reserve provision SR and MR
con10b_reserve_prov_PR          Reserve provision PR
;


********************************************************************************

* ---------------------------------------------------------------------------- *
***** Objective function *****
* ---------------------------------------------------------------------------- *

obj..
         Z =E=
                 sum( (ct,h) , c_m(ct)*G_L(ct,h) )
                 + sum( (ct,h)$(ord(h)>1) , c_up(ct)*G_UP(ct,h) )
                 + sum( (ct,h) , c_do(ct)*G_DO(ct,h) )
                 + sum( (res,h) , c_cu(res)*CU(res,h) )
                 + sum( (sto,h) , c_m_sto(sto) * ( STO_OUT(sto,h) + STO_IN(sto,h) ) )
%DSM%$ontext
                 + sum( (dsm_curt,h) , c_m_dsm_cu(dsm_curt)*DSM_CU(dsm_curt,h) )
                 + sum( (dsm_shift,h) , c_m_dsm_shift(dsm_shift) * DSM_UP_DEMAND(dsm_shift,h) )
                 + sum( (dsm_shift,h) , c_m_dsm_shift(dsm_shift) * DSM_DO_DEMAND(dsm_shift,h) )
$ontext
$offtext
                 + sum( ct , c_i(ct)*N_CON(ct) )
                 + sum( ct , c_fix_con(ct)*N_CON(ct) )

                 + sum( res , c_i_res(res)*N_RES(res) )
                 + sum( res , c_fix_res(res)*N_RES(res) )

                 + sum( sto , c_i_sto_e(sto)*N_STO_E(sto) )
                 + sum( sto , c_fix_sto(sto)/2*(N_STO_P(sto)+N_STO_E(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P(sto) )
%DSM%$ontext
                 + sum( dsm_curt , c_i_dsm_cu(dsm_curt)*N_DSM_CU(dsm_curt) )
                 + sum( dsm_curt , c_fix_dsm_cu(dsm_curt)*N_DSM_CU(dsm_curt) )
                 + sum( dsm_shift , c_i_dsm_shift(dsm_shift)*N_DSM_SHIFT(dsm_shift) )
                 + sum( dsm_shift , c_fix_dsm_shift(dsm_shift)*N_DSM_SHIFT(dsm_shift) )
$ontext
$offtext
%reserves%$ontext
                 - sum( (reserves,sto,h)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) , RP_STO_IN(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 + sum( (reserves,sto,h)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) , RP_STO_IN(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 + sum( (reserves,sto,h)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) , RP_STO_OUT(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 - sum( (reserves,sto,h)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) , RP_STO_OUT(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
                 + sum( (reserves,dsm_curt,h)$(ord(reserves) = 3 or ord(reserves) = 5) , RP_DSM_CU(reserves,dsm_curt,h) * phi_reserves_call(reserves,h) * c_m_dsm_cu(dsm_curt) )
                 + sum( (reserves,dsm_shift,h)$(ord(reserves) > 2) , RP_DSM_SHIFT(reserves,dsm_shift,h) * phi_reserves_call(reserves,h) * c_m_dsm_shift(dsm_shift) )
$ontext
$offtext
;


* ---------------------------------------------------------------------------- *
***** Energy balance and load levels *****
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(hh)..
         d(hh) + sum( sto , STO_IN(sto,hh) )
%DSM%$ontext
         + sum( dsm_shift , DSM_UP_DEMAND(dsm_shift,hh) )
$ontext
$offtext
         =E=
         sum( ct , G_L(ct,hh)) + sum( res , G_RES(res,hh)) + sum( sto , STO_OUT(sto,hh) )
%reserves%$ontext
* Balancing Correction Factor
        + sum( ct ,
        - RP_CON('PR_up',ct,hh) * phi_reserves_call('PR_up',hh)
        - RP_CON('SR_up',ct,hh) * phi_reserves_call('SR_up',hh)
        - RP_CON('MR_up',ct,hh) * phi_reserves_call('MR_up',hh)
        + RP_CON('PR_do',ct,hh) * phi_reserves_call('PR_do',hh)
        + RP_CON('SR_do',ct,hh) * phi_reserves_call('SR_do',hh)
        + RP_CON('MR_do',ct,hh) * phi_reserves_call('MR_do',hh) )
$ontext
$offtext
%DSM%$ontext
         + sum(dsm_curt, DSM_CU(dsm_curt,hh))
         + sum(dsm_shift, DSM_DO_DEMAND(dsm_shift,hh))
$ontext
$offtext
;

con2a_loadlevel(ct,h)$(ord(h) > 1)..
        G_L(ct,h) =E= G_L(ct,h-1) + G_UP(ct,h) - G_DO(ct,h)
;

con2b_loadlevelstart(ct,'h1')..
         G_L(ct,'h1') =E= G_UP(ct,'h1')
;


* ---------------------------------------------------------------------------- *
***** Hourly maximum generation caps and constraints related to reserves   *****
* ---------------------------------------------------------------------------- *

con3a_maxprod_conv(ct,h)$(ord(ct)>1 )..
        G_L(ct,h)
%reserves%$ontext
        + RP_CON('PR_up',ct,h)
        + RP_CON('SR_up',ct,h)
        + RP_CON('MR_up',ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
        =L= N_CON(ct)
;

con3b_minprod_conv(ct,h)..
        RP_CON('PR_do',ct,h)
        + RP_CON('SR_do',ct,h)
        + RP_CON('MR_do',ct,h)
        =L= G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h)
;

con3c_flex_PR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('PR_up',ct,h)
        =L= grad_per_min(ct) * 0.5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3d_flex_PR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('PR_do',ct,h)
        =L= grad_per_min(ct) * 0.5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3e_flex_SR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('SR_up',ct,h)
        =L= grad_per_min(ct) * 5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3f_flex_SR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('SR_do',ct,h)
        =L= grad_per_min(ct) * 5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3g_flex_MR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('MR_up',ct,h)
        =L= grad_per_min(ct) * 15 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3h_flex_MR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('MR_do',ct,h)
        =L= grad_per_min(ct) * 15 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

* Constraints on run of river
con3i_maxprod_ror(h)..
        G_L('ror',h)
%reserves%$ontext
        + RP_CON('PR_up','ror',h)
        + RP_CON('SR_up','ror',h)
        + RP_CON('MR_up','ror',h)
* Balancing Correction Factor
        - RP_CON('PR_up','ror',h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up','ror',h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up','ror',h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do','ror',h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do','ror',h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do','ror',h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
        =L= phi_ror(h)*N_CON('ror')
;

con3j_minprod_ror(h)..
        RP_CON('PR_do','ror',h)
        + RP_CON('SR_do','ror',h)
        + RP_CON('MR_do','ror',h)
        =L= G_L('ror',h)
* Balancing Correction Factor
        - RP_CON('PR_up','ror',h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up','ror',h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up','ror',h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do','ror',h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do','ror',h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do','ror',h) * phi_reserves_call('MR_do',h)
;


* Constraints on renewables
con3k_maxprod_res(res,h)..
        G_RES(res,h) + CU(res,h)
%reserves%$ontext
        + RP_RES('PR_up',res,h)
        + RP_RES('SR_up',res,h)
        + RP_RES('MR_up',res,h)
$ontext
$offtext
        =E= phi_res(res,h)*N_RES(res)
;

con3l_minprod_res(res,h)..
        RP_RES('PR_do',res,h)
        + RP_RES('SR_do',res,h)
        + RP_RES('MR_do',res,h)
        =L= G_RES(res,h)
;


* ---------------------------------------------------------------------------- *
***** Storage constraints *****
* ---------------------------------------------------------------------------- *

con4a_stolev_start(sto,'h1')..
        STO_L(sto,'h1') =E= phi_sto_ini(sto) * N_STO_E(sto) + STO_IN(sto,'h1')*(1+eta_sto(sto))/2 - STO_OUT(sto,'h1')/(1+eta_sto(sto))*2
;

con4b_stolev(sto,h)$( (ord(h)>1) )..
         STO_L(sto,h) =E= STO_L(sto,h-1) + STO_IN(sto,h)*(1+eta_sto(sto))/2 - STO_OUT(sto,h)/(1+eta_sto(sto))*2
%reserves%$ontext
        + sum( (reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) ,(RP_STO_IN(reserves,sto,h) * phi_reserves_call(reserves,h) )*(1+eta_sto(sto))/2 )
        - sum( (reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) ,(RP_STO_IN(reserves,sto,h) * phi_reserves_call(reserves,h) )*(1+eta_sto(sto))/2 )
        - sum( (reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) ,(RP_STO_OUT(reserves,sto,h) * phi_reserves_call(reserves,h) )/(1+eta_sto(sto))*2 )
        + sum( (reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) ,(RP_STO_OUT(reserves,sto,h) * phi_reserves_call(reserves,h) )/(1+eta_sto(sto))*2 )
$ontext
$offtext
;

con4c_stolev_max(sto,h)..
        STO_L(sto,h) =L= N_STO_E(sto)
;

con4d_maxin_sto(sto,h)..
        STO_IN(sto,h)
%reserves%$ontext
        + RP_STO_IN('PR_do',sto,h) + RP_STO_IN('SR_do',sto,h) + RP_STO_IN('MR_do',sto,h)
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4e_maxout_sto(sto,h)..
        STO_OUT(sto,h)
%reserves%$ontext
        + RP_STO_OUT('PR_up',sto,h) + RP_STO_OUT('SR_up',sto,h) + RP_STO_OUT('MR_up',sto,h)
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4f_resrv_sto(sto,h)..
        RP_STO_IN('PR_up',sto,h) + RP_STO_IN('SR_up',sto,h) + RP_STO_IN('MR_up',sto,h)
        =L= STO_IN(sto,h)
;

con4g_resrv_sto(sto,h)..
        RP_STO_OUT('PR_do',sto,h) + RP_STO_OUT('SR_do',sto,h) + RP_STO_OUT('MR_do',sto,h)
        =L= STO_OUT(sto,h)
;

con4h_maxout_lev(sto,h)..
        ( STO_OUT(sto,h)
%reserves%$ontext
        + RP_STO_OUT('PR_up',sto,h) + RP_STO_OUT('SR_up',sto,h) + RP_STO_OUT('MR_up',sto,h)
$ontext
$offtext
        ) /(1+eta_sto(sto))*2
        =L= STO_L(sto,h-1)
;

con4i_maxin_lev(sto,h)..
        ( STO_IN(sto,h)
%reserves%$ontext
        + RP_STO_IN('PR_do',sto,h) + RP_STO_IN('SR_do',sto,h) + RP_STO_IN('MR_do',sto,h)
$ontext
$offtext
        ) * (1+eta_sto(sto))/2
        =L= N_STO_E(sto) - STO_L(sto,h-1)
;

con4j_ending(sto,h)$( ord(h) = card(h) )..
         STO_L(sto,h) =E= phi_sto_ini(sto) * N_STO_E(sto)
;

con4k_PHS_EtoP('Sto5')..
        N_STO_E('Sto5') =L= etop_max('Sto5') * N_STO_P('Sto5')
;


* ---------------------------------------------------------------------------- *
***** Quotas for renewables and biomass *****
* ---------------------------------------------------------------------------- *

con5a_minRES..
sum( ct$(ord(ct) > 1 AND ord(ct) < card(ct)) , sum( h , G_L(ct,h) ) )
        =L= (1-phi_min_res) * sum( h , d(h) + sum( (sto) , STO_IN(sto,h) - STO_OUT(sto,h) )
%DSM%$ontext
         - sum( dsm_curt , DSM_CU(dsm_curt,h) )
$ontext
$offtext
%reserves%$ontext
        + phi_mean_reserves_call('PR_up') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES(res)/1000 ) ) )
        + phi_mean_reserves_call('SR_up') *( 1000 * phi_reserves_share('SR_up') * (reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * N_RES(res)/1000 ) ) )
        + phi_mean_reserves_call('MR_up') *( 1000 * phi_reserves_share('MR_up') * (reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * N_RES(res)/1000 ) ) )
        - phi_mean_reserves_call('PR_do') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES(res)/1000 ) ) )
        - phi_mean_reserves_call('SR_do') *( 1000 * phi_reserves_share('SR_do') * (reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * N_RES(res)/1000 ) ) )
        - phi_mean_reserves_call('MR_do') *( 1000 * phi_reserves_share('MR_do') * (reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * N_RES(res)/1000 ) ) )

       + sum( sto ,
       + RP_STO_IN('PR_do',sto,h) * phi_reserves_call('PR_do',h) + RP_STO_IN('SR_do',sto,h) * phi_reserves_call('SR_do',h) + RP_STO_IN('MR_do',sto,h) * phi_reserves_call('MR_do',h)
       - RP_STO_IN('PR_up',sto,h) * phi_reserves_call('PR_up',h) - RP_STO_IN('SR_up',sto,h) * phi_reserves_call('SR_up',h) - RP_STO_IN('MR_up',sto,h) * phi_reserves_call('MR_up',h)
       - RP_STO_OUT('PR_up',sto,h) * phi_reserves_call('PR_up',h) - RP_STO_OUT('SR_up',sto,h) * phi_reserves_call('SR_up',h) - RP_STO_OUT('MR_up',sto,h) * phi_reserves_call('MR_up',h)
       + RP_STO_OUT('PR_do',sto,h) * phi_reserves_call('PR_do',h) + RP_STO_OUT('SR_do',sto,h) * phi_reserves_call('SR_do',h) + RP_STO_OUT('MR_do',sto,h) * phi_reserves_call('MR_do',h) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
       - sum( dsm_curt , RP_DSM_CU('SR_up',dsm_curt,h) * phi_reserves_call('SR_up',h) )
       - sum( dsm_curt , RP_DSM_CU('MR_up',dsm_curt,h) * phi_reserves_call('MR_up',h) )
* ## Correction required in case eta_dsm_shift < 1
$ontext
$offtext
)
;

con5b_maxBIO..
         sum( h , G_L('bio',h) ) =L= m_con_e('bio')
;

* ---------------------------------------------------------------------------- *
***** DSM constraints - curtailment *****
* ---------------------------------------------------------------------------- *

con6a_DSMcurt_duration_max(dsm_curt,h)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + t_off_dsm_cu(dsm_curt) ) , DSM_CU(dsm_curt,hh)
%reserves%$ontext
         + RP_DSM_CU('SR_up',dsm_curt,hh) * phi_reserves_call('SR_up',hh)
         + RP_DSM_CU('MR_up',dsm_curt,hh) * phi_reserves_call('MR_up',hh)
$ontext
$offtext
         )
         =L= N_DSM_CU(dsm_curt) * t_dur_dsm_cu(dsm_curt)
;

con6b_DSMcurt_max(dsm_curt,h)..
        DSM_CU(dsm_curt,h)
%reserves%$ontext
        + RP_DSM_CU('SR_up',dsm_curt,h)
        + RP_DSM_CU('MR_up',dsm_curt,h)
$ontext
$offtext
          =L= N_DSM_CU(dsm_curt)
;


* ---------------------------------------------------------------------------- *
***** DSM constraints - shifting *****
* ---------------------------------------------------------------------------- *

con7a_DSMshift_upanddown(dsm_shift,h)..
         DSM_UP(dsm_shift,h) * eta_dsm_shift(dsm_shift) =E= sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(dsm_shift) ) , DSM_DO(dsm_shift,h,hh) )
;

con7b_DSMshift_granular_max(dsm_shift,h)..
         DSM_UP_DEMAND(dsm_shift,h) + DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + sum( reserves$(ord(reserves) > 2) , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
         =L= N_DSM_SHIFT(dsm_shift)
;

con7c_DSM_distrib_up(dsm_shift,h)..
         DSM_UP(dsm_shift,h) =E= DSM_UP_DEMAND(dsm_shift,h)
%reserves%$ontext
         + RP_DSM_SHIFT('SR_do',dsm_shift,h) * phi_reserves_call('SR_do',h)
         + RP_DSM_SHIFT('MR_do',dsm_shift,h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
;

con7d_DSM_distrib_do(dsm_shift,h)..
         sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(dsm_shift) ) , DSM_DO(dsm_shift,hh,h) )
                 =E=
         DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + RP_DSM_SHIFT('SR_up',dsm_shift,h) * phi_reserves_call('SR_up',h)
         + RP_DSM_SHIFT('MR_up',dsm_shift,h) * phi_reserves_call('MR_up',h)
$ontext
$offtext
;

con7e_DSMshift_recovery(dsm_shift,h)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + t_off_dsm_shift(dsm_shift) ) , DSM_UP(dsm_shift,hh))
         =L= N_DSM_SHIFT(dsm_shift) * t_dur_dsm_shift(dsm_shift)
;


* ---------------------------------------------------------------------------- *
***** Maximum installation constraints *****
* ---------------------------------------------------------------------------- *

con8a_max_I_con(ct)..
         N_CON(ct) =L= m_con(ct)
;

con8b_max_I_res(res)..
         N_RES(res) =L= m_res(res)
;

con8c_max_I_sto_e(sto)..
         N_STO_E(sto) =L= m_sto_e(sto)
;

con8d_max_I_sto_p(sto)..
         N_STO_P(sto) =L= m_sto_p(sto)
;

con8e_max_I_dsm_cu(dsm_curt)..
         N_DSM_CU(dsm_curt) =L= m_dsm_cu(dsm_curt)
;

con8f_max_I_dsm_shift_pos(dsm_shift)..
         N_DSM_SHIFT(dsm_shift) =L= m_dsm_shift(dsm_shift)
;

* ---------------------------------------------------------------------------- *
***** Reserve constraints *****
* ---------------------------------------------------------------------------- *

con10a_reserve_prov(reserves,h)$( ord(reserves) > 2)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h))
%DSM%$ontext
        + sum(dsm_curt, RP_DSM_CU(reserves,dsm_curt,h))$( ord(reserves) = 3 OR ord(reserves) = 5 )
        + sum(dsm_shift , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
        =E= (
            1000 * phi_reserves_share(reserves) * (
            reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES(res)/1000 ) ) )$(ord(h) > 1)
;

con10b_reserve_prov_PR(reserves,h)$( ord(reserves) < 3)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h) )
         =E= phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2), 1000 * phi_reserves_share(reservesreserves) * (
            reserves_intercept(reservesreserves) + sum( res , reserves_slope(reservesreserves,res) * N_RES(res)/1000 ) ) )$(ord(h) > 1)
;


********************************************************************************
***** MODEL *****
********************************************************************************

model DIETER /
obj

con1a_bal
con2a_loadlevel
con2b_loadlevelstart

con3a_maxprod_conv
con3i_maxprod_ror
con3k_maxprod_res

con4a_stolev_start
con4b_stolev
con4c_stolev_max
con4d_maxin_sto
con4e_maxout_sto
con4h_maxout_lev
con4i_maxin_lev
con4j_ending
con4k_PHS_EtoP

con5a_minRES
con5b_maxBIO

con8a_max_I_con
con8b_max_I_res
con8c_max_I_sto_e
con8d_max_I_sto_p

%DSM%$ontext
con6a_DSMcurt_duration_max
con6b_DSMcurt_max

con7a_DSMshift_upanddown
con7b_DSMshift_granular_max
con7c_DSM_distrib_up
con7d_DSM_distrib_do
* con_7e_DSMshift_recovery

con8e_max_I_dsm_cu
con8f_max_I_dsm_shift_pos
$ontext
$offtext

%reserves%$ontext
con3b_minprod_conv
con3c_flex_PR_up
con3d_flex_PR_do
con3e_flex_SR_up
con3f_flex_SR_do
con3g_flex_MR_up
con3h_flex_MR_do
con3j_minprod_ror
con3l_minprod_res

con4f_resrv_sto
con4g_resrv_sto

con10a_reserve_prov
con10b_reserve_prov_PR
$ontext
$offtext
/;


********************************************************************************
***** Options, fixings, report preparation *****
********************************************************************************

* Solver options
$onecho > cplex.opt
lpmethod 4
threads 2
parallelmode -1
$offecho

dieter.OptFile = 1;
dieter.holdFixed = 1 ;

* Fixings
$include fix.gms

* Parameters for the report file
parameter
corr_fac_con
corr_fac_res
corr_fac_sto
corr_fac_dsm_cu
corr_fac_dsm_shift
gross_energy_demand
calc_maxprice
calc_minprice
report
report_tech
report_tech_hours
report_hours
report_reserves
report_reserves_hours
report_reserves_tech
report_reserves_tech_hours
;

* Min and max for prices
calc_maxprice = 0 ;
calc_minprice = 1000 ;

* Parameters for default base year
d(h) = d_y('2013',h) ;
phi_res(res,h) = phi_res_y('2013',res,h) ;
phi_reserves_call(reserves,h) = phi_reserves_call_y('2013',reserves,h) ;
phi_mean_reserves_call(reserves) = phi_mean_reserves_call_y('2013',reserves) ;



********************************************************************************
***** Solve *****
********************************************************************************

* Default 100% RES if no loop over RES shares
%renewable_share%phi_min_res= 1 ;
%renewable_share%$goto skip_loop_res_share

* Loop over res shares
loop( loop_res_share ,
        phi_min_res= 0$(ord(loop_res_share) = 1)
         + 0.6$(ord(loop_res_share) = 2)
         + 0.7$(ord(loop_res_share) = 3)
         + 0.8$(ord(loop_res_share) = 4)
         + 0.9$(ord(loop_res_share) = 5)
         + 1$(ord(loop_res_share) = 6) ;
$label skip_loop_res_share

* Fix again within each loop
$include fix.gms

* Scenario within each loop
$include Scenario.gms

* Solve model
solve DIETER using lp minimizing Z ;

* Assign default correction factors
corr_fac_con(ct,h) = 0 ;
corr_fac_res(res,h) = 0 ;
corr_fac_sto(sto,h) = 0 ;
corr_fac_dsm_cu(dsm_curt,h) = 0 ;
corr_fac_dsm_shift(dsm_shift,h) = 0 ;

* Define gross energy demand for reporting, egual to equation 5a
gross_energy_demand = sum( h , d(h) + sum( (sto) , STO_IN.l(sto,h) - STO_OUT.l(sto,h) )
%DSM%$ontext
         - sum( dsm_curt , DSM_CU.l(dsm_curt,h) )
$ontext
$offtext
%reserves%$ontext
        + phi_mean_reserves_call('PR_up') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) )
        + phi_mean_reserves_call('SR_up') *( 1000 * phi_reserves_share('SR_up') * (reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * N_RES.l(res)/1000 ) ) )
        + phi_mean_reserves_call('MR_up') *( 1000 * phi_reserves_share('MR_up') * (reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * N_RES.l(res)/1000 ) ) )
        - phi_mean_reserves_call('PR_do') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) )
        - phi_mean_reserves_call('SR_do') *( 1000 * phi_reserves_share('SR_do') * (reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * N_RES.l(res)/1000 ) ) )
        - phi_mean_reserves_call('MR_do') *( 1000 * phi_reserves_share('MR_do') * (reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * N_RES.l(res)/1000 ) ) )

       + sum( sto ,
       + RP_STO_IN.l('PR_do',sto,h)*phi_reserves_call('PR_do',h) + RP_STO_IN.l('SR_do',sto,h)*phi_reserves_call('SR_do',h) + RP_STO_IN.l('MR_do',sto,h)*phi_reserves_call('MR_do',h)
       - RP_STO_IN.l('PR_up',sto,h)*phi_reserves_call('PR_up',h) - RP_STO_IN.l('SR_up',sto,h)*phi_reserves_call('SR_up',h) - RP_STO_IN.l('MR_up',sto,h)*phi_reserves_call('MR_up',h)
       - RP_STO_OUT.l('PR_up',sto,h)*phi_reserves_call('PR_up',h) - RP_STO_OUT.l('SR_up',sto,h)*phi_reserves_call('SR_up',h) - RP_STO_OUT.l('MR_up',sto,h)*phi_reserves_call('MR_up',h)
       + RP_STO_OUT.l('PR_do',sto,h)*phi_reserves_call('PR_do',h) + RP_STO_OUT.l('SR_do',sto,h)*phi_reserves_call('SR_do',h) + RP_STO_OUT.l('MR_do',sto,h)*phi_reserves_call('MR_do',h) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
       - sum( dsm_curt , RP_DSM_CU.l('SR_up',dsm_curt,h) * phi_reserves_call('SR_up',h) )
       - sum( dsm_curt , RP_DSM_CU.l('MR_up',dsm_curt,h) * phi_reserves_call('MR_up',h) )
$ontext
$offtext
)
;

* Determine correction factors
%reserves%$ontext
        corr_fac_con(ct,h) =
        - RP_CON.l('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON.l('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON.l('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON.l('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON.l('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON.l('MR_do',ct,h) * phi_reserves_call('MR_do',h)
;
        corr_fac_res(res,h) =
        - RP_RES.l('PR_up',res,h) * phi_reserves_call('PR_up',h)
        - RP_RES.l('SR_up',res,h) * phi_reserves_call('SR_up',h)
        - RP_RES.l('MR_up',res,h) * phi_reserves_call('MR_up',h)
        + RP_RES.l('PR_do',res,h) * phi_reserves_call('PR_do',h)
        + RP_RES.l('SR_do',res,h) * phi_reserves_call('SR_do',h)
        + RP_RES.l('MR_do',res,h) * phi_reserves_call('MR_do',h)
;
        corr_fac_sto(sto,h) =
        - RP_STO_IN.l('PR_up',sto,h) * phi_reserves_call('PR_up',h)
        - RP_STO_IN.l('SR_up',sto,h) * phi_reserves_call('SR_up',h)
        - RP_STO_IN.l('MR_up',sto,h) * phi_reserves_call('MR_up',h)
        - RP_STO_OUT.l('PR_up',sto,h) * phi_reserves_call('PR_up',h)
        - RP_STO_OUT.l('SR_up',sto,h) * phi_reserves_call('SR_up',h)
        - RP_STO_OUT.l('MR_up',sto,h) * phi_reserves_call('MR_up',h)
        + RP_STO_IN.l('PR_do',sto,h) * phi_reserves_call('PR_do',h)
        + RP_STO_IN.l('SR_do',sto,h) * phi_reserves_call('SR_do',h)
        + RP_STO_IN.l('MR_do',sto,h) * phi_reserves_call('MR_do',h)
        + RP_STO_OUT.l('PR_do',sto,h) * phi_reserves_call('PR_do',h)
        + RP_STO_OUT.l('SR_do',sto,h) * phi_reserves_call('SR_do',h)
        + RP_STO_OUT.l('MR_do',sto,h) * phi_reserves_call('MR_do',h)
;
%DSM%$ontext
        corr_fac_dsm_cu(dsm_curt,h) =
        - RP_DSM_CU.l('SR_up',dsm_curt,h) * phi_reserves_call('SR_up',h)
        - RP_DSM_CU.l('MR_up',dsm_curt,h) * phi_reserves_call('MR_up',h)
;
        corr_fac_dsm_shift(dsm_shift,h) =
        - RP_DSM_SHIFT.l('SR_up',dsm_shift,h) * phi_reserves_call('SR_up',h)
        - RP_DSM_SHIFT.l('MR_up',dsm_shift,h) * phi_reserves_call('MR_up',h)
        + RP_DSM_SHIFT.l('SR_do',dsm_shift,h) * phi_reserves_call('SR_do',h)
        + RP_DSM_SHIFT.l('MR_do',dsm_shift,h) * phi_reserves_call('MR_do',h)
;
$ontext
$offtext

* Report files
        report('model status'%res_share%%em_share%) = DIETER.modelstat ;
        report('solve time'%res_share%%em_share%) = DIETER.resUsd ;
        report('obj value'%res_share%%em_share%) = Z.l * %sec_hour% ;
        report_tech('capacities conventional'%res_share%%em_share%,ct)$(ord(ct) > 1 and not ord(ct)=card(ct)) =  N_CON.l(ct) ;
        report_tech('capacities renewable'%res_share%%em_share%,res) =  N_RES.l(res) ;
        report_tech('capacities renewable'%res_share%%em_share%,'bio') =  N_CON.l('bio') ;
        report_tech('capacities renewable'%res_share%%em_share%,'ror') = 0 + N_CON.l('ror') ;
        report_tech('capacities storage MW'%res_share%%em_share%,sto) =  N_STO_P.l(sto) ;
        report_tech('capacities storage MWh'%res_share%%em_share%,sto) =  N_STO_E.l(sto) * %sec_hour% ;
        report('gross_energy_demand'%res_share%%em_share%) = gross_energy_demand ;
        report_tech('conshares'%res_share%%em_share%,ct)$(not ord(ct)=card(ct)) = sum( h, G_L.l(ct,h) ) / gross_energy_demand ;
        report('conshare total'%res_share%%em_share%) = sum(ct, report_tech('conshares'%res_share%%em_share%,ct) ) ;
        report_tech('renshares'%res_share%%em_share%,res) = sum( h, G_RES.l(res,h) - corr_fac_res(res,h))/ gross_energy_demand ;
        report_tech('renshares'%res_share%%em_share%,'bio') = sum( h, G_L.l('bio',h) ) / gross_energy_demand ;
        report_tech('renshares'%res_share%%em_share%,'ror') = sum( h, G_L.l('ror',h) ) / gross_energy_demand ;
        report('renshare total'%res_share%%em_share%) = sum(res,report_tech('renshares'%res_share%%em_share%,res)) + report_tech('renshares'%res_share%%em_share%,'bio') + report_tech('renshares'%res_share%%em_share%,'ror') ;
        report_tech('curtailment of fluct res absolute'%res_share%%em_share%,res) =  sum(h,CU.l(res,h)) * %sec_hour% ;
        report_tech('curtailment of fluct res relative'%res_share%%em_share%,res)$report_tech('curtailment of fluct res absolute'%res_share%%em_share%,res) =  sum(h,CU.l(res,h))/( sum(h,G_RES.l(res,h) - corr_fac_res(res,h) ) + sum(h,CU.l(res,h)) ) ;
        report_tech_hours('generation conventional'%res_share%%em_share%,ct,h) =  G_L.l(ct,h) + corr_fac_con(ct,h) ;
        report_tech_hours('generation renewable'%res_share%%em_share%,'ror',h) = G_L.l('ror',h) ;
        report_tech_hours('generation renewable'%res_share%%em_share%,res,h) = G_RES.l(res,h) ;
        report_tech_hours('curtailment of fluct res'%res_share%%em_share%,res,h) =  CU.l(res,h) ;
        report_tech_hours('generation storage'%res_share%%em_share%,sto,h) =  STO_OUT.l(sto,h) ;
        report_tech_hours('storage loading'%res_share%%em_share%,sto,h) =  STO_IN.l(sto,h) ;
        report_tech_hours('storage level'%res_share%%em_share%,sto,h) =  STO_L.l(sto,h) ;
        report_hours('demand'%res_share%%em_share%,h) = d(h) ;
        report('curtailment of fluct res absolute'%res_share%%em_share%) = sum((res,h),CU.l(res,h)) * %sec_hour% ;
        report('curtailment of fluct res relative'%res_share%%em_share%)$report('curtailment of fluct res absolute'%res_share%%em_share%) = sum((res,h),CU.l(res,h))/( sum((res,h),G_RES.l(res,h) - corr_fac_res(res,h) ) + sum((res,h),CU.l(res,h)) ) ;
        report('bio not utilized absolute'%res_share%%em_share%)$(m_con_e('bio')) = (m_con_e('bio') - sum(h,G_L.l('bio',h))) * %sec_hour% ;
        report('bio not utilized relative'%res_share%%em_share%)$(m_con_e('bio')) = (m_con_e('bio') - sum(h,G_L.l('bio',h)))/m_con_e('bio') ;

        report_hours('price'%res_share%%em_share%,h) = -con1a_bal.m(h) ;
        loop(h,
                if(-con1a_bal.m(h) > calc_maxprice,
                calc_maxprice = -con1a_bal.m(h) ;);
                report('max price'%res_share%%em_share%) = calc_maxprice ;
        );
        report('mean price'%res_share%%em_share%) = -sum(h,con1a_bal.m(h))/card(h) ;
        loop(h,
                if(-con1a_bal.m(h) < calc_minprice,
                calc_minprice = -con1a_bal.m(h) ;);
                report('min price'%res_share%%em_share%) = calc_minprice ;
        );
        report('Capacity total'%res_share%%em_share%) = sum( ct , N_CON.l(ct) ) + sum( res , N_RES.l(res) ) + sum( sto , N_STO_P.l(sto) )
%DSM%$ontext
        + sum( dsm_curt , N_DSM_CU.l(dsm_curt) ) + sum( dsm_shift , N_DSM_SHIFT.l(dsm_shift) )
$ontext
$offtext
;
        report_tech('Capacity share'%res_share%%em_share%,ct) = N_CON.l(ct) / report('Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('Capacity share'%res_share%%em_share%,'ror') = N_CON.l('ror') / report('Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('Capacity share'%res_share%%em_share%,res) = N_RES.l(res) / report('Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('Capacity share'%res_share%%em_share%,sto) = N_STO_P.l(sto) / report('Capacity total'%res_share%%em_share%) + 1e-9 ;
        report('Energy total'%res_share%%em_share%) = (sum( h , d(h) ) + sum( (sto,h) , STO_IN.l(sto,h) )) * %sec_hour%
%DSM%$ontext
        + sum( (dsm_shift,h) , DSM_UP_DEMAND.l(dsm_shift,h) ) * %sec_hour%
$ontext
$offtext
%reserves%$ontext
       + sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) ) * phi_reserves_call('PR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_up') * ( reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('SR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_up') * ( reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('MR_up',h) )
       - sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) ) * phi_reserves_call('PR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_do') * ( reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('SR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_do') * ( reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('MR_do',h) )
$ontext
$offtext
;
        report_tech('Energy share'%res_share%%em_share%,ct) = sum( h , G_L.l(ct,h) ) / report('Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('Energy share'%res_share%%em_share%,'ror') = sum( h , G_L.l('ror',h) ) / report('Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('Energy share'%res_share%%em_share%,res) = sum( h , G_RES.l(res,h) - corr_fac_res(res,h) ) / report('Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('Energy share'%res_share%%em_share%,sto) = sum( h , STO_OUT.l(sto,h) - corr_fac_sto(sto,h) ) / report('Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('Storage out total wholesale'%res_share%%em_share%,sto) = sum(h, report_tech_hours('generation storage'%res_share%%em_share%,sto,h) ) * %sec_hour% ;
        report_tech('Storage in total wholesale'%res_share%%em_share%,sto) = sum(h, report_tech_hours('storage loading'%res_share%%em_share%,sto,h) ) * %sec_hour% ;
%reserves%$ontext
        report_tech('Storage positive reserves activation by storage in'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
        report_tech('Storage negative reserves activation by storage in'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
        report_tech('Storage positive reserves activation by storage out'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
        report_tech('Storage negative reserves activation by storage out'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
$ontext
$offtext
%reserves%        report_tech('Storage FLH'%res_share%%em_share%,sto)$N_STO_P.l(sto) = report_tech('Storage out total wholesale'%res_share%%em_share%,sto) / N_STO_P.l(sto) ;
%reserves%        report_tech('Storage cycles'%res_share%%em_share%,sto)$N_STO_E.l(sto) = report_tech('Storage out total wholesale'%res_share%%em_share%,sto) / N_STO_E.l(sto) * %sec_hour% ;
%reserves%$ontext
        report_tech('Storage FLH'%res_share%%em_share%,sto)$N_STO_P.l(sto) = ( report_tech('Storage out total wholesale'%res_share%%em_share%,sto)
                        + sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        - sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        ) / N_STO_P.l(sto) ;
        report_tech('Storage cycles'%res_share%%em_share%,sto)$N_STO_E.l(sto) = ( report_tech('Storage out total wholesale'%res_share%%em_share%,sto)
                        + sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        - sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        ) / N_STO_E.l(sto) * %sec_hour% ;
$ontext
$offtext
        report_tech('Storage EP-ratio'%res_share%%em_share%,sto)$N_STO_P.l(sto) = N_STO_E.l(sto) * %sec_hour% / N_STO_P.l(sto) ;

%reserves%$ontext
         report_reserves_tech('Reserves provision ratio'%res_share%%em_share%,reserves,ct)$(N_CON.l(ct)) = sum( h , RP_CON.l(reserves,ct,h) ) / sum( h , G_L.l(ct,h) + corr_fac_con(ct,h) ) ;
         report_reserves_tech('Reserves activation ratio'%res_share%%em_share%,reserves,ct)$(N_CON.l(ct)) = sum( h , RP_CON.l(reserves,ct,h) * phi_reserves_call(reserves,h)) / sum( h , G_L.l(ct,h) + corr_fac_con(ct,h) ) ;
         report_reserves_tech('Reserves provision ratio'%res_share%%em_share%,reserves,res)$(N_RES.l(res)) = sum( h , RP_RES.l(reserves,res,h) ) / sum( h , G_RES.l(res,h) ) ;
         report_reserves_tech('Reserves activation ratio'%res_share%%em_share%,reserves,res)$(N_RES.l(res)) = sum( h , RP_RES.l(reserves,res,h) * phi_reserves_call(reserves,h) ) / sum( h , G_RES.l(res,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage out positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage out negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage in positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) ) / sum( h , STO_IN.l(sto,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage in negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) ) / sum( h , STO_IN.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage out positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage out negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage in positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_IN.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage in negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_IN.l(sto,h) ) ;
$ontext
$offtext

%DSM%$ontext
        report('load curtailment absolute (non-reserves)'%res_share%%em_share%) =  sum((dsm_curt,h), DSM_CU.l(dsm_curt,h)) * %sec_hour% ;
        report('load shift pos absolute (non-reserves)'%res_share%%em_share%) =  sum((dsm_shift,h), DSM_UP_DEMAND.l(dsm_shift,h)) * %sec_hour% ;
        report('load shift neg absolute (non-reserves)'%res_share%%em_share%) =  sum((dsm_shift,h), DSM_DO_DEMAND.l(dsm_shift,h)) * %sec_hour% ;
        report_tech('capacities load curtailment'%res_share%%em_share%,dsm_curt) =  N_DSM_CU.l(dsm_curt) ;
        report_tech('capacities load shift'%res_share%%em_share%,dsm_shift) =  N_DSM_SHIFT.l(dsm_shift) ;
        report_tech_hours('load curtailment (non-reserves)'%res_share%%em_share%,dsm_curt,h) = DSM_CU.l(dsm_curt,h) ;
        report_tech_hours('load shift pos (non-reserves)'%res_share%%em_share%,dsm_shift,h) = DSM_UP_DEMAND.l(dsm_shift,h) ;
        report_tech_hours('load shift neg (non-reserves)'%res_share%%em_share%,dsm_shift,h) = DSM_DO_DEMAND.l(dsm_shift,h) ;
        report_tech('Capacity share'%res_share%%em_share%,dsm_curt) = N_DSM_CU.l(dsm_curt) / report('Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('Capacity share'%res_share%%em_share%,dsm_shift) = N_DSM_SHIFT.l(dsm_shift) / report('Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('Energy share'%res_share%%em_share%,dsm_curt) = sum( h , DSM_CU.l(dsm_curt,h) - corr_fac_dsm_cu(dsm_curt,h) )  / report('Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('Energy share'%res_share%%em_share%,dsm_shift) = sum( h , DSM_DO_DEMAND.l(dsm_shift,h) - corr_fac_dsm_shift(dsm_shift,h) ) / report('Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('Load shift pos absolute (total)'%res_share%%em_share%,dsm_shift) = sum( h , DSM_UP.l(dsm_shift,h) ) ;
        report_tech('Load shift neg absolute (total)'%res_share%%em_share%,dsm_shift) = sum( (h,hh) , DSM_DO.l(dsm_shift,h,hh) ) ;
        report_tech('Load shift pos absolute (wholesale)'%res_share%%em_share%,dsm_shift) = sum( h , DSM_UP_DEMAND.l(dsm_shift,h) ) ;
        report_tech('Load shift neg absolute (wholesale)'%res_share%%em_share%,dsm_shift) = sum( h , DSM_DO_DEMAND.l(dsm_shift,h) ) ;
        report_tech('Load shift pos absolute (reserves)'%res_share%%em_share%,dsm_shift) = report_tech('Load shift pos absolute (total)'%res_share%%em_share%,dsm_shift) - report_tech('Load shift pos absolute (wholesale)'%res_share%%em_share%,dsm_shift) ;
        report_tech('Load shift neg absolute (reserves)'%res_share%%em_share%,dsm_shift) = report_tech('Load shift neg absolute (total)'%res_share%%em_share%,dsm_shift) - report_tech('Load shift neg absolute (wholesale)'%res_share%%em_share%,dsm_shift) ;
$ontext
$offtext

%reserves%$ontext
        report_reserves('reserve provision requirements'%res_share%%em_share%,reserves)$(ord(reserves) < 3) = phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) ) ;
        report_reserves('reserve provision requirements'%res_share%%em_share%,reserves)$(ord(reserves) > 2) = 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) ) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,ct)$(ord(reserves) < 3) = sum( h , RP_CON.l(reserves,ct,h)) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (card(h) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,res)$(ord(reserves) < 3) = sum( h , RP_RES.l(reserves,res,h)) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (card(h) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,sto)$(ord(reserves) < 3) = sum( h , RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h)) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (card(h) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) ));
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,ct)$( ord(reserves) > 2 ) = sum( h , RP_CON.l(reserves,ct,h)) / (card(h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) ) ) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,res)$( ord(reserves) > 2 ) = sum( h , RP_RES.l(reserves,res,h)) / (card(h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,sto)$( ord(reserves) > 2 ) = sum( h , RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h)) / (card(h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,ct)$(ord(reserves) < 3) = sum(h,RP_CON.l(reserves,ct,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,res)$(ord(reserves) < 3) = sum(h,RP_RES.l(reserves,res,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,sto)$(ord(reserves) < 3) = sum(h,(RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000 ) ) )   );
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,ct)$( ord(reserves) > 2 ) = sum(h,RP_CON.l(reserves,ct,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,res)$( ord(reserves) > 2 ) = sum(h,RP_RES.l(reserves,res,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,sto)$( ord(reserves) > 2 ) = sum(h,(RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,'required',h) = report_reserves('reserve provision requirements'%res_share%%em_share%,reserves) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,'required',h) = phi_reserves_call(reserves,h) * report_reserves('reserve provision requirements'%res_share%%em_share%,reserves) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,ct,h) = RP_CON.l(reserves,ct,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,ct,h) = RP_CON.l(reserves,ct,h)*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,res,h) = RP_RES.l(reserves,res,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,res,h) = RP_RES.l(reserves,res,h)*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,sto,h) = RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,sto,h) = (RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h))*phi_reserves_call(reserves,h) ;
$ontext
$offtext

%DSM%$ontext
%reserves%$ontext
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,dsm_curt)$( ord(reserves) > 2 ) = sum( h , RP_DSM_CU.l(reserves,dsm_curt,h)) / ( card(h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,dsm_shift)$( ord(reserves) > 2 ) = sum( h , RP_DSM_SHIFT.l(reserves,dsm_shift,h)) / ( card(h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,dsm_curt)$( ord(reserves) > 2 ) = sum( h , RP_DSM_CU.l(reserves,dsm_curt,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,dsm_shift)$( ord(reserves) > 2 ) = sum( h , RP_DSM_SHIFT.l(reserves,dsm_shift,h) * phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,dsm_shift,h)$(ord(reserves) > 2) = RP_DSM_SHIFT.l(reserves,dsm_shift,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,dsm_shift,h)$(ord(reserves) > 2) = RP_DSM_SHIFT.l(reserves,dsm_shift,h)*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,dsm_curt,h)$(ord(reserves) > 2) = RP_DSM_CU.l(reserves,dsm_curt,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,dsm_curt,h)$(ord(reserves) > 2) = RP_DSM_CU.l(reserves,dsm_curt,h)*phi_reserves_call(reserves,h) ;
$ontext
$offtext

* Clear variables and equations to speed up calculation
$include clear.gms

* Close loop_res_share
%renewable_share%$ontext
);
$ontext
$offtext

* Read out solutions
%reserves%execute_unload "results", report, report_tech, report_tech_hours, report_hours ;
%reserves%$ontext
execute_unload "results", report, report_tech, report_tech_hours, report_hours, report_reserves, report_reserves_tech, report_reserves_tech_hours ;
$ontext
$offtext

%write_to_excel%$ontext
$include report_to_excel
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
