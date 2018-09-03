
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.2.0, February 2017.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************

**************************
***** GLOBAL OPTIONS *****
**************************

* Set star to skip Excel upload and load data from gdx
$setglobal skip_Excel ""

* Choose base year
$setglobal base_year "'2013'"

* Set star to activate options
$setglobal DSM ""
$setglobal reserves ""
$setglobal EV ""
$setglobal prosumage "*"

$setglobal EV_EXOG ""

* Set star to indicate renewables constraint on electric vehicles - DEFAULT is same quota as for the rest of the electricity system
$setglobal EV_DEFAULT ""
$setglobal EV_100RES ""
$setglobal EV_FREE ""

* Set star to select run-of-river options either as a simple exogenous parameter or as an endogenous variable including reserve provision:
* if nothing is selected, ROR capacity will be set to zero
* if parameter option is selected, set appropriate values in fix.gmx
* if variable option is selected, set appropriate bound in data_input excel
$setglobal ror_parameter "*"
$setglobal ror_variable ""

* Set star to determine loops, otherwise default 100% renewables
$setglobal loop_over_renewable_share "*"

* Set star to run test variant with each second hour
$setglobal second_hour ""

* Set star for no crossover to speed up calculation time by skipping crossover in LP solver
$setglobal no_crossover "*"

* Set reporting sensitivity. All results below will be reported as zero
Scalar eps_rep_rel Sensitivity for shares defined between 0 and 1        / 1e-4 / ;
Scalar eps_rep_abs Sensitivity for absolute values - e.g. hourly         / 1e-2 / ;
Scalar eps_rep_ins Sensitivity for absolute values - e.g. installed MW   / 1 /    ;

********************************************************************************

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal sec_hour "1"

%second_hour%$ontext
$setglobal sec_hour "8760/2208"
$ontext
$offtext

* Sanity checks
$if "%ror_parameter%" == "*" $if "%ror_variable%" == "*" $abort Choose appropriate ROR option! ;

$if "%EV%" == "" $if "%EV_EXOG%" == "*" $abort Switch on EV! ;

$if "%EV%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "" $abort Choose exactly one EV option! ;
$if "%EV%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "**" $abort Choose exactly one EV option! ;
$if "%EV%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "***" $abort Choose exactly one EV option! ;

$if "%EV_EXOG%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "" $abort Choose exactly one EV option! ;
$if "%EV_EXOG%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "**" $abort Choose exactly one EV option! ;
$if "%EV_EXOG%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "***" $abort Choose exactly one EV option! ;

********************************************************************************

**************************
***** SOLVER OPTIONS *****
**************************

options
optcr = 0.00
reslim = 10000000
lp = cplex
mip = cplex
nlp = conopt
;

options
dispwidth = 15
limrow = 0
limcol = 0
solprint = off
sysout = off
;

********************************************************************************

Sets
year      yearly time data                       /2011, 2012, 2013, 2013_windonsmooth/
ct        Dispatchable Technologies              /ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
 ct_ren(ct)       Renewable dispatchable technologies    /ror, bio/
 ct_the(ct)       Thermal dispatchable technologies      /nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff/

res       Renewable technologies                 /Wind_on, Wind_off, Solar/
sto       Storage technolgies                    /Sto1*Sto7/
ev        Set of 28 EV profiles                  /ev1*ev28/
dsm_shift DSM shifting technologies              /DSM_shift1*DSM_shift5/
dsm_curt  Set of load curtailment technologies   /DSM_curt1*DSM_curt3/
reserves  Set of reserve qualities               /PR_up, PR_do, SR_up, SR_do, MR_up, MR_do/
 reserves_up(reserves)  Set of positive reserve qualities       /PR_up, SR_up, MR_up/
 reserves_do(reserves)  Set of positive reserve qualities       /PR_do, SR_do, MR_do/
 reserves_spin(reserves)    Set of spinning reserves            /PR_up, PR_do, SR_up, SR_do/
 reserves_nonspin(reserves) Set of nonspinning reserves         /MR_up, MR_do/

%second_hour%h  hour                             /h1*h8760/
%second_hour%$ontext
$include second_hour.gms
$ontext
$offtext

%loop_over_renewable_share%$ontext
loop_res_share   Solution loop for different shares of renewables       /eps/
$ontext
$offtext

%EV%$ontext
loop_ev          Solution loop for different fleets of EVs              /0, 1e+6, 2e+6, 4e+6, 8e+6, 16e+6, 32e+6/
$ontext
$offtext

%prosumage%$ontext
loop_prosumage   Solution loop for different prosumer self-consumption levels    /40, 45, 50, 55, 60, 65, 70/
$ontext
$offtext

%loop_over_renewable_share%      loop_res_share                          /100/
%EV%                             loop_ev                                 /0/
%prosumage%                      loop_prosumage                          /0/
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

EV_CHARGE(ev,h)         Electric vehicle charging vehicle profile ev hour h in MWh
EV_DISCHARGE(ev,h)      Electric vehicle discharging vehicle profile ev hour h in MWh
EV_L(ev,h)              Electric vehicle charging level vehicle profile ev hour h in MWh
EV_PHEVFUEL(ev,h)       Plug in hybrid electric vehicle conventional fuel use vehicle profile ev hour h in MWh
EV_GED(ev,h)            Grid electricity demand for mobility vehicle profile ev hour h in MWh

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
RP_EV_V2G(reserves,ev,h)                  Reserve provision by electric vehicles V2G hour h in MW
RP_EV_G2V(reserves,ev,h)                  Reserve provision by electric vehicles G2V hour h in MW
RP_DSM_CU(reserves,dsm_curt,h)            Reserve provision by DSM load curtailment in hour h in MW
RP_DSM_SHIFT(reserves,dsm_shift,h)        Reserve provision by DSM load shifting in hour h in MW

CU_PRO(res,h)                    Prosumage: curtailment of renewable generation
G_MARKET_PRO2M(res,h)            Prosumage. energy sent to market
G_MARKET_M2PRO(h)                Prosumage: withdrawal of energy from market
G_RES_PRO(res,h)                 Prosumage: hourly renewables generation
STO_IN_PRO2PRO(res,sto,h)        Prosumage: storage loading from generation for discharging to consumption
STO_IN_PRO2M(res,sto,h)          Prosumage: storage loading from generation for discharging to market
STO_IN_M2PRO(sto,h)              Prosumage: storage loading from market for discharging to consumption
STO_IN_M2M(sto,h)                Prosumage: storage loading from market for discharging to market
STO_OUT_PRO2PRO(sto,h)           Prosumage: storage discharging to consumption from generation
STO_OUT_PRO2M(sto,h)             Prosumage: storage discharging to market from generation
STO_OUT_M2PRO(sto,h)             Prosumage: storage discharging to consumption from market
STO_OUT_M2M(sto,h)               Prosumage: storage discharging to market from market
STO_L_PRO2PRO(sto,h)             Prosumage: storage level generation to consumption
STO_L_PRO2M(sto,h)               Prosumage: storage level generation to market
STO_L_M2PRO(sto,h)               Prosumage: storage level market to consumotion
STO_L_M2M(sto,h)                 Prosumage: storage level market to market
N_STO_E_PRO(sto)                 Prosumage: installed storage energy
N_STO_P_PRO(sto)                 Prosumage: installed storage power
STO_L_PRO(sto,h)                 Prosumage: overall storage level
N_RES_PRO(res)                   Prosumage: installed renewables capacities
;

********************************************************************************

Equations
* Objective
obj                      Objective cost minimization

* Energy balance
con1a_bal(h)             Supply Demand Balance

con1b_bal_pro(h)

* Load change costs
con2a_loadlevel          Load change costs: Level
con2b_loadlevelstart     Load change costs: Level for first period

* Capacity contraints and flexibility constraints
con3a_maxprod_conv       Capacity Constraint conventionals
con3b_minprod_conv       Minimum production conventionals if reserves contracted

con3c_flex_reserves_spin     Flexibility of conventionals for reserves provision
con3d_flex_reserves_nonspin  Flexibility of conventionals for reserves provision

con3e_maxprod_ror        Capacity constraint Run-of-river
con3f_maxprod_res        Capacity constraints renewables
con3g_minprod_res        Minimum production RES if reserves contracted

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
con8g_max_pro_res               Maximum installable capacity: prosumage renewables
con8h_max_pro_sto_e             Maximum installable capacity: prosumage storage energy
con8i_max_sto_pro_p             Maximum installable capacity: prosumage storage power

* Reserves
con9a_reserve_prov             Reserve provision SR and MR
con9b_reserve_prov_PR          Reserve provision PR

* Electric vehicles
con10a_ev_ed                Energy balance of electric vehicles
con10b_ev_chargelev_start   Cumulative charging level in the first hour
con10c_ev_chargelev         Cumulative charging level in hour h
con10d_ev_chargelev_max     Cumulative maximal charging level
con10e_ev_maxin             Cumulative maximal charging power
con10f_ev_maxout            Cumulative maximal discharging power
con10g_ev_chargelev_ending  Cumulative charging level in the last hour
con10h_ev_minin             Cumulative minimal charging power
con10i_ev_maxin_lev         Cumulative maximal charging limit
con10j_ev_minout            Cumulative minimal discharging power
con10k_ev_maxout_lev        Cumulative maximal discharging limit
con10l_ev_exog              Exogenous EV charging


con11a_pro_distrib                       Prosumage: distribution of generated energy
con11b_pro_balance                       Prosumage: energy balance
con11c_pro_selfcon                       Prosumage: minimum self-generation requirement
con11d_pro_stolev_PRO2PRO                Prosumage: storage level prosumager-to-prosumagers
con11e_pro_stolev_PRO2M                  Prosumage: storage level prosumagers-to-market
con11f_pro_stolev_M2PRO                  Prosumage: storage level market-to-prosumagers
con11g_pro_stolev_M2M                    Prosumage: storage level market-to-market
con11h_1_pro_stolev_start_PRO2PRO        Prosumage: storage level initial conditions
con11h_2_pro_stolev_start_PRO2M          Prosumage: storage level initial conditions
con11h_3_pro_stolev_start_M2PRO          Prosumage: storage level initial conditions
con11h_4_pro_stolev_start_M2M            Prosumage: storage level initial conditions
con11i_pro_stolev                        Prosumage: storage level total
con11j_pro_stolev_max                    Prosumage: maximum overall storage level
con11k_pro_maxin_sto                     Prosumage: maximum storage inflow
con11l_pro_maxout_sto                    Prosumage: maximum storage outflow
con11m_pro_maxout_lev                    Prosumage: maximum storage outflow linked to level
con11n_pro_maxin_lev                     Prosumage: maximum storage inflow linked to level
con11o_pro_ending                        Prosumage: storage ending condition
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
%EV%$ontext
                 + sum( (ev,h) , c_m_ev(ev) * EV_DISCHARGE(ev,h) )
                 + sum( (ev,h) , pen_phevfuel * EV_PHEVFUEL(ev,h) )
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
                 + sum( (reserves_up,sto,h) , phi_reserves_call(reserves_up,h) * c_m_sto(sto) * (RP_STO_OUT(reserves_up,sto,h) - RP_STO_IN(reserves_up,sto,h)) )
                 - sum( (reserves_do,sto,h) , phi_reserves_call(reserves_do,h) * c_m_sto(sto) * (RP_STO_OUT(reserves_do,sto,h) - RP_STO_IN(reserves_do,sto,h)) )
$ontext
$offtext
%reserves%$ontext
%EV%$ontext
%EV_EXOG%        + sum( (reserves_up,ev,h) , RP_EV_V2G(reserves_up,ev,h)* phi_reserves_call(reserves_up,h) * c_m_ev(ev) )
%EV_EXOG%        - sum( (reserves_do,ev,h) , RP_EV_V2G(reserves_do,ev,h)* phi_reserves_call(reserves_do,h) * c_m_ev(ev) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
                 + sum( (reserves_up,dsm_curt,h) , RP_DSM_CU(reserves_up,dsm_curt,h) * phi_reserves_call(reserves_up,h) * c_m_dsm_cu(dsm_curt) )
                 + sum( (reserves,dsm_shift,h) , RP_DSM_SHIFT(reserves,dsm_shift,h) * phi_reserves_call(reserves,h) * c_m_dsm_shift(dsm_shift) )
$ontext
$offtext
%prosumage%$ontext
                 + sum( res , c_i_res(res)*N_RES_PRO(res) )
                 + sum( res , c_fix_res(res)*N_RES_PRO(res) )

                 + sum( sto , c_i_sto_e(sto)*N_STO_E_PRO(sto) )
                 + sum( sto , c_fix_sto(sto)/2*(N_STO_P_PRO(sto)+N_STO_E_PRO(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P_PRO(sto) )

                 + sum( (sto,h) , c_m_sto(sto) * ( STO_OUT_PRO2PRO(sto,h) + STO_OUT_M2PRO(sto,h) + STO_OUT_PRO2M(sto,h) + STO_OUT_M2M(sto,h) + sum( res , STO_IN_PRO2PRO(res,sto,h) + STO_IN_PRO2M(res,sto,h)) + STO_OUT_PRO2M(sto,h) + STO_OUT_M2M(sto,h) ) )
$ontext
$offtext
;

* ---------------------------------------------------------------------------- *
***** Energy balance and load levels *****
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(hh)..
         ( 1 - phi_pro_load ) * d(hh) + sum( sto , STO_IN(sto,hh) )
%DSM%$ontext
         + sum( dsm_shift , DSM_UP_DEMAND(dsm_shift,hh) )
$ontext
$offtext
%EV%$ontext
         + sum( ev , EV_CHARGE(ev,hh) )
$ontext
$offtext
%prosumage%$ontext
         + G_MARKET_M2PRO(hh)
         + sum( sto , STO_IN_M2PRO(sto,hh))
         + sum( sto , STO_IN_M2M(sto,hh))
$ontext
$offtext
         =E=
         sum( ct , G_L(ct,hh)) + sum( res , G_RES(res,hh)) + sum( sto , STO_OUT(sto,hh) )
%reserves%$ontext
* Balancing Correction Factor
        + sum( ct ,
          sum( reserves_do ,  RP_CON(reserves_do,ct,hh) * phi_reserves_call(reserves_do,hh))
        - sum( reserves_up ,  RP_CON(reserves_up,ct,hh) * phi_reserves_call(reserves_up,hh))
         )
$ontext
$offtext
%DSM%$ontext
         + sum(dsm_curt, DSM_CU(dsm_curt,hh))
         + sum(dsm_shift, DSM_DO_DEMAND(dsm_shift,hh))
$ontext
$offtext
%EV%$ontext
        + sum( ev , EV_DISCHARGE(ev,hh) )
$ontext
$offtext
%prosumage%$ontext
         + sum( res , G_MARKET_PRO2M(res,hh) )
         + sum( sto , STO_OUT_PRO2M(sto,hh))
         + sum( sto , STO_OUT_M2M(sto,hh))
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
        + sum( reserves_up , RP_CON(reserves_up,ct,h))
* Balancing Correction Factor
        + sum( reserves_do ,  RP_CON(reserves_do,ct,h) * phi_reserves_call(reserves_do,h))
        - sum( reserves_up ,  RP_CON(reserves_up,ct,h) * phi_reserves_call(reserves_up,h))
$ontext
$offtext
        =L= N_CON(ct)
;

con3b_minprod_conv(ct,h)..
        sum( reserves_do , RP_CON(reserves_do,ct,h))
        =L= G_L(ct,h)
* Balancing Correction Factor
        + sum( reserves_do ,  RP_CON(reserves_do,ct,h) * phi_reserves_call(reserves_do,h))
        - sum( reserves_up ,  RP_CON(reserves_up,ct,h) * phi_reserves_call(reserves_up,h))
;

con3c_flex_reserves_spin(reserves_spin,ct,h)..
        RP_CON(reserves_spin,ct,h)
        =L= grad_per_min(ct) * reserves_reaction(reserves_spin) * ( G_L(ct,h)
* Balancing Correction Factor
        + sum( reserves_do ,  RP_CON(reserves_do,ct,h) * phi_reserves_call(reserves_do,h))
        - sum( reserves_up ,  RP_CON(reserves_up,ct,h) * phi_reserves_call(reserves_up,h)) )
;

con3d_flex_reserves_nonspin(reserves_nonspin,ct,h)..
        RP_CON(reserves_nonspin,ct,h)
        =L= grad_per_min(ct) * reserves_reaction(reserves_nonspin) * N_CON(ct)
;


* Constraints on run of river
con3e_maxprod_ror(h)..
        G_L('ror',h)
%reserves%$ontext
        + sum( reserves_up , RP_CON(reserves_up,'ror',h))
* Balancing Correction Factor
        + sum( reserves_do ,  RP_CON(reserves_do,'ror',h) * phi_reserves_call(reserves_do,h))
        - sum( reserves_up ,  RP_CON(reserves_up,'ror',h) * phi_reserves_call(reserves_up,h))
$ontext
$offtext
        =L= phi_ror(h)*N_CON('ror')
;


* Constraints on renewables
con3f_maxprod_res(res,h)..
        G_RES(res,h) + CU(res,h)
%reserves%$ontext
        + sum( reserves_up , RP_RES(reserves_up,res,h))
$ontext
$offtext
        =E= phi_res(res,h)*N_RES(res)
;

con3g_minprod_res(res,h)..
        sum( reserves_do , RP_RES(reserves_do,res,h))
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
         + sum( reserves_do , phi_reserves_call(reserves_do,h) * ( RP_STO_IN(reserves_do,sto,h)*(1+eta_sto(sto))/2 + RP_STO_OUT(reserves_do,sto,h)/(1+eta_sto(sto))*2 ))
         - sum( reserves_up , phi_reserves_call(reserves_up,h) * ( RP_STO_IN(reserves_up,sto,h)*(1+eta_sto(sto))/2 + RP_STO_OUT(reserves_up,sto,h)/(1+eta_sto(sto))*2 ))
$ontext
$offtext
;

con4c_stolev_max(sto,h)..
        STO_L(sto,h) =L= N_STO_E(sto)
;

con4d_maxin_sto(sto,h)..
        STO_IN(sto,h)
%reserves%$ontext
        + sum( reserves_do , RP_STO_IN(reserves_do,sto,h))
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4e_maxout_sto(sto,h)..
        STO_OUT(sto,h)
%reserves%$ontext
        + sum( reserves_up , RP_STO_OUT(reserves_up,sto,h))
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4f_resrv_sto(sto,h)..
        sum( reserves_up , RP_STO_IN(reserves_up,sto,h))
        =L= STO_IN(sto,h)
;

con4g_resrv_sto(sto,h)..
        sum( reserves_do , RP_STO_OUT(reserves_do,sto,h))
        =L= STO_OUT(sto,h)
;

con4h_maxout_lev(sto,h)..
        ( STO_OUT(sto,h)
%reserves%$ontext
        + sum( reserves_up , RP_STO_OUT(reserves_up,sto,h))
$ontext
$offtext
        ) /(1+eta_sto(sto))*2
        =L= STO_L(sto,h-1)
;

con4i_maxin_lev(sto,h)..
        ( STO_IN(sto,h)
%reserves%$ontext
        + sum( reserves_do , RP_STO_IN(reserves_do,sto,h))
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
sum( ct_the , sum( h , G_L(ct_the,h) ) )
        =L= (1-phi_min_res) * sum( h , d(h) + sum( (sto) , STO_IN(sto,h) - STO_OUT(sto,h) )
%prosumage%$ontext
         + sum( sto , sum( res , STO_IN_PRO2PRO(res,sto,h) + STO_IN_PRO2M(res,sto,h)) + STO_IN_M2PRO(sto,h) + STO_IN_M2M(sto,h) - STO_OUT_PRO2PRO(sto,h) - STO_OUT_PRO2M(sto,h) - STO_OUT_M2PRO(sto,h) - STO_OUT_M2M(sto,h) )
$ontext
$offtext
%DSM%$ontext
         - sum( dsm_curt , DSM_CU(dsm_curt,h) )
         + sum( dsm_shift , DSM_UP(dsm_shift,h) - sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(dsm_shift) ) , DSM_DO(dsm_shift,h,hh)) )
$ontext
$offtext
%EV%$ontext
        + sum( ev , EV_CHARGE(ev,h) - EV_DISCHARGE(ev,h) )
%EV_DEFAULT%%EV_FREE%    - sum( ev , EV_GED(ev,h) )
%EV_DEFAULT%%EV_100RES%  + phi_min_res/(1-phi_min_res)*sum( ev , EV_GED(ev,h) )
$ontext
$offtext
%reserves%$ontext
        + phi_mean_reserves_call('PR_up') * phi_reserves_pr * sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES(res)/1000 ) ) )
        + phi_mean_reserves_call('SR_up') *( 1000 * phi_reserves_share('SR_up') * (reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * N_RES(res)/1000 ) ) )
        + phi_mean_reserves_call('MR_up') *( 1000 * phi_reserves_share('MR_up') * (reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * N_RES(res)/1000 ) ) )
        - phi_mean_reserves_call('PR_do') * phi_reserves_pr * sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * N_RES(res)/1000 ) ) )
        - phi_mean_reserves_call('SR_do') *( 1000 * phi_reserves_share('SR_do') * (reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * N_RES(res)/1000 ) ) )
        - phi_mean_reserves_call('MR_do') *( 1000 * phi_reserves_share('MR_do') * (reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * N_RES(res)/1000 ) ) )

       + sum( sto ,
           sum( reserves_do , phi_reserves_call(reserves_do,h) * (RP_STO_IN(reserves_do,sto,h) + RP_STO_OUT(reserves_do,sto,h)))
         - sum( reserves_up , phi_reserves_call(reserves_up,h) * (RP_STO_IN(reserves_up,sto,h) + RP_STO_OUT(reserves_up,sto,h))) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
       - sum( (dsm_curt,reserves_up) , RP_DSM_CU(reserves_up,dsm_curt,h) * phi_reserves_call(reserves_up,h) )
$ontext
$offtext
%reserves%$ontext
%EV%$ontext
%EV_EXOG%  + sum( ev ,
%EV_EXOG%      sum( reserves_do , phi_reserves_call(reserves_do,h) * (RP_EV_G2V(reserves_do,ev,h) + RP_EV_V2G(reserves_do,ev,h)))
%EV_EXOG%    - sum( reserves_up , phi_reserves_call(reserves_up,h) * (RP_EV_G2V(reserves_up,ev,h) + RP_EV_V2G(reserves_up,ev,h))) )
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
        + sum( reserves_up , RP_DSM_CU(reserves_up,dsm_curt,hh) * phi_reserves_call(reserves_up,hh) )
$ontext
$offtext
         )
         =L= N_DSM_CU(dsm_curt) * t_dur_dsm_cu(dsm_curt)
;

con6b_DSMcurt_max(dsm_curt,h)..
        DSM_CU(dsm_curt,h)
%reserves%$ontext
        + sum( reserves_up , RP_DSM_CU(reserves_up,dsm_curt,h) )
$ontext
$offtext
          =L= N_DSM_CU(dsm_curt)
;

* ---------------------------------------------------------------------------- *
***** DSM constraints - shifting *****
* ---------------------------------------------------------------------------- *

con7a_DSMshift_upanddown(dsm_shift,h)..
         DSM_UP(dsm_shift,h) * (1 + eta_dsm_shift(dsm_shift))/2 =E= 2/(1+eta_dsm_shift(dsm_shift)) * sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(dsm_shift) ) , DSM_DO(dsm_shift,h,hh))
;

con7b_DSMshift_granular_max(dsm_shift,h)..
         DSM_UP_DEMAND(dsm_shift,h) + DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + sum( reserves , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
         =L= N_DSM_SHIFT(dsm_shift)
;

con7c_DSM_distrib_up(dsm_shift,h)..
         DSM_UP(dsm_shift,h) =E= DSM_UP_DEMAND(dsm_shift,h)
%reserves%$ontext
         + sum( reserves_do , RP_DSM_SHIFT(reserves_do,dsm_shift,h) * phi_reserves_call(reserves_do,h))
$ontext
$offtext
;

con7d_DSM_distrib_do(dsm_shift,h)..
         sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(dsm_shift) ) , DSM_DO(dsm_shift,hh,h) )
                 =E=
         DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + sum( reserves_up , RP_DSM_SHIFT(reserves_up,dsm_shift,h) * phi_reserves_call(reserves_up,h))
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

con8g_max_pro_res(res)..
         N_RES_PRO(res) =L= m_res_pro(res)
;

con8h_max_pro_sto_e(sto)..
         N_STO_E_PRO(sto) =L= m_sto_pro_e(sto)
;

con8i_max_sto_pro_p(sto)..
         N_STO_P_PRO(sto) =L= m_sto_pro_p(sto)
;

* ---------------------------------------------------------------------------- *
***** Reserve constraints *****
* ---------------------------------------------------------------------------- *

con9a_reserve_prov(reserves,h)$( ord(reserves) > 2)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h))
%DSM%$ontext
        + sum(dsm_curt, RP_DSM_CU(reserves,dsm_curt,h))
        + sum(dsm_shift , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
%EV%$ontext
%EV_EXOG%   + sum(ev, RP_EV_G2V(reserves,ev,h) + RP_EV_V2G(reserves,ev,h) )
$ontext
$offtext
        =E= (
            1000 * phi_reserves_share(reserves) * (
            reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * (N_RES(res)
%prosumage%$ontext
            + N_RES_PRO(res)
$ontext
$offtext
            )/1000 ) ) )$(ord(h) > 1)
;

con9b_reserve_prov_PR(reserves,h)$( ord(reserves) < 3)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h) )
%EV%$ontext
%EV_EXOG%   + sum(ev, RP_EV_G2V(reserves,ev,h) + RP_EV_V2G(reserves,ev,h) )
$ontext
$offtext
         =E= phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2), 1000 * phi_reserves_share(reservesreserves) * (
             reserves_intercept(reservesreserves) + sum( res , reserves_slope(reservesreserves,res) * (N_RES(res)
%prosumage%$ontext
             + N_RES_PRO(res)
$ontext
$offtext
             )/1000 ) ) )$(ord(h) > 1)

;

* ---------------------------------------------------------------------------- *
***** Electric vehicle constraints *****
* ---------------------------------------------------------------------------- *

con10a_ev_ed(ev,h)..
         ev_ed(ev,h) * phi_ev(ev) * ev_quant
         =e= EV_GED(ev,h) + EV_PHEVFUEL(ev,h)$(ev_phev(ev)=1)
;

con10b_ev_chargelev_start(ev,'h1')..
         EV_L(ev,'h1') =E= phi_ev_ini(ev) * n_ev_e(ev) * phi_ev(ev) * ev_quant
         + EV_CHARGE(ev,'h1') * eta_ev_in(ev)
         - EV_DISCHARGE(ev,'h1') / eta_ev_out(ev)
         - EV_GED(ev,'h1')
;

con10c_ev_chargelev(ev,h)$( (ord(h)>1) )..
         EV_L(ev,h) =E= EV_L(ev,h-1)
         + EV_CHARGE(ev,h) * eta_ev_in(ev)
         - EV_DISCHARGE(ev,h) / eta_ev_out(ev)
%reserves%$ontext
%EV_EXOG%   + sum( reserves_do , phi_reserves_call(reserves_do,h) * (RP_EV_G2V(reserves_do,ev,h)*eta_ev_in(ev) + RP_EV_V2G(reserves_do,ev,h)/eta_ev_out(ev)) )
%EV_EXOG%   - sum( reserves_up , phi_reserves_call(reserves_up,h) * ( RP_EV_G2V(reserves_up,ev,h)*eta_ev_in(ev) + RP_EV_V2G(reserves_up,ev,h)/eta_ev_out(ev)) )
$ontext
$offtext
         - EV_GED(ev,h)
;

con10d_ev_chargelev_max(ev,h)..
        EV_L(ev,h) =L= n_ev_e(ev) * phi_ev(ev) * ev_quant
;

con10e_ev_maxin(ev,h)..
        EV_CHARGE(ev,h)
%reserves%$ontext
        + sum( reserves_do , RP_EV_G2V(reserves_do,ev,h))
$ontext
$offtext
        =L= n_ev_p(ev,h) * phi_ev(ev) * ev_quant
;

con10f_ev_maxout(ev,h)..
        EV_DISCHARGE(ev,h)
%reserves%$ontext
        + sum( reserves_up , RP_EV_V2G(reserves_up,ev,h))
$ontext
$offtext
        =L= n_ev_p(ev,h) * phi_ev(ev) * ev_quant
;

con10g_ev_chargelev_ending(ev,h)$( ord(h) = card(h) )..
         EV_L(ev,h) =E= phi_ev_ini(ev) * n_ev_e(ev) * phi_ev(ev) * ev_quant
;

con10h_ev_minin(ev,h)..
         0 =L= EV_CHARGE(ev,h)
        - sum( reserves_up , RP_EV_G2V(reserves_up,ev,h))
;

con10i_ev_maxin_lev(ev,h)..
        ( EV_CHARGE(ev,h)
        + sum( reserves_do , RP_EV_G2V(reserves_do,ev,h))) * eta_ev_in(ev)
        =L= n_ev_e(ev) * phi_ev(ev) * ev_quant - EV_L(ev,h-1)
;

con10j_ev_minout(ev,h)..
         0 =L= EV_DISCHARGE(ev,h)
        - sum( reserves_do , RP_EV_V2G(reserves_do,ev,h))
;

con10k_ev_maxout_lev(ev,h)..
        ( EV_DISCHARGE(ev,h)
        + sum( reserves_up , RP_EV_V2G(reserves_up,ev,h))) / eta_ev_out(ev)
        =L= EV_L(ev,h-1)
;

con10l_ev_exog(ev,h)..
         EV_CHARGE(ev,h) =E= ev_ged_exog(ev,h) * phi_ev(ev) * ev_quant
;

* ---------------------------------------------------------------------------- *
***** Prosumage constraints *****
* ---------------------------------------------------------------------------- *

con11a_pro_distrib(res,h)..
         phi_res(res,h) * N_RES_PRO(res)
         =E=
         CU_PRO(res,h) + G_MARKET_PRO2M(res,h) + G_RES_PRO(res,h) + sum( sto , STO_IN_PRO2PRO(res,sto,h) + STO_IN_PRO2M(res,sto,h) )
;

con11b_pro_balance(h)..
         phi_pro_load * d(h)
         =E=
         sum( res , G_RES_PRO(res,h)) + sum( sto , STO_OUT_PRO2PRO(sto,h) + STO_OUT_M2PRO(sto,h) ) + G_MARKET_M2PRO(h)
;

con11c_pro_selfcon..
         sum( (res,h) , G_RES_PRO(res,h) ) + sum( (h,sto) , STO_OUT_PRO2PRO(sto,h) )
         =g=
         phi_pro_self * sum( h , phi_pro_load * d(h))
;

con11d_pro_stolev_PRO2PRO(sto,h)$( (ord(h)>1) )..
         STO_L_PRO2PRO(sto,h) =E= STO_L_PRO2PRO(sto,h-1) + sum( res , STO_IN_PRO2PRO(res,sto,h))*(1+eta_sto(sto))/2 - STO_OUT_PRO2PRO(sto,h)/(1+eta_sto(sto))*2
;

con11e_pro_stolev_PRO2M(sto,h)$( (ord(h)>1) )..
         STO_L_PRO2M(sto,h) =E= STO_L_PRO2M(sto,h-1) + sum( res , STO_IN_PRO2M(res,sto,h))*(1+eta_sto(sto))/2 - STO_OUT_PRO2M(sto,h)/(1+eta_sto(sto))*2
;

con11f_pro_stolev_M2PRO(sto,h)$( (ord(h)>1) )..
         STO_L_M2PRO(sto,h) =E= STO_L_M2PRO(sto,h-1) + STO_IN_M2PRO(sto,h)*(1+eta_sto(sto))/2 - STO_OUT_M2PRO(sto,h)/(1+eta_sto(sto))*2
;

con11g_pro_stolev_M2M(sto,h)$( (ord(h)>1) )..
         STO_L_M2M(sto,h) =E= STO_L_M2M(sto,h-1) + STO_IN_M2M(sto,h)*(1+eta_sto(sto))/2 - STO_OUT_M2M(sto,h)/(1+eta_sto(sto))*2
;

con11h_1_pro_stolev_start_PRO2PRO(sto,'h1')..
        STO_L_PRO2PRO(sto,'h1') =E= 0.25 * phi_sto_pro_ini(sto) * N_STO_E(sto) + sum( res , STO_IN_PRO2PRO(res,sto,'h1'))*(1+eta_sto(sto))/2 - STO_OUT_PRO2PRO(sto,'h1')/(1+eta_sto(sto))*2
;

con11h_2_pro_stolev_start_PRO2M(sto,'h1')..
        STO_L_PRO2M(sto,'h1') =E= 0.25 * phi_sto_pro_ini(sto) * N_STO_E(sto) + sum( res , STO_IN_PRO2M(res,sto,'h1'))*(1+eta_sto(sto))/2 - STO_OUT_PRO2M(sto,'h1')/(1+eta_sto(sto))*2
;

con11h_3_pro_stolev_start_M2PRO(sto,'h1')..
        STO_L_M2PRO(sto,'h1') =E= 0.25 * phi_sto_pro_ini(sto) * N_STO_E(sto) + STO_IN_M2PRO(sto,'h1')*(1+eta_sto(sto))/2 - STO_OUT_M2PRO(sto,'h1')/(1+eta_sto(sto))*2
;

con11h_4_pro_stolev_start_M2M(sto,'h1')..
        STO_L_M2M(sto,'h1') =E= 0.25 * phi_sto_pro_ini(sto) * N_STO_E(sto) + STO_IN_M2M(sto,'h1')*(1+eta_sto(sto))/2 - STO_OUT_M2M(sto,'h1')/(1+eta_sto(sto))*2
;

con11i_pro_stolev(sto,h)$( (ord(h)>1) )..
         STO_L_PRO(sto,h) =E=   STO_L_PRO2PRO(sto,h) +  STO_L_PRO2M(sto,h) + STO_L_M2PRO(sto,h) + STO_L_M2M(sto,h)
;

con11j_pro_stolev_max(sto,h)..
        STO_L_PRO(sto,h) =L= N_STO_E_PRO(sto)
;

con11k_pro_maxin_sto(sto,h)..
        sum( res , STO_IN_PRO2PRO(res,sto,h) + STO_IN_PRO2M(res,sto,h) ) + STO_IN_M2PRO(sto,h) + STO_IN_M2M(sto,h)
        =L= N_STO_P_PRO(sto)
;

con11l_pro_maxout_sto(sto,h)..
        STO_OUT_PRO2PRO(sto,h) + STO_OUT_PRO2M(sto,h) + STO_OUT_M2PRO(sto,h) + STO_OUT_M2M(sto,h)
        =L= N_STO_P_PRO(sto)
;

con11m_pro_maxout_lev(sto,h)..
        ( STO_OUT_PRO2PRO(sto,h) + STO_OUT_M2PRO(sto,h) + STO_OUT_PRO2M(sto,h) + STO_OUT_M2M(sto,h) ) / (1+eta_sto(sto))*2
        =L= STO_L_PRO(sto,h-1)
;

con11n_pro_maxin_lev(sto,h)..
        ( sum( res , STO_IN_PRO2PRO(res,sto,h) + STO_IN_PRO2M(res,sto,h) ) + STO_IN_M2PRO(sto,h) + STO_IN_M2M(sto,h) ) * (1+eta_sto(sto))/2
        =L= N_STO_E_PRO(sto) - STO_L_PRO(sto,h-1)
;

con11o_pro_ending(sto,h)$( ord(h) = card(h) )..
         STO_L_PRO(sto,h) =E= phi_sto_pro_ini(sto) * N_STO_E_PRO(sto)
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
%reserves%$ontext
  con3b_minprod_conv
  con3c_flex_reserves_spin
  con3d_flex_reserves_nonspin
$ontext
$offtext
%ror_parameter%%ror_variable%$ontext
  con3e_maxprod_ror
$ontext
$offtext
con3f_maxprod_res
%reserves%$ontext
  con3g_minprod_res
$ontext
$offtext

con4a_stolev_start
con4b_stolev
con4c_stolev_max
con4d_maxin_sto
con4e_maxout_sto
%reserves%$ontext
  con4f_resrv_sto
  con4g_resrv_sto
$ontext
$offtext
con4h_maxout_lev
con4i_maxin_lev
con4j_ending
con4k_PHS_EtoP

con5a_minRES
con5b_maxBIO

%DSM%$ontext
con6a_DSMcurt_duration_max
con6b_DSMcurt_max

con7a_DSMshift_upanddown
con7b_DSMshift_granular_max
con7c_DSM_distrib_up
con7d_DSM_distrib_do
* con_7e_DSMshift_recovery
$ontext
$offtext

con8a_max_I_con
con8b_max_I_res
con8c_max_I_sto_e
con8d_max_I_sto_p
%DSM%$ontext
  con8e_max_I_dsm_cu
  con8f_max_I_dsm_shift_pos
$ontext
$offtext

%reserves%$ontext
 con9a_reserve_prov
 con9b_reserve_prov_PR
$ontext
$offtext

%EV%$ontext
 con10a_ev_ed
%EV_EXOG% con10b_ev_chargelev_start
 con10c_ev_chargelev
 con10d_ev_chargelev_max
%EV_EXOG% con10e_ev_maxin
%EV_EXOG% con10f_ev_maxout
%EV_EXOG% con10g_ev_chargelev_ending
$ontext
$offtext
%EV%$ontext
%reserves%$ontext
%EV_EXOG% con10h_ev_minin
%EV_EXOG% con10i_ev_maxin_lev
%EV_EXOG% con10j_ev_minout
%EV_EXOG% con10k_ev_maxout_lev
$ontext
$offtext
%EV%$ontext
%EV_EXOG%$ontext
 con10l_ev_exog
$ontext
$offtext

%prosumage%$ontext
con8g_max_pro_res
con8h_max_pro_sto_e
con8i_max_sto_pro_p
con11a_pro_distrib
con11b_pro_balance
con11c_pro_selfcon
con11d_pro_stolev_PRO2PRO
con11e_pro_stolev_PRO2M
con11f_pro_stolev_M2PRO
con11g_pro_stolev_M2M
con11h_1_pro_stolev_start_PRO2PRO
con11h_2_pro_stolev_start_PRO2M
con11h_3_pro_stolev_start_M2PRO
con11h_4_pro_stolev_start_M2M
con11i_pro_stolev
con11j_pro_stolev_max
con11k_pro_maxin_sto
con11l_pro_maxout_sto
con11m_pro_maxout_lev
con11n_pro_maxin_lev
con11o_pro_ending
$ontext
$offtext
/;

********************************************************************************
***** Options, fixings, report preparation *****
********************************************************************************

* Solver options
$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
parallelmode -1
$offecho

%no_crossover%$ontext
$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
parallelmode -1
barcrossalg -1
barepcomp 1e-8
$offecho
$ontext
$offtext

dieter.OptFile = 1;
dieter.holdFixed = 1 ;

* Parameters for default base year
d(h) = d_y(%base_year%,h) ;
phi_res(res,h) = phi_res_y(%base_year%,res,h) ;
phi_reserves_call(reserves,h) = phi_reserves_call_y(%base_year%,reserves,h) ;
phi_mean_reserves_call(reserves) = phi_mean_reserves_call_y(%base_year%,reserves) ;


********************************************************************************
***** Solve *****
********************************************************************************

* Preparation of GUSS tool for scenario analysis
phi_min_res = eps ;
ev_quant = eps ;
phi_pro_self = eps ;

$eval superscencount 1000

Set
modelstats       model stats collection                  /modelstat, solvestat, resusd/
superscen        Scenarios                               /scen1*scen%superscencount%/
map(superscen,loop_res_share,loop_ev,loop_prosumage)    /#superscen:(#loop_res_share.#loop_ev.#loop_prosumage)/
;

set
scen(superscen);
scen(superscen) = yes$( sum((loop_res_share,loop_ev,loop_prosumage) , map(superscen,loop_res_share,loop_ev,loop_prosumage)) )    ;

Parameters
gussoptions                              /Logoption 2, Optfile 1, Skipbasecase 1/
modstats(superscen, modelstats)
min_res
number_ev
pro_selfcon
;

min_res(scen) = sum( (loop_res_share,loop_ev,loop_prosumage)$map(scen,loop_res_share,loop_ev,loop_prosumage) , loop_res_share.val/100 ) ;
number_ev(scen) = sum( (loop_res_share,loop_ev,loop_prosumage)$map(scen,loop_res_share,loop_ev,loop_prosumage) , loop_ev.val ) ;
pro_selfcon(scen) = sum( (loop_res_share,loop_ev,loop_prosumage)$map(scen,loop_res_share,loop_ev,loop_prosumage) , loop_prosumage.val/100 ) ;

Parameters
marginal_con5a(superscen)
marginal_con1a(superscen,h)

lev_Z(superscen)
lev_G_L(superscen,ct,h)
lev_G_UP(superscen,ct,h)
lev_G_DO(superscen,ct,h)
lev_G_RES(superscen,res,h)
lev_CU(superscen,res,h)
lev_STO_IN(superscen,sto,h)
lev_STO_OUT(superscen,sto,h)
lev_STO_L(superscen,sto,h)
lev_N_CON(superscen,ct)
lev_N_RES(superscen,res)
lev_N_STO_E(superscen,sto)
lev_N_STO_P(superscen,sto)

%EV%$ontext
lev_EV_CHARGE(superscen,ev,h)
lev_EV_DISCHARGE(superscen,ev,h)
lev_EV_L(superscen,ev,h)
lev_EV_PHEVFUEL(superscen,ev,h)
lev_EV_GED(superscen,ev,h)
$ontext
$offtext

%DSM%$ontext
lev_DSM_CU(superscen,dsm_curt,h)
lev_DSM_UP(superscen,dsm_shift,h)
lev_DSM_DO(superscen,dsm_shift,h,hh)
lev_DSM_UP_DEMAND(superscen,dsm_shift,h)
lev_DSM_DO_DEMAND(superscen,dsm_shift,h)
lev_N_DSM_CU(superscen,dsm_curt)
lev_N_DSM_SHIFT(superscen,dsm_shift)
$ontext
$offtext

%reserves%$ontext
lev_RP_CON(superscen,reserves,ct,h)
lev_RP_RES(superscen,reserves,res,h)
lev_RP_STO_IN(superscen,reserves,sto,h)
lev_RP_STO_OUT(superscen,reserves,sto,h)
$ontext
$offtext

%EV%$ontext
%reserves%$ontext
lev_RP_EV_V2G(superscen,reserves,ev,h)
lev_RP_EV_G2V(superscen,reserves,ev,h)
$ontext
$offtext

%DSM%$ontext
%reserves%$ontext
lev_RP_DSM_CU(superscen,reserves,dsm_curt,h)
lev_RP_DSM_SHIFT(superscen,reserves,dsm_shift,h)
$ontext
$offtext

%prosumage%$ontext
lev_CU_PRO(superscen,res,h)
lev_G_MARKET_PRO2M(superscen,res,h)
lev_G_MARKET_M2PRO(superscen,h)
lev_G_RES_PRO(superscen,res,h)
lev_STO_IN_PRO2PRO(superscen,res,sto,h)
lev_STO_IN_PRO2M(superscen,res,sto,h)
lev_STO_IN_M2PRO(superscen,sto,h)
lev_STO_IN_M2M(superscen,sto,h)
lev_STO_OUT_PRO2PRO(superscen,sto,h)
lev_STO_OUT_PRO2M(superscen,sto,h)
lev_STO_OUT_M2PRO(superscen,sto,h)
lev_STO_OUT_M2M(superscen,sto,h)
lev_STO_L_PRO2PRO(superscen,sto,h)
lev_STO_L_PRO2M(superscen,sto,h)
lev_STO_L_M2PRO(superscen,sto,h)
lev_STO_L_M2M(superscen,sto,h)
lev_N_STO_E_PRO(superscen,sto)
lev_N_STO_P_PRO(superscen,sto)
lev_STO_L_PRO(superscen,sto,h)
lev_N_RES_PRO(superscen,res)
$ontext
$offtext
;


* Inclusion of scenario and fixing
$include fix.gms
$include scenario.gms


* Definition of dictionary set for GUSS tool
Set dict(*,*,*) /
scen             .scenario       .''
gussoptions      .opt            .modstats

phi_min_res      .param          .min_res
%EV%$ontext
ev_quant         .param          .number_ev
$ontext
$offtext
%prosumage%$ontext
phi_pro_self     .param          .pro_selfcon
$ontext
$offtext

con5a_minRES    .marginal       .marginal_con5a
con1a_bal       .marginal       .marginal_con1a

Z                .level          .lev_Z
G_L              .level          .lev_G_L
G_DO             .level          .lev_G_DO
G_RES            .level          .lev_G_RES
CU               .level          .lev_CU
STO_IN           .level          .lev_STO_IN
STO_OUT          .level          .lev_STO_OUT
STO_L            .level          .lev_STO_L
N_CON            .level          .lev_N_CON
N_RES            .level          .lev_N_RES
N_STO_E          .level          .lev_N_STO_E
N_STO_P          .level          .lev_N_STO_P

%EV%$ontext
EV_CHARGE        .level          .lev_EV_CHARGE
EV_DISCHARGE     .level          .lev_EV_DISCHARGE
EV_L             .level          .lev_EV_L
EV_PHEVFUEL      .level          .lev_EV_PHEVFUEL
EV_GED           .level          .lev_EV_GED
$ontext
$offtext

%DSM%$ontext
DSM_CU           .level          .lev_DSM_CU
DSM_UP           .level          .lev_DSM_UP
DSM_DO           .level          .lev_DSM_DO
DSM_UP_DEMAND    .level          .lev_DSM_UP_DEMAND
DSM_DO_DEMAND    .level          .lev_DSM_DO_DEMAND
N_DSM_CU         .level          .lev_N_DSM_CU
N_DSM_SHIFT      .level          .lev_N_DSM_SHIFT
$ontext
$offtext

%reserves%$ontext
RP_CON           .level          .lev_RP_CON
RP_RES           .level          .lev_RP_RES
RP_STO_IN        .level          .lev_RP_STO_IN
RP_STO_OUT       .level          .lev_RP_STO_OUT
$ontext
$offtext

%reserves%$ontext
%EV%$ontext
%EV_EXOG% RP_EV_V2G        .level          .lev_RP_EV_V2G
%EV_EXOG% RP_EV_G2V        .level          .lev_RP_EV_G2V
$ontext
$offtext

%reserves%$ontext
%DSM%$ontext
RP_DSM_CU        .level          .lev_RP_DSM_CU
RP_DSM_SHIFT     .level          .lev_RP_DSM_SHIFT
$ontext
$offtext

%prosumage%$ontext
CU_PRO           .level           .lev_CU_PRO
G_MARKET_PRO2M   .level           .lev_G_MARKET_PRO2M
G_MARKET_M2PRO   .level           .lev_G_MARKET_M2PRO
G_RES_PRO        .level           .lev_G_RES_PRO
STO_IN_PRO2PRO   .level           .lev_STO_IN_PRO2PRO
STO_IN_PRO2M     .level           .lev_STO_IN_PRO2M
STO_IN_M2PRO     .level           .lev_STO_IN_M2PRO
STO_IN_M2M       .level           .lev_STO_IN_M2M
STO_OUT_PRO2PRO  .level           .lev_STO_OUT_PRO2PRO
STO_OUT_PRO2M    .level           .lev_STO_OUT_PRO2M
STO_OUT_M2PRO    .level           .lev_STO_OUT_M2PRO
STO_OUT_M2M      .level           .lev_STO_OUT_M2M
STO_L_PRO        .level           .lev_STO_L_PRO
STO_L_PRO2PRO    .level           .lev_STO_L_PRO2PRO
STO_L_PRO2M      .level           .lev_STO_L_PRO2M
STO_L_M2PRO      .level           .lev_STO_L_M2PRO
STO_L_M2M        .level           .lev_STO_L_M2M
N_STO_E_PRO      .level           .lev_N_STO_E_PRO
N_STO_P_PRO      .level           .lev_N_STO_P_PRO
N_RES_PRO        .level           .lev_N_RES_PRO
$ontext
$offtext
/
;

solve DIETER using lp min Z scenario dict;

*$stop
* Reporting
$include report.gms

%reserves%%prosumage%execute_unload "results", report, report_tech, report_tech_hours, report_hours ;

%prosumage%$ontext
%reserves%execute_unload "results", report, report_tech, report_tech_hours, report_hours, report_prosumage, report_prosumage_tech, report_prosumage_tech_hours, report_market, report_market_tech, report_market_tech_hours ;
$ontext
$offtext

%reserves%$ontext
execute_unload "results", report, report_tech, report_tech_hours, report_hours, report_reserves, report_reserves_tech, report_reserves_tech_hours ;
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
