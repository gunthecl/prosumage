
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.3.0, October 2017.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************


Variables
Z                Value objective function [Euro]
F(l,h)           Energy flow over link l in hour h [MWh]
;

Positive Variables
G_L(n,tech,h)            Generation level in hour h [MWh]
G_UP(n,tech,h)           Generation upshift in hour h [MWh]
G_DO(n,tech,h)           Generation downshift in hour h [MWh]

G_RES(n,tech,h)          Generation renewables type res in hour h [MWh]
CU(n,tech,h)             Renewables curtailment technology res in hour h [MWh]

STO_IN(n,sto,h)          Storage inflow technology sto hour h [MWh]
STO_OUT(n,sto,h)         Storage outflow technology sto hour h [MWh]
STO_L(n,sto,h)           Storage level technology sto hour h [MWh]

EV_CHARGE(n,ev,h)        Electric vehicle charging vehicle profile ev hour h [MWh]
EV_DISCHARGE(n,ev,h)     Electric vehicle discharging vehicle profile ev hour h [MWh]
EV_L(n,ev,h)             Electric vehicle charging level vehicle profile ev hour h [MWh]
EV_PHEVFUEL(n,ev,h)      Plug in hybrid electric vehicle conventional fuel use vehicle profile ev hour h [MWh]
EV_GED(n,ev,h)           Grid electricity demand for mobility vehicle profile ev hour h [MWh]

N_TECH(n,tech)           Technology tech built [MW]
N_STO_E(n,sto)           Storage technology built - Energy [MWh]
N_STO_P(n,sto)           Storage loading and discharging capacity built - Capacity [MW]

DSM_CU(n,dsm,h)          DSM: Load curtailment hour h [MWh]
DSM_UP(n,dsm,h)          DSM: Load shifting up hour h technology dsm [MWh]
DSM_DO(n,dsm,h,hh)       DSM: Load shifting down in hour hh to account for upshifts in hour h technology dsm [MWh]

DSM_UP_DEMAND(n,dsm,h)   DSM: Load shifting up active for wholesale demand in hour h of technology dsm [MWh]
DSM_DO_DEMAND(n,dsm,h)   DSM: Load shifting down active for wholesale demand in hour h of technology dsm [MWh]

N_DSM_CU(n,dsm)          DSM: Load curtailment capacity [MW]
N_DSM_SHIFT(n,dsm)       DSM: Load shifting capacity [MWh]

RP_DIS(n,reserves,tech,h)        Reserve provision by conventionals in hour h [MW]
RP_NONDIS(n,reserves,tech,h)     Reserve provision by renewables in hour h [MW]
RP_STO_IN(n,reserves,sto,h)      Reserve provision by storage in in hour h [MW]
RP_STO_OUT(n,reserves,sto,h)     Reserve provision by storage out in hour h [MW]
RP_EV_V2G(n,reserves,ev,h)       Reserve provision by electric vehicles V2G hour h [MW]
RP_EV_G2V(n,reserves,ev,h)       Reserve provision by electric vehicles G2V hour h [MW]
RP_DSM_CU(n,reserves,dsm,h)      Reserve provision by DSM load curtailment in hour h [MW]
RP_DSM_SHIFT(n,reserves,dsm,h)   Reserve provision by DSM load shifting in hour h [MW]
RP_RSVR(n,reserves,rsvr,h)       Reserve provision by reservoirs h [MW]
RP_SETS(n,reserves,bu,ch,h)      Reserve provision by SETS [MW]
RP_SETS_AUX(n,reserves,bu,ch,h)  Reserve provision by SETS auxiliary DHW modules [MW]
RP_HP(n,reserves,bu,ch,h)        Reserve provision by heat pumps [MW]
RP_H_ELEC(n,reserves,bu,ch,h)    Reserve provision by hybrid electric heaters [MW]

CU_PRO(n,tech,h)                 Prosumage: curtailment of renewable generation in hour h [MWh]
G_MARKET_PRO2M(n,tech,h)         Prosumage. energy sent to market in hour h [MWh]
G_MARKET_M2PRO(n,h)              Prosumage: withdrawal of energy from market in hour h [MWh]
G_RES_PRO(n,tech,h)              Prosumage: hourly renewables generation in hour h [MWh]
STO_IN_PRO2PRO(n,tech,sto,h)     Prosumage: storage loading from generation for discharging to consumption in hour h [MWh]
STO_IN_PRO2M(n,tech,sto,h)       Prosumage: storage loading from generation for discharging to market in hour h [MWh]
STO_IN_M2PRO(n,sto,h)            Prosumage: storage loading from market for discharging to consumption in hour h [MWh]
STO_IN_M2M(n,sto,h)              Prosumage: storage loading from market for discharging to market in hour h [MWh]
STO_OUT_PRO2PRO(n,sto,h)         Prosumage: storage discharging to consumption from generation in hour h [MWh]
STO_OUT_PRO2M(n,sto,h)           Prosumage: storage discharging to market from generation in hour h [MWh]
STO_OUT_M2PRO(n,sto,h)           Prosumage: storage discharging to consumption from market in hour h [MWh]
STO_OUT_M2M(n,sto,h)             Prosumage: storage discharging to market from market in hour h [MWh]
STO_L_PRO2PRO(n,sto,h)           Prosumage: storage level generation to consumption in hour h [MWh]
STO_L_PRO2M(n,sto,h)             Prosumage: storage level generation to market in hour h [MWh]
STO_L_M2PRO(n,sto,h)             Prosumage: storage level market to consumotion in hour h [MWh]
STO_L_M2M(n,sto,h)               Prosumage: storage level market to market in hour h [MWh]
N_STO_E_PRO(n,sto)               Prosumage: installed storage energy [MWh]
N_STO_P_PRO(n,sto)               Prosumage: installed storage power [MW]
STO_L_PRO(n,sto,h)               Prosumage: overall storage level in hour h [MWh]
N_RES_PRO(n,tech)                Prosumage: installed renewables capacities [MW]

NTC(l)                           Trade: installed NTC on line l [MW]

RSVR_OUT(n,rsvr,h)               Reservoirs: outflow in hour h [MWh]
RSVR_L(n,rsvr,h)                 Reservoirs: level in hour h [MWh]
N_RSVR_E(n,rsvr)                 Reservoirs: installed energy capacity [MWh]
N_RSVR_P(n,rsvr)                 Reservoirs: installed power capacity [MW]

H_DIR(n,bu,ch,h)                 Heating: direct heating in hour h for building type bu with haeting technology ch [MWh]
H_SETS_LEV(n,bu,ch,h)            Heating: storage level SETS technologies [MWh]
H_SETS_IN(n,bu,ch,h)             Heating: storage inflow SETS technologies [MWh]
H_SETS_OUT(n,bu,ch,h)            Heating: storage outflow SETS technologies [MWh]
H_HP_IN(n,bu,ch,hh)              Heating: electricity demand heat pump technologies [MWh]
H_STO_LEV(n,bu,ch,h)             Heating: storage level storage technologies [MWh]
H_STO_IN_HP(n,bu,ch,h)           Heating: storage inflow from heat pumps to storage technologies [MWh]
H_STO_IN_ELECTRIC(n,bu,ch,h)     Heating: storage inflow from electric heating to storage technologies [MWh]
H_ELECTRIC_IN(n,bu,ch,h)         Heating: hybrid electric heaters electricity demand [MWh]
H_STO_IN_FOSSIL(n,bu,ch,h)       Heating: storage inflow from nonelectric heating to storage technologies [MWh]
H_STO_OUT(n,bu,ch,h)             Heating: storage outflow from storage technologies [MWh]

H_DHW_DIR(n,bu,ch,h)             Heating - domestic hot water: provision in case of direct electric heating [MWh]
H_DHW_STO_OUT(n,bu,ch,h)         Heating - domestic hot water: DHW storage outflow [MWh]

H_DHW_AUX_ELEC_IN(n,bu,ch,h)     Heating - domestic hot water: electrical energy input of auxiliary hot water tank for SETS [MWh]
H_DHW_AUX_LEV(n,bu,ch,h)         Heating - domestic hot water: level of auxiliary hot water tank for SETS [MWh]
H_DHW_AUX_OUT(n,bu,ch,h)         Heating - domestic hot water: auxiliary DHW provision for SETS [MWh]
;




********************************************************************************

Equations
* Objective
obj                      Objective cost minimization

* Energy balance
con1a_bal                Energy Balance

* Load change costs
con2a_loadlevel          Load change costs: Level
con2b_loadlevelstart     Load change costs: Level for first period

* Capacity contraints and flexibility constraints
con3a_maxprod_dispatchable       Capacity Constraint conventionals
con3e_maxprod_res                Capacity constraints renewables 

* Storage constraints
con4a_stolev_start        Storage Level Dynamics Initial Condition
con4b_stolev              Storage Level Dynamics
con4c_stolev_max          Storage Power Capacity
con4d_maxin_sto           Storage maximum inflow
con4e_maxout_sto          Storage maximum outflow
con4j_ending              End level equal to initial level
con4k_PHS_EtoP            Maximum E to P ratio for PHS

* Minimum restrictions for renewables and biomass
con5a_minRES             Minimum yearly renewables requirement
con5b_max_energy         Maximum yearly biomass energy


* Maximum installation conditions
con8a_max_I_power                Maximum installable capacity: Conventionals
con8b_max_I_sto_e                Maximum installable energy: Storage in MWh
con8c_max_I_sto_p                Maximum installable capacity: Storage inflow-outflow in MW
con8f_max_pro_res                Maximum installable capacity: prosumage renewables
con8g_max_pro_sto_e              Maximum installable capacity: prosumage storage energy
con8h_max_sto_pro_p              Maximum installable capacity: prosumage storage power

* Prosumage
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
                   sum( (h,map_n_tech(n,dis)) , c_m(n,dis)*G_L(n,dis,h) )
                 + sum( (h,map_n_tech(n,dis))$(ord(h)>1) , c_up(n,dis)*G_UP(n,dis,h) )
                 + sum( (h,map_n_tech(n,dis)) , c_do(n,dis)*G_DO(n,dis,h) )
                 + sum( (h,map_n_tech(n,nondis)) , c_cu(n,nondis)*CU(n,nondis,h) )
                 + sum( (h,map_n_sto(n,sto)) , c_m_sto(n,sto) * ( STO_OUT(n,sto,h) + STO_IN(n,sto,h) ) )
%DSM%$ontext
                 + sum( (h,map_n_dsm(n,dsm_curt)) , c_m_dsm_cu(n,dsm_curt)*DSM_CU(n,dsm_curt,h) )
                 + sum( (h,map_n_dsm(n,dsm_shift)) , c_m_dsm_shift(n,dsm_shift) * DSM_UP_DEMAND(n,dsm_shift,h) )
                 + sum( (h,map_n_dsm(n,dsm_shift)) , c_m_dsm_shift(n,dsm_shift) * DSM_DO_DEMAND(n,dsm_shift,h) )
$ontext
$offtext
%EV%$ontext
                 + sum( (h,map_n_ev(n,ev)) , c_m_ev(n,ev) * EV_DISCHARGE(n,ev,h) )
                 + sum( (h,map_n_ev(n,ev)) , pen_phevfuel(n,ev) * EV_PHEVFUEL(n,ev,h) )
$ontext
$offtext
                 + sum( map_n_tech(n,tech) , c_i(n,tech)*N_TECH(n,tech) )
                 + sum( map_n_tech(n,tech) , c_fix(n,tech)*N_TECH(n,tech) )
                 + sum( map_n_sto(n,sto) , c_i_sto_e(n,sto)*N_STO_E(n,sto) )
                 + sum( map_n_sto(n,sto) , c_fix_sto(n,sto)/2*(N_STO_P(n,sto)+N_STO_E(n,sto)) )
                 + sum( map_n_sto(n,sto) , c_i_sto_p(n,sto)*N_STO_P(n,sto) )
%DSM%$ontext
                 + sum( map_n_dsm(n,dsm_curt) , c_i_dsm_cu(n,dsm_curt)*N_DSM_CU(n,dsm_curt) )
                 + sum( map_n_dsm(n,dsm_curt) , c_fix_dsm_cu(n,dsm_curt)*N_DSM_CU(n,dsm_curt) )
                 + sum( map_n_dsm(n,dsm_shift) , c_i_dsm_shift(n,dsm_shift)*N_DSM_SHIFT(n,dsm_shift) )
                 + sum( map_n_dsm(n,dsm_shift) , c_fix_dsm_shift(n,dsm_shift)*N_DSM_SHIFT(n,dsm_shift) )
$ontext
$offtext
%reserves%$ontext
                 + sum( (h,map_n_sto(n,sto),reserves_up) , phi_reserves_call(n,reserves_up,h) * c_m_sto(n,sto) * (RP_STO_OUT(n,reserves_up,sto,h) - RP_STO_IN(n,reserves_up,sto,h)) )
                 - sum( (h,map_n_sto(n,sto),reserves_do) , phi_reserves_call(n,reserves_do,h) * c_m_sto(n,sto) * (RP_STO_OUT(n,reserves_do,sto,h) - RP_STO_IN(n,reserves_do,sto,h)) )
                 + sum( (h,map_n_rsvr(n,rsvr),reserves_up) , RP_RSVR(n,reserves_up,rsvr,h) * phi_reserves_call(n,reserves_up,h) * c_m_rsvr(n,rsvr) )
                 - sum( (h,map_n_rsvr(n,rsvr),reserves_do) , RP_RSVR(n,reserves_do,rsvr,h) * phi_reserves_call(n,reserves_do,h) * c_m_rsvr(n,rsvr) )
$ontext
$offtext
%reserves%$ontext
%EV%$ontext
%EV_EXOG%        + sum( (h,map_n_ev(n,ev),reserves_up) , RP_EV_V2G(n,reserves_up,ev,h) * phi_reserves_call(n,reserves_up,h) * c_m_ev(n,ev) )
%EV_EXOG%        - sum( (h,map_n_ev(n,ev),reserves_do) , RP_EV_V2G(n,reserves_do,ev,h) * phi_reserves_call(n,reserves_do,h) * c_m_ev(n,ev) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
                 + sum( (h,map_n_dsm(n,dsm_curt),reserves_up) , RP_DSM_CU(n,reserves_up,dsm_curt,h) * phi_reserves_call(n,reserves_up,h) * c_m_dsm_cu(n,dsm_curt) )
                 + sum( (h,map_n_dsm(n,dsm_shift),reserves) , RP_DSM_SHIFT(n,reserves,dsm_shift,h) * phi_reserves_call(n,reserves,h) * c_m_dsm_shift(n,dsm_shift) )
$ontext
$offtext
%prosumage%$ontext
                 + sum( map_n_res_pro(n,res) , c_i(n,res)*N_RES_PRO(n,res) )
                 + sum( map_n_res_pro(n,res) , c_fix(n,res)*N_RES_PRO(n,res) )

                 + sum( map_n_sto_pro(n,sto) , c_i_sto_e(n,sto)*N_STO_E_PRO(n,sto) )
                 + sum( map_n_sto_pro(n,sto) , c_fix_sto(n,sto)/2*(N_STO_P_PRO(n,sto) + N_STO_E_PRO(n,sto)) )
                 + sum( map_n_sto_pro(n,sto) , c_i_sto_p(n,sto)*N_STO_P_PRO(n,sto) )

                 + sum( (h,map_n_sto_pro(n,sto)) , c_m_sto(n,sto) * ( STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_M2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) + sum( res , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h)) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) ) )
$ontext
$offtext
                 + sum( map_l(l) , c_i_ntc(l) * NTC(l)*dist(l) )

                 + sum( (h,map_n_rsvr(n,rsvr)), c_m_rsvr(n,rsvr) * RSVR_OUT(n,rsvr,h) )
                 + sum( map_n_rsvr(n,rsvr) , c_i_rsvr_e(n,rsvr) * N_RSVR_E(n,rsvr) )
                 + sum( map_n_rsvr(n,rsvr) , c_i_rsvr_p(n,rsvr) * N_RSVR_P(n,rsvr) )
                 + sum( map_n_rsvr(n,rsvr) , c_fix_rsvr(n,rsvr) * N_RSVR_P(n,rsvr) )
%heat%$ontext
                 + sum( (h,n,bu,hfo) , pen_heat_fuel(n,bu,hfo) * H_STO_IN_FOSSIL(n,bu,hfo,h))
$ontext
$offtext
                 + sum( (h,n) , c_infes * G_INFES(n,h) )
;

* ---------------------------------------------------------------------------- *
***** Energy balance and load levels *****
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(n,hh)..
         ( 1 - phi_pro_load(n) ) * d(n,hh) + sum( map_n_sto(n,sto) , STO_IN(n,sto,hh) )
%DSM%$ontext
         + sum( map_n_dsm(n,dsm_shift) , DSM_UP_DEMAND(n,dsm_shift,hh) )
$ontext
$offtext
%EV%$ontext
         + sum( map_n_ev(n,ev) , EV_CHARGE(n,ev,hh) )
$ontext
$offtext
%prosumage%$ontext
         + G_MARKET_M2PRO(n,hh)
         + sum( map_n_sto_pro(n,sto) , STO_IN_M2PRO(n,sto,hh))
         + sum( map_n_sto_pro(n,sto) , STO_IN_M2M(n,sto,hh))
$ontext
$offtext
%heat%$ontext
        + sum( (bu,ch) , theta_dir(n,bu,ch) * (H_DIR(n,bu,ch,hh) + H_DHW_DIR(n,bu,ch,hh)) )
        + sum( (bu,ch) , theta_sets(n,bu,ch) * (H_SETS_IN(n,bu,ch,hh) + H_DHW_AUX_ELEC_IN(n,bu,ch,hh)) )
        + sum( (bu,hp) , theta_hp(n,bu,hp) * H_HP_IN(n,bu,hp,hh) )
        + sum( (bu,hel) , theta_elec(n,bu,hel) * H_ELECTRIC_IN(n,bu,hel,hh) )
$ontext
$offtext
         =E=
         sum( map_n_tech(n,dis) , G_L(n,dis,hh)) + sum( map_n_tech(n,nondis) , G_RES(n,nondis,hh)) + sum( sto , STO_OUT(n,sto,hh) ) + sum( map_n_rsvr(n,rsvr) , RSVR_OUT(n,rsvr,hh))
%GER_only%       + sum( map_l(l) , inc(l,n) * F(l,hh))
%reserves%$ontext
*Balancing Correction Factor
        + sum( map_n_tech(n,dis) ,
          sum( reserves_do ,  RP_DIS(n,reserves_do,dis,hh) * phi_reserves_call(n,reserves_do,hh))
        - sum( reserves_up ,  RP_DIS(n,reserves_up,dis,hh) * phi_reserves_call(n,reserves_up,hh))
         )
$ontext
$offtext
%DSM%$ontext
         + sum( map_n_dsm(n,dsm_curt) , DSM_CU(n,dsm_curt,hh))
         + sum( map_n_dsm(n,dsm_shift) , DSM_DO_DEMAND(n,dsm_shift,hh))
$ontext
$offtext
%EV%$ontext
        + sum( map_n_ev(n,ev) , EV_DISCHARGE(n,ev,hh) )
$ontext
$offtext
%prosumage%$ontext
         + sum( map_n_res_pro(n,res) , G_MARKET_PRO2M(n,res,hh) )
         + sum( map_n_sto_pro(n,sto) , STO_OUT_PRO2M(n,sto,hh))
         + sum( map_n_sto_pro(n,sto) , STO_OUT_M2M(n,sto,hh))
$ontext
$offtext
         + G_INFES(n,hh)
;

con2a_loadlevel(n,dis,h)$(ord(h) > 1 AND map_n_tech(n,dis))..
        G_L(n,dis,h) =E= G_L(n,dis,h-1) + G_UP(n,dis,h) - G_DO(n,dis,h)
;

con2b_loadlevelstart(n,dis,h)$(ord(h) = 1 AND map_n_tech(n,dis))..
         G_L(n,dis,h) =E= G_UP(n,dis,h)
;

* ---------------------------------------------------------------------------- *
***** Hourly maximum generation caps and constraints related to reserves   *****
* ---------------------------------------------------------------------------- *

con3a_maxprod_dispatchable(n,dis,h)$(map_n_tech(n,dis))..
        G_L(n,dis,h)
%reserves%$ontext
        + sum( reserves_up , RP_DIS(n,reserves_up,dis,h))
*Balancing Correction Factor
        + sum( reserves_do ,  RP_DIS(n,reserves_do,dis,h) * phi_reserves_call(n,reserves_do,h))
        - sum( reserves_up ,  RP_DIS(n,reserves_up,dis,h) * phi_reserves_call(n,reserves_up,h))
$ontext
$offtext
        =L= N_TECH(n,dis)
;


con3e_maxprod_res(n,nondis,h)$(map_n_tech(n,nondis))..
        G_RES(n,nondis,h) + CU(n,nondis,h)
%reserves%$ontext
        + sum( reserves_up , RP_NONDIS(n,reserves_up,nondis,h))
$ontext
$offtext
        =E= phi_res(n,nondis,h) * N_TECH(n,nondis)
;

* ---------------------------------------------------------------------------- *
***** Storage constraints *****
* ---------------------------------------------------------------------------- *

con4a_stolev_start(n,sto,h)$(map_n_sto(n,sto) AND ord(h) = 1)..
        STO_L(n,sto,h) =E= phi_sto_ini(n,sto) * N_STO_E(n,sto) + STO_IN(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT(n,sto,h)/(1+eta_sto(n,sto))*2
;

con4b_stolev(n,sto,h)$((ord(h)>1) AND map_n_sto(n,sto))..
         STO_L(n,sto,h) =E= STO_L(n,sto,h-1) + STO_IN(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT(n,sto,h)/(1+eta_sto(n,sto))*2
%reserves%$ontext
         + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * ( RP_STO_IN(n,reserves_do,sto,h)*(1+eta_sto(n,sto))/2 + RP_STO_OUT(n,reserves_do,sto,h)/(1+eta_sto(n,sto))*2 ))
         - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * ( RP_STO_IN(n,reserves_up,sto,h)*(1+eta_sto(n,sto))/2 + RP_STO_OUT(n,reserves_up,sto,h)/(1+eta_sto(n,sto))*2 ))
$ontext
$offtext
;

con4c_stolev_max(n,sto,h)$(map_n_sto(n,sto))..
        STO_L(n,sto,h) =L= N_STO_E(n,sto)
;

con4d_maxin_sto(n,sto,h)$(map_n_sto(n,sto))..
        STO_IN(n,sto,h)
%reserves%$ontext
        + sum( reserves_do , RP_STO_IN(n,reserves_do,sto,h))
$ontext
$offtext
        =L= N_STO_P(n,sto)
;

con4e_maxout_sto(n,sto,h)$(map_n_sto(n,sto))..
        STO_OUT(n,sto,h)
%reserves%$ontext
        + sum( reserves_up , RP_STO_OUT(n,reserves_up,sto,h))
$ontext
$offtext
        =L= N_STO_P(n,sto)
;

con4j_ending(n,sto,h)$(ord(h) = card(h) AND map_n_sto(n,sto))..
         STO_L(n,sto,h) =E= phi_sto_ini(n,sto) * N_STO_E(n,sto)
;

con4k_PHS_EtoP(n,sto)$(map_n_sto(n,sto))..
        N_STO_E(n,sto) =L= etop_max(n,sto) * N_STO_P(n,sto)
;

* ---------------------------------------------------------------------------- *
***** Quotas for renewables and biomass *****
* ---------------------------------------------------------------------------- *

con5a_minRES(n)..
sum( h , G_L(n,'bio',h) + sum( map_n_tech(n,nondis) , G_RES(n,nondis,h)) + sum( map_n_rsvr(n,rsvr) , RSVR_OUT(n,rsvr,h))
%reserves%$ontext
         - sum( reserves_do , (sum( map_n_tech(n,nondis) , RP_NONDIS(n,reserves_do,nondis,h)) + sum( map_n_rsvr(n,rsvr) , RP_RSVR(n,reserves_do,rsvr,h))) * phi_reserves_call(n,reserves_do,h))
         + sum( reserves_up , (sum( map_n_tech(n,nondis) , RP_NONDIS(n,reserves_up,nondis,h)) + sum( map_n_rsvr(n,rsvr) , RP_RSVR(n,reserves_up,rsvr,h))) * phi_reserves_call(n,reserves_up,h))
$ontext
$offtext
%prosumage%$ontext
         + sum( map_n_sto_pro(n,sto) , STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h)) + sum( map_n_res_pro(n,res) , G_MARKET_PRO2M(n,res,h) + G_RES_PRO(n,res,h))
$ontext
$offtext
)
        =G= phi_min_res * phi_min_res_exog(n) * sum( h ,
         sum( map_n_tech(n,dis) , G_L(n,dis,h)) + sum( map_n_tech(n,nondis) , G_RES(n,nondis,h)) + sum( map_n_rsvr(n,rsvr) , RSVR_OUT(n,rsvr,h))
%reserves%$ontext
         - sum( reserves_do , (sum( map_n_tech(n,nondis) , RP_NONDIS(n,reserves_do,nondis,h)) + sum( map_n_rsvr(n,rsvr) , RP_RSVR(n,reserves_do,rsvr,h))) * phi_reserves_call(n,reserves_do,h))
         + sum( reserves_up , (sum( map_n_tech(n,nondis) , RP_NONDIS(n,reserves_up,nondis,h)) + sum( map_n_rsvr(n,rsvr) , RP_RSVR(n,reserves_up,rsvr,h))) * phi_reserves_call(n,reserves_up,h))
$ontext
$offtext
%prosumage%$ontext
         + sum( map_n_res_pro(n,res) , phi_res(n,res,h) * N_RES_PRO(n,res) - CU_PRO(n,res,h))
$ontext
$offtext
         )
;

con5b_max_energy(n,dis)$(map_n_tech(n,dis) AND m_e(n,dis))..
         sum( h , G_L(n,dis,h) ) =L= m_e(n,dis)
;

* ---------------------------------------------------------------------------- *
***** Maximum installation constraints *****
* ---------------------------------------------------------------------------- *

con8a_max_I_power(n,tech)$(map_n_tech(n,tech))..
         N_TECH(n,tech) =L= m_p(n,tech)
;

con8b_max_I_sto_e(n,sto)$(map_n_sto(n,sto))..
         N_STO_E(n,sto) =L= m_sto_e(n,sto)
;

con8c_max_I_sto_p(n,sto)$(map_n_sto(n,sto))..
         N_STO_P(n,sto) =L= m_sto_p(n,sto)
;


con8f_max_pro_res(n,res)$(map_n_res_pro(n,res))..
         N_RES_PRO(n,res) =L= m_res_pro(n,res)
;

con8g_max_pro_sto_e(n,sto)$(map_n_sto_pro(n,sto))..
         N_STO_E_PRO(n,sto) =L= m_sto_pro_e(n,sto)
;

con8h_max_sto_pro_p(n,sto)$(map_n_sto_pro(n,sto))..
         N_STO_P_PRO(n,sto) =L= m_sto_pro_p(n,sto)
;

* ---------------------------------------------------------------------------- *
***** Prosumage constraints *****
* ---------------------------------------------------------------------------- *

con11a_pro_distrib(n,res,h)$(map_n_res_pro(n,res))..
         phi_res(n,res,h) * N_RES_PRO(n,res)
         =E=
         CU_PRO(n,res,h) + G_MARKET_PRO2M(n,res,h) + G_RES_PRO(n,res,h) + sum( map_n_sto_pro(n,sto) , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h) )
;

con11b_pro_balance(n,h)..
         phi_pro_load(n) * d(n,h)
         =E=
         sum( map_n_res_pro(n,res) , G_RES_PRO(n,res,h)) + sum( map_n_sto_pro(n,sto) , STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_M2PRO(n,sto,h) ) + G_MARKET_M2PRO(n,h)
;

con11c_pro_selfcon(n)..
         sum( (h,map_n_res_pro(n,res)) , G_RES_PRO(n,res,h) ) + sum( (h,sto) , STO_OUT_PRO2PRO(n,sto,h) )
         =G=
         phi_pro_self * sum( h , phi_pro_load(n) * d(n,h))
;

con11d_pro_stolev_PRO2PRO(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) > 1 )..
         STO_L_PRO2PRO(n,sto,h) =E= STO_L_PRO2PRO(n,sto,h-1) + sum( map_n_res_pro(n,res) , STO_IN_PRO2PRO(n,res,sto,h))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11e_pro_stolev_PRO2M(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) > 1)..
         STO_L_PRO2M(n,sto,h) =E= STO_L_PRO2M(n,sto,h-1) + sum( map_n_res_pro(n,res) , STO_IN_PRO2M(n,res,sto,h))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11f_pro_stolev_M2PRO(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) > 1)..
         STO_L_M2PRO(n,sto,h) =E= STO_L_M2PRO(n,sto,h-1) + STO_IN_M2PRO(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT_M2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11g_pro_stolev_M2M(n,sto,h)$(ord(h) > 1)..
         STO_L_M2M(n,sto,h) =E= STO_L_M2M(n,sto,h-1) + STO_IN_M2M(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT_M2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_1_pro_stolev_start_PRO2PRO(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) = 1)..
        STO_L_PRO2PRO(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + sum( map_n_res_pro(n,res) , STO_IN_PRO2PRO(n,res,sto,h))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_2_pro_stolev_start_PRO2M(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) = 1)..
        STO_L_PRO2M(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + sum( map_n_res_pro(n,res) , STO_IN_PRO2M(n,res,sto,h))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_3_pro_stolev_start_M2PRO(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) = 1)..
        STO_L_M2PRO(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + STO_IN_M2PRO(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT_M2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_4_pro_stolev_start_M2M(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) = 1)..
        STO_L_M2M(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + STO_IN_M2M(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT_M2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11i_pro_stolev(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h)>1)..
         STO_L_PRO(n,sto,h) =E=   STO_L_PRO2PRO(n,sto,h) +  STO_L_PRO2M(n,sto,h) + STO_L_M2PRO(n,sto,h) + STO_L_M2M(n,sto,h)
;

con11j_pro_stolev_max(n,sto,h)..
        STO_L_PRO(n,sto,h) =L= N_STO_E_PRO(n,sto)
;

con11k_pro_maxin_sto(n,sto,h)$(map_n_sto_pro(n,sto))..
        sum( map_n_res_pro(n,res) , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h) ) + STO_IN_M2PRO(n,sto,h) + STO_IN_M2M(n,sto,h)
        =L= N_STO_P_PRO(n,sto)
;

con11l_pro_maxout_sto(n,sto,h)$(map_n_sto_pro(n,sto))..
        STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2PRO(n,sto,h) + STO_OUT_M2M(n,sto,h)
        =L= N_STO_P_PRO(n,sto)
;

con11m_pro_maxout_lev(n,sto,h)$(map_n_sto_pro(n,sto))..
        ( STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_M2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) ) / (1+eta_sto(n,sto))*2
        =L= STO_L_PRO(n,sto,h-1)
;

con11n_pro_maxin_lev(n,sto,h)$(map_n_sto_pro(n,sto))..
        ( sum( map_n_res_pro(n,res) , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h) ) + STO_IN_M2PRO(n,sto,h) + STO_IN_M2M(n,sto,h) ) * (1+eta_sto(n,sto))/2
        =L= N_STO_E_PRO(n,sto) - STO_L_PRO(n,sto,h-1)
;

con11o_pro_ending(n,sto,h)$(map_n_sto_pro(n,sto) AND ord(h) = card(h))..
         STO_L_PRO(n,sto,h) =E= phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto)
;

********************************************************************************
***** MODEL *****
********************************************************************************

model DIETER /
obj

con1a_bal

con2a_loadlevel
con2b_loadlevelstart

con3a_maxprod_dispatchable

con3e_maxprod_res



con4a_stolev_start
con4b_stolev
con4c_stolev_max
con4d_maxin_sto
con4e_maxout_sto

con4j_ending
con4k_PHS_EtoP

con5a_minRES
con5b_max_energy


con8a_max_I_power
con8b_max_I_sto_e
con8c_max_I_sto_p

%prosumage%$ontext
con8f_max_pro_res
con8g_max_pro_sto_e
con8h_max_sto_pro_p
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
