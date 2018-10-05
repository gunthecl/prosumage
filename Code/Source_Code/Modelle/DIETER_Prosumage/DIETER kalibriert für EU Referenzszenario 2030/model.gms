
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.#, April 2018.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************


Variables
Z                Value objective function in Euro
F(l,h)           Energy flow over link l in hour h in MWh
;

Positive Variables
G_L(n,tech,h)            Generation level in hour h in MWh
G_UP(n,tech,h)           Generation upshift in hour h in MWh
G_DO(n,tech,h)           Generation downshift in hour h in MWh

G_RES(n,tech,h)          Generation renewables type res in hour h in MWh
CU(n,tech,h)             Renewables curtailment technology res in hour h in MWh

STO_IN(n,sto,h)          Storage inflow technology sto hour h in MWh
STO_OUT(n,sto,h)         Storage outflow technology sto hour h in MWh
STO_L(n,sto,h)           Storage level technology sto hour h in MWh

EV_CHARGE_M(n,ev,h)      Electric vehicle charging from market vehicle profile ev hour h in MWh
EV_CHARGE_PRO(n,tech,ev,h)       Electric vehicle charging under prosumage vehicle profile ev hour h in MWh

EV_DISCHARGE(n,ev,h)     Electric vehicle discharging vehicle profile ev hour h in MWh
EV_L(n,ev,h)             Electric vehicle charging level vehicle profile ev hour h in MWh
EV_PHEVFUEL(n,ev,h)      Plug in hybrid electric vehicle conventional fuel use vehicle profile ev hour h in MWh
EV_MPED(n,ev,h)          Electricity demand for mobility from market or from prosumage vehicle profile ev hour h in MWh

N_TECH(n,tech)           Technology ct built in MW
N_STO_E(n,sto)           Storage technology built - Energy in MWh
N_STO_P(n,sto)           Storage loading and discharging capacity built - Capacity in MW

DSM_CU(n,dsm,h)          DSM: Load curtailment hour h in MWh
DSM_UP(n,dsm,h)          DSM: Load shifting up hour h technology dsm in MWh
DSM_DO(n,dsm,h,hh)       DSM: Load shifting down in hour hh to account for upshifts in hour h technology dsm in MWh

DSM_UP_DEMAND(n,dsm,h)   DSM: Load shifting up active for wholesale demand in hour h of technology dsm in MWh
DSM_DO_DEMAND(n,dsm,h)   DSM: Load shifting down active for wholesale demand in hour h of technology dsm in MWh

N_DSM_CU(n,dsm)          DSM: Load curtailment capacity in MW
N_DSM_SHIFT(n,dsm)       DSM: Load shifting capacity in MWh

RP_DIS(n,reserves,tech,h)        Reserve provision by conventionals in hour h in MW
RP_NONDIS(n,reserves,tech,h)     Reserve provision by renewables in hour h in MW
RP_STO_IN(n,reserves,sto,h)      Reserve provision by storage in in hour h in MW
RP_STO_OUT(n,reserves,sto,h)     Reserve provision by storage out in hour h in MW
RP_EV_V2G(n,reserves,ev,h)       Reserve provision by electric vehicles V2G hour h in MW
RP_EV_G2V(n,reserves,ev,h)       Reserve provision by electric vehicles G2V hour h in MW
RP_DSM_CU(n,reserves,dsm,h)      Reserve provision by DSM load curtailment in hour h in MW
RP_DSM_SHIFT(n,reserves,dsm,h)   Reserve provision by DSM load shifting in hour h in MW
RP_RSVR(n,reserves,rsvr,h)       Reserve provision by reservoirs h in MW
RP_SETS(n,reserves,bu,ch,h)      Reserve provision by SETS
RP_SETS_AUX(n,reserves,bu,ch,h)
RP_HP(n,reserves,bu,ch,h)        Reserve provision by heat pumps
RP_H_ELEC(n,reserves,bu,ch,h)    Reserve provision by hybrid electric heaters

CU_PRO(n,tech,h)                 Prosumage: curtailment of renewable generation in hour h in MWh
G_MARKET_PRO2M(n,tech,h)         Prosumage. energy sent to market in hour h in MWh
G_MARKET_M2PRO(n,h)              Prosumage: withdrawal of energy from market in hour h in MWh
G_RES_PRO(n,tech,h)              Prosumage: hourly renewables generation in hour h in MWh
STO_IN_PRO2PRO(n,tech,sto,h)     Prosumage: storage loading from generation for discharging to consumption in hour h in MWh
STO_IN_PRO2M(n,tech,sto,h)       Prosumage: storage loading from generation for discharging to market in hour h in MWh
STO_IN_M2PRO(n,sto,h)            Prosumage: storage loading from market for discharging to consumption in hour h in MWh
STO_IN_M2M(n,sto,h)              Prosumage: storage loading from market for discharging to market in hour h in MWh
STO_OUT_PRO2PRO(n,sto,h)         Prosumage: storage discharging to consumption from generation in hour h in MWh
STO_OUT_PRO2M(n,sto,h)           Prosumage: storage discharging to market from generation in hour h in MWh
STO_OUT_M2PRO(n,sto,h)           Prosumage: storage discharging to consumption from market in hour h in MWh
STO_OUT_M2M(n,sto,h)             Prosumage: storage discharging to market from market in hour h in MWh
STO_L_PRO2PRO(n,sto,h)           Prosumage: storage level generation to consumption in hour h in MWh
STO_L_PRO2M(n,sto,h)             Prosumage: storage level generation to market in hour h in MWh
STO_L_M2PRO(n,sto,h)             Prosumage: storage level market to consumotion in hour h in MWh
STO_L_M2M(n,sto,h)               Prosumage: storage level market to market in hour h in MWh
N_STO_E_PRO(n,sto)               Prosumage: installed storage energy in MWh
N_STO_P_PRO(n,sto)               Prosumage: installed storage power in MW
STO_L_PRO(n,sto,h)               Prosumage: overall storage level in hour h in MWh
N_RES_PRO(n,tech)                Prosumage: installed renewables capacities in MW
*### new:
*PHI_PRO_HEAT(n)                  Prosumage: share of overall space heat demand covered by prosumage-P2H
*PHI_PRO_DHW(n)                   Prosumage: share of overall DHW demand covered by prosumage-P2H
*PHI_PRO_EV(n)                    Prosumage: share of overall EVs covered by prosumage
* note: currently, heating and EV modules only connected to prosumage, not to non-prosumage use
AUX_HEAT(n,bu,ch,h)
AUX_DHW(n,bu,ch,h)

NTC(l)                           Trade: installed NTC on line l in MW

RSVR_OUT(n,rsvr,h)               Reservoirs: outflow in hour h in MWh
RSVR_L(n,rsvr,h)                 Reservoirs: level in hour h in MWh
N_RSVR_E(n,rsvr)                 Reservoirs: installed energy capacity in MWh
N_RSVR_P(n,rsvr)                 Reservoirs: installed power capacity in MW

H_DIR(n,bu,ch,h)                 Heating: direct heating in hour h in MWh for building type bu with haeting technology ch
H_SETS_LEV(n,bu,ch,h)            Heating: storage level SETS technologies in MWh
H_SETS_IN(n,tech,bu,ch,h)             Heating: storage inflow SETS technologies in MWh
H_SETS_OUT(n,bu,ch,h)            Heating: storage outflow SETS technologies in MWh
H_HP_IN(n,bu,ch,hh)              Heating: electricity demand heat pump technologies in MWh
H_STO_LEV(n,bu,ch,h)             Heating: storage level storage technologies in MWh
H_STO_IN_HP(n,bu,ch,h)           Heating: storage inflow from heat pumps to storage technologies in MWh
H_STO_IN_ELECTRIC(n,bu,ch,h)     Heating: storage inflow from electric heating to storage technologies in MWh
H_ELECTRIC_IN(n,bu,ch,h)         Heating: hybrid electric heaters electricity demand
H_STO_IN_FOSSIL(n,bu,ch,h)       Heating: storage inflow from nonelectric heating to storage technologies in MWh
H_STO_OUT(n,bu,ch,h)             Heating: storage outflow from storage technologies in MWh
*### temporary solution:
N_SETS_P_OUT(n,bu,ch)
N_SETS_DHW_P_IN(n,bu,ch)

H_DHW_DIR(n,bu,ch,h)             Heating - domestic hot water: provision in case of direct electric heating
H_DHW_STO_OUT(n,bu,ch,h)         Heating - domestic hot water: DHW storage outflow

H_DHW_AUX_ELEC_IN(n,bu,ch,h)     Heating - domestic hot water: electrical energy input
H_DHW_AUX_LEV(n,bu,ch,h)         Heating - domestic hot water: level of auxiliary hot water tank for SETS
H_DHW_AUX_OUT(n,bu,ch,h)         Heating - domestic hot water: auxiliary DHW provision for SETS
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
con3b_minprod_dispatchable       Minimum production conventionals if reserves contracted
con3c_flex_reserves_spin         Flexibility of conventionals for reserves provision
con3d_flex_reserves_nonspin      Flexibility of conventionals for reserves provision
con3e_maxprod_res                Capacity constraints renewables
con3f_minprod_res                Minimum production RES if reserves contracted

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
con5b_max_energy         Maximum yearly biomass energy

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
con8a_max_I_power                Maximum installable capacity: Conventionals
con8b_max_I_sto_e                Maximum installable energy: Storage in MWh
con8c_max_I_sto_p                Maximum installable capacity: Storage inflow-outflow in MW
con8d_max_I_dsm_cu               Maximum installable capacity: DSM load curtailment
con8e_max_I_dsm_shift_pos        Maximum installable capacity: DSM load shifting
con8f_max_pro_res                Maximum installable capacity: prosumage renewables
con8g_max_pro_sto_e              Maximum installable capacity: prosumage storage energy
con8h_max_sto_pro_p              Maximum installable capacity: prosumage storage power
con8i_max_I_ntc                  Maximum installable NTC
con8j_max_I_rsvr_e               Maximum installable energy reservoirs
con8k_max_I_rsvr_p               Maximum installable power reservoirs

* Reserves
con9a_reserve_prov_endogenous    Reserve provision SR and MR (endogenous reserve provision)
con9a_reserve_prov_exogenous     Reserve provision SR and MR (exogenous reserve provision)
con9b_reserve_prov_PR_endogenous Reserve provision PR (endogenous reserve provision)
con9b_reserve_prov_PR_exogenous  Reserve provision PR (exogenous reserve provision)

* Electric vehicles
con10a_ev_ed                     Energy balance of electric vehicles
con10b_ev_chargelev_start        Cumulative charging level in the first hour
con10c_ev_chargelev              Cumulative charging level in hour h
con10d_ev_chargelev_max          Cumulative maximal charging level
con10e_ev_maxin                  Cumulative maximal charging power
con10f_ev_maxout                 Cumulative maximal discharging power
con10g_ev_chargelev_ending       Cumulative charging level in the last hour
con10h_ev_minin                  Cumulative minimal charging power
con10i_ev_maxin_lev              Cumulative maximal charging limit
con10j_ev_minout                 Cumulative minimal discharging power
con10k_ev_maxout_lev             Cumulative maximal discharging limit
con10l_ev_exog                   Exogenous EV charging

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

* Cross-nodal trade
con12a_max_f                     Maximum energy flow limited to positive NTC
con12b_min_f                     Minimum energy flow limited to negative NTC

* Resevoirs
con13a_rsvrlev_start             Reservoir level law of motion initial condition
con13b_rsvrlev                   Reservoir level law of motion
con13c_rsvrlev_max               Maximum reservoir energy level
con13d_maxout_rsvr               Maximum hourly reservoir outflow in relation to installed power capacity
con13e_resrv_rsvr                Minimum hourly reservoir outflow in relation to provided negativr reserves
con13f_maxout_lev                Maximum hourly reservoir outflow in relation tom installed energy capacity
con13g_ending                    Reservoir level law of motion ending condition
con13h_smooth                    Smooth reservoir outflow
con13i_min_level                 Reservoir minimum level
con13j_min_FLH


* Residential heat
con14a_heat_balance
con14b_dhw_balance
con14c_sets_level
con14d_sets_level_start
con14e_sets_maxin
con14f_sets_maxout
con14g_sets_minin
con14h_sets_maxlev
con14i_sets_aux_dhw_level
con14j_sets_aux_dhw_level_start
con14k_sets_aux_dhw_maxin
con14l_sets_aux_dhw_minin
con14m_sets_aux_dhw_maxlev
con14n_hp_in
con14o_hp_maxin
con14p_hp_minin
con14q_storage_elec_in
con14r_storage_elec_maxin
con14s_storage_elec_minin
con14t_storage_level
con14u_storage_level_start
con14v_storage_maxlev
;




********************************************************************************

* ---------------------------------------------------------------------------- *
***** Objective function *****
* ---------------------------------------------------------------------------- *

obj..
         Z =E=
                 sum( (dis,h,n) , c_m(n,dis)*G_L(n,dis,h) )
                 + sum( (dis,h,n)$(ord(h)>1) , c_up(n,dis)*G_UP(n,dis,h) )
                 + sum( (dis,h,n) , c_do(n,dis)*G_DO(n,dis,h) )
                 + sum( (nondis,h,n) , c_cu(n,nondis)*CU(n,nondis,h) )
                 + sum( (sto,h,n) , c_m_sto(n,sto) * ( STO_OUT(n,sto,h) + STO_IN(n,sto,h) ) )
%DSM%$ontext
                 + sum( (dsm_curt,h,n) , c_m_dsm_cu(n,dsm_curt)*DSM_CU(n,dsm_curt,h) )
                 + sum( (dsm_shift,h,n) , c_m_dsm_shift(n,dsm_shift) * DSM_UP_DEMAND(n,dsm_shift,h) )
                 + sum( (dsm_shift,h,n) , c_m_dsm_shift(n,dsm_shift) * DSM_DO_DEMAND(n,dsm_shift,h) )
$ontext
$offtext
%EV%$ontext
                 + sum( (ev,h,n) , c_m_ev(n,ev) * EV_DISCHARGE(n,ev,h) )
                 + sum( (ev,h,n) , pen_phevfuel(n,ev) * EV_PHEVFUEL(n,ev,h) )
* Heizölpreis = Dieselpreis (?)
*                 + sum( (n,ev,h) , EV_PHEVFUEL(n,ev,h) * 50.51778 )
$ontext
$offtext
                 + sum( (n,tech) , c_i(n,tech)*N_TECH(n,tech) )
                 + sum( (tech,n) , c_fix(n,tech)*N_TECH(n,tech) )
                 + sum( (sto,n) , c_i_sto_e(n,sto)*N_STO_E(n,sto) )
                 + sum( (sto,n) , c_fix_sto(n,sto)/2*(N_STO_P(n,sto)+N_STO_E(n,sto)) )
                 + sum( (sto,n) , c_i_sto_p(n,sto)*N_STO_P(n,sto) )
%DSM%$ontext
                 + sum( (dsm_curt,n) , c_i_dsm_cu(n,dsm_curt)*N_DSM_CU(n,dsm_curt) )
                 + sum( (dsm_curt,n) , c_fix_dsm_cu(n,dsm_curt)*N_DSM_CU(n,dsm_curt) )
                 + sum( (dsm_shift,n) , c_i_dsm_shift(n,dsm_shift)*N_DSM_SHIFT(n,dsm_shift) )
                 + sum( (dsm_shift,n) , c_fix_dsm_shift(n,dsm_shift)*N_DSM_SHIFT(n,dsm_shift) )
$ontext
$offtext
%reserves%$ontext
                 + sum( (reserves_up,sto,h,n) , phi_reserves_call(n,reserves_up,h) * c_m_sto(n,sto) * (RP_STO_OUT(n,reserves_up,sto,h) - RP_STO_IN(n,reserves_up,sto,h)) )
                 - sum( (reserves_do,sto,h,n) , phi_reserves_call(n,reserves_do,h) * c_m_sto(n,sto) * (RP_STO_OUT(n,reserves_do,sto,h) - RP_STO_IN(n,reserves_do,sto,h)) )
                 + sum( (reserves_up,rsvr,h,n) , RP_RSVR(n,reserves_up,rsvr,h) * phi_reserves_call(n,reserves_up,h) * c_m_rsvr(n,rsvr) )
                 - sum( (reserves_do,rsvr,h,n) , RP_RSVR(n,reserves_do,rsvr,h) * phi_reserves_call(n,reserves_do,h) * c_m_rsvr(n,rsvr) )
$ontext
$offtext
%reserves%$ontext
%EV%$ontext
%EV_EXOG%        + sum( (reserves_up,ev,h,n) , RP_EV_V2G(n,reserves_up,ev,h) * phi_reserves_call(n,reserves_up,h) * c_m_ev(n,ev) )
%EV_EXOG%        - sum( (reserves_do,ev,h,n) , RP_EV_V2G(n,reserves_do,ev,h) * phi_reserves_call(n,reserves_do,h) * c_m_ev(n,ev) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
                 + sum( (reserves_up,dsm_curt,h,n) , RP_DSM_CU(n,reserves_up,dsm_curt,h) * phi_reserves_call(n,reserves_up,h) * c_m_dsm_cu(n,dsm_curt) )
                 + sum( (reserves,dsm_shift,h,n) , RP_DSM_SHIFT(n,reserves,dsm_shift,h) * phi_reserves_call(n,reserves,h) * c_m_dsm_shift(n,dsm_shift) )
$ontext
$offtext
%prosumage%$ontext
                 + sum( (res,n) , c_i(n,res)*N_RES_PRO(n,res) )
                 + sum( (res,n) , c_fix(n,res)*N_RES_PRO(n,res) )

                 + sum( (sto,n) , c_i_sto_e(n,sto)*N_STO_E_PRO(n,sto) )
                 + sum( (sto,n) , c_fix_sto(n,sto)/2*(N_STO_P_PRO(n,sto) + N_STO_E_PRO(n,sto)) )
                 + sum( (sto,n) , c_i_sto_p(n,sto)*N_STO_P_PRO(n,sto) )

                 + sum( (sto,h,n) , c_m_sto(n,sto) * ( STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_M2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) + sum( res , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h)) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) ) )
*### new (temporarily hard-coded and preliminary data) :
                 + sum( (n,bu,ch) , c_i_sets_e(n,bu,ch) * 16 * N_SETS_P_OUT(n,bu,ch) )
*                 + sum( (n,bu,ch) , c_i_sets_dhw_e(n,bu,ch) * 2.2 * N_SETS_DHW_P_IN(n,bu,ch) )
*                 + sum( (n,bu,ch,h) , AUX_HEAT(n,bu,ch,h) * 1000000 )
* Erdgaspreis
                 + sum( (n,bu,ch,h) , AUX_HEAT(n,bu,ch,h) * 32.72733 )
*                 + sum( (n,bu,ch,h) , AUX_DHW(n,bu,ch,h) * 1000000 )
$ontext
$offtext
                 + sum( l , c_i_ntc(l) * NTC(l)*dist(l) )

                 + sum( (rsvr,h,n), c_m_rsvr(n,rsvr) * RSVR_OUT(n,rsvr,h) )
                 + sum( (rsvr,n) , c_i_rsvr_e(n,rsvr) * N_RSVR_E(n,rsvr) )
                 + sum( (rsvr,n) , c_i_rsvr_p(n,rsvr) * N_RSVR_P(n,rsvr) )
                 + sum( (n,rsvr) , c_fix_rsvr(n,rsvr) * N_RSVR_P(n,rsvr) )
%heat%$ontext
                 + sum( (n,bu,hfo,h) , pen_heat_fuel(n,bu,hfo) * H_STO_IN_FOSSIL(n,bu,hfo,h))
$ontext
$offtext
                 + sum( (n,h) , c_infes * G_INFES(n,h) )
;

* ---------------------------------------------------------------------------- *
***** Energy balance and load levels *****
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(n,hh)..
         ( 1 - phi_pro_load(n) ) * d(n,hh) + sum( sto , STO_IN(n,sto,hh) )
%DSM%$ontext
         + sum( dsm_shift , DSM_UP_DEMAND(n,dsm_shift,hh) )
$ontext
$offtext
%EV%$ontext
         + sum( ev , EV_CHARGE_M(n,ev,hh) )
$ontext
$offtext
%prosumage%$ontext
         + G_MARKET_M2PRO(n,hh)
         + sum( sto , STO_IN_M2PRO(n,sto,hh))
         + sum( sto , STO_IN_M2M(n,sto,hh))
$ontext
$offtext
%heat%$ontext
*        + sum( (bu,ch) , theta_dir(n,bu,ch) * (H_DIR(n,bu,ch,hh) + H_DHW_DIR(n,bu,ch,hh)) )
*°
*        + sum( (bu,ch) , theta_sets(n,bu,ch) * (H_SETS_IN(n,bu,ch,hh) + H_DHW_AUX_ELEC_IN(n,bu,ch,hh)) )
*        + sum( (bu,hp) , theta_hp(n,bu,hp) * H_HP_IN(n,bu,hp,hh) )
*        + sum( (bu,hel) , theta_elec(n,bu,hel) * H_ELECTRIC_IN(n,bu,hel,hh) )
$ontext
$offtext
         =E=
         sum( dis , G_L(n,dis,hh)) + sum( nondis , G_RES(n,nondis,hh)) + sum( sto , STO_OUT(n,sto,hh) )
         + sum( rsvr , RSVR_OUT(n,rsvr,hh))
%GER_only%       + sum( l , inc(l,n) * F(l,hh))
%reserves%$ontext
*Balancing Correction Factor
        + sum( dis ,
          sum( reserves_do ,  RP_DIS(n,reserves_do,dis,hh) * phi_reserves_call(n,reserves_do,hh))
        - sum( reserves_up ,  RP_DIS(n,reserves_up,dis,hh) * phi_reserves_call(n,reserves_up,hh))
         )
$ontext
$offtext
%DSM%$ontext
         + sum(dsm_curt, DSM_CU(n,dsm_curt,hh))
         + sum(dsm_shift, DSM_DO_DEMAND(n,dsm_shift,hh))
$ontext
$offtext
%EV%$ontext
        + sum( ev , EV_DISCHARGE(n,ev,hh) )
$ontext
$offtext
%prosumage%$ontext
         + sum( res , G_MARKET_PRO2M(n,res,hh) )
         + sum( sto , STO_OUT_PRO2M(n,sto,hh))
         + sum( sto , STO_OUT_M2M(n,sto,hh))
$ontext
$offtext
         + G_INFES(n,hh)
;

con2a_loadlevel(dis,h,n)$(ord(h) > 1 AND m_p(n,dis))..
        G_L(n,dis,h) =E= G_L(n,dis,h-1) + G_UP(n,dis,h) - G_DO(n,dis,h)
;

con2b_loadlevelstart(dis,h,n)$(ord(h) = 1 AND m_p(n,dis))..
         G_L(n,dis,h) =E= G_UP(n,dis,h)
;

* ---------------------------------------------------------------------------- *
***** Hourly maximum generation caps and constraints related to reserves   *****
* ---------------------------------------------------------------------------- *

con3a_maxprod_dispatchable(dis,h,n)..
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

con3b_minprod_dispatchable(dis,h,n)..
        sum( reserves_do , RP_DIS(n,reserves_do,dis,h))
        =L= G_L(n,dis,h)
* Balancing Correction Factor
        + sum( reserves_do ,  RP_DIS(n,reserves_do,dis,h) * phi_reserves_call(n,reserves_do,h))
        - sum( reserves_up ,  RP_DIS(n,reserves_up,dis,h) * phi_reserves_call(n,reserves_up,h))
;

con3c_flex_reserves_spin(reserves_spin,dis,h,n)..
        RP_DIS(n,reserves_spin,dis,h)
        =L= grad_per_min(n,dis) * reserves_reaction(n,reserves_spin) * ( G_L(n,dis,h)
* Balancing Correction Factor
        + sum( reserves_do ,  RP_DIS(n,reserves_do,dis,h) * phi_reserves_call(n,reserves_do,h))
        - sum( reserves_up ,  RP_DIS(n,reserves_up,dis,h) * phi_reserves_call(n,reserves_up,h)) )
;

con3d_flex_reserves_nonspin(reserves_nonspin,dis,h,n)..
        RP_DIS(n,reserves_nonspin,dis,h)
        =L= grad_per_min(n,dis) * reserves_reaction(n,reserves_nonspin) * N_TECH(n,dis)
;

con3e_maxprod_res(nondis,h,n)..
        G_RES(n,nondis,h) + CU(n,nondis,h)
%reserves%$ontext
        + sum( reserves_up , RP_NONDIS(n,reserves_up,nondis,h))
$ontext
$offtext
        =E= phi_res(n,nondis,h) * N_TECH(n,nondis)
;

con3f_minprod_res(nondis,h,n)..
        sum( reserves_do , RP_NONDIS(n,reserves_do,nondis,h))
        =L= G_RES(n,nondis,h)
;

* ---------------------------------------------------------------------------- *
***** Storage constraints *****
* ---------------------------------------------------------------------------- *

con4a_stolev_start(sto,h,n)$(ord(h) = 1)..
        STO_L(n,sto,h) =E= phi_sto_ini(n,sto) * N_STO_E(n,sto) + STO_IN(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT(n,sto,h)/(1+eta_sto(n,sto))*2
;

con4b_stolev(sto,h,n)$((ord(h)>1) AND m_sto_e(n,sto))..
         STO_L(n,sto,h) =E= STO_L(n,sto,h-1) + STO_IN(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT(n,sto,h)/(1+eta_sto(n,sto))*2
%reserves%$ontext
         + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * ( RP_STO_IN(n,reserves_do,sto,h)*(1+eta_sto(n,sto))/2 + RP_STO_OUT(n,reserves_do,sto,h)/(1+eta_sto(n,sto))*2 ))
         - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * ( RP_STO_IN(n,reserves_up,sto,h)*(1+eta_sto(n,sto))/2 + RP_STO_OUT(n,reserves_up,sto,h)/(1+eta_sto(n,sto))*2 ))
$ontext
$offtext
;

con4c_stolev_max(sto,h,n)..
        STO_L(n,sto,h) =L= N_STO_E(n,sto)
;

con4d_maxin_sto(sto,h,n)..
        STO_IN(n,sto,h)
%reserves%$ontext
        + sum( reserves_do , RP_STO_IN(n,reserves_do,sto,h))
$ontext
$offtext
        =L= N_STO_P(n,sto)
;

con4e_maxout_sto(sto,h,n)..
        STO_OUT(n,sto,h)
%reserves%$ontext
        + sum( reserves_up , RP_STO_OUT(n,reserves_up,sto,h))
$ontext
$offtext
        =L= N_STO_P(n,sto)
;

con4f_resrv_sto(sto,h,n)$m_sto_e(n,sto)..
        sum( reserves_up , RP_STO_IN(n,reserves_up,sto,h))
        =L= STO_IN(n,sto,h)
;

con4g_resrv_sto(sto,h,n)$m_sto_e(n,sto)..
        sum( reserves_do , RP_STO_OUT(n,reserves_do,sto,h))
        =L= STO_OUT(n,sto,h)
;

con4h_maxout_lev(sto,h,n)$m_sto_e(n,sto)..
        ( STO_OUT(n,sto,h)
%reserves%$ontext
        + sum( reserves_up , RP_STO_OUT(n,reserves_up,sto,h))
$ontext
$offtext
        ) /(1+eta_sto(n,sto))*2
        =L= STO_L(n,sto,h-1)
;

con4i_maxin_lev(sto,h,n)..
        ( STO_IN(n,sto,h)
%reserves%$ontext
        + sum( reserves_do , RP_STO_IN(n,reserves_do,sto,h))
$ontext
$offtext
        ) * (1+eta_sto(n,sto))/2
        =L= N_STO_E(n,sto) - STO_L(n,sto,h-1)
;

con4j_ending(sto,h,n)$(ord(h) = card(h) AND m_sto_e(n,sto))..
         STO_L(n,sto,h) =E= phi_sto_ini(n,sto) * N_STO_E(n,sto)
;

con4k_PHS_EtoP(sto,n)$m_sto_e(n,sto)..
        N_STO_E(n,sto) =L= etop_max(n,sto) * N_STO_P(n,sto)
;

* ---------------------------------------------------------------------------- *
***** Quotas for renewables and biomass *****
* ---------------------------------------------------------------------------- *

con5a_minRES(n)..
sum( h , G_L(n,'bio',h) + sum( nondis , G_RES(n,nondis,h)) + sum( rsvr , RSVR_OUT(n,rsvr,h))
%reserves%$ontext
         - sum( reserves_do , (sum( nondis , RP_NONDIS(n,reserves_do,nondis,h)) + sum( rsvr , RP_RSVR(n,reserves_do,rsvr,h))) * phi_reserves_call(n,reserves_do,h))
         + sum( reserves_up , (sum( nondis , RP_NONDIS(n,reserves_up,nondis,h)) + sum( rsvr , RP_RSVR(n,reserves_up,rsvr,h))) * phi_reserves_call(n,reserves_up,h))
$ontext
$offtext
%prosumage%$ontext
* ### folgende Formulierung sinnvoll?
         + sum( sto , STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h)) + sum( res , G_MARKET_PRO2M(n,res,h) + G_RES_PRO(n,res,h))
*### prüfen, ob überhaupt nötig - und wenn, dann auch unten?
*         + sum( (bu,ch) , theta_sets(n,bu,ch) * (H_SETS_OUT(n,bu,ch,h) + H_DHW_AUX_OUT(n,bu,ch,h)) )
$ontext
$offtext
)
        =G= phi_min_res * phi_min_res_exog(n) * sum( h ,
         sum( dis , G_L(n,dis,h)) + sum( nondis , G_RES(n,nondis,h)) + sum( rsvr , RSVR_OUT(n,rsvr,h))
%reserves%$ontext
         - sum( reserves_do , (sum( nondis , RP_NONDIS(n,reserves_do,nondis,h)) + sum( rsvr , RP_RSVR(n,reserves_do,rsvr,h))) * phi_reserves_call(n,reserves_do,h))
         + sum( reserves_up , (sum( nondis , RP_NONDIS(n,reserves_up,nondis,h)) + sum( rsvr , RP_RSVR(n,reserves_up,rsvr,h))) * phi_reserves_call(n,reserves_up,h))
$ontext
$offtext
%prosumage%$ontext
         + sum( res , phi_res(n,res,h) * N_RES_PRO(n,res) - CU_PRO(n,res,h))
$ontext
$offtext
         )
;

*sum( h , d(n,h) + sum( (sto) , STO_IN(n,sto,h) - STO_OUT(n,sto,h) )
*%prosumage%$ontext
*         + sum( sto , sum( res , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h)) + STO_IN_M2PRO(n,sto,h) + STO_IN_M2M(n,sto,h) - STO_OUT_PRO2PRO(n,sto,h) - STO_OUT_PRO2M(n,sto,h) - STO_OUT_M2PRO(n,sto,h) - STO_OUT_M2M(n,sto,h) )
*$ontext
*$offtext
*%DSM%$ontext
*         - sum( dsm_curt , DSM_CU(n,dsm_curt,h) )
*         + sum( dsm_shift , DSM_UP(n,dsm_shift,h) - sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(n,dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(n,dsm_shift) ) , DSM_DO(n,dsm_shift,h,hh)) )
*$ontext
*$offtext
*%EV%$ontext
*        + sum( ev , EV_CHARGE(n,ev,h) - EV_DISCHARGE(n,ev,h) )
*$ontext
*$offtext
*%heat%$ontext
*        + sum( (bu,ch) , theta_dir(n,bu,ch) * H_DIR(n,bu,ch,h) )
*        + sum( (bu,ch) , theta_sets(n,bu,ch) * H_SETS_IN(n,bu,ch,h) )
*        + sum( (bu,hp) , theta_hp(n,bu,hp) * H_HP_IN(n,bu,hp,h) )
*        + sum( (bu,hst) , theta_hye(n,bu,hst) * H_ELECTRIC_IN(n,bu,hst,h) )
*$ontext
*$offtext
*%reserves%$ontext
*        + sum( reserves_prim_up , phi_mean_reserves_call(n,reserves_prim_up) * phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum( nondis , reserves_slope(n,reserves_nonprim,nondis) * N_TECH(n,nondis)/1000))) )
*        - sum( reserves_prim_do , phi_mean_reserves_call(n,reserves_prim_do) * phi_reserves_pr_do(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum( nondis , reserves_slope(n,reserves_nonprim,nondis) * N_TECH(n,nondis)/1000))) )
*        + sum( reserves_nonprim_up ,  + phi_mean_reserves_call(n,reserves_nonprim_up) *( 1000 * phi_reserves_share(n,reserves_nonprim_up) * (reserves_intercept(n,reserves_nonprim_up) + sum( nondis , reserves_slope(n,reserves_nonprim_up,nondis) * N_TECH(n,nondis)/1000))) )
*        - sum( reserves_nonprim_do ,  + phi_mean_reserves_call(n,reserves_nonprim_do) *( 1000 * phi_reserves_share(n,reserves_nonprim_do) * (reserves_intercept(n,reserves_nonprim_do) + sum( nondis , reserves_slope(n,reserves_nonprim_do,nondis) * N_TECH(n,nondis)/1000))) )
*       + sum( sto ,
*           sum( reserves_do , phi_reserves_call(n,reserves_do,h) * (RP_STO_IN(n,reserves_do,sto,h) + RP_STO_OUT(n,reserves_do,sto,h)))
*         - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * (RP_STO_IN(n,reserves_up,sto,h) + RP_STO_OUT(n,reserves_up,sto,h))) )
*$ontext
*$offtext
*%DSM%$ontext
*%reserves%$ontext
*       - sum( (dsm_curt,reserves_up) , RP_DSM_CU(n,reserves_up,dsm_curt,h) * phi_reserves_call(n,reserves_up,h) )
*$ontext
*$offtext
*%reserves%$ontext
*%EV%$ontext
*%EV_EXOG%  + sum( ev ,
*%EV_EXOG%      sum( reserves_do , phi_reserves_call(n,reserves_do,h) * (RP_EV_G2V(n,reserves_do,ev,h) + RP_EV_V2G(n,reserves_do,ev,h)))
*%EV_EXOG%    - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * (RP_EV_G2V(n,reserves_up,ev,h) + RP_EV_V2G(n,reserves_up,ev,h))) )
*$ontext
*$offtext
*)
*%EV%$ontext
*%EV_DEFAULT%%EV_FREE%  + (1 - phi_min_res) * sum( (h,ev) , EV_GED(n,ev,h) )
*%EV_DEFAULT%%EV_100RES%  - phi_min_res * sum( (h,ev) , EV_GED(n,ev,h) )
*$ontext
*$offtext
*;

con5b_max_energy(n,dis)$m_e(n,dis)..
         sum( h , G_L(n,dis,h) ) =L= m_e(n,dis)
;

* ---------------------------------------------------------------------------- *
***** DSM constraints - curtailment *****
* ---------------------------------------------------------------------------- *

con6a_DSMcurt_duration_max(n,dsm_curt,h,n)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + t_off_dsm_cu(n,dsm_curt) ) , DSM_CU(n,dsm_curt,hh)
%reserves%$ontext
        + sum( reserves_up , RP_DSM_CU(n,reserves_up,dsm_curt,hh) * phi_reserves_call(n,reserves_up,hh) )
$ontext
$offtext
         )
         =L= N_DSM_CU(n,dsm_curt) * t_dur_dsm_cu(n,dsm_curt)
;

con6b_DSMcurt_max(dsm_curt,h,n)..
        DSM_CU(n,dsm_curt,h)
%reserves%$ontext
        + sum( reserves_up , RP_DSM_CU(n,reserves_up,dsm_curt,h) )
$ontext
$offtext
          =L= N_DSM_CU(n,dsm_curt)
;

* ---------------------------------------------------------------------------- *
***** DSM constraints - shifting *****
* ---------------------------------------------------------------------------- *

con7a_DSMshift_upanddown(dsm_shift,h,n)..
         DSM_UP(n,dsm_shift,h) * (1 + eta_dsm_shift(n,dsm_shift))/2 =E= 2/(1+eta_dsm_shift(n,dsm_shift)) * sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(n,dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(n,dsm_shift) ) , DSM_DO(n,dsm_shift,h,hh))
;

con7b_DSMshift_granular_max(dsm_shift,h,n)..
         DSM_UP_DEMAND(n,dsm_shift,h) + DSM_DO_DEMAND(n,dsm_shift,h)
%reserves%$ontext
         + sum( reserves , RP_DSM_SHIFT(n,reserves,dsm_shift,h) )
$ontext
$offtext
         =L= N_DSM_SHIFT(n,dsm_shift)
;

con7c_DSM_distrib_up(dsm_shift,h,n)..
         DSM_UP(n,dsm_shift,h) =E= DSM_UP_DEMAND(n,dsm_shift,h)
%reserves%$ontext
         + sum( reserves_do , RP_DSM_SHIFT(n,reserves_do,dsm_shift,h) * phi_reserves_call(n,reserves_do,h))
$ontext
$offtext
;

con7d_DSM_distrib_do(dsm_shift,h,n)..
         sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(n,dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(n,dsm_shift) ) , DSM_DO(n,dsm_shift,hh,h) )
                 =E=
         DSM_DO_DEMAND(n,dsm_shift,h)
%reserves%$ontext
         + sum( reserves_up , RP_DSM_SHIFT(n,reserves_up,dsm_shift,h) * phi_reserves_call(n,reserves_up,h))
$ontext
$offtext
;

con7e_DSMshift_recovery(dsm_shift,h,n)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + t_off_dsm_shift(n,dsm_shift) ) , DSM_UP(n,dsm_shift,hh))
         =L= N_DSM_SHIFT(n,dsm_shift) * t_dur_dsm_shift(n,dsm_shift)
;

* ---------------------------------------------------------------------------- *
***** Maximum installation constraints *****
* ---------------------------------------------------------------------------- *

con8a_max_I_power(n,tech)..
         N_TECH(n,tech) =L= m_p(n,tech)
;

con8b_max_I_sto_e(sto,n)..
         N_STO_E(n,sto) =L= m_sto_e(n,sto)
;

con8c_max_I_sto_p(sto,n)..
         N_STO_P(n,sto) =L= m_sto_p(n,sto)
;

con8d_max_I_dsm_cu(dsm_curt,n)..
         N_DSM_CU(n,dsm_curt) =L= m_dsm_cu(n,dsm_curt)
;

con8e_max_I_dsm_shift_pos(dsm_shift,n)..
         N_DSM_SHIFT(n,dsm_shift) =L= m_dsm_shift(n,dsm_shift)
;

con8f_max_pro_res(res,n)..
         N_RES_PRO(n,res) =L= m_res_pro(n,res)
;

con8g_max_pro_sto_e(sto,n)..
         N_STO_E_PRO(n,sto) =L= m_sto_pro_e(n,sto)
;

con8h_max_sto_pro_p(sto,n)..
         N_STO_P_PRO(n,sto) =L= m_sto_pro_p(n,sto)
;

con8i_max_I_ntc(l)..
         NTC(l) =L= m_ntc(l)
;

con8j_max_I_rsvr_e(rsvr,n)..
         N_RSVR_E(n,rsvr) =L= m_rsvr_e(n,rsvr)
;

con8k_max_I_rsvr_p(rsvr,n)..
         N_RSVR_P(n,rsvr) =L= m_rsvr_p(n,rsvr)
;

* ---------------------------------------------------------------------------- *
***** Reserve constraints *****
* ---------------------------------------------------------------------------- *

con9a_reserve_prov_endogenous(reserves_nonprim,h,n)..
        sum( dis , RP_DIS(n,reserves_nonprim,dis,h))
        + sum(nondis, RP_NONDIS(n,reserves_nonprim,nondis,h))
        + sum(rsvr, RP_RSVR(n,reserves_nonprim,rsvr,h))
        + sum(sto, RP_STO_IN(n,reserves_nonprim,sto,h) + RP_STO_OUT(n,reserves_nonprim,sto,h))
%DSM%$ontext
        + sum(dsm_curt, RP_DSM_CU(n,reserves_nonprim,dsm_curt,h))
        + sum(dsm_shift , RP_DSM_SHIFT(n,reserves_nonprim,dsm_shift,h) )
$ontext
$offtext
%EV%$ontext
%EV_EXOG%   + sum(ev, RP_EV_G2V(n,reserves_nonprim,ev,h) + RP_EV_V2G(n,reserves_nonprim,ev,h) )
$ontext
$offtext
%heat%$ontext
        + sum( (bu,ch) , theta_sets(n,bu,ch) * ( RP_SETS(n,reserves_nonprim,bu,ch,h) + RP_SETS_AUX(n,reserves_nonprim,bu,ch,h)) )
        + sum( (bu,ch) , theta_hp(n,bu,ch) * RP_HP(n,reserves_nonprim,bu,ch,h) )
        + sum( (bu,ch) , theta_elec(n,bu,ch) * RP_H_ELEC(n,reserves_nonprim,bu,ch,h) )
$ontext
$offtext
        =E= (
            feat_node('reserves',n) *
            1000 * phi_reserves_share(n,reserves_nonprim) * (
            reserves_intercept(n,reserves_nonprim) + sum( res , reserves_slope(n,reserves_nonprim,res) * (N_TECH(n,res)
%prosumage%$ontext
            + N_RES_PRO(n,res)
$ontext
$offtext
            )/1000 ) ) )$(ord(h) > 1)
;

con9a_reserve_prov_exogenous(reserves_nonprim,h,n)..
        sum( dis , RP_DIS(n,reserves_nonprim,dis,h))
        + sum(nondis, RP_NONDIS(n,reserves_nonprim,nondis,h))
        + sum(rsvr, RP_RSVR(n,reserves_nonprim,rsvr,h))
        + sum(sto, RP_STO_IN(n,reserves_nonprim,sto,h) + RP_STO_OUT(n,reserves_nonprim,sto,h))
%DSM%$ontext
        + sum(dsm_curt, RP_DSM_CU(n,reserves_nonprim,dsm_curt,h))
        + sum(dsm_shift , RP_DSM_SHIFT(n,reserves_nonprim,dsm_shift,h) )
$ontext
$offtext
%EV%$ontext
%EV_EXOG%   + sum(ev, RP_EV_G2V(n,reserves_nonprim,ev,h) + RP_EV_V2G(n,reserves_nonprim,ev,h) )
$ontext
$offtext
%heat%$ontext
        + sum( (bu,ch) , theta_sets(n,bu,ch) * ( RP_SETS(n,reserves_nonprim,bu,ch,h) + RP_SETS_AUX(n,reserves_nonprim,bu,ch,h)) )
        + sum( (bu,ch) , theta_hp(n,bu,ch) * RP_HP(n,reserves_nonprim,bu,ch,h) )
        + sum( (bu,ch) , theta_elec(n,bu,ch) * RP_H_ELEC(n,reserves_nonprim,bu,ch,h) )
$ontext
$offtext
        =E= feat_node('reserves',n) * reserves_exogenous(n,reserves_nonprim,h)$(ord(h) > 1)
;

con9b_reserve_prov_PR_endogenous(reserves_prim,h,n)..
        sum(dis, RP_DIS(n,reserves_prim,dis,h))
        + sum(nondis, RP_NONDIS(n,reserves_prim,nondis,h))
        + sum(rsvr, RP_RSVR(n,reserves_prim,rsvr,h))
        + sum(sto, RP_STO_IN(n,reserves_prim,sto,h) + RP_STO_OUT(n,reserves_prim,sto,h) )
%EV%$ontext
%EV_EXOG%   + sum(ev, RP_EV_G2V(n,reserves_prim,ev,h) + RP_EV_V2G(n,reserves_prim,ev,h) )
$ontext
$offtext
         =E=
             feat_node('reserves',n) *
             phi_reserves_pr_up(n)* sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * (
             reserves_intercept(n,reserves_nonprim) + sum( res , reserves_slope(n,reserves_nonprim,res) * (N_TECH(n,res)
%prosumage%$ontext
             + N_RES_PRO(n,res)
$ontext
$offtext
             )/1000 ) ) )$(ord(h) > 1)
;

con9b_reserve_prov_PR_exogenous(reserves_prim,h,n)..
        sum(dis, RP_DIS(n,reserves_prim,dis,h))
        + sum(nondis, RP_NONDIS(n,reserves_prim,nondis,h))
        + sum(rsvr, RP_RSVR(n,reserves_prim,rsvr,h))
        + sum(sto, RP_STO_IN(n,reserves_prim,sto,h) + RP_STO_OUT(n,reserves_prim,sto,h) )
%EV%$ontext
%EV_EXOG%   + sum(ev, RP_EV_G2V(n,reserves_prim,ev,h) + RP_EV_V2G(n,reserves_prim,ev,h) )
$ontext
$offtext
         =E= feat_node('reserves',n) * reserves_exogenous(n,reserves_prim,h)$(ord(h) > 1)
;

* ---------------------------------------------------------------------------- *
***** Electric vehicle constraints *****
* ---------------------------------------------------------------------------- *

con10a_ev_ed(ev,h,n)..
         feat_node('ev',n) *
         ev_ed(n,ev,h) * phi_ev(n,ev) * ev_quant
*         =e= EV_GED(n,ev,h) + EV_PHEVFUEL(n,ev,h)$(ev_phev(n,ev)=1)
         =e= ( EV_MPED(n,ev,h) + EV_PHEVFUEL(n,ev,h)$(ev_phev(n,ev)=1) )
;

con10b_ev_chargelev_start(ev,h,n)$(ord(h) = 1 AND feat_node('ev',n))..
         EV_L(n,ev,h) =E= feat_node('ev',n) * phi_ev_ini(n,ev) * n_ev_e(n,ev) * phi_ev(n,ev) * ev_quant
         + ( EV_CHARGE_M(n,ev,h) + sum(res, EV_CHARGE_PRO(n,res,ev,h)) ) * eta_ev_in(n,ev)
         - EV_DISCHARGE(n,ev,h) / eta_ev_out(n,ev)
         - EV_MPED(n,ev,h)
;

con10c_ev_chargelev(ev,h,n)$(ord(h) > 1 AND feat_node('ev',n))..
         EV_L(n,ev,h) =E= EV_L(n,ev,h-1)
         + ( EV_CHARGE_M(n,ev,h) + sum(res, EV_CHARGE_PRO(n,res,ev,h)) ) * eta_ev_in(n,ev)
         - EV_DISCHARGE(n,ev,h) / eta_ev_out(n,ev)
%reserves%$ontext
%EV_EXOG%   + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * (RP_EV_G2V(n,reserves_do,ev,h)*eta_ev_in(n,ev) + RP_EV_V2G(n,reserves_do,ev,h)/eta_ev_out(n,ev)) )
%EV_EXOG%   - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * (RP_EV_G2V(n,reserves_up,ev,h)*eta_ev_in(n,ev) + RP_EV_V2G(n,reserves_up,ev,h)/eta_ev_out(n,ev)) )
$ontext
$offtext
         - EV_MPED(n,ev,h)
;

con10d_ev_chargelev_max(ev,h,n)$feat_node('ev',n)..
         EV_L(n,ev,h)
         =L= n_ev_e(n,ev) * phi_ev(n,ev) * ev_quant
             * feat_node('ev',n)
;

con10e_ev_maxin(ev,h,n)$feat_node('ev',n)..
        EV_CHARGE_M(n,ev,h) + sum(res, EV_CHARGE_PRO(n,res,ev,h) )
%reserves%$ontext
        + sum( reserves_do , RP_EV_G2V(n,reserves_do,ev,h))
$ontext
$offtext
        =L= n_ev_p(n,ev,h) * phi_ev(n,ev) * ev_quant
            * feat_node('ev',n)
;

con10f_ev_maxout(ev,h,n)$feat_node('ev',n)..
        EV_DISCHARGE(n,ev,h)
%reserves%$ontext
        + sum( reserves_up , RP_EV_V2G(n,reserves_up,ev,h))
$ontext
$offtext
        =L= n_ev_p(n,ev,h) * phi_ev(n,ev) * ev_quant
            * feat_node('ev',n)
;

con10g_ev_chargelev_ending(ev,h,n)$(ord(h) = card(h) AND feat_node('ev',n))..
         EV_L(n,ev,h) =E= phi_ev_ini(n,ev) * n_ev_e(n,ev) * phi_ev(n,ev) * ev_quant * feat_node('ev',n)
;

con10h_ev_minin(ev,h,n)$feat_node('ev',n)..
         0 =L= EV_CHARGE_M(n,ev,h) + sum(res, EV_CHARGE_PRO(n,res,ev,h) )
        - sum( reserves_up , RP_EV_G2V(n,reserves_up,ev,h))
;

con10i_ev_maxin_lev(ev,h,n)..
        ( EV_CHARGE_M(n,ev,h) + sum(res, EV_CHARGE_PRO(n,res,ev,h) )
        + sum( reserves_do , RP_EV_G2V(n,reserves_do,ev,h))
        ) * eta_ev_in(n,ev)
        =L= n_ev_e(n,ev) * phi_ev(n,ev) * ev_quant - EV_L(n,ev,h-1)
;

con10j_ev_minout(ev,h,n)$feat_node('ev',n)..
         0 =L= EV_DISCHARGE(n,ev,h)
        - sum( reserves_do , RP_EV_V2G(n,reserves_do,ev,h))
;

con10k_ev_maxout_lev(ev,h,n)$feat_node('ev',n)..
        ( EV_DISCHARGE(n,ev,h)
        + sum( reserves_up , RP_EV_V2G(n,reserves_up,ev,h))
) / eta_ev_out(n,ev)
        =L= EV_L(n,ev,h-1)
;

con10l_ev_exog(ev,h,n)$feat_node('ev',n)..
         EV_CHARGE_M(n,ev,h) + sum(res, EV_CHARGE_PRO(n,res,ev,h) )
         =E=
         ev_ged_exog(n,ev,h) * phi_ev(n,ev) * ev_quant
         * feat_node('ev',n)
;

* ---------------------------------------------------------------------------- *
***** Prosumage constraints *****
* ---------------------------------------------------------------------------- *

con11a_pro_distrib(res,h,n)..
         phi_res(n,res,h) * N_RES_PRO(n,res)
         =E=
         CU_PRO(n,res,h) + G_MARKET_PRO2M(n,res,h) + G_RES_PRO(n,res,h) + sum( sto , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h) )
*## new:
*        + sum( (bu,ch) , theta_dir(n,bu,ch) * (H_DIR(n,bu,ch,hh) + H_DHW_DIR(n,bu,ch,hh)) )
%heat%$ontext
         + sum( (bu,ch) , theta_sets(n,bu,ch) * H_SETS_IN(n,res,bu,ch,h) )
$ontext
$offtext
%EV%$ontext
         + sum(ev, EV_CHARGE_PRO(n,res,ev,h))
$ontext
$offtext
*        + sum( (bu,ch) , theta_sets(n,bu,ch) * (H_SETS_IN(n,bu,ch,h) + H_DHW_AUX_ELEC_IN(n,bu,ch,h)) )
*        + sum( (bu,hp) , theta_hp(n,bu,hp) * H_HP_IN(n,bu,hp,hh) )
*        + sum( (bu,hel) , theta_elec(n,bu,hel) * H_ELECTRIC_IN(n,bu,hel,hh) )
;

con11b_pro_balance(h,n)..
         phi_pro_load(n) * d(n,h)
         =E=
         sum( res , G_RES_PRO(n,res,h)) + sum( sto , STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_M2PRO(n,sto,h) ) + G_MARKET_M2PRO(n,h)
;

con11c_pro_selfcon(n)..
         sum( (res,h) , G_RES_PRO(n,res,h) ) + sum( (res,sto,h) , STO_IN_PRO2PRO(n,res,sto,h) )
*### new:
%heat%$ontext
         + sum( (res,bu,ch,h) , theta_sets(n,bu,ch) * H_SETS_IN(n,res,bu,ch,h) )
$ontext
$offtext
%EV%$ontext
         + sum( (res,ev,h), EV_CHARGE_PRO(n,res,ev,h))
$ontext
$offtext
*         + sum( (bu,ch,h) , theta_sets(n,bu,ch) * (H_SETS_IN(n,bu,ch,h) + H_DHW_AUX_ELEC_IN(n,bu,ch,h)) )
         =G=
*         phi_pro_self * sum( h , phi_pro_load(n) * d(n,h))
*### new: no longer self-generation requirement, but self-consumption
* ###
         phi_pro_self * sum( (res,h) , phi_res(n,res,h) * N_RES_PRO(n,res))
;

con11d_pro_stolev_PRO2PRO(sto,h,n)$(ord(h) > 1 )..
         STO_L_PRO2PRO(n,sto,h) =E= STO_L_PRO2PRO(n,sto,h-1) + sum( res , STO_IN_PRO2PRO(n,res,sto,h))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11e_pro_stolev_PRO2M(sto,h,n)$(ord(h) > 1)..
         STO_L_PRO2M(n,sto,h) =E= STO_L_PRO2M(n,sto,h-1) + sum( res , STO_IN_PRO2M(n,res,sto,h))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11f_pro_stolev_M2PRO(sto,h,n)$(ord(h) > 1)..
         STO_L_M2PRO(n,sto,h) =E= STO_L_M2PRO(n,sto,h-1) + STO_IN_M2PRO(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT_M2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11g_pro_stolev_M2M(sto,h,n)$(ord(h) > 1)..
         STO_L_M2M(n,sto,h) =E= STO_L_M2M(n,sto,h-1) + STO_IN_M2M(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT_M2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_1_pro_stolev_start_PRO2PRO(sto,h,n)$(ord(h) = 1)..
        STO_L_PRO2PRO(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + sum( res , STO_IN_PRO2PRO(n,res,sto,'h1'))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_2_pro_stolev_start_PRO2M(sto,h,n)$(ord(h) = 1)..
        STO_L_PRO2M(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + sum( res , STO_IN_PRO2M(n,res,sto,'h1'))*(1+eta_sto(n,sto))/2 - STO_OUT_PRO2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_3_pro_stolev_start_M2PRO(sto,h,n)$(ord(h) = 1)..
        STO_L_M2PRO(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + STO_IN_M2PRO(n,sto,h)*(1+eta_sto(n,sto))/2 - STO_OUT_M2PRO(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11h_4_pro_stolev_start_M2M(sto,h,n)$(ord(h) = 1)..
        STO_L_M2M(n,sto,h) =E= 0.25 * phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto) + STO_IN_M2M(n,sto,'h1')*(1+eta_sto(n,sto))/2 - STO_OUT_M2M(n,sto,h)/(1+eta_sto(n,sto))*2
;

con11i_pro_stolev(sto,h,n)$(ord(h)>1)..
         STO_L_PRO(n,sto,h) =E=   STO_L_PRO2PRO(n,sto,h) +  STO_L_PRO2M(n,sto,h) + STO_L_M2PRO(n,sto,h) + STO_L_M2M(n,sto,h)
;

con11j_pro_stolev_max(sto,h,n)..
        STO_L_PRO(n,sto,h) =L= N_STO_E_PRO(n,sto)
;

con11k_pro_maxin_sto(sto,h,n)..
        sum( res , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h) ) + STO_IN_M2PRO(n,sto,h) + STO_IN_M2M(n,sto,h)
        =L= N_STO_P_PRO(n,sto)
;

con11l_pro_maxout_sto(sto,h,n)..
        STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2PRO(n,sto,h) + STO_OUT_M2M(n,sto,h)
        =L= N_STO_P_PRO(n,sto)
;

con11m_pro_maxout_lev(sto,h,n)..
        ( STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_M2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) ) / (1+eta_sto(n,sto))*2
        =L= STO_L_PRO(n,sto,h-1)
;

con11n_pro_maxin_lev(sto,h,n)..
        ( sum( res , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h) ) + STO_IN_M2PRO(n,sto,h) + STO_IN_M2M(n,sto,h) ) * (1+eta_sto(n,sto))/2
        =L= N_STO_E_PRO(n,sto) - STO_L_PRO(n,sto,h-1)
;

con11o_pro_ending(sto,h,n)$(ord(h) = card(h))..
         STO_L_PRO(n,sto,h) =E= phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto)
*         STO_L_PRO(n,sto,h) =G= phi_sto_pro_ini(n,sto) * N_STO_E_PRO(n,sto)
;

* ---------------------------------------------------------------------------- *
***** NTC constraints *****
* ---------------------------------------------------------------------------- *

***** Constraint on energy flow between nodes ******
con12a_max_f(l,h)..
         F(l,h) =L= NTC(l)
;

con12b_min_f(l,h)..
         F(l,h) =G= -NTC(l)
;

* ---------------------------------------------------------------------------- *
***** Reservoir constraints *****
* ---------------------------------------------------------------------------- *

con13a_rsvrlev_start(rsvr,h,n)$(ord(h) = 1)..
        RSVR_L(n,rsvr,h) =E= phi_rsvr_ini(n,rsvr) * N_RSVR_E(n,rsvr) + rsvr_in(n,rsvr,h)/1000 * N_RSVR_E(n,rsvr) - RSVR_OUT(n,rsvr,h)
;

con13b_rsvrlev(rsvr,h,n)$(ord(h) > 1 AND m_rsvr_e(n,rsvr))..
         RSVR_L(n,rsvr,h) =E= RSVR_L(n,rsvr,h-1) + rsvr_in(n,rsvr,h)/1000 * N_RSVR_E(n,rsvr) - RSVR_OUT(n,rsvr,h)
%reserves%$ontext
                - sum( reserves_up , RP_RSVR(n,reserves_up,rsvr,h) * phi_reserves_call(n,reserves_up,h) )
                + sum( reserves_do , RP_RSVR(n,reserves_do,rsvr,h) * phi_reserves_call(n,reserves_do,h) )
$ontext
$offtext
;

con13c_rsvrlev_max(rsvr,h,n)..
        RSVR_L(n,rsvr,h) =L= N_RSVR_E(n,rsvr)
;

con13d_maxout_rsvr(rsvr,h,n)..
        RSVR_OUT(n,rsvr,h)
%reserves%$ontext
        + sum( reserves_up , RP_RSVR(n,reserves_up,rsvr,h))
$ontext
$offtext
        =L= N_RSVR_P(n,rsvr)
;

con13e_resrv_rsvr(rsvr,h,n)..
        sum( reserves_do , RP_RSVR(n,reserves_do,rsvr,h))
        =L= RSVR_OUT(n,rsvr,h)
;

con13f_maxout_lev(rsvr,h,n)..
        RSVR_OUT(n,rsvr,h)
%reserves%$ontext
        + sum( reserves_up , RP_RSVR(n,reserves_up,rsvr,h))
$ontext
$offtext
        =L= RSVR_L(n,rsvr,h-1)
;

con13g_ending(rsvr,h,n)$(ord(h) = card(h))..
         RSVR_L(n,rsvr,h) =E= phi_rsvr_ini(n,rsvr) * N_RSVR_E(n,rsvr)
;

con13h_smooth(rsvr,n,h)$(m_rsvr_e(n,rsvr) AND feat_node('rsvr_outflow',n))..
         RSVR_OUT(n,rsvr,h) =G= phi_rsvr_min(n) * sum( hh , rsvr_in(n,rsvr,hh)/1000/card(hh)) * N_RSVR_E(n,rsvr)
;

con13i_min_level(n,rsvr,h)$m_rsvr_e(n,rsvr)..
         RSVR_L(n,rsvr,h) =G= phi_rsvr_lev_min(n,rsvr) * N_RSVR_E(n,rsvr)
;

con13j_min_FLH(n,rsvr)$m_rsvr_e(n,rsvr)..
         sum( h , RSVR_OUT(n,rsvr,h) ) =G= min_flh(n,rsvr) * N_RSVR_P(n,rsvr)
;


* ---------------------------------------------------------------------------- *
***** Heating constraints *****
* ---------------------------------------------------------------------------- *

* Energy balances
*con14a_heat_balance(n,bu,ch,h)$feat_node('heat',n)..
con14a_heat_balance(n,bu,ch,h)$(feat_node('heat',n) and ord(bu)=7)..
*         theta_dir(n,bu,ch) * H_DIR(n,bu,ch,h) + theta_sets(n,bu,ch) * H_SETS_OUT(n,bu,ch,h)+ theta_storage(n,bu,ch) * H_STO_OUT(n,bu,ch,h)
*         + theta_sets(n,bu,ch) * (1-eta_heat_stat(n,bu,ch)) * H_SETS_LEV(n,bu,ch,h-1)$(theta_sets(n,bu,ch) AND ord(h) > 1)
         theta_sets(n,bu,ch) * H_SETS_OUT(n,bu,ch,h)
         + theta_sets(n,bu,ch) * (1-eta_heat_stat(n,bu,ch)) * H_SETS_LEV(n,bu,ch,h-1)$(theta_sets(n,bu,ch) AND ord(h) > 1)
* ###
         + AUX_HEAT(n,bu,ch,h)
         =E= dh(n,bu,ch,h)
;

*con14b_dhw_balance(n,bu,ch,h)$feat_node('heat',n)..
con14b_dhw_balance(n,bu,ch,h)$(feat_node('heat',n) and ord(bu)=7)..
         theta_storage(n,bu,ch) * H_DHW_STO_OUT(n,bu,ch,h) + theta_dir(n,bu,ch) * H_DHW_DIR(n,bu,ch,h) + theta_sets(n,bu,ch) * H_DHW_AUX_OUT(n,bu,ch,h)
* ###
         + AUX_DHW(n,bu,ch,h)
         =E=
         d_dhw(n,bu,ch,h)
;

* SETS
*con14c_sets_level(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) AND ord(h) > 1)..
con14c_sets_level(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) AND ord(h) > 1 and ord(bu)=7)..
         H_SETS_LEV(n,bu,ch,h) =E= eta_heat_stat(n,bu,ch) * H_SETS_LEV(n,bu,ch,h-1) + eta_heat_dyn(n,bu,ch) * sum(res, H_SETS_IN(n,res,bu,ch,h)) - H_SETS_OUT(n,bu,ch,h)
%reserves%$ontext
         - theta_sets(n,bu,ch) * eta_heat_dyn(n,bu,ch) * (sum( reserves_up , RP_SETS(n,reserves_up,bu,ch,h) * phi_reserves_call(n,reserves_up,h) )
         - sum( reserves_do , RP_SETS(n,reserves_do,bu,ch,h) * phi_reserves_call(n,reserves_do,h) ))
$ontext
$offtext
;

*con14d_sets_level_start(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) AND (ord(h) = 1 OR ord(h) = card(h)))..
con14d_sets_level_start(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) AND (ord(h) = 1 OR ord(h) = card(h)) and ord(bu)=7 )..
*         H_SETS_LEV(n,bu,ch,h) =E= phi_heat_ini(n,bu,ch) * n_sets_e(n,bu,ch)
         H_SETS_LEV(n,bu,ch,h) =E= phi_heat_ini(n,bu,ch) * 16 * N_SETS_P_OUT(n,bu,ch)
;

*con14e_sets_maxin(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch))..
con14e_sets_maxin(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) and ord(bu)=7 )..
         sum(res , H_SETS_IN(n,res,bu,ch,h))
%reserves%$ontext
         + theta_sets(n,bu,ch) * sum( reserves_do , RP_SETS(n,reserves_do,bu,ch,h) )
$ontext
$offtext
*         =L= theta_night(h) * n_sets_p_in(n,bu,ch)
         =L= theta_night(h) * 2 * N_SETS_P_OUT(n,bu,ch)
;

*con14f_sets_maxout(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch))..
con14f_sets_maxout(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) and ord(bu)=7)..
*         H_SETS_OUT(n,bu,ch,h) =L= n_sets_p_out(n,bu,ch)
         H_SETS_OUT(n,bu,ch,h) =L= N_SETS_P_OUT(n,bu,ch)
;

*con14g_sets_minin(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch))..
con14g_sets_minin(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) and ord(bu)=7 )..
        sum( reserves_up , RP_SETS(n,reserves_up,bu,ch,h))
        =L= sum(res, H_SETS_IN(n,res,bu,ch,h))
;

*con14h_sets_maxlev(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch))..
con14h_sets_maxlev(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) and ord(bu)=7 )..
*         H_SETS_LEV(n,bu,ch,h) =L= n_sets_e(n,bu,ch)
         H_SETS_LEV(n,bu,ch,h) =L= 16 * N_SETS_P_OUT(n,bu,ch)
;

* SETS and DHW
con14i_sets_aux_dhw_level(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) AND ord(h) > 1)..
         H_DHW_AUX_LEV(n,bu,ch,h) =E= eta_dhw_aux_stat(n,bu,ch) * H_DHW_AUX_LEV(n,bu,ch,h-1) + H_DHW_AUX_ELEC_IN(n,bu,ch,h) - H_DHW_AUX_OUT(n,bu,ch,h)
%reserves%$ontext
         - theta_sets(n,bu,ch) * (sum( reserves_up , RP_SETS_AUX(n,reserves_up,bu,ch,h) * phi_reserves_call(n,reserves_up,h) )
         - sum( reserves_do , RP_SETS_AUX(n,reserves_do,bu,ch,h) * phi_reserves_call(n,reserves_do,h) ))
$ontext
$offtext
;

con14j_sets_aux_dhw_level_start(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) AND (ord(h) = 1 OR ord(h) = card(h)) )..
*         H_DHW_AUX_LEV(n,bu,ch,h) =E= phi_heat_ini(n,bu,ch) * n_sets_dhw_e(n,bu,ch)
         H_DHW_AUX_LEV(n,bu,ch,h) =E= phi_heat_ini(n,bu,ch) * 2.2 * N_SETS_DHW_P_IN(n,bu,ch)
;

con14k_sets_aux_dhw_maxin(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) )..
         H_DHW_AUX_ELEC_IN(n,bu,ch,h)
%reserves%$ontext
         + theta_sets(n,bu,ch) * sum( reserves_do , RP_SETS_AUX(n,reserves_do,bu,ch,h) )
$ontext
$offtext
*         =L= n_sets_dhw_p_in(n,bu,ch)
         =L= N_SETS_DHW_P_IN(n,bu,ch)
;

con14l_sets_aux_dhw_minin(n,bu,ch,h)$(feat_node('heat',n) AND feat_node('reserves',n) AND theta_sets(n,bu,ch)  )..
        sum( reserves_up , RP_SETS_AUX(n,reserves_up,bu,ch,h))
        =L= H_DHW_AUX_ELEC_IN(n,bu,ch,h)
;

con14m_sets_aux_dhw_maxlev(n,bu,ch,h)$(feat_node('heat',n) AND theta_sets(n,bu,ch) )..
*         H_DHW_AUX_LEV(n,bu,ch,h) =L= n_sets_dhw_e(n,bu,ch)
         H_DHW_AUX_LEV(n,bu,ch,h) =L= 2.2 * N_SETS_DHW_P_IN(n,bu,ch)
;

* HEAT PUMPS
con14n_hp_in(n,bu,hp,h)$(feat_node('heat',n) AND theta_hp(n,bu,hp))..
         H_STO_IN_HP(n,bu,hp,h) =E= (H_HP_IN(n,bu,hp,h)
%reserves%$ontext
         - theta_hp(n,bu,hp) * (sum( reserves_up , RP_HP(n,reserves_up,bu,hp,h) * phi_reserves_call(n,reserves_up,h) )
         - sum( reserves_do , RP_HP(n,reserves_do,bu,hp,h) * phi_reserves_call(n,reserves_do,h) ))
$ontext
$offtext
         ) * eta_heat_dyn(n,bu,hp) * ((temp_sink(n,bu,hp)+273.15)/(temp_sink(n,bu,hp) - temp_source(n,bu,hp,h)))
;

con14o_hp_maxin(n,bu,hp,h)$(feat_node('heat',n) AND theta_hp(n,bu,hp))..
         H_HP_IN(n,bu,hp,h)
%reserves%$ontext
         + sum( reserves_do , RP_HP(n,reserves_do,bu,hp,h) )
$ontext
$offtext
         =L= n_heat_p_in(n,bu,hp)
;

con14p_hp_minin(n,bu,hp,h)$(feat_node('heat',n) AND theta_hp(n,bu,hp))..
        sum( reserves_up , RP_HP(n,reserves_up,bu,hp,h))
        =L= H_HP_IN(n,bu,hp,h)
;

* (Hybrid) ELECTRIC HEATING
con14q_storage_elec_in(n,bu,hel,h)$(feat_node('heat',n) AND theta_storage(n,bu,hel) AND theta_elec(n,bu,hel) )..
         H_STO_IN_ELECTRIC(n,bu,hel,h) =E= H_ELECTRIC_IN(n,bu,hel,h)
%reserves%$ontext
         - theta_elec(n,bu,hel) * (sum( reserves_up , RP_H_ELEC(n,reserves_up,bu,hel,h) * phi_reserves_call(n,reserves_up,h) )
         - sum( reserves_do , RP_H_ELEC(n,reserves_do,bu,hel,h) * phi_reserves_call(n,reserves_do,h) ))
$ontext
$offtext
;

con14r_storage_elec_maxin(n,bu,hel,h)$(feat_node('heat',n) AND theta_storage(n,bu,hel) AND theta_elec(n,bu,hel ))..
         H_ELECTRIC_IN(n,bu,hel,h)
%reserves%$ontext
         + sum( reserves_do , RP_H_ELEC(n,reserves_do,bu,hel,h) )
$ontext
$offtext
         =L= n_heat_p_in(n,bu,hel)
;

con14s_storage_elec_minin(n,bu,hel,h)$(feat_node('heat',n) AND feat_node('reserves',n) AND theta_storage(n,bu,hel) AND theta_elec(n,bu,hel) )..
        sum( reserves_up , RP_H_ELEC(n,reserves_up,bu,hel,h))
        =L= H_ELECTRIC_IN(n,bu,hel,h)
;

* HEAT STORAGE
con14t_storage_level(n,bu,hst,h)$(feat_node('heat',n) AND theta_storage(n,bu,hst) AND ord(h) > 1)..
         H_STO_LEV(n,bu,hst,h)
         =E=
         eta_heat_stat(n,bu,hst) * H_STO_LEV(n,bu,hst,h-1) + theta_hp(n,bu,hst)*H_STO_IN_HP(n,bu,hst,h) + theta_elec(n,bu,hst)*H_STO_IN_ELECTRIC(n,bu,hst,h) + theta_fossil(n,bu,hst) * H_STO_IN_FOSSIL(n,bu,hst,h)
         - H_STO_OUT(n,bu,hst,h) - H_DHW_STO_OUT(n,bu,hst,h)
;

con14u_storage_level_start(n,bu,hst,h)$(feat_node('heat',n) AND theta_storage(n,bu,hst) AND (ord(h) = 1 OR ord(h) = card(h)))..
         H_STO_LEV(n,bu,hst,h) =E= phi_heat_ini(n,bu,hst) * theta_storage(n,bu,hst)*n_heat_e(n,bu,hst)
;

con14v_storage_maxlev(n,bu,hst,h)$(feat_node('heat',n) AND theta_storage(n,bu,hst))..
         H_STO_LEV(n,bu,hst,h) =L= n_heat_e(n,bu,hst)
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
%reserves%$ontext
  con3b_minprod_dispatchable
  con3c_flex_reserves_spin
  con3d_flex_reserves_nonspin
$ontext
$offtext
con3e_maxprod_res
%reserves%$ontext
  con3f_minprod_res
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
con5b_max_energy

%DSM%$ontext
con6a_DSMcurt_duration_max
con6b_DSMcurt_max

con7a_DSMshift_upanddown
con7b_DSMshift_granular_max
con7c_DSM_distrib_up
con7d_DSM_distrib_do
*con_7e_DSMshift_recovery
$ontext
$offtext

con8a_max_I_power
con8b_max_I_sto_e
con8c_max_I_sto_p
%DSM%$ontext
  con8d_max_I_dsm_cu
  con8e_max_I_dsm_shift_pos
$ontext
$offtext
con8i_max_I_ntc
con8j_max_I_rsvr_e
con8k_max_I_rsvr_p

%reserves_endogenous%$ontext
 con9a_reserve_prov_endogenous
 con9b_reserve_prov_PR_endogenous
$ontext
$offtext

%reserves_exogenous%$ontext
 con9a_reserve_prov_exogenous
 con9b_reserve_prov_PR_exogenous
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

con12a_max_f
con12b_min_f

con13a_rsvrlev_start
con13b_rsvrlev
con13c_rsvrlev_max
con13d_maxout_rsvr
con13e_resrv_rsvr
con13f_maxout_lev
con13g_ending
*con13h_smooth
con13i_min_level
*con13j_min_FLH

%heat%$ontext
con14a_heat_balance
* ### NEW:
*con14b_dhw_balance
con14c_sets_level
con14d_sets_level_start
con14e_sets_maxin
con14f_sets_maxout
con14h_sets_maxlev

* ### NEW:
*con14i_sets_aux_dhw_level
*con14j_sets_aux_dhw_level_start
*con14k_sets_aux_dhw_maxin
*con14l_sets_aux_dhw_minin
*con14m_sets_aux_dhw_maxlev

* ### NEW:
*con14n_hp_in
*con14o_hp_maxin
*con14q_storage_elec_in
*con14r_storage_elec_maxin
*con14t_storage_level
*con14u_storage_level_start
*con14v_storage_maxlev
$ontext
$offtext

%heat%$ontext
%reserves%$ontext
con14g_sets_minin
con14p_hp_minin
con14s_storage_elec_minin
$ontext
$offtext
/;
