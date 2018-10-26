
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

     zesz
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

N_TECH(n,tech)           Technology tech built [MW]
N_STO_E(n,sto)           Storage technology built - Energy [MWh]
N_STO_P(n,sto)           Storage loading and discharging capacity built - Capacity [MW]

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

                 + sum( map_n_tech(n,tech) , c_i(n,tech)*N_TECH(n,tech) )
                 + sum( map_n_tech(n,tech) , c_fix(n,tech)*N_TECH(n,tech) )
                 + sum( map_n_sto(n,sto) , c_i_sto_e(n,sto)*N_STO_E(n,sto) )
                 + sum( map_n_sto(n,sto) , c_fix_sto(n,sto)/2*(N_STO_P(n,sto)+N_STO_E(n,sto)) )
                 + sum( map_n_sto(n,sto) , c_i_sto_p(n,sto)*N_STO_P(n,sto) )
%prosumage%$ontext
                 + sum( map_n_res_pro(n,res) , c_i(n,res)*N_RES_PRO(n,res) )
                 + sum( map_n_res_pro(n,res) , c_fix(n,res)*N_RES_PRO(n,res) )

                 + sum( map_n_sto_pro(n,sto) , c_i_sto_e(n,sto)*N_STO_E_PRO(n,sto) )
                 + sum( map_n_sto_pro(n,sto) , c_fix_sto(n,sto)/2*(N_STO_P_PRO(n,sto) + N_STO_E_PRO(n,sto)) )
                 + sum( map_n_sto_pro(n,sto) , c_i_sto_p(n,sto)*N_STO_P_PRO(n,sto) )

                 + sum( (h,map_n_sto_pro(n,sto)) , c_m_sto(n,sto) * ( STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_M2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) + sum( res , STO_IN_PRO2PRO(n,res,sto,h) + STO_IN_PRO2M(n,res,sto,h)) + STO_OUT_PRO2M(n,sto,h) + STO_OUT_M2M(n,sto,h) ) )
$ontext
$offtext
;

* ---------------------------------------------------------------------------- *
***** Energy balance and load levels *****
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(n,hh)..
         ( 1 - phi_pro_load(n) ) * d(n,hh) + sum( map_n_sto(n,sto) , STO_IN(n,sto,hh) )
%prosumage%$ontext
         + G_MARKET_M2PRO(n,hh)
         + sum( map_n_sto_pro(n,sto) , STO_IN_M2PRO(n,sto,hh))
         + sum( map_n_sto_pro(n,sto) , STO_IN_M2M(n,sto,hh))
$ontext
$offtext

         =E=
         sum( map_n_tech(n,dis) , G_L(n,dis,hh)) + sum( map_n_tech(n,nondis) , G_RES(n,nondis,hh)) + sum( sto , STO_OUT(n,sto,hh) )

%prosumage%$ontext
         + sum( map_n_res_pro(n,res) , G_MARKET_PRO2M(n,res,hh) )
         + sum( map_n_sto_pro(n,sto) , STO_OUT_PRO2M(n,sto,hh))
         + sum( map_n_sto_pro(n,sto) , STO_OUT_M2M(n,sto,hh))
$ontext
$offtext

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

        =L= N_TECH(n,dis)
;

con3e_maxprod_res(n,nondis,h)$(map_n_tech(n,nondis))..
        G_RES(n,nondis,h) + CU(n,nondis,h)

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

;

con4c_stolev_max(n,sto,h)$(map_n_sto(n,sto))..
        STO_L(n,sto,h) =L= N_STO_E(n,sto)
;

con4d_maxin_sto(n,sto,h)$(map_n_sto(n,sto))..
        STO_IN(n,sto,h)

        =L= N_STO_P(n,sto)
;

con4e_maxout_sto(n,sto,h)$(map_n_sto(n,sto))..
        STO_OUT(n,sto,h)

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
sum( h , G_L(n,'bio',h) + sum( map_n_tech(n,nondis) , G_RES(n,nondis,h))

%prosumage%$ontext
         + sum( map_n_sto_pro(n,sto) , STO_OUT_PRO2PRO(n,sto,h) + STO_OUT_PRO2M(n,sto,h)) + sum( map_n_res_pro(n,res) , G_MARKET_PRO2M(n,res,h) + G_RES_PRO(n,res,h))
$ontext
$offtext
)
        =G= phi_min_res * phi_min_res_exog(n) * sum( h ,
         sum( map_n_tech(n,dis) , G_L(n,dis,h)) + sum( map_n_tech(n,nondis) , G_RES(n,nondis,h))

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
