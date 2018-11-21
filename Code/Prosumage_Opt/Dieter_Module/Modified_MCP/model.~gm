
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
;

Positive Variables
G_L(tech,h)            Generation level in hour h [MWh]
G_UP(tech,h)           Generation upshift in hour h [MWh]
G_DO(tech,h)           Generation downshift in hour h [MWh]

G_RES(tech,h)          Generation renewables type res in hour h [MWh]
CU(tech,h)             Renewables curtailment technology res in hour h [MWh]

STO_IN(sto,h)          Storage inflow technology sto hour h [MWh]
STO_OUT(sto,h)         Storage outflow technology sto hour h [MWh]
STO_L(sto,h)           Storage level technology sto hour h [MWh]

N_TECH(tech)           Technology tech built [MW]
N_STO_E(sto)           Storage technology built - Energy [MWh]
N_STO_P(sto)           Storage loading and discharging capacity built - Capacity [MW]

CU_PRO(tech,h)                 Prosumage: curtailment of renewable generation in hour h [MWh]
G_MARKET_PRO2M(tech,h)         Prosumage. energy sent to market in hour h [MWh]
G_MARKET_M2PRO(h)              Prosumage: withdrawal of energy from market in hour h [MWh]
G_RES_PRO(tech,h)              Prosumage: hourly renewables generation in hour h [MWh]
STO_IN_PRO2PRO(tech,sto,h)     Prosumage: storage loading from generation for discharging to consumption in hour h [MWh]
STO_OUT_PRO2PRO(sto,h)         Prosumage: storage discharging to consumption from generation in hour h [MWh]
STO_L_PRO2PRO(sto,h)           Prosumage: storage level generation to consumption in hour h [MWh]
N_STO_E_PRO(sto)               Prosumage: installed storage energy [MWh]
N_STO_P_PRO(sto)               Prosumage: installed storage power [MW]
STO_L_PRO(sto,h)               Prosumage: overall storage level in hour h [MWh]
N_RES_PRO(tech)                Prosumage: installed renewables capacities [MW]

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
con11h_1_pro_stolev_start_PRO2PRO        Prosumage: storage level initial conditions
con11j_pro_stolev_max                    Prosumage: maximum overall storage level
con11k_pro_maxin_sto                     Prosumage: maximum storage inflow
con11l_pro_maxout_sto                    Prosumage: maximum storage outflow
con11o_pro_ending                        Prosumage: storage ending condition
;




********************************************************************************

* ---------------------------------------------------------------------------- *
***** Objective function *****
* ---------------------------------------------------------------------------- *

obj..
         Z =E=
                   sum( (h,dis) , c_m(dis)*G_L(dis,h) )
                 + sum( (h,dis)$(ord(h)>1) , c_up(dis)*G_UP(dis,h) )
                 + sum( (h,dis) , c_do(dis)*G_DO(dis,h) )
                 + sum( (h,nondis) , c_cu(nondis)*CU(nondis,h) )
                 + sum( (h,sto) , c_m_sto(sto) * ( STO_OUT(sto,h) + STO_IN(sto,h) ) )
                 + sum( tech , c_i(tech)*N_TECH(tech) )
                 + sum( tech , c_fix(tech)*N_TECH(tech) )
                 + sum( sto , c_i_sto_e(sto)*N_STO_E(sto) )
                 + sum( sto , c_fix_sto(sto)/2*(N_STO_P(sto)+ N_STO_E(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P(sto) )
%prosumage%$ontext
                 + sum( res , c_i(res)*N_RES_PRO(res) )
                 + sum( res , c_fix(res)*N_RES_PRO(res) )

                 + sum( sto , c_i_sto_e(sto)*N_STO_E_PRO(sto) )
                 + sum( sto , c_fix_sto(sto)/2*(N_STO_P_PRO(sto) + N_STO_E_PRO(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P_PRO(sto) )
                 + sum( (h,sto) , c_m_sto(sto) * ( STO_OUT_PRO2PRO(sto,h) + sum( res , STO_IN_PRO2PRO(res,sto,h)) ) )
$ontext
$offtext
;

* ---------------------------------------------------------------------------- *
***** Energy balance and load levels *****
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(hh)..
         ( 1 - phi_pro_load )* d(hh) + sum( sto , STO_IN(sto,hh) )
%prosumage%$ontext
         + G_MARKET_M2PRO(hh)
$ontext
$offtext

         =E=
         sum( dis , G_L(dis,hh)) + sum( nondis , G_RES(nondis,hh)) + sum( sto , STO_OUT(sto,hh) )

%prosumage%$ontext
         + sum( res , G_MARKET_PRO2M(res,hh) )
$ontext
$offtext

;

con2a_loadlevel(dis,h)$(ord(h) > 1)..
        G_L(dis,h) =E= G_L(dis,h-1) + G_UP(dis,h) - G_DO(dis,h)
;

con2b_loadlevelstart(dis,h)$(ord(h) = 1)..
         G_L(dis,h) =E= G_UP(dis,h)
;

* ---------------------------------------------------------------------------- *
***** Hourly maximum generation caps *****
* ---------------------------------------------------------------------------- *

con3a_maxprod_dispatchable(dis,h)..
        G_L(dis,h)

        =L= N_TECH(dis)
;

con3e_maxprod_res(nondis,h)..
        G_RES(nondis,h) + CU(nondis,h)

        =E= phi_res(nondis,h) * N_TECH(nondis)
;

* ---------------------------------------------------------------------------- *
***** Storage constraints *****
* ---------------------------------------------------------------------------- *

con4a_stolev_start(sto,h)$(ord(h) = 1)..
        STO_L(sto,h) =E= phi_sto_ini(sto) * N_STO_E(sto) + STO_IN(sto,h)*(1+eta_sto(sto))/2 - STO_OUT(sto,h)/(1+eta_sto(sto))*2
;

con4b_stolev(sto,h)$(ord(h)>1)..
         STO_L(sto,h) =E=  STO_L(sto,h-1) + STO_IN(sto,h)*(1+eta_sto(sto))/2 - STO_OUT(sto,h)/(1+eta_sto(sto))*2
;

con4c_stolev_max(sto,h)..
        STO_L(sto,h) =L= N_STO_E(sto)
;

con4d_maxin_sto(sto,h)..
        STO_IN(sto,h)

        =L= N_STO_P(sto)
;

con4e_maxout_sto(sto,h)..
        STO_OUT(sto,h)

        =L= N_STO_P(sto)
;

con4j_ending(sto,h)$(ord(h) = card(h))..
         STO_L(sto,h) =E= phi_sto_ini(sto) * N_STO_E(sto)
;

con4k_PHS_EtoP(sto)..
        N_STO_E(sto) =L= etop_max(sto) * N_STO_P(sto)
;

* ---------------------------------------------------------------------------- *
***** Quotas for renewables and biomass *****
* ---------------------------------------------------------------------------- *

con5a_minRES..
sum( h , G_L('bio',h) + sum( nondis , G_RES(nondis,h))

%prosumage%$ontext
         + sum( sto , STO_OUT_PRO2PRO(sto,h)) + sum( res , G_MARKET_PRO2M(res,h) + G_RES_PRO(res,h))
$ontext
$offtext
)
        =G= phi_min_res * phi_min_res_exog * sum( h ,
         sum( dis , G_L(dis,h)) + sum( nondis , G_RES(nondis,h))

%prosumage%$ontext
         + sum( res , phi_res(res,h) * N_RES_PRO(res) - CU_PRO(res,h))
$ontext
$offtext
         )
;

con5b_max_energy(dis)$(m_e(dis))..
         sum( h , G_L(dis,h) ) =L= m_e(dis)
;


* ---------------------------------------------------------------------------- *
***** Maximum installation constraints *****
* ---------------------------------------------------------------------------- *

con8a_max_I_power(tech)..
         N_TECH(tech) =L= m_p(tech)
;

con8b_max_I_sto_e(sto)..
         N_STO_E(sto) =L= m_sto_e(sto)
;

con8c_max_I_sto_p(sto)..
         N_STO_P(sto) =L= m_sto_p(sto)
;



con8f_max_pro_res(res)..
         N_RES_PRO(res) =L= m_res_pro(res)
;

con8g_max_pro_sto_e(sto)..
         N_STO_E_PRO(sto) =L= m_sto_pro_e(sto)
;

con8h_max_sto_pro_p(sto)..
         N_STO_P_PRO(sto) =L= m_sto_pro_p(sto)
;

* ---------------------------------------------------------------------------- *
***** Prosumage constraints *****
* ---------------------------------------------------------------------------- *

con11a_pro_distrib(res,h)..
         phi_res(res,h) * N_RES_PRO(res)
         =E=
         CU_PRO(res,h) + G_MARKET_PRO2M(res,h) + G_RES_PRO(res,h) + sum( sto , STO_IN_PRO2PRO(res,sto,h) )
;

con11b_pro_balance(h)..
         phi_pro_load * d(h)
         =E=
         sum( res , G_RES_PRO(res,h)) + sum( sto , STO_OUT_PRO2PRO(sto,h) ) + G_MARKET_M2PRO(h)
;

con11c_pro_selfcon..
         sum( (h,res) , G_RES_PRO(res,h) ) + sum( (h,sto) , STO_OUT_PRO2PRO(sto,h) )
         =G=
         phi_pro_self * sum( h , phi_pro_load * d(h))
;

con11d_pro_stolev_PRO2PRO(sto,h)$(ord(h) > 1 )..
         STO_L_PRO2PRO(sto,h) =E= STO_L_PRO2PRO(sto,h-1) + sum( res , STO_IN_PRO2PRO(res,sto,h))*(1+eta_sto(sto))/2 - STO_OUT_PRO2PRO(sto,h)/(1+eta_sto(sto))*2
;


con11h_1_pro_stolev_start_PRO2PRO(sto,h)$( ord(h) = 1)..
        STO_L_PRO2PRO(sto,h) =E=   phi_sto_pro_ini(sto) * N_STO_E_PRO(sto) + sum( res , STO_IN_PRO2PRO(res,sto,h))*(1+eta_sto(sto))/2 - STO_OUT_PRO2PRO(sto,h)/(1+eta_sto(sto))*2
;


con11j_pro_stolev_max(sto,h)..
        STO_L_PRO2PRO(sto,h) =L= N_STO_E_PRO(sto)
;

con11k_pro_maxin_sto(sto,h)..
        sum( res , STO_IN_PRO2PRO(res,sto,h) )
        =L= N_STO_P_PRO(sto)
;

con11l_pro_maxout_sto(sto,h)..
        STO_OUT_PRO2PRO(sto,h)
        =L= N_STO_P_PRO(sto)
;

con11o_pro_ending(sto,h)$( ord(h) = card(h))..
         STO_L_PRO2PRO(sto,h) =E= phi_sto_pro_ini(sto) * N_STO_E_PRO(sto)
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
con11h_1_pro_stolev_start_PRO2PRO
con11j_pro_stolev_max
con11k_pro_maxin_sto
con11l_pro_maxout_sto
con11o_pro_ending
$ontext
$offtext

/;
