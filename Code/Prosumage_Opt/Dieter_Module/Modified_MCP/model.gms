
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
Z                  Value objective function [Euro]
lambda_enerbal     Dual variable on energy balance (1a)
lambda_resgen      Dual variable on renewable generation (3e)
lambda_convgen     Dual variable on conventional generation level (2a)
lambda_stolev      Dual variable on storage level  (4a-4b)

lambda_enerbal_pro     Prosumage: Dual variable on prosumage energy balance (11b)
lambda_resgen_pro      Prosumage: Dual variable on renewable generation (11a)
lambda_stolev_pro      Prosumage: Dual variable on storage level  (11d-11h)
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
STO_IN_PRO2PRO(sto,tech,h)     Prosumage: storage loading from generation for discharging to consumption in hour h [MWh]
STO_OUT_PRO2PRO(sto,h)         Prosumage: storage discharging to consumption from generation in hour h [MWh]
STO_L_PRO2PRO(sto,h)           Prosumage: storage level generation to consumption in hour h [MWh]
N_STO_E_PRO(sto)               Prosumage: installed storage energy [MWh]
N_STO_P_PRO(sto)               Prosumage: installed storage power [MW]
N_RES_PRO(tech)                Prosumage: installed renewables capacities [MW]


mu_stoin_cap          Dual variable on storage loading capacity constraint (4d)
mu_stout_cap          Dual variable on storage discharging capacity constraint (4e)
mu_stolev_cap         Dual variable on energy capacity constraint (4c)
mu_conv_cap           Dual variable on conventional generation capacity constraint (3a)
mu_bio_cap            Dual variable on storage bio energy constraint (5b)
mu_dis_max_i          Dual variable on dispatchable installation constraint (8a)
mu_nondis_max_i       Dual variable on nondispatchable installation constraint (8a)
mu_tech_max_i         Dual variable on tech installation constraint (8a)
mu_stop_max_i         Dual variable on storage power installation constraint  (8c)
mu_stoe_max_i         Dual variable on storage energy installation constraint  (8b)
mu_minRES             Dual variable on minimum renewable share constraint (5a)

mu_stoin_cap_pro          Prosumage: Dual variable on storage loading capacity constraint (11k)
mu_stout_cap_pro          Prosumage: Dual variable on storage discharging capacity constraint (11l)
mu_stolev_cap_pro         Prosumage: Dual variable on energy capacity constraint (11j)
mu_tech_max_i_pro         Prosumage: Dual variable on res installation constraint (8f)
mu_stop_max_i_pro         Prosumage: Dual variable on storage power installation constraint  (8g)
mu_stoe_max_i_pro         Prosumage: Dual variable on storage energy installation constraint  (8h)
mu_self_con_pro           Prosumage: Constraint on miminum self-consumption level (8c)
;

Set
dis_bio(tech)          Subset of dispatchable technology: Bio mass        /bio/
res_pro(tech)          Prosumage renewable generation technologies        /pv/
sto_pro(sto)           Storage technologies                               /sto1/
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
con2_loadlevel           Load change costs: Level of all periods (2a+2b)
* Capacity contraints and flexibility constraints
con3a_maxprod_dispatchable       Capacity Constraint conventionals
con3e_maxprod_res                Capacity constraints renewables

* Storage constraints
con4a_stolev_start        Storage Level Dynamics Initial Condition
con4b_stolev              Storage Level Dynamics
con4_stolev               Storage Level Dynamics all periods(4a + 4b)
con4c_stolev_max          Storage Power Capacity
con4d_maxin_sto           Storage maximum inflow
con4e_maxout_sto          Storage maximum outflow
con4j_ending              End level equal to initial level
con4k_PHS_EtoP            Maximum E-P ratio

* Minimum restrictions for renewables and biomass
con5a_minRES             Minimum yearly renewables requirement
con5b_max_energy         Maximum yearly biomass energy

* Maximum installation conditions
con8a_max_I_power                Maximum installable capacity: Conventionals
con8b_max_I_sto_e                Maximum installable energy: Storage energy in MWh
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

* KKT optimality conditions
KKTG_L                   KKT w.r.t. G_L
KKTG_UP                  KKT w.r.t. G_UP
KKTG_DO                  KKT w.r.t. G_DO
KKTG_RES                 KKT w.r.t. G_RES
KKTCU                    KKT w.r.t. CU
KKTSTO_IN                KKT w.r.t. STO_IN
KKTSTO_OUT               KKT w.r.t. STO_OUT
KKTSTO_L                 KKT w.r.t. STO_L
KKTN_TECH_NONDIS         KKT w.r.t. N_TECH(nondis)
KKTN_TECH_DIS            KKT w.r.t. N_TECH(dis)
KKTN_TECH                KKT w.r.t. N_TECH
KKTN_STO_E               KKT w.r.t. N_STO_E
KKTN_STO_P               KKT w.r.t. N_STO_P

KKT_CU_PRO                  Prosumage: FOC w.r.t CU_PRO
KKT_N_RES_PRO               Prosumage: FOC w.r.t N_RES_PRO
KKT_N_STO_E_PRO             Prosumage: FOC w.r.t N_STO_E_PRO
KKT_N_STO_P_PRO             Prosumage: FOC w.r.t N_STO_P_PRO
KKT_G_MARKET_M2PRO          Prosumage: FOC w.r.t G_MARKET_M2PRO
KKT_G_MARKET_PRO2M          Prosumage: FOC w.r.t G_MARKET_PRO2M
KKT_G_RES_PRO               Prosumage: FOC w.r.t G_RES_PRO
KKT_STO_IN_PRO2PRO          Prosumage: FOC w.r.t STO_IN_PRO2PRO
KKT_STO_OUT_PRO2PRO         Prosumage: FOC w.r.t STO_OUT_PRO2PRO
KKT_STO_L_PRO2PRO           Prosumage: FOC w.r.t STO_L_PRO2PRO

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
                 + sum( res_pro , c_i(res_pro)*N_RES_PRO(res_pro) )
                 + sum( res_pro , c_fix(res_pro)*N_RES_PRO(res_pro) )
                 + sum( sto_pro , c_i_sto_e(sto_pro)*N_STO_E_PRO(sto_pro) )
                 + sum( sto_pro , c_fix_sto(sto_pro)/2*(N_STO_P_PRO(sto_pro) + N_STO_E_PRO(sto_pro)) )
                 + sum( sto_pro , c_i_sto_p(sto_pro)*N_STO_P_PRO(sto_pro) )
                 + sum( (h,sto_pro) , c_m_sto(sto_pro) * ( STO_OUT_PRO2PRO(sto_pro,h) + sum( res_pro , STO_IN_PRO2PRO(sto_pro,res_pro,h)) ) )
$ontext
$offtext  
;

* ---------------------------------------------------------------------------- *
***** Energy balance and load levels *****
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(h)..

           sum( dis , G_L(dis,h)) + sum( nondis , G_RES(nondis,h)) + sum( sto , STO_OUT(sto,h) )
%prosumage%$ontext
         + sum( res , G_MARKET_PRO2M(res,h) )
$ontext
$offtext
         -  ( 1 - phi_pro_load )* d(h)
         -   sum( sto , STO_IN(sto,h) )

%prosumage%$ontext
         - G_MARKET_M2PRO(h)
$ontext
$offtext

       =E= 0
;

con2a_loadlevel(dis,h)$(ord(h) > 1)..
         G_L(dis,h-1) + G_UP(dis,h) - G_DO(dis,h)  - G_L(dis,h) =E= 0
;

con2b_loadlevelstart(dis,h)$(ord(h) = 1)..
        G_UP(dis,h) - G_L(dis,h) =E= 0
;

con2_loadlevel(dis,h)..
           G_UP(dis,h)  - G_L(dis,h)
        +  (G_L(dis,h-1) - G_DO(dis,h))$(ord(h) > 1)
        =E= 0
;



* ---------------------------------------------------------------------------- *
***** Hourly maximum generation caps *****
* ---------------------------------------------------------------------------- *

con3a_maxprod_dispatchable(dis,h)..

         N_TECH(dis) -  G_L(dis,h)  =G= 0

;

con3e_maxprod_res(nondis,h)..


       phi_res(nondis,h)*N_TECH(nondis) - G_RES(nondis,h) - CU(nondis,h) =E= 0
;

* ---------------------------------------------------------------------------- *
***** Storage constraints *****
* ---------------------------------------------------------------------------- *

con4a_stolev_start(sto,h)$(ord(h) = 1)..
        phi_sto_ini(sto) * N_STO_E(sto) + STO_IN(sto,h)*(1+eta_sto(sto))/2 - STO_OUT(sto,h)/(1+eta_sto(sto))*2  - STO_L(sto,h)  =E= 0
;

con4b_stolev(sto,h)$(ord(h)>1)..
        STO_L(sto,h-1) + STO_IN(sto,h)*(1+eta_sto(sto))/2 - STO_OUT(sto,h)/(1+eta_sto(sto))*2  -  STO_L(sto,h) =E= 0
;

con4_stolev(sto,h)..

       + STO_IN(sto,h)*(1+eta_sto(sto))/2 - STO_OUT(sto,h)/(1+eta_sto(sto))*2  -  STO_L(sto,h)
       + (STO_L(sto,h-1))$(ord(h)>1)
       =E= 0
;


con4c_stolev_max(sto,h)..
       N_STO_E(sto) -  STO_L(sto,h) =G= 0
;

con4d_maxin_sto(sto,h)..

        N_STO_P(sto) -  STO_IN(sto,h)   =G= 0
;

con4e_maxout_sto(sto,h)..

        N_STO_P(sto) - STO_OUT(sto,h)   =G= 0
;

con4j_ending(sto,h)$(ord(h) = card(h))..

        phi_sto_ini(sto) * N_STO_E(sto) -  STO_L(sto,h) =E= 0
;


con4k_PHS_EtoP(sto)..
         etop_max(sto) * N_STO_P(sto) - N_STO_E(sto) =G= 0
;

* ---------------------------------------------------------------------------- *
***** Quotas for renewables and biomass *****
* ---------------------------------------------------------------------------- *

con5a_minRES..
sum( h , G_L('bio',h) + sum(nondis , G_RES(nondis,h))

%prosumage%$ontext
         + sum( sto , STO_OUT_PRO2PRO(sto,h)) + sum( res , G_MARKET_PRO2M(res,h) + G_RES_PRO(res,h))
$ontext
$offtext
)
        - phi_min_res * phi_min_res_exog * sum( h ,
         sum( (dis) , G_L(dis,h)) + sum( (nondis) , G_RES(nondis,h))

%prosumage%$ontext
         + sum( (res) , phi_res(res,h) * N_RES_PRO(res) - CU_PRO(res,h))
$ontext
$offtext
         )  =G= 0
;

con5b_max_energy(tech)$dis_bio(tech)..
         m_e(tech) -  sum( h , G_L(tech,h) ) =G= 0
;


* ---------------------------------------------------------------------------- *
***** Maximum installation constraints *****
* ---------------------------------------------------------------------------- *

con8a_max_I_power(tech)..
       m_p(tech) - N_TECH(tech)     =G= 0
;

con8b_max_I_sto_e(sto)..
       m_sto_e(sto) - N_STO_E(sto)  =G= 0
;

con8c_max_I_sto_p(sto)..
       m_sto_p(sto) -  N_STO_P(sto) =G= 0
;



con8f_max_pro_res(res_pro)..
       m_res_pro(res_pro)   -  N_RES_PRO(res_pro)   =G= 0
;

con8g_max_pro_sto_e(sto_pro)..
       m_sto_pro_e(sto_pro) -  N_STO_E_PRO(sto_pro) =G= 0
;

con8h_max_sto_pro_p(sto_pro)..
        m_sto_pro_p(sto_pro) -  N_STO_P_PRO(sto_pro) =G= 0
;

* ---------------------------------------------------------------------------- *
***** Prosumage constraints *****
* ---------------------------------------------------------------------------- *

con11a_pro_distrib(res_pro,h)..
         phi_res(res_pro,h) * N_RES_PRO(res_pro)
         - CU_PRO(res_pro,h) - G_MARKET_PRO2M(res_pro,h) - G_RES_PRO(res_pro,h) - sum( sto_pro , STO_IN_PRO2PRO(sto_pro,res_pro,h) )
         =E= 0         
;

con11b_pro_balance(h).. 
         sum( res_pro , G_RES_PRO(res_pro,h)) + sum( sto_pro , STO_OUT_PRO2PRO(sto_pro,h) ) + G_MARKET_M2PRO(h)
         - phi_pro_load * d(h)
         =E= 0
;

*** Not used in MCP model
con11c_pro_selfcon..
         sum( (h,res_pro) , G_RES_PRO(res_pro,h) ) + sum( (h,sto_pro) , STO_OUT_PRO2PRO(sto_pro,h) )
         -  phi_pro_self * sum( h , phi_pro_load * d(h))
         =G=     0
;

con11d_pro_stolev_PRO2PRO(sto_pro,h)..
        
         + sum( res_pro , STO_IN_PRO2PRO(sto_pro,res_pro,h))*(1+eta_sto(sto_pro))/2
         - STO_OUT_PRO2PRO(sto_pro,h)/(1+eta_sto(sto_pro))*2
         - STO_L_PRO2PRO(sto_pro,h)
         + STO_L_PRO2PRO(sto_pro,h-1)$((ord(h)>1) )
         =E= 0
;

*** Not used in MCP model
con11h_1_pro_stolev_start_PRO2PRO(sto_pro,h)$( ord(h) = 1)..
        STO_L_PRO2PRO(sto_pro,h) =E=  sum( res_pro , STO_IN_PRO2PRO(sto_pro,res_pro,h))*(1+eta_sto(sto_pro))/2 - STO_OUT_PRO2PRO(sto_pro,h)/(1+eta_sto(sto_pro))*2
;


con11j_pro_stolev_max(sto_pro,h)..
       N_STO_E_PRO(sto_pro) - STO_L_PRO2PRO(sto_pro,h) =G= 0
;

con11k_pro_maxin_sto(sto_pro,h)..
        N_STO_P_PRO(sto_pro) - sum( res_pro , STO_IN_PRO2PRO(sto_pro,res_pro,h) )
        =G= 0
;

con11l_pro_maxout_sto(sto_pro,h)..
        N_STO_P_PRO(sto_pro) - STO_OUT_PRO2PRO(sto_pro,h)
        =G= 0
;

*** Not used in MCP model
con11o_pro_ending(sto_pro,h)$( ord(h) = card(h))..
         STO_L_PRO2PRO(sto_pro,h) =E= phi_sto_pro_ini(sto_pro) * N_STO_E_PRO(sto_pro)
;

* ---------------------------------------------------------------------------- *
***** FOC conditions *****
* ---------------------------------------------------------------------------- *


*** System FOC ***
KKTG_L(tech,h)$dis(tech)..

    + c_m(tech)
    - lambda_enerbal(h)
%load_change_costs%$ontext    
    + lambda_convgen(tech,h)
$ontext
$offtext    
    + mu_conv_cap(tech,h)
%investment_model%$ontext      
    + mu_bio_cap(tech)$dis_bio(tech)
$ontext
$offtext
%load_change_costs%$ontext    
   - (lambda_convgen(tech,h+1))$(ord(h) > 1)
$ontext
$offtext
    =G= 0

;

KKTG_UP(dis,h)..

     - lambda_convgen(dis,h)
     + (c_up(dis))$(ord(h)> 1)
   =G= 0

;

KKTG_DO(dis,h)..

     + c_do(dis)
     + (lambda_convgen(dis,h))$(ord(h) > 1)
     =G= 0

;

* Note: Fixed generation from ror is excluded since this is a parameter
KKTG_RES(nondis,h)$(not fx(nondis))..

     - lambda_enerbal(h) + lambda_resgen(nondis,h)
     =G= 0
;

KKTCU(nondis,h)..

     c_cu(nondis) + lambda_resgen(nondis,h) =G= 0

;

KKTSTO_IN(sto,h)..

    c_m_sto(sto)  + lambda_enerbal(h) -  lambda_stolev(sto,h)*(1+eta_sto(sto))/2
    + mu_stoin_cap(sto,h) =G= 0
;

KKTSTO_OUT(sto,h)..

     c_m_sto(sto)  -  lambda_enerbal(h) +  lambda_stolev(sto,h)/(1+eta_sto(sto))*2
     +  mu_stout_cap(sto,h)
      =G= 0
;

KKTSTO_L(sto,h)..

  + lambda_stolev(sto,h)
  +  mu_stolev_cap(sto,h)
  -  (lambda_stolev(sto,h+1))$(ord(h) > 1 )

  =G= 0

;

KKTN_TECH(tech)..

          +  c_i(tech)
          +  c_fix(tech)
%investment_model%$ontext
          +  mu_tech_max_i(tech)       
$ontext
$offtext
          - sum( h,   mu_conv_cap(tech,h))$dis(tech)         
          - sum( h,  lambda_resgen(tech,h)*phi_res(tech,h))$nondis(tech)
     =G= 0

;



KKTN_STO_E(sto)..

      +  c_fix_sto(sto)/2 +  c_i_sto_e(sto)
      -  sum( h,   mu_stolev_cap(sto,h))
%investment_model%$ontext      
      +  mu_stoe_max_i(sto)
$ontext
$offtext
      =G= 0
;

KKTN_STO_P(sto)..


     c_fix_sto(sto)/2 + c_i_sto_p(sto)
     - sum( h, (mu_stoin_cap(sto,h) + mu_stout_cap(sto,h)))
%investment_model%$ontext
     + mu_stop_max_i(sto)
$ontext
$offtext
     =G= 0

;

*** Prosumage FOC ***
* FOC w.r.t CU_PRO
KKT_CU_PRO(res_pro,h)..
        lambda_resgen_pro(res_pro,h)
      =G= 0
;

* FOC w.r.t N_RES_PRO
KKT_N_RES_PRO(res_pro)..
*             c_i_pv_PRO(res_pro)
           + c_i(res_pro) + c_fix(res_pro)
           - sum(h, lambda_resgen_pro(res_pro,h)*phi_res(res_pro,h)  )
           + mu_tech_max_i_pro(res_pro)  =G= 0

;

* FOC w.r.t N_STO_E_PRO
KKT_N_STO_E_PRO(sto_pro)..
*            c_i_sto_pro_e_PRO(sto_pro)
          + c_i_sto_e(sto_pro) + c_fix_sto(sto_pro)/2
          - sum(h, mu_stolev_cap_pro(sto_pro,h) )       =G=  0

;

* FOC w.r.t N_STO_P_PRO
KKT_N_STO_P_PRO(sto_pro)..
*            c_i_sto_pro_p_PRO(sto_pro)
            c_i_sto_p(sto_pro) + c_fix_sto(sto_pro)/2
          - sum(h, mu_stoin_cap_pro(sto_pro,h))
          - sum(h, mu_stout_cap_pro(sto_pro,h))
          =G=  0
          
;

* FOC w.r.t G_MARKET_M2PRO
KKT_G_MARKET_M2PRO(h)..
*           price_consume_PRO(h)
           + lambda_enerbal(h)
*
*
           - lambda_enerbal_pro(h) =G=  0
;

* FOC w.r.t G_MARKET_PRO2M
KKT_G_MARKET_PRO2M(res_pro,h)..
*         - price_produce_PRO(h)
           - lambda_enerbal(h)
*
*
         + lambda_resgen_pro(res_pro,h)  =G= 0
;

* FOC w.r.t G_RES_PRO
KKT_G_RES_PRO(res_pro,h)..
         - lambda_enerbal_pro(h)
         + lambda_resgen_pro(res_pro,h)
%prosumage%$ontext
%minimum_SC%$ontext
         - mu_self_con_pro
$ontext
$offtext
        =G= 0
;

* FOC w.r.t STO_IN_PRO2PRO
KKT_STO_IN_PRO2PRO(sto_pro,res_pro,h)..
*            c_var_sto_pro_PRO(sto_pro)
         +  c_m_sto(sto_pro)
         +  lambda_resgen_pro(res_pro,h)
         -  lambda_stolev_pro(sto_pro,h)*(1+eta_sto(sto_pro))/2
         +  mu_stoin_cap_pro(sto_pro,h)
        =G= 0

;

* FOC w.r.t STO_OUT_PRO2PRO
KKT_STO_OUT_PRO2PRO(sto_pro,h)..
*         c_var_sto_pro_PRO(sto_pro)
       + c_m_sto(sto_pro)
       - lambda_enerbal_pro(h)
       + lambda_stolev_pro(sto_pro,h)*2/(1+eta_sto(sto_pro))
       + mu_stout_cap_pro(sto_pro,h)
%prosumage%$ontext
%minimum_SC%$ontext
        - mu_self_con_pro
$ontext
$offtext
         =G= 0
;

* FOC w.r.t STO_L_PRO2PRO
KKT_STO_L_PRO2PRO(sto_pro,h)..
      + lambda_stolev_pro(sto_pro,h)
      + mu_stolev_cap_pro(sto_pro,h)
      - lambda_stolev_pro(sto_pro,h+1)$(ord(h) > 1 )
      =G= 0
;


********************************************************************************
***** Fix unmatched variables of first period *****
********************************************************************************

G_DO.fx(dis,'h1') = 0;

* Default for reporting
G_DO.l(dis,h)   = 0;
G_L.l(dis,h)    = 0;
G_UP.l(dis,h)   = 0;
G_RES.l(tech,h) = 0;
CU.l(tech,h)    = 0;

STO_IN.l(sto,h)  = 0;
STO_OUT.l(sto,h) = 0;
STO_L.l(sto,h)   = 0;

N_TECH.l(tech)   = 0;
N_STO_E.l(sto)   = 0;
N_STO_P.l(sto)   = 0;


********************************************************************************
***** MODEL *****
********************************************************************************

model DIETER /
obj

con1a_bal

%load_change_costs%$ontext
con2_loadlevel
$ontext
$offtext

con3a_maxprod_dispatchable
con3e_maxprod_res

con4_stolev
con4c_stolev_max
con4d_maxin_sto
con4e_maxout_sto

con5a_minRES
%investment_model%$ontext   
con5b_max_energy
$ontext
$offtext

%investment_model%$ontext     
con8a_max_I_power
con8b_max_I_sto_e
con8c_max_I_sto_p
$ontext
$offtext

%prosumage%$ontext
con8f_max_pro_res
*con8g_max_pro_sto_e
*con8h_max_sto_pro_p
con11a_pro_distrib
con11b_pro_balance
con11c_pro_selfcon
con11d_pro_stolev_PRO2PRO
con11j_pro_stolev_max
con11k_pro_maxin_sto
con11l_pro_maxout_sto
$ontext
$offtext

/;

model DIETER_MCP /

con1a_bal.lambda_enerbal

%load_change_costs%$ontext
con2_loadlevel.lambda_convgen
$ontext
$offtext

con3a_maxprod_dispatchable.mu_conv_cap
con3e_maxprod_res.lambda_resgen

con4_stolev.lambda_stolev
con4c_stolev_max.mu_stolev_cap
con4d_maxin_sto.mu_stoin_cap
con4e_maxout_sto.mu_stout_cap

%investment_model%$ontext  
con5b_max_energy.mu_bio_cap
$ontext
$offtext

%investment_model%$ontext     
con8a_max_I_power.mu_tech_max_i
con8b_max_I_sto_e.mu_stoe_max_i
con8c_max_I_sto_p.mu_stop_max_i
$ontext
$offtext

KKTG_L.G_L

%load_change_costs%$ontext
KKTG_UP.G_UP
KKTG_DO.G_DO
$ontext
$offtext

KKTG_RES.G_RES
KKTCU.CU
KKTSTO_IN.STO_IN
KKTSTO_OUT.STO_OUT
KKTSTO_L.STO_L

%investment_model%$ontext 
KKTN_TECH.N_TECH
KKTN_STO_E.N_STO_E
KKTN_STO_P.N_STO_P
$ontext
$offtext

%prosumage%$ontext
con8f_max_pro_res.mu_tech_max_i_pro
*con8g_max_pro_sto_e
*con8h_max_sto_pro_p
con11a_pro_distrib.lambda_resgen_pro
con11b_pro_balance.lambda_enerbal_pro
con11c_pro_selfcon.mu_self_con_pro
con11d_pro_stolev_PRO2PRO.lambda_stolev_pro
con11j_pro_stolev_max.mu_stolev_cap_pro
con11k_pro_maxin_sto.mu_stoin_cap_pro
con11l_pro_maxout_sto.mu_stout_cap_pro

KKT_CU_PRO.CU_PRO
KKT_N_RES_PRO.N_RES_PRO
KKT_N_STO_E_PRO.N_STO_E_PRO
KKT_N_STO_P_PRO.N_STO_P_PRO
KKT_G_MARKET_M2PRO.G_MARKET_M2PRO
KKT_G_MARKET_PRO2M.G_MARKET_PRO2M
KKT_G_RES_PRO.G_RES_PRO
KKT_STO_IN_PRO2PRO.STO_IN_PRO2PRO
KKT_STO_OUT_PRO2PRO.STO_OUT_PRO2PRO
KKT_STO_L_PRO2PRO.STO_L_PRO2PRO
$ontext
$offtext




/ ;

