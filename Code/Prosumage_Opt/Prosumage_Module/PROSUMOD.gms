********************************************************************************
********************************************************************************
*   This model is a dispatch and investment model for PROSUMAGE households
*   PROSUMAGE households PROduce, conSUMe electricity and usage storAGE
*
*   Date of this version: October 17, 2018
*
********************************************************************************
********************************************************************************


******************* Format and model run specifications ************************

* ------------- Household selection---------------------------------------------
* Select a household profile between V1 and V74:

$setglobal household_profile "V17"


* ------------- Base year selection---------------------------------------------
* Select year between 2010 and 2016:

$setglobal base_year "2014"

* ----------------- Select if to use MCP or LP format --------------------------

* Set to "*" to select linear program, leave blank to select MCP
$setglobal LP "*"

* Do not change these two lines
$if "%LP%" == "" $setglobal MCP "*"
$if "%LP%" == "*" $setglobal MCP ""

* --Select if to restrict storage level optimization to 24h horizon (faster)----

$setglobal horizon24 "*"

* ------------- Set data import and export options -----------------------------

* mark offXcel with a star to turn off excel import and import gdx file
* mark modelkill to create a gdx file only without model run

$setglobal offXcel "*"
$setglobal modelkill ""

* Set column index (alphabetic) in excel file furthest to the right
* to speed up data import

$setglobal colindex "H"

* Name input file suffix other than "input data"
$setglobal modelrun ""

* Auto set of input file (do not change)
$setglobal uploadfile "data\%modelrun%input_data"

* Auto set of output file (do not change)
$setglobal outputfile "results\%modelrun%_results"


**************** Sets, variables, parameters declaration ***********************

Sets
sto_pro          Storage technolgies of prosumage HH        /storage/
res_pro          Renewable technologies of prosumage HH     /solar/
h                Hours                                      /h1*h8760/
year             Base years                                 /2010*2016/
hh_profile       Household load data                        /V1*V74/

* Include subset of all first hours of days within a year
$include 24h_FirstHours.gms
;

Variables
Z_PRO                        Prosumage: Objective value
lambda_enerbal_PRO(h)        Prosumage: Dual variable of energy balance equation
lambda_pvgen_PRO(res_pro,h)  Prosumage: Dual variable of pv generation equation
lambda_stolev_PRO(sto_pro,h) Prosumage: Dual variable of storage level equation
lambda_stolev1_PRO           Prosumage: Dual variable of storage initial level equation
lambda_stolev24h_PRO(sto_pro,h) Prosumage: Dual variable of storage 24h horizon
;

Positive Variables
mu_pv_cap_PRO(res_pro)               Prosumage: Dual variable of max pv capacity inequality
mu_stolev_cap_PRO(sto_pro,h)         Prosumage: Dual variable of max storage level inequality
mu_stoin_cap_PRO(sto_pro,h)          Prosumage: Dual variable of max storing in inequality
mu_stoout_cap_PRO(sto_pro,h)         Prosumage: Dual variable of max storing out inequality
;

Positive variables
CU_PRO(res_pro,h)                     Prosumage: Curtailment of prosumage pv energy
N_PV_PRO(res_pro)                     Prosumage: PV generation capacities
N_STO_E_PRO(sto_pro)                  Prosumage: Capacity of prosumage storage energy
N_STO_P_PRO(sto_pro)                  Prosumage: Capacity of prosumage storage power
E_M2PRO_PRO(h)                        Prosumage: Energy purchased from market for prosumage demand
G_PRO2M_PRO(res_pro,h)                Prosumage: Energy sold to market from prosumage pv generation
G_PRO2PRO_PRO(res_pro,h)              Prosumage: Prosumage pv generation direct consumed by household
STO_IN_PRO2PRO_PRO(sto_pro,res_pro,h) Prosumage: Storage loading from prosumage pv generation
STO_OUT_PRO2PRO_PRO(sto_pro,h)        Prosumage: Storage discharging to household demand
STO_L_PRO(sto_pro,h)                  Prosumage: Storage level prosumage household

;

Parameters
sto_pro_ini_last_PRO(sto_pro)        Prosumage: Level of storage in initial and last period
eta_sto_pro_in_PRO(sto_pro)          Prosumage: Efficiency: storage in
eta_sto_pro_out_PRO(sto_pro)         Prosumage: Efficiency: storage out
d_PRO(h)                             Prosumage: Household load
d_upload(h,hh_profile)               Prosumage: Household load - upload parameter
avail_solar_PRO(h)                   Prosumage: Hourly capacity factor for pv
avail_solar_upload(h,year)           Prosumage: Hourly capacity factor pv - upload parameter
pv_cap_max_PRO(res_pro)              Prosumage: PV capacity maximum
price_produce_PRO(h)                 Prosumage: Price for selling energy per kWh
price_produce_upload(h,year)         Prosumage: Price for selling energy per MWh - upload parameter
price_consume_PRO(h)                 Prosumage: Price for energy consumption per kWh
c_i_sto_pro_e_PRO(sto_pro)           Prosumage: Cost: investment into storage energy
c_i_sto_pro_p_PRO(sto_pro)           Prosumage: Cost: investment into storage power
c_i_pv_PRO(res_pro)                  Prosumage: Cost: investment into renewable capacity
c_var_sto_pro_PRO(sto_pro)           Prosumage: Cost: variable generation costs storage

;


* Declare efficiency parameters
sto_pro_ini_last_PRO(sto_pro)  =  0.5    ;
eta_sto_pro_in_PRO(sto_pro)    =  0.81   ;
eta_sto_pro_out_PRO(sto_pro)   =  0.926  ;

* Declare cost parameters
c_i_sto_pro_e_PRO(sto_pro)  =  5418.14/1000 ;
c_i_sto_pro_p_PRO(sto_pro)  = 50995.48/1000 ;
c_i_pv_PRO('solar')         = 60526.64/1000 ;
c_var_sto_pro_PRO(sto_pro)  =     0.5/1000  ;
price_consume_PRO(h)        =     0.30      ;

* Declare further restrictions
pv_cap_max_PRO(res_pro)     = 10;


***************************** Upload data **************************************

$onecho >%uploadfile%.tmp

par=d_upload                 rng=demand!a3:bw8764             rdim=1 cdim=1
par=avail_solar_upload       rng=solar!a3:%colindex%8764      rdim=1 cdim=1
par=price_produce_upload     rng=price!a3:%colindex%8764      rdim=1 cdim=1

$offecho


%offXcel%$call "gdxxrw %uploadfile%.xlsx squeeze=N @%uploadfile%.tmp  o=%uploadfile%.gdx  ";
$GDXin %uploadfile%.gdx
$load d_upload avail_solar_upload price_produce_upload
;

* Load data for specific household and base year
d_PRO(h)                   = d_upload(h,'%household_profile%') ;
avail_solar_PRO(h)         = avail_solar_upload(h,'%base_year%') ;

*Load market price as price per kWh
price_produce_PRO(h)        = price_produce_upload(h,'%base_year%')/1000 ;
*price_produce_PRO(h)        = 0.12 ;

$if "%modelkill%" == "*"  $abort Check GDX upload

****************************** Model setup *************************************

* Minimization Problem equations (Dual variables in parenthesis)
Equations
objective_PRO               Prosumage: Household objective function
energy_balance_PRO          Prosumage: Household energy balance (lambda_enerbal_PRO )
pv_generation_PRO           Prosumage: Household use of pv energy generation (lambda_pvgen_PRO)
stolev_PRO                  Prosumage: Storage level dynamics (lambda_stolev_PRO)
stolev_init_PRO             Prosumage: Storage level in initial and last period  (lambda_stolev1_PRO)
pv_install_max_PRO          Prosumage: Household PV installation capacity constraint (mu_pv_cap_PRO)
stolev_max_energy_PRO       Prosumage: Storage capacity constraint on maximum energy (mu_stolev_cap_PRO)
stolev_24h_PRO              Prosumage: Storage optimization time horizon constraint
stoin_max_power_PRO         Prosumage: Storage capacity constraint on maximum power - storing in (mu_stoin_cap_PRO)
stoout_max_power_PRO        Prosumage: Storage capacity constraint on maximum power - storing out (mu_stoout_cap_PRO)
;

* Additional MCP equations and inequalities
Equations
KKT_CU_PRO                  Prosumage: FOC w.r.t CU_PRO
KKT_N_PV_PRO                Prosumage: FOC w.r.t N_PV_PRO
KKT_N_STO_E_PRO             Prosumage: FOC w.r.t N_STO_E_PRO
KKT_N_STO_P_PRO             Prosumage: FOC w.r.t N_STO_P_PRO
KKT_E_M2PRO_PRO             Prosumage: FOC w.r.t E_M2PRO_PRO
KKT_G_PRO2M_PRO             Prosumage: FOC w.r.t G_PRO2M_PRO
KKT_G_PRO2PRO_PRO           Prosumage: FOC w.r.t G_PRO2PRO_PRO
KKT_STO_IN_PRO2PRO_PRO      Prosumage: FOC w.r.t STO_IN_PRO2PRO_PRO
KKT_STO_OUT_PRO2PRO_PRO     Prosumage: FOC w.r.t STO_OUT_PRO2PRO_PRO
KKT_STO_L_PRO               Prosumage: FOC w.r.t STO_L_PRO
;

*** Objective function prosumage household: Minimize total electricity costs
objective_PRO..
 Z_PRO =E=
      sum( res_pro , c_i_pv_PRO(res_pro) * N_PV_PRO(res_pro) )
    + sum( sto_pro , c_i_sto_pro_e_PRO(sto_pro) * N_STO_E_PRO(sto_pro))
    + sum( sto_pro , + c_i_sto_pro_p_PRO(sto_pro) * N_STO_P_PRO(sto_pro) )
    + sum( (sto_pro,res_pro,h) ,
            c_var_sto_pro_PRO(sto_pro)*(STO_IN_PRO2PRO_PRO(sto_pro,res_pro,h)))
    + sum( (sto_pro,h) ,
            c_var_sto_pro_PRO(sto_pro)*STO_OUT_PRO2PRO_PRO(sto_pro,h) )
    + sum(  h , price_consume_PRO(h) * (E_M2PRO_PRO(h) ))
    - sum( (res_pro,h) , price_produce_PRO(h) * G_PRO2M_PRO(res_pro,h) )
;

*** Household energy balance: Satisfy load with own generation, storage and grid electricity
energy_balance_PRO(h)..
          + sum( res_pro , G_PRO2PRO_PRO(res_pro,h))
          + sum( sto_pro , STO_OUT_PRO2PRO_PRO(sto_pro,h))
          + E_M2PRO_PRO(h)
          - d_PRO(h)
          =E= 0
;

*** Household PV generation usage: Directly consumed, curtailed, stored or sold
pv_generation_PRO(res_pro,h)..
       +  avail_solar_PRO(h)* N_PV_PRO(res_pro)
       -  CU_PRO(res_pro,h)
       -  G_PRO2M_PRO(res_pro,h)
       -  G_PRO2PRO_PRO(res_pro,h)
       -  sum( sto_pro , STO_IN_PRO2PRO_PRO(sto_pro,res_pro,h) )
       =E= 0
;

*** Restrict PV capacity
pv_install_max_PRO(res_pro)..

       pv_cap_max_PRO(res_pro) - N_PV_PRO(res_pro) =G= 0
;


*** Technical constraints on storage

*Storage level for all hours except first: Prio level plus intake minus outflow
$ontext
stolev_init_PRO(sto_pro,'h1')..
        STO_L_PRO(sto_pro,'h1') =E=  STO_L_PRO(sto_pro,'h8760')
;
$offtext

** Overall storage level
stolev_PRO(sto_pro,h)$((ord(h)>1) )..
        + STO_L_PRO(sto_pro,h-1)
        + sum(res_pro ,
          STO_IN_PRO2PRO_PRO(sto_pro,res_pro,h))*eta_sto_pro_in_PRO(sto_pro)
        - STO_OUT_PRO2PRO_PRO(sto_pro,h)/eta_sto_pro_out_PRO(sto_pro)
        - STO_L_PRO(sto_pro,h)
        =E=   0
;

stolev_24h_PRO(sto_pro,h)..
        - STO_L_PRO(sto_pro,h)$(h24(h)) =E= 0
;

* Storage maximum energy capacity
stolev_max_energy_PRO(sto_pro,h)..
         N_STO_E_PRO(sto_pro) - STO_L_PRO(sto_pro,h) =G= 0
;

* Storage maximum charging capacity (power in)
stoin_max_power_PRO(sto_pro,h)..
        N_STO_P_PRO(sto_pro)
     -  sum(res_pro, STO_IN_PRO2PRO_PRO(sto_pro,res_pro,h)) =G= 0
;

* Storage maximum discharging capacity (power out)
stoout_max_power_PRO(sto_pro,h)..
         N_STO_P_PRO(sto_pro) - STO_OUT_PRO2PRO_PRO(sto_pro,h) =G= 0
;

* FOC w.r.t CU_PRO
KKT_CU_PRO(res_pro,h)..
        lambda_pvgen_PRO(res_pro,h)
      =G= 0
;

* FOC w.r.t N_PV_PRO
KKT_N_PV_PRO(res_pro)..
             c_i_pv_PRO(res_pro)
           - sum(h, lambda_pvgen_PRO(res_pro,h)*avail_solar_PRO(h)  )
           + mu_pv_cap_PRO(res_pro)  =G= 0

;

* FOC w.r.t N_STO_E_PRO
KKT_N_STO_E_PRO(sto_pro)..
            c_i_sto_pro_e_PRO(sto_pro)
          - sum(h, mu_stolev_cap_PRO(sto_pro,h) )       =G=  0

;

* FOC w.r.t N_STO_P_PRO
KKT_N_STO_P_PRO(sto_pro)..
            c_i_sto_pro_p_PRO(sto_pro)
          - sum(h, mu_stoin_cap_PRO(sto_pro,h))
          - sum(h, mu_stoout_cap_PRO(sto_pro,h))        =G=  0
;

* FOC w.r.t E_M2PRO_PRO
KKT_E_M2PRO_PRO(h)..
           price_consume_PRO(h) - lambda_enerbal_PRO(h) =G=  0
;

* FOC w.r.t G_PRO2M_PRO
KKT_G_PRO2M_PRO(res_pro,h)..
         - price_produce_PRO(h) + lambda_pvgen_PRO(res_pro,h)  =G= 0
;

* FOC w.r.t G_PRO2PRO_PRO
KKT_G_PRO2PRO_PRO(res_pro,h)..
         - lambda_enerbal_PRO(h)
         + lambda_pvgen_PRO(res_pro,h)
        =G= 0
;

* FOC w.r.t STO_IN_PRO2PRO_PRO
KKT_STO_IN_PRO2PRO_PRO(sto_pro,res_pro,h)..
            c_var_sto_pro_PRO(sto_pro)
         +  lambda_pvgen_PRO(res_pro,h)
         -  lambda_stolev_PRO(sto_pro,h)*eta_sto_pro_in_PRO(sto_pro)
         +  mu_stoin_cap_PRO(sto_pro,h)
        =G= 0

;

* FOC w.r.t STO_OUT_PRO2PRO_PRO
KKT_STO_OUT_PRO2PRO_PRO(sto_pro,h)..
         c_var_sto_pro_PRO(sto_pro)
       - lambda_enerbal_PRO(h)
       + lambda_stolev_PRO(sto_pro,h)/eta_sto_pro_out_PRO(sto_pro)
       + mu_stoout_cap_PRO(sto_pro,h)
         =G= 0
;

* FOC w.r.t STO_L_PRO
KKT_STO_L_PRO(sto_pro,h)..
      + lambda_stolev_PRO(sto_pro,h)
      - lambda_stolev_PRO(sto_pro,h+1)
      + mu_stolev_cap_PRO(sto_pro,h)
%horizon24%$ontext
      + (lambda_stolev24h_PRO(sto_pro,h))$h24(h)
$ontext
$offtext
      =G= 0
;




***************************** Initialize model *********************************

*Linear Program
Model prosumod_lp /
objective_PRO
energy_balance_PRO
pv_generation_PRO
pv_install_max_PRO
stolev_max_energy_PRO
stoin_max_power_PRO
stoout_max_power_PRO
stolev_PRO
%horizon24%$ontext
stolev_24h_PRO
$ontext
$offtext
*stolev_init_PRO
/


* MCP Format
Model prosumod_mcp /
energy_balance_PRO.lambda_enerbal_PRO
pv_generation_PRO.lambda_pvgen_PRO
stolev_PRO.lambda_stolev_PRO
*stolev_init_PRO.lambda_stolev1_PRO
pv_install_max_PRO.mu_pv_cap_PRO
stolev_max_energy_PRO.mu_stolev_cap_PRO
stoin_max_power_PRO.mu_stoin_cap_PRO
stoout_max_power_PRO.mu_stoout_cap_PRO
KKT_CU_PRO.CU_PRO
KKT_N_PV_PRO.N_PV_PRO
KKT_N_STO_E_PRO.N_STO_E_PRO
KKT_N_STO_P_PRO.N_STO_P_PRO
KKT_E_M2PRO_PRO.E_M2PRO_PRO
KKT_G_PRO2M_PRO.G_PRO2M_PRO
KKT_G_PRO2PRO_PRO.G_PRO2PRO_PRO
KKT_STO_IN_PRO2PRO_PRO.STO_IN_PRO2PRO_PRO
KKT_STO_OUT_PRO2PRO_PRO.STO_OUT_PRO2PRO_PRO
KKT_STO_L_PRO.STO_L_PRO
%horizon24%$ontext
stolev_24h_PRO.lambda_stolev24h_PRO
$ontext
$offtext

/

options
optcr = 0.00
reslim = 10000000
lp = cplex
mip = cplex
nlp = conopt
dispwidth = 15
limrow = 0
limcol = 0
solprint = off
sysout = off
optcr = 1e-3
optca = 10
PROFILETOL = 10;

;

$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3


epagap 10
parallelmode -1
$offecho

* Fix first storage level dual
lambda_stolev_PRO.fx(sto_pro,'h1') = 100;
;

%LP%$ontext
solve   prosumod_lp using lp min Z_PRO;
$ontext
$offtext

%MCP%$ontext
solve   prosumod_mcp using mcp;
$ontext
$offtext



*Reporting Parameters
Parameters
E_purchased
E_sold
Z_PRO_mcp
mean_price
full_load
self_cons_rate;

E_purchased = sum((sto_pro,h), E_M2PRO_PRO.l(h) );
E_sold      = sum((res_pro,sto_pro,h), G_PRO2M_PRO.l(res_pro,h));
Z_PRO_mcp   = sum( res_pro , c_i_pv_PRO(res_pro) * N_PV_PRO.l(res_pro) )
                 + sum( sto_pro , c_i_sto_pro_e_PRO(sto_pro) * N_STO_E_PRO.l(sto_pro) + c_i_sto_pro_p_PRO(sto_pro) * N_STO_P_PRO.l(sto_pro) )
                 + sum( (sto_pro,res_pro,h) , c_var_sto_pro_PRO(sto_pro) * ( STO_IN_PRO2PRO_PRO.l(sto_pro,res_pro,h)))
                 + sum( (sto_pro,h) , c_var_sto_pro_PRO(sto_pro) *  STO_OUT_PRO2PRO_PRO.l(sto_pro,h) )
                 + sum(  h , price_consume_PRO(h) * (E_M2PRO_PRO.l(h) ))
                 - sum(  (res_pro,h) , price_produce_PRO(h) * G_PRO2M_PRO.l(res_pro,h) )  ;

mean_price = sum( h,  price_produce_PRO(h))/card(h)*1000;
full_load  = sum(h, avail_solar_PRO(h));
self_cons_rate(res_pro) = sum(h, G_PRO2PRO_PRO.l(res_pro,h)
+ sum(sto_pro, STO_IN_PRO2PRO_PRO.l(sto_pro,res_pro,h)*eta_sto_pro_in_PRO(sto_pro)) ) / sum(h, avail_solar_PRO(h)*N_PV_PRO.l(res_pro));


display d_PRO , N_PV_PRO.l , N_STO_E_PRO.l, N_STO_P_PRO.l,
          E_M2PRO_PRO.l , G_PRO2M_PRO.l ,
        STO_L_PRO.l, price_produce_PRO, energy_balance_PRO.m ,
        E_purchased , E_sold, Z_PRO_mcp , mean_price , full_load, self_cons_rate
;
