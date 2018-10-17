********************************************************************************
********************************************************************************
*   This model is a dispatch and investment model for PROSUMAGE households
*   PROSUMAGE households PROduce, conSUMe electricity and usage storAGE
*
*   Date of this version: October 10, 2018
*
********************************************************************************
********************************************************************************


******************* Format and model run specifications ************************

* ------------- household selection---------------------------------------------
* Select a household profile between V1 and V74:

$setglobal household_profile "V17"


* ------------- Base year selection---------------------------------------------
* Select year between 2010 and 2016:

$setglobal base_year "2014"

* ------------- Storage charching ----------------------------------------------
* Select if storage may only charge from own PV production (*) or also from grid

$setglobal sto_pv_only "*"


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
h                Hours                                   /h1*h8760/
res              Renewable technologies                  /solar/
sto              Storage technolgies                     /storage/
year             Base years                              /2010*2016/
hh_profile       Household load data                     /V1*V74/

;

Variables
Z                        Objective
;

Positive variables
CU_PRO(res,h)                   Prosumage:Curtailment of pv energy
N_RES_PRO(res)                  Prosumage: PV generation capacities
N_STO_E_PRO(sto)                Prosumage:Capacities: storage energy
N_STO_P_PRO(sto)                Prosumage:Capacities: storage power
G_MARKET_M2PRO(h)               Prosumage: Energy generation purchased from market
G_MARKET_PRO2M(res,h)           Prosumage: Energy generation sold to market
G_RES_PRO(res,h)                Prosumage: Energy generation directly consumed

STO_IN_PRO(res,sto,h)        Prosumage: storage loading from generation
STO_IN_M(sto,h)              Prosumage: storage loading from market
STO_OUT_PRO(sto,h)           Prosumage: storage discharging to consumption from generation
STO_OUT_M(sto,h)             Prosumage: storage discharging to consumption from market
STO_L_PRO(sto,h)             Prosumage: storage level generation to consumption
STO_L_M(sto,h)               Prosumage: storage level market to consumotion
STO_L_all(sto,h)                 Prosumage: overall storage level

;

Parameters
sto_ini_last(sto)           Level of storage in initial and last period
eta_sto_in(sto)             Efficiency: storage in
eta_sto_out(sto)            Efficiency: storage out
d(h)                        Household load
d_upload(h,hh_profile)      Household load - upload parameter
avail_solar(h)              Hourly capacity factor for pv
avail_solar_upload(h,year)  Hourly capacity factor pv - upload parameter
pv_cap_max(res)             PV capacity maximum
price_market(h)             Price for selling energy per kWh
price_market_upload(h,year) Price for selling energy per MWh - upload parameter
price_buy                   Price for energy consumption per kWh
c_i_sto_e(sto)              Cost: investment into storage energy
c_i_sto_p(sto)              Cost: investment into storage power
c_i_pv(res)                 Cost: investment into renewable capacity
c_var_sto(sto)              Cost: variable generation costs storage
penalty                     Penalty term


;


* Declare efficiency parameters
sto_ini_last(sto)  =  0.5 ;
eta_sto_in(sto)    =  0.81 ;
eta_sto_out(sto)   =  0.926 ;
penalty = 0 ;


* Declare cost parameters
c_i_sto_e(sto)  =  5418.14/1000 ;
c_i_sto_p(sto)  = 50995.48/1000 ;
c_i_pv('solar') = 60526.64/1000 ;
c_var_sto(sto)  =     0.5/1000 ;
price_buy       =     0.30 ;

* Declare further restrictions
pv_cap_max(res) = 10;


***************************** Upload data **************************************

$onecho >%uploadfile%.tmp

par=d_upload             rng=demand!a3:bw8764             rdim=1 cdim=1
par=avail_solar_upload   rng=solar!a3:%colindex%8764      rdim=1 cdim=1
par=price_market_upload  rng=price!a3:%colindex%8764      rdim=1 cdim=1

$offecho


%offXcel%$call "gdxxrw %uploadfile%.xlsx squeeze=N @%uploadfile%.tmp  o=%uploadfile%.gdx  ";
$GDXin %uploadfile%.gdx
$load d_upload avail_solar_upload price_market_upload
;

* Load data for specific household and base year
d(h)                   = d_upload(h,'%household_profile%') ;
avail_solar(h)         = avail_solar_upload(h,'%base_year%') ;

*Load market price as price per kWh
price_market(h)        = price_market_upload(h,'%base_year%')/1000 ;
*price_market(h)        = 0.12 ;

$if "%modelkill%" == "*"  $abort Check GDX upload

****************************** Model setup *************************************

Equations
objective                  Objective function
hh_energy_balance          Household energy balance
pv_generation              Household use of pv energy generation
pv_install_max(res)        PV capacity constraint
energy_tomarket            Amount of energy sold to market
energy_frommarket          Amount of energy purchased from market
stolev_no_freelunch        Storage level in initial and last period
stolevel                   Storage level dynamics
stolev_max_energy          Storage capacity constraint on maximum energy
stoin_max_power            Storage capacity constraint on maximum power - storing in
stoout_max_power           Storage capacity constraint on maximum power - storing out
foresight_24h              Perfect foresight only for next 24h

pro_stolev_PRO
pro_stolev_M
pro_stolev_start_PRO
pro_stolev_start_M
pro_stolev
pro_stolev_start
;


*** Objective function: Minimize total system costs
objective..
   Z =E=

           sum( res , c_i_pv(res) * N_RES_PRO(res) )
         + sum( sto , c_i_sto_e(sto) * N_STO_E_PRO(sto) + c_i_sto_p(sto) * N_STO_P_PRO(sto) )
         + sum( (res,sto,h) , c_var_sto(sto) * ( STO_IN_PRO(res,sto,h)))
         + sum( (sto,h) , c_var_sto(sto) * ( STO_IN_M(sto,h) + STO_OUT_PRO(sto,h) + STO_OUT_M(sto,h)) )
         + sum(  h , price_buy * (G_MARKET_M2PRO(h) ))
         + sum(  (sto,h) , price_buy * ( STO_IN_M(sto,h) ))
         - sum(  (res,h) , price_market(h) * G_MARKET_PRO2M(res,h) )
         - sum(  (sto,h) , price_market(h)* STO_OUT_M(sto,h)  )
;

*** Household energy balance: Satisfy load with own generation, storage and grid
hh_energy_balance(h)..

  d(h) =E=

            sum( res , G_RES_PRO(res,h))
          + sum( sto , STO_OUT_PRO(sto,h))
          + G_MARKET_M2PRO(h)
          - sum(sto, STO_IN_M(sto,h))
;

*** Household PV generation usage: Directly consumed, curtailed,stored or sold
pv_generation(res,h)..

      avail_solar(h) * N_RES_PRO(res) =E=

        CU_PRO(res,h) + G_MARKET_PRO2M(res,h) + G_RES_PRO(res,h) + sum( sto , STO_IN_PRO(res,sto,h) )

;

*** Restrict PV capacity
pv_install_max(res)..

         N_RES_PRO(res) =L= pv_cap_max(res)
;


*** Technical constraints on storage

*Storage level for all hours except first: Prio level plus intake minus outflow
$ontext
pro_stolev_PRO(sto,h)$( (ord(h)>1) )..
         STO_L_PRO(sto,h) =E= STO_L_PRO(sto,h-1) + sum( res , STO_IN_PRO(res,sto,h))*eta_sto_in(sto) - STO_OUT_PRO(sto,h)/eta_sto_out(sto)
;


pro_stolev_M(sto,h)$( (ord(h)>1) )..
         STO_L_M(sto,h) =E= STO_L_M(sto,h-1) + STO_IN_M(sto,h)*eta_sto_in(sto) - STO_OUT_M(sto,h)/eta_sto_out(sto)
;


* Storage level for first period
pro_stolev_start_PRO(sto,'h1')..
        STO_L_PRO(sto,'h1') =E= 0.5 * sto_ini_last(sto) * N_STO_E_PRO(sto) + sum( res , STO_IN_PRO(res,sto,'h1'))*eta_sto_in(sto) - STO_OUT_PRO(sto,'h1')/eta_sto_out(sto)
;


pro_stolev_start_M(sto,'h1')..
        STO_L_M(sto,'h1') =E= 0.5 * sto_ini_last(sto) * N_STO_E_PRO(sto) + STO_IN_M(sto,'h1')*eta_sto_in(sto) - STO_OUT_M(sto,'h1')/eta_sto_out(sto)
;
$offtext

pro_stolev_start(sto,'h1')..
        STO_L_all(sto,'h1') =E=  STO_L_all(sto,'h8760')
;

** Overall storage level
pro_stolev(sto,h)$( (ord(h)>1) )..
         STO_L_all(sto,h) =E=   STO_L_all(sto,h-1) + sum( res , STO_IN_PRO(res,sto,h))*eta_sto_in(sto) - STO_OUT_PRO(sto,h)/eta_sto_out(sto)
                                +  STO_IN_M(sto,h)*eta_sto_in(sto) - STO_OUT_M(sto,h)/eta_sto_out(sto)
;

* Storage maximum energy capacity
stolev_max_energy(sto,h)..

         STO_L_all(sto,h) =L= N_STO_E_PRO(sto)
;

* Storage maximum charging capacity (power in)
stoin_max_power(sto,h)..

         sum(res, STO_IN_PRO(res,sto,h))+ STO_IN_M(sto,h) =L= N_STO_P_PRO(sto)
;

* Storage maximum discharging capacity (power out)
stoout_max_power(sto,h)..

         STO_OUT_PRO(sto,h) + STO_OUT_M(sto,h) =L= N_STO_P_PRO(sto)
;

***************************** Initialize model *********************************
Model prosumod /
objective
hh_energy_balance
pv_generation
pv_install_max
stolev_max_energy
stoin_max_power
stoout_max_power
*pro_stolev_PRO
*pro_stolev_M
*pro_stolev_start_PRO
*pro_stolev_start_M
pro_stolev
pro_stolev_start
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
;

$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
epagap 10
parallelmode -1
$offecho


* Fix variables for comparison
*STO_IN_M.fx(sto,h) = 0;
*STO_OUT_M.fx(sto,h) = 0;

solve   prosumod using lp min Z;
display d , N_RES_PRO.l , N_STO_E_PRO.l, N_STO_P_PRO.l,
         Z.l, G_MARKET_M2PRO.l , G_MARKET_PRO2M.l ,
        STO_L_all.l, price_market, hh_energy_balance.m ,
         STO_IN_M.l, STO_OUT_M.l         ;


***************************** Set up reporting *********************************


