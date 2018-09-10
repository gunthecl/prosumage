********************************************************************************
********************************************************************************
*   This model is a dispatch and investment model for PROSUMAGE households
*   PROSUMAGE households PROduce, conSUMe electricity and usage storAGE
*
*   Date of this version: August 6, 2018
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
G_PV(h)              Generation of pv plant
CU(h)                Curtailment of pv energy
N_PV                 PV generation capacities
N_STO_E             Capacities: storage energy
N_STO_P             Capacities: storage power
STO_L(h)             Storage level
STO_IN(h)            Storage intake
STO_OUT(h)           Storage generation
E_buy(h)                 Energy purchased from market
E_sell(h)                Energy sold to market
lambda(h)
mu(h)
gamma1
gamma2(h)
gamma3(h)
gamma4(h)
levelsto

;

Parameters
sto_ini_last           Level of storage in initial and last period
eta_sto_in             Efficiency: storage in
eta_sto_out            Efficiency: storage out
d(h)                        Household load
d_upload(h,hh_profile)      Household load - upload parameter
avail_solar(h)              Hourly capacity factor for pv
avail_solar_upload(h,year)  Hourly capacity factor pv - upload parameter
pv_cap_max             PV capacity maximum
price_market(h)             Price for selling energy per kWh
price_market_upload(h,year) Price for selling energy per MWh - upload parameter
price_buy                   Price for energy consumption per kWh
c_i_sto_e              Cost: investment into storage energy
c_i_sto_p             Cost: investment into storage power
c_i_pv                Cost: investment into renewable capacity
c_var_sto              Cost: variable generation costs storage
penalty                     Penalty term
lev_Z
lev_EB
lev_ES

;


* Declare efficiency parameters
sto_ini_last  =  0.5 ;
eta_sto_in    =  0.81 ;
eta_sto_out   =  0.926 ;
penalty = 0 ;


* Declare cost parameters
c_i_sto_e  =  5418.14/1000 ;
c_i_sto_p  = 50995.48/1000 ;
c_i_pv           = 60526.64/1000 ;
c_var_sto  =     0.5/1000 ;
price_buy       =     0.30 ;

* Declare further restrictions
pv_cap_max = 10;


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
pv_install_max        PV capacity constraint
energy_tomarket            Amount of energy sold to market
energy_frommarket          Amount of energy purchased from market
stolev_no_freelunch        Storage level in initial and last period
stolevel                   Storage level dynamics
stolev_max_energy          Storage capacity constraint on maximum energy
stoin_max_power            Storage capacity constraint on maximum power - storing in
stoout_max_power           Storage capacity constraint on maximum power - storing out
KKTNPV
KKTNSTOE
KKTNSTOP
KKTEB
KKTES
KKTG
KKTSTOIN
KKTSTOUT
KKTSTOLEV
KKTCU


;


*** Objective function: Minimize total system costs
objective..

   Z =E=

          c_i_pv * N_PV
         + c_i_sto_e * N_STO_E + c_i_sto_p * N_STO_P
         + sum( (h) , c_var_sto * (STO_IN(h) + STO_OUT(h)) )
         + sum(  h , price_buy * E_buy(h))
         - sum(  h , price_market(h) * E_sell(h))
;

*** Household energy balance: Satisfy load with own generation, storage and grid
hh_energy_balance(h)..


           G_PV(h)
         + STO_OUT(h)
         + E_buy(h)   =E=
          d(h)
;

*** Household PV generation usage: Directly consumed, curtailed,stored or sold
pv_generation(h)..

      avail_solar(h) * N_PV =E=
        G_PV(h)
      + CU(h)
      + STO_IN(h)
      + E_sell(h)
;

*** Restrict PV capacity
pv_install_max..

          pv_cap_max - N_PV =G= 0
;




*** Technical constraints on storage
*Storage level in first and last period must be the same
$ontext
stolev_no_freelunch..

          STO_L('h8760') - STO_L('h1') =E= 0
;

$offtext

*Storage level for all hours except first: Prio level plus intake minus outflow
stolevel(h)..


         STO_L(h--1)
         + STO_IN(h) * eta_sto_in
         - STO_OUT(h)/eta_sto_out
         =E=  STO_L(h)
;

* Storage maximum energy capacity
stolev_max_energy(h)..

          N_STO_E  - STO_L(h) =G= 0
;

* Storage maximum charging capacity (power in)
stoin_max_power(h)..

           N_STO_P - STO_IN(h) =G= 0
;

* Storage maximum discharging capacity (power out)
stoout_max_power(h)..

          N_STO_P  - STO_OUT(h) =G= 0
;


KKTNPV..

  c_i_pv - sum( h, mu(h)*avail_solar(h)) + gamma1 =G= 0

;
KKTNSTOE..

  c_i_sto_e  - sum(h, gamma2(h)) =G= 0

;
KKTNSTOP..

    c_i_sto_p  - sum(h, gamma3(h)) - sum(h, gamma4(h)) =G= 0

;

KKTEB(h)..

         price_buy - lambda(h) =G= 0

;

KKTES(h)..

           -price_market(h)   + mu(h) =G=  0

;

KKTG(h)..

      - lambda(h) + mu(h) =G= 0


;

KKTCU(h)..

      + mu(h) =G= 0
;

KKTSTOIN(h)..

         c_var_sto + mu(h) + gamma3(h) - levelsto(h)*eta_sto_in =G= 0

;
KKTSTOUT(h)..

          c_var_sto -lambda(h) + gamma4(h) + levelsto(h)/eta_sto_out =G= 0


;

KKTSTOLEV(h)..


     gamma2(h) + levelsto(h) - levelsto(h++1) =G= 0

;



***************************** Initialize model *********************************
Model prosumodmcp /

hh_energy_balance.lambda
pv_generation.mu
pv_install_max.gamma1
stolevel.levelsto
stolev_max_energy.gamma2
stoin_max_power.gamma3
stoout_max_power.gamma4
KKTNPV.N_PV
KKTNSTOE.N_STO_E
KKTNSTOP.N_STO_P
KKTEB.E_buy
KKTES.E_sell
KKTG.G_PV
KKTSTOIN.STO_IN
KKTSTOUT.STO_OUT
KKTSTOLEV.STO_L
KKTCU.CU
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
*PROFILETOL = 10

;

$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
epagap 10
parallelmode -1
$offecho

* Set up initial values for speed up
STO_L.l(h) = 4.367
;
N_PV.l       = 8    ;
N_STO_E.l    = 10   ;
N_STO_P.l    = 1.5  ;

solve prosumodmcp using mcp;

display d , N_PV.l , E_buy.l , E_sell.l ,  G_PV.l,
        price_market, hh_energy_balance.m,
        pv_install_max.m, pv_generation.m, N_STO_E.l, N_STO_P.l


***************************** Set up reporting *********************************


