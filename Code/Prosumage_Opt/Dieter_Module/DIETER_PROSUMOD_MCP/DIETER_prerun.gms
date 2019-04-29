
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.3.0, October 2018.
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
***** Set star to activate options

* Set star to skip Excel upload and load data from gdx
$setglobal skip_Excel ""

* Choose base year
$setglobal base_year "'2030'"

* Germany only - also adjust Excel inputs!
$setglobal GER_only "*"

* Dispatch only - use fixed capacities (also deactivates bio energy restriction)
$setglobal dispatch_model "*"

* Select if you want to use load change costs
$setglobal load_change_costs ""

$setglobal prosumage "*"

* Select system-friendly prosumage or selfish prosumage
$setglobal prosumage_system_version ""

* Set star to select run-of-river options either as exogenous parameter or as endogenous variable including reserve provision:
* if nothing is selected, ROR capacity will be set to zero
* if parameter option is selected, set appropriate values in fix.gmx
$setglobal ror_parameter "*"

* Set star for reporting to Excel
$setglobal report_to_excel "*"

* Set star if only second hour is used

$setglobal second_hour "*"

* ----------------- Select if to use MCP or LP format --------------------------

* Set to "*" to select linear program, leave blank to select MCP
$setglobal LP ""


********************************************************************************

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)

* Sanity checks
$if "%ror_parameter%" == "*" $if "%ror_variable%" == "*" $abort Choose appropriate ROR option! ;

* Do not change these  lines
$if "%LP%" == ""  $setglobal MCP "*"
$if "%LP%" == "*" $setglobal MCP ""
$if "%LP%" == "*"  $if "%prosumage%" == "*" $if "%prosumage_system_version%" == "" $abort Switch on system version of LP or choose MCP format ;

$if "%dispatch_model%" == ""  $setglobal investment_model "*"
$if "%dispatch_model%" == "*" $setglobal investment_model ""


$if "%prosumage_system_version%" == ""  $setglobal selfish_prosumage "*"
$if "%prosumage_system_version%" == "*" $setglobal selfish_prosumage ""

$if "%second_hour%" == ""  $setglobal sec_hour "1"
$if "%second_hour%" == "*"  $setglobal sec_hour "2"
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
limrow = 0
limcol = 0
;

options
dispwidth = 15
limrow = 0
limcol = 0
solprint = off
sysout = off
;




********************************************************************************

**************************
***** Dataload *****
**************************

set h ;
set h_small(h) ;

$offorder

$include dataload.gms

* Define first subset of hours
h_small(h) = yes$(ord(h) < 1500) ;
display h_small ;

********************************************************************************

***************************
***** Initialize data *****
***************************

* Parameters for default base year
phi_min_res    = 0   ;
phi_pro_self   = 0.5 ;
numb_pro_load  = 0   ;


%prosumage%$ontext
* Number of prosumage households in thousands
numb_pro_load = 1000 ;
$ontext
$offtext

%prosumage_system_version%$ontext
$setglobal minimum_SC "*"

$ontext
$offtext

%selfish_prosumage%$ontext
$setglobal minimum_SC ""

$ontext
$offtext

* Load demand
d(h)           = d_y(%base_year%,h) - numb_pro_load*d_pro(h) ;
phi_res(res,h) = phi_res_y(%base_year%,res,h)  ;


Parameters
phi_min_res_exog
price_consumption_pro(h)
price_production_pro(h)
retail_price
FIT
SC_tax
alpha_max
max_solar_avail     ;
phi_min_res_exog = 1;


********************************************************************************
***** Tariff design scenario 2030 *****
********************************************************************************

$include tariff_scenario.gms

********************************************************************************
***** Model *****
********************************************************************************

$include model_prerun.gms

********************************************************************************
***** Options, fixings, report preparation *****

* Inclusion of scenario and fixing
$include fix.gms
$include scenario.gms

********************************************************************************
***** Solve *****
********************************************************************************


%LP%$ontext

* Solver options
$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
$offecho
dieter.OptFile = 1;
dieter.holdFixed = 1 ;

* Save and load solution points
DIETER.savepoint=1;

option limrow = 10, limcol = 10, solprint = on ;

solve  DIETER using lp min Z ;
$ontext
$offtext

%MCP%$ontext

* Save and load solution points
DIETER_MCP.savepoint=1;

option limrow = 10, limcol = 10, solprint = on ;

* Step 1: Solve first set of 1500 hours
solve   DIETER_MCP using mcp;

* Step 2: Loop for solving set of remaining sets of 1500 hours
scalar count /1/ ;
for (count = 1 to 2,
         h_small(h) = yes$(ord(h) >= 1500*count and ord(h) < 1500*(count+1));
         solve   DIETER_MCP using mcp;
     );

* Step 3: Solve entire set of hours
h_small(h) = yes

display h_small ;

DIETER_MCP.savepoint=1

solve   DIETER_MCP using mcp;

$ontext
$offtext

* ---------------------------------------------------------------------------- *
