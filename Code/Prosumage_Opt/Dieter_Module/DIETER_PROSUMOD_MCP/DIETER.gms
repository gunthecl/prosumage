
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

* Dispatch only - used fixed capacities (also deactivates bio energy restriction)
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
$setglobal report_to_excel ""


* ----------------- Select if to use MCP or LP format --------------------------

* Set to "*" to select linear program, leave blank to select MCP
$setglobal LP ""


********************************************************************************

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal sec_hour "1"

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

$include dataload.gms


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
numb_pro_load = 5000 ;
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


Parameter
phi_min_res_exog     ;
phi_min_res_exog = 1 ;



********************************************************************************
***** Model *****
********************************************************************************

$include model.gms

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
$if exist DIETER_p.gdx execute_loadpoint "DIETER_p";
option limrow = 10, limcol = 10, solprint = on ;

solve  DIETER using lp min Z ;
$ontext
$offtext

%MCP%$ontext

* Save and load solution points
DIETER_MCP.savepoint=1;
*$if exist DIETER_p.gdx execute_loadpoint "DIETER_p";
$if exist DIETER_MCP_p.gdx execute_loadpoint "DIETER_MCP_p";
option limrow = 10, limcol = 10, solprint = on ;
DIETER_MCP.optfile= 1;

solve   DIETER_MCP using mcp;
$ontext
$offtext

* Reporting
$include report.gms

execute_unload "results", report, report_tech, report_node,
report_tech_hours, report_hours, report_cost

%prosumage%$ontext
, report_prosumage, report_prosumage_tech, report_prosumage_tech_hours, report_market, report_market_tech, report_market_tech_hours
$ontext
$offtext
;

%report_to_excel%$ontext
execute "gdxxrw i=results.gdx o=results.xlsx @results.tmp squeeze=N";
$ontext
$offtext


* ---------------------------------------------------------------------------- *
