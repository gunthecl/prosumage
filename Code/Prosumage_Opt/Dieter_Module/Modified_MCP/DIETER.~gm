
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

* Set star to skip Excel upload and load data from gdx
$setglobal skip_Excel "*"


* Choose base year
$setglobal base_year "'2030'"

* Germany only - also adjust Excel inputs!
$setglobal GER_only "*"

* Set star to activate options

$setglobal prosumage ""

* Set star to select run-of-river options either as exogenous parameter or as endogenous variable including reserve provision:
* if nothing is selected, ROR capacity will be set to zero
* if parameter option is selected, set appropriate values in fix.gmx
* if variable option is selected, set appropriate bound in data_input excel
$setglobal ror_parameter "*"
$setglobal ror_variable ""

* Set star to determine loops, otherwise default 100% renewables
$setglobal loop_over_renewable_share "*"

* Set star for no crossover to speed up calculation time by skipping crossover in LP solver
$setglobal no_crossover ""

* Set star for reporting to Excel
$setglobal report_to_excel ""


* ----------------- Select if to use MCP or LP format --------------------------

* Set to "*" to select linear program, leave blank to select MCP
$setglobal LP "*"

* Do not change these two lines
$if "%LP%" == "" $setglobal MCP "*"
$if "%LP%" == "*" $setglobal MCP ""

********************************************************************************

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal sec_hour "1"

* Sanity checks
$if "%ror_parameter%" == "*" $if "%ror_variable%" == "*" $abort Choose appropriate ROR option! ;

********************************************************************************

****************************
***** INITIALIZE LOOPS *****
****************************

sets
%loop_over_renewable_share%$ontext
loop_res_share   Solution loop for different shares of renewables       /50/
$ontext
$offtext

%prosumage%$ontext
loop_prosumage   Solution loop for different prosumer self-consumption levels    /50/
$ontext
$offtext

%loop_over_renewable_share%      loop_res_share                          /50/
%prosumage%                      loop_prosumage                          /0/
;




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
d(h) = d_y(%base_year%,h) ;
phi_res(res,h) = phi_res_y(%base_year%,res,h) ;
phi_min_res = 0 ;
phi_pro_self = 0 ;
phi_pro_load = 0 ;

%prosumage%$ontext
phi_pro_load = 0.2 ;
$ontext
$offtext


Parameter
phi_min_res_exog;
phi_min_res_exog = 1 ;

Sets
h_ini(h)                       First hour  /h1/
*h_end(h)                       Last hour   /h8760/
;

********************************************************************************
***** Model *****
********************************************************************************

$include model.gms

********************************************************************************
***** Options, fixings, report preparation *****
********************************************************************************

* Solver options
$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
$offecho

%no_crossover%$ontext
$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
barcrossalg -1
barepcomp 1e-8
$offecho
$ontext
$offtext

dieter.OptFile = 1;
dieter.holdFixed = 1 ;


********************************************************************************
***** Solve *****
********************************************************************************

* Preparation of GUSS tool for scenario analysis
phi_min_res = eps ;
phi_pro_self = eps ;

$eval superscencount 1000

Set
modelstats       model stats collection                  /modelstat, solvestat, resusd/
superscen        Scenarios                               /scen1*scen%superscencount%/
map(superscen,loop_res_share,loop_prosumage)    /#superscen:(#loop_res_share.#loop_prosumage)/
;

set
scen(superscen);
scen(superscen) = yes$( sum((loop_res_share,loop_prosumage) , map(superscen,loop_res_share,loop_prosumage)) )    ;

Parameters
gussoptions                              /Logoption 2, Optfile 1, Skipbasecase 1/
modstats(superscen, modelstats)
min_res
pro_selfcon
;

min_res(scen)     = sum( (loop_res_share,loop_prosumage)$map(scen,loop_res_share,loop_prosumage) , loop_res_share.val/100 ) ;
pro_selfcon(scen) = sum( (loop_res_share,loop_prosumage)$map(scen,loop_res_share,loop_prosumage) , loop_prosumage.val/100 ) ;

Parameters
* Equations
marginal_con5a(superscen)
marginal_con1a(superscen,*)
marginal_con9a(superscen,*,*,*)
marginal_con9b(superscen,*,*,*)

* Basic
lev_Z(superscen)
lev_G_L(superscen,tech,h)
lev_G_UP(superscen,tech,h)
lev_G_DO(superscen,tech,h)
lev_G_RES(superscen,tech,h)
lev_CU(superscen,tech,h)
lev_STO_IN(superscen,sto,h)
lev_STO_OUT(superscen,sto,h)
lev_STO_L(superscen,sto,h)
lev_N_TECH(superscen,tech)
lev_N_STO_E(superscen,sto)
lev_N_STO_P(superscen,sto)


* Prosumage
lev_CU_PRO(superscen,tech,h)
lev_G_MARKET_PRO2M(superscen,tech,h)
lev_G_MARKET_M2PRO(superscen,h)
lev_G_RES_PRO(superscen,tech,h)
lev_STO_IN_PRO2PRO(superscen,tech,sto,h)
lev_STO_IN_PRO2M(superscen,tech,sto,h)
lev_STO_IN_M2PRO(superscen,sto,h)
lev_STO_IN_M2M(superscen,sto,h)
lev_STO_OUT_PRO2PRO(superscen,sto,h)
lev_STO_OUT_PRO2M(superscen,sto,h)
lev_STO_OUT_M2PRO(superscen,sto,h)
lev_STO_OUT_M2M(superscen,sto,h)
lev_STO_L_PRO2PRO(superscen,sto,h)
lev_STO_L_PRO2M(superscen,sto,h)
lev_STO_L_M2PRO(superscen,sto,h)
lev_STO_L_M2M(superscen,sto,h)
lev_N_STO_E_PRO(superscen,sto)
lev_N_STO_P_PRO(superscen,sto)
lev_STO_L_PRO(superscen,sto,h)
lev_N_RES_PRO(superscen,tech)
;


* Inclusion of scenario and fixing
$include fix.gms
$include scenario.gms


* Definition of dictionary set for GUSS tool

Set dict(*,*,*) /
scen             .scenario       .''
gussoptions      .opt            .modstats

phi_min_res      .param          .min_res

%prosumage%$ontext
phi_pro_self     .param          .pro_selfcon
$ontext
$offtext


con5a_minRES     .marginal       .marginal_con5a
con1a_bal        .marginal       .marginal_con1a


%LP%$ontext
Z                .level          .lev_Z
$ontext
$offtext
G_L              .level          .lev_G_L
G_UP             .level          .lev_G_UP
G_DO             .level          .lev_G_DO
G_RES            .level          .lev_G_RES
CU               .level          .lev_CU
STO_IN           .level          .lev_STO_IN
STO_OUT          .level          .lev_STO_OUT
STO_L            .level          .lev_STO_L
N_TECH           .level          .lev_N_TECH
N_STO_E          .level          .lev_N_STO_E
N_STO_P          .level          .lev_N_STO_P


%prosumage%$ontext
CU_PRO           .level          .lev_CU_PRO
G_MARKET_PRO2M   .level          .lev_G_MARKET_PRO2M
G_MARKET_M2PRO   .level          .lev_G_MARKET_M2PRO
G_RES_PRO        .level          .lev_G_RES_PRO
STO_IN_PRO2PRO   .level          .lev_STO_IN_PRO2PRO
STO_OUT_PRO2PRO  .level          .lev_STO_OUT_PRO2PRO
STO_L_PRO2PRO    .level          .lev_STO_L_PRO2PRO
N_STO_E_PRO      .level          .lev_N_STO_E_PRO
N_STO_P_PRO      .level          .lev_N_STO_P_PRO
N_RES_PRO        .level          .lev_N_RES_PRO
$ontext
$offtext
/
;



%LP%$ontext
solve  DIETER using lp min Z scenario dict;



* Reporting
$include report.gms


execute_unload "results", report, report_tech, report_node,
*report_line,
report_tech_hours, report_hours, report_cost
%prosumage%$ontext
, report_prosumage, report_prosumage_tech, report_prosumage_tech_hours, report_market, report_market_tech, report_market_tech_hours
$ontext
$offtext
;


%report_to_excel%$ontext
$include report_to_excel.gms
$ontext
$offtext


$ontext
$offtext

%MCP%$ontext
solve   DIETER_MCP using mcp ;
*scenario dict;


Parameters
Z_MCP
;

Z_MCP =            sum( (h,dis) , c_m(dis)*G_L.l(dis,h) )
                 + sum( (h,dis)$(ord(h)>1) , c_up(dis)*G_UP.l(dis,h) )
                 + sum( (h,dis) , c_do(dis)*G_DO.l(dis,h) )
                 + sum( (h,nondis) , c_cu(nondis)*CU.l(nondis,h) )
                 + sum( (h,sto) , c_m_sto(sto) * ( STO_OUT.l(sto,h) + STO_IN.l(sto,h) ) )
                 + sum( tech , c_i(tech)*N_TECH.l(tech) )
                 + sum( tech , c_fix(tech)*N_TECH.l(tech) )
                 + sum( sto , c_i_sto_e(sto)*N_STO_E.l(sto) )
                 + sum( sto , c_fix_sto(sto)/2*(N_STO_P.l(sto)+ N_STO_E.l(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P.l(sto) )
;


Display
Z_MCP,
N_TECH.l,
N_STO_E.l,
N_STO_P.l,
G_L.l,
G_UP.l,
G_DO.l,
G_RES.l,
CU.l,
STO_IN.l,
STO_OUT.l,
STO_L.l
lambda_enerbal.m,
lambda_enerbal.l,
lambda_resgen.m,
lambda_convgen.m,
lambda_stolev.m
;

$ontext
$offtext

* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
