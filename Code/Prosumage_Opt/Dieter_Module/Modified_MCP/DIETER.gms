
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
$setglobal DSM ""

$setglobal reserves_endogenous ""
$setglobal reserves_exogenous ""

$setglobal prosumage ""

$setglobal heat ""

$setglobal EV ""
$setglobal EV_EXOG ""
* Set star to indicate renewables constraint on electric vehicles - DEFAULT is same quota as for the rest of the electricity system
$setglobal EV_DEFAULT ""
$setglobal EV_100RES ""
$setglobal EV_FREE ""

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


********************************************************************************

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal sec_hour "1"

$setglobal reserves "%reserves_endogenous%%reserves_exogenous%"

* Sanity checks
$if "%ror_parameter%" == "*" $if "%ror_variable%" == "*" $abort Choose appropriate ROR option! ;

$if "%reserves%" == "**" $abort Choose only one reserve option (endogenous or exogenous)!

$if "%EV%" == "" $if "%EV_EXOG%" == "*" $abort Switch on EV! ;

$if "%EV%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "" $abort Choose exactly one EV option! ;
$if "%EV%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "**" $abort Choose exactly one EV option! ;
$if "%EV%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "***" $abort Choose exactly one EV option! ;

$if "%EV_EXOG%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "" $abort Choose exactly one EV option! ;
$if "%EV_EXOG%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "**" $abort Choose exactly one EV option! ;
$if "%EV_EXOG%" == "*" $if "%EV_DEFAULT%%EV_100RES%%EV_FREE%" == "***" $abort Choose exactly one EV option! ;




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
%EV%                             loop_ev                                 /0/
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

*************************************
***** Features for single nodes *****
*************************************


*Set
*features /dsm, ev, reserves, prosumage, rsvr_outflow, heat/
*;


*Table
*feat_node(features,n)
*                 DE

*%prosumage%$ontext
*prosumage        1
*$ontext
*$offtext
*rsvr_outflow     0

*;



*%prosumage%$ontext
*m_res_pro(res)$(feat_node('prosumage',n) = 0) = 0 ;
*m_sto_pro_e(n,sto)$(feat_node('prosumage',n) = 0) = 0 ;
*m_sto_pro_p(n,sto)$(feat_node('prosumage',n) = 0) = 0 ;
*$ontext
*$offtext

*phi_rsvr_min(n) = 0
*feat_node('rsvr_outflow',n)
*;




Set
map_n_tech(tech)
map_n_sto(sto)
*map_n_rsvr(n,rsvr)
*map_n_dsm(n,dsm)
*map_n_ev(n,ev)
*map_l(l)
map_n_sto_pro(sto)
map_n_res_pro(tech)
;

map_n_tech(tech) = yes$m_p(tech) ;
map_n_sto(sto) = yes$m_sto_p(sto) ;
*map_n_rsvr(n,rsvr) = yes$m_rsvr_p(n,rsvr) ;
*map_n_dsm(n,dsm_curt) = yes$m_dsm_cu(n,dsm_curt) ;
*map_n_dsm(n,dsm_shift) = yes$m_dsm_shift(n,dsm_shift) ;
*map_n_ev(n,ev) = yes$ev_data(n,ev,'share_ev') ;
*map_l(l) = yes$m_ntc(l) ;
map_n_sto_pro(sto) = yes$(yes$m_sto_pro_p(sto)) ;
map_n_res_pro(res) = yes$(yes$m_res_pro(res)) ;
;




********************************************************************************

***************************
***** Initialize data *****
***************************

* Parameters for default base year
d(h) = d_y(%base_year%,h) ;
phi_res(res,h) = phi_res_y(%base_year%,res,h) ;
*rsvr_in(n,rsvr,h) = rsvr_in_y(n,%base_year%,rsvr,h) ;
*phi_reserves_call(n,reserves,h) = phi_reserves_call_y(n,%base_year%,reserves,h) ;
*phi_mean_reserves_call(n,reserves) = phi_mean_reserves_call_y(n,%base_year%,reserves) ;
*reserves_exogenous(n,reserves,h) = reserves_exogenous_y(n,%base_year%,reserves,h) ;
*dh(n,bu,ch,h) = dh_y(n,%base_year%,bu,ch,h) ;
*d_dhw(n,bu,ch,h) = d_dhw_y(n,%base_year%,bu,ch,h) ;

*dh(n,bu,ch,h) = area_floor(n,bu,ch) * dh(n,bu,ch,h) ;
*d_dhw(n,bu,ch,h) = area_floor(n,bu,ch) * d_dhw(n,bu,ch,h) ;


* No interconnections between non-adjacent or nonuploaded nodes
*m_ntc(l)$( smax(n,inc(l,n)) = 0 OR smin(n,inc(l,n)) = 0 ) = 0 ;


* Set loop parameters to default zero
phi_min_res = 0 ;
ev_quant = 0 ;
phi_pro_self = 0 ;
phi_pro_load = 0 ;

%prosumage%$ontext
phi_pro_load = 0.2 ;
$ontext
$offtext


Parameter
phi_min_res_exog
*min_flh(n,rsvr)
;

phi_min_res_exog = 1 ;
*min_flh(n,rsvr) = 0 ;



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
ev_quant = eps ;
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
*number_ev
pro_selfcon
;

min_res(scen) = sum( (loop_res_share,loop_prosumage)$map(scen,loop_res_share,loop_prosumage) , loop_res_share.val/100 ) ;
*number_ev(scen) = sum( (loop_res_share,loop_prosumage)$map(scen,loop_res_share,loop_prosumage) , loop_ev.val ) ;
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

Z                .level          .lev_Z
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
STO_IN_PRO2M     .level          .lev_STO_IN_PRO2M
STO_IN_M2PRO     .level          .lev_STO_IN_M2PRO
STO_IN_M2M       .level          .lev_STO_IN_M2M
STO_OUT_PRO2PRO  .level          .lev_STO_OUT_PRO2PRO
STO_OUT_PRO2M    .level          .lev_STO_OUT_PRO2M
STO_OUT_M2PRO    .level          .lev_STO_OUT_M2PRO
STO_OUT_M2M      .level          .lev_STO_OUT_M2M
STO_L_PRO        .level          .lev_STO_L_PRO
STO_L_PRO2PRO    .level          .lev_STO_L_PRO2PRO
STO_L_PRO2M      .level          .lev_STO_L_PRO2M
STO_L_M2PRO      .level          .lev_STO_L_M2PRO
STO_L_M2M        .level          .lev_STO_L_M2M
N_STO_E_PRO      .level          .lev_N_STO_E_PRO
N_STO_P_PRO      .level          .lev_N_STO_P_PRO
N_RES_PRO        .level          .lev_N_RES_PRO
$ontext
$offtext
/
;

solve DIETER using lp min Z scenario dict;


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



* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
