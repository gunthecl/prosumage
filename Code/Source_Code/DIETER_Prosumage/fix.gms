
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.2.0, February 2017.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************


*************************************************************
**** Fixes unneccessary or inadequate variables to zero  ****
*************************************************************

** Run-of-river **
* If $setglobal ror_parameter "*": choose appropriate value for N_CON.fx('ror') (but no reserve provision):
%ror_variable%N_CON.fx('ror') = 4200 ;
%ror_variable%G_L.fx('ror',h) = N_CON.l('ror')*phi_ror(h) ;
%ror_variable%RP_CON.fx(reserves,'ror',h) = 0 ;

* Fix everything to zero in case no ROR option is selected:
%ror_parameter%%ror_variable%N_CON.fx('ror') = 0 ;
%ror_parameter%%ror_variable%G_L.fx('ror',h) = 0 ;
%ror_parameter%%ror_variable%G_UP.fx('ror',h) = 0 ;
%ror_parameter%%ror_variable%G_DO.fx('ror',h) = 0 ;
%ror_parameter%%ror_variable%RP_CON.fx(reserves,'ror',h) = 0 ;


** Exogenous EV **
%EV%$ontext
%EV_EXOG%$ontext
EV_DISCHARGE.fx(ev,h) = 0 ;
RP_EV_G2V.fx(reserves,ev,h) = 0 ;
RP_EV_V2G.fx(reserves,ev,h) = 0 ;
$ontext
$offtext


** No nuclear **
G_L.fx('nuc',h) = 0 ;
G_UP.fx('nuc',h) = 0 ;
G_DO.fx('nuc',h) = 0 ;
RP_CON.fx(reserves,'nuc',h) = 0 ;
N_CON.fx('nuc') = 0 ;


** No storage types 2, 3, 4, 6 and 7 by default **
N_STO_P.fx('Sto2') = 0 ;
N_STO_E.fx('Sto2') = 0 ;
STO_IN.fx('Sto2',h) = 0 ;
STO_OUT.fx('Sto2',h) = 0 ;
STO_L.fx('Sto2',h) = 0 ;
RP_STO_IN.fx(reserves,'Sto2',h) = 0 ;
RP_STO_OUT.fx(reserves,'Sto2',h) = 0 ;

N_STO_P.fx('Sto3') = 0 ;
N_STO_E.fx('Sto3') = 0 ;
STO_IN.fx('Sto3',h) = 0 ;
STO_OUT.fx('Sto3',h) = 0 ;
STO_L.fx('Sto3',h) = 0 ;
RP_STO_IN.fx(reserves,'Sto3',h) = 0 ;
RP_STO_OUT.fx(reserves,'Sto3',h) = 0 ;

N_STO_P.fx('Sto4') = 0 ;
N_STO_E.fx('Sto4') = 0 ;
STO_IN.fx('Sto4',h) = 0 ;
STO_OUT.fx('Sto4',h) = 0 ;
STO_L.fx('Sto4',h) = 0 ;
RP_STO_IN.fx(reserves,'Sto4',h) = 0 ;
RP_STO_OUT.fx(reserves,'Sto4',h) = 0 ;

N_STO_P.fx('Sto6') = 0 ;
N_STO_E.fx('Sto6') = 0 ;
STO_IN.fx('Sto6',h) = 0 ;
STO_OUT.fx('Sto6',h) = 0 ;
STO_L.fx('Sto6',h) = 0 ;
RP_STO_IN.fx(reserves,'Sto6',h) = 0 ;
RP_STO_OUT.fx(reserves,'Sto6',h) = 0 ;


*Prosumers can only use li-ion batteries by default
%prosumage%$ontext
N_STO_P_PRO.fx(sto) = 0 ;
N_STO_E_PRO.fx(sto) = 0 ;
STO_IN_PRO2PRO.fx(res,sto,h) = 0 ;
STO_IN_M2PRO.fx(sto,h) = 0 ;
STO_OUT_PRO2PRO.fx(sto,h) = 0 ;
STO_OUT_PRO2M.fx(sto,h) = 0 ;
STO_L_PRO.fx(sto,h) = 0 ;

N_STO_P_PRO.lo('sto1') = 0 ;
N_STO_E_PRO.lo('sto1') = 0 ;
STO_IN_PRO2PRO.lo(res,'sto1',h) = 0 ;
STO_IN_M2PRO.lo('sto1',h) = 0 ;
STO_OUT_PRO2PRO.lo('sto1',h) = 0 ;
STO_OUT_PRO2M.lo('sto1',h) = 0 ;
STO_L_PRO.lo('sto1',h) = 0 ;

N_STO_P_PRO.up('sto1') = inf ;
N_STO_E_PRO.up('sto1') = inf ;
STO_IN_PRO2PRO.up(res,'sto1',h) = inf ;
STO_IN_M2PRO.up('sto1',h) = inf ;
STO_OUT_PRO2PRO.up('sto1',h) = inf ;
STO_OUT_PRO2M.up('sto1',h) = inf ;
STO_L_PRO.up('sto1',h) = inf ;
$ontext
$offtext

** Default share of prosumage load
phi_pro_load = 0 ;

** E to P ratio of PHS free by default **
etop_max('Sto5') = 1000 ;

** No storage inflow in first period **
STO_IN.fx(sto,'h1') = 0;


%DSM%$ontext
** No DSM load shifting in the first period **
         DSM_UP.fx(dsm_shift,'h1') = 0;
         DSM_DO.fx(dsm_shift,'h1',hh) = 0 ;
         DSM_DO.fx(dsm_shift,h,'h1') = 0 ;
         DSM_UP_DEMAND.fx(dsm_shift,'h1') = 0 ;
         DSM_DO_DEMAND.fx(dsm_shift,'h1') = 0 ;

** No reserves provision by DSM in first period **
         RP_DSM_SHIFT.fx(reserves,dsm_shift,'h1') = 0;
         RP_DSM_CU.fx('SR_up',dsm_curt,'h1') = 0 ;
         RP_DSM_CU.fx('MR_up',dsm_curt,'h1') = 0 ;

** No provision of PR and negative reserves by DSM load curtailment **
         RP_DSM_CU.fx('PR_up',dsm_curt,h) = 0 ;
         RP_DSM_CU.fx('PR_do',dsm_curt,h) = 0 ;
         RP_DSM_CU.fx('SR_do',dsm_curt,h) = 0 ;
         RP_DSM_CU.fx('MR_do',dsm_curt,h) = 0 ;

** No provision of PR by DSM load shifting **
         RP_DSM_SHIFT.fx('PR_up',dsm_shift,h) = 0 ;
         RP_DSM_SHIFT.fx('PR_do',dsm_shift,h) = 0 ;
$ontext
$offtext


* Default level zero for report parameters
lev_Z(scen) = 0 ;
lev_G_L(scen,ct,h) = 0 ;
lev_G_UP(scen,ct,h) = 0 ;
lev_G_DO(scen,ct,h) = 0 ;
lev_G_RES(scen,res,h) = 0 ;
lev_CU(scen,res,h) = 0 ;
lev_STO_IN(scen,sto,h) = 0 ;
lev_STO_OUT(scen,sto,h) = 0 ;
lev_STO_L(scen,sto,h) = 0 ;
lev_N_CON(scen,ct) = 0 ;
lev_N_RES(scen,res) = 0 ;
lev_N_STO_E(scen,sto) = 0 ;
lev_N_STO_P(scen,sto) = 0 ;

%EV%$ontext
lev_EV_CHARGE(scen,ev,h) = 0 ;
lev_EV_DISCHARGE(scen,ev,h) = 0 ;
lev_EV_L(scen,ev,h) = 0 ;
lev_EV_PHEVFUEL(scen,ev,h) = 0 ;
lev_EV_GED(scen,ev,h) = 0 ;
$ontext
$offtext

%DSM%$ontext
lev_DSM_CU(scen,dsm_curt,h) = 0 ;
lev_DSM_UP(scen,dsm_shift,h) = 0 ;
lev_DSM_DO(scen,dsm_shift,h,hh) = 0 ;
lev_DSM_UP_DEMAND(scen,dsm_shift,h) = 0 ;
lev_DSM_DO_DEMAND(scen,dsm_shift,h) = 0 ;
lev_N_DSM_CU(scen,dsm_curt) = 0 ;
lev_N_DSM_SHIFT(scen,dsm_shift) = 0 ;
$ontext
$offtext

%reserves%$ontext
lev_RP_CON(scen,reserves,ct,h) = 0 ;
lev_RP_RES(scen,reserves,res,h) = 0 ;
lev_RP_STO_IN(scen,reserves,sto,h) = 0 ;
lev_RP_STO_OUT(scen,reserves,sto,h) = 0 ;
$ontext
$offtext

%EV%$ontext
%reserves%$ontext
lev_RP_EV_V2G(scen,reserves,ev,h) = 0 ;
lev_RP_EV_G2V(scen,reserves,ev,h) = 0 ;
$ontext
$offtext

%DSM%$ontext
%reserves%$ontext
lev_RP_DSM_CU(scen,reserves,dsm_curt,h) = 0 ;
lev_RP_DSM_SHIFT(scen,reserves,dsm_shift,h) = 0 ;
$ontext
$ontext
$offtext

%prosumage%$ontext
lev_CU_PRO(scen,res,h) = 0 ;
lev_G_MARKET_PRO2M(scen,res,h) = 0  ;
lev_G_MARKET_M2PRO(scen,h) = 0  ;
lev_G_RES_PRO(scen,res,h) = 0  ;
lev_STO_IN_PRO2PRO(scen,res,sto,h) = 0  ;
lev_STO_IN_PRO2M(scen,res,sto,h) = 0  ;
lev_STO_IN_M2PRO(scen,sto,h) = 0   ;
lev_STO_IN_M2M(scen,sto,h) = 0  ;
lev_STO_OUT_PRO2PRO(scen,sto,h) = 0  ;
lev_STO_OUT_PRO2M(scen,sto,h) = 0  ;
lev_STO_OUT_M2PRO(scen,sto,h) = 0  ;
lev_STO_OUT_M2M(scen,sto,h) = 0  ;
lev_STO_L_PRO2PRO(scen,sto,h) = 0  ;
lev_STO_L_PRO2M(scen,sto,h) = 0  ;
lev_STO_L_M2PRO(scen,sto,h) = 0  ;
lev_STO_L_M2M(scen,sto,h) = 0  ;
lev_N_STO_E_PRO(scen,sto) = 0  ;
lev_N_STO_P_PRO(scen,sto) = 0  ;
lev_STO_L_PRO(scen,sto,h) = 0  ;
lev_N_RES_PRO(scen,res) = 0  ;
$ontext
$offtext


* Fixing of report parameters for run-of-river
%ror_variable%lev_N_CON(scen,'ror') = 4200 ;
%ror_variable%lev_G_L(scen,'ror',h) = lev_N_CON(scen,'ror')*phi_ror(h) ;
%reserves%$ontext
%ror_variable%lev_RP_CON(scen,reserves,'ror',h) = 0 ;
$ontext
$offtext
%ror_parameter%%ror_variable%lev_N_CON(scen,'ror') = 0 ;
%ror_parameter%%ror_variable%lev_G_L(scen,'ror',h) = 0 ;
%ror_parameter%%ror_variable%lev_G_UP(scen,'ror',h) = 0 ;
%ror_parameter%%ror_variable%lev_G_DO(scen,'ror',h) = 0 ;
%reserves%$ontext
%ror_parameter%%ror_variable%lev_RP_CON(scen,reserves,'ror',h) = 0 ;
$ontext
$offtext










