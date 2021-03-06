
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



********************************************************************************
**** Run of river  *************************************************************
********************************************************************************

* If $setglobal ror_parameter "*": choose appropriate value for N_CON.fx('ror') (but no reserve provision):

%ror_parameter%$ontext
N_TECH.fx('ror') = 5857.47 ;
G_RES.fx('ror',h) = N_TECH.l('ror') * phi_res('ror',h) ;

$ontext
$offtext

* Fix everything to zero in case no ROR option is selected:
%ror_parameter%%ror_variable%N_TECH.fx('ror') = 0 ;
%ror_parameter%%ror_variable%G_RES.fx('ror',h) = 0 ;
*%ror_parameter%%ror_variable%RP_NONDIS.fx('ror',h) = 0 ;


********************************************************************************
**** No storage and DSM in first period  ***************************************
********************************************************************************

** No storage inflow in first period **
STO_IN.fx(sto,h)$(ord(h) = 1) = 0;



********************************************************************************
**** No primary reserves by heating devices  ***********************************
********************************************************************************


********************************************************************************
**** Fixing to reduce model size  **********************************************
********************************************************************************

G_L.fx(tech,h)$(m_p(tech) = 0) = 0 ;
G_UP.fx(tech,h)$(m_p(tech) = 0) = 0 ;
G_DO.fx(tech,h)$(m_p(tech) = 0) = 0 ;
N_TECH.fx(tech)$(m_p(tech) = 0) = 0 ;
G_RES.fx(tech,h)$(m_p(tech) = 0) = 0 ;
CU.fx(tech,h)$(m_p(tech) = 0) = 0 ;

N_STO_P.fx(sto)$(m_sto_p(sto) = 0) = 0 ;
N_STO_E.fx(sto)$(m_sto_e(sto) = 0) = 0 ;
STO_IN.fx(sto,h)$(m_sto_p(sto) = 0) = 0 ;
STO_OUT.fx(sto,h)$(m_sto_p(sto) = 0) = 0 ;
STO_L.fx(sto,h)$(m_sto_p(sto) = 0) = 0 ;


Variables
STO_IN_PRO2M(tech,sto,h)
STO_IN_M2PRO(sto,h)
STO_IN_M2M(sto,h)
STO_OUT_PRO2M(sto,h)
STO_OUT_M2PRO(sto,h)
STO_OUT_M2M(sto,h)
STO_L_PRO(sto,h)
STO_L_PRO2M(sto,h)
STO_L_M2PRO(sto,h)
STO_L_M2M(sto,h)
;

%prosumage%$ontext
CU_PRO.fx(res,h)$(m_res_pro(res) = 0) = 0 ;
G_MARKET_PRO2M.fx(res,h)$( m_res_pro(res) = 0) = 0 ;
*G_MARKET_M2PRO.fx(h)$(feat_node('prosumage',n) = 0) = 0 ;
G_RES_PRO.fx(res,h)$( m_res_pro(res) = 0) = 0 ;
STO_IN_PRO2PRO.fx(sto,res,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_IN_PRO2M.fx(res,sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_IN_M2PRO.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_IN_M2M.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_OUT_PRO2PRO.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_OUT_PRO2M.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_OUT_M2PRO.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_OUT_M2M.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_L_PRO2PRO.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_L_PRO2M.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_L_M2PRO.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_L_M2M.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
N_STO_E_PRO.fx(sto)$( m_sto_pro_p(sto) = 0) = 0 ;
N_STO_P_PRO.fx(sto)$( m_sto_pro_p(sto) = 0) = 0 ;
STO_L_PRO.fx(sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
N_RES_PRO.fx(res)$( m_res_pro(res) = 0) = 0 ;
$ontext
$offtext

* Deactivated market storage links

STO_IN_PRO2M.fx(tech,sto,h) = 0 ;
STO_IN_M2PRO.fx(sto,h)      = 0 ;
STO_IN_M2M.fx(sto,h)        = 0 ;
STO_OUT_PRO2M.fx(sto,h)     = 0 ;
STO_OUT_M2PRO.fx(sto,h)     = 0 ;
STO_OUT_M2M.fx(sto,h)       = 0 ;
STO_L_PRO.fx(sto,h)         = 0 ;
STO_L_PRO2M.fx(sto,h)       = 0 ;
STO_L_M2PRO.fx(sto,h)       = 0 ;
STO_L_M2M.fx(sto,h)         = 0 ;

********************************************************************************
**** DEFAULT LEVELS FOR REPORT PARAMETERS  *************************************
********************************************************************************

* Reporting level
Set
loop_res_share  /0/
loop_prosumage  /0/ ;

$eval superscencount 1000

Set
modstats       model stats collection                  /modelstat, solvestat, resusd/
superscen        Scenarios                               /scen1*scen%superscencount%/
map(superscen,loop_res_share,loop_prosumage)    /#superscen:(#loop_res_share.#loop_prosumage)/
;

set
scen(superscen);
scen(superscen) = yes$( sum((loop_res_share,loop_prosumage) , map(superscen,loop_res_share,loop_prosumage)) ) ;


Parameters
* Equations
marginal_con5a(superscen)
marginal_con1a(superscen,h)
*marginal_con9a(superscen,*,*,*)
*marginal_con9b(superscen,*,*,*)

* Basic
modelstat
solvestat
resusd
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

* Default level zero for report parameters
lev_Z(scen) = 0 ;
lev_G_L(scen,tech,h) = 0 ;
lev_G_UP(scen,tech,h) = 0 ;
lev_G_DO(scen,tech,h) = 0 ;
lev_G_RES(scen,tech,h) = 0 ;
lev_CU(scen,tech,h) = 0 ;
lev_STO_IN(scen,sto,h) = 0 ;
lev_STO_OUT(scen,sto,h) = 0 ;
lev_STO_L(scen,sto,h) = 0 ;
lev_N_TECH(scen,tech) = 0 ;
lev_N_STO_E(scen,sto) = 0 ;
lev_N_STO_P(scen,sto) = 0 ;
marginal_con1a(scen,h) = 0 ;
marginal_con5a(superscen) = 0 ;

* Prosumage
lev_CU_PRO(scen,tech,h) = 0 ;
lev_G_MARKET_PRO2M(scen,tech,h) = 0  ;
lev_G_MARKET_M2PRO(scen,h) = 0  ;
lev_G_RES_PRO(scen,tech,h) = 0  ;
lev_STO_IN_PRO2PRO(scen,tech,sto,h) = 0  ;
lev_STO_IN_PRO2M(scen,tech,sto,h) = 0  ;
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
lev_N_RES_PRO(scen,tech) = 0  ;


* Fixing of report parameters for run-of-river
%ror_parameter%$ontext
lev_N_TECH(scen,'ror') = 5857.47 ;
lev_G_RES(scen,'ror',h) = lev_N_TECH(scen,'ror') * phi_res('ror',h) ;
$ontext
$offtext








