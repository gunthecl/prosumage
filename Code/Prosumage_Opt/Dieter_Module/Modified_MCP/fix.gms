
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
**** Exogenous EV  *************************************************************
********************************************************************************



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

*F.fx(l,h)$(m_ntc(l) = 0) = 0 ;
*NTC.fx(l)$(m_ntc(l) = 0) = 0 ;

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


%prosumage%$ontext
CU_PRO.fx(res,h)$(m_res_pro(res) = 0) = 0 ;
G_MARKET_PRO2M.fx(res,h)$( m_res_pro(res) = 0) = 0 ;
*G_MARKET_M2PRO.fx(h)$(feat_node('prosumage',n) = 0) = 0 ;
G_RES_PRO.fx(res,h)$( m_res_pro(res) = 0) = 0 ;
STO_IN_PRO2PRO.fx(res,sto,h)$( m_sto_pro_p(sto) = 0) = 0 ;
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



********************************************************************************
**** DEFAULT LEVELS FOR REPORT PARAMETERS  *************************************
********************************************************************************

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
*lev_NTC(superscen,l) = 0 ;
*lev_F(superscen,l,h) = 0 ;
*lev_RSVR_OUT(superscen,rsvr,h) = 0 ;
*lev_RSVR_L(superscen,rsvr,h) = 0 ;
*lev_N_RSVR_E(superscen,rsvr) = 0 ;
*lev_N_RSVR_P(superscen,rsvr) = 0 ;
*lev_G_INFES(superscen,h) = 0 ;


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
$ontext
%ror_parameter%%ror_variable%lev_N_TECH(scen'ror') = 0 ;
%ror_parameter%%ror_variable%lev_G_RES(scen'ror',h) = 0 ;
%reserves%$ontext
*%ror_parameter%%ror_variable%lev_RP_CON(scenreserves,'ror',h) = 0 ;
$ontext
$offtext






