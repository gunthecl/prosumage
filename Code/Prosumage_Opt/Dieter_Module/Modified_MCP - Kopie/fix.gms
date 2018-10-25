
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
N_TECH.fx(n,'ror') = 5857.47 ;
G_RES.fx(n,'ror',h) = N_TECH.l(n,'ror') * phi_res(n,'ror',h) ;
%reserves%$ontext
RP_NONDIS.fx(n,reserves,'ror',h) = 0 ;
$ontext
$offtext

* Fix everything to zero in case no ROR option is selected:
%ror_parameter%%ror_variable%N_TECH.fx(n,'ror') = 0 ;
%ror_parameter%%ror_variable%G_RES.fx(n,'ror',h) = 0 ;
%ror_parameter%%ror_variable%RP_NONDIS.fx(n,reserves,'ror',h) = 0 ;



********************************************************************************
**** Exogenous EV  *************************************************************
********************************************************************************



********************************************************************************
**** No storage and DSM in first period  ***************************************
********************************************************************************

** No storage inflow in first period **
STO_IN.fx(n,sto,h)$(ord(h) = 1) = 0;



********************************************************************************
**** No primary reserves by heating devices  ***********************************
********************************************************************************


********************************************************************************
**** Fixing to reduce model size  **********************************************
********************************************************************************

F.fx(l,h)$(m_ntc(l) = 0) = 0 ;
*NTC.fx(l)$(m_ntc(l) = 0) = 0 ;

G_L.fx(n,tech,h)$(m_p(n,tech) = 0) = 0 ;
G_UP.fx(n,tech,h)$(m_p(n,tech) = 0) = 0 ;
G_DO.fx(n,tech,h)$(m_p(n,tech) = 0) = 0 ;
N_TECH.fx(n,tech)$(m_p(n,tech) = 0) = 0 ;
G_RES.fx(n,tech,h)$(m_p(n,tech) = 0) = 0 ;
CU.fx(n,tech,h)$(m_p(n,tech) = 0) = 0 ;

N_STO_P.fx(n,sto)$(m_sto_p(n,sto) = 0) = 0 ;
N_STO_E.fx(n,sto)$(m_sto_e(n,sto) = 0) = 0 ;
STO_IN.fx(n,sto,h)$(m_sto_p(n,sto) = 0) = 0 ;
STO_OUT.fx(n,sto,h)$(m_sto_p(n,sto) = 0) = 0 ;
STO_L.fx(n,sto,h)$(m_sto_p(n,sto) = 0) = 0 ;


%prosumage%$ontext
CU_PRO.fx(n,res,h)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0) = 0 ;
G_MARKET_PRO2M.fx(n,res,h)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0) = 0 ;
G_MARKET_M2PRO.fx(n,h)$(feat_node('prosumage',n) = 0) = 0 ;
G_RES_PRO.fx(n,res,h)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0) = 0 ;
STO_IN_PRO2PRO.fx(n,res,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_IN_PRO2M.fx(n,res,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_IN_M2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_IN_M2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_OUT_PRO2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_OUT_PRO2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_OUT_M2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_OUT_M2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_L_PRO2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_L_PRO2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_L_M2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_L_M2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
N_STO_E_PRO.fx(n,sto)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
N_STO_P_PRO.fx(n,sto)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
STO_L_PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0) = 0 ;
N_RES_PRO.fx(n,res)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0) = 0 ;
$ontext
$offtext



********************************************************************************
**** DEFAULT LEVELS FOR REPORT PARAMETERS  *************************************
********************************************************************************

* Default level zero for report parameters
lev_Z(scen) = 0 ;
lev_G_L(scen,n,tech,h) = 0 ;
lev_G_UP(scen,n,tech,h) = 0 ;
lev_G_DO(scen,n,tech,h) = 0 ;
lev_G_RES(scen,n,tech,h) = 0 ;
lev_CU(scen,n,tech,h) = 0 ;
lev_STO_IN(scen,n,sto,h) = 0 ;
lev_STO_OUT(scen,n,sto,h) = 0 ;
lev_STO_L(scen,n,sto,h) = 0 ;
lev_N_TECH(scen,n,tech) = 0 ;
lev_N_STO_E(scen,n,sto) = 0 ;
lev_N_STO_P(scen,n,sto) = 0 ;
marginal_con1a(scen,h,n) = 0 ;
marginal_con5a(superscen,n) = 0 ;
lev_NTC(superscen,l) = 0 ;
lev_F(superscen,l,h) = 0 ;
lev_RSVR_OUT(superscen,n,rsvr,h) = 0 ;
lev_RSVR_L(superscen,n,rsvr,h) = 0 ;
lev_N_RSVR_E(superscen,n,rsvr) = 0 ;
lev_N_RSVR_P(superscen,n,rsvr) = 0 ;
lev_G_INFES(superscen,n,h) = 0 ;

* Prosumage
lev_CU_PRO(scen,n,tech,h) = 0 ;
lev_G_MARKET_PRO2M(scen,n,tech,h) = 0  ;
lev_G_MARKET_M2PRO(scen,n,h) = 0  ;
lev_G_RES_PRO(scen,n,tech,h) = 0  ;
lev_STO_IN_PRO2PRO(scen,n,tech,sto,h) = 0  ;
lev_STO_IN_PRO2M(scen,n,tech,sto,h) = 0  ;
lev_STO_IN_M2PRO(scen,n,sto,h) = 0   ;
lev_STO_IN_M2M(scen,n,sto,h) = 0  ;
lev_STO_OUT_PRO2PRO(scen,n,sto,h) = 0  ;
lev_STO_OUT_PRO2M(scen,n,sto,h) = 0  ;
lev_STO_OUT_M2PRO(scen,n,sto,h) = 0  ;
lev_STO_OUT_M2M(scen,n,sto,h) = 0  ;
lev_STO_L_PRO2PRO(scen,n,sto,h) = 0  ;
lev_STO_L_PRO2M(scen,n,sto,h) = 0  ;
lev_STO_L_M2PRO(scen,n,sto,h) = 0  ;
lev_STO_L_M2M(scen,n,sto,h) = 0  ;
lev_N_STO_E_PRO(scen,n,sto) = 0  ;
lev_N_STO_P_PRO(scen,n,sto) = 0  ;
lev_STO_L_PRO(scen,n,sto,h) = 0  ;
lev_N_RES_PRO(scen,n,tech) = 0  ;



* Fixing of report parameters for run-of-river
%ror_parameter%$ontext
lev_N_TECH(scen,n,'ror') = 5857.47 ;
lev_G_RES(scen,n,'ror',h) = lev_N_TECH(scen,n,'ror') * phi_res(n,'ror',h) ;
%reserves%$ontext
lev_RP_NONDIS(scen,n,reserves,'ror',h) = 0 ;
$ontext
$offtext

%ror_parameter%%ror_variable%lev_N_TECH(scen,n,'ror') = 0 ;
%ror_parameter%%ror_variable%lev_G_RES(scen,n,'ror',h) = 0 ;
%reserves%$ontext
%ror_parameter%%ror_variable%lev_RP_CON(scen,n,reserves,'ror',h) = 0 ;
$ontext
$offtext






