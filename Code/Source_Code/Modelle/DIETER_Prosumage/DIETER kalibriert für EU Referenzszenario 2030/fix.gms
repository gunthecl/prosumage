
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.#, April 2018.
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

%ror_variable%N_TECH.fx(n,'ror') = 0 ;
%ror_variable%G_RES.fx(n,'ror',h) = N_TECH.l(n,'ror') * phi_res(n,'ror',h) ;
%ror_variable%RP_NONDIS.fx(n,reserves,'ror',h) = 0 ;

* Fix everything to zero in case no ROR option is selected:
%ror_parameter%%ror_variable%N_TECH.fx(n,'ror') = 0 ;
%ror_parameter%%ror_variable%G_RES.fx(n,'ror',h) = 0 ;
%ror_parameter%%ror_variable%RP_CON.fx(n,reserves,'ror',h) = 0 ;

* Fixing of report parameters for run-of-river
%ror_variable%lev_N_TECH(scen,n,'ror') = 0 ;
%ror_variable%lev_G_RES(scen,n,'ror',h) = lev_N_TECH(scen,n,'ror') * phi_res(n,'ror',h) ;
%reserves%$ontext
%ror_variable%lev_RP_NONDIS(scen,n,reserves,'ror',h) = 0 ;
$ontext
$offtext
%ror_parameter%%ror_variable%lev_N_TECH(scen,n,'ror') = 0 ;
%ror_parameter%%ror_variable%lev_G_RES(scen,n,'ror',h) = 0 ;
%reserves%$ontext
%ror_parameter%%ror_variable%lev_RP_CON(scen,n,reserves,'ror',h) = 0 ;
$ontext
$offtext



********************************************************************************
**** Exogenous EV  *************************************************************
********************************************************************************

%EV%$ontext
%EV_EXOG%$ontext
EV_DISCHARGE.fx(n,ev,h) = 0 ;
RP_EV_G2V.fx(n,reserves,ev,h) = 0 ;
RP_EV_V2G.fx(n,reserves,ev,h) = 0 ;
$ontext
$offtext




********************************************************************************
**** No storage and DSM in first period  ***************************************
********************************************************************************

** No storage inflow in first period **
STO_IN.fx(n,sto,h)$(ord(h) = 1) = 0;

%DSM%$ontext
** No DSM load shifting in the first period **
         DSM_UP.fx(n,dsm_shift,h)$(ord(h) = 1) = 0;
         DSM_DO.fx(n,dsm_shift,h,hh)$(ord(h) = 1) = 0 ;
         DSM_DO.fx(n,dsm_shift,h,h)$(ord(h) = 1) = 0 ;
         DSM_UP_DEMAND.fx(n,dsm_shift,h)$(ord(h) = 1) = 0 ;
         DSM_DO_DEMAND.fx(n,dsm_shift,h)$(ord(h) = 1) = 0 ;

** No reserves provision by DSM in first period **
         RP_DSM_SHIFT.fx(n,reserves,dsm_shift,h)$(ord(h) = 1) = 0;
         RP_DSM_CU.fx(n,reserves,dsm_curt,h)$(ord(h) = 1) = 0 ;
         RP_DSM_CU.fx(n,reserves,dsm_curt,h)$(ord(h) = 1) = 0 ;

** No provision of PR and negative reserves by DSM load curtailment **
         RP_DSM_CU.fx(n,reserves_prim,dsm_curt,h) = 0 ;
         RP_DSM_CU.fx(n,reserves_do,dsm_curt,h) = 0 ;

** No provision of PR by DSM load shifting **
         RP_DSM_SHIFT.fx(n,reserves_prim,dsm_shift,h) = 0 ;
$ontext
$offtext




********************************************************************************
**** No primary reserves by heating devices  ***********************************
********************************************************************************

%heat%$ontext
%reserves%$ontext
RP_HP.fx(n,reserves_prim,bu,hp,h) = 0 ;
RP_SETS.fx(n,reserves_prim,bu,ch,h) = 0 ;
RP_SETS_AUX.fx(n,reserves_prim,bu,ch,h) = 0 ;
RP_H_ELEC.fx(n,reserves_prim,bu,ch,h) = 0 ;
$ontext
$offtext




********************************************************************************
**** Fixing to reduce model size  **********************************************
********************************************************************************

F.fx(l,h)$(m_ntc(l) = 0 ) = 0 ;
NTC.fx(l)$(m_ntc(l) = 0 ) = 0 ;

G_L.fx(n,tech,h)$(m_p(n,tech) = 0 ) = 0 ;
G_UP.fx(n,tech,h)$(m_p(n,tech) = 0 ) = 0 ;
G_DO.fx(n,tech,h)$(m_p(n,tech) = 0 ) = 0 ;
N_TECH.fx(n,tech)$(m_p(n,tech) = 0 ) = 0 ;
G_RES.fx(n,tech,h)$(m_p(n,tech) = 0 ) = 0 ;
CU.fx(n,tech,h)$(m_p(n,tech) = 0 ) = 0 ;

N_STO_P.fx(n,sto)$(m_sto_p(n,sto) = 0 ) = 0 ;
N_STO_E.fx(n,sto)$(m_sto_e(n,sto) = 0 ) = 0 ;
STO_IN.fx(n,sto,h)$(m_sto_p(n,sto) = 0 ) = 0 ;
STO_OUT.fx(n,sto,h)$(m_sto_p(n,sto) = 0 ) = 0 ;
STO_L.fx(n,sto,h)$(m_sto_p(n,sto) = 0 ) = 0 ;
RP_STO_IN.fx(n,reserves,sto,h)$(m_sto_p(n,sto) = 0 ) = 0 ;
RP_STO_OUT.fx(n,reserves,sto,h)$(m_sto_p(n,sto) = 0 ) = 0 ;

RSVR_OUT.fx(n,rsvr,h)$(m_rsvr_p(n,rsvr) = 0) = 0 ;
RSVR_L.fx(n,rsvr,h)$(m_rsvr_p(n,rsvr) = 0) = 0 ;
N_RSVR_E.fx(n,rsvr)$(m_rsvr_p(n,rsvr) = 0) = 0 ;
N_RSVR_P.fx(n,rsvr)$(m_rsvr_p(n,rsvr) = 0) = 0 ;

%reserves%$ontext
RP_DIS.fx(n,reserves,tech,h)$(feat_node('reserves',n) = 0 OR m_p(n,tech) = 0 ) = 0 ;
RP_NONDIS.fx(n,reserves,tech,h)$(feat_node('reserves',n) = 0 OR m_p(n,tech) = 0 ) = 0 ;
RP_STO_IN.fx(n,reserves,sto,h)$(feat_node('reserves',n) = 0 OR m_sto_p(n,sto) = 0 ) = 0 ;
RP_STO_OUT.fx(n,reserves,sto,h)$(feat_node('reserves',n) = 0 OR m_sto_p(n,sto) = 0 ) = 0 ;
RP_EV_V2G.fx(n,reserves,ev,h)$(feat_node('reserves',n) = 0) = 0 ;
RP_EV_G2V.fx(n,reserves,ev,h)$(feat_node('reserves',n) = 0) = 0 ;
RP_DSM_CU.fx(n,reserves,dsm_curt,h)$(feat_node('reserves',n) = 0 OR m_dsm_cu(n,dsm_curt) = 0 ) = 0 ;
RP_DSM_SHIFT.fx(n,reserves,dsm_shift,h)$(feat_node('reserves',n) = 0 OR m_dsm_shift(n,dsm_shift) = 0 ) = 0 ;
RP_RSVR.fx(n,reserves,rsvr,h)$(feat_node('reserves',n) = 0  OR m_rsvr_p(n,rsvr) = 0 ) = 0 ;
$ontext
$offtext

%prosumage%$ontext
CU_PRO.fx(n,res,h)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0 ) = 0 ;
G_MARKET_PRO2M.fx(n,res,h)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0 ) = 0 ;
G_MARKET_M2PRO.fx(n,h)$(feat_node('prosumage',n) = 0) = 0 ;
G_RES_PRO.fx(n,res,h)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0 ) = 0 ;
STO_IN_PRO2PRO.fx(n,res,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_IN_PRO2M.fx(n,res,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_IN_M2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_IN_M2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_OUT_PRO2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_OUT_PRO2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_OUT_M2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_OUT_M2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_L_PRO2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_L_PRO2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_L_M2PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_L_M2M.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
N_STO_E_PRO.fx(n,sto)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
N_STO_P_PRO.fx(n,sto)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
STO_L_PRO.fx(n,sto,h)$(feat_node('prosumage',n) = 0 OR m_sto_pro_p(n,sto) = 0 ) = 0 ;
N_RES_PRO.fx(n,res)$(feat_node('prosumage',n) = 0 OR m_res_pro(n,res) = 0 ) = 0 ;
$ontext
$offtext

%EV%$ontext
EV_CHARGE_M.fx(n,ev,h)$(feat_node('ev',n) = 0) = 0 ;
EV_CHARGE_PRO.fx(n,res,ev,h)$(feat_node('ev',n) = 0) = 0 ;
EV_DISCHARGE.fx(n,ev,h)$(feat_node('ev',n) = 0) = 0 ;
EV_L.fx(n,ev,h)$(feat_node('ev',n) = 0) = 0 ;
EV_PHEVFUEL.fx(n,ev,h)$(feat_node('ev',n) = 0) = 0 ;
EV_MPED.fx(n,ev,h)$(feat_node('ev',n) = 0) = 0 ;
RP_EV_V2G.fx(n,reserves,ev,h)$(feat_node('ev',n) = 0) = 0 ;
RP_EV_G2V.fx(n,reserves,ev,h)$(feat_node('ev',n) = 0) = 0 ;
$ontext
$offtext

*%heat%
$ontext
H_DIR.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_SETS_LEV.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_SETS_IN.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_SETS_OUT.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_HP_IN.up(n,bu,ch,hh)$(feat_node('heat',n) = 0) = 0 ;
H_STO_LEV.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_STO_IN_HP.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_STO_IN_ELECTRIC.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_STO_IN_NONELECTRIC.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
H_STO_OUT.up(n,bu,ch,h)$(feat_node('heat',n) = 0) = 0 ;
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

* EV
lev_EV_CHARGE_M(scen,n,ev,h) = 0 ;
lev_EV_CHARGE_PRO(scen,n,res,ev,h) = 0 ;
lev_EV_DISCHARGE(scen,n,ev,h) = 0 ;
lev_EV_L(scen,n,ev,h) = 0 ;
lev_EV_PHEVFUEL(scen,n,ev,h) = 0 ;
lev_EV_MPED(scen,n,ev,h) = 0 ;

* DSM
lev_DSM_CU(scen,n,dsm,h) = 0 ;
lev_DSM_UP(scen,n,dsm,h) = 0 ;
lev_DSM_DO(scen,n,dsm,h,hh) = 0 ;
lev_DSM_UP_DEMAND(scen,n,dsm,h) = 0 ;
lev_DSM_DO_DEMAND(scen,n,dsm,h) = 0 ;
lev_N_DSM_CU(scen,n,dsm) = 0 ;
lev_N_DSM_SHIFT(scen,n,dsm) = 0 ;

* Reserves
lev_RP_DIS(scen,n,reserves,dis,h) = 0 ;
lev_RP_NONDIS(scen,n,reserves,nondis,h) = 0 ;
lev_RP_STO_IN(scen,n,reserves,sto,h) = 0 ;
lev_RP_STO_OUT(scen,n,reserves,sto,h) = 0 ;
lev_RP_RSVR(superscen,n,reserves,rsvr,h) = 0 ;

* EV & reserves
lev_RP_EV_V2G(scen,n,reserves,ev,h) = 0 ;
lev_RP_EV_G2V(scen,n,reserves,ev,h) = 0 ;

* DSM & reserves
lev_RP_DSM_CU(scen,n,reserves,dsm_curt,h) = 0 ;
lev_RP_DSM_SHIFT(scen,n,reserves,dsm_shift,h) = 0 ;

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

* Heat
lev_H_DIR(superscen,n,bu,ch,h) = 0 ;
lev_H_SETS_LEV(superscen,n,bu,ch,h) = 0 ;
lev_H_SETS_IN(superscen,n,res,bu,ch,h) = 0 ;
lev_H_SETS_OUT(superscen,n,bu,ch,h) = 0 ;
lev_H_HP_IN(superscen,n,bu,ch,h) = 0 ;
lev_H_STO_LEV(superscen,n,bu,ch,h) = 0 ;
lev_H_STO_IN_HP(superscen,n,bu,ch,h) = 0 ;
lev_H_STO_IN_ELECTRIC(superscen,n,bu,ch,h) = 0 ;
lev_H_STO_IN_FOSSIL(superscen,n,bu,ch,h) = 0 ;
lev_H_STO_OUT(superscen,n,bu,ch,h) = 0 ;

* Heat - DHW
lev_H_DHW_DIR(superscen,n,bu,ch,h) = 0  ;
lev_H_DHW_STO_OUT(superscen,n,bu,ch,h) = 0  ;
lev_H_DHW_AUX_ELEC_IN(superscen,n,bu,ch,h) = 0  ;
lev_H_DHW_AUX_LEV(superscen,n,bu,ch,h) = 0  ;
lev_H_DHW_AUX_OUT(superscen,n,bu,ch,h) = 0  ;

*### new:
*lev_PHI_PRO_HEAT(superscen,n) = 0 ;
*lev_PHI_PRO_DHW(superscen,n) = 0 ;
*lev_PHI_PRO_EV(superscen,n) = 0 ;
lev_N_SETS_P_OUT(superscen,n,bu,ch) = 0 ;
*lev_N_SETS_DHW_P_IN(superscen,n,bu,ch) = 0 ;
lev_AUX_HEAT(superscen,n,bu,ch,h) = 0 ;
*lev_AUX_DHW(superscen,n,bu,ch,h) = 0 ;

* Heat & reserves
lev_RP_SETS_AUX(superscen,n,reserves,bu,ch,h) = 0 ;
lev_RP_SETS(superscen,n,reserves,bu,ch,h) = 0 ;
lev_RP_HP(superscen,n,reserves,bu,ch,h) = 0 ;
lev_RP_H_ELEC(superscen,n,reserves,bu,ch,h) = 0 ;






