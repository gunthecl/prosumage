
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

*****************************************
**** Scenario file                   ****
****                                 ****
*****************************************

** Conventionals in scenario 2035 **
N_CON.fx('bio') = 8400;
N_CON.fx('lig') = 9100;
N_CON.fx('hc') = 11000;
N_CON.fx('CCGT') = 40700/2 ;
N_CON.fx('OCGT_eff') = 0;
N_CON.fx('OCGT_ineff') = 40700/2 ;
N_STO_P.lo('Sto5') = 6400 ;
N_STO_E.lo('Sto5') = 6400*7 ;

lev_N_CON(scen,'bio') = 8400;
lev_N_CON(scen,'lig') = 9100;
lev_N_CON(scen,'hc') = 11000;
lev_N_CON(scen,'CCGT') = 40700/2 ;
lev_N_CON(scen,'OCGT_eff') = 0;
lev_N_CON(scen,'OCGT_ineff') = 40700/2 ;


Parameter
phi_pro_pv
;

phi_pro_pv = 0.25 ;

** Renewables in scenario 2035 **
N_RES.fx('Wind_on') = 88800 ;
N_RES.fx('Wind_off') = 18500 ;
N_RES.fx('Solar') = 59900*(1-phi_pro_pv) ;

lev_N_RES(scen,'Wind_on') = 88800 ;
lev_N_RES(scen,'Wind_off') = 18500 ;
lev_N_RES(scen,'Solar') = 59900*(1-phi_pro_pv) ;

%prosumage%$ontext
N_RES_PRO.fx('Wind_on') = 0 ;
N_RES_PRO.fx('Wind_off') = 0 ;
N_RES_PRO.fx('Solar') = 59900*phi_pro_pv ;

lev_N_RES_PRO(scen,'Wind_on') = 0 ;
lev_N_RES_PRO(scen,'Wind_off') = 0 ;
lev_N_RES_PRO(scen,'Solar') = 59900*phi_pro_pv ;

phi_pro_load = 0.95 * N_RES_PRO.l('Solar')*sum(h,phi_res('Solar',h))/sum(h,d_y(%base_year%,h)) ;


** Prosumage cases **

*STO_IN_PRO2M.fx(res,sto,h) = 0 ;
*STO_OUT_PRO2M.fx(sto,h) = 0 ;

*STO_IN_M2PRO.fx(sto,h) = 0 ;
*STO_OUT_M2PRO.fx(sto,h) = 0 ;

*STO_IN_M2M.fx(sto,h) = 0 ;
*STO_OUT_M2M.fx(sto,h) = 0 ;

$ontext
$offtext


