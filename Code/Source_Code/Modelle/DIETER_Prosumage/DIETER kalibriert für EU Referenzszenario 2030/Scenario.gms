
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




*****************************************
**** Scenario file                   ****
****                                 ****
*****************************************

Parameter
m_exog_p(n,tech)
m_exog_sto_e(n,sto)
m_exog_sto_p(n,sto)
m_exog_rsvr_p(n,rsvr)
m_exog_ntc(l)
;

m_exog_p(n,tech) = technology_data(n,tech,'fixed_capacities') ;
m_exog_sto_e(n,sto) = storage_data(n,sto,'fixed_capacities_energy');
m_exog_sto_p(n,sto) = storage_data(n,sto,'fixed_capacities_power');
m_exog_rsvr_p(n,rsvr) = reservoir_data(n,rsvr,'fixed_capacities_power');
m_exog_ntc(l) = topology_data(l,'fixed_capacities_ntc') ;


*** Dispatch model
$ontext
N_TECH.lo(n,tech) = m_exog_p(n,tech) ;
N_STO_P.lo(n,sto) = m_exog_sto_p(n,sto) ;
N_STO_E.lo(n,sto) = m_exog_sto_e(n,sto) ;
N_RSVR_P.lo(n,rsvr) =  m_exog_rsvr_p(n,rsvr) ;
NTC.lo(l) = m_exog_ntc(l) ;

N_TECH.up(n,tech) = m_exog_p(n,tech) + 0.1 ;
N_STO_P.up(n,sto) = m_exog_sto_p(n,sto) + 0.1 ;
N_STO_E.up(n,sto) = m_exog_sto_e(n,sto) + 0.1 ;
N_RSVR_P.up(n,rsvr) =  m_exog_rsvr_p(n,rsvr) + 0.1 ;
NTC.up(l) = m_exog_ntc(l) + 0.1 ;

*N_TECH.lo(n,'OCGT') = m_exog_p(n,'OCGT') ;
*N_TECH.up(n,'OCGT') = inf ;
$offtext

*** Investment model
*$ontext
*N_TECH.lo(n,tech) = 0 ;
N_TECH.lo(n,tech) = m_exog_p(n,tech) ;
*N_TECH.lo(n,'wind_on') = m_exog_p(n,'wind_on') ;
*N_TECH.lo(n,'wind_off') = m_exog_p(n,'wind_off') ;
*N_TECH.lo(n,'pv') = m_exog_p(n,'pv') ;
N_STO_P.lo(n,sto) = m_exog_sto_p(n,sto) ;
N_STO_E.lo(n,sto) = m_exog_sto_e(n,sto) ;
N_RSVR_P.lo(n,rsvr) =  m_exog_rsvr_p(n,rsvr) ;
NTC.lo(l) = m_exog_ntc(l) ;

N_TECH.up(n,tech) = m_exog_p(n,tech) + 0.1 ;
*N_TECH.up(n,'wind_on') = inf ;
*N_TECH.up(n,'wind_off') = inf ;
*N_TECH.up(n,'pv') = inf ;
N_STO_P.up(n,sto) = m_exog_sto_p(n,sto) + 0.1 ;
N_STO_E.up(n,sto) = m_exog_sto_e(n,sto) + 0.1 ;
N_STO_P.up(n,'sto1') = inf ;
*N_STO_P.up(n,'sto5') = inf ;
*N_STO_P.up(n,'sto7') = inf ;
N_STO_E.up(n,'sto1') = inf ;
*N_STO_E.up(n,'sto5') = inf ;
*N_STO_E.up(n,'sto7') = inf ;
N_RSVR_P.up(n,rsvr) =  m_exog_rsvr_p(n,rsvr) + 0.1 ;
NTC.up(l) = m_exog_ntc(l) + 0.1 ;
*$offtext


*phi_rsvr_lev_min(n,rsvr) = 0.0 ;

%GER_only%N_RSVR_E.lo('FR',rsvr) = 600 * m_exog_rsvr_p('FR',rsvr) ;
%GER_only%N_RSVR_E.lo('AT',rsvr) = 1700 * m_exog_rsvr_p('AT',rsvr) ;
%GER_only%N_RSVR_E.lo('CH',rsvr) = 3000 * m_exog_rsvr_p('CH',rsvr) ;


* Germany only, no infeasibility
NTC.fx(l) = 0 ;
F.fx(l,h) = 0 ;
G_INFES.fx(n,h) = 0 ;


* Heating
Parameter
security_margin_n_heat_out /1.0/
;

* Parameterization of water-based heat storage
n_heat_p_out(n,bu,ch) = security_margin_n_heat_out * smax( h , dh(n,bu,ch,h) + d_dhw(n,bu,ch,h) ) ;
n_heat_e(n,bu,ch) = 3 * n_heat_p_out(n,bu,ch) ;
*n_heat_p_in(n,bu,ch)$n_heat_e(n,bu,ch) = n_heat_p_out(n,bu,ch) / n_heat_e(n,bu,ch) ;
n_heat_p_in(n,bu,ch) = n_heat_p_out(n,bu,ch) ;
n_heat_p_in(n,bu,'hp_gs') = n_heat_p_out(n,bu,'hp_gs') / ( eta_heat_dyn(n,bu,'hp_gs') * (temp_sink(n,bu,'hp_gs')+273.15) / (temp_sink(n,bu,'hp_gs') - 10) ) ;
* mindestens -5°C entspricht 98% der Stunden; Minimum: -13.4
n_heat_p_in(n,bu,'hp_as') = n_heat_p_out(n,bu,'hp_as') / ( eta_heat_dyn(n,bu,'hp_as') * (temp_sink(n,bu,'hp_as')+273.15) / (temp_sink(n,bu,'hp_as') + 5) ) ;
*n_heat_p_in(n,bu,hel) = ? * n_heat_p_out(n,bu,ch) ;

* Parameterization of SETS
*n_sets_p_out(n,bu,ch) = security_margin_n_heat_out * smax( h , dh(n,bu,ch,h) ) ;
*n_sets_p_in(n,bu,ch) = 2 * n_sets_p_out(n,bu,ch) ;
*n_sets_e(n,bu,ch) = 16 * n_sets_p_out(n,bu,ch) ;

* Parameterization of DHW SETS
*** ### Check with RV team!
*n_sets_dhw_p_out(n,bu,ch) = security_margin_n_heat_out * smax( h , d_dhw(n,bu,ch,h) ) ;
*n_sets_dhw_p_in(n,bu,ch) = n_sets_dhw_p_out(n,bu,ch) ;
*n_sets_dhw_e(n,bu,ch) = 2.2 * n_sets_dhw_p_out(n,bu,ch) ;

*RP_SETS_AUX.fx(n,reserves,bu,ch,h) = 0 ;
*RP_SETS.fx(n,reserves,bu,ch,h) = 0 ;
*RP_HP.fx(n,reserves,bu,ch,h) = 0 ;
*RP_H_ELEC.fx(n,reserves,bu,ch,h) = 0 ;


*** Prosumage

Parameter
phi_pro_pv
;

phi_pro_pv = 0.25 ;

%prosumage%$ontext
N_RES_PRO.fx(n,'Wind_on') = 0 ;
N_RES_PRO.fx(n,'Wind_off') = 0 ;
N_RES_PRO.fx(n,'pv') = 0 ;
N_RES_PRO.fx(n,'pv')$feat_node('prosumage',n) = m_exog_p(n,'pv')*phi_pro_pv ;
N_TECH.lo(n,'pv') = (1-phi_pro_pv)*m_exog_p(n,'pv') ;
N_TECH.up(n,'pv') = (1-phi_pro_pv)*m_exog_p(n,'pv') + 0.1 ;

lev_N_RES_PRO(scen,n,'Wind_on') = 0 ;
lev_N_RES_PRO(scen,n,'Wind_off') = 0 ;
lev_N_RES_PRO(scen,n,'pv') = 0 ;
lev_N_RES_PRO(scen,n,'pv')$feat_node('prosumage',n) = m_exog_p(n,'pv')*phi_pro_pv ;

* Neue Annahme: PV-Anlagen könnten rechnerisch genau die "normale" HH-Last der Prosumager decken
phi_pro_load(n)$feat_node('prosumage',n) = N_RES_PRO.l(n,'pv')*sum(h,phi_res(n,'pv',h))/sum(h,d_y(n,%base_year%,h)) ;

* ### NEW:
* Keine Rückspeisung V2G, da Elektroautos vollständig für Eigenverbrauch genutzt werden
EV_DISCHARGE.fx(n,ev,h) = 0 ;

** Prosumage cases **

STO_IN_PRO2M.fx(n,res,sto,h) = 0 ;
STO_OUT_PRO2M.fx(n,sto,h) = 0 ;

STO_IN_M2PRO.fx(n,sto,h) = 0 ;
STO_OUT_M2PRO.fx(n,sto,h) = 0 ;

STO_IN_M2M.fx(n,sto,h) = 0 ;
STO_OUT_M2M.fx(n,sto,h) = 0 ;

*EV_CHARGE_PRO.fx(n,res,ev,h) = 0 ;

$ontext
$offtext
