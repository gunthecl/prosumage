
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




*****************************************
**** Scenario file                   ****
****                                 ****
*****************************************

Parameter
m_exog_p(tech)
m_exog_sto_e(sto)
m_exog_sto_p(sto)
*m_exog_rsvr_p(rsvr)
*m_exog_ntc(l)
;

m_exog_p(tech) = technology_data(tech,'fixed_capacities') ;
m_exog_sto_e(sto) = storage_data(sto,'fixed_capacities_energy');
m_exog_sto_p(sto) = storage_data(sto,'fixed_capacities_power');
*m_exog_rsvr_p(rsvr) = reservoir_data(rsvr,'fixed_capacities_power');
*m_exog_ntc(l) = topology_data(l,'fixed_capacities_ntc') ;


*** Dispatch model
$ontext
N_TECH.lo(tech) = m_exog_p(tech) ;
N_STO_P.lo(sto) = m_exog_sto_p(sto) ;
N_STO_E.lo(sto) = m_exog_sto_e(sto) ;
N_RSVR_P.lo(rsvr) =  m_exog_rsvr_p(rsvr) ;
NTC.lo(l) = m_exog_ntc(l) ;

N_TECH.up(tech) = m_exog_p(tech) + 0.1 ;
N_STO_P.up(sto) = m_exog_sto_p(sto) + 0.1 ;
N_STO_E.up(sto) = m_exog_sto_e(sto) + 0.1 ;
N_RSVR_P.up(rsvr) =  m_exog_rsvr_p(rsvr) + 0.1 ;
NTC.up(l) = m_exog_ntc(l) + 0.1 ;

*N_TECH.lo('OCGT') = m_exog_p('OCGT') ;
*N_TECH.up('OCGT') = inf ;
$offtext

*** Investment model
*$ontext
N_TECH.lo(tech) = 0 ;
N_TECH.lo('wind_on') = m_exog_p('wind_on') ;
N_TECH.lo('wind_off') = m_exog_p('wind_off') ;
N_TECH.lo('pv') = m_exog_p('pv') ;
N_STO_P.lo(sto) = m_exog_sto_p(sto) ;
N_STO_E.lo(sto) = m_exog_sto_e(sto) ;
*N_RSVR_P.lo(rsvr) =  m_exog_rsvr_p(rsvr) ;
*NTC.lo(l) = m_exog_ntc(l) ;

N_TECH.up(tech) = m_exog_p(tech) + 0.1 ;
N_TECH.up('wind_on') = inf ;
N_TECH.up('wind_off') = inf ;
N_TECH.up('pv') = inf ;
N_STO_P.up(sto) = m_exog_sto_p(sto) + 0.1 ;
N_STO_E.up(sto) = m_exog_sto_e(sto) + 0.1 ;
N_STO_P.up('sto1') = inf ;
N_STO_P.up('sto5') = inf ;
N_STO_P.up('sto7') = inf ;
N_STO_E.up('sto1') = inf ;
N_STO_E.up('sto5') = inf ;
N_STO_E.up('sto7') = inf ;
*N_RSVR_P.up(rsvr) =  m_exog_rsvr_p(rsvr) + 0.1 ;
*NTC.up(l) = m_exog_ntc(l) + 0.1 ;
*$offtext


*** Reservoir assumption - not used here
*phi_rsvr_lev_min(rsvr) = 0.0 ;
*%GER_only%N_RSVR_E.lo('FR',rsvr) = 600 * m_exog_rsvr_p('FR',rsvr) ;
*%GER_only%N_RSVR_E.lo('AT',rsvr) = 1700 * m_exog_rsvr_p('AT',rsvr) ;
*%GER_only%N_RSVR_E.lo('CH',rsvr) = 3000 * m_exog_rsvr_p('CH',rsvr) ;


*** Germany only, no infeasibility
*NTC.fx(l) = 0 ;
*F.fx(l,h) = 0 ;
*G_INFES.fx(h) = 0 ;


$ontext
*** Heating
Parameter
security_margin_n_heat_out /1.0/
;

* Parameterization of water-based heat storage
n_heat_p_out(bu,ch) = security_margin_n_heat_out * smax( h , dh(bu,ch,h) + d_dhw(bu,ch,h) ) ;
n_heat_e(bu,ch) = 3 * n_heat_p_out(bu,ch) ;
n_heat_p_in(bu,ch) = n_heat_p_out(bu,ch) ;
n_heat_p_in(bu,'hp_gs') = n_heat_p_out(bu,'hp_gs') / ( eta_heat_dyn(bu,'hp_gs') * (temp_sink(bu,'hp_gs')+273.15) / (temp_sink(bu,'hp_gs') - 10) ) ;
* at least -5°C; applied to 98% of hoursn; minimum: -13.4
n_heat_p_in(bu,'hp_as') = n_heat_p_out(bu,'hp_as') / ( eta_heat_dyn(bu,'hp_as') * (temp_sink(bu,'hp_as')+273.15) / (temp_sink(bu,'hp_as') + 5) ) ;

* Parameterization of SETS
n_sets_p_out(bu,ch) = security_margin_n_heat_out * smax( h , dh(bu,ch,h) ) ;
n_sets_p_in(bu,ch) = 2 * n_sets_p_out(bu,ch) ;
n_sets_e(bu,ch) = 16 * n_sets_p_out(bu,ch) ;

* Parameterization of DHW SETS
n_sets_dhw_p_out(bu,ch) = security_margin_n_heat_out * smax( h , d_dhw(bu,ch,h) ) ;
n_sets_dhw_p_in(bu,ch) = n_sets_dhw_p_out(bu,ch) ;
n_sets_dhw_e(bu,ch) = 2.2 * n_sets_dhw_p_out(bu,ch) ;

*RP_SETS_AUX.fx(reserves,bu,ch,h) = 0 ;
*RP_SETS.fx(reserves,bu,ch,h) = 0 ;
*RP_HP.fx(reserves,bu,ch,h) = 0 ;
*RP_H_ELEC.fx(reserves,bu,ch,h) = 0 ;

$offtext