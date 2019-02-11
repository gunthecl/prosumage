
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

;

m_exog_p(tech)    = technology_data(tech,'fixed_capacities') ;
%second_hour%m_exog_sto_e(sto)= storage_data(sto,'fixed_capacities_energy');
m_exog_sto_p(sto)= storage_data(sto,'fixed_capacities_power');


%second_hour%$ontext
m_exog_sto_e(sto)= storage_data(sto,'fixed_capacities_energy')/2;

$ontext
$offtext


*** Dispatch model
%dispatch_model%$ontext
N_TECH.fx(tech)  = m_exog_p(tech) ;
N_STO_P.fx(sto_sys) = m_exog_sto_p(sto_sys);
N_STO_E.fx(sto_sys) = m_exog_sto_e(sto_sys);

$ontext
$offtext

*** Investment model
%investment_model%$ontext
N_TECH.lo(tech)        = 0 ;
N_TECH.lo('wind_on')   = m_exog_p('wind_on') ;
N_TECH.lo('wind_off')  = m_exog_p('wind_off') ;
N_TECH.lo('pv')        = m_exog_p('pv') ;
N_STO_P.lo(sto_sys)    = m_exog_sto_p(sto_sys);
N_STO_E.lo(sto_sys)    = m_exog_sto_e(sto_sys);
N_TECH.up(tech)        = m_exog_p(tech) + 0.1 ;
N_TECH.up('wind_on')   = inf ;
N_TECH.up('wind_off')  = inf ;
N_TECH.up('pv')        = inf ;
N_STO_P.up(sto_sys)    = m_exog_sto_p(sto_sys)+ 0.1 ;
N_STO_E.up(sto_sys)    = m_exog_sto_e(sto_sys)+ 0.1 ;
N_STO_E.up('sto5')     = inf ;
N_STO_P.up('sto5')     = inf ;
N_STO_E.up('sto1')     = inf ;
N_STO_P.up('sto1')     = inf ;

$ontext
$offtext




