
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.0.2, January 2016.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
This version constitutes a minor revision of the model documented in Zerrahn, A., Schill, W.-P. (2015): A greenfield model to evaluate long-run power storage requirements for high shares of renewables. DIW Discussion Paper 1457. http://www.diw.de/documents/publikationen/73/diw_01.c.498475.de/dp1457.pdf
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************


*************************************************************
**** Fixes unneccessary or inadequate variables to zero  ****
*************************************************************


** Run-of-river **
* If $setglobal ror_parameter "*": choose appropriate value for N_CON.fx('ror') (but no reserve provision):
%ror_variable%N_CON.fx('ror') = 0.00 ;
%ror_variable%G_L.fx('ror',h) = N_CON.l('ror')*phi_ror(h) ;
%ror_variable%RP_CON.fx(reserves,'ror',h) = 0 ;
* Fix everything to zero in case no ROR option is selected:
%ror_parameter%%ror_variable%N_CON.fx('ror') = 0 ;
%ror_parameter%%ror_variable%G_L.fx('ror',h) = 0 ;
%ror_parameter%%ror_variable%G_UP.fx('ror',h) = 0 ;
%ror_parameter%%ror_variable%G_DO.fx('ror',h) = 0 ;
%ror_parameter%%ror_variable%RP_CON.fx(reserves,'ror',h) = 0 ;

** No nuclear **
G_L.fx('nuc',h) = 0 ;
G_UP.fx('nuc',h) = 0 ;
G_DO.fx('nuc',h) = 0 ;
RP_CON.fx(reserves,'nuc',h) = 0 ;
N_CON.fx('nuc') = 0 ;

** No lignite **
G_L.fx('lig',h) = 0 ;
G_UP.fx('lig',h) = 0 ;
G_DO.fx('lig',h) = 0 ;
RP_CON.fx(reserves,'lig',h) = 0 ;
N_con.fx('lig') = 0 ;

** No storage types 2, 3, 4 and 6 by default **
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

** E to P ratio of PHS free by default **
etop_max('Sto5') = 1000 ;

** No storage inflow in first period **
STO_IN.fx(sto,'h1') = 0;


%DSM%$ontext
** No DSM in the first period **
         DSM_UP.fx(dsm_shift,'h1') = 0;
         DSM_DO.fx(dsm_shift,'h1',hh) = 0 ;
         DSM_DO.fx(dsm_shift,h,'h1') = 0 ;
         DSM_UP_DEMAND.fx(dsm_shift,'h1') = 0 ;
         DSM_DO_DEMAND.fx(dsm_shift,'h1') = 0 ;

** No reserves provision by DSM in first period **
         RP_DSM_SHIFT.fx(reserves,dsm_shift,'h1') = 0;
         RP_DSM_CU.fx('SR_up',dsm_curt,'h1') = 0 ;
         RP_DSM_CU.fx('MR_up',dsm_curt,'h1') = 0 ;

** No provision of PRL and negative reserves by DSM load curtailmet **
         RP_DSM_CU.fx('PR_up',dsm_curt,h) = 0 ;
         RP_DSM_CU.fx('PR_do',dsm_curt,h) = 0 ;
         RP_DSM_CU.fx('SR_do',dsm_curt,h) = 0 ;
         RP_DSM_CU.fx('MR_do',dsm_curt,h) = 0 ;
$ontext
$offtext
