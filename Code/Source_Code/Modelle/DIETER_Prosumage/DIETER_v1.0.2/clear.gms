
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


*************************************************************************************
**** Clears all variables and equations after each run to speed up calculations  ****
*************************************************************************************


* Clear all variables
option clear = Z ;
option clear = G_L ;
option clear = G_UP ;
option clear = G_DO ;

option clear = G_RES ;
option clear = CU ;

option clear = STO_IN ;
option clear = STO_OUT ;
option clear = STO_L ;

option clear = N_CON ;
option clear = N_RES ;
option clear = N_STO_E ;
option clear = N_STO_P ;

option clear = DSM_CU ;
option clear = DSM_UP ;
option clear = DSM_UP ;
option clear = DSM_UP_DEMAND ;
option clear = DSM_DO_DEMAND ;

option clear = N_DSM_CU ;
option clear = N_DSM_SHIFT ;

option clear = RP_CON ;
option clear = RP_RES ;
option clear = RP_STO_IN ;
option clear = RP_STO_OUT ;
option clear = RP_DSM_CU ;
option clear = RP_DSM_SHIFT ;


* Clear all equations
option clear = obj ;
option clear = con1a_bal ;
option clear = con2a_loadlevel ;
option clear = con2b_loadlevelstart ;

option clear = con3a_maxprod_conv ;
option clear = con3i_maxprod_ror ;
option clear = con3k_maxprod_res ;

option clear = con4a_stolev_start ;
option clear = con4b_stolev ;
option clear = con4c_stolev_max ;
option clear = con4d_maxin_sto ;
option clear = con4e_maxout_sto ;
option clear = con4h_maxout_lev ;
option clear = con4i_maxin_lev ;
option clear = con4j_ending ;
option clear = con4k_PHS_EtoP ;

option clear = con5a_minRES ;
option clear = con5b_maxBIO ;

option clear = con8a_max_I_con ;
option clear = con8b_max_I_res ;
option clear = con8c_max_I_sto_e ;
option clear = con8d_max_I_sto_p ;

%DSM%$ontext
option clear = con6a_DSMcurt_duration_max ;
option clear = con6b_DSMcurt_max ;

option clear = con7a_DSMshift_upanddown ;
option clear = con7b_DSMshift_granular_max ;
option clear = con7c_DSM_distrib_up ;
option clear = con7d_DSM_distrib_do ;
*option clear =  con_7e_DSMshift_recovery ;

option clear = con8e_max_I_dsm_cu ;
option clear = con8f_max_I_dsm_shift_pos ;
$ontext
$offtext

%reserves%$ontext
option clear = con3b_minprod_conv ;
option clear = con3c_flex_PR_up ;
option clear = con3d_flex_PR_do ;
option clear = con3e_flex_SR_up ;
option clear = con3f_flex_SR_do ;
option clear = con3g_flex_MR_up ;
option clear = con3h_flex_MR_do ;
option clear = con3j_minprod_ror ;
option clear = con3l_minprod_res ;

option clear = con4f_resrv_sto ;
option clear = con4g_resrv_sto ;

option clear = con10a_reserve_prov ;
option clear = con10b_reserve_prov_PR ;
$ontext
$offtext
