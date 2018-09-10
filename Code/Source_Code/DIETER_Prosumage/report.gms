
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.2.0, February 2017.
Written by Alexander Zerrahn and Wolf-Peter Schill. Moritz Niemeyer contributed to electric vehicle modeling.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************

$setenv gdxcompress 1


* Parameters for the report file
parameter
corr_fac_con
corr_fac_res
corr_fac_sto
corr_fac_dsm_cu
corr_fac_dsm_shift
corr_fac_ev

gross_energy_demand
gross_energy_demand_market
gross_energy_demand_prosumers
gross_energy_demand_prosumers_selfgen
gross_energy_demand_prosumers_market

calc_maxprice
calc_minprice

report
report_tech
report_tech_hours
report_hours

%reserves%
report_reserves
report_reserves_hours
report_reserves_tech
report_reserves_tech_hours
$ontext
$offtext

%prosumage%$ontext
report_prosumage
report_prosumage_tech
report_prosumage_tech_hours
report_market
report_market_tech
report_market_tech_hours
$ontext
$offtext
;


* Min and max for prices
calc_maxprice = 0 ;
calc_minprice = 1000 ;


* Default values for correction factors
corr_fac_con(scen,ct,h) = 0 ;
corr_fac_res(scen,res,h) = 0 ;
corr_fac_sto(scen,sto,h) = 0 ;
corr_fac_dsm_cu(scen,dsm_curt,h) = 0 ;
corr_fac_dsm_shift(scen,dsm_shift,h) = 0 ;
corr_fac_ev(scen,h) = 0 ;


* Define gross energy demand for reporting, egual to equation 5a
gross_energy_demand(scen) = sum( h , d(h) + sum( sto , lev_STO_IN(scen,sto,h) - lev_STO_OUT(scen,sto,h) )
%prosumage%$ontext
         + sum( (sto) , sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h) ) + lev_STO_IN_M2PRO(scen,sto,h) + lev_STO_IN_M2M(scen,sto,h) - lev_STO_OUT_PRO2PRO(scen,sto,h) - lev_STO_OUT_PRO2M(scen,sto,h) - lev_STO_OUT_M2PRO(scen,sto,h) - lev_STO_OUT_M2M(scen,sto,h) )
$ontext
$offtext
%DSM%$ontext
         - sum( dsm_curt , lev_DSM_CU(scen,dsm_curt,h) )
         + sum( dsm_shift , lev_DSM_UP(scen,dsm_shift,h) - sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(dsm_shift) ) , lev_DSM_DO(scen,dsm_shift,h,hh)) )
$ontext
$offtext
%EV%$ontext
         + sum( ev , lev_EV_CHARGE(scen,ev,h) - lev_EV_DISCHARGE(scen,ev,h) )
$ontext
$offtext
%reserves%$ontext
        + phi_mean_reserves_call('PR_up') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res))/1000 )) )
        + phi_mean_reserves_call('SR_up') *( 1000 * phi_reserves_share('SR_up') * (reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res))/1000)) )
        + phi_mean_reserves_call('MR_up') *( 1000 * phi_reserves_share('MR_up') * (reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res))/1000)) )
        - phi_mean_reserves_call('PR_do') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res))/1000)) )
        - phi_mean_reserves_call('SR_do') *( 1000 * phi_reserves_share('SR_do') * (reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res))/1000)) )
        - phi_mean_reserves_call('MR_do') *( 1000 * phi_reserves_share('MR_do') * (reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res))/1000)) )

       + sum( sto ,
           sum( reserves_do , phi_reserves_call(reserves_do,h) * ( lev_RP_STO_IN(scen,reserves_do,sto,h) + lev_RP_STO_OUT(scen,reserves_do,sto,h)))
         - sum( reserves_up , phi_reserves_call(reserves_up,h) * ( lev_RP_STO_IN(scen,reserves_up,sto,h) + lev_RP_STO_OUT(scen,reserves_up,sto,h))))
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
       - sum( (dsm_curt,reserves_up) , lev_RP_DSM_CU(scen,reserves_up,dsm_curt,h) * phi_reserves_call(reserves_up,h) )
$ontext
$offtext
%reserves%$ontext
%EV%$ontext
       + sum( ev ,
           sum( reserves_do , phi_reserves_call(reserves_do,h) * (lev_RP_EV_G2V(scen,reserves_do,ev,h) + lev_RP_EV_V2G(scen,reserves_do,ev,h)))
         - sum( reserves_up , phi_reserves_call(reserves_up,h) * (lev_RP_EV_G2V(scen,reserves_up,ev,h) + lev_RP_EV_V2G(scen,reserves_up,ev,h))) )
$ontext
$offtext
)
;


* Prepare prosumage reporting parameters
%prosumage%$ontext
gross_energy_demand_prosumers(scen) = sum( h , phi_pro_load * d(h));
gross_energy_demand_prosumers_selfgen(scen) = sum( (h,res) , lev_G_RES_PRO(scen,res,h)) + sum( (sto,h) , lev_STO_OUT_PRO2PRO(scen,sto,h) ) ;
gross_energy_demand_prosumers_market(scen) = sum( h , lev_G_MARKET_M2PRO(scen,h)) + sum( (sto,h) , lev_STO_OUT_M2PRO(scen,sto,h) ) ;
gross_energy_demand_market(scen) = gross_energy_demand(scen) - gross_energy_demand_prosumers_selfgen(scen) ;
$ontext
$offtext

%prosumage% Parameters
%prosumage% lev_CU_PRO(superscen,res,h)
%prosumage% lev_G_MARKET_PRO2M(superscen,res,h)
%prosumage% lev_G_MARKET_M2PRO(superscen,h)
%prosumage% lev_G_RES_PRO(superscen,res,h)
%prosumage% lev_STO_IN_PRO2PRO(superscen,res,sto,h)
%prosumage% lev_STO_IN_PRO2M(superscen,res,sto,h)
%prosumage% lev_STO_IN_M2PRO(superscen,sto,h)
%prosumage% lev_STO_IN_M2M(superscen,sto,h)
%prosumage% lev_STO_OUT_PRO2PRO(superscen,sto,h)
%prosumage% lev_STO_OUT_PRO2M(superscen,sto,h)
%prosumage% lev_STO_OUT_M2PRO(superscen,sto,h)
%prosumage% lev_STO_OUT_M2M(superscen,sto,h)
%prosumage% lev_STO_L_PRO2PRO(superscen,sto,h)
%prosumage% lev_STO_L_PRO2M(superscen,sto,h)
%prosumage% lev_STO_L_M2PRO(superscen,sto,h)
%prosumage% lev_STO_L_M2M(superscen,sto,h)
%prosumage% lev_N_STO_E_PRO(superscen,sto)
%prosumage% lev_N_STO_P_PRO(superscen,sto)
%prosumage% lev_STO_L_PRO(superscen,sto,h)
%prosumage% lev_N_RES_PRO(superscen,res)
;

%prosumage% lev_CU_PRO(superscen,res,h) = 0 ;
%prosumage% lev_G_MARKET_PRO2M(superscen,res,h) = 0 ;
%prosumage% lev_G_MARKET_M2PRO(superscen,h) = 0 ;
%prosumage% lev_G_RES_PRO(superscen,res,h) = 0 ;
%prosumage% lev_STO_IN_PRO2PRO(superscen,res,sto,h) = 0 ;
%prosumage% lev_STO_IN_PRO2M(superscen,res,sto,h) = 0 ;
%prosumage% lev_STO_IN_M2PRO(superscen,sto,h) = 0 ;
%prosumage% lev_STO_IN_M2M(superscen,sto,h) = 0 ;
%prosumage% lev_STO_OUT_PRO2PRO(superscen,sto,h) = 0 ;
%prosumage% lev_STO_OUT_PRO2M(superscen,sto,h) = 0 ;
%prosumage% lev_STO_OUT_M2PRO(superscen,sto,h) = 0 ;
%prosumage% lev_STO_OUT_M2M(superscen,sto,h) = 0 ;
%prosumage% lev_STO_L_PRO2PRO(superscen,sto,h) = 0 ;
%prosumage% lev_STO_L_PRO2M(superscen,sto,h) = 0 ;
%prosumage% lev_STO_L_M2PRO(superscen,sto,h) = 0 ;
%prosumage% lev_STO_L_M2M(superscen,sto,h) = 0 ;
%prosumage% lev_N_STO_E_PRO(superscen,sto) = 0 ;
%prosumage% lev_N_STO_P_PRO(superscen,sto) = 0 ;
%prosumage% lev_STO_L_PRO(superscen,sto,h) = 0 ;
%prosumage% lev_N_RES_PRO(superscen,res) = 0 ;


* Determine correction factors
%reserves%$ontext
        corr_fac_con(scen,ct,h) =
          sum( reserves_do ,  lev_RP_CON(scen,reserves_do,ct,h) * phi_reserves_call(reserves_do,h))
        - sum( reserves_up ,  lev_RP_CON(scen,reserves_up,ct,h) * phi_reserves_call(reserves_up,h))
;
        corr_fac_res(scen,res,h) =
          sum( reserves_do ,  lev_RP_RES(scen,reserves_do,res,h) * phi_reserves_call(reserves_do,h))
        - sum( reserves_up ,  lev_RP_RES(scen,reserves_up,res,h) * phi_reserves_call(reserves_up,h))
;
        corr_fac_sto(scen,sto,h) =
           sum( reserves_do , phi_reserves_call(reserves_do,h) * ( lev_RP_STO_IN(scen,reserves_do,sto,h) + lev_RP_STO_OUT(scen,reserves_do,sto,h)) )
         - sum( reserves_up , phi_reserves_call(reserves_up,h) * ( lev_RP_STO_IN(scen,reserves_up,sto,h) + lev_RP_STO_OUT(scen,reserves_up,sto,h)) )
;
%DSM%$ontext
        corr_fac_dsm_cu(scen,dsm_curt,h) =
       - sum( reserves_up , lev_RP_DSM_CU(scen,reserves_up,dsm_curt,h) * phi_reserves_call(reserves_up,h) )
;
        corr_fac_dsm_shift(scen,dsm_shift,h) =
          sum( reserves_do ,  lev_RP_DSM_SHIFT(scen,reserves_do,dsm_shift,h) * phi_reserves_call(reserves_do,h))
        - sum( reserves_up ,  lev_RP_DSM_SHIFT(scen,reserves_up,dsm_shift,h) * phi_reserves_call(reserves_up,h))
;
$ontext
$offtext
%EV%$ontext
%reserves%$ontext
         corr_fac_ev(scen,h) = sum( ev ,
         + sum( reserves_do , phi_reserves_call(reserves_do,h) * (lev_RP_EV_G2V(scen,reserves_do,ev,h) + lev_RP_EV_V2G(scen,reserves_do,ev,h)))
         - sum( reserves_up , phi_reserves_call(reserves_up,h) * (lev_RP_EV_G2V(scen,reserves_up,ev,h) + lev_RP_EV_V2G(scen,reserves_up,ev,h))) )
;
$ontext
$offtext

* ----------------------------------------------------------------------------

* Report parameter RPEORT
        report('model status',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , modstats(scen, 'modelstat')) ;
        report('solve time',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , modstats(scen, 'resusd')) ;
        report('obj value',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_Z(scen) * %sec_hour%) ;
        report('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage) = sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,res,h) + lev_CU(scen,res,h))) * %sec_hour% ;
        report('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage)$(sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(res,h) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)) )) > eps_rep_abs*card(res)*card(h)) = sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,res,h) + lev_CU(scen,res,h))) * %sec_hour% / sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(res,h) * (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)) )) ;
        report('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage)$(m_con_e('bio')) = (m_con_e('bio') - sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,'bio',h)))) * %sec_hour% ;
        report('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage)$(m_con_e('bio')) = (m_con_e('bio') - sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,'bio',h)))) / m_con_e('bio') ;
        report('max price',loop_res_share,loop_ev,loop_prosumage) = max( calc_maxprice , smax( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - marginal_con1a(scen,h))) ) ;
        report('min price',loop_res_share,loop_ev,loop_prosumage) = min( calc_minprice , smin( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - marginal_con1a(scen,h))) ) ;
        report('mean price',loop_res_share,loop_ev,loop_prosumage) = -sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con1a(scen,h)))/card(h) ;
        report('Capacity total',loop_res_share,loop_ev,loop_prosumage) = sum( ct , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct)) ) + sum( res , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res) )) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(Scen,sto) ))
%DSM%$ontext
        + sum( dsm_curt , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,dsm_curt)) ) + sum( dsm_shift , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,dsm_shift)) )
$ontext
$offtext
;
                 report('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage)$(report('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage)$(report('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage)$(report('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage)$(report('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report('min price',loop_res_share,loop_ev,loop_prosumage)$(report('min price',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs AND report('min price',loop_res_share,loop_ev,loop_prosumage) > -eps_rep_abs) = eps ;

* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT HOURS
        report_hours('demand',loop_res_share,loop_ev,loop_prosumage,h) = d(h) ;
        report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) = -sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con1a(scen,h)) ;
%prosumage%$ontext
        report_hours('demand prosumers',loop_res_share,loop_ev,loop_prosumage,h) = phi_pro_load * d(h) ;
        report_hours('demand market',loop_res_share,loop_ev,loop_prosumage,h) = (1 - phi_pro_load) * d(h) ;
$ontext
$offtext
;
                 report_hours('demand',loop_res_share,loop_ev,loop_prosumage,h)$(report_hours('demand',loop_res_share,loop_ev,loop_prosumage,h) < eps_rep_abs) = 0 ;
                 report_hours('price',loop_res_share,loop_ev,loop_prosumage,h)$(report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) < eps_rep_abs AND report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) > -eps_rep_abs) = eps ;
%prosumage%$ontext
                 report_hours('demand prosumers',loop_res_share,loop_ev,loop_prosumage,h)$(report_hours('demand prosumers',loop_res_share,loop_ev,loop_prosumage,h) < eps_rep_abs) = 0 ;
                 report_hours('demand market',loop_res_share,loop_ev,loop_prosumage,h)$(report_hours('demand market',loop_res_share,loop_ev,loop_prosumage,h) < eps_rep_abs) = 0 ;
$ontext
$offtext

* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT TECH
        report_tech('capacities conventional',loop_res_share,loop_ev,loop_prosumage,ct_the) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the)) ;
        report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,ct_ren) = 0 + sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren)) ;
        report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,res) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res));
        report_tech('capacities storage MW',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto)+ lev_N_STO_P_PRO(scen,sto)) ;
        report_tech('capacities storage MWh',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto)+ lev_N_STO_E_PRO(scen,sto)) * %sec_hour% ;
        report('gross energy demand',loop_res_share,loop_ev,loop_prosumage) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen)) ;
%prosumage%$ontext
        report('gross energy demand market',loop_res_share,loop_ev,loop_prosumage) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen)) ;
$ontext
$offtext
        report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,ct_ren) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen) ) ;
        report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,res) = sum( h, sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) + lev_G_RES_PRO(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h)) + lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)))/ sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen) );
        report('renshare total',loop_res_share,loop_ev,loop_prosumage) = sum(res, report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,res)) + sum(ct_ren,report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,ct_ren)) ;
        report_tech('conshares',loop_res_share,loop_ev,loop_prosumage,ct_the) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen) ) ;
        report('conshare total',loop_res_share,loop_ev,loop_prosumage) = sum(ct_the, report_tech('conshares',loop_res_share,loop_ev,loop_prosumage,ct_the) ) ;

        report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_PRO(scen,res,h) )) * %sec_hour% ;
        report_tech('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res) AND sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES_PRO(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) + sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,res,h) + lev_CU(scen,res,h))) > card(h)*eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_PRO(scen,res,h) ))/( sum(h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(res,h)*(lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res) ))) ) ;

                 report_tech('capacities conventional',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_tech('capacities conventional',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_ins) = 0 ;
                 report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_ins) = 0 ;
                 report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_ins) = 0 ;
                 report_tech('capacities storage MW',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('capacities storage MW',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_ins) = 0 ;
                 report_tech('capacities storage MWh',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('capacities storage MWh',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_ins) = 0 ;
                 report_tech('conshares',loop_res_share,loop_ev,loop_prosumage,ct)$(report_tech('conshares',loop_res_share,loop_ev,loop_prosumage,ct) < eps_rep_rel) = 0 ;
                 report('conshare total',loop_res_share,loop_ev,loop_prosumage)$(report('conshare total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_rel) = 0 ;
                 report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('renshares',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report('renshare total',loop_res_share,loop_ev,loop_prosumage)$(report('renshare total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;

* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT TECH HOURS

        report_tech_hours('generation conventional',loop_res_share,loop_ev,loop_prosumage,ct_the,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h) + corr_fac_con(scen,ct_the,h)) ;
        report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,ct_ren,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h) + corr_fac_con(scen,ct_ren,h)) ;
        report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,res,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h) + lev_G_RES_PRO(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h)) ) ;
        report_tech_hours('curtailment of fluct res',loop_res_share,loop_ev,loop_prosumage,res,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_PRO(scen,res,h) ) ;
        report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_PRO2PRO(scen,sto,h) ) ;
        report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h)) + lev_STO_IN_M2PRO(scen,sto,h) ) ;
        report_tech_hours('storage level',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L(scen,sto,h) + lev_STO_L_PRO(scen,sto,h) ) ;

                 report_tech_hours('generation conventional',loop_res_share,loop_ev,loop_prosumage,ct,h)$(report_tech_hours('generation conventional',loop_res_share,loop_ev,loop_prosumage,ct,h) < eps_rep_abs) = 0 ;
                 report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,ct_ren,h)$(report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,ct_ren,h) < eps_rep_abs) = 0 ;
                 report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) = 0 ;
                 report_tech_hours('curtailment of fluct res',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_tech_hours('curtailment of fluct res',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) = 0 ;
                 report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_tech_hours('storage level',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_tech_hours('storage level',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT PROSUMAGE TECH HOURS

%prosumage%$ontext
        report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_ev,loop_prosumage,res,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(res,h) * lev_N_RES_PRO(scen,res) ) ;
        report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_ev,loop_prosumage,res,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,res,h)) ;
        report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES_PRO(scen,res,h) ) ;
        report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) ) ;
        report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h) ) ;
        report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h))) ;
        report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2M(scen,res,sto,h))) ;
        report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_PRO2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_PRO2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_M2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_M2M(scen,sto,h)) ;

                 report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h)$(report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT PROSUMAGE TECH

        report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_ev,loop_prosumage,res) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,res) ) ;
        report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) ;
        report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto)) * %sec_hour% ;        report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h));
        report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h));
        report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h));
        report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h));
        report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h, report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( h, report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto) = report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) + report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) + report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) + report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'') =  sum( (h,res) , report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h)) ;
        report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'') =  sum( h , report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h)) ;
        report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'') =  sum( (res,h) , report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h)) ;
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,res) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , lev_G_RES_PRO(scen,res,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load * d(h) ) );
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,sto) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load * d(h) ) );
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,'market') = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , lev_G_MARKET_M2PRO(scen,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load * d(h) ) );
        report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,res,h) )) * %sec_hour% ;
        report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,res)) > eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,res,h) ))/ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;

        report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h)))) / report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2M(scen,res,sto,h)))) / report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2PRO(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2M(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2M(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2PRO(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2M(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto) ;
        report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'') > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,res,h)))) / report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'') ;
        report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'') > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h))) / report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'') ;
        report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'') > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_RES_PRO(scen,res,h)))) / report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'') ;

                 report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_ins) =  0 ;
                 report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_ins) =  0 ;
                 report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_ins) =  0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,'market')$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,'market') < eps_rep_rel) = 0 ;
                 report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_ins) = 0 ;
                 report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'') < eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'') < eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'') < eps_rep_abs * card(h)) = 0 ;

                 report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_ev,loop_prosumage,'') < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_ev,loop_prosumage,'') < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_ev,loop_prosumage,'')$(report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_ev,loop_prosumage,'') < eps_rep_abs) = 0 ;

* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT PROSUMAGE

        report_prosumage('gross energy demand prosumers',loop_res_share,loop_ev,loop_prosumage) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen)) ;
        report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_ev,loop_prosumage) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_market(scen)) ;
        report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen)) ;
        report_prosumage('self-generation share prosumers total',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen)) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen) ) ;
        report_prosumage('market share prosumers',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_market(scen)) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen) ) ;
        report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage) = sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,res,h))) * %sec_hour% ;
        report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_CU_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation curtailed',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_CU_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation direct consumption',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_G_RES_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation to market',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_G_MARKET_PRO2M(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,sto,res) , lev_STO_IN_PRO2PRO(scen,res,sto,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,sto,res) , lev_STO_IN_PRO2M(scen,res,sto,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage) = sum( res , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,res)) ) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) ) ;

                 report_prosumage('gross energy demand prosumers',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('gross energy demand prosumers',loop_res_share,loop_ev,loop_prosumage) < eps_rep_ins) = 0 ;
                 report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_ev,loop_prosumage) < eps_rep_ins) = 0 ;
                 report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage) < eps_rep_ins) = 0 ;
                 report_prosumage('self-generation share prosumage total',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('self-generation share prosumage total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('market share prosumage',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('market share prosumage',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage) < eps_rep_ins) = 0 ;
                 report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation curtailed',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('share self-generation curtailed',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation direct consumption',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('share self-generation direct consumption',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation to market',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('share self-generation to market',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage)$(report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs) = 0 ;

       report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,res) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,res) ) / report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
       report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,sto) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto) ) / report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;

                 report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;

* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT MARKET TECH HOURS

        report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,ct_the,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h) + corr_fac_con(scen,ct_the,h)) ;
        report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,ct_ren,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h) + corr_fac_con(scen,ct_ren,h)) ;
        report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,res,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h) + corr_fac_res(scen,res,h)) ;
        report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_ev,loop_prosumage,res,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h)) ;
        report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h)) ;
        report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h)) ;
        report_market_tech_hours('storage level market',loop_res_share,loop_ev,loop_prosumage,sto,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L(scen,sto,h)) ;
        report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2PRO(scen,sto,h)) ) ;
        report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2M(scen,sto,h)) ) ;
        report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_PRO2M(scen,sto,h)) ) ;
        report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_M2M(scen,sto,h)) ) ;
        report_market_tech_hours('energy market to prosumer',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h) )   ;
        report_market_tech_hours('energy prosumer to market',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,res,h)) )  ;

                 report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,ct_the,h)$(report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,ct_the,h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,ct_ren,h)$(report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,ct_ren,h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_ev,loop_prosumage,res,h)$(report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_ev,loop_prosumage,res,h) < eps_rep_abs) =  0 ;
                 report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) =  0 ;
                 report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('storage level market',loop_res_share,loop_ev,loop_prosumage,sto,h)$(report_market_tech_hours('storage level market',loop_res_share,loop_ev,loop_prosumage,sto,h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('energy market to prosumer',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('energy market to prosumer',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('energy prosumer to market',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('energy prosumer to market',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h) < eps_rep_abs) = 0 ;

* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT MARKET TECH

        report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) ) ;
        report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto)) ;
        report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto)) * %sec_hour% ;
        report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h) )) * %sec_hour% ;
        report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res) AND sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) + sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h))) > card(h)*eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h) ))/( sum(h,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) + sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h)  )) ) ;
        report_market_tech('capacities conventional market',loop_res_share,loop_ev,loop_prosumage,ct_the) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the)) ;
        report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,ct_ren) = 0 + sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren)) ;
        report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) );
        report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto)) ;
        report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto)) * %sec_hour% ;
        report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) = sum(h, report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h) ) * %sec_hour% ;
        report_market_tech('Storage in total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) = sum(h, report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h) ) * %sec_hour% ;
        report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res)) ;

                 report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_abs) =  0 ;
                 report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) =  0 ;
                 report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) =  0 ;
                 report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_abs*card(h)) = 0 ;
                 report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_market_tech('capacities conventional market',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_market_tech('capacities conventional market',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_ins) = 0 ;
                 report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_ins) = 0 ;
                 report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_ins) =  0;
                 report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_ins) =  0 ;
                 report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_ins) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_abs) = 0 ;
                 report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
                 report_market_tech('Storage in total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('Storage in total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;

* ----------------------------------------------------------------------------

*$exit
* Report parameter REPORT MARKET

        report_market('gross energy demand market',loop_res_share,loop_ev,loop_prosumage) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_market('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage) = sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h))) * %sec_hour% ;
        report_market('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage)$( sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , phi_res(res,h)*lev_N_RES(scen,res)) ) > eps_rep_abs*card(res)*card(h) ) = sum( (res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,res,h)))/ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES(scen,res)) ) ;
        report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_ev,loop_prosumage) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen) ) ;
        report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2PRO(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen) ) ;
        report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2M(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen) ) ;
        report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_ev,loop_prosumage) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,res,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen) ) ;
        report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_ev,loop_prosumage) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_PRO2M(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen) ) ;
        report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_M2M(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen) ) ;
        report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage) = sum( ct , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct)) ) + sum( res , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res)  )) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) )) ;

                 report_market('gross energy demand market',loop_res_share,loop_ev,loop_prosumage)$(report_market('gross energy demand market',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs) = 0 ;
                 report_market('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage)$(report_market('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs) = 0 ;
                 report_market('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage)$(report_market('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_ev,loop_prosumage)$(report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage)$(report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage)$(report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_ev,loop_prosumage)$(report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_ev,loop_prosumage)$(report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage)$(report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage)$(report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs) = 0 ;

       report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,ct_the) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
       report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,ct_ren) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
       report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,res) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
       report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,sto) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;

                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_rel) = 0 ;
                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_rel) = 0 ;
                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;
$ontext
$offtext

%prosumage%$ontext
       report_market('Energy total market',loop_res_share,loop_ev,loop_prosumage) = (sum( h , (1 - phi_pro_load) * d(h) ) + sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h))) + sum( (sto,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h) + lev_STO_IN_M2M(scen,sto,h) + lev_STO_IN_M2PRO(scen,sto,h) ) )) * %sec_hour%
%prosumage%$ontext
%DSM%$ontext
        + sum( (dsm_shift,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,dsm_shift,h)) ) * %sec_hour%
$ontext
$offtext
%prosumage%$ontext
%EV%$ontext
         + sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_CHARGE(scen,ev,h)) ) * %sec_hour%
$ontext
$offtext
%prosumage%$ontext
%reserves%$ontext
       + sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) ) * phi_reserves_call('PR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_up') * ( reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('SR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_up') * ( reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('MR_up',h) )
       - sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) ) * phi_reserves_call('PR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_do') * ( reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('SR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_do') * ( reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('MR_do',h) )
$ontext
$offtext
;

%prosumage%$ontext
       report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,ct_the) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h)) ) / report_market('Energy total market',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
       report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,ct_ren) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h)) ) / report_market('Energy total market',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
       report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,res) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) / report_market('Energy total market',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
       report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2M(scen,sto,h) + lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h)) ) / report_market('Energy total market',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;

                 report_market('Energy total market',loop_res_share,loop_ev,loop_prosumage)$(report_market('Energy total market',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs) = 0 ;
                 report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('Energy share market',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;
$ontext
$offtext

%EV%$ontext
        report('gross_energy_demand w/o EV',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen)) - sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_GED(scen,ev,h)) ) ;
        report('EV electricity demand',loop_res_share,loop_ev,loop_prosumage) = sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_GED(scen,ev,h)) ) ;
        report('renshare EV',loop_res_share,loop_ev,loop_prosumage)$(sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_GED(scen,ev,h))) > card(h)*eps_rep_abs AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con5a(scen) = 0)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , min_res(scen)) + (sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen))/ sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_GED(scen,ev,h))) )*(report('renshare total',loop_res_share,loop_ev,loop_prosumage) - sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , min_res(scen)) ) ;
%EV_FREE%%EV_DEFAULT%        report('renshare EV',loop_res_share,loop_ev,loop_prosumage) = 1 ;
        report('conshare EV',loop_res_share,loop_ev,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con5a(scen) = 0)) = 1 - report('renshare EV',loop_res_share,loop_ev,loop_prosumage) ;
%EV_FREE%%EV_DEFAULT%        report('conshare EV',loop_res_share,loop_ev,loop_prosumage) = 0 ;
        report_tech_hours('EV charge',loop_res_share,loop_ev,loop_prosumage,ev,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_CHARGE(scen,ev,h)) ;
        report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_DISCHARGE(scen,ev,h)) ;
        report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_PHEVFUEL(scen,ev,h));
        report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_GED(scen,ev,h));
        report_tech_hours('EV battery level',loop_res_share,loop_ev,loop_prosumage,ev,h) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_L(scen,ev,h)) ;

         report('renshare EV',loop_res_share,loop_ev,loop_prosumage)$(report('renshare EV',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = eps ;
         report('conshare EV',loop_res_share,loop_ev,loop_prosumage)$(report('conshare EV',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = eps ;
         report('EV_electricity_demand',loop_res_share,loop_ev,loop_prosumage)$(report('EV_electricity_demand',loop_res_share,loop_ev,loop_prosumage) < card(h)*eps_rep_abs) = 0 ;
         report_tech_hours('EV charge',loop_res_share,loop_ev,loop_prosumage,ev,h)$(report_tech_hours('EV charge',loop_res_share,loop_ev,loop_prosumage,ev,h) < eps_rep_abs) = 0 ;
         report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h)$(report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h) < eps_rep_abs) = 0 ;
         report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h)$(report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) < eps_rep_abs) = 0 ;
         report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h)$(report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) < eps_rep_abs) = 0 ;
$ontext
$offtext

        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,ct_the) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the) ) / report('Capacity total',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,ct_ren) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren)) / report('Capacity total',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,res) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res) ) / report('Capacity total',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,sto) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) / report('Capacity total',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
        report('Energy total',loop_res_share,loop_ev,loop_prosumage) = (sum( h , d(h) ) + sum( (sto,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_STO_IN_M2PRO(scen,sto,h) + lev_STO_IN_M2M(scen,sto,h)) )) * %sec_hour%
%DSM%$ontext
        + sum( (dsm_shift,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,dsm_shift,h)) ) * %sec_hour%
$ontext
$offtext
%EV%$ontext
         + sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_CHARGE(scen,ev,h)) ) * %sec_hour%
$ontext
$offtext
%reserves%$ontext
       + sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) ) * phi_reserves_call('PR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_up') * ( reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('SR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_up') * ( reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('MR_up',h) )
       - sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum( res , reserves_slope(reserves,res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) ) * phi_reserves_call('PR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_do') * ( reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('SR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_do') * ( reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res)))/1000 ) ) * phi_reserves_call('MR_do',h) )
$ontext
$offtext
;

        report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,ct_the) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h)) ) / report('Energy total',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
        report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,ct_ren) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h)) ) / report('Energy total',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
        report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,res) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) + sum(sto , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_G_RES_PRO(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) / report('Energy total',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
        report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,sto) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h) + lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h)) ) / report('Energy total',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;

                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;
                 report('Energy total',loop_res_share,loop_ev,loop_prosumage)$(report('Energy total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs) = 0 ;

                 report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_rel) = 0 ;
                 report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_rel) = 0 ;
                 report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_rel) = 0 ;
                 report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;
%EV%$ontext
        report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_DISCHARGE(scen,ev,h) - corr_fac_ev(scen,h)) ) / report('Energy total',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
                 report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_rel) = 0 ;
$ontext
$offtext

         report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,ct_the)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the)) ;
         report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren)) ;
         report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,res)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res) ) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h)) + lev_G_RES_PRO(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) + lev_N_RES_PRO(scen,res) ) ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_abs) = 0 ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_abs) = 0 ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,res)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_abs) = 0 ;
%DSM%$ontext
         report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,dsm_shift)) > eps_rep_ins) = sum( (h,hh) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO(scen,dsm_shift,h,hh)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,dsm_shift)) ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < eps_rep_abs) = 0 ;
$ontext
$offtext
       report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) = sum(h, report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h) ) * %sec_hour% ;
       report_tech('Storage in total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) = sum(h, report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h) ) * %sec_hour% ;

                 report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage in total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage in total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;

%reserves%$ontext
       report_tech('Storage positive reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto) = sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves_up,sto,h)) * phi_reserves_call(reserves_up,h)) * %sec_hour% ;
       report_tech('Storage negative reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto) = sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves_do,sto,h)) * phi_reserves_call(reserves_do,h)) * %sec_hour% ;
       report_tech('Storage positive reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto) = sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_up,sto,h)) * phi_reserves_call(reserves_up,h)) * %sec_hour% ;
       report_tech('Storage negative reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto) = sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_do,sto,h)) * phi_reserves_call(reserves_do,h)) * %sec_hour% ;

                 report_tech('Storage positive reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage positive reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage negative reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage negative reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage positive reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage positive reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage negative reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage negative reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
$ontext
$offtext

%reserves%        report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) > eps_rep_ins) = report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) ;
%reserves%         report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
%reserves%        report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto) + lev_N_STO_E_PRO(scen,sto) ) > eps_rep_ins) = report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto) + lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% ;
%reserves%         report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;


%reserves%$ontext
        report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto)) > eps_rep_ins) = ( report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)
                        + sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_up,sto,h)) * phi_reserves_call(reserves_up,h)) * %sec_hour%
                        - sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_do,sto,h)) * phi_reserves_call(reserves_do,h)) * %sec_hour%
                        ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto)) ;
        report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto)) > eps_rep_ins) = ( report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)
                        + sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_up,sto,h)) * phi_reserves_call(reserves_up,h)) * %sec_hour%
                        - sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_do,sto,h)) * phi_reserves_call(reserves_do,h)) * %sec_hour%
                        ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto)) * %sec_hour% ;

         report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
         report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
$ontext
$offtext

%prosumage%$ontext
%reserves%        report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) > eps_rep_ins) = report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) ;
%reserves%         report_prosumage_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
%reserves%        report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto)) > eps_rep_ins) = report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto)) * %sec_hour% ;
%reserves%         report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;

                 report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,res) > eps_rep_ins)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h)) + lev_G_RES_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,res) ) ;
                         report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,res)$(report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_abs) = 0 ;

         report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) ) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h) - corr_fac_res(scen,res,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES(scen,res) ) ;
         report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,ct_the)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_the)) ;
         report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_CON(scen,ct_ren)) ;

                 report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res) < eps_rep_abs) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,ct_the)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,ct_the) < eps_rep_abs) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,ct_ren)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,ct_ren) < eps_rep_abs) = 0 ;

%reserves%        report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto)) > eps_rep_ins) = report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto)) ;
%reserves%         report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
%reserves%        report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto)) > eps_rep_ins) = report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto)) * %sec_hour% ;
%reserves%         report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_abs) = 0 ;
$ontext
$offtext

        report_tech('Storage EP-ratio',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto) + lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto) + lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) ;
         report_tech('Storage EP-ratio',loop_res_share,loop_ev,loop_prosumage,sto)$(report_tech('Storage EP-ratio',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;

%prosumage%$ontext
        report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto) ) ;
         report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_ev,loop_prosumage,sto)$(report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;

        report_market_tech('Storage EP-ratio market',loop_res_share,loop_ev,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,sto) ) ;
         report_market_tech('Storage EP-ratio market',loop_res_share,loop_ev,loop_prosumage,sto)$(report_market_tech('Storage EP-ratio market',loop_res_share,loop_ev,loop_prosumage,sto) < eps_rep_rel) = 0 ;

$ontext
$offtext

%EV%$ontext
        report_tech('EV charge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum((h,ev), report_tech_hours('EV charge',loop_res_share,loop_ev,loop_prosumage,ev,h) ) * %sec_hour% ;
        report_tech('EV discharge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum((h,ev), report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h) ) * %sec_hour% ;
        report_tech('EV phevfuel total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum((h,ev), report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) ) * %sec_hour% ;
        report_tech('EV electrical total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) ) * %sec_hour% ;

         report_tech('EV charge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV charge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
         report_tech('EV discharge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV discharge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
         report_tech('EV phevfuel total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV phevfuel total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
         report_tech('EV electrical total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV electrical total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;

       report_tech('EV share of electrical consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h)) > card(h)*eps_rep_abs) = sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) ) * %sec_hour% / (sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) ) ) ;
       report_tech('EV share of phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h)) > card(h)*eps_rep_abs) = sum((h,ev), report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) ) * %sec_hour% / (sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h) ) ) ;

         report_tech('EV share of electrical consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV share of electrical consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_rel) = 0 ;
         report_tech('EV share of phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV share of phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_rel) = 0 ;

%reserves%$ontext
        report_tech('EV positive reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum( (ev,h,reserves_up) ,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves_up,ev,h)) * phi_reserves_call(reserves_up,h)) * %sec_hour% ;
        report_tech('EV negative reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum( (ev,h,reserves_do) ,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves_do,ev,h)) * phi_reserves_call(reserves_do,h)) * %sec_hour% ;
        report_tech('EV positive reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum( (ev,h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves_up,ev,h)) * phi_reserves_call(reserves_up,h)) * %sec_hour% ;
        report_tech('EV negative reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') = sum( (ev,h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves_do,ev,h)) * phi_reserves_call(reserves_do,h)) * %sec_hour% ;

         report_tech('EV positive reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV positive reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
         report_tech('EV negative reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV negative reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
         report_tech('EV positive reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV positive reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
         report_tech('EV negative reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum')$(report_tech('EV negative reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum') < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
$ontext
$offtext


%reserves%$ontext
         report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h) + corr_fac_con(scen,ct_the,h))) > card(h)*eps_rep_abs)= sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h) + corr_fac_con(scen,ct_the,h)) ) ;
         report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h) + corr_fac_con(scen,ct_ren,h))) > card(h)*eps_rep_abs)= sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h) + corr_fac_con(scen,ct_ren,h)) ) ;
         report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h) + corr_fac_con(scen,ct_the,h))) > card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h)) * phi_reserves_call(reserves,h)) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_the,h) + corr_fac_con(scen,ct_the,h)) ) ;
         report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h) + corr_fac_con(scen,ct_ren,h))) > card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h)) * phi_reserves_call(reserves,h)) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,ct_ren,h) + corr_fac_con(scen,ct_ren,h)) ) ;
         report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h))) >  card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h)) ) ;
         report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h))) > card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h)) * phi_reserves_call(reserves,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,res,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_up,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_do,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves_up,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves_do,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_up,sto,h)) * phi_reserves_call(reserves_up,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves_do,sto,h)) * phi_reserves_call(reserves_do,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves_up,sto,h)) * phi_reserves_call(reserves_up,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto)$( sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves_do,sto,h)) * phi_reserves_call(reserves_do,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,sto,h)) ) ;

          report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,res) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,res) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves provision ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves provision ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves provision ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves provision ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves provision ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves provision ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves provision ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves provision ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves activation ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves activation ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves activation ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves activation ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves activation ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves activation ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
          report_reserves_tech('Reserves activation ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('Reserves activation ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;
$ontext
$offtext


%DSM%$ontext
        report('load curtailment absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage) =  sum((dsm_curt,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,dsm_curt,h))) * %sec_hour% ;
        report('load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage) =  sum((dsm_shift,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,dsm_shift,h))) * %sec_hour% ;
        report('load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage) =  sum((dsm_shift,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,dsm_shift,h))) * %sec_hour% ;
        report_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,dsm_curt)) ;
        report_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,dsm_shift)) ;
        report_tech_hours('load curtailment (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_curt,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,dsm_curt,h)) ;
        report_tech_hours('load shift pos (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,dsm_shift,h)) ;
        report_tech_hours('load shift neg (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,dsm_shift,h)) ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_curt) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,dsm_curt)) / report('Capacity total',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,dsm_shift)) / report('Capacity total',loop_res_share,loop_ev,loop_prosumage) + 1e-9 ;
        report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,dsm_curt) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,dsm_curt,h) - corr_fac_dsm_cu(scen,dsm_curt,h)) )  / report('Energy total',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
        report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,dsm_shift,h) - corr_fac_dsm_shift(scen,dsm_shift,h)) ) / report('Energy total',loop_res_share,loop_ev,loop_prosumage) * %sec_hour% + 1e-9 ;
        report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP(scen,dsm_shift,h)) ) ;
        report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = sum( (h,hh) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO(scen,dsm_shift,h,hh)) ) ;
        report_tech('Load shift pos absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,dsm_shift,h)) ) ;
        report_tech('Load shift neg absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,dsm_shift,h)) ) ;
        report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) - report_tech('Load shift pos absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) ;
        report_tech('Load shift neg absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) = report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) - report_tech('Load shift neg absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) ;

         report('load curtailment absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage)$(report('load curtailment absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage) < card(h)*eps_rep_abs*%sec_hour%) = 0 ;
         report('load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage)$(report('load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage) < card(h)*eps_rep_abs*%sec_hour%) = 0 ;
         report('load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage)$(report('load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage) < card(h)*eps_rep_abs*%sec_hour%) = 0 ;
         report_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt)$(report_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt) < eps_rep_ins) = 0 ;
         report_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < eps_rep_ins) = 0 ;
         report_tech_hours('load curtailment (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_curt,h)$(report_tech_hours('load curtailment (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_curt,h) < eps_rep_abs) = 0 ;
         report_tech_hours('load shift pos (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h)$(report_tech_hours('load shift pos (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h) < eps_rep_abs) = 0 ;
         report_tech_hours('load shift neg (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h)$(report_tech_hours('load shift neg (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h) < eps_rep_abs) = 0 ;
         report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_curt)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_curt) < eps_rep_rel) = 0 ;
         report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < eps_rep_rel) = 0 ;
         report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,dsm_curt)$(report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,dsm_curt) < eps_rep_rel) = 0 ;
         report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Energy share',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < eps_rep_rel) = 0 ;
         report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < card(h)*eps_rep_abs) = 0 ;
         report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < card(h)*eps_rep_abs) = 0 ;
         report_tech('Load shift pos absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Load shift pos absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < card(h)*eps_rep_abs) = 0 ;
         report_tech('Load shift neg absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Load shift neg absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < card(h)*eps_rep_abs) = 0 ;
         report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < card(h)*eps_rep_abs) = 0 ;
         report_tech('Load shift neg absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift)$(report_tech('Load shift neg absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift) < card(h)*eps_rep_abs) = 0 ;
$ontext
$offtext

%reserves%$ontext
        report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves)$(ord(reserves) < 3) = phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres) + lev_N_RES_PRO(scen,resres)))/1000) )) ) ;
        report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves)$(ord(reserves) > 2) = 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres) + lev_N_RES_PRO(scen,resres)))/1000) ) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(ord(reserves) < 3) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h))) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  ((card(h)-1) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_n_RES_PRO(scen,resres)))/1000) )) )) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(ord(reserves) < 3) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h))) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  ((card(h)-1) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_n_RES_PRO(scen,resres)))/1000) )) )) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(ord(reserves) < 3) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h))) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  ((card(h)-1) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_n_RES_PRO(scen,resres)))/1000) )) )) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(ord(reserves) < 3) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves,sto,h))) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  ((card(h)-1) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_n_RES_PRO(scen,resres)))/1000) )) ));

         report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves)$(report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves) < eps_rep_abs) = 0 ;
         report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves)$(report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves) < eps_rep_abs) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,res) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;

%EV%$ontext
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$(ord(reserves) < 3) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h))) / (phi_reserves_pr * sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  ((card(h)-1) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_n_RES_PRO(scen,resres)))/1000) )) ));
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$(ord(reserves) < 3) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h))) / (phi_reserves_pr * sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  ((card(h)-1) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ));
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$(ord(reserves) < 3) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))) / (phi_reserves_pr * sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  ((card(h)-1) * 1000 * phi_reserves_share(reservesreserves) * (reserves_intercept(reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ));

         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum') < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum') < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum') < eps_rep_rel) = 0 ;
$ontext
$offtext

%reserves%$ontext
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$( ord(reserves) > 2 ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h))) / ((card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) ) ) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$( ord(reserves) > 2 ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h))) / ((card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) ) ) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$( ord(reserves) > 2 ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h))) / ((card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$( ord(reserves) > 2 ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves,sto,h))) / ((card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;

         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,res) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;

%EV%$ontext
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$( ord(reserves) > 2 ) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h))) / ((card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$( ord(reserves) > 2 ) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h))) / ((card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$( ord(reserves) > 2 ) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))) / ((card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;

         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum') < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum') < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum') < eps_rep_rel) = 0 ;
$ontext
$offtext

%reserves%$ontext
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(ord(reserves) < 3) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(ord(reserves) < 3) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(ord(reserves) < 3) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(ord(reserves) < 3) = sum(h,(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves,sto,h)))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000 ) ) )   );

         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,res) < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel) = 0 ;

%EV%$ontext
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$(ord(reserves) < 3) = sum((h,ev),(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h)))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr * sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$(ord(reserves) < 3) = sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr * sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$(ord(reserves) < 3) = sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))*phi_reserves_call(reserves,h)) /  sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr * sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reserves_intercept(reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000 ) ) )   ) ;

         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum') < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum') < eps_rep_rel) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum') < eps_rep_rel) = 0 ;
$ontext
$offtext

%reserves%$ontext
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$( ord(reserves) > 2 ) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$( ord(reserves) > 2 ) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$( ord(reserves) > 2 ) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$( ord(reserves) > 2 ) = sum(h,(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves,sto,h)))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )) ;

         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the) < eps_rep_rel ) = 0;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren) < eps_rep_rel ) = 0;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,res)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,res) < eps_rep_rel ) = 0;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,sto) < eps_rep_rel ) = 0;

%EV%$ontext
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$( ord(reserves) > 2 ) = (sum((h,ev),(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h)))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) ))) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$( ord(reserves) > 2 ) = (sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) ))) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$( ord(reserves) > 2 ) = (sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) ))) ;

         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum')$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum') < eps_rep_rel ) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum')$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'G2V_cum') < eps_rep_rel ) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum')$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,'V2G_cum') < eps_rep_rel ) = 0 ;
$ontext
$offtext

%reserves%$ontext
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h) = (report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves)) ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h) = (phi_reserves_call(reserves,h) * report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves)) ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h)) ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_the,h))*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h)) ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_CON(scen,reserves,ct_ren,h))*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,res,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h)) ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,res,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RES(scen,reserves,res,h))*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h) = (sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves,sto,h))) ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h) = ((sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,reserves,sto,h)))*phi_reserves_call(reserves,h)) ;

         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the,h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the,h) < eps_rep_abs ) = 0;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the,h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ct_the,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren,h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren,h) < eps_rep_abs ) = 0;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren,h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ct_ren,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,res,h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,res,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,res,h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,res,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h) < eps_rep_abs ) = 0 ;

%EV%$ontext
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ev,h) = (sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h)))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ev,h) = ((sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,reserves,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,reserves,ev,h)))*phi_reserves_call(reserves,h))  ;

         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ev,h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,ev,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ev,h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,ev,h) < eps_rep_abs ) = 0 ;
$ontext
$offtext

%DSM%$ontext
%reserves%$ontext
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt)$( ord(reserves) > 2 ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,reserves,dsm_curt,h))) / ( (card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )))  ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift)$( ord(reserves) > 2 ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,reserves,dsm_shift,h))) / ( (card(h)-1) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )))   ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt)$( ord(reserves) > 2 ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,reserves,dsm_curt,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )))  ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift)$( ord(reserves) > 2 ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,reserves,dsm_shift,h)) * phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reserves_intercept(reserves) + sum(resres,reserves_slope(reserves,resres) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_RES(scen,resres)+lev_N_RES_PRO(scen,resres)))/1000) )))   ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,reserves,dsm_shift,h))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,reserves,dsm_shift,h))*phi_reserves_call(reserves,h)  ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,reserves,dsm_curt,h))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,reserves,dsm_curt,h))*phi_reserves_call(reserves,h)  ;

         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt) < eps_rep_rel ) = 0 ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift) < eps_rep_rel ) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt) < eps_rep_rel ) = 0 ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift) < eps_rep_rel ) = 0 ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h) < eps_rep_abs) = 0 ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h) < eps_rep_abs ) = 0 ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h) < eps_rep_abs ) = 0 ;
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
