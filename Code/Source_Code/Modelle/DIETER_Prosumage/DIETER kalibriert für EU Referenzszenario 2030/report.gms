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

$setenv gdxcompress 1




********************************************************************************
**** Parameters for report file  ***********************************************
********************************************************************************

Parameter
corr_fac_dis             Balancing correction factor - dispatchable technologies
corr_fac_nondis          Balancing correction factor - nondispatchable technologies
corr_fac_sto             Balancing correction factor - storage technologies
corr_fac_dsm_cu          Balancing correction factor - DSM curtailment technologies
corr_fac_dsm_shift       Balancing correction factor - DSM shifting technologies
corr_fac_ev              Balancing correction factor - electric vehicles
corr_fac_rsvr            Balancing correction factor - reservoir
corr_fac_sets            Balancing correction factor - SETS
corr_fac_sets_aux        Balancing correction factor - SETS auxiliary DHW
corr_fac_hp              Balancing correction factor - heat pumps
corr_fac_h_elec          Balancing correction factor - hybrid electric storage heating

gross_energy_demand
gross_energy_demand_market
gross_energy_demand_prosumers
gross_energy_demand_prosumers_selfgen
gross_energy_demand_prosumers_market

reserves_activated

calc_maxprice
calc_minprice

report
report_tech
report_tech_hours
report_hours
report_node
report_line
report_cost

report_reserves
report_reserves_hours
report_reserves_tech
report_reserves_tech_hours

report_prosumage
report_prosumage_tech
report_prosumage_tech_hours
report_market
report_market_tech
report_market_tech_hours

report_heat_tech_hours
report_heat_tech
;




********************************************************************************
**** Initialize reporting paremetrs  *******************************************
********************************************************************************

* Set reporting sensitivity. All results below will be reported as zero
Scalar eps_rep_rel Sensitivity for shares defined between 0 and 1        / 1e-4 / ;
Scalar eps_rep_abs Sensitivity for absolute values - e.g. hourly         / 1e-2 / ;
Scalar eps_rep_ins Sensitivity for absolute values - e.g. installed MW   / 1 /    ;


* ----------------------------------------------------------------------------

* Min and max for prices
calc_maxprice = 0 ;
calc_minprice = 1000 ;


* ----------------------------------------------------------------------------

* Default values for correction factors
corr_fac_dis(scen,n,dis,h) = 0 ;
corr_fac_nondis(scen,n,nondis,h) = 0 ;
corr_fac_sto(scen,n,sto,h) = 0 ;
corr_fac_dsm_cu(scen,n,dsm_curt,h) = 0 ;
corr_fac_dsm_shift(scen,n,dsm_shift,h) = 0 ;
corr_fac_ev(scen,n,h) = 0 ;
corr_fac_rsvr(scen,n,rsvr,h) = 0 ;
corr_fac_sets(scen,n,bu,ch,h) = 0 ;
corr_fac_sets_aux(scen,n,bu,ch,h) = 0 ;
corr_fac_hp(scen,n,bu,ch,h) = 0 ;
corr_fac_h_elec(scen,n,bu,ch,h) = 0 ;


* ----------------------------------------------------------------------------

* Parameter for hourly nodal reserves activated
reserves_activated(scen,n,h) = 0 ;
%reserves_endogenous%$ontext
reserves_activated(scen,n,h)$(ord(h) > 1) =
feat_node('reserves',n) * (
sum( reserves_nonprim_up , phi_reserves_call(n,reserves_nonprim_up,h) * 1000 * phi_reserves_share(n,reserves_nonprim_up) * ( reserves_intercept(n,reserves_nonprim_up) + sum( nondis , reserves_slope(n,reserves_nonprim_up,nondis) * (lev_N_TECH(scen,n,nondis) + lev_N_RES_PRO(scen,n,nondis)))/1000))
- sum( reserves_nonprim_do , phi_reserves_call(n,reserves_nonprim_do,h) * 1000 * phi_reserves_share(n,reserves_nonprim_do) * ( reserves_intercept(n,reserves_nonprim_do) + sum( nondis , reserves_slope(n,reserves_nonprim_do,nondis) * (lev_N_TECH(scen,n,nondis) + lev_N_RES_PRO(scen,n,nondis)))/1000))
) ;
$ontext
$offtext

%reserves_exogenous%$ontext
reserves_activated(scen,n,h)$(ord(h) > 1) =
feat_node('reserves',n) * (
sum( reserves_nonprim_up , phi_reserves_call(n,reserves_nonprim_up,h) * reserves_exogenous(n,reserves_nonprim_up,h))
- sum( reserves_nonprim_do , phi_reserves_call(n,reserves_nonprim_do,h) * reserves_exogenous(n,reserves_nonprim_do,h))
) ;
$ontext
$offtext



* ----------------------------------------------------------------------------

*Determine balancing correction factors
%reserves%$ontext
        corr_fac_dis(scen,n,tech,h) =
          sum( reserves_do ,  lev_RP_DIS(scen,n,reserves_do,tech,h) * phi_reserves_call(n,reserves_do,h))
        - sum( reserves_up ,  lev_RP_DIS(scen,n,reserves_up,tech,h) * phi_reserves_call(n,reserves_up,h))
;
        corr_fac_nondis(scen,n,tech,h) =
          sum( reserves_do ,  lev_RP_NONDIS(scen,n,reserves_do,tech,h) * phi_reserves_call(n,reserves_do,h))
        - sum( reserves_up ,  lev_RP_NONDIS(scen,n,reserves_up,tech,h) * phi_reserves_call(n,reserves_up,h))
;
        corr_fac_sto(scen,n,sto,h) =
           sum( reserves_do , phi_reserves_call(n,reserves_do,h) * ( lev_RP_STO_IN(scen,n,reserves_do,sto,h) + lev_RP_STO_OUT(scen,n,reserves_do,sto,h)) )
         - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * ( lev_RP_STO_IN(scen,n,reserves_up,sto,h) + lev_RP_STO_OUT(scen,n,reserves_up,sto,h)) )
;
        corr_fac_rsvr(scen,n,rsvr,h) =
          sum( reserves_do ,  lev_RP_RSVR(scen,n,reserves_do,rsvr,h) * phi_reserves_call(n,reserves_do,h))
        - sum( reserves_up ,  lev_RP_RSVR(scen,n,reserves_up,rsvr,h) * phi_reserves_call(n,reserves_up,h))
;

%DSM%$ontext
        corr_fac_dsm_cu(scen,n,dsm_curt,h) =
       - sum( reserves_up , lev_RP_DSM_CU(scen,n,reserves_up,dsm_curt,h) * phi_reserves_call(n,reserves_up,h) )
;
        corr_fac_dsm_shift(scen,n,dsm_shift,h) =
          sum( reserves_do ,  lev_RP_DSM_SHIFT(scen,n,reserves_do,dsm_shift,h) * phi_reserves_call(n,reserves_do,h))
        - sum( reserves_up ,  lev_RP_DSM_SHIFT(scen,n,reserves_up,dsm_shift,h) * phi_reserves_call(n,reserves_up,h))
;
$ontext
$offtext
%EV%$ontext
%reserves%$ontext
         corr_fac_ev(scen,n,h) = sum( ev ,
         + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * (lev_RP_EV_G2V(scen,n,reserves_do,ev,h) + lev_RP_EV_V2G(scen,n,reserves_do,ev,h)))
         - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * (lev_RP_EV_G2V(scen,n,reserves_up,ev,h) + lev_RP_EV_V2G(scen,n,reserves_up,ev,h))) )
;
$ontext
$offtext

%heat%$ontext
%reserves%$ontext
         corr_fac_sets(scen,n,bu,ch,h) =
        + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * ( theta_sets(n,bu,ch) * lev_RP_SETS(scen,n,reserves_do,bu,ch,h) ))
        - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * ( theta_sets(n,bu,ch) * lev_RP_SETS(scen,n,reserves_up,bu,ch,h) ))
;
         corr_fac_sets_aux(scen,n,bu,ch,h) =
        + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * ( theta_sets(n,bu,ch) * lev_RP_SETS_AUX(scen,n,reserves_do,bu,ch,h) ))
        - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * ( theta_sets(n,bu,ch) * lev_RP_SETS_AUX(scen,n,reserves_up,bu,ch,h) ))
;
         corr_fac_hp(scen,n,bu,ch,h) =
        + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * ( theta_hp(n,bu,ch) * lev_RP_HP(scen,n,reserves_do,bu,ch,h) ))
        - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * ( theta_hp(n,bu,ch) * lev_RP_HP(scen,n,reserves_up,bu,ch,h) ))
;
         corr_fac_h_elec(scen,n,bu,ch,h) =
        + sum( reserves_do , phi_reserves_call(n,reserves_do,h) * ( theta_storage(n,bu,ch) * lev_RP_H_ELEC(scen,n,reserves_do,bu,ch,h) ))
        - sum( reserves_up , phi_reserves_call(n,reserves_up,h) * ( theta_storage(n,bu,ch) * lev_RP_H_ELEC(scen,n,reserves_up,bu,ch,h) ))
;
$ontext
$offtext


* ----------------------------------------------------------------------------

* Define gross energy demand for reporting
gross_energy_demand(scen,n) = sum( h , d(n,h) + sum( sto , lev_STO_IN(scen,n,sto,h) - lev_STO_OUT(scen,n,sto,h) )
%prosumage%$ontext
         + sum( sto , sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h) ) + lev_STO_IN_M2PRO(scen,n,sto,h) + lev_STO_IN_M2M(scen,n,sto,h) - lev_STO_OUT_PRO2PRO(scen,n,sto,h) - lev_STO_OUT_PRO2M(scen,n,sto,h) - lev_STO_OUT_M2PRO(scen,n,sto,h) - lev_STO_OUT_M2M(scen,n,sto,h) )
$ontext
$offtext
%DSM%$ontext
         - sum( dsm_curt , lev_DSM_CU(scen,n,dsm_curt,h) )
         + sum( dsm_shift , lev_DSM_UP(scen,n,dsm_shift,h) - sum( hh$( ord(hh) >= ord(h) - t_dur_dsm_shift(n,dsm_shift) AND ord(hh) <= ord(h) + t_dur_dsm_shift(n,dsm_shift) ) , lev_DSM_DO(scen,n,dsm_shift,h,hh)) )
$ontext
$offtext
%EV%$ontext
*         + sum( ev , lev_EV_CHARGE(scen,n,ev,h) - lev_EV_DISCHARGE(scen,n,ev,h) )
* ###
         + sum( (res,ev) , lev_EV_CHARGE_M(scen,n,ev,h) + lev_EV_CHARGE_PRO(scen,n,res,ev,h) - lev_EV_DISCHARGE(scen,n,ev,h) )
$ontext
$offtext
%reserves%$ontext
         + reserves_activated(scen,n,h)
       + sum( sto , corr_fac_sto(scen,n,sto,h) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
       - sum( (dsm,reserves_up) , lev_RP_DSM_CU(scen,n,reserves_up,dsm,h) * phi_reserves_call(n,reserves_up,h) )
$ontext
$offtext
%reserves%$ontext
%EV%$ontext
       + sum( ev , corr_fac_ev(scen,n,h) )
$ontext
$offtext
%heat%$ontext
*        + sum( (bu,ch) , theta_dir(n,bu,ch) * lev_H_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * lev_H_SETS_IN(scen,n,bu,ch,h) + theta_hp(n,bu,ch) * lev_H_HP_IN(scen,n,bu,ch,h) + theta_elec(n,bu,ch) * lev_H_ELECTRIC_IN(scen,n,bu,ch,h) )
*        + sum( (bu,ch) , theta_dir(n,bu,ch) * lev_H_DHW_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * lev_H_DHW_AUX_ELEC_IN(scen,n,bu,ch,h) )
* ### NEW:
        + sum( (res,bu,ch) , theta_sets(n,bu,ch) * lev_H_SETS_IN(scen,n,res,bu,ch,h) )
$ontext
$offtext
%heat%$ontext
%reserves%$ontext
*        + sum( (bu,ch) , corr_fac_sets(scen,n,bu,ch,h) + corr_fac_sets_aux(scen,n,bu,ch,h) + corr_fac_hp(scen,n,bu,ch,h) + corr_fac_h_elec(scen,n,bu,ch,h) )
* ### new
        + sum( (bu,ch) , corr_fac_sets(scen,n,bu,ch,h)
$ontext
$offtext
)
;

* ----------------------------------------------------------------------------

* Prepare prosumage reporting parameters
%prosumage%$ontext
gross_energy_demand_prosumers(scen,n) = sum( h , phi_pro_load(n) * d(n,h)
%prosumage%$ontext
%heat%$ontext
+ sum( (res,bu,ch) , theta_sets(n,bu,ch) * (lev_H_SETS_IN(scen,n,res,bu,ch,h) ) )
* + lev_H_DHW_AUX_ELEC_IN(scen,n,bu,ch,h)
$ontext
$offtext
%prosumage%$ontext
%EV%$ontext
+ sum( (res,ev) , EV_CHARGE_PRO(n,res,ev,h) )
$ontext
$offtext
%prosumage%$ontext
) ;
$ontext
$offtext
gross_energy_demand_prosumers_selfgen(scen,n) = sum( (h,res) , lev_G_RES_PRO(scen,n,res,h)) + sum( (sto,h) , lev_STO_OUT_PRO2PRO(scen,n,sto,h) ) + sum( (bu,ch,h) , theta_sets(n,bu,ch) * (lev_H_SETS_OUT(scen,n,bu,ch,h) + lev_H_DHW_AUX_OUT(scen,n,bu,ch,h)) ) ;
gross_energy_demand_prosumers_market(scen,n) = sum( h , lev_G_MARKET_M2PRO(scen,n,h)) + sum( (sto,h) , lev_STO_OUT_M2PRO(scen,n,sto,h) ) ;
gross_energy_demand_market(scen,n) = gross_energy_demand(scen,n) - gross_energy_demand_prosumers_selfgen(scen,n) ;
$ontext
$offtext



********************************************************************************
**** Report  *******************************************************************
********************************************************************************

* RPEORT model statistics
        report('model status',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , modstats(scen,'modelstat')) ;
        report('solve time',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , modstats(scen,'resusd')) ;
        report('obj value',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_Z(scen) * %sec_hour%) ;
        report('obj value w/o sectorcpl',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_Z(scen) - sum(h, lev_AUX_HEAT(scen,'DE','bu7','setsh',h)) * 32.72733 - sum(h, lev_EV_PHEVFUEL(scen,'DE','ev43',h) * 50.51778) ) * %sec_hour% ) ;
%prosumage%              report('obj value plus heat costs',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (lev_Z(scen) +  sum( h , dh('DE','bu7','setsh',h) * 32.72733 ) ) * %sec_hour% ) ;

* ----------------------------------------------------------------------------

* REPORT HOURS
        report_hours('demand consumers',loop_res_share,loop_ev,loop_prosumage,h,n) = d(n,h) ;
        report_hours('energy generated',loop_res_share,loop_ev,loop_prosumage,h,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis , lev_G_L(scen,n,dis,h) ) + sum( nondis , lev_G_RES(scen,n,nondis,h) - corr_fac_nondis(scen,n,nondis,h)) + sum( sto , lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h)) + sum( dsm_shift, lev_DSM_DO_DEMAND(scen,n,dsm_shift,h) - corr_fac_dsm_shift(scen,n,dsm_shift,h)) + sum( ev , lev_EV_DISCHARGE(scen,n,ev,h)) - corr_fac_ev(scen,n,h) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)) + sum( res , lev_G_RES_PRO(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h))) + sum( sto , lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h)) ) ;
        report_hours('infeasibility',loop_res_share,loop_ev,loop_prosumage,h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_INFES(scen,n,h) ) ;
*        report_hours('energy demanded',loop_res_share,loop_ev,loop_prosumage,h,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , d(n,h) - sum( dsm_curt , lev_DSM_CU(scen,n,dsm_curt,h) - corr_fac_dsm_cu(scen,n,dsm_curt,h)) + sum( sto , lev_STO_IN(scen,n,sto,h)) + sum( dsm_shift, lev_DSM_UP_DEMAND(scen,n,dsm_shift,h)) + reserves_activated(scen,n,h) + sum( ev , lev_EV_CHARGE(scen,n,ev,h)) +  sum( sto , sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_STO_IN_M2PRO(scen,n,sto,h) + lev_STO_IN_M2M(scen,n,sto,h)) + sum( (bu,ch) , theta_dir(n,bu,ch) * ( lev_H_DIR(scen,n,bu,ch,h) + lev_H_DHW_DIR(scen,n,bu,ch,h) ) + theta_sets(n,bu,ch) * lev_H_SETS_IN(scen,n,bu,ch,h) + theta_hp(n,bu,ch) * lev_H_HP_IN(scen,n,bu,ch,h) + theta_elec(n,bu,ch) * lev_H_STO_IN_ELECTRIC(scen,n,bu,ch,h)) ) ;
        report_hours('gross exports',loop_res_share,loop_ev,loop_prosumage,h,n) = sum( l , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , -min(inc(l,n) * lev_F(scen,l,h),0) )) ;
        report_hours('gross imports',loop_res_share,loop_ev,loop_prosumage,h,n) = sum( l , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , max(inc(l,n) * lev_F(scen,l,h),0) ) ) ;
        report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) = - sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con1a(scen,n,h)) ;

                 report_hours('energy generated',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('energy generated',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs ) = 0 ;
                 report_hours('energy demanded',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('energy demanded',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs ) = 0 ;
                 report_hours('gross exports',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('gross exports',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs ) = 0 ;
                 report_hours('gross imports',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('gross imports',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs ) = 0 ;
                 report_hours('demand consumers',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('demand consumers',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs) = 0 ;
                 report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs AND report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) > -eps_rep_abs) = eps ;
                 report_hours('infeasibility',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('infeasibility',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

* REPORT TECH HOURS
        report_tech_hours('generation conventional',loop_res_share,loop_ev,loop_prosumage,con,h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h) + corr_fac_dis(scen,n,con,h)) ;
        report_tech_hours('infeasibility',loop_res_share,loop_ev,loop_prosumage,'',h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_INFES(scen,n,h) ) ;
        report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,res,h,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + corr_fac_dis(scen,n,res,h) + lev_G_RES(scen,n,res,h) + lev_G_RES_PRO(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) ) ;
        report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) ) ;
        report_tech_hours('reservoir inflow',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , rsvr_in(n,rsvr,h)/1000 * lev_N_RSVR_E(scen,n,rsvr) ) ;
        report_tech_hours('reservoir level',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_L(scen,n,rsvr,h) ) ;
        report_tech_hours('curtailment of fluct res',loop_res_share,loop_ev,loop_prosumage,res,h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h) + lev_CU_PRO(scen,n,res,h) ) ;
        report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h) ) ;
        report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_STO_IN_M2PRO(scen,n,sto,h) + lev_STO_IN_M2M(scen,n,sto,h) ) ;
        report_tech_hours('storage level',loop_res_share,loop_ev,loop_prosumage,sto,h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L(scen,n,sto,h) + lev_STO_L_PRO(scen,n,sto,h) ) ;
        report_tech_hours('netto exports',loop_res_share,loop_ev,loop_prosumage,'',h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - sum( l , inc(l,n) * lev_F(scen,l,h)) ) ;

                 report_tech_hours('generation conventional',loop_res_share,loop_ev,loop_prosumage,con,h,n)$(report_tech_hours('generation conventional',loop_res_share,loop_ev,loop_prosumage,con,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n)$(report_tech_hours('generation renewable',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('curtailment of fluct res',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_tech_hours('curtailment of fluct res',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('storage level',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_tech_hours('storage level',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('netto exports',loop_res_share,loop_ev,loop_prosumage,'',h,n)$(abs(report_tech_hours('netto exports',loop_res_share,loop_ev,loop_prosumage,'',h,n)) < eps_rep_abs ) = 0 ;
                 report_tech_hours('infeasibility',loop_res_share,loop_ev,loop_prosumage,'',h,n)$(report_tech_hours('infeasibility',loop_res_share,loop_ev,loop_prosumage,'',h,n) < eps_rep_abs ) = 0 ;
                 report_tech_hours('reservoir inflow',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n)$(report_tech_hours('reservoir inflow',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('reservoir level',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n)$(report_tech_hours('reservoir level',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n) < eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

* RPEORT LINE
         report_line('NTC',loop_res_share,loop_ev,loop_prosumage,l) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_NTC(scen,l) ) ;
                         report_line('NTC',loop_res_share,loop_ev,loop_prosumage,l)$(report_line('NTC',loop_res_share,loop_ev,loop_prosumage,l) < eps_rep_ins ) = 0 ;

         report_line('average line use',loop_res_share,loop_ev,loop_prosumage,l)$report_line('NTC',loop_res_share,loop_ev,loop_prosumage,l) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h, abs(lev_F(scen,l,h)))) / sum( h , report_line('NTC',loop_res_share,loop_ev,loop_prosumage,l)) ;
                 report_line('avergae line use',loop_res_share,loop_ev,loop_prosumage,l)$(report_line('average line use',loop_res_share,loop_ev,loop_prosumage,l) < eps_rep_rel) = 0 ;


* ----------------------------------------------------------------------------

* REPORT NODE
        report_node('energy demand total',loop_res_share,loop_ev,loop_prosumage,n) = (sum( h , d(n,h) ) + sum( (sto,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_STO_IN_M2PRO(scen,n,sto,h) + lev_STO_IN_M2M(scen,n,sto,h)) )) * %sec_hour%
%DSM%$ontext
        + sum( (dsm_shift,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,n,dsm_shift,h)) ) * %sec_hour%
$ontext
$offtext
%EV%$ontext
* ###
*         + sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_CHARGE(scen,n,ev,h)) ) * %sec_hour%
$ontext
$offtext
%reserves%$ontext
         + sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , reserves_activated(scen,n,h)))
$ontext
$offtext
%heat%$ontext
* ###
*        + sum( (bu,ch,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_dir(n,bu,ch) * ( lev_H_DIR(scen,n,bu,ch,h) + lev_H_DHW_DIR(scen,n,bu,ch,h) ) + theta_sets(n,bu,ch) * lev_H_SETS_IN(scen,n,bu,ch,h) + theta_hp(n,bu,ch) * lev_H_HP_IN(scen,n,bu,ch,h) + theta_elec(n,bu,ch) * lev_H_STO_IN_ELECTRIC(scen,n,bu,ch,h)) )
$ontext
$offtext
;
        report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen,n)) ;
        report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis , lev_G_L(scen,n,dis,h)) + sum( nondis , lev_G_RES(scen,n,nondis,h) - corr_fac_nondis(scen,n,nondis,h)) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)) + sum( res , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res) - lev_CU_PRO(scen,n,res,h)) )) ;
        report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis , lev_G_L(scen,n,dis,h)) + sum( nondis , lev_G_RES(scen,n,nondis,h) - corr_fac_nondis(scen,n,nondis,h)) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h)- corr_fac_rsvr(scen,n,rsvr,h)) + sum( sto , lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h)) + sum( dsm_shift , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h) - corr_fac_dsm_shift(scen,n,dsm_shift,h)) + sum( dsm_curt , lev_DSM_CU(scen,n,dsm_curt,h)) + sum( ev , lev_EV_DISCHARGE(scen,n,ev,h)) - corr_fac_ev(scen,n,h) +  sum( res , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res) - lev_CU_PRO(scen,n,res,h)) + sum( sto , lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h)) )) ;
        report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n) = sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - inc(l,n) * lev_F(scen,l,h)) ) ;
        report_node('gross exports',loop_res_share,loop_ev,loop_prosumage,n) = sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , -min(inc(l,n) * lev_F(scen,l,h),0) ) ) ;
        report_node('gross imports',loop_res_share,loop_ev,loop_prosumage,n) = sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , max(inc(l,n) * lev_F(scen,l,h),0) ) ) ;
        report_node('net import share in gross demand',loop_res_share,loop_ev,loop_prosumage,n) = -min(report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n),0)/report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n) ;
        report_node('net export share in net generation',loop_res_share,loop_ev,loop_prosumage,n) = max(report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n),0)/report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) ;
        report_node('trade capacity',loop_res_share,loop_ev,loop_prosumage,n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( l , abs(inc(l,n) * lev_NTC(scen,l))) ) ;
*        report_node('gross import share',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , report_hours('gross imports',loop_res_share,loop_ev,loop_prosumage,h,n))/ sum( h , report_hours('energy demanded',loop_res_share,loop_ev,loop_prosumage,h,n) ) ;
        report_node('gross export share in net generation',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , report_hours('gross exports',loop_res_share,loop_ev,loop_prosumage,h,n))/sum( h , report_hours('energy generated',loop_res_share,loop_ev,loop_prosumage,h,n) ) ;
        report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) = sum( tech , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,tech))) + sum( res , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res))) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) + lev_N_STO_P_PRO(scen,n,sto) )) + sum( rsvr , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr)))
%DSM%$ontext
        + sum( dsm_curt , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt)) ) + sum( dsm_shift , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift)) )
$ontext
$offtext
;
        report_node('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,n) = sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,n,nondis,h) + lev_CU(scen,n,nondis,h))) * %sec_hour% ;
        report_node('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,n)$(sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(n,nondis,h) * (lev_N_TECH(scen,n,nondis)))) + sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res))) > eps_rep_abs*card(res)*card(h)) = (sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,n,res,h))) + sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,nondis,h)))) * %sec_hour% / (sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res))) + sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(n,nondis,h) * (lev_N_TECH(scen,n,nondis))))) ;
        report_node('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage,n)$(m_e(n,'bio')) = (m_e(n,'bio') - sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,'bio',h)))) * %sec_hour% ;
        report_node('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage,n)$(m_e(n,'bio')) = (m_e(n,'bio') - sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,'bio',h)))) / m_e(n,'bio') ;
        report_node('max price',loop_res_share,loop_ev,loop_prosumage,n)$sum(h,d(n,h)) = max( calc_maxprice , smax( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - marginal_con1a(scen,n,h))) ) ;
        report_node('min price',loop_res_share,loop_ev,loop_prosumage,n)$sum(h,d(n,h)) = min( calc_minprice , smin( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - marginal_con1a(scen,n,h))) ) ;
        report_node('mean price',loop_res_share,loop_ev,loop_prosumage,n)$sum(h,d(n,h)) = -sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con1a(scen,n,h)))/card(h) ;

                 report_node('energy demand total',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('energy demand total',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_node('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_node('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_node('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_node('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_node('min price',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('min price',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs AND report_node('min price',loop_res_share,loop_ev,loop_prosumage,n) > -eps_rep_abs) = eps ;
                 report_node('gross exports',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('gross exports',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs*card(h)) = 0 ;
                 report_node('gross imports',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('gross imports',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs*card(h)) = 0 ;
                 report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n)$(abs(report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n)) < eps_rep_abs*card(h)) = 0 ;
                 report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs*card(h)) = 0 ;
                 report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs*card(h)) = 0 ;
                 report_node('net import share in gross demand',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('net import share in gross demand',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_node('net export share in net generation',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('net export share in net generation',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_ins) = 0 ;
                 report_node('gross import share',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('gross import share',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel ) = 0 ;
                 report_node('gross export share in net generation',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('gross export share in net generation',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel ) = 0 ;


* ----------------------------------------------------------------------------

* REPORT COST
        report_cost('Nodal cost: dispatch',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis , c_m(n,dis)*lev_G_L(scen,n,dis,h) + c_up(n,dis)*lev_G_UP(scen,n,dis,h)$(ord(h)>1) + c_do(n,dis)*lev_G_DO(scen,n,dis,h)) + sum( nondis , c_cu(n,nondis)*lev_CU(scen,n,nondis,h)) + sum( sto , c_m_sto(n,sto) * ( lev_STO_OUT(scen,n,sto,h) + lev_STO_IN(scen,n,sto,h) ) )
%DSM%$ontext
                 + sum( dsm_curt , c_m_dsm_cu(n,dsm_curt)*lev_DSM_CU(scen,n,dsm_curt,h) ) + sum( dsm_shift , c_m_dsm_shift(n,dsm_shift)*(lev_DSM_UP_DEMAND(scen,n,dsm_shift,h) + lev_DSM_DO_DEMAND(scen,n,dsm_shift,h)))
$ontext
$offtext
%EV%$ontext
                 + sum( ev , c_m_ev(n,ev) * lev_EV_DISCHARGE(scen,n,ev,h))
$ontext
$offtext
%reserves%$ontext
                 + sum( (reserves_up,sto) , phi_reserves_call(n,reserves_up,h) * c_m_sto(n,sto) * (lev_RP_STO_OUT(scen,n,reserves_up,sto,h) - lev_RP_STO_IN(scen,n,reserves_up,sto,h)) ) - sum( (reserves_do,sto) , phi_reserves_call(n,reserves_do,h) * c_m_sto(n,sto) * (lev_RP_STO_OUT(scen,n,reserves_do,sto,h) - lev_RP_STO_IN(scen,n,reserves_do,sto,h)) ) + sum( (reserves_up,rsvr) , lev_RP_RSVR(scen,n,reserves_up,rsvr,h) * phi_reserves_call(n,reserves_up,h) * c_m_rsvr(n,rsvr) ) - sum( (reserves_do,rsvr) , lev_RP_RSVR(scen,n,reserves_do,rsvr,h) * phi_reserves_call(n,reserves_do,h) * c_m_rsvr(n,rsvr))
$ontext
$offtext
%reserves%$ontext
%EV%$ontext
%EV_EXOG%        + sum( (reserves_up,ev) , lev_RP_EV_V2G(scen,n,reserves_up,ev,h) * phi_reserves_call(n,reserves_up,h) * c_m_ev(n,ev) ) - sum( (reserves_do,ev) , lev_RP_EV_V2G(scen,n,reserves_do,ev,h) * phi_reserves_call(n,reserves_do,h) * c_m_ev(n,ev) )
%EV_EXOG%
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
                 + sum( (reserves_up,dsm_curt) , lev_RP_DSM_CU(scen,n,reserves_up,dsm_curt,h) * phi_reserves_call(n,reserves_up,h) * c_m_dsm_cu(n,dsm_curt) ) + sum( (reserves,dsm_shift) , lev_RP_DSM_SHIFT(scen,n,reserves,dsm_shift,h) * phi_reserves_call(n,reserves,h) * c_m_dsm_shift(n,dsm_shift) )
$ontext
$offtext
));
         report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( tech , c_i(n,tech)*lev_N_TECH(scen,n,tech) + c_fix(n,tech)*lev_N_TECH(scen,n,tech) ) + sum( sto , c_i_sto_e(n,sto)*lev_N_STO_E(scen,n,sto) + c_fix_sto(n,sto)/2*(lev_N_STO_P(scen,n,sto)+lev_N_STO_E(scen,n,sto)) + c_i_sto_p(n,sto)*lev_N_STO_P(scen,n,sto) ) + sum( rsvr , c_i_rsvr_e(n,rsvr)*lev_N_RSVR_E(scen,n,rsvr) + c_i_rsvr_p(n,rsvr)*lev_N_RSVR_P(scen,n,rsvr) + c_fix_rsvr(n,rsvr) * lev_N_RSVR_P(scen,n,rsvr))
%DSM%$ontext
                 + sum( dsm_curt , c_i_dsm_cu(n,dsm_curt)*lev_N_DSM_CU(scen,n,dsm_curt) + c_fix_dsm_cu(n,dsm_curt)*lev_N_DSM_CU(scen,n,dsm_curt) ) + sum( dsm_shift , c_i_dsm_shift(n,dsm_shift)*lev_N_DSM_SHIFT(scen,n,dsm_shift) + c_fix_dsm_shift(n,dsm_shift)*lev_N_DSM_SHIFT(scen,n,dsm_shift) )
$ontext
$offtext
%prosumage%$ontext
                 + sum( res , c_i(n,res)*lev_N_RES_PRO(scen,n,res) + c_fix(n,res)*lev_N_RES_PRO(scen,n,res) ) + sum( sto , c_i_sto_e(n,sto)*lev_N_STO_E_PRO(scen,n,sto) + c_fix_sto(n,sto)/2*(lev_N_STO_P_PRO(scen,n,sto) + lev_N_STO_E_PRO(scen,n,sto)) + c_i_sto_p(n,sto)*lev_N_STO_P_PRO(scen,n,sto)) + sum( (sto,h) , c_m_sto(n,sto) * ( lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h) ) )
$ontext
$offtext
);

         report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , c_infes * lev_G_INFES(scen,n,h)) ) ;
         report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,ev) , pen_phevfuel(n,ev) * lev_EV_PHEVFUEL(scen,n,ev,h)) ) ;
         report_cost('Nodal cost: fossil heating',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (bu,hst,h) , pen_heat_fuel(n,bu,hst) * lev_H_STO_IN_FOSSIL(scen,n,bu,hst,h)) ) ;
         report_cost('Nodal cost: total',loop_res_share,loop_ev,loop_prosumage,n) = report_cost('Nodal cost: dispatch',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: fossil heating',loop_res_share,loop_ev,loop_prosumage,n) ;
         report_cost('Costs lines',loop_res_share,loop_ev,loop_prosumage,l) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , c_i_ntc(l) * lev_NTC(scen,l)*dist(l) ) ;

                 report_cost('Nodal cost: dispatch',loop_res_share,loop_ev,loop_prosumage,n)$(report_cost('Nodal cost: dispatch',loop_res_share,loop_ev,loop_prosumage,n) < card(h) * eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n)$(report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_ins) = 0 ;
                 report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n)$(report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n) < card(h) * eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_ev,loop_prosumage,n)$(report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: fossil heating',loop_res_share,loop_ev,loop_prosumage,n)$(report_cost('Nodal cost: fossil heating',loop_res_share,loop_ev,loop_prosumage,n) < card(h) * eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: total',loop_res_share,loop_ev,loop_prosumage,n)$(report_cost('Nodal cost: total',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_cost('Costs lines',loop_res_share,loop_ev,loop_prosumage,l)$(report_cost('Costs lines',loop_res_share,loop_ev,loop_prosumage,l) < eps_rep_ins) = 0 ;


* ----------------------------------------------------------------------------

* REPORT TECH
        report_tech('capacities conventional',loop_res_share,loop_ev,loop_prosumage,con,n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,con)) ;
        report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,res,n) = 0 + sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res) + lev_N_RES_PRO(scen,n,res)) ;
        report_tech('capacities reservoir MW',loop_res_share,loop_ev,loop_prosumage,rsvr,n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr));
        report_tech('capacities reservoir MWh',loop_res_share,loop_ev,loop_prosumage,rsvr,n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_E(scen,n,rsvr));
        report_tech('capacities storage MW',loop_res_share,loop_ev,loop_prosumage,sto,n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)+ lev_N_STO_P_PRO(scen,n,sto)) ;
        report_tech('capacities storage MWh',loop_res_share,loop_ev,loop_prosumage,sto,n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto)+ lev_N_STO_E_PRO(scen,n,sto)) * %sec_hour% ;

        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,con,n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,con) ) / report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,res,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res)) / report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,rsvr,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr)) / report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,res,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res) + lev_N_RES_PRO(scen,n,res) ) / report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,sto,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) + lev_N_STO_P_PRO(scen,n,sto) ) / report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;

        report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,res,n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + lev_G_RES_PRO(scen,n,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h))) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen,n) ) ;
        report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,rsvr,n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen,n) ) ;
        report_tech('conshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,con,n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen,n) ) ;
        report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,res,n)$report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + lev_G_RES_PRO(scen,n,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h)) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h))) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) ) ;
        report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) ) ;
        report_tech('conshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,con,n)$report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) ) ;

        report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res,n) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h) + lev_CU_PRO(scen,n,res,h) )) * %sec_hour% ;
        report_tech('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res,n) AND sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES_PRO(scen,n,res,h) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h)) ) + sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,n,res,h) + lev_CU(scen,n,res,h))) > card(h)*eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h) + lev_CU_PRO(scen,n,res,h) ))/( sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(n,res,h) * (lev_N_TECH(scen,n,res) + lev_N_RES_PRO(scen,n,res) ))) ) ;

        report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) = sum(h, report_tech_hours('generation storage',loop_res_share,loop_ev,loop_prosumage,sto,h,n) ) * %sec_hour% ;
        report_tech('Storage in total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) = sum(h, report_tech_hours('storage loading',loop_res_share,loop_ev,loop_prosumage,sto,h,n) ) * %sec_hour% ;

        report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,con,n)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,con)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,con)) ;
        report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,res,n)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h)) + lev_G_RES_PRO(scen,n,res,h) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h))) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res)) ;
        report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr)) ;
        report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)) > eps_rep_ins) = ( report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)) ;


                 report_tech('capacities conventional',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_tech('capacities conventional',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_ins) = 0 ;
                 report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_ins) = 0 ;
                 report_tech('capacities storage MW',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('capacities storage MW',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_ins) = 0 ;
                 report_tech('capacities storage MWh',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('capacities storage MWh',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_ins) = 0 ;
                 report_tech('capacities reservoir MW',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('capacities reservoir MW',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_ins) = 0 ;
                 report_tech('capacities reservoir MWh',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('capacities reservoir MWh',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_ins) = 0 ;
                 report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_rel) = 0 ;
                 report_tech('conshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_tech('conshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_rel) = 0 ;
                 report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_rel) = 0 ;
                 report_tech('conshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_tech('conshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_rel) = 0 ;
                 report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_abs) = 0 ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_abs) = 0 ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_abs) = 0 ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage in total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage in total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;


* ----------------------------------------------------------------------------

* REPORT NODE
        report_node('renshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) = sum(res, report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,res,n)) + sum( rsvr , report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,rsvr,n)) ;
        report_node('conshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) = sum(con, report_tech('conshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,con,n) ) ;
        report_node('net import share in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) = - report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen,n) ) ;
        report_node('renshare in nodal net generation',loop_res_share,loop_ev,loop_prosumage,n) = sum(res, report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,res,n)) + sum( rsvr , report_tech('renshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n)) ;
        report_node('conshare in nodal net generation',loop_res_share,loop_ev,loop_prosumage,n) = sum(con, report_tech('conshares in nodal net generation',loop_res_share,loop_ev,loop_prosumage,con,n) ) ;

                 report_node('renshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) $(report_node('renshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel ) = 0 ;
                 report_node('conshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('conshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel ) = 0 ;
                 report_node('renshare in nodal net generation',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('renshare in nodal net generation',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel ) = 0 ;
                 report_node('conshare in nodal net generation',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('conshare in nodal net generation',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel ) = 0 ;



* ----------------------------------------------------------------------------

* REPORT TECH
        report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,con,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h) ) ) ;
        report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,res,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + sum(sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_G_RES_PRO(scen,n,res,h) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h))) ;
        report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,rsvr,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h) ) ) ;
        report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,sto,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h) + lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h)) ) ;

        report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,con,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h) ) ) / report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,res,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + sum(sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_G_RES_PRO(scen,n,res,h) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h))) / report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h) ) ) / report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,sto,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h) + lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h)) ) / report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
%reserves% report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) + lev_N_STO_P_PRO(scen,n,sto) ) > eps_rep_ins) = report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) + lev_N_STO_P_PRO(scen,n,sto) ) ;
%reserves% report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto,n)$(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto) + lev_N_STO_E_PRO(scen,n,sto) ) > eps_rep_ins) = report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto) + lev_N_STO_E_PRO(scen,n,sto) ) * %sec_hour% ;

                 report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_ins ) = 0 ;
                 report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_ins ) = 0 ;
                 report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_ins ) = 0 ;
                 report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_ins ) = 0 ;

                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_rel) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_rel) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;
%reserves%       report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
%reserves%       report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

* RPEORT
        report('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage) = sum( n , report_node('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,n) ) ;
        report('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage) = sum( n , report_node('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage,n) ) / sum((res,h,n), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(n,res,h) * (lev_N_TECH(scen,n,res) + lev_N_RES_PRO(scen,n,res)) )) ;
        report('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage)$(sum( n , m_e(n,'bio'))) = sum( n , report_node('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage,n) ) ;
        report('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage)$(sum( n , m_e(n,'bio'))) = report('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage) / sum( n , m_e(n,'bio')) ;
        report('Capacity total',loop_res_share,loop_ev,loop_prosumage) = sum( n , report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n)) ;
        report('energy demand gross',loop_res_share,loop_ev,loop_prosumage) = sum( n , report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n) ) ;
        report('energy demand total',loop_res_share,loop_ev,loop_prosumage) = sum ( n , report_node('energy demand total',loop_res_share,loop_ev,loop_prosumage,n) ) ;
        report('energy generated net',loop_res_share,loop_ev,loop_prosumage) =  sum( n , report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n)) ;
        report('energy generated gross',loop_res_share,loop_ev,loop_prosumage) =  sum( n , report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n)) ;
*        report('gross trade share',loop_res_share,loop_ev,loop_prosumage) = sum( (h,n) , report_hours('gross imports',loop_res_share,loop_ev,loop_prosumage,h,n))/ sum( (h,n) , report_hours('energy demanded',loop_res_share,loop_ev,loop_prosumage,h,n) ) ;
        report('renshare total',loop_res_share,loop_ev,loop_prosumage) = sum( (h,n) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_L(scen,n,res,h) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h) + phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res) - lev_CU_PRO(scen,n,res,h)) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)))) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( n , gross_energy_demand(scen,n)) ) ;
        report('conshare total',loop_res_share,loop_ev,loop_prosumage) = sum( (h,n) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( con , lev_G_L(scen,n,con,h)) )) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( n, gross_energy_demand(scen,n)) ) ;
        report('Energy total',loop_res_share,loop_ev,loop_prosumage) = sum( n , report_node('Energy total',loop_res_share,loop_ev,loop_prosumage,n) ) ;

                 report('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage)$(report('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs*card(h)) = 0 ;
                 report('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage)$(report('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage)$(report('bio not utilized absolute',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs*card(h)) = 0 ;
                 report('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage)$(report('bio not utilized relative',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel) = 0 ;
                 report('Capacity total',loop_res_share,loop_ev,loop_prosumage)$(report('Capacity total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_ins) = 0 ;
                 report('gross trade share',loop_res_share,loop_ev,loop_prosumage)$(report('gross trade share',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel ) = 0 ;
                 report('renshare total',loop_res_share,loop_ev,loop_prosumage)$(report('renshare total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel ) = 0 ;
                 report('conshare total',loop_res_share,loop_ev,loop_prosumage)$(report('conshare total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_rel ) = 0 ;
                 report('Energy total',loop_res_share,loop_ev,loop_prosumage)$(report('Energy total',loop_res_share,loop_ev,loop_prosumage) < eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

* DSM
%DSM%$ontext
        report_tech_hours('load curtailment (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_curt,h,n)$(feat_node('dsm',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,n,dsm_curt,h)) ;
        report_tech_hours('load shift pos (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h,n)$(feat_node('dsm',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,n,dsm_shift,h)) ;
        report_tech_hours('load shift neg (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h,n)$(feat_node('dsm',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h)) ;

        report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift)) > eps_rep_ins) = sum( (h,hh) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO(scen,n,dsm_shift,h,hh)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift)) ;
        report_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(feat_node('dsm',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt)) ;
        report_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift)) ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(feat_node('dsm',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt)) / report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift)) / report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(feat_node('dsm',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,n,dsm_curt,h) - corr_fac_dsm_cu(scen,n,dsm_curt,h)) )  / report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h) - corr_fac_dsm_shift(scen,n,dsm_shift,h)) ) / report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP(scen,n,dsm_shift,h)) ) ;
        report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n)) = sum( (h,hh) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO(scen,n,dsm_shift,h,hh)) ) ;

        report_node('load curtailment absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('dsm',n)) =  sum((dsm_curt,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,n,dsm_curt,h))) * %sec_hour% ;
        report_node('load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('dsm',n)) =  sum((dsm_shift,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,n,dsm_shift,h))) * %sec_hour% ;
        report_node('load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('dsm',n)) =  sum((dsm_shift,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h))) * %sec_hour% ;

                 report_tech_hours('load curtailment (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_curt,h,n)$(report_tech_hours('load curtailment (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_curt,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('load shift pos (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h,n)$(report_tech_hours('load shift pos (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('load shift neg (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h,n)$(report_tech_hours('load shift neg (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,h,n) < eps_rep_abs) = 0 ;

                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_abs) = 0 ;
                 report_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(report_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n) < eps_rep_ins) = 0 ;
                 report_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_ins) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n) < eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Capacity share',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_rel) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n) < eps_rep_rel) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_rel) = 0 ;
                 report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h)*eps_rep_abs) = 0 ;
                 report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h)*eps_rep_abs) = 0 ;
*                 report_tech('Load shift pos absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift pos absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h)*eps_rep_abs) = 0 ;
*                 report_tech('Load shift neg absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift neg absolute (wholesale)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h)*eps_rep_abs) = 0 ;
*                 report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h)*eps_rep_abs) = 0 ;

                 report_node('load curtailment absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('load curtailment absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n) < card(h)*eps_rep_abs*%sec_hour%) = 0 ;
                 report_node('load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n) < card(h)*eps_rep_abs*%sec_hour%) = 0 ;
                 report_node('load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,n) < card(h)*eps_rep_abs*%sec_hour%) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* EV
%EV%$ontext
* Hier vereinfacht: geht nur, wenn EVs komplett im Prosumage-Segment sind
%prosumage$ontext
*        report_tech_hours('EV charge',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(feat_node('ev',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_CHARGE(scen,n,ev,h)) ;
        report_tech_hours('EV charge market',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(feat_node('ev',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum(res, lev_EV_CHARGE_M(scen,n,ev,h))) ;
        report_tech_hours('EV charge prosumage',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(feat_node('ev',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum(res, lev_EV_CHARGE_PRO(scen,n,res,ev,h))) ;
        report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(feat_node('ev',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_DISCHARGE(scen,n,ev,h)) ;
        report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(feat_node('ev',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_PHEVFUEL(scen,n,ev,h));
        report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(feat_node('ev',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_MPED(scen,n,ev,h));
        report_tech_hours('EV battery level',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(feat_node('ev',n)) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_L(scen,n,ev,h)) ;

        report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n)) = (sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_DISCHARGE(scen,n,ev,h))) - sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , corr_fac_ev(scen,n,h))))/ report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_tech('EV charge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n)) = sum((h,ev), report_tech_hours('EV charge market',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) * %sec_hour% ;
        report_tech('EV charge total prosumage',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n)) = sum((h,ev), report_tech_hours('EV charge prosumage',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) * %sec_hour% ;
        report_tech('EV discharge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n)) = sum((h,ev), report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) * %sec_hour% ;
        report_tech('EV phevfuel total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n)) = sum((h,ev), report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) * %sec_hour% ;
        report_tech('EV electrical total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n)) = sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) * %sec_hour% ;
        report_tech('EV share of electrical consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n) AND sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n)) > card(h)*eps_rep_abs) = sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) * %sec_hour% / (sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) ) ;
        report_tech('EV share of phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n) AND sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n)) > card(h)*eps_rep_abs) = sum((h,ev), report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) * %sec_hour% / (sum((h,ev), report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) + report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) ) ) ;

        report_node('gross_energy_demand w/o EV',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('ev',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen,n)) - sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_MPED(scen,n,ev,h)) ) ;
        report_node('EV electricity demand',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('ev',n)) = sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_MPED(scen,n,ev,h)) ) ;
        report_node('renshare EV',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('ev',n) = (report_node('renshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) - report_node('gross_energy_demand w/o EV',loop_res_share,loop_ev,loop_prosumage,n) * report_node('EV electricity demand',loop_res_share,loop_ev,loop_prosumage,n) ) / report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n) ;
        report_node('conshare EV',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('ev',n) = 1 - report_node('renshare EV',loop_res_share,loop_ev,loop_prosumage,n) ;
%EV_FREE%%EV_DEFAULT%        report_node('renshare EV',loop_res_share,loop_ev,loop_prosumage)$(feat_node('ev',n)) = 1 ;
%EV_FREE%%EV_DEFAULT%        report_node('conshare EV',loop_res_share,loop_ev,loop_prosumage)$(feat_node('ev',n)) = 0 ;

                 report_tech_hours('EV charge market',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(report_tech_hours('EV charge market',loop_res_share,loop_ev,loop_prosumage,ev,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('EV charge prosumage',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(report_tech_hours('EV charge prosumage',loop_res_share,loop_ev,loop_prosumage,ev,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(report_tech_hours('EV discharge',loop_res_share,loop_ev,loop_prosumage,ev,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(report_tech_hours('EV phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(report_tech_hours('EV electrical consumption',loop_res_share,loop_ev,loop_prosumage,ev,h,n) < eps_rep_abs) = 0 ;
                 report_tech_hours('EV battery level',loop_res_share,loop_ev,loop_prosumage,ev,h,n)$(report_tech_hours('EV battery level',loop_res_share,loop_ev,loop_prosumage,ev,h,n) < eps_rep_abs) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_rel) = 0 ;
                 report_tech('EV charge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV charge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV charge total prosumage',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV charge total prosumage',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV discharge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV discharge total wholesale',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV phevfuel total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV phevfuel total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV electrical total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV electrical total consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV share of electrical consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV share of electrical consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_rel) = 0 ;
                 report_tech('EV share of phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV share of phevfuel consumption',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_rel) = 0 ;
                 report_node('EV_electricity_demand',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('EV_electricity_demand',loop_res_share,loop_ev,loop_prosumage,n) < card(h)*eps_rep_abs) = 0 ;
                 report_node('renshare EV',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('renshare EV',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = eps ;
                 report_node('conshare EV',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('conshare EV',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = eps ;
                 report_node('gross_energy_demand w/o EV',loop_res_share,loop_ev,loop_prosumage,n)$(report_node('gross_energy_demand w/o EV',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* Reserves
%reserves_endogenous%$ontext
         report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_prim,n) = feat_node('reserves',n) * phi_reserves_pr_up(n) * sum( reserves_nonprim ,  (1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum( nondis , reserves_slope(n,reserves_nonprim,nondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondis) + lev_N_RES_PRO(scen,n,nondis)))/1000) )) ) ;
         report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,n) = feat_node('reserves',n) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum( nondis , reserves_slope(n,reserves_nonprim,nondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondis) + lev_N_RES_PRO(scen,n,nondis)))/1000) ) ;
* ## marginals missing
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n)$(ord(h) > 1) = feat_node('reserves',n) * (report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves,n)) ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n)$(ord(h) > 1) = feat_node('reserves',n) * (phi_reserves_call(n,reserves,h) * report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves,n)) ;
$ontext
$offtext
%reserves_exogenous%$ontext
         report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_prim,h,n) = feat_node('reserves',n) * reserves_exogenous(n,reserves_prim,h) ;
         report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n) = feat_node('reserves',n) * reserves_exogenous(n,reserves_nonprim,h) ;
         report_reserves_hours('reserve prices',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n) = feat_node('reserves',n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con9a(scen,reserves_nonprim,h,n) );
         report_reserves_hours('reserve prices',loop_res_share,loop_ev,loop_prosumage,reserves_prim,h,n) = feat_node('reserves',n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , marginal_con9b(scen,reserves_prim,h,n) );
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n)$(ord(h) > 1) = feat_node('reserves',n) * (report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves,h,n)) ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n)$(ord(h) > 1) = feat_node('reserves',n) * (phi_reserves_call(n,reserves,h) * report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves,h,n)) ;
$ontext
$offtext

%reserves%$ontext
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dis,h,n)$(feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves,dis,h)) ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dis,h,n)$(feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves,dis,h))*phi_reserves_call(n,reserves,h) ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,h,n)$(feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves,nondis,h)) ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,h,n)$(feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves,nondis,h))*phi_reserves_call(n,reserves,h) ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,h,n)$(feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves,rsvr,h)) ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,h,n)$(feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves,rsvr,h))*phi_reserves_call(n,reserves,h) ;
         report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h,n)$(feat_node('reserves',n)) = (sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves,sto,h))) ;
         report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h,n)$(feat_node('reserves',n)) = ((sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves,sto,h)))*phi_reserves_call(n,reserves,h)) ;

         report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,dis,n)$(feat_node('reserves',n) AND sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,dis,h) + corr_fac_dis(scen,n,dis,h))) > card(h)*eps_rep_abs)= sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves,dis,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,dis,h) + corr_fac_dis(scen,n,dis,h)) ) ;
         report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,dis,n)$(feat_node('reserves',n) AND sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,dis,h) + corr_fac_dis(scen,n,dis,h))) > card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves,dis,h)) * phi_reserves_call(n,reserves,h)) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,dis,h) + corr_fac_dis(scen,n,dis,h)) ) ;
         report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,n)$(feat_node('reserves',n) AND sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,n,nondis,h))) >  card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves,nondis,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,n,nondis,h)) ) ;
         report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,n)$(feat_node('reserves',n) AND sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,n,nondis,h))) > card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves,nondis,h)) * phi_reserves_call(n,reserves,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,n,nondis,h)) ) ;
         report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,n)$(feat_node('reserves',n) AND sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h))) >  card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves,rsvr,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h)) ) ;
         report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,n)$(feat_node('reserves',n) AND sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h))) >  card(h)*eps_rep_abs) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves,rsvr,h)) * phi_reserves_call(n,reserves,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_up,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_do,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_up,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h)) ) ;
         report_reserves_tech('Reserves provision ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_do,sto,h)) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_up,sto,h)) * phi_reserves_call(n,reserves_up,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_do,sto,h)) * phi_reserves_call(n,reserves_do,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_up,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_up,sto,h)) * phi_reserves_call(n,reserves_up,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h)) ) ;
         report_reserves_tech('Reserves activation ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves_do,sto,n)$(feat_node('reserves',n) AND  sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h))) > card(h)*eps_rep_abs ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_do,sto,h)) * phi_reserves_call(n,reserves_do,h) ) / sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h)) ) ;
$ontext
$offtext

%reserves_endogenous%$ontext
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_nonprim,dis,h))) / ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum( nondis ,reserves_slope(n,reserves_nonprim,nondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondis) + lev_N_RES_PRO(scen,n,nondis)))/1000) ) ) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_nonprim,nondis,h))) / ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum( nondisnondis ,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis) + lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_nonprim,rsvr,h))) / ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis) + lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_nonprim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_nonprim,sto,h))) / ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_prim,dis,h))) / (phi_reserves_pr_up(n)* sum( reserves_nonprim ,  ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) )) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_prim,nondis,h))) / (phi_reserves_pr_up(n) * sum( reserves_nonprim ,  ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) )) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_prim,rsvr,h))) / (phi_reserves_pr_up(n) * sum( reserves_nonprim ,  ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) )) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_prim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_prim,sto,h))) / (phi_reserves_pr_up(n) * sum( reserves_nonprim ,  ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis) + lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ));
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_prim,dis,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_prim,nondis,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n)* sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis) + lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_prim,rsvr,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n)* sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis) + lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n)$(feat_node('reserves',n)) = sum(h,(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_prim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_prim,sto,h)))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum( nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   );
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_nonprim,dis,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_nonprim,nondis,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis) + lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_nonprim,rsvr,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n)$(feat_node('reserves',n)) = sum(h,(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_nonprim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_nonprim,sto,h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
$ontext
$offtext
%reserves_exogenous%$ontext
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_nonprim,dis,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h))  ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_nonprim,nondis,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_nonprim,rsvr,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_nonprim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_nonprim,sto,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_prim,dis,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_prim,h)) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_prim,nondis,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_prim,h)) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_prim,rsvr,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_prim,h)) ;
         report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_prim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_prim,sto,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_prim,h)) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_prim,dis,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) * reserves_exogenous(n,reserves_prim,h) )     ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_prim,nondis,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) * reserves_exogenous(n,reserves_prim,h) ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_prim,rsvr,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) * reserves_exogenous(n,reserves_prim,h) ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n)$(feat_node('reserves',n)) = sum(h,(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_prim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_prim,sto,h)))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) * reserves_exogenous(n,reserves_prim,h) );
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DIS(scen,n,reserves_nonprim,dis,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h) ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_NONDIS(scen,n,reserves_nonprim,nondis,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h) ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n)$(feat_node('reserves',n)) = sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_RSVR(scen,n,reserves_nonprim,rsvr,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h) ) ;
         report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n)$(feat_node('reserves',n)) = sum(h,(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_nonprim,sto,h)) + sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_nonprim,sto,h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h) ) ;
$ontext
$offtext

%reserves%$ontext
         report_tech('Storage positive reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('reserves',n)) = sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_up,sto,h)) * phi_reserves_call(n,reserves_up,h)) * %sec_hour% ;
         report_tech('Storage negative reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('reserves',n)) = sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_IN(scen,n,reserves_do,sto,h)) * phi_reserves_call(n,reserves_do,h)) * %sec_hour% ;
         report_tech('Storage positive reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('reserves',n)) = sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_up,sto,h)) * phi_reserves_call(n,reserves_up,h)) * %sec_hour% ;
         report_tech('Storage negative reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('reserves',n)) = sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_do,sto,h)) * phi_reserves_call(n,reserves_do,h)) * %sec_hour% ;
         report_tech('Load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,n,dsm_shift,h)) ) ;
         report_tech('Load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h)) ) ;

         report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('reserves',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)) > eps_rep_ins) = ( report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)
                        + sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_up,sto,h)) * phi_reserves_call(n,reserves_up,h)) * %sec_hour%
                        - sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_do,sto,h)) * phi_reserves_call(n,reserves_do,h)) * %sec_hour%
                        ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)) ;
         report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('reserves',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto)) > eps_rep_ins) = ( report_tech('Storage out total non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)
                        + sum( (h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_up,sto,h)) * phi_reserves_call(n,reserves_up,h)) * %sec_hour%
                        - sum( (h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_STO_OUT(scen,n,reserves_do,sto,h)) * phi_reserves_call(n,reserves_do,h)) * %sec_hour%
                        ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto)) * %sec_hour% ;
         report_tech('Storage EP-ratio',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('reserves',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) + lev_N_STO_P_PRO(scen,n,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto) + lev_N_STO_E_PRO(scen,n,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto) + lev_N_STO_E_PRO(scen,n,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) + lev_N_STO_P_PRO(scen,n,sto) ) ;


%reserves_exogenous%                 report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_prim,n)$(feat_node('reserves',n) AND report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_prim,n) < eps_rep_abs) = 0 ;
%reserves_exogenous%                 report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,n)$(feat_node('reserves',n) AND report_reserves('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,n) < eps_rep_abs) = 0 ;
%reserves_endogenous%                report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_prim,h,n)$(feat_node('reserves',n) AND report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_prim,h,n) < eps_rep_abs) = 0 ;
%reserves_endogenous%                report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n)$(feat_node('reserves',n) AND report_reserves_hours('reserve provision requirements',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n) < eps_rep_abs) = 0 ;

                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'required',h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dis,h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dis,h,n) < eps_rep_abs ) = 0;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dis,h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dis,h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,sto,h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,h,n) < eps_rep_abs) = 0 ;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,h,n) < eps_rep_abs) = 0 ;

                 report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,dis,n)$(report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,dis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,dis,n)$(report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,dis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,n)$(report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,n)$(report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,nondis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,n)$(report_reserves_tech('Reserves provision ratio',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,n)$(report_reserves_tech('Reserves activation ratio',loop_res_share,loop_ev,loop_prosumage,reserves,rsvr,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves provision ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves provision ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves provision ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves provision ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves provision ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves provision ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves provision ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves provision ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves activation ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves activation ratio (storage out positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves activation ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves activation ratio (storage out negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves activation ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves activation ratio (storage in positive reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('Reserves activation ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n)$(report_reserves_tech('Reserves activation ratio (storage in negative reserves)',loop_res_share,loop_ev,loop_prosumage,reserves,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n) < eps_rep_rel) = 0 ;

                 report_tech('Storage positive reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage positive reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage negative reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage negative reserves activation by storage in',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage positive reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage positive reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage negative reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage negative reserves activation by storage out',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage cycles',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_tech('Storage EP-ratio',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_tech('Storage EP-ratio',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;
                 report_tech('Load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h) * eps_rep_abs) = 0 ;
                 report_tech('Load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h) * eps_rep_abs) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* Reserves and DSM
%DSM%$ontext
%reserves_endogenous%$ontext
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,n,reserves_nonprim,dsm_curt,h))) / ( (card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,n,reserves_nonprim,dsm_shift,h))) / ( (card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))   ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,n,reserves_nonprim,dsm_curt,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,n,reserves_nonprim,dsm_shift,h)) * phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))   ;
$ontext
$offtext
%reserves_exogenous%$ontext
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,n,reserves_nonprim,dsm_curt,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,n,reserves_nonprim,dsm_shift,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h))   ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,n,reserves_nonprim,dsm_curt,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h))  ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,n,reserves_nonprim,dsm_shift,h)) * phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h))   ;
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,n,reserves,dsm_shift,h))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_SHIFT(scen,n,reserves,dsm_shift,h))*phi_reserves_call(n,reserves,h)  ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,n,reserves,dsm_curt,h))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_DSM_CU(scen,n,reserves,dsm_curt,h))*phi_reserves_call(n,reserves,h)  ;

        report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = report_tech('Load shift pos absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) - report_tech('Load shift pos absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) ;
        report_tech('Load shift neg absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('dsm',n) AND feat_node('reserves',n)) = report_tech('Load shift neg absolute (total)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) - report_tech('Load shift neg absolute (non-reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) ;

                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n) < eps_rep_rel ) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n) < eps_rep_rel ) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n) < eps_rep_rel ) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n) < eps_rep_rel ) = 0 ;
                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h,n) < eps_rep_abs) = 0 ;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_shift,h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,dsm_curt,h,n) < eps_rep_abs ) = 0 ;
                 report_tech('Load shift neg absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift neg absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h)*eps_rep_abs) = 0 ;
                 report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_tech('Load shift pos absolute (reserves)',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < card(h)*eps_rep_abs) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* Reserves and EV
%EV%$ontext
%reserves%$ontext
       report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum',h,n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( ev , lev_RP_EV_V2G(scen,n,reserves,ev,h)))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( ev , lev_RP_EV_G2V(scen,n,reserves,ev,h)))  ;
       report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum',h,n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( ev , phi_reserves_call(n,reserves,h) * lev_RP_EV_V2G(scen,n,reserves,ev,h)))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( ev , phi_reserves_call(n,reserves,h) * lev_RP_EV_G2V(scen,n,reserves,ev,h)))  ;
$ontext
$offtext
%EV%$ontext
%reserves_endogenous%$ontext
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h))) / (phi_reserves_pr_up(n) * sum( reserves_nonprim ,  ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ));
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h))) / (phi_reserves_pr_up(n) * sum( reserves_nonprim ,  ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ));
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))) / (phi_reserves_pr_up(n) * sum( reserves_nonprim ,  ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ));
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h))) / ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h))) / ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))) / ((card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )) ;
       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum((h,ev),(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h)))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))*phi_reserves_call(n,reserves_prim,h)) /  sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = (sum((h,ev),(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))) ;
       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = (sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))) ;
       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = (sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))) ;
$ontext
$offtext
%EV%$ontext
%reserves_exogenous%$ontext
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_prim,h)) ;
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_prim,h)) ;
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_prim,h)) ;
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
       report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
*** ### not completed
*       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum((h,ev),(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h)))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
*       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_prim,ev,h))*phi_reserves_call(n,reserves_prim,h)) / sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
*       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_prim,ev,h))*phi_reserves_call(n,reserves_prim,h)) /  sum( h , phi_reserves_call(n,reserves_prim,h) *  phi_reserves_pr_up(n) * sum( reserves_nonprim , 1000 * phi_reserves_share(n,reserves_nonprim) * ( reserves_intercept(n,reserves_nonprim) + sum(nondisnondis , reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000 ) ) )   ) ;
*       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = (sum((h,ev),(sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))+ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))) ;
*       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = (sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_nonprim,ev,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))) ;
*       report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = (sum((h,ev),sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_nonprim,ev,h))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))) ;
$ontext
$offtext
%EV%$ontext
%reserves%$ontext
       report_tech('EV positive reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (ev,h,reserves_up) ,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_up,ev,h)) * phi_reserves_call(n,reserves_up,h)) * %sec_hour% ;
       report_tech('EV negative reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (ev,h,reserves_do) ,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_G2V(scen,n,reserves_do,ev,h)) * phi_reserves_call(n,reserves_do,h)) * %sec_hour% ;
       report_tech('EV positive reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (ev,h,reserves_up) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_up,ev,h)) * phi_reserves_call(n,reserves_up,h)) * %sec_hour% ;
       report_tech('EV negative reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('ev',n) AND feat_node('reserves',n)) = sum( (ev,h,reserves_do) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RP_EV_V2G(scen,n,reserves_do,ev,h)) * phi_reserves_call(n,reserves_do,h)) * %sec_hour% ;

                 report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum',h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum',h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum',h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'ev_cum',h,n) < eps_rep_abs ) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n)$(report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'ev_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'G2V_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'V2G_cum',n) < eps_rep_rel) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'ev_cum',n) < eps_rep_rel ) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'G2V_cum',n) < eps_rep_rel ) = 0 ;
                 report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n)$(report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'V2G_cum',n) < eps_rep_rel ) = 0 ;
                 report_tech('EV positive reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV positive reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV negative reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV negative reserves activation by charging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV positive reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV positive reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('EV negative reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(report_tech('EV negative reserves activation by discharging',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) < eps_rep_abs*card(h)*%sec_hour%) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* PROSUMAGE
%prosumage%$ontext
        report_hours('demand prosumers',loop_res_share,loop_ev,loop_prosumage,h,n)$feat_node('prosumage',n) = phi_pro_load(n) * d(n,h) ;
        report_hours('demand market',loop_res_share,loop_ev,loop_prosumage,h,n)$feat_node('prosumage',n) = (1 - phi_pro_load(n)) * d(n,h) ;

        report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_ev,loop_prosumage,res,h,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res) ) ;
        report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_ev,loop_prosumage,res,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,n,res,h)) ;
        report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES_PRO(scen,n,res,h) ) ;
        report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,n,res,h) ) ;
        report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,n,h) ) ;
        report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h))) ;
        report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2M(scen,n,res,sto,h))) ;
        report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2PRO(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2M(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2M(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2PRO(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2M(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_PRO(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_PRO2PRO(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_PRO2M(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_M2PRO(scen,n,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L_M2M(scen,n,sto,h)) ;

        report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,con,h,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h) + corr_fac_dis(scen,n,con,h)) ;
        report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,res,h,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + corr_fac_dis(scen,n,res,h) + lev_G_RES(scen,n,res,h)) ;
        report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) + corr_fac_rsvr(scen,n,rsvr,h)) ;
        report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_ev,loop_prosumage,res,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h)) ;
        report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT(scen,n,sto,h)) ;
        report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h)) ;
        report_market_tech_hours('storage level market',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_L(scen,n,sto,h)) ;
        report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2PRO(scen,n,sto,h)) ) ;
        report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2M(scen,n,sto,h)) ) ;
        report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_PRO2M(scen,n,sto,h)) ) ;
        report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_M2M(scen,n,sto,h)) ) ;
        report_market_tech_hours('energy market to prosumer',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,n,h) )   ;
        report_market_tech_hours('energy prosumer to market',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,n,res,h)) )  ;

        report_node('gross energy demand market',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n)) ;
        report_node('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen,n)) ;

        report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res) ) ;
        report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,n,sto)) ;
        report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,n,sto)) * %sec_hour% ;

        report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n));
        report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n));
        report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n));
        report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n));
        report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)) ;
        report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)) ;
        report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)) ;
        report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) =  sum( h, report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)) ;
        report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) + report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n) + report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) + report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n) ;

        report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n)$feat_node('prosumage',n) =  sum( (h,res) , report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h,n)) ;
        report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n)$feat_node('prosumage',n) =  sum( h , report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h,n)) ;
        report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'',n)$feat_node('prosumage',n) =  sum( (res,h) , report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h,n)) ;
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(feat_node('prosumage',n) AND sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load(n) * d(n,h) )) ) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , lev_G_RES_PRO(scen,n,res,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load(n) * d(n,h) ) );
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load(n) * d(n,h) )) ) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load(n) * d(n,h) ) );
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,'market',n)$sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load(n) * d(n,h) ) ) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , lev_G_MARKET_M2PRO(scen,n,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_pro_load(n) * d(n,h) ) );
        report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$feat_node('prosumage',n) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,n,res,h) )) * %sec_hour% ;
        report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res)) > eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,n,res,h) ))/ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
        report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h)))) / report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_STO_IN_PRO2M(scen,n,res,sto,h)))) / report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2PRO(scen,n,sto,h))) / report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN_M2M(scen,n,sto,h))) / report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,n,sto,h))) / report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_PRO2M(scen,n,sto,h))) / report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2PRO(scen,n,sto,h))) / report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2M(scen,n,sto,h))) / report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n) ;
        report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,n,res,h)))) / report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n) ;
        report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,n,h))) / report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n) ;
        report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'',n) > eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_RES_PRO(scen,n,res,h)))) / report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'',n) ;
        report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res) > eps_rep_ins)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,n,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h)) + lev_G_RES_PRO(scen,n,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res) ) ;
%reserves% report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,n,sto)) > eps_rep_ins) = report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,n,sto)) ;
%reserves% report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,n,sto)) > eps_rep_ins) = report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,n,sto)) * %sec_hour% ;
        report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,n,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,n,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E_PRO(scen,n,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,n,sto) ) ;

        report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res) ) ;
        report_market_tech('capacities reservoir MW market',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr) );
        report_market_tech('capacities reservoir MWh market',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_E(scen,n,rsvr) );
        report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)) ;
        report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto)) * %sec_hour% ;
        report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res,n)$feat_node('prosumage',n) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h) )) * %sec_hour% ;
        report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res,n) AND sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h)) ) + sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h))) > card(h)*eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h) ))/( sum(h,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h)) ) + sum(h,sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h)  )) ) ;
        report_market_tech('capacities conventional market',loop_res_share,loop_ev,loop_prosumage,con,n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,con)) ;
        report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum(h, report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h,n) ) * %sec_hour% ;
        report_market_tech('Storage in total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum(h, report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h,n) ) * %sec_hour% ;
        report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_RES(scen,n,res,h) + lev_G_L(scen,n,res,h)- corr_fac_dis(scen,n,res,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,res)) ;
        report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,con,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,con)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,con)) ;
%reserves% report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)) > eps_rep_ins) = report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto)) ;
%reserves% report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto)) > eps_rep_ins) = report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto)) * %sec_hour% ;
        report_market_tech('Storage EP-ratio market',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_E(scen,n,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) ) ;

        report_prosumage('gross energy demand prosumers',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen,n)) ;
        report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_market(scen,n)) ;
        report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen,n)) ;
        report_prosumage('self-generation share prosumers total',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen,n) )) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen,n)) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen,n) ) ;
        report_prosumage('market share prosumers',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen,n) )) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers_market(scen,n)) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_prosumers(scen,n) ) ;
        report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU_PRO(scen,n,res,h))) * %sec_hour% ;
        report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res))))  = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_CU_PRO(scen,n,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
        report_prosumage('share self-generation curtailed',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) )) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_CU_PRO(scen,n,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
        report_prosumage('share self-generation direct consumption',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) )) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_G_RES_PRO(scen,n,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
        report_prosumage('share self-generation to market',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res))) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , lev_G_MARKET_PRO2M(scen,n,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
        report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res))) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,sto,res) , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
        report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res))) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,sto,res) , lev_STO_IN_PRO2M(scen,n,res,sto,h) )) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
* ################### hier noch weitere
        report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( res , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res)) ) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,n,sto)) ) ;

        report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res) ) / report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P_PRO(scen,n,sto) ) / report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;

        report_market('gross energy demand market',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n)) ;
        report_market('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum((res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h))) * %sec_hour% ;
        report_market('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,n)$(feat_node('prosumage',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,res) , phi_res(n,res,h)*lev_N_TECH(scen,n,res)) ) > eps_rep_abs*card(res)*card(h) ) = sum( (res,h), sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_CU(scen,n,res,h)))/ sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h) , phi_res(n,res,h) * lev_N_TECH(scen,n,res)) ) ;
        report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,n,h)) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n) ) ;
        report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2PRO(scen,n,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n) ) ;
        report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_IN_M2M(scen,n,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n) ) ;
        report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,n,res,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n) ) ;
        report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_PRO2M(scen,n,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n) ) ;
        report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( h, sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( sto , lev_STO_OUT_M2M(scen,n,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand_market(scen,n) ) ;
        report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( tech , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,tech))) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) )) + sum( rsvr ,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr))) + sum( dsm_curt ,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt))) + sum( dsm_shift ,  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift))) ;
        report_market('Energy demand total market',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = (sum( h , (1 - phi_pro_load(n)) * d(n,h) ) + sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,n,h))) + sum( (sto,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_IN(scen,n,sto,h) + lev_STO_IN_M2M(scen,n,sto,h) + lev_STO_IN_M2PRO(scen,n,sto,h) ) )) * %sec_hour%
%prosumage%$ontext
%DSM%$ontext
        + sum( (dsm_shift,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_UP_DEMAND(scen,n,dsm_shift,h)) ) * %sec_hour%
$ontext
$offtext
%prosumage%$ontext
%EV%$ontext
*         + sum( (ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_CHARGE(scen,n,ev,h)) ) * %sec_hour%
         + sum( (res,ev,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_CHARGE_M(scen,n,ev,h) + lev_EV_CHARGE_PRO(scen,n,res,ev,h)) ) * %sec_hour%
$ontext
$offtext
%prosumage%$ontext
%reserves%$ontext
         + sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , reserves_activated(scen,n,h)))
$ontext
$offtext
;

* ### NEW:
%prosumage%$ontext
%heat%$ontext
        report_prosumage_tech_hours('SETS loading prosumers',loop_res_share,loop_ev,loop_prosumage,'SETS',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( res , lev_H_SETS_IN(scen,n,res,'bu7','setsh',h))) ;
        report_prosumage_tech_hours('SETS discharging prosumers',loop_res_share,loop_ev,loop_prosumage,'SETS',h,n)$feat_node('prosumage',n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_H_SETS_OUT(scen,n,'bu7','setsh',h)) ;

        report_prosumage_tech('capacities SETS P out prosumers',loop_res_share,loop_ev,loop_prosumage,'SETS',n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_SETS_P_OUT(scen,n,'bu7','setsh')) * %sec_hour% ;
        report_prosumage_tech('capacities SETS P in prosumers',loop_res_share,loop_ev,loop_prosumage,'SETS',n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , 2 * lev_N_SETS_P_OUT(scen,n,'bu7','setsh')) * %sec_hour% ;
        report_prosumage_tech('capacities SETS E prosumers',loop_res_share,loop_ev,loop_prosumage,'SETS',n)$feat_node('prosumage',n) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , 16 * lev_N_SETS_P_OUT(scen,n,'bu7','setsh')) * %sec_hour% ;

        report_prosumage_tech('SETS loading prosumers total',loop_res_share,loop_ev,loop_prosumage,'SETS',n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('SETS loading prosumers',loop_res_share,loop_ev,loop_prosumage,'SETS',h,n)) ;
        report_prosumage_tech('SETS discharging prosumers total',loop_res_share,loop_ev,loop_prosumage,'SETS',n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('SETS discharging prosumers',loop_res_share,loop_ev,loop_prosumage,'SETS',h,n)) ;
*         report_prosumage_tech('TEST RHS con11c_pro_selfcon',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), pro_selfcon(scen) * phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
*         report_prosumage_tech('TEST LHS con11c_pro_selfcon',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), lev_G_RES_PRO(scen,n,res,h) + sum(sto, lev_STO_IN_PRO2PRO(scen,n,res,sto,h)) + lev_H_SETS_IN(scen,n,res,'bu7','setsh',h) ) ) ;
*         report_prosumage_tech('TEST LHS con11c_pro_selfcon g_res_pro',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), lev_G_RES_PRO(scen,n,res,h) ) ) ;
*         report_prosumage_tech('TEST LHS con11c_pro_selfcon sto_in_pro2pro',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), sum(sto, lev_STO_IN_PRO2PRO(scen,n,res,sto,h)) ) ) ;
*         report_prosumage_tech('TEST LHS con11c_pro_selfcon h_sets_in',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), lev_H_SETS_IN(scen,n,res,'bu7','setsh',h) ) ) ;
$ontext
$offtext

%prosumage%$ontext
        report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis ,lev_G_L(scen,n,dis,h)) + sum( nondis ,lev_G_RES(scen,n,nondis,h)) + sum( res , lev_G_MARKET_PRO2M(scen,n,res,h) - corr_fac_nondis(scen,n,res,h)) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h)- corr_fac_rsvr(scen,n,rsvr,h)) + sum( sto , lev_STO_OUT_M2M(scen,n,sto,h) + lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h)) + sum( dsm_shift , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h) - corr_fac_dsm_shift(scen,n,dsm_shift,h)) + sum( dsm_curt , lev_DSM_CU(scen,n,dsm_curt,h)) + sum( ev , lev_EV_DISCHARGE(scen,n,ev,h)) - corr_fac_ev(scen,n,h) )) ;

        report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,tech,n)$(feat_node('prosumage',n) AND report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,tech) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,con,n)$(feat_node('prosumage',n) AND report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,con,h)) ) / report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,res,n)$(feat_node('prosumage',n) AND report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_L(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + lev_G_RES(scen,n,res,h) - corr_fac_nondis(scen,n,res,h)) ) / report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(feat_node('prosumage',n) AND report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)) ) / report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,sto,n)$(feat_node('prosumage',n) AND report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_STO_OUT_M2M(scen,n,sto,h) + lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h)) ) / report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;

                 report_hours('demand prosumers',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('demand prosumers',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs) = 0 ;
                 report_hours('demand market',loop_res_share,loop_ev,loop_prosumage,h,n)$(report_hours('demand market',loop_res_share,loop_ev,loop_prosumage,h,n) < eps_rep_abs) = 0 ;

                 report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h,n)$(report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_ev,loop_prosumage,'',h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;

                 report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,con,h,n)$(report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,con,h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n)$(report_market_tech_hours('generation market',loop_res_share,loop_ev,loop_prosumage,rsvr,h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_ev,loop_prosumage,res,h,n)$(report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_ev,loop_prosumage,res,h,n) < eps_rep_abs) =  0 ;
                 report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_market_tech_hours('generation storage market',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) =  0 ;
                 report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_market_tech_hours('storage loading market',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('storage level market',loop_res_share,loop_ev,loop_prosumage,sto,h,n)$(report_market_tech_hours('storage level market',loop_res_share,loop_ev,loop_prosumage,sto,h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$(report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$(report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$(report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$(report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('energy market to prosumer',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$(report_market_tech_hours('energy market to prosumer',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n) < eps_rep_abs) = 0 ;
                 report_market_tech_hours('energy prosumer to market',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n)$(report_market_tech_hours('energy prosumer to market',loop_res_share,loop_ev,loop_prosumage,'Interaction with prosumers',h,n) < eps_rep_abs) = 0 ;

                 report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_ins) =  0 ;
                 report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_ins) =  0 ;
                 report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_ins) =  0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,'market',n)$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_ev,loop_prosumage,'market',n) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_ins) = 0 ;
                 report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n) < eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n) < eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'',n) < eps_rep_abs * card(h)) = 0 ;
                 report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_ev,loop_prosumage,'',n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_ev,loop_prosumage,'',n)$(report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_ev,loop_prosumage,'',n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;
                 report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_prosumage_tech('FLH prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_abs) = 0 ;
%reserves%       report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
%reserves%       report_prosumage_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('FLH',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;

                 report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_abs) =  0 ;
                 report_market_tech('capacities reservoir MW market',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_market_tech('capacities reservoir MW market',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_ins) = 0 ;
                 report_market_tech('capacities reservoir MWh market',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_market_tech('capacities reservoir MWh market',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_ins) = 0 ;
                 report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) =  0 ;
                 report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) =  0 ;
                 report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_abs*card(h)) = 0 ;
                 report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_market_tech('capacities conventional market',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_market_tech('capacities conventional market',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_ins) = 0 ;
                 report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_market_tech('capacities renewable market',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_ins) = 0 ;
                 report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('capacities storage MW market',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_ins) =  0 ;
                 report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('capacities storage MWh market',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_ins) = 0 ;
                 report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('Storage out total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)) = 0 ;
                 report_market_tech('Storage in total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('Storage in total market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs*card(h)) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_abs) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_abs) = 0 ;
%reserves%       report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
%reserves%       report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_abs) = 0 ;
                 report_market_tech('Storage EP-ratio market',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('Storage EP-ratio market',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;

                 report_prosumage('gross energy demand prosumers',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('gross energy demand prosumers',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_ins) = 0 ;
                 report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_ins) = 0 ;
                 report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_ins) = 0 ;
                 report_prosumage('self-generation share prosumage total',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('self-generation share prosumage total',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('market share prosumage',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('market share prosumage',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_ins) = 0 ;
                 report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation curtailed',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('share self-generation curtailed',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation direct consumption',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('share self-generation direct consumption',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation to market',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('share self-generation to market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage,n)$(report_prosumage('Capacity total prosumers',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;

                 report_market('gross energy demand market',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('gross energy demand market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_market('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('curtailment of fluct res absolute market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_market('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('curtailment of fluct res relative market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_rel) = 0 ;
                 report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_market('Energy demand total market',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('Energy demand total market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;
                 report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)$(report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) < eps_rep_abs) = 0 ;

                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,tech,n)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,tech,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,con,n)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,con,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,res,n)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,res,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,sto,n)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,sto,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,rsvr,n) < eps_rep_rel) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* PROSUMAGE & DSM
%prosumage%$ontext
%DSM%$ontext
        report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(feat_node('prosumage',n) AND feat_node('dsm',n) AND report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('prosumage',n) AND feat_node('dsm',n) AND report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift) ) / report_market('Capacity total market',loop_res_share,loop_ev,loop_prosumage,n) + 1e-9 ;
        report_market_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(feat_node('prosumage',n) AND feat_node('dsm',n)) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt) );
        report_market_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('prosumage',n) AND feat_node('dsm',n)) =  sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift) );
        report_market_tech('FLH market',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(feat_node('prosumage',n) AND feat_node('dsm',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,n,dsm_curt,h) - corr_fac_dsm_cu(scen,n,dsm_curt,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_CU(scen,n,dsm_curt)) ;
        report_market_tech('FLH market',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('prosumage',n) AND feat_node('dsm',n) AND sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h)) ) / sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_DSM_SHIFT(scen,n,dsm_shift)) ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(feat_node('prosumage',n) AND feat_node('dsm',n) AND report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_CU(scen,n,dsm_curt,h) - corr_fac_dsm_cu(scen,n,dsm_curt,h)) ) / report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(feat_node('prosumage',n) AND feat_node('dsm',n) AND report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h) - corr_fac_dsm_shift(scen,n,dsm_shift,h)) ) / report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;

                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_market_tech('Capacity share market',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_rel) = 0 ;
                 report_market_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(report_market_tech('capacities load curtailment',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n) < eps_rep_ins) = 0 ;
                 report_market_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_market_tech('capacities load shift',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_ins) = 0 ;
                 report_market_tech('FLH market',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(report_market_tech('FLH market',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n) < eps_rep_abs) = 0 ;
                 report_market_tech('FLH market',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_market_tech('FLH market',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_abs) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,dsm_curt,n) < eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,dsm_shift,n) < eps_rep_rel) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* PROSUMAGE & EV
%prosumage%$ontext
%EV%$ontext
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n)$(feat_node('prosumage',n) AND feat_node('ev',n) AND report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n)) = (sum( (h,ev) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_EV_DISCHARGE(scen,n,ev,h))) - sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , corr_fac_ev(scen,n,h))))/ report_market('energy generated gross market',loop_res_share,loop_ev,loop_prosumage,n) * %sec_hour% + 1e-9 ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* HEAT
* #########
%prosumage%$ontext
%heat%$ontext
         report_heat_tech_hours('AUX_HEAT',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_AUX_HEAT(scen,n,bu,ch,h) ) ;
         report_heat_tech('AUX_HEAT total',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$feat_node('heat',n) = sum( h, report_heat_tech_hours('AUX_HEAT',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) ) ;

         report_heat_tech('share SETS',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$feat_node('heat',n) = report_heat_tech('SETS discharging prosumers total',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) / ( report_prosumage_tech('SETS discharging prosumers total',loop_res_share,loop_ev,loop_prosumage,'SETS',n) + report_heat_tech('AUX_HEAT total',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) ) ;
         report_heat_tech('share AUX_HEAT',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$feat_node('heat',n) = report_heat_tech('AUX_HEAT total',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) / ( report_prosumage_tech('SETS discharging prosumers total',loop_res_share,loop_ev,loop_prosumage,'SETS',n) + report_heat_tech('AUX_HEAT total',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) ) ;
$ontext
$offtext

* ######### Weitere Prosumage-Indikatoren
%prosumage%$ontext
         report_prosumage('total potential prosumage PV generation (TPPPVG)',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res)) ) ;
         report_prosumage('share curtailment in TPPPVG',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( res , report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_ev,loop_prosumage,res,n) ) / report_prosumage('total potential prosumage PV generation (TPPPVG)',loop_res_share,loop_ev,loop_prosumage,n) ;
         report_prosumage('share PRO2M in TPPPVG',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_ev,loop_prosumage,'',n) / report_prosumage('total potential prosumage PV generation (TPPPVG)',loop_res_share,loop_ev,loop_prosumage,n) ;
         report_prosumage('share direct self-consumption in TPPPVG',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_ev,loop_prosumage,'',n) / report_prosumage('total potential prosumage PV generation (TPPPVG)',loop_res_share,loop_ev,loop_prosumage,n) ;
         report_prosumage('share storage loading in TPPPVG',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = sum( sto , report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)) / report_prosumage('total potential prosumage PV generation (TPPPVG)',loop_res_share,loop_ev,loop_prosumage,n) ;
%heat%$ontext
         report_prosumage('share SETS loading in TPPPVG',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = report_prosumage_tech('SETS loading prosumers total',loop_res_share,loop_ev,loop_prosumage,'SETS',n) / report_prosumage('total potential prosumage PV generation (TPPPVG)',loop_res_share,loop_ev,loop_prosumage,n) ;
$ontext
$offtext
%EV%$ontext
         report_prosumage('share EV charging in TPPPVG',loop_res_share,loop_ev,loop_prosumage,n)$feat_node('prosumage',n) = report_tech('EV charge total prosumage',loop_res_share,loop_ev,loop_prosumage,'ev_cum',n) / report_prosumage('total potential prosumage PV generation (TPPPVG)',loop_res_share,loop_ev,loop_prosumage,n) ;
$ontext
$offtext

*         report_prosumage_tech('TEST LHS con11c_pro_selfcon',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), lev_G_RES_PRO(scen,n,res,h) + sum(sto, lev_STO_IN_PRO2PRO(scen,n,res,sto,h)) + lev_H_SETS_IN(scen,n,res,'bu7','setsh',h) ) ) ;
*         report_prosumage_tech('TEST LHS con11c_pro_selfcon g_res_pro',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), lev_G_RES_PRO(scen,n,res,h) ) ) ;
*         report_prosumage_tech('TEST LHS con11c_pro_selfcon sto_in_pro2pro',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), sum(sto, lev_STO_IN_PRO2PRO(scen,n,res,sto,h)) ) ) ;
*         report_prosumage_tech('TEST LHS con11c_pro_selfcon h_sets_in',loop_res_share,loop_ev,loop_prosumage,'TEST',n)$feat_node('prosumage',n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (res,h), lev_H_SETS_IN(scen,n,res,'bu7','setsh',h) ) ) ;

* HEAT
$ontext
         report_heat_tech_hours('heat demand - heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = dh(n,bu,ch,h) ;
         report_heat_tech_hours('heat supply direct electric heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_dir(n,bu,ch) * lev_H_DIR(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('heat supply SETS',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * (lev_H_SETS_OUT(scen,n,bu,ch,h) + (1-eta_heat_stat(n,bu,ch)) * lev_H_SETS_LEV(scen,n,bu,ch,h-1)) ) ;
         report_heat_tech_hours('heat supply storage heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_storage(n,bu,ch) * lev_H_STO_OUT(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('electricity demand direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_dir(n,bu,ch) * lev_H_DIR(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('electricity demand SETS',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * (lev_H_SETS_IN(scen,n,bu,ch,h) + corr_fac_sets(scen,n,bu,ch,h)) ) ;
         report_heat_tech_hours('electricity demand HP',loop_res_share,loop_ev,loop_prosumage,n,bu,hp,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_hp(n,bu,hp) * ( lev_H_HP_IN(scen,n,bu,hp,h) + corr_fac_hp(scen,n,bu,hp,h)) ) ;
         report_heat_tech_hours('electricity demand hybrid heating',loop_res_share,loop_ev,loop_prosumage,n,bu,hel,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_elec(n,bu,hel) * (lev_H_ELECTRIC_IN(scen,n,bu,hel,h) + corr_fac_h_elec(scen,n,bu,hel,h)) ) ;
         report_heat_tech_hours('fossil fuel demand hybrid heating',loop_res_share,loop_ev,loop_prosumage,n,bu,hfo,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_fossil(n,bu,hfo) * lev_H_STO_IN_FOSSIL(scen,n,bu,hfo,h) ) ;
         report_heat_tech_hours('SETS inflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_H_SETS_IN(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('SETS level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_H_SETS_LEV(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('SETS outflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * (lev_H_SETS_OUT(scen,n,bu,ch,h) + (1-eta_heat_stat(n,bu,ch)) * lev_H_SETS_LEV(scen,n,bu,ch,h-1) ) ) ;
         report_heat_tech_hours('Storage inflow',loop_res_share,loop_ev,loop_prosumage,n,bu,hst,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_hp(n,bu,hst) * lev_H_STO_IN_HP(scen,n,bu,hst,h) + theta_elec(n,bu,hst) * lev_H_STO_IN_ELECTRIC(scen,n,bu,hst,h) + theta_elec(n,bu,hst) * lev_H_STO_IN_FOSSIL(scen,n,bu,hst,h) ) ;
         report_heat_tech_hours('Storage level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_storage(n,bu,ch) * lev_H_STO_LEV(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('Storage outflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_storage(n,bu,ch) * lev_H_STO_OUT(scen,n,bu,ch,h) ) ;

         report_heat_tech_hours('heat demand - DHW',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = d_dhw(n,bu,ch,h) ;
         report_heat_tech_hours('DHW supply - direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_dir(n,bu,ch) * lev_H_DHW_DIR(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('DHW supply - SETS aux electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_H_DHW_AUX_OUT(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('DHW supply - storage heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_storage(n,bu,ch) * lev_H_DHW_STO_OUT(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('electricity demand DHW - direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_dir(n,bu,ch) * lev_H_DHW_DIR(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('electricity demand DHW - SETS aux electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * (lev_H_DHW_AUX_ELEC_IN(scen,n,bu,ch,h) + corr_fac_sets_aux(scen,n,bu,ch,h)) ) ;
         report_heat_tech_hours('DHW - SETS aux hot water storage level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_H_DHW_AUX_LEV(scen,n,bu,ch,h) ) ;

         report_heat_tech_hours('price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(feat_node('heat',n) AND sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_dir(n,bu,ch) * lev_H_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * lev_H_SETS_IN(scen,n,bu,ch,h) + theta_storage(n,bu,ch) * lev_H_HP_IN(scen,n,bu,ch,h) + theta_storage(n,bu,ch) * lev_H_ELECTRIC_IN(scen,n,bu,ch,h)) > 0.25 ) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - marginal_con1a(scen,n,h)) ;
         report_heat_tech_hours('price DHW electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(feat_node('heat',n) AND sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_dir(n,bu,ch) * lev_H_DHW_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * lev_H_DHW_AUX_ELEC_IN(scen,n,bu,ch,h)) > 0.25 ) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - marginal_con1a(scen,n,h)) ;

* ### new:
         report_heat_tech_hours('AUX_HEAT',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_AUX_HEAT(scen,n,bu,ch,h) ) ;
         report_heat_tech_hours('AUX_DHW',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_AUX_DHW(scen,n,bu,ch,h) ) ;


                 report_heat_tech_hours('heat demand - heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('heat demand - heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('heat supply direct electric heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('heat supply direct electric heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('heat supply SETS',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('heat supply SETS',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('heat supply storage heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('heat supply storage heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('electricity demand direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('electricity demand direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('electricity demand SETS',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('electricity demand SETS',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('electricity demand HP',loop_res_share,loop_ev,loop_prosumage,n,bu,hp,h)$(report_heat_tech_hours('electricity demand HP',loop_res_share,loop_ev,loop_prosumage,n,bu,hp,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('electricity demand hybrid heating',loop_res_share,loop_ev,loop_prosumage,n,bu,hel,h)$(report_heat_tech_hours('electricity demand hybrid heating',loop_res_share,loop_ev,loop_prosumage,n,bu,hel,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('fossil fuel demand hybrid heating',loop_res_share,loop_ev,loop_prosumage,n,bu,hfo,h)$(report_heat_tech_hours('fossil fuel demand hybrid heating',loop_res_share,loop_ev,loop_prosumage,n,bu,hfo,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('SETS inflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('SETS inflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('SETS level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('SETS level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('SETS outflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('SETS outflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('Storage inflow',loop_res_share,loop_ev,loop_prosumage,n,bu,hst,h)$(report_heat_tech_hours('Storage inflow',loop_res_share,loop_ev,loop_prosumage,n,bu,hst,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('Storage level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('Storage level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('Storage outflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('Storage outflow',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;

                 report_heat_tech_hours('heat demand - DHW',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('heat demand - DHW',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('DHW supply - direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('DHW supply - direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('DHW supply - SETS aux electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('DHW supply - SETS aux electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('DHW supply - storage heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('DHW supply - storage heating',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('electricity demand DHW - direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('electricity demand DHW - direct electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('electricity demand DHW - SETS aux electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('electricity demand DHW - SETS aux electric',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('DHW - SETS aux hot water storage level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('DHW - SETS aux hot water storage level',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;

                 report_heat_tech_hours('price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$(report_heat_tech_hours('price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) < eps_rep_abs) = 0 ;
                 report_heat_tech_hours('price DHW electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)$( report_heat_tech_hours('price DHW electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h)< eps_rep_abs) = 0 ;


         report_heat_tech('heat supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , theta_dir(n,bu,ch) * lev_H_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * (lev_H_SETS_OUT(scen,n,bu,ch,h) + (1-eta_heat_stat(n,bu,ch)) * lev_H_SETS_LEV(scen,n,bu,ch,h-1)) + theta_storage(n,bu,ch) * lev_H_STO_OUT(scen,n,bu,ch,h) )) ;
         report_heat_tech('DHW supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , theta_dir(n,bu,ch) * lev_H_DHW_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * lev_H_DHW_AUX_OUT(scen,n,bu,ch,h) + theta_storage(n,bu,ch) * lev_H_DHW_STO_OUT(scen,n,bu,ch,h) )) ;
         report_heat_tech('total costs',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , report_heat_tech_hours('price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) * ( theta_dir(n,bu,ch) * lev_H_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * (lev_H_SETS_IN(scen,n,bu,ch,h) + corr_fac_sets(scen,n,bu,ch,h)) + theta_hp(n,bu,ch) * ( lev_H_HP_IN(scen,n,bu,ch,h) + corr_fac_hp(scen,n,bu,ch,h)) + theta_elec(n,bu,ch) * lev_H_ELECTRIC_IN(scen,n,bu,ch,h) + corr_fac_h_elec(scen,n,bu,ch,h)) + report_heat_tech_hours('price DHW electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch,h) * (theta_dir(n,bu,ch) * lev_H_DHW_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * (lev_H_DHW_AUX_ELEC_IN(scen,n,bu,ch,h) + corr_fac_sets_aux(scen,n,bu,ch,h))) )) ;
         report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$feat_node('heat',n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , theta_dir(n,bu,ch) * lev_H_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * (lev_H_SETS_IN(scen,n,bu,ch,h) + corr_fac_sets(scen,n,bu,ch,h)) + theta_hp(n,bu,ch) * ( lev_H_HP_IN(scen,n,bu,ch,h) + corr_fac_hp(scen,n,bu,ch,h)) + theta_elec(n,bu,ch) * lev_H_ELECTRIC_IN(scen,n,bu,ch,h) + corr_fac_h_elec(scen,n,bu,ch,h) + theta_dir(n,bu,ch) * lev_H_DHW_DIR(scen,n,bu,ch,h) + theta_sets(n,bu,ch) * (lev_H_DHW_AUX_ELEC_IN(scen,n,bu,ch,h) + corr_fac_sets_aux(scen,n,bu,ch,h)) )) ;
         report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) AND report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)) = report_heat_tech('total costs',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) / report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) ;
         report_heat_tech('average price heat electricity consumption over all bu',loop_res_share,loop_ev,loop_prosumage,n,'over all bu',ch)$(feat_node('heat',n) AND report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,'bu1',ch)) = sum( bu, report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) * report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) ) / sum( bu, report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) ) ;
         report_heat_tech('Storage cycles (inflow)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) AND n_heat_e(n,bu,ch)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * (lev_H_SETS_IN(scen,n,bu,ch,h) + corr_fac_sets(scen,n,bu,ch,h)) + theta_hp(n,bu,ch) * ( lev_H_HP_IN(scen,n,bu,ch,h) + corr_fac_hp(scen,n,bu,ch,h))) )/n_heat_e(n,bu,ch);
*### new:
         report_heat_tech('Sets capacity (power out)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_N_SETS_P_OUT(scen,n,bu,ch) ) );
         report_heat_tech('Sets capacity (power in)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_N_SETS_P_OUT(scen,n,bu,ch) * 2 ) );
         report_heat_tech('Sets capacity (energy)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_N_SETS_P_OUT(scen,n,bu,ch) * 16 ) );
         report_heat_tech('Sets DHW capacity (power in)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_N_SETS_DHW_P_IN(scen,n,bu,ch) ) );
         report_heat_tech('Sets DHW capacity (power out)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_N_SETS_DHW_P_IN(scen,n,bu,ch) ) );
         report_heat_tech('Sets DHW capacity (energy)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(feat_node('heat',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,ch) * lev_N_SETS_DHW_P_IN(scen,n,bu,ch) * 2.2 ) );

                 report_heat_tech('heat supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(report_heat_tech('heat supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) < eps_rep_abs) = 0 ;
                 report_heat_tech('DHW supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(report_heat_tech('DHW supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) < eps_rep_abs) = 0 ;
                 report_heat_tech('total costs',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(report_heat_tech('total costs',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) < eps_rep_abs) = 0 ;
                 report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(report_heat_tech('total electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) < eps_rep_abs) = 0 ;
                 report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) < eps_rep_abs) = 0 ;
                 report_heat_tech('average price heat electricity consumption over all bu',loop_res_share,loop_ev,loop_prosumage,n,'over all bu',ch)$(report_heat_tech('average price heat electricity consumption over all bu',loop_res_share,loop_ev,loop_prosumage,n,'over all bu',ch) < eps_rep_abs) = 0 ;
                 report_heat_tech('Storage cycles (inflow)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)$(report_heat_tech('Storage cycles (inflow)',loop_res_share,loop_ev,loop_prosumage,n,bu,ch) < eps_rep_abs) = 0 ;
$ontext
$offtext


* ----------------------------------------------------------------------------


* Reserves and heat
%heat%$ontext
%reserves_endogenous%$ontext
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS(scen,n,reserves_nonprim,bu,'setsh',h)))) / ( (card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS(scen,n,reserves_nonprim,bu,'setsh',h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
*        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h) )))) / ( (card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))  ;
*        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h)  ))*phi_reserves_call(n,reserves_nonprim,h))) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))  ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h))))) / ( (card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h))))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS_AUX(scen,n,reserves_nonprim,bu,'setsh',h)))) / ( (card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS_AUX(scen,n,reserves_nonprim,bu,'setsh',h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) )))  ;
$ontext
$offtext
%heat%$ontext
%reserves_exogenous%$ontext
*** ### Noch nicht vollständig
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS(scen,n,reserves_nonprim,bu,'setsh',h)))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS(scen,n,reserves_nonprim,bu,'setsh',h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h) )  ;
*        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h) )))) / ( (card(h)-1) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))  ;
*        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = (sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h)  ))*phi_reserves_call(n,reserves_nonprim,h))) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * 1000 * phi_reserves_share(n,reserves_nonprim) * (reserves_intercept(n,reserves_nonprim) + sum(nondisnondis,reserves_slope(n,reserves_nonprim,nondisnondis) * sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage))  , (lev_N_TECH(scen,n,nondisnondis)+lev_N_RES_PRO(scen,n,nondisnondis)))/1000) ))  ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h))))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hst) * (lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h))))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h) )  ;
        report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS_AUX(scen,n,reserves_nonprim,bu,'setsh',h)))) / sum( h$(ord(h) > 1), reserves_exogenous(n,reserves_nonprim,h)) ;
        report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS_AUX(scen,n,reserves_nonprim,bu,'setsh',h)))*phi_reserves_call(n,reserves_nonprim,h)) / sum( h , phi_reserves_call(n,reserves_nonprim,h) * reserves_exogenous(n,reserves_nonprim,h) )  ;
$ontext
$offtext

%heat%$ontext
%reserves%$ontext
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'sets aux',h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS_AUX(scen,n,reserves,bu,'setsh',h)))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'sets aux',h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS_AUX(scen,n,reserves,bu,'setsh',h)))*phi_reserves_call(n,reserves,h)  ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'sets',h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS(scen,n,reserves,bu,'setsh',h)))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'sets',h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_sets(n,bu,'setsh') * lev_RP_SETS(scen,n,reserves,bu,'setsh',h)))*phi_reserves_call(n,reserves,h)  ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,hp,h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_hp(n,bu,hp) * lev_RP_HP(scen,n,reserves,bu,hp,h)))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,hp,h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_hp(n,bu,hp) * lev_RP_HP(scen,n,reserves,bu,hp,h)))*phi_reserves_call(n,reserves,h)  ;
        report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,hel,h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hel) * lev_RP_H_ELEC(scen,n,reserves,bu,hel,h)))  ;
        report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,hel,h,n)$(feat_node('heat',n) AND feat_node('reserves',n) ) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( bu , theta_storage(n,bu,hel) * lev_RP_H_ELEC(scen,n,reserves,bu,hel,h)))*phi_reserves_call(n,reserves,h)  ;

                  report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n)$( report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n) < eps_rep_rel) = 0 ;
                  report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n)$( report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n) < eps_rep_rel) = 0 ;
                  report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$( report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n) < eps_rep_rel) = 0 ;
                  report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n)$( report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n) < eps_rep_rel) = 0 ;
*                  report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hel,n)$( report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hel,n) < eps_rep_rel) = 0 ;
*                  report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hel,n)$( report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hel,n) < eps_rep_rel) = 0 ;
                  report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n)$( report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n) < eps_rep_rel) = 0 ;
                  report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n)$( report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n) < eps_rep_rel) = 0 ;

                  report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'sets aux',h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'sets aux',h,n) < eps_rep_abs) = 0 ;
                  report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'sets aux',h,n)$(report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'sets aux',h,n) < eps_rep_abs) = 0 ;
                  report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'sets',h,n)$(report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,'sets',h,n) < eps_rep_abs) = 0 ;
                  report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'sets',h,n)$( report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,'sets',h,n) < eps_rep_abs) = 0 ;
                  report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,hp,h,n)$( report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,hp,h,n) < eps_rep_abs) = 0 ;
                  report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,hp,h,n)$( report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,hp,h,n) < eps_rep_abs) = 0 ;
                  report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,hel,h,n)$( report_reserves_tech_hours('Reserves provision',loop_res_share,loop_ev,loop_prosumage,reserves,hel,h,n) < eps_rep_abs) = 0 ;
                  report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,hel,h,n)$( report_reserves_tech_hours('Reserves activation',loop_res_share,loop_ev,loop_prosumage,reserves,hel,h,n) < eps_rep_abs) = 0 ;
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
