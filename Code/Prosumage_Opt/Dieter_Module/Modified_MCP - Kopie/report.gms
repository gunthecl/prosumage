
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

corr_fac_ev_sep

gross_energy_demand                      Gross energy demand
gross_energy_demand_market               Gross energy demand - market (non-prosumage segment)
gross_energy_demand_prosumers            Gross energy demand - prosumagers
gross_energy_demand_prosumers_selfgen    Gross energy demand - prosumagers self-generated
gross_energy_demand_prosumers_market     Gross energy demand - prosumagers from market

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

corr_fac_ev_sep(scen,n,ev,h) = 0 ;


* ----------------------------------------------------------------------------

* Parameter for hourly nodal reserves activated
reserves_activated(scen,n,h) = 0 ;





* ----------------------------------------------------------------------------

* Prepare prosumage reporting parameters
%prosumage%$ontext
gross_energy_demand_prosumers(scen,n) = sum( h , phi_pro_load(n) * d(n,h));
gross_energy_demand_prosumers_selfgen(scen,n) = sum( (h,res) , lev_G_RES_PRO(scen,n,res,h)) + sum( (sto,h) , lev_STO_OUT_PRO2PRO(scen,n,sto,h) ) ;
gross_energy_demand_prosumers_market(scen,n) = sum( h , lev_G_MARKET_M2PRO(scen,n,h)) + sum( (sto,h) , lev_STO_OUT_M2PRO(scen,n,sto,h) ) ;
$ontext
$offtext


* ----------------------------------------------------------------------------

*Determine balancing correction factors




* ----------------------------------------------------------------------------

* Define gross energy demand for reporting
gross_energy_demand(scen,n) = sum( h , d(n,h) + sum( sto , lev_STO_IN(scen,n,sto,h) - lev_STO_OUT(scen,n,sto,h) )
%prosumage%$ontext
         + sum( sto , sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h) ) + lev_STO_IN_M2PRO(scen,n,sto,h) + lev_STO_IN_M2M(scen,n,sto,h) - lev_STO_OUT_PRO2PRO(scen,n,sto,h) - lev_STO_OUT_PRO2M(scen,n,sto,h) - lev_STO_OUT_M2PRO(scen,n,sto,h) - lev_STO_OUT_M2M(scen,n,sto,h) )
$ontext
$offtext


)
;


********************************************************************************
**** Report  *******************************************************************
********************************************************************************

* RPEORT model statistics
        report('model status',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , modstats(scen,'modelstat')) ;
        report('solve time',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , modstats(scen,'resusd')) ;
        report('obj value',loop_res_share,loop_ev,loop_prosumage) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_Z(scen) * %sec_hour%) ;


* ----------------------------------------------------------------------------

* REPORT HOURS
        report_hours('demand consumers',loop_res_share,loop_ev,loop_prosumage,h,n) = d(n,h) ;
        report_hours('energy generated',loop_res_share,loop_ev,loop_prosumage,h,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis , lev_G_L(scen,n,dis,h) ) + sum( nondis , lev_G_RES(scen,n,nondis,h) - corr_fac_nondis(scen,n,nondis,h)) + sum( sto , lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h))
*+ sum( dsm_shift, lev_DSM_DO_DEMAND(scen,n,dsm_shift,h)
*- corr_fac_dsm_shift(scen,n,dsm_shift,h))
*+ sum( ev , lev_EV_DISCHARGE(scen,n,ev,h)) - corr_fac_ev(scen,n,h) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h))
+ sum( res , lev_G_RES_PRO(scen,n,res,h) + lev_G_MARKET_PRO2M(scen,n,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h))) + sum( sto , lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h)) ) ;
        report_hours('infeasibility',loop_res_share,loop_ev,loop_prosumage,h,n) =  sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_G_INFES(scen,n,h) ) ;
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

;
        report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , gross_energy_demand(scen,n)) ;
        report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis , lev_G_L(scen,n,dis,h)) + sum( nondis , lev_G_RES(scen,n,nondis,h) - corr_fac_nondis(scen,n,nondis,h)) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h) - corr_fac_rsvr(scen,n,rsvr,h)) + sum( res , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res) - lev_CU_PRO(scen,n,res,h)) )) ;
        report_node('energy generated gross',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( dis , lev_G_L(scen,n,dis,h)) + sum( nondis , lev_G_RES(scen,n,nondis,h) - corr_fac_nondis(scen,n,nondis,h)) + sum( rsvr , lev_RSVR_OUT(scen,n,rsvr,h)- corr_fac_rsvr(scen,n,rsvr,h)) + sum( sto , lev_STO_OUT(scen,n,sto,h) - corr_fac_sto(scen,n,sto,h))
*+ sum( dsm_shift , lev_DSM_DO_DEMAND(scen,n,dsm_shift,h) - corr_fac_dsm_shift(scen,n,dsm_shift,h)) + sum( dsm_curt , lev_DSM_CU(scen,n,dsm_curt,h)) + sum( ev , lev_EV_DISCHARGE(scen,n,ev,h)) - corr_fac_ev(scen,n,h)
+  sum( res , phi_res(n,res,h) * lev_N_RES_PRO(scen,n,res) - lev_CU_PRO(scen,n,res,h)) + sum( sto , lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h)) )) ;
        report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n) = sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , - inc(l,n) * lev_F(scen,l,h)) ) ;
        report_node('gross exports',loop_res_share,loop_ev,loop_prosumage,n) = sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , -min(inc(l,n) * lev_F(scen,l,h),0) ) ) ;
        report_node('gross imports',loop_res_share,loop_ev,loop_prosumage,n) = sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , max(inc(l,n) * lev_F(scen,l,h),0) ) ) ;
        report_node('net import share in gross demand',loop_res_share,loop_ev,loop_prosumage,n) = -min(report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n),0)/report_node('energy demand gross',loop_res_share,loop_ev,loop_prosumage,n) ;
        report_node('net export share in net generation',loop_res_share,loop_ev,loop_prosumage,n) = max(report_node('net exports',loop_res_share,loop_ev,loop_prosumage,n),0)/report_node('energy generated net',loop_res_share,loop_ev,loop_prosumage,n) ;
        report_node('trade capacity',loop_res_share,loop_ev,loop_prosumage,n) = sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( l , abs(inc(l,n) * lev_NTC(scen,l))) ) ;
*        report_node('gross import share',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , report_hours('gross imports',loop_res_share,loop_ev,loop_prosumage,h,n))/ sum( h , report_hours('energy demanded',loop_res_share,loop_ev,loop_prosumage,h,n) ) ;
        report_node('gross export share in net generation',loop_res_share,loop_ev,loop_prosumage,n) = sum( h , report_hours('gross exports',loop_res_share,loop_ev,loop_prosumage,h,n))/sum( h , report_hours('energy generated',loop_res_share,loop_ev,loop_prosumage,h,n) ) ;
        report_node('Capacity total',loop_res_share,loop_ev,loop_prosumage,n) = sum( tech , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_TECH(scen,n,tech))) + sum( res , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RES_PRO(scen,n,res))) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_STO_P(scen,n,sto) + lev_N_STO_P_PRO(scen,n,sto) )) + sum( rsvr , sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , lev_N_RSVR_P(scen,n,rsvr)))

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

));
         report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( tech , c_i(n,tech)*lev_N_TECH(scen,n,tech) + c_fix(n,tech)*lev_N_TECH(scen,n,tech) ) + sum( sto , c_i_sto_e(n,sto)*lev_N_STO_E(scen,n,sto) + c_fix_sto(n,sto)/2*(lev_N_STO_P(scen,n,sto)+lev_N_STO_E(scen,n,sto)) + c_i_sto_p(n,sto)*lev_N_STO_P(scen,n,sto) ) + sum( rsvr , c_i_rsvr_e(n,rsvr)*lev_N_RSVR_E(scen,n,rsvr) + c_i_rsvr_p(n,rsvr)*lev_N_RSVR_P(scen,n,rsvr) + c_fix_rsvr(n,rsvr) * lev_N_RSVR_P(scen,n,rsvr))

%prosumage%$ontext
                 + sum( res , c_i(n,res)*lev_N_RES_PRO(scen,n,res) + c_fix(n,res)*lev_N_RES_PRO(scen,n,res) ) + sum( sto , c_i_sto_e(n,sto)*lev_N_STO_E_PRO(scen,n,sto) + c_fix_sto(n,sto)/2*(lev_N_STO_P_PRO(scen,n,sto) + lev_N_STO_E_PRO(scen,n,sto)) + c_i_sto_p(n,sto)*lev_N_STO_P_PRO(scen,n,sto)) + sum( (sto,h) , c_m_sto(n,sto) * ( lev_STO_OUT_PRO2PRO(scen,n,sto,h) + lev_STO_OUT_M2PRO(scen,n,sto,h) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,n,res,sto,h) + lev_STO_IN_PRO2M(scen,n,res,sto,h)) + lev_STO_OUT_PRO2M(scen,n,sto,h) + lev_STO_OUT_M2M(scen,n,sto,h) ) )
$ontext
$offtext
);

         report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( h , c_infes * lev_G_INFES(scen,n,h)) ) ;
*         report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (h,ev) , pen_phevfuel(n,ev) * lev_EV_PHEVFUEL(scen,n,ev,h)) ) ;
*         report_cost('Nodal cost: fossil heating',loop_res_share,loop_ev,loop_prosumage,n) = sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , sum( (bu,hst,h) , pen_heat_fuel(n,bu,hst) * lev_H_STO_IN_FOSSIL(scen,n,bu,hst,h)) ) ;
         report_cost('Nodal cost: total',loop_res_share,loop_ev,loop_prosumage,n) = report_cost('Nodal cost: dispatch',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n)
* + report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_ev,loop_prosumage,n)
* + report_cost('Nodal cost: fossil heating',loop_res_share,loop_ev,loop_prosumage,n)
 ;
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


* ----------------------------------------------------------------------------

* EV



* ----------------------------------------------------------------------------

* Reserves




* ----------------------------------------------------------------------------

* Reserves and DSM


* ----------------------------------------------------------------------------

* ----------------------------------------------------------------------------

* PROSUMAGE
%prosumage%$ontext
        report_hours('demand prosumers',loop_res_share,loop_ev,loop_prosumage,h,n)$feat_node('prosumage',n) = phi_pro_load(n) * d(n,h) ;
        report_hours('demand market',loop_res_share,loop_ev,loop_prosumage,h,n)$feat_node('prosumage',n) = (1 - phi_pro_load(n)) * d(n,h) ;
        gross_energy_demand_market(scen,n) = gross_energy_demand(scen,n) - gross_energy_demand_prosumers_selfgen(scen,n) ;

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
        report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,n)$feat_node('prosumage',n) = sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_ev,loop_prosumage,sto,h,n)) ;
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
%prosumage%$ontext
%prosumage%$ontext
%prosumage%$ontext
;
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



* ----------------------------------------------------------------------------

* HEAT

* ----------------------------------------------------------------------------

* Reserves and heat


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
