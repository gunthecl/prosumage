
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
calc_maxdemand
calc_mindemand

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

Z_MCP
Z_FIX
Z_VAR

lev_demand
lev_gross_demand
lev_residual_load
lev_CU_energybal
;



* Declare reporting parameters

marginal_con1a(scen,h)  = con1a_bal.m(h)   ;
%LP%$ontext
modelstat               = DIETER.modelstat ;
solvestat               = DIETER.solveStat ;
resusd                  = DIETER.resusd    ;
lev_Z(scen)             = Z.l              ;
marginal_con5a(scen)    = con5a_minRES.m   ;
$ontext
$offtext
lev_G_L(scen,tech,h)    = G_L.l(tech,h)    ;
lev_G_UP(scen,tech,h)   = G_UP.l(tech,h)   ;
lev_G_DO(scen,tech,h)   = G_DO.l(tech,h)   ;
lev_G_RES(scen,tech,h)  = G_RES.l(tech,h)  ;
lev_CU(scen,tech,h)     = CU.l(tech,h)     ;
lev_STO_IN(scen,sto,h)  = STO_IN.l(sto,h)  ;
lev_STO_OUT(scen,sto,h) = STO_OUT.l(sto,h) ;
lev_STO_L(scen,sto,h)   = STO_L.l(sto,h)   ;
lev_N_TECH(scen,tech)   = N_TECH.l(tech)   ;
lev_N_STO_E(scen,sto)   = N_STO_E.l(sto)   ;
lev_N_STO_P(scen,sto)   = N_STO_P.l(sto)   ;
lev_demand(scen,h)      = d(h)   ;
lev_gross_demand(scen,h)= lev_demand(scen,h)  ;
* Curtailment at energy balance level
lev_CU_energybal(scen,tech,h) = 0;


%prosumage%$ontext
lev_CU_PRO(scen,tech,h)              = CU_PRO.l(tech,h)                ;
lev_G_MARKET_PRO2M(scen,tech,h)      = G_MARKET_PRO2M.l(tech,h)        ;
lev_G_MARKET_M2PRO(scen,h)           = G_MARKET_M2PRO.l(h)             ;
lev_G_RES_PRO(scen,tech,h)           = G_RES_PRO.l(tech,h)             ;
lev_STO_IN_PRO2PRO(scen,tech,sto,h)  = STO_IN_PRO2PRO.l(sto,tech,h)    ;
lev_STO_IN_PRO2M(scen,tech,sto,h)    = STO_IN_PRO2M.l(tech,sto,h)      ;
lev_STO_IN_M2PRO(scen,sto,h)         = STO_IN_M2PRO.l(sto,h)           ;
lev_STO_IN_M2M(scen,sto,h)           = STO_IN_M2M.l(sto,h)             ;
lev_STO_OUT_PRO2PRO(scen,sto,h)      = STO_OUT_PRO2PRO.l(sto,h)        ;
lev_STO_OUT_PRO2M(scen,sto,h)        = STO_OUT_PRO2M.l(sto,h)          ;
lev_STO_OUT_M2PRO(scen,sto,h)        = STO_OUT_M2PRO.l(sto,h)          ;
lev_STO_OUT_M2M(scen,sto,h)          = STO_OUT_M2M.l(sto,h)            ;
lev_STO_L_PRO2PRO(scen,sto,h)        = STO_L_PRO2PRO.l(sto,h)          ;
lev_STO_L_PRO2M(scen,sto,h)          = STO_L_PRO2M.l(sto,h)            ;
lev_STO_L_M2PRO(scen,sto,h)          = STO_L_M2PRO.l(sto,h)            ;
lev_STO_L_M2M(scen,sto,h)            = STO_L_M2M.l(sto,h)              ;
lev_N_STO_E_PRO(scen,sto)            = N_STO_E_PRO.l(sto)              ;
lev_N_STO_P_PRO(scen,sto)            = N_STO_P_PRO.l(sto)              ;
lev_STO_L_PRO(scen,sto,h)            = N_STO_P_PRO.l(sto)              ;
lev_N_RES_PRO(scen,tech)             = N_RES_PRO.l(tech)               ;
lev_gross_demand(scen,h)             = lev_demand(scen,h)
                                     + lev_G_MARKET_M2PRO(scen,h)
                                     - sum (tech, lev_G_MARKET_PRO2M(scen,tech,h)) ;
lev_CU_energybal(scen,tech,h)        =

           sum( dis_sys , G_L.l(dis_sys,h)) + sum( nondis_sys , G_RES.l(nondis_sys,h))
         + sum( sto_sys , STO_OUT.l(sto_sys,h) ) + sum( res , G_MARKET_PRO2M.l(res,h) )
         -   d(h)  -   sum( sto_sys , STO_IN.l(sto_sys,h) ) - G_MARKET_M2PRO.l(h)

;
$ontext
$offtext


%MCP%$ontext
modelstat               = DIETER_MCP.modelstat ;
solvestat               = DIETER_MCP.solveStat ;
resusd                  = DIETER_MCP.resusd    ;
Z_VAR =


                   sum( (h,dis) , c_m(dis)*G_L.l(dis,h) )
                 + sum( (h,dis)$(ord(h)>1) , c_up(dis)*G_UP.l(dis,h) )
                 + sum( (h,dis) , c_do(dis)*G_DO.l(dis,h) )
                 + sum( (h,nondis) , c_cu(nondis)*CU.l(nondis,h) )
                 + sum( (h,sto) , c_m_sto(sto) * ( STO_OUT.l(sto,h) + STO_IN.l(sto,h) ) )
%prosumage%$ontext
                 + sum( (h,sto) , c_m_sto(sto) * ( STO_OUT_PRO2PRO.l(sto,h)
                 + sum( res , STO_IN_PRO2PRO.l(sto,res,h)) ))



$ontext
$offtext



;

%MCP%$ontext
Z_FIX =
                 + sum( tech , c_i(tech)*N_TECH.l(tech) )
                 + sum( tech , c_fix(tech)*N_TECH.l(tech) )
                 + sum( sto , c_i_sto_e(sto)*N_STO_E.l(sto) )
                 + sum( sto , c_fix_sto(sto)/2*(N_STO_P.l(sto)+ N_STO_E.l(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P.l(sto) )
%prosumage%$ontext
                 + sum( res , c_i(res)*N_RES_PRO.l(res) )
                 + sum( res , c_fix(res)*N_RES_PRO.l(res) )
                 + sum( sto , c_i_sto_e(sto)*N_STO_E_PRO.l(sto) )
                 + sum( sto , c_fix_sto(sto)/2*(N_STO_P_PRO.l(sto) + N_STO_E_PRO.l(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P_PRO.l(sto) )
$ontext
$offtext


;

%MCP%$ontext
Z_MCP = Z_VAR + Z_FIX            ;
lev_Z(scen)    = Z_MCP           ;
$ontext
$offtext





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


* Min and max for demand
calc_maxdemand = 0 ;
calc_mindemand = 100000 ;


* ----------------------------------------------------------------------------

* Default values for correction factors
corr_fac_dis(scen,dis,h) = 0 ;
corr_fac_nondis(scen,nondis,h) = 0 ;
corr_fac_sto(scen,sto,h) = 0 ;


* ----------------------------------------------------------------------------

* Parameter for hourly nodal reserves activated
reserves_activated(scen,h) = 0 ;





* ----------------------------------------------------------------------------

* Prepare prosumage reporting parameters
%prosumage%$ontext
gross_energy_demand_prosumers(scen)= sum( h , numb_pro_load * d_pro(h));
gross_energy_demand_prosumers_selfgen(scen)= sum( (h,res) , lev_G_RES_PRO(scen,res,h)) + sum( (sto,h) , lev_STO_OUT_PRO2PRO(scen,sto,h) ) ;
gross_energy_demand_prosumers_market(scen)= sum( h , lev_G_MARKET_M2PRO(scen,h)) + sum( (sto,h) , lev_STO_OUT_M2PRO(scen,sto,h) ) ;
$ontext
$offtext


* ----------------------------------------------------------------------------

* Define gross energy demand for reporting
gross_energy_demand(scen)= sum( h , d(h) + sum( sto , lev_STO_IN(scen,sto,h) - lev_STO_OUT(scen,sto,h) )
%prosumage%$ontext
         + sum( sto , sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h) ) + lev_STO_IN_M2PRO(scen,sto,h) + lev_STO_IN_M2M(scen,sto,h) - lev_STO_OUT_PRO2PRO(scen,sto,h) - lev_STO_OUT_PRO2M(scen,sto,h) - lev_STO_OUT_M2PRO(scen,sto,h) - lev_STO_OUT_M2M(scen,sto,h) )
$ontext
$offtext


)
;

lev_residual_load(h) =     d(h)  - sum( nondis_sys , G_RES.l(nondis_sys,h))
%prosumage%$ontext
         - sum( res , G_MARKET_PRO2M.l(res,h) )
         + G_MARKET_M2PRO.l(h)
$ontext
$offtext
;


********************************************************************************
**** Report  *******************************************************************
********************************************************************************

* REPORT model statistics
        report('model status',loop_res_share,loop_prosumage)  = modelstat ;
        report('solver status',loop_res_share,loop_prosumage) = solvestat ;
        report('solve time',loop_res_share,loop_prosumage)    = resusd ;
        report('obj value',loop_res_share,loop_prosumage)     = sum(scen, lev_Z(scen)* %sec_hour%) ;


* ----------------------------------------------------------------------------

* REPORT HOURS
        report_hours('demand consumers',loop_res_share,loop_prosumage,h)= d(h) ;
        report_hours('energy generated',loop_res_share,loop_prosumage,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( dis , lev_G_L(scen,dis,h) ) + sum( nondis , lev_G_RES(scen,nondis,h) - corr_fac_nondis(scen,nondis,h)) + sum( sto , lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h))
        + sum( res , lev_G_RES_PRO(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h))) + sum( sto , lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h)) ) ;
*        report_hours('infeasibility',loop_res_share,loop_prosumage,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_INFES(scen,h));
*        report_hours('gross exports',loop_res_share,loop_prosumage,h)= sum( l , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , -min(inc(l)* lev_F(scen,l,h),0) )) ;
*        report_hours('gross imports',loop_res_share,loop_prosumage,h)= sum( l , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , max(inc(l)* lev_F(scen,l,h),0) ) ) ;
        report_hours('price',loop_res_share,loop_prosumage,h)= - sum(scen$(map(scen,loop_res_share,loop_prosumage)) , marginal_con1a(scen,h)) ;

                 report_hours('energy generated',loop_res_share,loop_prosumage,h)$(report_hours('energy generated',loop_res_share,loop_prosumage,h)< eps_rep_abs ) = 0 ;
                 report_hours('energy demanded',loop_res_share,loop_prosumage,h)$(report_hours('energy demanded',loop_res_share,loop_prosumage,h)< eps_rep_abs ) = 0 ;
*                 report_hours('gross exports',loop_res_share,loop_prosumage,h)$(report_hours('gross exports',loop_res_share,loop_prosumage,h)< eps_rep_abs ) = 0 ;
*                 report_hours('gross imports',loop_res_share,loop_prosumage,h)$(report_hours('gross imports',loop_res_share,loop_prosumage,h)< eps_rep_abs ) = 0 ;
                 report_hours('demand consumers',loop_res_share,loop_prosumage,h)$(report_hours('demand consumers',loop_res_share,loop_prosumage,h)< eps_rep_abs) = 0 ;
                 report_hours('price',loop_res_share,loop_prosumage,h)$(report_hours('price',loop_res_share,loop_prosumage,h)< eps_rep_abs AND report_hours('price',loop_res_share,loop_prosumage,h)> -eps_rep_abs) = eps ;
*                 report_hours('infeasibility',loop_res_share,loop_prosumage,h)$(report_hours('infeasibility',loop_res_share,loop_prosumage,h)< eps_rep_abs) = 0 ;
                 report_hours('residual load',loop_res_share,loop_prosumage,h) = lev_residual_load(h) ;


* ----------------------------------------------------------------------------

* REPORT TECH HOURS
        report_tech_hours('generation conventional',loop_res_share,loop_prosumage,con,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h) + corr_fac_dis(scen,con,h)) ;
*        report_tech_hours('infeasibility',loop_res_share,loop_prosumage,'',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_INFES(scen,h) ) ;
        report_tech_hours('generation renewable',loop_res_share,loop_prosumage,res,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + corr_fac_dis(scen,res,h) + lev_G_RES(scen,res,h) + lev_G_RES_PRO(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) ) ;
*        report_tech_hours('generation renewable',loop_res_share,loop_prosumage,rsvr,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_OUT(scen,rsvr,h) ) ;
*        report_tech_hours('reservoir inflow',loop_res_share,loop_prosumage,rsvr,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , rsvr_in(rsvr,h)/1000 * lev_N_RSVR_E(scen,rsvr) ) ;
*        report_tech_hours('reservoir level',loop_res_share,loop_prosumage,rsvr,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_L(scen,rsvr,h) ) ;
        report_tech_hours('curtailment of fluct res',loop_res_share,loop_prosumage,res,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_PRO(scen,res,h) + lev_CU_energybal(scen,res,h) ) ;
        report_tech_hours('generation storage',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h) ) ;
        report_tech_hours('storage loading',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN(scen,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_STO_IN_M2PRO(scen,sto,h) + lev_STO_IN_M2M(scen,sto,h) ) ;
        report_tech_hours('storage level',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_L(scen,sto,h) + lev_STO_L_PRO(scen,sto,h) ) ;
*        report_tech_hours('netto exports',loop_res_share,loop_prosumage,'',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , - sum( l , inc(l)* lev_F(scen,l,h)) ) ;

                 report_tech_hours('generation conventional',loop_res_share,loop_prosumage,con,h)$(report_tech_hours('generation conventional',loop_res_share,loop_prosumage,con,h)< eps_rep_abs) = 0 ;
                 report_tech_hours('generation renewable',loop_res_share,loop_prosumage,res,h)$(report_tech_hours('generation renewable',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) = 0 ;
*  star added        report_tech_hours('generation renewable',loop_res_share,loop_prosumage,rsvr,h)$(report_tech_hours('generation renewable',loop_res_share,loop_prosumage,rsvr,h)< eps_rep_abs) = 0 ;
                 report_tech_hours('curtailment of fluct res',loop_res_share,loop_prosumage,res,h)$(report_tech_hours('curtailment of fluct res',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) = 0 ;
                 report_tech_hours('generation storage',loop_res_share,loop_prosumage,sto,h)$(report_tech_hours('generation storage',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_tech_hours('storage loading',loop_res_share,loop_prosumage,sto,h)$(report_tech_hours('storage loading',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_tech_hours('storage level',loop_res_share,loop_prosumage,sto,h)$(report_tech_hours('storage level',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
*                 report_tech_hours('netto exports',loop_res_share,loop_prosumage,'',h)$(abs(report_tech_hours('netto exports',loop_res_share,loop_prosumage,'',h)) < eps_rep_abs ) = 0 ;
*                 report_tech_hours('infeasibility',loop_res_share,loop_prosumage,'',h)$(report_tech_hours('infeasibility',loop_res_share,loop_prosumage,'',h)< eps_rep_abs ) = 0 ;
*                 report_tech_hours('reservoir inflow',loop_res_share,loop_prosumage,rsvr,h)$(report_tech_hours('reservoir inflow',loop_res_share,loop_prosumage,rsvr,h)< eps_rep_abs) = 0 ;
*                 report_tech_hours('reservoir level',loop_res_share,loop_prosumage,rsvr,h)$(report_tech_hours('reservoir level',loop_res_share,loop_prosumage,rsvr,h)< eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

* RPEORT LINE

* ----------------------------------------------------------------------------

* REPORT NODE
        report_node('energy demand total',loop_res_share,loop_prosumage)= (sum( h , d(h) ) + sum( (sto,h) , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN(scen,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_STO_IN_M2PRO(scen,sto,h) + lev_STO_IN_M2M(scen,sto,h)) )) * %sec_hour%;
        report_node('energy demand gross',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand(scen)) ;
        report_node('energy generated net',loop_res_share,loop_prosumage)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( dis , lev_G_L(scen,dis,h)) + sum( nondis , lev_G_RES(scen,nondis,h) - corr_fac_nondis(scen,nondis,h))
*        + sum( rsvr , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h))
         + sum( res , phi_res(res,h) * lev_N_RES_PRO(scen,res) - lev_CU_PRO(scen,res,h)) )) ;
        report_node('energy generated gross',loop_res_share,loop_prosumage)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( dis , lev_G_L(scen,dis,h)) + sum( nondis , lev_G_RES(scen,nondis,h) - corr_fac_nondis(scen,nondis,h))
*        + sum( rsvr , lev_RSVR_OUT(scen,rsvr,h)- corr_fac_rsvr(scen,rsvr,h))
         + sum( sto , lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h))
*+ sum( dsm_shift , lev_DSM_DO_DEMAND(scen,dsm_shift,h) - corr_fac_dsm_shift(scen,dsm_shift,h)) + sum( dsm_curt , lev_DSM_CU(scen,dsm_curt,h)) + sum( ev , lev_EV_DISCHARGE(scen,ev,h)) - corr_fac_ev(scen,h)
+  sum( res , phi_res(res,h) * lev_N_RES_PRO(scen,res) - lev_CU_PRO(scen,res,h)) + sum( sto , lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h)) )) ;
*        report_node('net exports',loop_res_share,loop_prosumage)= sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , - inc(l)* lev_F(scen,l,h)) ) ;
*        report_node('gross exports',loop_res_share,loop_prosumage)= sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , -min(inc(l)* lev_F(scen,l,h),0) ) ) ;
*        report_node('gross imports',loop_res_share,loop_prosumage)= sum( (l,h) , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , max(inc(l)* lev_F(scen,l,h),0) ) ) ;
*        report_node('net import share in gross demand',loop_res_share,loop_prosumage)= -min(report_node('net exports',loop_res_share,loop_prosumage),0)/report_node('energy demand gross',loop_res_share,loop_prosumage);
*        report_node('net export share in net generation',loop_res_share,loop_prosumage)= max(report_node('net exports',loop_res_share,loop_prosumage),0)/report_node('energy generated net',loop_res_share,loop_prosumage);
*        report_node('trade capacity',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( l , abs(inc(l)* lev_NTC(scen,l))) ) ;
*        report_node('gross import share',loop_res_share,loop_prosumage)= sum( h , report_hours('gross imports',loop_res_share,loop_prosumage,h))/ sum( h , report_hours('energy demanded',loop_res_share,loop_prosumage,h)) ;
*        report_node('gross export share in net generation',loop_res_share,loop_prosumage)= sum( h , report_hours('gross exports',loop_res_share,loop_prosumage,h))/sum( h , report_hours('energy generated',loop_res_share,loop_prosumage,h)) ;
        report_node('Capacity total',loop_res_share,loop_prosumage)= sum( tech , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,tech))) + sum( res , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RES_PRO(scen,res))) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ))
*        + sum( rsvr , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_P(scen,rsvr)))

;
        report_node('curtailment of fluct res absolute',loop_res_share,loop_prosumage)= sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU_PRO(scen,nondis,h) + lev_CU(scen,nondis,h) + lev_CU_energybal(scen,nondis,h)    )) * %sec_hour%  ;
        report_node('curtailment of fluct res relative',loop_res_share,loop_prosumage)$(sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , phi_res(nondis,h) * (lev_N_TECH(scen,nondis)))) + sum((res,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , phi_res(res,h) * lev_N_RES_PRO(scen,res))) > eps_rep_abs*card(res)*card(h)) = (sum((res,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU_PRO(scen,res,h))) + sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,nondis,h) + lev_CU_energybal(scen,nondis,h)))) * %sec_hour% / (sum((res,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , phi_res(res,h) * lev_N_RES_PRO(scen,res))) + sum((nondis,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , phi_res(nondis,h) * (lev_N_TECH(scen,nondis))))) ;
        report_node('bio not utilized absolute',loop_res_share,loop_prosumage)$(m_e('bio')) = (m_e('bio') - sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,'bio',h)))) * %sec_hour% ;
        report_node('bio not utilized relative',loop_res_share,loop_prosumage)$(m_e('bio')) = (m_e('bio') - sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,'bio',h)))) / m_e('bio') ;
        report_node('max price',loop_res_share,loop_prosumage)$sum(h,d(h)) = max( calc_maxprice , smax( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) ,  marginal_con1a(scen,h))) ) ;
        report_node('min price',loop_res_share,loop_prosumage)$sum(h,d(h)) = min( calc_minprice , smin( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) ,  marginal_con1a(scen,h))) ) ;
        report_node('mean price',loop_res_share,loop_prosumage)$sum(h,d(h)) = sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , marginal_con1a(scen,h)))/card(h) ;
        report_node('max res demand',loop_res_share,loop_prosumage)$sum(h,d(h)) = max( calc_maxdemand , smax( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) ,  lev_residual_load(h)  )) ) ;
        report_node('min res demand',loop_res_share,loop_prosumage)$sum(h,d(h)) = min( calc_mindemand , smin( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) ,    lev_residual_load(h)       )) ) ;

                 report_node('energy demand total',loop_res_share,loop_prosumage)$(report_node('energy demand total',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_node('curtailment of fluct res absolute',loop_res_share,loop_prosumage)$(report_node('curtailment of fluct res absolute',loop_res_share,loop_prosumage)< eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_node('curtailment of fluct res relative',loop_res_share,loop_prosumage)$(report_node('curtailment of fluct res relative',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_node('bio not utilized absolute',loop_res_share,loop_prosumage)$(report_node('bio not utilized absolute',loop_res_share,loop_prosumage)< eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_node('bio not utilized relative',loop_res_share,loop_prosumage)$(report_node('bio not utilized relative',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_node('min price',loop_res_share,loop_prosumage)$(report_node('min price',loop_res_share,loop_prosumage)< eps_rep_abs AND report_node('min price',loop_res_share,loop_prosumage)> -eps_rep_abs) = eps ;
*                 report_node('gross exports',loop_res_share,loop_prosumage)$(report_node('gross exports',loop_res_share,loop_prosumage)< eps_rep_abs*card(h)) = 0 ;
*                 report_node('gross imports',loop_res_share,loop_prosumage)$(report_node('gross imports',loop_res_share,loop_prosumage)< eps_rep_abs*card(h)) = 0 ;
*                 report_node('net exports',loop_res_share,loop_prosumage)$(abs(report_node('net exports',loop_res_share,loop_prosumage)) < eps_rep_abs*card(h)) = 0 ;
                 report_node('energy generated net',loop_res_share,loop_prosumage)$(report_node('energy generated net',loop_res_share,loop_prosumage)< eps_rep_abs*card(h)) = 0 ;
                 report_node('energy generated gross',loop_res_share,loop_prosumage)$(report_node('energy generated gross',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_node('energy demand gross',loop_res_share,loop_prosumage)$(report_node('energy demand gross',loop_res_share,loop_prosumage)< eps_rep_abs*card(h)) = 0 ;
*                 report_node('net import share in gross demand',loop_res_share,loop_prosumage)$(report_node('net import share in gross demand',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
*                 report_node('net export share in net generation',loop_res_share,loop_prosumage)$(report_node('net export share in net generation',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_node('Capacity total',loop_res_share,loop_prosumage)$(report_node('Capacity total',loop_res_share,loop_prosumage)< eps_rep_ins) = 0 ;
*                 report_node('gross import share',loop_res_share,loop_prosumage)$(report_node('gross import share',loop_res_share,loop_prosumage)< eps_rep_rel ) = 0 ;
*                 report_node('gross export share in net generation',loop_res_share,loop_prosumage)$(report_node('gross export share in net generation',loop_res_share,loop_prosumage)< eps_rep_rel ) = 0 ;


* ----------------------------------------------------------------------------

* REPORT COST
        report_cost('Nodal cost: dispatch',loop_res_share,loop_prosumage)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( dis , c_m(dis)*lev_G_L(scen,dis,h) + c_up(dis)*lev_G_UP(scen,dis,h)$(ord(h)>1) + c_do(dis)*lev_G_DO(scen,dis,h)) + sum( nondis , c_cu(nondis)*(lev_CU(scen,nondis,h) + lev_CU_energybal(scen,nondis,h))) + sum( sto , c_m_sto(sto) * ( lev_STO_OUT(scen,sto,h) + lev_STO_IN(scen,sto,h) ) )

));
         report_cost('Nodal cost: investment & fix',loop_res_share,loop_prosumage)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( tech , c_i(tech)*lev_N_TECH(scen,tech) + c_fix(tech)*lev_N_TECH(scen,tech) ) + sum( sto , c_i_sto_e(sto)*lev_N_STO_E(scen,sto) + c_fix_sto(sto)/2*(lev_N_STO_P(scen,sto)+lev_N_STO_E(scen,sto)) + c_i_sto_p(sto)*lev_N_STO_P(scen,sto) )
*        + sum( rsvr , c_i_rsvr_e(rsvr)*lev_N_RSVR_E(scen,rsvr) + c_i_rsvr_p(rsvr)*lev_N_RSVR_P(scen,rsvr) + c_fix_rsvr(rsvr) * lev_N_RSVR_P(scen,rsvr))

%prosumage%$ontext
                 + sum( res , c_i(res)*lev_N_RES_PRO(scen,res) + c_fix(res)*lev_N_RES_PRO(scen,res) ) + sum( sto , c_i_sto_e(sto)*lev_N_STO_E_PRO(scen,sto) + c_fix_sto(sto)/2*(lev_N_STO_P_PRO(scen,sto) + lev_N_STO_E_PRO(scen,sto)) + c_i_sto_p(sto)*lev_N_STO_P_PRO(scen,sto)) + sum( (sto,h) , c_m_sto(sto) * ( lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h) + sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h) ) )
$ontext
$offtext
);

*         report_cost('Nodal cost: infeasibility',loop_res_share,loop_prosumage)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , c_infes * lev_G_INFES(scen,h)) ) ;
*         report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_prosumage)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,ev) , pen_phevfuel(ev) * lev_EV_PHEVFUEL(scen,ev,h)) ) ;
*         report_cost('Nodal cost: fossil heating',loop_res_share,loop_prosumage)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (bu,hst,h) , pen_heat_fuel(bu,hst) * lev_H_STO_IN_FOSSIL(scen,bu,hst,h)) ) ;
         report_cost('Nodal cost: total',loop_res_share,loop_prosumage)= report_cost('Nodal cost: dispatch',loop_res_share,loop_prosumage)+ report_cost('Nodal cost: investment & fix',loop_res_share,loop_prosumage)
* + report_cost('Nodal cost: infeasibility',loop_res_share,loop_prosumage)
* + report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_prosumage)
* + report_cost('Nodal cost: fossil heating',loop_res_share,loop_prosumage)
 ;
$ontext
         report_cost('Costs lines',loop_res_share,loop_prosumage,l) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , c_i_ntc(l) * lev_NTC(scen,l)*dist(l) ) ;

                 report_cost('Nodal cost: dispatch',loop_res_share,loop_prosumage)$(report_cost('Nodal cost: dispatch',loop_res_share,loop_prosumage)< card(h) * eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: investment & fix',loop_res_share,loop_prosumage)$(report_cost('Nodal cost: investment & fix',loop_res_share,loop_prosumage)< eps_rep_ins) = 0 ;
                 report_cost('Nodal cost: infeasibility',loop_res_share,loop_prosumage)$(report_cost('Nodal cost: infeasibility',loop_res_share,loop_prosumage)< card(h) * eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_prosumage)$(report_cost('Nodal cost: PHEV fuel',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: fossil heating',loop_res_share,loop_prosumage)$(report_cost('Nodal cost: fossil heating',loop_res_share,loop_prosumage)< card(h) * eps_rep_abs) = 0 ;
                 report_cost('Nodal cost: total',loop_res_share,loop_prosumage)$(report_cost('Nodal cost: total',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_cost('Costs lines',loop_res_share,loop_prosumage,l)$(report_cost('Costs lines',loop_res_share,loop_prosumage,l) < eps_rep_ins) = 0 ;
$offtext

* ----------------------------------------------------------------------------

* REPORT TECH
        report_tech('capacities conventional',loop_res_share,loop_prosumage,con)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,con)) ;
        report_tech('capacities renewable',loop_res_share,loop_prosumage,res)= 0 + sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res) + lev_N_RES_PRO(scen,res)) ;
*        report_tech('capacities reservoir MW',loop_res_share,loop_prosumage,rsvr)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_P(scen,rsvr));
*        report_tech('capacities reservoir MWh',loop_res_share,loop_prosumage,rsvr)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_E(scen,rsvr));
        report_tech('capacities storage MW',loop_res_share,loop_prosumage,sto)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto)+ lev_N_STO_P_PRO(scen,sto)) ;
        report_tech('capacities storage MWh',loop_res_share,loop_prosumage,sto)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto)+ lev_N_STO_E_PRO(scen,sto)) * %sec_hour% ;

        report_tech('Capacity share',loop_res_share,loop_prosumage,con)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,con)) / report_node('Capacity total',loop_res_share,loop_prosumage)+ 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_prosumage,res)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res)) / report_node('Capacity total',loop_res_share,loop_prosumage)+ 1e-9 ;
*        report_tech('Capacity share',loop_res_share,loop_prosumage,rsvr)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_P(scen,rsvr)) / report_node('Capacity total',loop_res_share,loop_prosumage)+ 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_prosumage,res)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res) + lev_N_RES_PRO(scen,res) ) / report_node('Capacity total',loop_res_share,loop_prosumage)+ 1e-9 ;
        report_tech('Capacity share',loop_res_share,loop_prosumage,sto)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) / report_node('Capacity total',loop_res_share,loop_prosumage)+ 1e-9 ;

        report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,res)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + lev_G_RES_PRO(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h))) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand(scen)) ;
*        report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,rsvr)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand(scen)) ;
        report_tech('conshares in nodal gross demand',loop_res_share,loop_prosumage,con)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand(scen)) ;
        report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,res)$report_node('energy generated net',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + lev_G_RES_PRO(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h)) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h))) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , report_node('energy generated net',loop_res_share,loop_prosumage)) ;
*        report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,rsvr)$report_node('energy generated net',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , report_node('energy generated net',loop_res_share,loop_prosumage)) ;
        report_tech('conshares in nodal net generation',loop_res_share,loop_prosumage,con)$report_node('energy generated net',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , report_node('energy generated net',loop_res_share,loop_prosumage)) ;

        report_tech('curtailment of fluct res absolute',loop_res_share,loop_prosumage,res)=  sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_PRO(scen,res,h) + lev_CU_energybal(scen,res,h)   )) * %sec_hour% ;
        report_tech('curtailment of fluct res relative',loop_res_share,loop_prosumage,res)$(report_tech('curtailment of fluct res absolute',loop_res_share,loop_prosumage,res)AND sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_RES_PRO(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h)) ) + sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU_PRO(scen,res,h) + lev_CU(scen,res,h)+ lev_CU_energybal(scen,res,h))) > card(h)*eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_PRO(scen,res,h) + lev_CU_energybal(scen,res,h) ))/( sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , phi_res(res,h) * (lev_N_TECH(scen,res) + lev_N_RES_PRO(scen,res) ))) ) ;

        report_tech('Storage out total non-reserves',loop_res_share,loop_prosumage,sto)= sum(h, report_tech_hours('generation storage',loop_res_share,loop_prosumage,sto,h)) * %sec_hour% ;
        report_tech('Storage in total non-reserves',loop_res_share,loop_prosumage,sto)= sum(h, report_tech_hours('storage loading',loop_res_share,loop_prosumage,sto,h)) * %sec_hour% ;

        report_tech('FLH',loop_res_share,loop_prosumage,con)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,con)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h)) ) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,con)) ;
        report_tech('FLH',loop_res_share,loop_prosumage,res)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h)) + lev_G_RES_PRO(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h))) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res)) ;
*        report_tech('FLH',loop_res_share,loop_prosumage,rsvr)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_P(scen,rsvr)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h)) ) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_P(scen,rsvr)) ;
        report_tech('FLH',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto)) > eps_rep_ins) = ( report_tech('Storage out total non-reserves',loop_res_share,loop_prosumage,sto)) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto)) ;


                 report_tech('capacities conventional',loop_res_share,loop_prosumage,con)$(report_tech('capacities conventional',loop_res_share,loop_prosumage,con)< eps_rep_ins) = 0 ;
                 report_tech('capacities renewable',loop_res_share,loop_prosumage,res)$(report_tech('capacities renewable',loop_res_share,loop_prosumage,res)< eps_rep_ins) = 0 ;
                 report_tech('capacities storage MW',loop_res_share,loop_prosumage,sto)$(report_tech('capacities storage MW',loop_res_share,loop_prosumage,sto)< eps_rep_ins) = 0 ;
                 report_tech('capacities storage MWh',loop_res_share,loop_prosumage,sto)$(report_tech('capacities storage MWh',loop_res_share,loop_prosumage,sto)< eps_rep_ins) = 0 ;
*                 report_tech('capacities reservoir MW',loop_res_share,loop_prosumage,rsvr)$(report_tech('capacities reservoir MW',loop_res_share,loop_prosumage,rsvr)< eps_rep_ins) = 0 ;
*                 report_tech('capacities reservoir MWh',loop_res_share,loop_prosumage,rsvr)$(report_tech('capacities reservoir MWh',loop_res_share,loop_prosumage,rsvr)< eps_rep_ins) = 0 ;
                 report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,res)$(report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
*                 report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,rsvr)$(report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,rsvr)< eps_rep_rel) = 0 ;
                 report_tech('conshares in nodal gross demand',loop_res_share,loop_prosumage,con)$(report_tech('conshares in nodal gross demand',loop_res_share,loop_prosumage,con)< eps_rep_rel) = 0 ;
                 report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,res)$(report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
*                 report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,rsvr)$(report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,rsvr)< eps_rep_rel) = 0 ;
                 report_tech('conshares in nodal net generation',loop_res_share,loop_prosumage,con)$(report_tech('conshares in nodal net generation',loop_res_share,loop_prosumage,con)< eps_rep_rel) = 0 ;
                 report_tech('curtailment of fluct res absolute',loop_res_share,loop_prosumage,res)$(report_tech('curtailment of fluct res absolute',loop_res_share,loop_prosumage,res)< eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('curtailment of fluct res relative',loop_res_share,loop_prosumage,res)$(report_tech('curtailment of fluct res relative',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_prosumage,con)$(report_tech('Capacity share',loop_res_share,loop_prosumage,con)< eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_prosumage,res)$(report_tech('Capacity share',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
*                 report_tech('Capacity share',loop_res_share,loop_prosumage,rsvr)$(report_tech('Capacity share',loop_res_share,loop_prosumage,rsvr)< eps_rep_rel) = 0 ;
                 report_tech('Capacity share',loop_res_share,loop_prosumage,sto)$(report_tech('Capacity share',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;
                 report_tech('FLH',loop_res_share,loop_prosumage,con)$(report_tech('FLH',loop_res_share,loop_prosumage,con)< eps_rep_abs) = 0 ;
                 report_tech('FLH',loop_res_share,loop_prosumage,res)$(report_tech('FLH',loop_res_share,loop_prosumage,res)< eps_rep_abs) = 0 ;
*                 report_tech('FLH',loop_res_share,loop_prosumage,rsvr)$(report_tech('FLH',loop_res_share,loop_prosumage,rsvr)< eps_rep_abs) = 0 ;
                 report_tech('FLH',loop_res_share,loop_prosumage,sto)$(report_tech('FLH',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_tech('Storage out total non-reserves',loop_res_share,loop_prosumage,sto)$(report_tech('Storage out total non-reserves',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)*%sec_hour%) = 0 ;
                 report_tech('Storage in total non-reserves',loop_res_share,loop_prosumage,sto)$(report_tech('Storage in total non-reserves',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)*%sec_hour%) = 0 ;


* ----------------------------------------------------------------------------

* REPORT NODE
        report_node('renshare in nodal gross demand',loop_res_share,loop_prosumage)= sum(res, report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,res))
*        + sum( rsvr , report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,rsvr))
         ;
        report_node('conshare in nodal gross demand',loop_res_share,loop_prosumage)= sum(con, report_tech('conshares in nodal gross demand',loop_res_share,loop_prosumage,con)) ;
*        report_node('net import share in nodal gross demand',loop_res_share,loop_prosumage)= - report_node('net exports',loop_res_share,loop_prosumage)/ sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand(scen)) ;
        report_node('renshare in nodal net generation',loop_res_share,loop_prosumage)= sum(res, report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,res))
*+ sum( rsvr , report_tech('renshares in nodal net generation',loop_res_share,loop_prosumage,rsvr))
         ;
        report_node('conshare in nodal net generation',loop_res_share,loop_prosumage)= sum(con, report_tech('conshares in nodal net generation',loop_res_share,loop_prosumage,con)) ;

                 report_node('renshare in nodal gross demand',loop_res_share,loop_prosumage)$(report_node('renshare in nodal gross demand',loop_res_share,loop_prosumage)< eps_rep_rel ) = 0 ;
                 report_node('conshare in nodal gross demand',loop_res_share,loop_prosumage)$(report_node('conshare in nodal gross demand',loop_res_share,loop_prosumage)< eps_rep_rel ) = 0 ;
                 report_node('renshare in nodal net generation',loop_res_share,loop_prosumage)$(report_node('renshare in nodal net generation',loop_res_share,loop_prosumage)< eps_rep_rel ) = 0 ;
                 report_node('conshare in nodal net generation',loop_res_share,loop_prosumage)$(report_node('conshare in nodal net generation',loop_res_share,loop_prosumage)< eps_rep_rel ) = 0 ;



* ----------------------------------------------------------------------------

* REPORT TECH
        report_tech('Yearly energy',loop_res_share,loop_prosumage,con)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h) ) ) ;
        report_tech('Yearly energy',loop_res_share,loop_prosumage,res)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + sum(sto , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_G_RES_PRO(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h))) ;
*        report_tech('Yearly energy',loop_res_share,loop_prosumage,rsvr)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h) ) ) ;
        report_tech('Yearly energy',loop_res_share,loop_prosumage,sto)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h) + lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h)) ) ;

        report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,con)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h) ) ) / report_node('energy generated gross',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;
        report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,res)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + sum(sto , lev_STO_IN_PRO2PRO(scen,res,sto,h) + lev_STO_IN_PRO2M(scen,res,sto,h)) + lev_G_RES_PRO(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h))) / report_node('energy generated gross',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;
*        report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,rsvr)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h) ) ) / report_node('energy generated gross',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;
        report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,sto)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_PRO2M(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h) + lev_STO_OUT_M2M(scen,sto,h) + lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h)) ) / report_node('energy generated gross',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;
        report_tech('FLH',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) > eps_rep_ins) = report_tech('Storage out total non-reserves',loop_res_share,loop_prosumage,sto)/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) + lev_N_STO_P_PRO(scen,sto) ) ;
        report_tech('Storage cycles',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto) + lev_N_STO_E_PRO(scen,sto) ) > eps_rep_ins) = report_tech('Storage out total non-reserves',loop_res_share,loop_prosumage,sto)/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto) + lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% ;

                 report_tech('Yearly energy',loop_res_share,loop_prosumage,con)$(report_tech('Yearly energy',loop_res_share,loop_prosumage,con)< eps_rep_ins ) = 0 ;
                 report_tech('Yearly energy',loop_res_share,loop_prosumage,res)$(report_tech('Yearly energy',loop_res_share,loop_prosumage,res)< eps_rep_ins ) = 0 ;
*                 report_tech('Yearly energy',loop_res_share,loop_prosumage,rsvr)$(report_tech('Yearly energy',loop_res_share,loop_prosumage,rsvr)< eps_rep_ins ) = 0 ;
                 report_tech('Yearly energy',loop_res_share,loop_prosumage,sto)$(report_tech('Yearly energy',loop_res_share,loop_prosumage,sto)< eps_rep_ins ) = 0 ;

                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,con)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,con)< eps_rep_rel) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,res)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
*                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,rsvr)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,rsvr)< eps_rep_rel) = 0 ;
                 report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,sto)$(report_tech('Energy share in nodal gross generation',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;
                 report_tech('FLH',loop_res_share,loop_prosumage,sto)$(report_tech('FLH',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_tech('Storage cycles',loop_res_share,loop_prosumage,sto)$(report_tech('Storage cycles',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

* REPORT
        report('curtailment of fluct res absolute',loop_res_share,loop_prosumage) = report_node('curtailment of fluct res absolute',loop_res_share,loop_prosumage)  ;
        report('curtailment of fluct res relative',loop_res_share,loop_prosumage) = report_node('curtailment of fluct res absolute',loop_res_share,loop_prosumage) / sum((res,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , phi_res(res,h) * (lev_N_TECH(scen,res) + lev_N_RES_PRO(scen,res)) )) ;
        report('bio not utilized absolute',loop_res_share,loop_prosumage)$( m_e('bio')) = report_node('bio not utilized absolute',loop_res_share,loop_prosumage)  ;
        report('bio not utilized relative',loop_res_share,loop_prosumage)$( m_e('bio')) = report('bio not utilized absolute',loop_res_share,loop_prosumage) /m_e('bio') ;
        report('Capacity total',loop_res_share,loop_prosumage) = report_node('Capacity total',loop_res_share,loop_prosumage) ;
        report('energy demand gross',loop_res_share,loop_prosumage) = report_node('energy demand gross',loop_res_share,loop_prosumage)  ;
        report('energy demand total',loop_res_share,loop_prosumage) = report_node('energy demand total',loop_res_share,loop_prosumage)  ;
        report('energy generated net',loop_res_share,loop_prosumage) =  report_node('energy generated net',loop_res_share,loop_prosumage) ;
        report('energy generated gross',loop_res_share,loop_prosumage) =  report_node('energy generated gross',loop_res_share,loop_prosumage) ;
*        report('gross trade share',loop_res_share,loop_prosumage) = sum( (h), report_hours('gross imports',loop_res_share,loop_prosumage,h))/ sum( (h), report_hours('energy demanded',loop_res_share,loop_prosumage,h)) ;
        report('renshare total',loop_res_share,loop_prosumage) = sum( (h) , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_G_L(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h) + phi_res(res,h) * lev_N_RES_PRO(scen,res) - lev_CU_PRO(scen,res,h))
*       + sum( rsvr , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h)))
         ))/ sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand(scen) ) ;
        report('conshare total',loop_res_share,loop_prosumage) = sum( (h) , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( con , lev_G_L(scen,con,h)) )) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand(scen) ) ;
        report('Energy total',loop_res_share,loop_prosumage) = report_node('Energy total',loop_res_share,loop_prosumage) ;

                 report('curtailment of fluct res absolute',loop_res_share,loop_prosumage)$(report('curtailment of fluct res absolute',loop_res_share,loop_prosumage) < eps_rep_abs*card(h)) = 0 ;
                 report('curtailment of fluct res relative',loop_res_share,loop_prosumage)$(report('curtailment of fluct res relative',loop_res_share,loop_prosumage) < eps_rep_rel) = 0 ;
                 report('bio not utilized absolute',loop_res_share,loop_prosumage)$(report('bio not utilized absolute',loop_res_share,loop_prosumage) < eps_rep_abs*card(h)) = 0 ;
                 report('bio not utilized relative',loop_res_share,loop_prosumage)$(report('bio not utilized relative',loop_res_share,loop_prosumage) < eps_rep_rel) = 0 ;
                 report('Capacity total',loop_res_share,loop_prosumage)$(report('Capacity total',loop_res_share,loop_prosumage) < eps_rep_ins) = 0 ;
                 report('gross trade share',loop_res_share,loop_prosumage)$(report('gross trade share',loop_res_share,loop_prosumage) < eps_rep_rel ) = 0 ;
                 report('renshare total',loop_res_share,loop_prosumage)$(report('renshare total',loop_res_share,loop_prosumage) < eps_rep_rel ) = 0 ;
                 report('conshare total',loop_res_share,loop_prosumage)$(report('conshare total',loop_res_share,loop_prosumage) < eps_rep_rel ) = 0 ;
                 report('Energy total',loop_res_share,loop_prosumage)$(report('Energy total',loop_res_share,loop_prosumage) < eps_rep_abs) = 0 ;


* ----------------------------------------------------------------------------

* PROSUMAGE
%prosumage%$ontext
        report_hours('demand prosumers',loop_res_share,loop_prosumage,h)= numb_pro_load * d_pro(h) ;
        report_hours('demand market',loop_res_share,loop_prosumage,h)=  d(h) ;
        gross_energy_demand_market(scen)= gross_energy_demand(scen)- gross_energy_demand_prosumers_selfgen(scen);

        report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_prosumage,res,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , phi_res(res,h) * lev_N_RES_PRO(scen,res) ) ;
        report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_prosumage,res,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU_PRO(scen,res,h)) ;
        report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_prosumage,res,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_RES_PRO(scen,res,h) ) ;
        report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_prosumage,res,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) ) ;
        report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_prosumage,'',h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h) ) ;
        report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h))) ;
        report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_STO_IN_PRO2M(scen,res,sto,h))) ;
        report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN_M2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN_M2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_PRO2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_M2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_M2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_L_PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_L_PRO2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_L_PRO2M(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_L_M2PRO(scen,sto,h)) ;
        report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_L_M2M(scen,sto,h)) ;

        report_market_tech_hours('generation market',loop_res_share,loop_prosumage,con,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h) + corr_fac_dis(scen,con,h)) ;
        report_market_tech_hours('generation market',loop_res_share,loop_prosumage,res,h)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + corr_fac_dis(scen,res,h) + lev_G_RES(scen,res,h)) ;
        report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_prosumage,res,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_energybal(scen,res,h)   ) ;
        report_market_tech_hours('generation storage market',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT(scen,sto,h)) ;
        report_market_tech_hours('storage loading market',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN(scen,sto,h)) ;
        report_market_tech_hours('storage level market',loop_res_share,loop_prosumage,sto,h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_L(scen,sto,h)) ;
        report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_prosumage,'Interaction with prosumers',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_IN_M2PRO(scen,sto,h)) ) ;
        report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_IN_M2M(scen,sto,h)) ) ;
        report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_OUT_PRO2M(scen,sto,h)) ) ;
        report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_OUT_M2M(scen,sto,h)) ) ;
        report_market_tech_hours('energy market to prosumer',loop_res_share,loop_prosumage,'Interaction with prosumers',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h) )   ;
        report_market_tech_hours('energy prosumer to market',loop_res_share,loop_prosumage,'Interaction with prosumers',h)=  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,res,h)) )  ;

        report_node('gross energy demand market',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_node('gross energy demand prosumers self generation',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen)) ;

        report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_prosumage,res)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RES_PRO(scen,res) ) ;
        report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_prosumage,sto)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) ;
        report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_prosumage,sto)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto)) * %sec_hour% ;
        report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h));
        report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_prosumage,sto,h));
        report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_prosumage,sto,h));
        report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_prosumage,sto,h));
        report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_prosumage,sto)= sum( h, report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_prosumage,sto)=  sum( h, report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_prosumage,sto,h)) ;
        report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_prosumage,sto)= report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)+ report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_prosumage,sto)+ report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_prosumage,sto)+ report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_prosumage,'')=  sum( (h,res) , report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_prosumage,res,h)) ;
        report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_prosumage,'')=  sum( h , report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_prosumage,'',h)) ;
        report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_prosumage,'')=  sum( (res,h) , report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_prosumage,res,h)) ;
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,res)$(sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , numb_pro_load * d_pro(h) )) ) = sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , lev_G_RES_PRO(scen,res,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , numb_pro_load * d_pro(h) ) );
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,sto)$(sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , numb_pro_load * d_pro(h) )) ) = sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , lev_STO_OUT_PRO2PRO(scen,sto,h) + lev_STO_OUT_M2PRO(scen,sto,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , numb_pro_load * d_pro(h) ) );
        report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,'market')$sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , numb_pro_load * d_pro(h) ) ) = sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , lev_G_MARKET_M2PRO(scen,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , numb_pro_load * d_pro(h) ) );
        report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_prosumage,res)=  sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU_PRO(scen,res,h) )) * %sec_hour% ;
        report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_prosumage,res)$(report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_prosumage,res)AND sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RES_PRO(scen,res)) > eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU_PRO(scen,res,h) ))/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( h , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_STO_IN_PRO2PRO(scen,res,sto,h)))) / report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_STO_IN_PRO2M(scen,res,sto,h)))) / report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN_M2PRO(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN_M2M(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_PRO2PRO(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_PRO2M(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_M2PRO(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_prosumage,sto)> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_M2M(scen,sto,h))) / report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_prosumage,sto);
        report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_prosumage,'')> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,res,h)))) / report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_prosumage,'');
        report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_prosumage,'')> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h))) / report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_prosumage,'');
        report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_prosumage,'')> eps_rep_abs*card(h)) = sum( h , report_hours('price',loop_res_share,loop_prosumage,h)* sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_G_RES_PRO(scen,res,h)))) / report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_prosumage,'');
        report_prosumage_tech('FLH prosumers',loop_res_share,loop_prosumage,res)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RES_PRO(scen,res) > eps_rep_ins)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_MARKET_PRO2M(scen,res,h) + sum( sto , lev_STO_IN_PRO2PRO(scen,res,sto,h)) + lev_G_RES_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RES_PRO(scen,res) ) ;
        report_prosumage_tech('FLH prosumers',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) > eps_rep_ins) = report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_prosumage,sto)/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) ;
        report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto)) > eps_rep_ins) = report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_prosumage,sto)/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto)) * %sec_hour% ;
        report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E_PRO(scen,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto) ) ;

        report_market_tech('capacities renewable market',loop_res_share,loop_prosumage,res)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res) ) ;
*        report_market_tech('capacities reservoir MW market',loop_res_share,loop_prosumage,rsvr)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_P(scen,rsvr) );
*        report_market_tech('capacities reservoir MWh market',loop_res_share,loop_prosumage,rsvr)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_E(scen,rsvr) );
        report_market_tech('capacities storage MW market',loop_res_share,loop_prosumage,sto)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto)) ;
        report_market_tech('capacities storage MWh market',loop_res_share,loop_prosumage,sto)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto)) * %sec_hour% ;
        report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_prosumage,res)=  sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_energybal(scen,res,h)    )) * %sec_hour% ;
        report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_prosumage,res)$(report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_prosumage,res)AND sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h)) ) + sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h)+ lev_CU_energybal(scen,res,h)   )) > card(h)*eps_rep_abs ) =  sum(h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h) + lev_CU_energybal(scen,res,h)    ))/( sum(h,  sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h)) ) + sum(h,sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h)  + lev_CU_energybal(scen,res,h)   )) ) ;
        report_market_tech('capacities conventional market',loop_res_share,loop_prosumage,con)=  sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,con)) ;
        report_market_tech('Storage out total market non-reserves',loop_res_share,loop_prosumage,sto)= sum(h, report_market_tech_hours('generation storage market',loop_res_share,loop_prosumage,sto,h)) * %sec_hour% ;
        report_market_tech('Storage in total market non-reserves',loop_res_share,loop_prosumage,sto)= sum(h, report_market_tech_hours('storage loading market',loop_res_share,loop_prosumage,sto,h)) * %sec_hour% ;
        report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,res)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_RES(scen,res,h) + lev_G_L(scen,res,h)- corr_fac_dis(scen,res,h)) ) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,res)) ;
        report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,con)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,con)) > eps_rep_ins) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h)) ) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,con)) ;
        report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto)) > eps_rep_ins) = report_market_tech('Storage out total market non-reserves',loop_res_share,loop_prosumage,sto)/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto)) ;
        report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto)) > eps_rep_ins) = report_market_tech('Storage out total market non-reserves',loop_res_share,loop_prosumage,sto)/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto)) * %sec_hour% ;
        report_market_tech('Storage EP-ratio market',loop_res_share,loop_prosumage,sto)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) ) > eps_rep_ins AND sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto) ) * %sec_hour% > eps_rep_ins ) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_E(scen,sto) ) * %sec_hour% / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) ) ;

        report_prosumage('gross energy demand prosumers',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers(scen)) ;
        report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers_market(scen)) ;
        report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen)) ;
        report_prosumage('self-generation share prosumers total',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers(scen))) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers_selfgen(scen)) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers(scen)) ;
        report_prosumage('market share prosumers',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers(scen))) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers_market(scen)) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_prosumers(scen)) ;
        report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_prosumage)= sum((res,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU_PRO(scen,res,h))) * %sec_hour% ;
        report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res))))  = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,res) , lev_CU_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation curtailed',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) )) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,res) , lev_CU_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation direct consumption',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) )) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,res) , lev_G_RES_PRO(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation to market',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res))) ) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,res) , lev_G_MARKET_PRO2M(scen,res,h) )) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res))) ) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,sto,res) , lev_STO_IN_PRO2PRO(scen,res,sto,h) )) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res))) ) = sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,sto,res) , lev_STO_IN_PRO2M(scen,res,sto,h) )) / sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_RES_PRO(scen,res)) ) ;
        report_prosumage('Capacity total prosumers',loop_res_share,loop_prosumage)= sum( res , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RES_PRO(scen,res)) ) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto)) ) ;

        report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_prosumage,res)$report_prosumage('Capacity total prosumers',loop_res_share,loop_prosumage)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RES_PRO(scen,res) ) / report_prosumage('Capacity total prosumers',loop_res_share,loop_prosumage)+ 1e-9 ;
        report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_prosumage,sto)$report_prosumage('Capacity total prosumers',loop_res_share,loop_prosumage)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P_PRO(scen,sto) ) / report_prosumage('Capacity total prosumers',loop_res_share,loop_prosumage)+ 1e-9 ;

        report_market('gross energy demand market',loop_res_share,loop_prosumage)= sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_market('curtailment of fluct res absolute market',loop_res_share,loop_prosumage)= sum((res,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h)+ lev_CU_energybal(scen,res,h))) * %sec_hour% ;
        report_market('curtailment of fluct res relative market',loop_res_share,loop_prosumage)$(sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (h,res) , phi_res(res,h)*lev_N_TECH(scen,res)) ) > eps_rep_abs*card(res)*card(h) ) = sum( (res,h), sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_CU(scen,res,h)+ lev_CU_energybal(scen,res,h)))/ sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( (res,h) , phi_res(res,h) * lev_N_TECH(scen,res)) ) ;
        report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h)) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_IN_M2PRO(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_IN_M2M(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( res , lev_G_MARKET_PRO2M(scen,res,h))) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_OUT_PRO2M(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
        report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_prosumage)= sum( h, sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( sto , lev_STO_OUT_M2M(scen,sto,h))) ) / sum( scen$(map(scen,loop_res_share,loop_prosumage)) , gross_energy_demand_market(scen)) ;
* line below changed
        report_market('Capacity total market',loop_res_share,loop_prosumage)= sum( tech , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,tech))) + sum( sto , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) )) ;
        report_market('Energy demand total market',loop_res_share,loop_prosumage)= (sum( h , d(h) ) + sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_MARKET_M2PRO(scen,h))) + sum( (sto,h) , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_IN(scen,sto,h) + lev_STO_IN_M2M(scen,sto,h) + lev_STO_IN_M2PRO(scen,sto,h) ) )) * %sec_hour%
%prosumage%$ontext
%prosumage%$ontext
%prosumage%$ontext
%prosumage%$ontext
;
        report_market('energy generated gross market',loop_res_share,loop_prosumage)= sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , sum( dis ,lev_G_L(scen,dis,h)) + sum( nondis ,lev_G_RES(scen,nondis,h)) + sum( res , lev_G_MARKET_PRO2M(scen,res,h) - corr_fac_nondis(scen,res,h))
*         + sum( rsvr , lev_RSVR_OUT(scen,rsvr,h)- corr_fac_rsvr(scen,rsvr,h))
         + sum( sto , lev_STO_OUT_M2M(scen,sto,h) + lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h))
*         + sum( dsm_shift , lev_DSM_DO_DEMAND(scen,dsm_shift,h) - corr_fac_dsm_shift(scen,dsm_shift,h)) + sum( dsm_curt , lev_DSM_CU(scen,dsm_curt,h)) + sum( ev , lev_EV_DISCHARGE(scen,ev,h)) - corr_fac_ev(scen,h)
          )) ;

        report_market_tech('Capacity share market',loop_res_share,loop_prosumage,tech)$(report_market('Capacity total market',loop_res_share,loop_prosumage)) = sum( scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_TECH(scen,tech) ) / report_market('Capacity total market',loop_res_share,loop_prosumage)+ 1e-9 ;
*        report_market_tech('Capacity share market',loop_res_share,loop_prosumage,rsvr)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_RSVR_P(scen,rsvr) ) / report_market('Capacity total market',loop_res_share,loop_prosumage)+ 1e-9 ;
        report_market_tech('Capacity share market',loop_res_share,loop_prosumage,sto)= sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_N_STO_P(scen,sto) ) / report_market('Capacity total market',loop_res_share,loop_prosumage)+ 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,con)$(report_market('energy generated gross market',loop_res_share,loop_prosumage)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,con,h)) ) / report_market('energy generated gross market',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,res)$(report_market('energy generated gross market',loop_res_share,loop_prosumage)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_G_L(scen,res,h) + lev_G_MARKET_PRO2M(scen,res,h) + lev_G_RES(scen,res,h) - corr_fac_nondis(scen,res,h)) ) / report_market('energy generated gross market',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;
*        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,rsvr)$(AND report_market('energy generated gross market',loop_res_share,loop_prosumage)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_RSVR_OUT(scen,rsvr,h) - corr_fac_rsvr(scen,rsvr,h)) ) / report_market('energy generated gross market',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;
        report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,sto)$(report_market('energy generated gross market',loop_res_share,loop_prosumage)) = sum( h , sum(scen$(map(scen,loop_res_share,loop_prosumage)) , lev_STO_OUT_M2M(scen,sto,h) + lev_STO_OUT(scen,sto,h) - corr_fac_sto(scen,sto,h)) ) / report_market('energy generated gross market',loop_res_share,loop_prosumage)* %sec_hour% + 1e-9 ;

                 report_hours('demand prosumers',loop_res_share,loop_prosumage,h)$(report_hours('demand prosumers',loop_res_share,loop_prosumage,h)< eps_rep_abs) = 0 ;
                 report_hours('demand market',loop_res_share,loop_prosumage,h)$(report_hours('demand market',loop_res_share,loop_prosumage,h)< eps_rep_abs) = 0 ;

                 report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_prosumage,res,h)$(report_prosumage_tech_hours('generation prosumers',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_prosumage,res,h)$(report_prosumage_tech_hours('curtailment of fluct res prosumers',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_prosumage,res,h)$(report_prosumage_tech_hours('generation prosumers self-consumption',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_prosumage,res,h)$(report_prosumage_tech_hours('generation prosumers to market',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_prosumage,'',h)$(report_prosumage_tech_hours('withdrawal prosumers from market',loop_res_share,loop_prosumage,'',h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage loading prosumers M2M',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage generation prosumers M2M',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers PRO2PRO',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers PRO2M',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers M2PRO',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_prosumage,sto,h)$(report_prosumage_tech_hours('storage level prosumers M2M',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;

                 report_market_tech_hours('generation market',loop_res_share,loop_prosumage,con,h)$(report_market_tech_hours('generation market',loop_res_share,loop_prosumage,con,h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('generation market',loop_res_share,loop_prosumage,res,h)$(report_market_tech_hours('generation market',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) = 0 ;
*                 report_market_tech_hours('generation market',loop_res_share,loop_prosumage,rsvr,h)$(report_market_tech_hours('generation market',loop_res_share,loop_prosumage,rsvr,h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_prosumage,res,h)$(report_market_tech_hours('curtailment of fluct res market',loop_res_share,loop_prosumage,res,h)< eps_rep_abs) =  0 ;
                 report_market_tech_hours('generation storage market',loop_res_share,loop_prosumage,sto,h)$(report_market_tech_hours('generation storage market',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) =  0 ;
                 report_market_tech_hours('storage loading market',loop_res_share,loop_prosumage,sto,h)$(report_market_tech_hours('storage loading market',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('storage level market',loop_res_share,loop_prosumage,sto,h)$(report_market_tech_hours('storage level market',loop_res_share,loop_prosumage,sto,h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('market to prosumer storage M2PRO',loop_res_share,loop_prosumage,'Interaction with prosumers',h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('market to prosumer storage M2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('prosumer storage to market PRO2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('prosumer storage to market M2M',loop_res_share,loop_prosumage,'Interaction with prosumers',h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('energy market to prosumer',loop_res_share,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('energy market to prosumer',loop_res_share,loop_prosumage,'Interaction with prosumers',h)< eps_rep_abs) = 0 ;
                 report_market_tech_hours('energy prosumer to market',loop_res_share,loop_prosumage,'Interaction with prosumers',h)$(report_market_tech_hours('energy prosumer to market',loop_res_share,loop_prosumage,'Interaction with prosumers',h)< eps_rep_abs) = 0 ;

                 report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_prosumage,res)$(report_prosumage_tech('capacities renewable prosumers',loop_res_share,loop_prosumage,res)< eps_rep_ins) =  0 ;
                 report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('capacities storage MW prosumers',loop_res_share,loop_prosumage,sto)< eps_rep_ins) =  0 ;
                 report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('capacities storage MWh prosumers',loop_res_share,loop_prosumage,sto)< eps_rep_ins) =  0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,res)$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
                 report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,'market')$(report_prosumage_tech('consumption share prosumers',loop_res_share,loop_prosumage,'market')< eps_rep_rel) = 0 ;
                 report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_prosumage,res)$(report_prosumage_tech('curtailment of fluct res absolute prosumers',loop_res_share,loop_prosumage,res)< eps_rep_ins) = 0 ;
                 report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_prosumage,res)$(report_prosumage_tech('curtailment of fluct res relative prosumers',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
                 report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers PRO2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage in total prosumers M2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers PRO2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers M2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage out total prosumers',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('Generation total prosumers PRO2M',loop_res_share,loop_prosumage,'')< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('Withdrawal total prosumers M2PRO',loop_res_share,loop_prosumage,'')< eps_rep_abs*card(h)) = 0 ;
                 report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('generation prosumers self-consumption',loop_res_share,loop_prosumage,'')< eps_rep_abs * card(h)) = 0 ;
                 report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in PRO2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in PRO2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in M2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage in M2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out PRO2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out PRO2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out M2PRO',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('average market value storage out M2M',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('average market value generation PRO2M',loop_res_share,loop_prosumage,'')< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('average market value withdrawal M2PRO',loop_res_share,loop_prosumage,'')< eps_rep_abs) = 0 ;
                 report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_prosumage,'')$(report_prosumage_tech('average market value generation PRO2PRO',loop_res_share,loop_prosumage,'')< eps_rep_abs) = 0 ;
                 report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_prosumage,res)$(report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
                 report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Capacity share prosumers',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;
                 report_prosumage_tech('FLH prosumers',loop_res_share,loop_prosumage,res)$(report_prosumage_tech('FLH prosumers',loop_res_share,loop_prosumage,res)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage cycles prosumers',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('FLH',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('FLH',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_prosumage,sto)$(report_prosumage_tech('Storage EP-ratio prosumers',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;

                 report_market_tech('capacities renewable market',loop_res_share,loop_prosumage,res)$(report_market_tech('capacities renewable market',loop_res_share,loop_prosumage,res)< eps_rep_abs) =  0 ;
                 report_market_tech('capacities storage MW market',loop_res_share,loop_prosumage,sto)$(report_market_tech('capacities storage MW market',loop_res_share,loop_prosumage,sto)< eps_rep_abs) =  0 ;
                 report_market_tech('capacities storage MWh market',loop_res_share,loop_prosumage,sto)$(report_market_tech('capacities storage MWh market',loop_res_share,loop_prosumage,sto)< eps_rep_abs) =  0 ;
                 report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_prosumage,res)$(report_market_tech('curtailment of fluct res absolute market',loop_res_share,loop_prosumage,res)< eps_rep_abs*card(h)) = 0 ;
                 report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_prosumage,res)$(report_market_tech('curtailment of fluct res relative market',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
                 report_market_tech('capacities conventional market',loop_res_share,loop_prosumage,con)$(report_market_tech('capacities conventional market',loop_res_share,loop_prosumage,con)< eps_rep_ins) = 0 ;
                 report_market_tech('capacities renewable market',loop_res_share,loop_prosumage,res)$(report_market_tech('capacities renewable market',loop_res_share,loop_prosumage,res)< eps_rep_ins) = 0 ;
                 report_market_tech('capacities storage MW market',loop_res_share,loop_prosumage,sto)$(report_market_tech('capacities storage MW market',loop_res_share,loop_prosumage,sto)< eps_rep_ins) =  0 ;
                 report_market_tech('capacities storage MWh market',loop_res_share,loop_prosumage,sto)$(report_market_tech('capacities storage MWh market',loop_res_share,loop_prosumage,sto)< eps_rep_ins) = 0 ;
                 report_market_tech('Storage out total market non-reserves',loop_res_share,loop_prosumage,sto)$(report_market_tech('Storage out total market non-reserves',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_market_tech('Storage in total market non-reserves',loop_res_share,loop_prosumage,sto)$(report_market_tech('Storage in total market non-reserves',loop_res_share,loop_prosumage,sto)< eps_rep_abs*card(h)) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,res)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,res)< eps_rep_abs) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,con)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,con)< eps_rep_abs) = 0 ;
                 report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,sto)$(report_market_tech('FLH market non-reserves',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_prosumage,sto)$(report_market_tech('Storage cycles market non-reserves',loop_res_share,loop_prosumage,sto)< eps_rep_abs) = 0 ;
                 report_market_tech('Storage EP-ratio market',loop_res_share,loop_prosumage,sto)$(report_market_tech('Storage EP-ratio market',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;

                 report_prosumage('gross energy demand prosumers',loop_res_share,loop_prosumage)$(report_prosumage('gross energy demand prosumers',loop_res_share,loop_prosumage)< eps_rep_ins) = 0 ;
                 report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_prosumage)$(report_prosumage('gross energy demand prosumers from market',loop_res_share,loop_prosumage)< eps_rep_ins) = 0 ;
                 report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_prosumage)$(report_prosumage('gross energy demand prosumers self generation',loop_res_share,loop_prosumage)< eps_rep_ins) = 0 ;
                 report_prosumage('self-generation share prosumage total',loop_res_share,loop_prosumage)$(report_prosumage('self-generation share prosumage total',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('market share prosumage',loop_res_share,loop_prosumage)$(report_prosumage('market share prosumage',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_prosumage)$(report_prosumage('curtailment of fluct res absolute prosumers',loop_res_share,loop_prosumage)< eps_rep_ins) = 0 ;
                 report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_prosumage)$(report_prosumage('curtailment of fluct res relative prosumers',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation curtailed',loop_res_share,loop_prosumage)$(report_prosumage('share self-generation curtailed',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation direct consumption',loop_res_share,loop_prosumage)$(report_prosumage('share self-generation direct consumption',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation to market',loop_res_share,loop_prosumage)$(report_prosumage('share self-generation to market',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_prosumage)$(report_prosumage('share self-generation stored PRO2PRO',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_prosumage)$(report_prosumage('share self-generation stored PRO2M',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_prosumage('Capacity total prosumers',loop_res_share,loop_prosumage)$(report_prosumage('Capacity total prosumers',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;

                 report_market('gross energy demand market',loop_res_share,loop_prosumage)$(report_market('gross energy demand market',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_market('curtailment of fluct res absolute market',loop_res_share,loop_prosumage)$(report_market('curtailment of fluct res absolute market',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_market('curtailment of fluct res relative market',loop_res_share,loop_prosumage)$(report_market('curtailment of fluct res relative market',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_prosumage)$(report_market('Share market energy transferred to prosumer consumption',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_prosumage)$(report_market('Share market energy transferred to prosumer storage M2PRO',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_prosumage)$(report_market('Share market energy transferred to prosumer storage M2M',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_prosumage)$(report_market('Share market energy tranferred from prosumer generation',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_prosumage)$(report_market('Share market energy transferred from prosumer storage PRO2M',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_prosumage)$(report_market('Share market energy transferred from prosumer storage M2M',loop_res_share,loop_prosumage)< eps_rep_rel) = 0 ;
                 report_market('Capacity total market',loop_res_share,loop_prosumage)$(report_market('Capacity total market',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_market('Energy demand total market',loop_res_share,loop_prosumage)$(report_market('Energy demand total market',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;
                 report_market('energy generated gross market',loop_res_share,loop_prosumage)$(report_market('energy generated gross market',loop_res_share,loop_prosumage)< eps_rep_abs) = 0 ;

                 report_market_tech('Capacity share market',loop_res_share,loop_prosumage,tech)$(report_market_tech('Capacity share market',loop_res_share,loop_prosumage,tech)< eps_rep_rel) = 0 ;
                 report_market_tech('Capacity share market',loop_res_share,loop_prosumage,sto)$(report_market_tech('Capacity share market',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;
*                 report_market_tech('Capacity share market',loop_res_share,loop_prosumage,rsvr)$(report_market_tech('Capacity share market',loop_res_share,loop_prosumage,rsvr)< eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,con)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,con)< eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,res)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,res)< eps_rep_rel) = 0 ;
                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,sto)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,sto)< eps_rep_rel) = 0 ;
*                 report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,rsvr)$(report_market_tech('Energy share in gross nodal market generation',loop_res_share,loop_prosumage,rsvr)< eps_rep_rel) = 0 ;
$ontext
$offtext




$onecho >results.tmp
par=report            rng=report!A1             rdim=1 cdim=2
par=report_cost       rng=report_cost!A1        rdim=1 cdim=2
par=report_hours      rng=report_hours!A1       rdim=2 cdim=2
par=report_node       rng=report_node!A1        rdim=1 cdim=2
par=report_tech       rng=report_tech!A1        rdim=2 cdim=2
par=report_tech_hours rng=report_tech_hours!A1  rdim=3 cdim=2
$offecho

%prosumage%$ontext

$onecho >results.tmp
par=report_prosumage            rng=report_prosumage!A1               rdim=1 cdim=2
par=report_prosumage_tech       rng=report_prosumage_tech!A1          rdim=2 cdim=2
par=report_prosumage_tech_hours rng=report_prosumage_tech_hours!A1    rdim=3 cdim=2
par=report_market               rng=report_market!A1                  rdim=1 cdim=2
par=report_market_tech          rng=report_market_tech!A1             rdim=2 cdim=2
par=report_market_tech_hours    rng=report_market_tech_hours!A1       rdim=3 cdim=2
$offecho

$ontext
$offtext




