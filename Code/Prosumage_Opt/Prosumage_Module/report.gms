

********************************************************************************
**** Parameters for report file  ***********************************************
********************************************************************************

Parameter
calc_maxprice
calc_minprice

report
report_tech
report_tech_hours
report_hours
report_cost

gross_energy_demand
full_load_hours
energy_to_market
energy_from_market
self_consumption_rate
self_sufficiency_rate

;




********************************************************************************
**** Initialize reporting paremetrs  *******************************************
********************************************************************************
* ----------------------------------------------------------------------------

* Define gross energy demand for reporting
gross_energy_demand = sum( h , d_PRO(h)  )   ;

* Define full load hours of PV
full_load_hours(res_pro)     = sum( h , avail_solar_PRO(h));

* Define market interaction parameters
energy_to_market                   = sum ( (res_pro,h),  G_MARKET_PRO2M.l(res_pro,h) )      ;
energy_from_market                 = sum ( h, G_MARKET_M2PRO.l(h) )               ;
self_consumption_rate              = sum ( (sto_pro,res_pro,h), G_RES_PRO.l(res_pro,h) + STO_IN_PRO2PRO.l(sto_pro,res_pro,h) ) / sum(res_pro, full_load_hours(res_pro) );
self_sufficiency_rate              = sum ( (sto_pro,res_pro,h), G_RES_PRO.l(res_pro,h) + STO_IN_PRO2PRO.l(sto_pro,res_pro,h) ) / gross_energy_demand   ;


;


********************************************************************************
**** Report  *******************************************************************
********************************************************************************

       report('Household demand')      = gross_energy_demand ;
       report('Energy from market')    = energy_from_market ;
       report('Energy to   market')    = energy_to_market ;
       report('Self consumption rate') = self_consumption_rate ;
       report('Self sufficiency rate') = self_sufficiency_rate ;
* ----------------------------------------------------------------------------

* REPORT HOURS
        report_hours('demand consumers',h)         = d_PRO(h) ;
        report_hours('energy generated',h)         =  sum( res_pro, avail_solar_PRO(h)*N_RES_PRO.l(res_pro) ) ;
        report_hours('energy curtailed',h)         =  sum( res_pro, CU_PRO.l(res_pro, h));
        report_hours('energy directly consumed',h) =  sum( res_pro, G_RES_PRO.l(res_pro,h));
        report_hours('energy to market',h)         =  sum( res_pro, G_MARKET_PRO2M.l(res_pro,h));
        report_hours('energy from market',h)       =  G_MARKET_M2PRO.l(h) ;
        report_hours('energy stored in',h)         =  sum( (sto_pro,res_pro), STO_IN_PRO2PRO.l(sto_pro,res_pro,h));
        report_hours('energy stored out',h)        =  sum( (sto_pro), STO_OUT_PRO2PRO.l(sto_pro,h));
        report_hours('storage level',h)            =  sum( (sto_pro), STO_L_PRO2PRO.l(sto_pro,h));

* REPORT TECH HOURS
        report_tech_hours('energy curtailed',res_pro,h)=  CU_PRO.l(res_pro,h);


* REPORT TECH
         report_tech('Full load hours', res_pro)  = full_load_hours(res_pro) ;

* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
