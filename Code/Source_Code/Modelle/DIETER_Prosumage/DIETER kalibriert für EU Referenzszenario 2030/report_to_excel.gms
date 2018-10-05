
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
**** Reporting to Excel              ****
*****************************************


Parameter
excel_cost
excel_cost_building_heat
excel_cost_building
excel_cost_building_heat_other
excel_heat
excel_capacities
excel_capacities_storage
excel_res_curt
excel_prices
excel_dispatch
excel_reserves
;

Parameter
building_size(bu) /
bu1      176
bu2      584
bu3      192
bu4      2739
bu5      160
bu6      1815
bu7      137
bu8      1182
bu9      153
bu10     715
bu11     150
bu12     700
/
;


excel_cost('Nodal cost: dispatch & investment') = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report_cost('Nodal cost: dispatch',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n)) ;
excel_cost('Nodal cost: dispatch') = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report_cost('Nodal cost: dispatch',loop_res_share,loop_ev,loop_prosumage,n) + report_cost('Nodal cost: infeasibility',loop_res_share,loop_ev,loop_prosumage,n)) ;
excel_cost('Nodal cost: investment and fix') = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report_cost('Nodal cost: investment & fix',loop_res_share,loop_ev,loop_prosumage,n)) ;
excel_cost('Energy generated net') = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report('energy generated net',loop_res_share,loop_ev,loop_prosumage)) ;
excel_cost('Average cost') = excel_cost('Nodal cost: dispatch & investment')/excel_cost('Energy generated net') ;

excel_cost_building_heat('Costs per square meter',bu,'setsh')$(sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh'))) = sum((n,loop_res_share,loop_ev,loop_prosumage) , report_heat_tech('total costs',loop_res_share,loop_ev,loop_prosumage,n,bu,'setsh')) / sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) ;
excel_cost_building_heat('Costs per square meter heating',bu,'setsh')$(sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh'))) = sum( (n,h,loop_res_share,loop_ev,loop_prosumage) , report_heat_tech_hours('price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,'setsh',h) * sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , ( theta_sets(n,bu,'setsh') * (lev_H_SETS_IN(scen,n,bu,'setsh',h) + corr_fac_sets(scen,n,bu,'setsh',h))))) / sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) ;
excel_cost_building_heat('Costs per square meter DHW',bu,'setsh')$(sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh'))) = sum( (n,h,loop_res_share,loop_ev,loop_prosumage) , report_heat_tech_hours('price DHW electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,'setsh',h) * sum( scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , (theta_sets(n,bu,'setsh') * (lev_H_DHW_AUX_ELEC_IN(scen,n,bu,'setsh',h) + corr_fac_sets_aux(scen,n,bu,'setsh',h))))) / sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) ;
excel_cost_building_heat('Costs per building',bu,'setsh')$(excel_cost_building_heat('Costs per square meter',bu,'setsh')) = excel_cost_building_heat('Costs per square meter',bu,'setsh') * building_size(bu) ;
excel_cost_building_heat('Costs per building heating',bu,'setsh')$(excel_cost_building_heat('Costs per square meter heating',bu,'setsh')) = excel_cost_building_heat('Costs per square meter heating',bu,'setsh') * building_size(bu) ;
excel_cost_building_heat('Costs per building DHW',bu,'setsh')$(excel_cost_building_heat('Costs per square meter DHW',bu,'setsh')) = excel_cost_building_heat('Costs per square meter DHW',bu,'setsh') * building_size(bu) ;
excel_cost_building_heat('Reserve revenues heating per square meter',bu,'setsh')$(sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh'))) = (sum( (loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n) , (sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,'setsh') * lev_RP_SETS(scen,n,reserves_nonprim,bu,'setsh',h))) * report_reserves_hours('reserve prices',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n)) ) / sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) ;
excel_cost_building_heat('Reserve revenues DHW per square meter',bu,'setsh')$(sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh'))) = sum( (loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n) , (sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_sets(n,bu,'setsh') * lev_RP_SETS_AUX(scen,n,reserves_nonprim,bu,'setsh',h))) * report_reserves_hours('reserve prices',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n) ) / sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) ;
excel_cost_building_heat('Reserve revenues heating per building',bu,'setsh')$(excel_cost_building_heat('Reserve revenues heating per square meter',bu,'setsh')) = excel_cost_building_heat('Reserve revenues heating per square meter',bu,'setsh') * building_size(bu) ;
excel_cost_building_heat('Reserve revenues DHW per building',bu,'setsh')$(excel_cost_building_heat('Reserve revenues DHW per square meter',bu,'setsh')) = excel_cost_building_heat('Reserve revenues DHW per square meter',bu,'setsh') * building_size(bu) ;
excel_cost_building_heat('Mean price heating',bu,'setsh')$(sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,'setsh'))) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,'setsh') ) ;

excel_cost_building_heat_other('Costs per square meter',bu,hst)$(sum( n, area_floor(n,bu,hst)*phi_heat_type(n,bu,hst))) = sum((n,loop_res_share,loop_ev,loop_prosumage) , report_heat_tech('total costs',loop_res_share,loop_ev,loop_prosumage,n,bu,hst)) / sum( n, area_floor(n,bu,hst)*phi_heat_type(n,bu,hst)) ;
excel_cost_building_heat_other('Costs per building',bu,hst)$(excel_cost_building_heat_other('Costs per square meter',bu,hst)) = excel_cost_building_heat_other('Costs per square meter',bu,hst) * building_size(bu) ;
excel_cost_building_heat_other('Reserve revenues per square meter',bu,hst)$(sum( n, area_floor(n,bu,hst)*phi_heat_type(n,bu,hst))) = (sum( (loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n) , (sum(scen$(map(scen,loop_res_share,loop_ev,loop_prosumage)) , theta_storage(n,bu,hst)*(lev_RP_HP(scen,n,reserves_nonprim,bu,hst,h) + lev_RP_H_ELEC(scen,n,reserves_nonprim,bu,hst,h)) )) * report_reserves_hours('reserve prices',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,h,n)) ) / sum( n, area_floor(n,bu,hst)*phi_heat_type(n,bu,hst)) ;
excel_cost_building_heat_other('Reserve revenues per building',bu,hst)$(excel_cost_building_heat_other('Reserve revenues per square meter',bu,hst)) = excel_cost_building_heat_other('Reserve revenues per square meter',bu,hst) * building_size(bu) ;
excel_cost_building_heat_other('Heating mean price',bu,hst)$(sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,hst))) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_heat_tech('average price heat electricity consumption',loop_res_share,loop_ev,loop_prosumage,n,bu,hst) ) ;

excel_cost_building('Costs per square meter over building types',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = sum((n,bu,loop_res_share,loop_ev,loop_prosumage) , report_heat_tech('total costs',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)) / sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch)) ;
excel_cost_building('Costs per square meter over building types','NETS') = sum((n,loop_res_share,loop_ev,loop_prosumage) , sum( h, nets_profile(h) * report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n)) ) / 60.03819066E+6 ;
excel_cost_building('Costs over all buildings',ch)$(excel_cost_building('Costs per square meter over building types',ch)) = excel_cost_building('Costs per square meter over building types',ch)*sum( (n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch));
excel_cost_building('Average reserves revenue heating per m2 over all building types','setsh')$(sum( (n,bu) , area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh'))) = sum( bu , (excel_cost_building_heat('Reserve revenues heating per square meter',bu,'setsh')*sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')))) / sum( (n,bu) , area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) ;
excel_cost_building('Average reserves revenue heating per m2 over all building types',hp)$sum( (n,bu) , area_floor(n,bu,hp)*phi_heat_type(n,bu,hp)) = sum( bu , (excel_cost_building_heat_other('Reserve revenues per square meter',bu,hp)*sum( n, area_floor(n,bu,hp)*phi_heat_type(n,bu,hp)))) / sum( (n,bu) , area_floor(n,bu,hp)*phi_heat_type(n,bu,hp)) ;
excel_cost_building('Average reserves revenue DHW per m2 over all building types','setsh')$sum( (n,bu) , area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) = sum( bu , (excel_cost_building_heat_other('Reserve revenues DHW per square meter',bu,'setsh')*sum( n, area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')))) / sum( (n,bu) , area_floor(n,bu,'setsh')*phi_heat_type(n,bu,'setsh')) ;

excel_heat('Heat supply',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = sum( (n,bu,loop_res_share,loop_ev,loop_prosumage) , report_heat_tech('heat supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch));
excel_heat('DHW supply',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = sum( (n,bu,loop_res_share,loop_ev,loop_prosumage) , report_heat_tech('DHW supply',loop_res_share,loop_ev,loop_prosumage,n,bu,ch)) ;
excel_heat('Total heat supply',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = excel_heat('Heat supply',ch) + excel_heat('DHW supply',ch) ;
excel_heat('Savings natural gas replacement',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = sum( n , fuelprice(n,'CCGT')) * sum ( (loop_res_share,loop_ev,loop_prosumage) , excel_heat('Total heat supply',ch) ) ;
excel_heat('Savings natural gas replacement incl CO2 price',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = ( sum( n , fuelprice(n,'CCGT')) + sum( n , carbon_content(n,'CCGT')*CO2price(n,'CCGT')) ) * excel_heat('Total heat supply',ch) ;
excel_heat('Total energy in MWh',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = sum( (n,bu) , n_sets_e(n,bu,ch) ) ;
excel_heat('Total power in',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = sum( (n,bu) , n_sets_p_in(n,bu,ch) ) ;
excel_heat('Total power out',ch)$(sum((n,bu) , area_floor(n,bu,ch)*phi_heat_type(n,bu,ch))) = sum( (n,bu) , n_sets_p_out(n,bu,ch) ) ;
excel_heat('Mean price heating',ch)$(sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_heat_tech('average price heat electricity consumption over all bu',loop_res_share,loop_ev,loop_prosumage,n,'over all bu',ch))) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_heat_tech('average price heat electricity consumption over all bu',loop_res_share,loop_ev,loop_prosumage,n,'over all bu',ch) ) ;
excel_heat('Mean price heating','NETS') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , sum( h, nets_profile(h) * report_hours('price',loop_res_share,loop_ev,loop_prosumage,h,n)) / sum( h, nets_profile(h)) ) ;

excel_capacities('Capacities',tech) = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report_tech('capacities conventional',loop_res_share,loop_ev,loop_prosumage,tech,n) + report_tech('capacities renewable',loop_res_share,loop_ev,loop_prosumage,tech,n)) ;
excel_capacities('Capacities',rsvr) = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report_tech('capacities reservoir MW',loop_res_share,loop_ev,loop_prosumage,rsvr,n) ) ;
excel_capacities('Capacities',sto) = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report_tech('capacities storage MW',loop_res_share,loop_ev,loop_prosumage,sto,n) ) ;

excel_res_curt('res share') = sum( (n,loop_res_share,loop_ev,loop_prosumage) , report_node('renshare in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,n) ) ;
excel_res_curt('curtailment absolute') = sum( (loop_res_share,loop_ev,loop_prosumage) , report('curtailment of fluct res absolute',loop_res_share,loop_ev,loop_prosumage) ) ;
excel_res_curt('curtailment relative') = sum( (loop_res_share,loop_ev,loop_prosumage) , report('curtailment of fluct res relative',loop_res_share,loop_ev,loop_prosumage) ) ;

excel_prices('max price') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_node('max price',loop_res_share,loop_ev,loop_prosumage,n) ) ;
excel_prices('mean price') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_node('mean price',loop_res_share,loop_ev,loop_prosumage,n) ) ;
excel_prices('min price') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_node('min price',loop_res_share,loop_ev,loop_prosumage,n) ) ;

excel_dispatch('technology shares in nodal gross demand',res) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,res,n) ) ;
excel_dispatch('technology shares in nodal gross demand',rsvr) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_tech('renshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,rsvr,n) ) ;
excel_dispatch('technology shares in nodal gross demand',con) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_tech('conshares in nodal gross demand',loop_res_share,loop_ev,loop_prosumage,con,n) ) ;

excel_dispatch('yearly energy',tech) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,tech,n) ) ;
excel_dispatch('yearly energy',sto) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,sto,n) ) ;
excel_dispatch('yearly energy',dsm) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_tech('Yearly energy',loop_res_share,loop_ev,loop_prosumage,dsm,n) ) ;

%reserves%execute_unload "results_to_excel", excel_cost excel_cost_building_heat excel_cost_building_heat_other excel_cost_building excel_heat excel_capacities excel_res_curt excel_prices excel_dispatch ;

%reserves%$ontext
excel_reserves('provision shares prim',reserves_prim,dis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n) ) ;
excel_reserves('provision shares prim',reserves_prim,nondis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n) ) ;
excel_reserves('provision shares prim',reserves_prim,rsvr) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n) ) ;
excel_reserves('provision shares prim',reserves_prim,sto) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n) ) ;
excel_reserves('provision shares prim',reserves_prim,'sets') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'sets',n) ) ;
excel_reserves('provision shares prim',reserves_prim,'sets aux') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'sets aux',n) ) ;
excel_reserves('provision shares prim',reserves_prim,hst) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,hst,n) ) ;
excel_reserves('provision shares prim',reserves_prim,hp) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,hp,n) ) ;
excel_reserves('provision shares prim',reserves_prim,hel) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,hel,n) ) ;

excel_reserves('activation shares prim',reserves_prim,dis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,dis,n) ) ;
excel_reserves('activation shares prim',reserves_prim,nondis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,nondis,n) ) ;
excel_reserves('activation shares prim',reserves_prim,rsvr) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,rsvr,n) ) ;
excel_reserves('activation shares prim',reserves_prim,sto) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,sto,n) ) ;
excel_reserves('activation shares prim',reserves_prim,'sets') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'sets',n) ) ;
excel_reserves('activation shares prim',reserves_prim,'sets aux') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,'sets aux',n) ) ;
excel_reserves('activation shares prim',reserves_prim,hst) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,hst,n) ) ;
excel_reserves('activation shares prim',reserves_prim,hp) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,hp,n) ) ;
excel_reserves('activation shares prim',reserves_prim,hel) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_prim,hel,n) ) ;

excel_reserves('provision shares nonprim',reserves_nonprim,dis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,nondis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,rsvr) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,sto) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,'sets') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,'sets aux') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,hst) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,hp) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hp,n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,hel) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hel,n) ) ;

excel_reserves('activation shares nonprim',reserves_nonprim,dis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dis,n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,nondis) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,nondis,n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,rsvr) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,rsvr,n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,sto) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,sto,n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,'sets') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets',n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,'sets aux') = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,'sets aux',n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,hst) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hst,n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,hp) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hp,n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,hel) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,hel,n) ) ;

%DSM%$ontext
excel_reserves('provision shares nonprim',reserves_nonprim,dsm_curt) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n) ) ;
excel_reserves('provision shares nonprim',reserves_nonprim,dsm_shift) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve provision shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n) ) ;

excel_reserves('activation shares nonprim',reserves_nonprim,dsm_curt) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_curt,n) ) ;
excel_reserves('activation shares nonprim',reserves_nonprim,dsm_shift) = sum( (loop_res_share,loop_ev,loop_prosumage,n) , report_reserves_tech('reserve activation shares',loop_res_share,loop_ev,loop_prosumage,reserves_nonprim,dsm_shift,n) ) ;
$ontext
$offtext

%reserves%$ontext
execute_unload "results_to_excel", excel_cost excel_cost_building_heat excel_cost_building_heat_other excel_cost_building excel_heat excel_capacities excel_res_curt excel_prices excel_dispatch excel_reserves ;
$ontext
$offtext

execute 'gdxxrw.exe results_to_excel.gdx par=excel_cost rng=a2 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_cost_building_heat rng=a12 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_cost_building_heat_other rng=a26 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_cost_building rng=a34 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_heat rng=a40 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_capacities rng=a52' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_res_curt rng=a56 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_prices rng=a60 cdim=1' ;
%reserves%$ontext
execute 'gdxxrw.exe results_to_excel.gdx par=excel_reserves rng=a64' ;
$ontext
$offtext
execute 'gdxxrw.exe results_to_excel.gdx par=excel_dispatch rng=a82' ;
