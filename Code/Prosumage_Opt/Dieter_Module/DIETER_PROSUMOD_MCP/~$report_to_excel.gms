
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables DIETER).
Version 1.3.0, October 2018.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License MIT).
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
excel_capacities
excel_capacities_storage
excel_res_curt
excel_prices
excel_dispatch
excel_hours
;

excel_cost('Nodal cost: dispatch & investment') = sum( (loop_res_share,loop_prosumage) , report_cost('Nodal cost: dispatch',loop_res_share,loop_prosumage) + report_cost('Nodal cost: investment & fix',loop_res_share,loop_prosumage));
excel_cost('Nodal cost: dispatch') = sum( (loop_res_share,loop_prosumage) , report_cost('Nodal cost: dispatch',loop_prosumage)) ;
excel_cost('Nodal cost: investment and fix') = sum( (loop_res_share,loop_prosumage) , report_cost('Nodal cost: investment & fix',loop_res_share,loop_prosumage)) ;
excel_cost('Energy generated net') = sum( (loop_res_share,loop_prosumage) , report('energy generated net',loop_res_share,,loop_prosumage)) ;
excel_cost('Average cost') = excel_cost('Nodal cost: dispatch & investment')/excel_cost('Energy generated net') ;

excel_capacities('Capacities',tech) = sum( (loop_res_share,loop_prosumage) , report_tech('capacities conventional',loop_res_share,loop_prosumage,tech) + report_tech('capacities renewable',loop_res_share,loop_prosumage,tech)) ;
excel_capacities('Capacities',sto) = sum( (loop_res_share,loop_prosumage) , report_tech('capacities storage MW',loop_res_share,loop_prosumage,sto) ) ;

excel_res_curt('res share') = sum( (loop_res_share,loop_prosumage) , report_node('renshare in nodal gross demand',loop_res_share,loop_prosumage) ) ;
excel_res_curt('curtailment absolute') = sum( (loop_res_share,loop_prosumage) , report('curtailment of fluct res absolute',loop_res_share,loop_prosumage) ) ;
excel_res_curt('curtailment relative') = sum( (loop_res_share,loop_prosumage) , report('curtailment of fluct res relative',loop_res_share,loop_prosumage) ) ;

excel_prices('max price') = sum( (loop_res_share,loop_prosumage) , report_node('max price',loop_res_share,loop_prosumage) ) ;
excel_prices('mean price') = sum( (loop_res_share,loop_prosumage) , report_node('mean price',loop_res_share,loop_prosumage) ) ;
excel_prices('min price') = sum( (loop_res_share,loop_prosumage) , report_node('min price',loop_res_share,loop_prosumage) ) ;

excel_dispatch('technology shares in nodal gross demand',res) = sum( (loop_res_share,loop_prosumage) , report_tech('renshares in nodal gross demand',loop_res_share,loop_prosumage,res) ) ;
excel_dispatch('technology shares in nodal gross demand',con) = sum( (loop_res_share,loop_prosumage) , report_tech('conshares in nodal gross demand',loop_res_share,loop_prosumage,con) ) ;

excel_dispatch('yearly energy',tech) = sum( (loop_res_share,loop_prosumage) , report_tech('Yearly energy',loop_res_share,loop_prosumage,tech) ) ;
excel_dispatch('yearly energy',sto) = sum( (loop_res_share,loop_prosumage) , report_tech('Yearly energy',loop_res_share,loop_prosumage,sto) ) ;

excel_hours('dispatch',res,h) = sum( (loop_res_share,loop_prosumage) , report_tech_hours('generation renewable',loop_res_share,loop_prosumage,res,h) ) ;

execute_unload "results_to_excel", excel_cost excel_capacities excel_res_curt excel_prices excel_dispatch excel_hours

;


execute 'gdxxrw.exe results_to_excel.gdx par=excel_cost rng=a2 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_capacities rng=a10' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_res_curt rng=a20 rdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_prices rng=a30 cdim=1' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_dispatch rng=a40' ;
execute 'gdxxrw.exe results_to_excel.gdx par=excel_hours rng=a45' ;
