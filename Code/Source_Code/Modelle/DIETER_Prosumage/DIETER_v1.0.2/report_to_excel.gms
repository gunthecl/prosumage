
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


*****************************************
**** Reporting to Excel              ****
*****************************************


Parameter report_to_excel ;

report_to_excel(loop_res_share,'Capacity shares',ct) =  report_tech('Capacity share'%res_share%,ct) ;
report_to_excel(loop_res_share,'Capacity shares',res) =  report_tech('Capacity share'%res_share%,res) ;

report_to_excel(loop_res_share,'Capacity shares',sto) =  report_tech('Capacity share'%res_share%,sto) ;
report_to_excel(loop_res_share,'Capacity shares',dsm_shift) =  report_tech('Capacity share'%res_share%,dsm_shift) ;
report_to_excel(loop_res_share,'Capacity shares',dsm_curt) =  report_tech('Capacity share'%res_share%,dsm_curt) ;
report_to_excel(loop_res_share,'Capacity shares',ct) =  report_tech('Capacity share'%res_share%,ct) ;

report_to_excel(loop_res_share,'Energy share',ct) =  report_tech('Energy share'%res_share%,ct) ;
report_to_excel(loop_res_share,'Energy share',res) =  report_tech('Energy share'%res_share%,res) ;
report_to_excel(loop_res_share,'Energy share',sto) =  report_tech('Energy share'%res_share%,sto) ;
report_to_excel(loop_res_share,'Energy share',dsm_shift) =  report_tech('Energy share'%res_share%,dsm_shift) ;
report_to_excel(loop_res_share,'Energy share',dsm_curt) =  report_tech('Energy share'%res_share%,dsm_curt) ;
report_to_excel(loop_res_share,'Energy share',ct) =  report_tech('Energy share'%res_share%,ct) ;

report_to_excel(loop_res_share,'Capacity absolute conventional',ct) = report_tech('capacities conventional'%res_share%%em_share%,ct) ;
report_to_excel(loop_res_share,'Capacity absolute RES ',res) =  report_tech('capacities renewable'%res_share%%em_share%,res) ;
report_to_excel(loop_res_share,'Capacity absolute RES ','bio') = report_tech('capacities renewable'%res_share%%em_share%,'bio') ;
report_to_excel(loop_res_share,'Storage / DSM capacity',sto) =  report_tech('capacities storage MW'%res_share%%em_share%,sto) ;
report_to_excel(loop_res_share,'Storage energy',sto) = report_tech('capacities storage MWh'%res_share%%em_share%,sto) ;

%DSM%$ontext
report_to_excel(loop_res_share,'Storage / DSM capacity',dsm_shift) =  report_tech('capacities load shift'%res_share%%em_share%,dsm_shift) ;
report_to_excel(loop_res_share,'Storage / DSM capacity',dsm_curt) =  report_tech('capacities load curtailment'%res_share%%em_share%,dsm_curt) ;
$ontext
$offtext

execute_unload "results_to_excel", report_to_excel ;

execute 'gdxxrw.exe results_to_excel.gdx par=report_to_excel' ;
