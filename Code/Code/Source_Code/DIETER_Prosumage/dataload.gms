
********************************************************************************
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.2.0, February 2017.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
********************************************************************************


*********************************************
**** Defines and uploads parameters      ****
*********************************************


Parameters

***** Conventionals *****

*--- Generation and fixed ---*
eta_con(ct)              Efficiency of conventional technologies
carbon_content(ct)       CO2 emissions per fuel unit used
c_up(ct)                 Load change costs UP in EUR per MW
c_do(ct)                 Load change costs DOWN in EUR per MW
c_fix_con(ct)            Annual fixed costs per MW
c_var_con(ct)            Variable O&M costs per MWh

*--- Investment ---*
c_inv_overnight_con(ct)  Investment costs: Overnight
inv_lifetime_con(ct)     Investment costs: technical lifetime
inv_recovery_con(ct)     Investment costs: Recovery period according to depreciation tables
inv_interest_con(ct)     Investment costs: Interest rate
m_con(ct)                Investment: maximum installable capacity per technology
m_con_e(ct)              Investment: maximum installable energy in TWh per a

*--- Flexibility ---*
grad_per_min(ct)         Maximum load change per minute relative to installed capacity


***** Fuel and CO2 costs *****

con_fuelprice(ct)        Fuel price conventionals in Euro per MWth
con_CO2price             CO2 price


***** Renewables *****

*--- Generation and fixed costs ---*
c_cu(res)                Hourly Curtailment costs for renewables per MW
c_fix_res(res)           Annual fixed costs per MW
phi_min_res              Upload parameter: Minimum required renewables generation

*--- Investment ---*
c_inv_overnight_res(res) Investment costs: Overnight
inv_lifetime_res(res)    Investment costs: technical lifetime
inv_recovery_res(res)    Investment costs: Recovery period
inv_interest_res(res)    Investment costs: Interest rate
m_res(res)               Investment: maximum installable capacity
m_res_e(res)             Investment: maximum installable energy in TWh per a


***** Time Data *****
d_y(year,h)              Demand hour h for cost minimization for different years
d(h)                     Demand hour h for cost minimization
phi_res_y(year,res,h)    Renewables availability technology res in hour h for different years
phi_res(res,h)           Renewables availability technology res in hour h
phi_ror(h)               Run-of-river availability in hour h
n_ev_p(ev,h)             Power rating of the charging connection in MW in hour h (0 when car is in use or parked without grid connection)
ev_ed(ev,h)              Electricity demand for mobility vehicle profile ev in hour h in MW
ev_ged_exog(ev,h)        Electricity demand for mobility in case of uncontrolled charging vehicle profile ev in hour h in MW


***** Storage *****

*--- Generation and fixed costs ---*
c_m_sto(sto)             Marginal costs of storing in or out
eta_sto(sto)             Storage efficiency
eta_sto_in(sto)          Storage loading efficiency
eta_sto_out(sto)         Storage discharging efficiency
phi_sto_ini(sto)         Initial storage level
etop_max(sto)            Maximum E to P ratio of storage types
c_fix_sto(sto)           Annual fixed costs per MW

*--- Investment ---*
c_inv_overnight_sto_e(sto)       Investment costs for storage energy in MWh: Overnight
c_inv_overnight_sto_p(sto)       Investment costs for storage capacity in MW: Overnight
inv_lifetime_sto(sto)            Investment costs for storage: technical lifetime
inv_recovery_sto(sto)            Investment costs for storage: Recovery period
inv_interest_sto(sto)            Investment costs for storage: Interest rate
m_sto_e(sto)                     Investment into storage: maximum installable energy in MWh
m_sto_p(sto)                     Investment into storage: maximum installable capacity in MW


***** Electric vehicles *****
c_m_ev(ev)               Marginal costs of discharging V2G
pen_phevfuel             Penalty for non-electric PHEV operation mode
eta_ev_in(ev)            Electric vehicle efficiency of charging (G2V)
eta_ev_out(ev)           Electric vehicle efficiency of discharging (V2G)
phi_ev_ini(ev)           Electric vehicle charging level in initial period

n_ev_e(ev)               Electric vehicle battery capacity in MWh
ev_quant                 Overall number of electirc vehicles
phi_ev(ev)               Share of electric vehicles per load profile in actual scenario
ev_phev(ev)              Defines whether an electric vehicle is a PHEV REEV (1 if yes 0 otherwise)


***** DSM *****

*--- Generation and fixed costs ---*
c_m_dsm_cu(dsm_curt)             DSM: hourly costs of load curtailment
c_m_dsm_shift(dsm_shift)         DSM: costs for load shifting
c_fix_dsm_cu(dsm_curt)           Annual fixed costs per MW load curtailment capacity
c_fix_dsm_shift(dsm_shift)       Annual fixed costs per MW load shifting capacity

*--- Flexibility, efficiency, recovery ---*
t_dur_dsm_cu(dsm_curt)           DSM: Maximum duration load curtailment
t_off_dsm_cu(dsm_curt)           DSM: Minimum recovery time between two load curtailment instances

t_dur_dsm_shift(dsm_shift)       DSM: Maximum duration load shifting
t_off_dsm_shift(dsm_shift)       DSM: Minimum recovery time between two granular load upshift instances
eta_dsm_shift(dsm_shift)         DSM: Efficiency of load shifting technologies

*--- Investment ---*
c_inv_overnight_dsm_cu(dsm_curt)         Investment costs for DSM load curtailment: Overnight
c_inv_overnight_dsm_shift(dsm_shift)     Investment costs for DSM load shifting: Overnight
inv_recovery_dsm_cu(dsm_curt)            Investment costs for DSM load curtailment: Recovery period
inv_recovery_dsm_shift(dsm_shift)        Investment costs for DSM load shifting: Recovery period
inv_interest_dsm_cu(dsm_curt)            Investment costs for DSM load curtailment: Interest rate
inv_interest_dsm_shift(dsm_shift)        Investment costs for DSM load shifting: Interest rate
m_dsm_cu(dsm_curt)                       DSM: Maximum installable capacity load curtailment
m_dsm_shift(dsm_shift)                   DSM: Maximum installable capacity load shifting


***** Reserves *****
phi_reserves_share(reserves)             Shares of SRL and MRL up and down
reserves_intercept(reserves)
reserves_slope(reserves,res)
reserves_reaction(reserves)              Activation reaction time for reserves qualities in minutes
phi_reserves_call_y(year,reserves,h)     Hourly share of reserve provision that is actually activated
phi_reserves_call(reserves,h)            Hourly share of reserve provision that is actually activated
phi_reserves_pr                          Primary reserves demand as fraction of sum of all secondary and tertiary reserves demand


***** Prosumage *****
phi_pro_load                             Share of prosumagers among total load
phi_pro_self                             Minimum self-generation shares for prosumagers
m_res_pro(res)                           Maximum installable: renewables capacity
m_sto_pro_e(sto)                         Maximum installable: storage energy
m_sto_pro_p(sto)                         Maximum installable: storage capacity
phi_sto_pro_ini                          Prosumagers' initial storage loading
;

********************************************************************************


$onecho >temp.tmp
par=eta_con              rng=Conventionals!c5:d12        rdim=1 cdim=0
par=carbon_content       rng=Conventionals!c15:d22       rdim=1 cdim=0
par=c_up                 rng=Conventionals!c105:d112     rdim=1 cdim=0
par=c_do                 rng=Conventionals!c115:d122     rdim=1 cdim=0
par=c_fix_con            rng=Conventionals!c25:d32       rdim=1 cdim=0
par=c_var_con            rng=Conventionals!c35:d42       rdim=1 cdim=0
par=c_inv_overnight_con  rng=Conventionals!c45:d52       rdim=1 cdim=0
par=inv_lifetime_con     rng=Conventionals!c55:d62       rdim=1 cdim=0
par=inv_recovery_con     rng=Conventionals!c65:d72       rdim=1 cdim=0
par=inv_interest_con     rng=Conventionals!c75:d82       rdim=1 cdim=0
par=m_con                rng=Conventionals!c85:d92       rdim=1 cdim=0
par=m_con_e              rng=Conventionals!c95:d102      rdim=1 cdim=0
par=grad_per_min         rng=Conventionals!c125:d132     rdim=1 cdim=0

par=c_cu                 rng=Renewables!c4:d6            rdim=1 cdim=0
par=c_fix_res            rng=Renewables!c9:d11           rdim=1 cdim=0
par=c_inv_overnight_res  rng=Renewables!c15:d17          rdim=1 cdim=0
par=inv_lifetime_res     rng=Renewables!c20:d22          rdim=1 cdim=0
par=inv_recovery_res     rng=Renewables!c25:d27          rdim=1 cdim=0
par=inv_interest_res     rng=Renewables!c30:d32          rdim=1 cdim=0
par=m_res                rng=Renewables!c35:d37          rdim=1 cdim=0
par=m_res_e              rng=Renewables!c40:d42          rdim=1 cdim=0

par=con_fuelprice        rng=Fuel_CO2!a4:b12             rdim=1 cdim=0
par=con_CO2price         rng=Fuel_CO2!b18:b18            rdim=0 cdim=0

par=d_y                  rng=Time_Data!c48:lya52         rdim=1 cdim=1
par=phi_res_y            rng=Time_Data!b55:lya67         rdim=2 cdim=1
par=phi_ror              rng=Time_Data!d30:lya31         rdim=0 cdim=1

par=n_ev_p               rng=Time_Data!c75:lya103        rdim=1 cdim=1
par=ev_ed                rng=Time_Data!c109:lya137       rdim=1 cdim=1
par=ev_ged_exog          rng=Time_Data!c144:lya172       rdim=1 cdim=1

par=c_m_dsm_shift                rng=DSM!c10:d14         rdim=1 cdim=0
par=c_fix_dsm_shift              rng=DSM!c20:d24         rdim=1 cdim=0
par=c_inv_overnight_dsm_shift    rng=DSM!c33:d37         rdim=1 cdim=0
par=inv_recovery_dsm_shift       rng=DSM!c43:d47         rdim=1 cdim=0
par=inv_interest_dsm_shift       rng=DSM!c52:d56         rdim=1 cdim=0
par=m_dsm_shift                  rng=DSM!c64:d68         rdim=1 cdim=0
par=t_dur_dsm_shift              rng=DSM!c74:d78         rdim=1 cdim=0
par=t_off_dsm_shift              rng=DSM!c83:d87         rdim=1 cdim=0
par=eta_dsm_shift                rng=DSM!c92:d96         rdim=1 cdim=0

par=c_m_dsm_cu                   rng=DSM!c5:d7           rdim=1 cdim=0
par=c_fix_dsm_cu                 rng=DSM!c17:d19         rdim=1 cdim=0
par=c_inv_overnight_dsm_cu       rng=DSM!c28:d30         rdim=1 cdim=0
par=inv_recovery_dsm_cu          rng=DSM!c40:d42         rdim=1 cdim=0
par=inv_interest_dsm_cu          rng=DSM!c49:d51         rdim=1 cdim=0
par=m_dsm_cu                     rng=DSM!c59:d61         rdim=1 cdim=0
par=t_dur_dsm_cu                 rng=DSM!c71:d73         rdim=1 cdim=0
par=t_off_dsm_cu                 rng=DSM!c80:d82         rdim=1 cdim=0

par=c_m_sto                      rng=Storage!c4:d10      rdim=1 cdim=0
par=eta_sto                      rng=Storage!c13:d19     rdim=1 cdim=0
par=c_fix_sto                    rng=Storage!c22:d28     rdim=1 cdim=0
par=c_inv_overnight_sto_e        rng=Storage!c31:d37     rdim=1 cdim=0
par=c_inv_overnight_sto_p        rng=Storage!c40:d46     rdim=1 cdim=0
par=inv_lifetime_sto             rng=Storage!c48:d54     rdim=1 cdim=0
par=inv_recovery_sto             rng=Storage!c32:d34     rdim=1 cdim=0
par=inv_interest_sto             rng=Storage!c60:d66     rdim=1 cdim=0
par=m_sto_e                      rng=Storage!c69:d75     rdim=1 cdim=0
par=m_sto_p                      rng=Storage!c77:d83     rdim=1 cdim=0
par=phi_sto_ini                  rng=Storage!c85:d91     rdim=1 cdim=0
par=etop_max                     rng=Storage!c93:d99     rdim=1 cdim=0

par=c_m_ev               rng=EV!c4:d31           rdim=1 cdim=0
par=pen_phevfuel         rng=EV!d34:d34          rdim=0 cdim=0
par=eta_ev_in            rng=EV!c37:d64          rdim=1 cdim=0
par=eta_ev_out           rng=EV!c67:d94          rdim=1 cdim=0
par=phi_ev_ini           rng=EV!c97:d124         rdim=1 cdim=0
par=n_ev_e               rng=EV!c127:d154        rdim=1 cdim=0
par=phi_ev               rng=EV!c157:d184        rdim=1 cdim=0
par=ev_phev              rng=EV!c187:d214        rdim=1 cdim=0

par=phi_reserves_share           rng=Reserves!e13:f16    rdim=1 cdim=0
par=reserves_intercept           rng=Reserves!e2:f5      rdim=1 cdim=0
par=reserves_slope               rng=Reserves!d6:g10     rdim=1 cdim=1
par=reserves_reaction            rng=Reserves!d79:e85    rdim=1 cdim=0
par=phi_reserves_call_y          rng=Reserves!b49:lya73  rdim=2 cdim=1
par=phi_reserves_pr              rng=Reserves!d87:d87    rdim=0 cdim=0

par=m_res_pro          rng=Prosumage!c5:d7     rdim=1 cdim=0
par=m_sto_pro_e        rng=Prosumage!c10:d16   rdim=1 cdim=0
par=m_sto_pro_p        rng=Prosumage!c19:d25   rdim=1 cdim=0
par=phi_sto_pro_ini    rng=Prosumage!c28:d34   rdim=1 cdim=0
$offecho

%skip_Excel%$call "gdxxrw Data_Input_2035.xlsx @temp.tmp o=Data_input";

$GDXin Data_input.gdx
$load d_y phi_ror phi_res_y n_ev_p ev_ed ev_ged_exog
$load eta_con carbon_content c_up c_do c_fix_con c_var_con c_inv_overnight_con inv_lifetime_con inv_recovery_con inv_interest_con m_con m_con_e grad_per_min
$load con_fuelprice con_CO2price
$load c_cu c_fix_res c_inv_overnight_res inv_lifetime_res inv_recovery_res inv_interest_res m_res m_res_e
$load c_m_sto eta_sto c_fix_sto c_inv_overnight_sto_e c_inv_overnight_sto_p inv_lifetime_sto inv_interest_sto m_sto_e m_sto_p phi_sto_ini etop_max
$load c_m_dsm_shift c_fix_dsm_shift c_inv_overnight_dsm_shift inv_recovery_dsm_shift inv_interest_dsm_shift m_dsm_shift t_dur_dsm_shift eta_dsm_shift t_off_dsm_shift
$load c_m_dsm_cu c_fix_dsm_cu c_inv_overnight_dsm_cu inv_recovery_dsm_cu inv_interest_dsm_cu m_dsm_cu t_dur_dsm_cu t_off_dsm_cu
$load c_m_ev eta_ev_in eta_ev_out pen_phevfuel phi_ev_ini n_ev_e phi_ev ev_phev
$load phi_reserves_share reserves_intercept reserves_slope reserves_reaction phi_reserves_call_y phi_reserves_pr
$load m_res_pro m_sto_pro_e m_sto_pro_p phi_sto_pro_ini
;


********************************************************************************


Parameters
c_m(ct)        Marginal production costs for conventional plants including variable O and M costs
c_i(ct)        Annualized investment costs by conventioanl plant per MW

c_i_res(res)     Annualized investment costs by renewable plant per MW

c_i_sto_e(sto)   Annualized investment costs storage energy per MWh
c_i_sto_p(sto)   Annualized investment costs storage capacity per MW

c_i_dsm_cu(dsm_curt)     DSM: Investment costs load curtailment
c_i_dsm_shift(dsm_shift) DSM: Investment costs load shifting
;

c_m(ct) = con_fuelprice(ct)/eta_con(ct) + carbon_content(ct)/eta_con(ct)*con_CO2price + c_var_con(ct)   ;
c_i(ct) = c_inv_overnight_con(ct)*( inv_interest_con(ct) * (1+inv_interest_con(ct))**(inv_lifetime_con(ct)) )
                 / ( (1+inv_interest_con(ct))**(inv_lifetime_con(ct))-1 )       ;

c_i_res(res) = c_inv_overnight_res(res)*( inv_interest_res(res) * (1+inv_interest_res(res))**(inv_lifetime_res(res)) )
                 / ( (1+inv_interest_res(res))**(inv_lifetime_res(res))-1 )       ;

c_i_sto_e(sto) = c_inv_overnight_sto_e(sto)*( inv_interest_sto(sto) * (1+inv_interest_sto(sto))**(inv_lifetime_sto(sto)) )
                 / ( (1+inv_interest_sto(sto))**(inv_lifetime_sto(sto))-1 )       ;
c_i_sto_p(sto) = c_inv_overnight_sto_p(sto)*( inv_interest_sto(sto) * (1+inv_interest_sto(sto))**(inv_lifetime_sto(sto)) )
                 / ( (1+inv_interest_sto(sto))**(inv_lifetime_sto(sto))-1 )       ;

c_i_dsm_cu(dsm_curt) = c_inv_overnight_dsm_cu(dsm_curt)*( inv_interest_dsm_cu(dsm_curt) * (1+inv_interest_dsm_cu(dsm_curt))**(inv_recovery_dsm_cu(dsm_curt)) )
                 / ( (1+inv_interest_dsm_cu(dsm_curt))**(inv_recovery_dsm_cu(dsm_curt))-1 )       ;
c_i_dsm_shift(dsm_shift) = c_inv_overnight_dsm_shift(dsm_shift)*( inv_interest_dsm_shift(dsm_shift) * (1+inv_interest_dsm_shift(dsm_shift))**(inv_recovery_dsm_shift(dsm_shift)) )
                 / ( (1+inv_interest_dsm_shift(dsm_shift))**(inv_recovery_dsm_shift(dsm_shift))-1 )       ;



* Adjust investment costs on model's hourly basis

c_i(ct) = c_i(ct)*card(h)/8760 ;
c_i_res(res) = c_i_res(res)*card(h)/8760 ;
%second_hour%c_i_sto_e(sto) = c_i_sto_e(sto)*card(h)/8760 ;
c_i_sto_p(sto) = c_i_sto_p(sto)*card(h)/8760 ;
c_i_dsm_cu(dsm_curt) = c_i_dsm_cu(dsm_curt)*card(h)/8760 ;
c_i_dsm_shift(dsm_shift) = c_i_dsm_shift(dsm_shift)*card(h)/8760 ;
%second_hour%$ontext
c_i_sto_e(sto) = c_i_sto_e(sto)*card(h)/8760 * 2 ;
t_dur_dsm_cu(dsm_curt) = t_dur_dsm_cu(dsm_curt) / 2 ;
t_off_dsm_cu(dsm_curt) = t_off_dsm_cu(dsm_curt) / 2 ;
t_dur_dsm_shift(dsm_shift)$(ord(dsm_shift)=2 or ord(dsm_shift)=4 or ord(dsm_shift)=5) = t_dur_dsm_shift(dsm_shift) / 2 ;
t_dur_dsm_shift(dsm_shift)$(ord(dsm_shift)=1 or ord(dsm_shift)=3) = 2 ;
$ontext
$offtext

c_fix_con(ct) = c_fix_con(ct)*card(h)/8760 ;
c_fix_res(res) = c_fix_res(res)*card(h)/8760 ;
c_fix_sto(sto) = c_fix_sto(sto)*card(h)/8760 ;
c_fix_dsm_cu(dsm_curt) = c_fix_dsm_cu(dsm_curt)*card(h)/8760 ;
c_fix_dsm_shift(dsm_shift) = c_fix_dsm_shift(dsm_shift)*card(h)/8760 ;

m_con_e('bio') = m_con_e('bio')*card(h)/8760 ;

eta_sto_in(sto) = 0.5*eta_sto(sto);
eta_sto_out(sto) = 0.5*eta_sto(sto);

parameter phi_mean_reserves_call, phi_mean_reserves_call_y ;
phi_mean_reserves_call_y(year,reserves) = sum(h, phi_reserves_call_y(year,reserves,h) ) / card(h) + eps ;



