
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




***************  SETS  *********************************************************

***** Sets used in the model *****
Sets
tech                     Generation technologies
 dis(tech)               Dispatchable generation technologies
 nondis(tech)            Nondispatchable generation technologies
 con(tech)               Conventional generation technologies
 res(tech)               Renewable generation technologies
sto                      Storage technologies
rsvr                     Reservoir technologies
dsm                      DSM technologies
 dsm_shift(dsm)          DSM load shifting technologies
 dsm_curt(dsm)           DSM load curtailment technologies
year                     Base year for temporal data
h                        Hours
n                        Nodes
l                        Lines
ev                       EV types
reserves                         Reserve qualities
 reserves_up(reserves)           Positive reserves
 reserves_do(reserves)           Negative reserves
 reserves_spin(reserves)         Spinning reserves
 reserves_nonspin(reserves)      Nonspinning reserves
 reserves_prim(reserves)         Primary reserves
 reserves_nonprim(reserves)      Nonprimary reserves
 reserves_prim_up(reserves)      Primary positive reserves
 reserves_nonprim_up(reserves)   Nonprimary positive reserves
 reserves_prim_do(reserves)      Primary negative reserves
 reserves_nonprim_do(reserves)   Nonprimary negative reserves
bu                       Building archtypes
ch                       Heating combination type
 hst(ch)                 Heating technology that feeds to storage
 hp(ch)                  Heat pump technologies
 hel(ch)                 Hybrid electric heating technologies - electric part
 hfo(ch)                 Hybrid electric heating technologies - fossil part


***** Sets used for data upload *****
headers_tech                     Generation technologies - upload headers
 tech_dispatch                   Generation technologies - dispatchable or nondispatchable
 tech_res_con                    Generation technologies - renewable or "conventional"
headers_sto                      Storage technologies - upload headers
headers_reservoir                Reservoir technologies - upload headers
headers_dsm                      DSM technologies - upload headers
 dsm_type                        DSM technologies - shifting or curtailment
headers_time                     Temporal data - upload headers
headers_topology                 Spatial data - upload headers
headers_ev                       EV data - upload headers
headers_time_ev                  EV temporal data - upload headers
headers_prosumage_generation     Prosumage generation data - upload headers
headers_prosumage_storage        Prosumage storage data - upload headers
headers_reserves                 Reserve data - upload headers
 reserves_up_down                Reserve data - positive and neagtive reserves
 reserves_spin_nonspin           Reserve data - spinning and nonspinning reserves
 reserves_prim_nonprim           Reserve data - primary and nonprimary reserves
headers_heat                     Heat data - upload headers
 heat_storage                    Heat data - storage technologies
 heat_hp                         Heat data - heat pump technologies
 heat_elec                       Heat data - hybrid heating technologies - electric part
 heat_fossil                     Heat data - hybrid heating technologies - fossil part
headers_time_heat                Heat data - upload headers time data
headers_time_dhw                 Heat data - upload headers time data on DHW
;




***************  PARAMETERS  ***************************************************

Parameters

***** Generation technologies *****
*--- Variable and fixed costs ---*
eta              Efficiency of conventional technologies [0 1]
carbon_content   CO2 emissions per fuel unit used [tons per MWh thermal]
c_up             Load change costs UP [EUR per MWh]
c_do             Load change costs DOWN [EUR per MWh]
c_fix            Annual fixed costs [EUR per MW per year]
c_vom            Variable O&M costs [EUR per MWh]
CO2price         CO2 price in [EUR per ton]

*--- Investment ---*
c_inv_overnight  Investment costs: Overnight [EUR per MW]
lifetime         Investment costs: technical lifetime [a]
recovery         Investment costs: Recovery period according to depreciation tables [a]
interest_rate    Investment costs: Interest rate [%]
m_p              Investment: maximum installable capacity per technology [MW]
m_e              Investment: maximum installable energy [TWh per a]

*--- Flexibility ---*
grad_per_min     Maximum load change relative to installed capacity [% of installed capacity per minute]


***** Fuel and CO2 costs *****
fuelprice        Fuel price conventionals [EUR per MWh thermal]


***** Renewables *****
c_cu             Hourly Curtailment costs for renewables [Euro per MW]
phi_min_res      Minimum renewables share [0 1]


***** Storage *****
*--- Variable and fixed costs ---*
c_m_sto          Marginal costs of storing in or out [EUR per MWh]
eta_sto          Storage efficiency [0 1]
phi_sto_ini      Initial storage level [0 1]
etop_max         Maximum E to P ratio of storage types [#]
c_fix_sto        Annual fixed costs [EUR per MW]

*--- Investment ---*
c_inv_overnight_sto_e       Investment costs for storage energy: Overnight [EUR per MWh]
c_inv_overnight_sto_p       Investment costs for storage capacity: Overnight [EUR per MW]
m_sto_e                     Investment into storage: maximum installable energy [MWh]
m_sto_p                     Investment into storage: maximum installable power [MW]


***** Reservoir*****
*--- Variable and fixed costs ---*
c_m_rsvr                 Marginal costs of generating energy from reservoir [EUR per MWh]
eta_rsvr                 Generation efficiency [0 1]
phi_rsvr_ini             Initial reservoir level [0 1]
c_fix_rsvr               Annual fixed costs [EUR per MW per a]
phi_rsvr_min             Minimum hourly reservoir outflow as fraction of annual energy [0 1]
phi_rsvr_lev_min         Minimum filling level [0 1]

*--- Investment ---*
c_inv_overnight_rsvr_e   Investment costs for reservoir energy: Overnight [EUR per MWh]
c_inv_overnight_rsvr_p   Investment costs for reservoir capacity: Overnight [EUR per MW]
inv_lifetime_rsvr        Investment costs for reservoir: technical lifetime [a]
inv_interest_rsvr        Investment costs for reservoir: Interest rate [%]
m_rsvr_e                 Investment into reservoir: maximum installable energy [MWh]
m_rsvr_p                 Investment into reservoir: maximum installable capacity [MW]


***** DSM *****
*--- Variable and fixed costs ---*
c_m_dsm_cu       DSM: hourly costs of load curtailment [EUR per MWh]
c_m_dsm_shift    DSM: costs for load shifting [EUR per MWh]
c_fix_dsm_cu     Annual fixed costs load curtailment capacity [EUR per MW per a]
c_fix_dsm_shift  Annual fixed costs load shifting capacity [EUR per MW per a]

*--- Flexibility, efficiency, recovery ---*
t_dur_dsm_cu     DSM: Maximum duration load curtailment [h]
t_off_dsm_cu     DSM: Minimum recovery time between two load curtailment instances [h]

t_dur_dsm_shift  DSM: Maximum duration load shifting [h]
t_off_dsm_shift  DSM: Minimum recovery time between two granular load upshift instances [h]
eta_dsm_shift    DSM: Efficiency of load shifting technologies [0 1]

*--- Investment ---*
c_inv_overnight_dsm_cu           Investment costs for DSM load curtailment: Overnight [EUR per MW]
c_inv_overnight_dsm_shift        Investment costs for DSM load shifting: Overnight [EUR per MW]
inv_recovery_dsm_cu              Investment costs for DSM load curtailment: Recovery period [a]
inv_recovery_dsm_shift           Investment costs for DSM load shifting: Recovery period [a]
inv_interest_dsm_cu              Investment costs for DSM load curtailment: Interest rate [%]
inv_interest_dsm_shift           Investment costs for DSM load shifting: Interest rate [%]
m_dsm_cu                         DSM: Maximum installable capacity load curtailment [MW]
m_dsm_shift                      DSM: Maximum installable capacity load shifting [MW]


***** Time Data *****
d_y                      Demand hour h for different years [MWh]
d                        Demand hour h [MWh]
phi_res_y                Renewables availability technology res in hour h for different years [0 1]
phi_res                  Renewables availability technology res in hour h [0 1]
phi_ror_y                Run-of-river availability technology ror in hour h for different years [0 1]
phi_ror                  Run-of-river availability technology ror in hour h [0 1]
rsvr_in_y                Reservoir inflow in hour h for different years [0 1]
rsvr_in                  Reservoir inflow in hour h [0 1]
n_ev_p_upload            Power rating of the charging connection in hour h [MW - 0 when car is in use or parked without grid connection]
ev_ed_upload             Electricity demand for mobility vehicle profile ev in hour h [MW]
ev_ged_exog_upload       Electricity demand for mobility in case of uncontrolled charging vehicle profile ev in hour h [MW]
phi_reserves_call_y      Hourly share of reserve provision that is actually activated for different years [0 1]
phi_reserves_call        Hourly share of reserve provision that is actually activated [0 1]
reserves_exogenous_y     Hourly reserve provision for different years [MW]
reserves_exogenous       Hourly reserve provision [MW]

***** Transmission *****
*--- Investment ---*
c_inv_overnight_ntc       Investment costs in: overnight [EUR per MW]
c_fix_ntc                 Fixed costs [EUR per MW per a]
inv_lifetime_ntc          Investment costs: technical lifetime [a]
inv_recovery_ntc          Investment costs: Recovery period in [a]
inv_interest_ntc          Investment costs: Interest rate [%]
m_ntc                     Investment into NTC: maximum installable capacity [MW]

*--- Topology and distance ---*
inc              Incidence index of link l on node n
dist             Distance covered by link l [km]


***** Electric vehicles *****
*--- Costs and attributes ---*
c_m_ev           Marginal costs of discharging V2G [EUR per MWh]
pen_phevfuel     Penalty for non-electric PHEV operation mode [EUR per MWh]
eta_ev_in        Electric vehicle efficiency of charging (G2V) [0 1]
eta_ev_out       Electric vehicle efficiency of discharging (V2G) [0 1]
phi_ev_ini       Electric vehicle charging level in initial period [0 1]

n_ev_e           Electric vehicle battery capacity [MWh]
ev_quant         Overall number of electirc vehicles [#]
phi_ev           Share of electric vehicles per load profile in actual scenario [0 1]
ev_phev          Defines whether an electric vehicle is a PHEV REEV [1 if yes 0 otherwise]

*--- Temporal data ---*
n_ev_p           Electric vehicle power rating [MWh]
ev_ed            Electric vehicle electricity demand [MWh]
ev_ged_exog      Electric vehicle grid electricity demand for exogenous charging pattern [MWh]


***** Prosumage *****
phi_pro_load             Share of prosumagers among total load [0 1]
phi_pro_self             Minimum self-generation shares for prosumagers [0 1]
m_res_pro                Maximum installable: renewables capacity [MW]
m_sto_pro_e              Maximum installable: storage energy [MWh]
m_sto_pro_p              Maximum installable: storage capacity [MW]
phi_sto_pro_ini          Prosumagers' initial storage loading [0 1]


***** Reserves *****
phi_reserves_share       Shares of SRL and MRL up and down [0 1]
reserves_intercept       Intercept of regression line determining reserves demand
reserves_slope           Slope of regression line determining reserves demand
reserves_reaction        Activation reaction time for reserves qualities [min]
phi_reserves_pr_up       Positive primary reserves fraction of total nonprimary reserves demand [0 1]
phi_reserves_pr_do       Negative primary reserves fraction of total nonprimary reserves demand [0 1]
*reserves_exog


***** Heat *****
*--- Time data ---*
dh_upload                Hourly heat demand per year for upload [MWh per m2]
dh_y                     Hourly heat demand per year [MWh per m2]
dh                       Hourly heat demand [MWh per m2]
temp_source              Heat pumps - source temperature [�Celsius]
d_dhw_upload             Hourly DHW demand per year for upload [MWh per m2]
d_dhw_y                  Hourly DHW demand per year [MWh per m2]
d_dhw                    Hourly DHW demand [MWh per m2]
nets_profile(h)          Hourly exogenous heat demand by nonsmart night-time storage heaters [MWh per m2]

*--- Technololgy attributes ---*
phi_heat_type            Share of heating type ch per building archetype bu [0 1]
eta_heat_stat            Static efficiency for heating technologies [0 1]
eta_heat_dyn             Static efficiency for heating technologies [0 1]
eta_dhw_aux_stat         Static efficiency for auxiliary DHW technologies [0 1]
n_heat_p_in              Maximum power inflow into heating technologies [MW]
n_heat_p_out             Maximum power outflow from heating technologies [MW]
n_heat_e                 Maximum energy level of heating storage technologies [MWh]
n_sets_p_in              SETS - Power rating - electricity intake [MW]
n_sets_p_out             SETS - Power rating - heat output [MW]
n_sets_e                 SETS - Energy storage capacity [MWh]
n_sets_dhw_p_in          SETS auxiliary DHW module - power rating - electricity intake [MW]
n_sets_dhw_p_out         SETS auxiliary DHW module - power rating - DHW output [MW]
n_sets_dhw_e             SETS auxiliary DHW module - energy storage capacity [MWh]
phi_heat_ini             Inititial storage level of heating technologies [0 1]
temp_sink                Heat pumps - sink temperature [�Celsius]
pen_heat_fuel            Penalty term for non-electric fuel usage for hybrid heating technologies [EUR per MWh]
area_floor               Floor area subject to specific heating technology in specific building type [m2]
theta_night              Indicator for night hours {0 1}



***************  DERIVED PARAMETERS  *******************************************

c_m              Marginal production costs for conventional plants including variable O and M costs [EUR per MWh]
c_i              Annualized investment costs by conventioanl plant [EUR per MW]

c_i_res          Annualized investment costs by renewable plant [EUR per MW]
c_fix_res        Annualized fixed costs by renewable plant [EUR per MW per a]

c_i_sto_e        Annualized investment costs storage energy [EUR per MWh]
c_i_sto_p        Annualized investment costs storage capacity [EUR per MW]

c_i_rsvr_e       Annualized investment costs storage energy [EUR per MWh]
c_i_rsvr_p       Annualized investment costs storage capacity [EUR per MW]

c_i_dsm_cu       DSM: Annualized investment costs load curtailment [EUR per MW]
c_i_dsm_shift    DSM: Annualized investment costs load shifting [EUR per MW]

c_i_ntc          Investment for net transfer capacity [EUR per MW and km]

phi_mean_reserves_call_y         Hourly mean of share reserves called per year [0 1]
phi_mean_reserves_call           Hourly mean of share reserves called [0 1]

theta_dir        Dummy equal to 1 if building type bu has direct heating type ch [0 1]
theta_sets       Dummy equal to 1 if building type bu has SETS heating type ch [0 1]
theta_hp         Dummy equal to 1 if building type bu has heat pump heating type ch [0 1]
theta_elec       Dummy equal to 1 if building type bu has hybrif electric heating - electric part [0 1]
theta_fossil     Dummy equal to 1 if building type bu has hybrif electric heating - fossil part [0 1]
theta_storage    Dummy equal to 1 if building type ch has storage heating type ch [0 1]




***************  PARAMETERS FOR DATA UPLOAD  ***********************************

technology_data_upload(n,tech,tech_res_con,tech_dispatch,headers_tech)
technology_data(n,tech,headers_tech)
storage_data(n,sto,headers_sto)
reservoir_data(n,rsvr,headers_reservoir)
time_data_upload(h,n,year,headers_time)
dsm_data_upload(n,dsm,dsm_type,headers_dsm)
dsm_data(n,dsm,headers_dsm)
topology_data(l,headers_topology)
ev_data(n,ev,headers_ev)
ev_time_data_upload(h,headers_time_ev,ev)
prosumage_data_generation(n,tech,headers_prosumage_generation)
prosumage_data_storage(n,sto,headers_prosumage_storage)
reserves_time_data_activation(h,year,reserves)
reserves_time_data_provision(h,year,reserves)
reserves_data_upload(n,reserves,reserves_up_down,reserves_spin_nonspin,reserves_prim_nonprim,headers_reserves)
reserves_data(n,reserves,headers_reserves)
heat_data_upload(n,bu,ch,heat_storage,heat_hp,heat_elec,heat_fossil,headers_heat)
heat_data(n,bu,ch,headers_heat)
dh_upload(h,n,year,headers_time_heat,bu)
d_dhw_upload(h,n,year,headers_time_heat,bu)
temp_source_upload
;




***************  UPLOAD TIME-CONSTANT SETS AND PARAMETERS  *********************

$onecho >temp.tmp
se=0

dset=n                                   rng=spatial!M2                  rdim=0 cdim=1

dset=tech                                rng=Technologies!B6             rdim=1 cdim=0
dset=headers_tech                        rng=Technologies!E5             rdim=0 cdim=1
dset=tech_dispatch                       rng=Technologies!D6             rdim=1 cdim=0
dset=tech_res_con                        rng=Technologies!C6             rdim=1 cdim=0

dset=sto                                 rng=storage!B6                  rdim=1 cdim=0
dset=headers_sto                         rng=storage!C5                  rdim=0 cdim=1

dset=rsvr                                rng=reservoir!B6                rdim=1 cdim=0
dset=headers_reservoir                   rng=reservoir!C5                rdim=0 cdim=1

dset=dsm                                 rng=DSM!B6                      rdim=1 cdim=0
dset=headers_dsm                         rng=DSM!D5                      rdim=0 cdim=1
dset=dsm_type                            rng=DSM!C6                      rdim=1 cdim=0

dset=l                                   rng=spatial!B3                  rdim=1 cdim=0
dset=headers_topology                    rng=spatial!B2                  rdim=0 cdim=1

dset=ev                                  rng=ev!B6                       rdim=1 cdim=0
dset=headers_ev                          rng=ev!C5                       rdim=0 cdim=1

dset=headers_prosumage_generation        rng=prosumage!C5                rdim=0 cdim=1
dset=headers_prosumage_storage           rng=prosumage!I5                rdim=0 cdim=1

dset=reserves                            rng=reserves!B6                 rdim=1 cdim=0
dset=headers_reserves                    rng=reserves!F5                 rdim=0 cdim=1
dset=reserves_up_down                    rng=reserves!C6                 rdim=1 cdim=0
dset=reserves_spin_nonspin               rng=reserves!D6                 rdim=1 cdim=0
dset=reserves_prim_nonprim               rng=reserves!E6                 rdim=1 cdim=0

dset=bu                                  rng=heat!B6                     rdim=1 cdim=0
dset=ch                                  rng=heat!C6                     rdim=1 cdim=0
dset=heat_storage                        rng=heat!D6                     rdim=1 cdim=0
dset=heat_hp                             rng=heat!E6                     rdim=1 cdim=0


dset=heat_elec                           rng=heat!F6                     rdim=1 cdim=0
dset=heat_fossil                         rng=heat!G6                     rdim=1 cdim=0


dset=headers_heat                        rng=heat!H5                     rdim=0 cdim=1

par=technology_data_upload       rng=Technologies!A5     rdim=4 cdim=1
par=storage_data                 rng=storage!A5          rdim=2 cdim=1
par=reservoir_data               rng=reservoir!A5        rdim=2 cdim=1
par=dsm_data_upload              rng=DSM!A5              rdim=3 cdim=1
par=topology_data                rng=spatial!B2          rdim=1 cdim=1
%GER_only%par=inc                rng=spatial!L2          rdim=1 cdim=1
par=ev_data                      rng=ev!A5               rdim=2 cdim=1
par=prosumage_data_generation    rng=prosumage!A5        rdim=2 cdim=1
par=prosumage_data_storage       rng=prosumage!G5        rdim=2 cdim=1
par=reserves_data_upload         rng=reserves!A5         rdim=5 cdim=1
par=heat_data_upload             rng=heat!A5             rdim=7 cdim=1
$offecho

%skip_Excel%$call "gdxxrw data_input.xlsx @temp.tmp o=Data_input maxdupeerrors=100";

$GDXin Data_input.gdx
$load n tech headers_tech tech_dispatch tech_res_con
$load sto headers_sto rsvr headers_reservoir reservoir_data dsm headers_dsm dsm_type
$load technology_data_upload storage_data dsm_data_upload
%GER_only%$load l headers_topology topology_data inc
%GER_only%$ontext
$load l headers_topology topology_data
$ontext
$offtext
$load ev headers_ev ev_data
$load headers_prosumage_generation headers_prosumage_storage prosumage_data_generation prosumage_data_storage
$load reserves reserves_up_down reserves_spin_nonspin reserves_prim_nonprim headers_reserves reserves_data_upload
$load bu ch heat_storage heat_hp heat_elec heat_fossil headers_heat heat_data_upload
;

%GER_only%$ontext
parameter inc ;
inc(l,n) = 1 ;
$ontext
$offtext


***************  UPLOAD TIME-SERIES SETS AND PARAMETERS  ***********************

$onecho >temp2.tmp
se=0

dset=h                           rng=basic!A9            rdim=1 cdim=0
dset=headers_time                rng=basic!B8            rdim=0 cdim=1
dset=year                        rng=basic!B7            rdim=0 cdim=1
par=time_data_upload             rng=basic!A6            rdim=1 cdim=3

dset=headers_time_ev             rng=ev!B7               rdim=0 cdim=1
par=ev_time_data_upload          rng=ev!A7               rdim=1 cdim=2

par=reserves_time_data_activation        rng=reserves_activation!A7      rdim=1 cdim=2
par=reserves_time_data_provision         rng=reserves_provision!A7       rdim=1 cdim=2

dset=headers_time_heat           rng=heat!D9             rdim=0 cdim=1
par=dh_upload                    rng=heat!C7             rdim=1 cdim=4
par=theta_night                  rng=heat!A11            rdim=1 cdim=0

dset=headers_time_dhw            rng=heat_dhw!A8         rdim=0 cdim=1
par=d_dhw_upload                 rng=heat_dhw!A6         rdim=1 cdim=4

par=nets_profile                 rng=NETS!A7:B8766       rdim=1 cdim=0

par=temp_source_upload           rng=heat_pump!A7        rdim=1 cdim=2
$offecho

%skip_Excel%$call "gdxxrw time_series.xlsx @temp2.tmp o=time_series";

$GDXin time_series.gdx
$load h headers_time year time_data_upload
$load headers_time_ev ev_time_data_upload
$load reserves_time_data_activation
$load reserves_time_data_provision
$load headers_time_heat dh_upload
$load theta_night
$load temp_source_upload
$load headers_time_dhw d_dhw_upload nets_profile
;




***************  ASSIGNMENTS  **************************************************



***** Aliases *****
alias (h,hh) ;
alias (res,resres) ;
alias (reserves,reservesreserves) ;
alias (nondis,nondisnondis) ;



***** Derived sets *****
dis(tech)$sum( (n,tech_res_con,headers_tech), technology_data_upload(n,tech,tech_res_con,'dis',headers_tech)) = yes;
nondis(tech)$sum( (n,tech_res_con,headers_tech), technology_data_upload(n,tech,tech_res_con,'nondis',headers_tech)) = yes;

con(tech)$sum( (n,tech_dispatch,headers_tech), technology_data_upload(n,tech,'con',tech_dispatch,headers_tech)) = yes;
res(tech)$sum( (n,tech_dispatch,headers_tech), technology_data_upload(n,tech,'res',tech_dispatch,headers_tech)) = yes;

reserves_up(reserves)$sum( (n,reserves_spin_nonspin,reserves_prim_nonprim,headers_reserves), reserves_data_upload(n,reserves,'up',reserves_spin_nonspin,reserves_prim_nonprim,headers_reserves)) = yes;
reserves_do(reserves)$sum( (n,reserves_spin_nonspin,reserves_prim_nonprim,headers_reserves), reserves_data_upload(n,reserves,'do',reserves_spin_nonspin,reserves_prim_nonprim,headers_reserves)) = yes;

reserves_spin(reserves)$sum( (n,reserves_up_down,reserves_prim_nonprim,headers_reserves), reserves_data_upload(n,reserves,reserves_up_down,'spin',reserves_prim_nonprim,headers_reserves)) = yes;
reserves_nonspin(reserves)$sum( (n,reserves_up_down,reserves_prim_nonprim,headers_reserves), reserves_data_upload(n,reserves,reserves_up_down,'nonspin',reserves_prim_nonprim,headers_reserves)) = yes;

reserves_prim(reserves)$sum( (n,reserves_up_down,reserves_spin_nonspin,headers_reserves), reserves_data_upload(n,reserves,reserves_up_down,reserves_spin_nonspin,'prim',headers_reserves)) = yes;
reserves_nonprim(reserves)$sum( (n,reserves_up_down,reserves_spin_nonspin,headers_reserves), reserves_data_upload(n,reserves,reserves_up_down,reserves_spin_nonspin,'nonprim',headers_reserves)) = yes;

reserves_prim_up(reserves)$sum( (n,reserves_spin_nonspin,headers_reserves), reserves_data_upload(n,reserves,'up',reserves_spin_nonspin,'prim',headers_reserves)) = yes;
reserves_prim_do(reserves)$sum( (n,reserves_spin_nonspin,headers_reserves), reserves_data_upload(n,reserves,'do',reserves_spin_nonspin,'prim',headers_reserves)) = yes;
reserves_nonprim_up(reserves)$sum( (n,reserves_spin_nonspin,headers_reserves), reserves_data_upload(n,reserves,'up',reserves_spin_nonspin,'nonprim',headers_reserves)) = yes;
reserves_nonprim_do(reserves)$sum( (n,reserves_spin_nonspin,headers_reserves), reserves_data_upload(n,reserves,'do',reserves_spin_nonspin,'nonprim',headers_reserves)) = yes;

hst(ch)$sum( (n,bu,heat_hp,heat_elec,heat_fossil,headers_heat), heat_data_upload(n,bu,ch,'yes',heat_hp,heat_elec,heat_fossil,headers_heat)) = yes;
hp(ch)$sum( (n,bu,heat_storage,heat_elec,heat_fossil,headers_heat), heat_data_upload(n,bu,ch,heat_storage,'yes',heat_elec,heat_fossil,headers_heat)) = yes;

hel(ch)$sum( (n,bu,heat_storage,heat_hp,heat_fossil,headers_heat), heat_data_upload(n,bu,ch,heat_storage,heat_hp,'yes',heat_fossil,headers_heat)) = yes;
hfo(ch)$sum( (n,bu,heat_storage,heat_hp,heat_elec,headers_heat), heat_data_upload(n,bu,ch,heat_storage,heat_hp,heat_elec,'yes',headers_heat)) = yes;




***** Parameters *****

*--- Generation technologies ---*
technology_data(n,tech,headers_tech) = sum((tech_res_con,tech_dispatch), technology_data_upload(n,tech,tech_res_con,tech_dispatch,headers_tech)) ;
eta(n,tech) = technology_data(n,tech,'eta_con') ;
carbon_content(n,tech) = technology_data(n,tech,'carbon_content') ;
c_up(n,dis) =technology_data(n,dis,'load change costs up') ;
c_do(n,dis) = technology_data(n,dis,'load change costs down') ;
c_fix(n,tech) = technology_data(n,tech,'fixed_costs') ;
c_vom(n,tech) = technology_data(n,tech,'variable_om') ;
CO2price(n,tech) = technology_data(n,tech,'CO2_price') ;

c_inv_overnight(n,tech) = technology_data(n,tech,'oc') ;
lifetime(n,tech) = technology_data(n,tech,'lifetime') ;
recovery(n,tech) = technology_data(n,tech,'recovery_period') ;
interest_rate(n,tech) = technology_data(n,tech,'interest_rate') ;
m_p(n,tech) = technology_data(n,tech,'max_installable') ;
m_e(n,tech) = technology_data(n,tech,'max_energy') ;
grad_per_min(n,dis) = technology_data(n,dis,'load change flexibility') ;
fuelprice(n,tech) = technology_data(n,tech,'fuel costs') ;

c_cu(n,res) = technology_data(n,res,'curtailment_costs') ;


*--- Storage technologies ---*
c_m_sto(n,sto) = storage_data(n,sto,'mc');
eta_sto(n,sto) = storage_data(n,sto,'efficiency');
c_fix_sto(n,sto) = storage_data(n,sto,'fixed_costs');
phi_sto_ini(n,sto) = storage_data(n,sto,'level_start');
etop_max(n,sto) = storage_data(n,sto,'etop_max') ;

c_inv_overnight_sto_e(n,sto) = storage_data(n,sto,'oc_energy');
c_inv_overnight_sto_p(n,sto) = storage_data(n,sto,'oc_capacity');
lifetime(n,sto) = storage_data(n,sto,'lifetime');
interest_rate(n,sto) = storage_data(n,sto,'interest_rate');
m_sto_e(n,sto) = storage_data(n,sto,'max_energy');
m_sto_p(n,sto) = storage_data(n,sto,'max_power');


*--- Reservoir technologies ---*
c_m_rsvr(n,rsvr) = reservoir_data(n,rsvr,'mc');
eta_rsvr(n,rsvr) = reservoir_data(n,rsvr,'efficiency');
c_fix_rsvr(n,rsvr) = reservoir_data(n,rsvr,'fixed_costs');
phi_rsvr_ini(n,rsvr) = reservoir_data(n,rsvr,'level_start');
phi_rsvr_lev_min(n,rsvr) = reservoir_data(n,rsvr,'level_min');

c_inv_overnight_rsvr_e(n,rsvr) = reservoir_data(n,rsvr,'oc_energy');
c_inv_overnight_rsvr_p(n,rsvr) = reservoir_data(n,rsvr,'oc_capacity');
inv_lifetime_rsvr(n,rsvr) = reservoir_data(n,rsvr,'lifetime');
inv_interest_rsvr(n,rsvr) = reservoir_data(n,rsvr,'interest_rate');
m_rsvr_e(n,rsvr) = reservoir_data(n,rsvr,'max_energy');
m_rsvr_p(n,rsvr) = reservoir_data(n,rsvr,'max_power');


*--- DSM technologies ---*
dsm_curt(dsm)$sum( (n,dsm_type,headers_dsm), dsm_data_upload(n,dsm,'curt',headers_dsm)) = yes;
dsm_shift(dsm)$sum( (n,dsm_type,headers_dsm), dsm_data_upload(n,dsm,'shift',headers_dsm)) = yes;
dsm_data(n,dsm,headers_dsm) = sum(dsm_type, dsm_data_upload(n,dsm,dsm_type,headers_dsm) ) ;

c_m_dsm_cu(n,dsm_curt) = dsm_data(n,dsm_curt,'mc')     ;
c_m_dsm_shift(n,dsm_shift) = dsm_data(n,dsm_shift,'mc')  ;
c_fix_dsm_cu(n,dsm_curt) = dsm_data(n,dsm_curt,'fc')  ;
c_fix_dsm_shift(n,dsm_shift) = dsm_data(n,dsm_shift,'fc') ;

t_dur_dsm_cu(n,dsm_curt) = dsm_data(n,dsm_curt,'max_duration')   ;
t_off_dsm_cu(n,dsm_curt) = dsm_data(n,dsm_curt,'recovery_time')   ;
t_dur_dsm_shift(n,dsm_shift) = dsm_data(n,dsm_shift,'max_duration')   ;
t_off_dsm_shift(n,dsm_shift) = dsm_data(n,dsm_shift,'recovery_time')   ;
eta_dsm_shift(n,dsm_shift)  = dsm_data(n,dsm_shift,'efficiency')   ;

c_inv_overnight_dsm_cu(n,dsm_curt) =  dsm_data(n,dsm_curt,'oc')   ;
c_inv_overnight_dsm_shift(n,dsm_shift)  =  dsm_data(n,dsm_shift,'oc')   ;
inv_recovery_dsm_cu(n,dsm_curt)  =  dsm_data(n,dsm_curt,'lifetime')   ;
inv_recovery_dsm_shift(n,dsm_shift)   =  dsm_data(n,dsm_shift,'lifetime')   ;
inv_interest_dsm_cu(n,dsm_curt)   =  dsm_data(n,dsm_curt,'interest_rate')   ;
inv_interest_dsm_shift(n,dsm_shift)  =  dsm_data(n,dsm_shift,'interest_rate')   ;
m_dsm_cu(n,dsm_curt) =  dsm_data(n,dsm_curt,'max_installable')   ;
m_dsm_shift(n,dsm_shift) =  dsm_data(n,dsm_shift,'max_installable')   ;


*--- Temporal data ---*
d_y(n,year,h) = time_data_upload(h,n,year,'demand')  ;
phi_res_y(n,year,res,h) = sum(headers_time$(sameas(res,headers_time)), time_data_upload(h,n,year,headers_time));
rsvr_in_y(n,year,rsvr,h) = sum(headers_time$(sameas(rsvr,headers_time)), time_data_upload(h,n,year,headers_time));
phi_reserves_call_y(n,year,reserves,h) = reserves_time_data_activation(h,year,reserves) ;
reserves_exogenous_y(n,year,reserves,h) = reserves_time_data_provision(h,year,reserves) ;


*--- Spatial data ---*
inv_lifetime_ntc(l) = topology_data(l,'lifetime') ;
inv_recovery_ntc(l) = topology_data(l,'recovery_period') ;
inv_interest_ntc(l) = topology_data(l,'interest_rate') ;
c_inv_overnight_ntc(l) = topology_data(l,'overnight_costs') ;
c_fix_ntc(l) = topology_data(l,'fixed_costs') ;
m_ntc(l) = topology_data(l,'max_installable') ;
dist(l) = topology_data(l,'distance') ;


*--- Electric vehicles ---*
c_m_ev(n,ev) = ev_data(n,ev,'mc') ;
pen_phevfuel(n,ev) = ev_data(n,ev,'penalty_fuel') ;
eta_ev_in(n,ev) = ev_data(n,ev,'efficiency_charge') ;
eta_ev_out(n,ev) = ev_data(n,ev,'efficiency_discharge') ;
phi_ev_ini(n,ev) = ev_data(n,ev,'ev_start') ;

n_ev_e(n,ev) = ev_data(n,ev,'ev_capacity') ;
phi_ev(n,ev) = ev_data(n,ev,'share_ev') ;
ev_phev(n,ev) = ev_data(n,ev,'ev_type') ;

n_ev_p(n,ev,h) = ev_time_data_upload(h,'n_ev_p',ev) ;
ev_ed(n,ev,h) = ev_time_data_upload(h,'ev_ed',ev) ;
ev_ged_exog(n,ev,h) = ev_time_data_upload(h,'ev_ged_exog',ev) ;


*--- Prosumage ---*
m_res_pro(n,res) = prosumage_data_generation(n,res,'max_power') ;
m_sto_pro_e(n,sto) = prosumage_data_storage(n,sto,'max_energy') ;
m_sto_pro_p(n,sto) = prosumage_data_storage(n,sto,'max_power') ;
phi_sto_pro_ini(n,sto) = 0 ;


*--- Reserves ---*
reserves_data(n,reserves,headers_reserves) = sum((reserves_up_down,reserves_spin_nonspin,reserves_prim_nonprim), reserves_data_upload(n,reserves,reserves_up_down,reserves_spin_nonspin,reserves_prim_nonprim,headers_reserves)) ;
phi_reserves_share(n,reserves) = reserves_data(n,reserves,'share_sr_mr') ;
reserves_intercept(n,reserves) = reserves_data(n,reserves,'intercept') ;
reserves_slope(n,reserves,'wind_on') = reserves_data(n,reserves,'slope_wind_on') ;
reserves_slope(n,reserves,'wind_off') = reserves_data(n,reserves,'slope_wind_off') ;
reserves_slope(n,reserves,'pv') = reserves_data(n,reserves,'slope_pv') ;
reserves_reaction(n,reserves) = reserves_data(n,reserves,'reaction_time') ;
phi_reserves_pr_up(n) = reserves_data(n,'PR_up','fraction_pr') ;
phi_reserves_pr_do(n) = reserves_data(n,'PR_do','fraction_pr') ;


*--- Heat ---*
heat_data(n,bu,ch,headers_heat) = sum((heat_storage,heat_hp,heat_elec,heat_fossil), heat_data_upload(n,bu,ch,heat_storage,heat_hp,heat_elec,heat_fossil,headers_heat)) ;
phi_heat_type(n,bu,ch) = heat_data(n,bu,ch,'share') ;
dh_y(n,year,bu,ch,h) = phi_heat_type(n,bu,ch) * dh_upload(h,n,year,'demand',bu) ;
d_dhw_y(n,year,bu,ch,h) = phi_heat_type(n,bu,ch) * d_dhw_upload(h,n,year,'demand',bu) ;

eta_heat_stat(n,bu,ch) = heat_data(n,bu,ch,'static_efficiency') ;
eta_heat_dyn(n,bu,ch) = heat_data(n,bu,ch,'dynamic_efficiency') ;
eta_dhw_aux_stat(n,bu,ch) = heat_data(n,bu,ch,'static_efficiency_sets_aux_dhw') ;
* currently not used
n_heat_p_in(n,bu,ch) = heat_data(n,bu,ch,'max_power') ;
n_heat_p_out(n,bu,ch) = heat_data(n,bu,ch,'max_outflow') ;
n_heat_e(n,bu,ch) = heat_data(n,bu,ch,'max_level') ;
n_sets_p_in(n,bu,ch) = heat_data(n,bu,ch,'max_power') ;
n_sets_p_out(n,bu,ch) = heat_data(n,bu,ch,'max_outflow') ;
n_sets_e(n,bu,ch) = heat_data(n,bu,ch,'max_level') ;
n_sets_dhw_p_in(n,bu,ch) = heat_data(n,bu,ch,'max_power_in_sets_aux_dhw') ;
n_sets_dhw_p_out(n,bu,ch) = heat_data(n,bu,ch,'max_power_out_sets_aux_dhw') ;
n_sets_dhw_e(n,bu,ch) = heat_data(n,bu,ch,'max_energy_sets_aux_dhw') ;
phi_heat_ini(n,bu,ch) = heat_data(n,bu,ch,'level_ini') ;
temp_sink(n,bu,ch) = heat_data(n,bu,ch,'temperature_sink') ;

temp_source(n,bu,'hp_as',h) = temp_source_upload(h,n,'hp_as') ;
temp_source(n,bu,'hp_gs',h) = heat_data(n,bu,'hp_gs','temperature_source') ;

temp_source(n,bu,'gas_hp_gs',h) = heat_data(n,bu,'gas_hp_gs','temperature_source') ;
temp_source(n,bu,'hp_gs_elec',h) = heat_data(n,bu,'hp_gs_elec','temperature_source') ;

pen_heat_fuel(n,bu,ch) = heat_data(n,bu,ch,'penalty_non-electric_heat_supply') ;

area_floor(n,bu,ch) = heat_data(n,bu,ch,'area_floor') ;



***************  CALCULATE DERIVED PARAMETERS  *********************************

c_m(n,tech) = fuelprice(n,tech)/eta(n,tech) + carbon_content(n,tech)/eta(n,tech)*CO2price(n,tech) + c_vom(n,tech)   ;

c_i(n,tech) = c_inv_overnight(n,tech)*( interest_rate(n,tech) * (1+interest_rate(n,tech))**(lifetime(n,tech)) )
                  / ( (1+interest_rate(n,tech))**(lifetime(n,tech))-1 )       ;

c_i_res(n,res) = c_i(n,res) ;
c_fix_res(n,res) = c_fix(n,res) ;

c_i_sto_e(n,sto) = c_inv_overnight_sto_e(n,sto)*( interest_rate(n,sto) * (1+interest_rate(n,sto))**(lifetime(n,sto)) )
                 / ( (1+interest_rate(n,sto))**(lifetime(n,sto))-1 )       ;

c_i_sto_p(n,sto) = c_inv_overnight_sto_p(n,sto)*( interest_rate(n,sto) * (1+interest_rate(n,sto))**(lifetime(n,sto)) )
                 / ( (1+interest_rate(n,sto))**(lifetime(n,sto))-1 )       ;

c_i_rsvr_e(n,rsvr) = c_inv_overnight_rsvr_e(n,rsvr)*( inv_interest_rsvr(n,rsvr) * (1+inv_interest_rsvr(n,rsvr))**(inv_lifetime_rsvr(n,rsvr)) )
                 / ( (1+inv_interest_rsvr(n,rsvr))**(inv_lifetime_rsvr(n,rsvr))-1 )       ;

c_i_rsvr_p(n,rsvr) = c_inv_overnight_rsvr_p(n,rsvr)*( inv_interest_rsvr(n,rsvr) * (1+inv_interest_rsvr(n,rsvr))**(inv_lifetime_rsvr(n,rsvr)) )
                 / ( (1+inv_interest_rsvr(n,rsvr))**(inv_lifetime_rsvr(n,rsvr))-1 )       ;

c_i_dsm_cu(n,dsm_curt) =c_inv_overnight_dsm_cu(n,dsm_curt)*( inv_interest_dsm_cu(n,dsm_curt) * (1+inv_interest_dsm_cu(n,dsm_curt))**(inv_recovery_dsm_cu(n,dsm_curt)) )
                 / ( (1+inv_interest_dsm_cu(n,dsm_curt))**(inv_recovery_dsm_cu(n,dsm_curt))-1 )       ;

c_i_dsm_shift(n,dsm_shift) = c_inv_overnight_dsm_shift(n,dsm_shift)*( inv_interest_dsm_shift(n,dsm_shift) * (1+inv_interest_dsm_shift(n,dsm_shift))**(inv_recovery_dsm_shift(n,dsm_shift)) )
                 / ( (1+inv_interest_dsm_shift(n,dsm_shift))**(inv_recovery_dsm_shift(n,dsm_shift))-1 )       ;

c_i_ntc(l) = c_inv_overnight_ntc(l) * (inv_interest_ntc(l) * (1 + inv_interest_ntc(l))**(inv_lifetime_ntc(l)) )
                 / ((1 + inv_interest_ntc(l)) ** (inv_lifetime_ntc(l))-1 ) ;

phi_mean_reserves_call_y(n,year,reserves) = sum(h, phi_reserves_call_y(n,year,reserves,h) ) / card(h) + eps ;


theta_sets(n,bu,'setsh')$(smax( (heat_storage,heat_hp,heat_elec,heat_fossil,headers_heat) , heat_data_upload(n,bu,'setsh',heat_storage,heat_hp,heat_elec,heat_fossil,headers_heat)) > 0 AND phi_heat_type(n,bu,'setsh')) = 1;
theta_dir(n,bu,'dir')$(smax((heat_storage,heat_hp,heat_elec,heat_fossil,headers_heat) , heat_data_upload(n,bu,'dir',heat_storage,heat_hp,heat_elec,heat_fossil,headers_heat)) > 0 AND phi_heat_type(n,bu,'dir')) = 1;

theta_storage(n,bu,ch)$(sum((heat_hp,heat_elec,heat_fossil,headers_heat), heat_data_upload(n,bu,ch,'yes',heat_hp,heat_elec,heat_fossil,headers_heat)) AND phi_heat_type(n,bu,ch)) = 1;
theta_hp(n,bu,ch)$sum( (heat_storage,heat_elec,heat_fossil,headers_heat), heat_data_upload(n,bu,ch,heat_storage,'yes',heat_elec,heat_fossil,headers_heat) AND phi_heat_type(n,bu,ch)) = 1;

theta_elec(n,bu,ch)$sum( (heat_storage,heat_hp,heat_fossil,headers_heat), heat_data_upload(n,bu,ch,heat_storage,heat_hp,'yes',heat_fossil,headers_heat) AND phi_heat_type(n,bu,ch)) = 1;
theta_fossil(n,bu,ch)$sum( (heat_storage,heat_hp,heat_elec,headers_heat), heat_data_upload(n,bu,ch,heat_storage,heat_hp,heat_elec,'yes',headers_heat) AND phi_heat_type(n,bu,ch)) = 1;



***************  Adjust costs to model's hourly basis **************************

c_i(n,tech) = c_i(n,tech)*card(h)/8760 ;
c_i_res(n,tech) = c_i_res(n,tech)*card(h)/8760 ;
c_i_sto_p(n,sto) = c_i_sto_p(n,sto)*card(h)/8760 ;
c_i_sto_e(n,sto) = c_i_sto_e(n,sto)*card(h)/8760 ;
c_i_rsvr_e(n,rsvr) = c_i_rsvr_e(n,rsvr)*card(h)/8760 ;
c_i_rsvr_p(n,rsvr) = c_i_rsvr_p(n,rsvr)*card(h)/8760 ;
c_i_dsm_cu(n,dsm_curt) = c_i_dsm_cu(n,dsm_curt)*card(h)/8760 ;
c_i_dsm_shift(n,dsm_shift) = c_i_dsm_shift(n,dsm_shift)*card(h)/8760 ;

c_fix(n,tech) = c_fix(n,tech)*card(h)/8760 ;
c_fix_sto(n,sto) = c_fix_sto(n,sto)*card(h)/8760 ;
c_fix_dsm_cu(n,dsm_curt) = c_fix_dsm_cu(n,dsm_curt)*card(h)/8760 ;
c_fix_dsm_shift(n,dsm_shift) = c_fix_dsm_shift(n,dsm_shift)*card(h)/8760 ;
c_fix_rsvr(n,rsvr) = c_fix_rsvr(n,rsvr)*card(h)/8760 ;

m_e(n,'bio') = m_e(n,'bio')*card(h)/8760 ;

c_i_ntc(l) = c_i_ntc(l) * card(h)/8760 ;

*t_dur_dsm_cu(n,dsm_curt) = t_dur_dsm_cu(n,dsm_curt) ;
*t_off_dsm_cu(n,dsm_curt) = t_off_dsm_cu(n,dsm_curt) ;
*t_dur_dsm_shift(n,dsm_shift)$(ord(dsm_shift)=2 or ord(dsm_shift)=4 or ord(dsm_shift)=5) = t_dur_dsm_shift(n,dsm_shift) / 2 ;
*t_dur_dsm_shift(n,dsm_shift)$(ord(dsm_shift)=1 or ord(dsm_shift)=3) = 2 ;



***************  Check for parameter sanity ************************************

Parameter
check_heat
check_heat_agg ;
check_heat(n,bu) = sum( ch , phi_heat_type(n,bu,ch)) ;
check_heat_agg = smax( (n,bu) , check_heat(n,bu) ) ;
*abort$(check_heat_agg > 1) "DATA: heating technologies for a building type do not add up to 100 percent" ;




***************  Infeasibility *************************************************

Positive variable
G_INFES(n,h)
;

Parameter
c_infes  /100000/
;














