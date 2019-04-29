********************************************************************************
***** Tariff Scenario 2030 *****
********************************************************************************

*** Define tariffs per MWh (additive)

* Time-invariant retail price
energy_component      =  50  ;
non_energy_component  = 250  ;
retail_price          = energy_component + non_energy_component ;

* Time-invariant FIT
FIT          =  80 ;

* Define volumetric prices for prosumage households (additivie tariffs per MWh)

** Consumption side
price_consumption_pro(h)  =   retail_price ;

* Choose that real-time pricing is added on consumption side by setting "*"
$setglobal  RTP_cons ""   ;

* Self-consumption tariff  (applies to G_RES_PRO and STO_OUT_PRO2PRO)
SC_tax                    =  0 ;

** Production side
price_production_pro(h)   =   FIT ;

* Choose that real-time pricing is added on consumption side by setting "*"
$setglobal  RTP_prod ""  ;


*** Further options
* Limit feed-in of prosumage feed-in to alpha_max of maximum PV energy
* Note: Maximum PV energy referes to installed PV capacity multiplied by maximum
*       solar availability (determined by time series)
alpha_max       = 0.5    ;
max_solar_avail = 0.7067 ;

* Remove star to select the feed-in restriction
$setglobal no_feed_in_max "*"

*** Allow no feed-in from prosumage households
* Remove star to prevent feed-in from prosumage households (i.e. FIT = 0)
$setglobal allow_feed_in "*"


*** Define annual fixed network fee

flat_network_fee  = 0 ;

********************************************************************************
