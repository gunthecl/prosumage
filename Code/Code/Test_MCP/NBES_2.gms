Sets
tech                Technology                                   /c,g,o/
t                    hour                                        /1,2/
;

Variables
Z
lambda(t)
mu(tech,t)

Positive variables
g(tech, t) generation

;

Parameters
d(t)       demand
cv(tech)   variable costs
cap(tech)  capactiy max
lev_Z      total costs
;

d('1')   = 500  ;
d('2')   = 500 ;
cv('g')  = 200;
cv('c')  = 50;
cv('o')  = 2000;

cap('g')  = 300;
cap('c')  = 300;
cap('o')  = 300;

Equations
obj
energy_balance
cap_constraint
KKTG
KKTL
KKTM
;

obj..


         Z =E=  -sum ( (tech,t), cv(tech)*g(tech,t) )

;

energy_balance(t)..

                 sum( tech, g(tech, t)) - d(t) =E= 0
;

cap_constraint(tech, t)..

          g(tech, t) =L= cap(tech)  ;


KKTG(tech,t)..

     cv(tech) - lambda(t) + mu(tech,t) =G= 0

;

KKTL(t)..


       sum( tech, g(tech, t)) =E= d(t)

;

KKTM(tech,t)..

        cap(tech) - g(tech, t)  =G= 0 


;




Model testmodel /
KKTG.g
KKTL
KKTM.mu
/


options
optcr = 0.00
reslim = 10000000
lp = cplex
mip = cplex
nlp = conopt
dispwidth = 15
limrow = 0
limcol = 0
solprint = off
sysout = off
optcr = 1e-3
optca = 10
;

$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
epagap 10
parallelmode -1
$offecho


solve   testmodel using mcp;

lev_Z = sum( (tech, t), cv(tech)*g.l(tech,t))


display g.l , lambda.l , lev_Z, mu.l