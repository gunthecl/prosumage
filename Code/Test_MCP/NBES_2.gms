Sets
tech                Technology                                   /c,g,o/
t                    hour                                        /1,2/
;

Variables
Z


Positive variables
g(tech,t) generation
lambda(t)
mu(tech,t)

;

Parameters
d(t)       demand
cv(tech)   variable costs
cap(tech)  capactiy max
lev_Z      total costs
;

d('1')   = 500  ;
d('2')   = 400 ;
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


         Z =E=  sum ( (tech,t), cv(tech)*g(tech,t) )

;

energy_balance(t)..

                 sum( tech, g(tech,t)) =E= d(t)
;

cap_constraint(tech,t)..

         cap(tech)  =G= g(tech,t)   ;


KKTG(tech,t)..

     cv(tech) + mu(tech,t) =G= lambda(t)

;

KKTL(t)..


       sum( tech, g(tech,t)) =G= d(t)

;

KKTM(tech,t)..

        cap(tech) =G= g(tech,t)


;




Model testmodel /
KKTG.g
KKTL.lambda
KKTM.mu
/  ;

*Set  some  i n i t i a l  v a l u e s

g.l(tech,t) = 1;


solve   testmodel using mcp;

lev_Z = sum( (tech,t), cv(tech)*g.l(tech,t))


display g.l , lambda.l , lev_Z, mu.l
