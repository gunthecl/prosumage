
sets   i   canning plants,
       j   markets ;

parameter
       s(i)    capacity of plant i in cases,
       d(j)    demand at market j in cases,
       c(i,j)  transport cost in thousands of dollars per case ;

$include transmcp.dat

positive variables
       x(i,j)           shipment quantities in cases
       p_demand(j)      price at market j
       p_supply(i)      price at plant i;

equations
       supply(i)      observe supply limit at plant i
       demand(j)      satisfy demand at market j
       rational(i,j);

supply(i) ..     s(i) =g= sum(j, x(i,j)) ;

demand(j) ..     sum(i, x(i,j))  =g=  d(j) ;

rational(i,j) .. p_supply(i) + c(i,j) =g= p_demand(j) ;

model transport / rational.x, demand.p_demand, supply.p_supply /;

solve transport using mcp;
