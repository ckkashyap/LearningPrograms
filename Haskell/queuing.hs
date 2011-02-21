-- MM1

lambda = 8 -- arrival rate
mu = 9 -- service rate

rho = lambda / mu -- expected number of people being served

ls = rho / (1-rho) -- length of the system

lq = ls - rho -- length of the queue - from the fact that ls = lq + expected number of people being served which is rho

ws = ls / lambda -- little's law ls = lambda * ws
wq = lq / lambda -- little's law lq = lambda * wq


