## Coverage Heatmap Plot

This plot presents both the coverage information as well as one of two values: AUC or back-exchange.

### AUC

Area under the deuterium uptake curve for specific peptide. If the value is bigger that one (as it is calculated with respect to the last time point of the curve) it is an indicator of back-exchange, e.q. deuterium uptake value being higher and then lower in time.

### Back-exchange

Back-exchange is a very important phenomenon in HDX-MS, as it tells the difference between possible exchange and actual exchange. It is calculated as follows:

$$bex = 1 - frac\_du_{theo, t=100\%}$$

where $frac\_du_{theo, t=100\%}$ is the theoretical fractional deuterium uptake calculated for the maximum deuteration control.

