## Measurement variability plot

This plot presents the measurement values - mass transformed from centroid m/z values of isotopic envelopes gathered for possible charge values during the experimental course.

On the Y axis there is each file containing separate replicate of experiment. Within each, there is measurement for each charge value, and then the aggregated value, distinguished as the black dot. The color indicated the charge value for which the measurement was taken, with dot size signifying the relative intensity of the measurement. The measurements are shown on daltons, as shown on the X axis.

The horizontal red dashed line shows the mass value, as mean value from the replicate values. The red are signifies the error of measurement surrounding the mean value - calculated as standard deviation. 

For more information about mass measurement aggregation see this [article](https://hadexversum.github.io/HaDeX2/articles/transformation.html).

### HaDeX tips

The peptide table can be sorted by the standard deviation - to easily spot the measurements with higher variability.

When the `Show replicate values?` option from the settings Visualization panel is checked, the values are shown in aggregated form for replicates, without the infomration on charge value.
