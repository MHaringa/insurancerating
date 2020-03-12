# insurancerating 0.5.0

* Function construct_tariff_classes is now split in functions fit_gam and construct_tariff_classes.
* A vignette is added on how to use the package

# insurancerating 0.4.3
 
* Function added to split rows with a time period longer than one month to multiple rows with a time period of exactly one month each.

# insurancerating 0.4.2

* It is now possible to also construct tariff classes for claim severity models. This can be done by choosing 'severity' as specification for the model argument in the construct_tariff_classes function. 

