#1. Sort variables

# 1.1. Exposures
#Grouping foods
#by weight
data_weight<-data %>%
    mutate(Wlegumes=(beans+),
           Wleg_pea= (Wlegumes + peas)
           Wmeats=(beef+lamb+...),
           Wpoultry=(),
           Wfish=(),
           Wmixdish=(),
           Wruits= (),
           Wveggies=()
           Wfats=(),
           Wdrinks= (),
           Wcereal = (),
           Wdairy= (),
           Wsauces=(),
           Wsweets=(),
           Walcohol=(),

           )

           )


#by energy
#Energy provided per food <-- I will need to mutate as above with a multiplication for energy contributions from each foods, e.g. weight_beef/(100*E*0.239kcal)
data_kcal<-data %>%
    mutate(energy_foods=(p26002)*0.293,
           Elegumes=(),
           Emeats=((beef*E)+(lamb*E)+...)*0.239kcal,
    )


#1.2. outcomes
nafld
nash
end of follow-up
death
loss to follow-up

#1.3. Covariables

#For metabolic disease variable: hypertension, high cholesterol, diabetes
p20002,"Non-cancer illness code, self-reported",387470,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20002
#p20002: hypertension, high cholesterol, type 2 diabetes



