dataset "agediff"

This is just a reference dataset with the hatching dates of each nestling.

Brood_ID = ID of each  brood
hdate_A = hatching date of nestling A
hdate_B = hatching date of nestling B
hdate_C = hatching date of nestling C
direction = direction of the aggression
age_diff = age difference between the attacker and the recipient. Negative differences indicate older attacking younger individual

-------------------------
dataset "food_df"

This is a dataset with all the single feeding bouts recorded and the corresponding total biomass brought to the nest each day

eater = the receiver of the feeding bout
Age_A = age of the oldest nestling
brood_ID = ID of each brood
weight_tot = total biomass delivered to the nest
Date_analysis = julian date of the day the feeding event occurred
phase = early/late (see Methods section)

--------------------------
dataset "peck_tot"

This is the dataset with information about each recorded peck.

attack_don = id of the individual attacking
attack_rec = id of the individual receiving the attack
Date_analysis = julian date of the day the pecking event occurred
Age_A = age of the oldest nestling
Age_B = age of the second hatched nestling
Age_C = age of the youngest nestling
direction = direction of the attack
brood_ID = ID of each brood
phase = early/late (see Methods section)
weight_tot = total amount of biomass delivered per day to the nest
food_cat = categorical version of the biomass delivered (if in the 1st quantile = low, if above the 3rd quantile = high, if in between = medium)



