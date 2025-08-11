# TACOS Study Codebook

## Dataset Overview
This codebook documents the variables in the TACOS dataset. The dataset contains observations of participants' food preferences, demographic information, experimental conditions, and survey metadata.

## Variables

**record_id**  
Unique identifier assigned to each participant

**treatment_arm**  
Experimental condition assigned to participant
- 1: Control condition
- 2: Condition with veggies and Sofritas options
- 3: Condition with veggies, Sofritas, and Chick'nitas options

**taco_choice**  
Participant's selected taco filling as a factor
- "Chicken"
- "Steak"
- "Beef barbacoa"
- "Carnitas"
- "Sofritas (PMA)"
- "Chick'nitas (PMA)"
- "Veggies (includes guacamole)"
- "I would not order any of these fillings."

**meat_select**  
Binary indicator of whether participant selected an animal-based protein
- 0: Plant-based protein selected (Sofritas, Chick'nitas, Veggies)
- 1: Animal protein selected (Chicken, Steak, Beef barbacoa, Carnitas)
- NA: Missing data or no selection

**chicken_select**  
Binary indicator of whether participant specifically selected chicken
- 0: Did not select chicken
- 1: Selected chicken

**age**  
Age of participant in years

**gender**  
Gender of participant
- "Male"
- "Female"
- NA: Not reported or other

**has_dairy**  
Boolean indicating if participant's order includes dairy products
- TRUE: Contains dairy
- FALSE: Does not contain dairy

**is_vegan**  
Binary indicator of vegan status
- 0: Not vegan
- 1: Vegan

**county_name**  
County where participant resides

**state**  
State or territory code where participant resides

**location**  
Combined location in format "County, State"

**dob**  
Year of birth

**times_visited_chipotle_past_month**  
Number of times participant visited Chipotle in the past month

**ffq_cow_freq**  
Frequency of beef consumption
- "Never"
- "1 time in the past week"
- "2 times in the past week"
- "3-4 times in the past week"
- "5-6 times in the past week"
- "1 time per day"
- "2 or more times per day"

**ffq_pig_freq**  
Frequency of pork consumption (same options as ffq_cow_freq)

**ffq_attn_freq**  
Attention check in the food frequency questionnaire
- Participants were instructed to select "2 or more times per day" for this question

**ffq_hen_freq**  
Frequency of chicken consumption (same options as ffq_cow_freq)

**ffq_pb_freq**  
Frequency of plant-based protein consumption (same options as ffq_cow_freq)

**ffq_fish_freq**  
Frequency of fish consumption (same options as ffq_cow_freq)

**ffq_dairy_freq**  
Frequency of dairy consumption (same options as ffq_cow_freq)

**ffq_eggs_freq**  
Frequency of egg consumption (same options as ffq_cow_freq)

**opted_out_all**  
Asked what participants who did not select a taco would eat instead
- "My preferred meal would be something vegetarian"
- "My preferred meal would be something with a different kind of meat"
- "My preferred meal would be something with beef"
- "My preferred meal would be something with chicken"
- NA: Participant selected a taco

**merged_protein**  
Numeric code for the participant's protein choice
- 1: Chicken
- 2: Steak
- 3: Beef barbacoa
- 4: Carnitas
- 5: Sofritas (plant-based protein)
- 6: Chick'nitas (plant-based protein)
- 7: Veggies (includes guacamole)
- 97: I would not order any of these fillings

**attention_check**  
Numeric response to attention check question ( a recall of what meat they ordered in their taco)
- Those who passed the attention check are participants for whom the merged_protein value matches their
attention_check value

**block_order_pre_tacos**  
Order of survey blocks presented before the taco selection task
- "None": Taco task was presented first
- "Pen": Pen task preceded taco task
- "Shirt": Shirt task preceded taco task
- "Pen-Shirt": Both pen and shirt tasks preceded taco task (in that order)
- "Shirt-Pen": Both shirt and pen tasks preceded taco task (in that order)

**tortilla_type**  
Type of tortilla selected
- "Soft flour tortillas"
- "Crispy corn tortillas"
- Other options as present in dataset

**regular_toppings**  
Regular toppings selected by participant
- String containing the combination of selected toppings (e.g., "White rice + Black beans + Cheese")

**premium_toppings**  
Premium toppings selected by participant
- Examples: "Guacamole", "Queso blanco", "No premium toppings"

**bev_type**  
Type of beverage selected
- "Bottled drink"
- "Fountain drink"
- NA: None selected or missing data

**purpose_guess**  
Participant's guess about the study's purpose (free text response)

**education**  
Highest level of education attained
- "8th grade or less"
- "Some high school, but did not graduate"
- "High school or GED"
- "Some college or 2-year degree"
- "4-year college graduate"
- "More than 4-year college degree"
- NA: Not reported

**ethnicity**  
Self-reported ethnicity
- "Caucasian"
- "African American"
- "Hispanic or Latino"
- "Asian or Pacific Islander"
- "Native American"
- Multi-ethnic categories (e.g., "Multi: Caucasian + East Asian")
- "Other"
- NA: Not reported

**political_party**  
Political affiliation
- "Democrat"
- "Republican" 
- "Independent"
- "Other"
- "Prefer not to answer"
- NA: Not reported

**time_elapsed**  
Total time spent completing the survey (in seconds)