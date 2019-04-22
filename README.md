# dhs_iycf
calculate infant and young child feeding indicators using DHS dataset

*----------------------------------------------------------------------------------
** (1): USE ALL BIRTH DATASET - TO CONSTRUCT INFANT CHILD FEEDING INDEX (ICFI) **
*----------------------------------------------------------------------------------

use "${kr_child}/kr_child.dta", clear // change your kr dataset directory and name her!
gen in_kr = 1
order in_kr
sort v001 v002 v003


** child feeding related variable collection **
lookfor breastfeeding

// breastfeeding practice var //
codebook v404 m4 m5 v415 v426


// liquid recall 24 hrs var //
codebook v409 v409a v410 v410a v411 v411a v412 v412a v412b ///
v412c v413 v413a v413b v413c v413d v414a v414b v414c v414d 

// liquid consumption time //
codebook v469e v469f v469x

// solid diet recall 24 hrs var //
codebook v414e v414f v414g v414h v414i v414j v414k v414l v414m v414n v414o ///
v414p v414q v414r v414s v414t v414u v414v v414w 

// meal frequency time //
codebook m39

lookfor months
// child age in months and Child DOB var //
codebook hw1 v006 v007 v008 v008a b1 b2 b3 hw16

tab hw1, m

br v404 m4 m5 v409 v409a v410 v410a v411 v411a v412 v412a v412b ///
v412c v413 v413a v413b v413c v413d v414a v414b v414c v414d v414e v414f ///
v414g v414h v414i v414j v414k v414l v414m v414n v414o v414p v414q v414r ///
v414s v414t v414u v414v v414w v415 v426 if hw1 >23 & !mi(hw1)

br v404 m4 m5 v409 v409a v410 v410a v411 v411a v412 v412a v412b ///
v412c v413 v413a v413b v413c v413d v414a v414b v414c v414d v414e v414f ///
v414g v414h v414i v414j v414k v414l v414m v414n v414o v414p v414q v414r ///
v414s v414t v414u v414v v414w v415 v426 if hw1 <=23 & !mi(hw1)


br v414n if hw1 >23 & !mi(hw1)
tab v414n if hw1 >23 & !mi(hw1)
tab v414n if hw1 < 24

tab m39,m
tab m39 if hw1 >23 & !mi(hw1)
tab m39 if hw1 < 24

** Note: no food consumption and meal frequency var for over 23 months child population 
** not available to calculate ICFI index for >23 months population 


** (A) - SELECTED ONLY POPULATION INTEREST IN STUDY **
** sample code for breastfeeding indicators from DHS User forumn
** weblink - https://userforum.dhsprogram.com/index.php?t=msg&goto=9598
// use "GHKR70FL.DTA" 


* Last child in the last 2 years living with mother
* age in months
gen child_valid_age = v008-b3 // if calculate child age with hw1, got slightly different result


* drop if too old or not alive
keep if child_valid_age < 24 & b5 == 1


* recode age into groups
recode child_valid_age	(0/1 = 1 "0-1")(2/3 = 2 "2-3")(4/5 = 3 "4-5")(6/8 = 4 "6-8") ///
						(9/11 = 5 "9-11")(12/17 = 6 "12-17")(18/23 = 7 "18-23") ///
						(24/59 = .n), gen(child_age)

recode child_valid_age 	(0/5 = 0 "0-5")(6/23 = 1 "6-23")(24/59 = .n), gen(child_age_2)

recode child_valid_age 	(6/8 = 1 "6-8")(9/11 = 2 "9-11")(12/17 = 3 "12-17") ///
						(18/23 = 4 "18-23")(0/5 = .n)(24/59 = .n), gen(child_age_3)

recode child_valid_age 	(0/11 = 1 "0-11")(12/23 = 2 "12-23")(24/59 = .n), gen(child_age_4)

recode child_valid_age	(0/5 = 1 "0-5")(6/8 = 2 "6-8")(9/11 = 3 "9-11")(12/17 = 4 "12-17") ///
						(18/23 = 5 "18-23")(24/35 = 6 "24-35")(36/47 = 7 "36-47") ///
						(48/59 = 8 "48-59"), gen(child_age_grp)


* tab of all living children born in the last 2 years
tab child_age
tab child_age [iw = v005/1000000]

* keep only those children living with mother ...
keep if b9 == 0

* ... and keep the last born of those
drop if _n > 0 & caseid == caseid[_n-1]


* weighted caluclation var construction 
gen wgt = v005/1000000
tab wgt, m

lookfor sampling // search for samping unit var

gen psu = v021
gen strata = v022


* check the deonimnator
tab child_age
tab child_age [iw = wgt]

tab child_age_2, m
tab child_age_2 [iw = wgt]

tab child_age_3, m
tab child_age_3 [iw = wgt]


*----------------------------------------------------------------------------------
** (2) - PREPARE THE COVARIATES FOR ANALYSIS **
*----------------------------------------------------------------------------------

** (A.1) Geographical Location **
// create dummies var for each state/region var
tab v024, m // hv024
tab v024, m nolab

gen state_region_name = v024
lab def state_region_name ///
1 "Kachin" ///
2 "Kayah" ///
3 "Kayin" ///
4 "Chin" ///
5 "Sagaing" ///
6 "Taninthayi" ///
7 "Bago" ///
8 "Magway" ///
9 "Mandalay" ///
10 "Mon" ///
11 "Rakhine" ///
12 "Yangon" ///
13 "Shan" ///
14 "Ayeyarwaddy" ///
15 "Naypyitaw"
lab val state_region_name state_region_name

forvalues x = 1/15 {
	egen region_`x' = anymatch(v024), values(`x')
	replace region_`x' = .n if mi(v024) 
	tab region_`x', m
}

rename region_1 region_kachin
rename region_2 region_kayah
rename region_3 region_kayin
rename region_4 region_chin
rename region_5 region_sagaing
rename region_6 region_taninthayi
rename region_7 region_bago
rename region_8 region_magway
rename region_9 region_mandalay
rename region_10 region_mon
rename region_11 region_rakhine
rename region_12 region_yangon
rename region_13 region_shan
rename region_14 region_ayeyarwaddy
rename region_15 region_naypyitaw

// create dummies var to identify state/region
gen state_region = 0
lab def state_region 1"State" 0"Region"
foreach num of numlist 1/4 10/11 13 {
	replace state_region = 1 if v024 == `num'
	replace state_region = .n if mi(v024) 
	lab val state_region state_region
	tab state_region, m

}

rename v024 region

// create dummies var to identify rural/urban
gen rural_urban 	= (v025 == 1) //hv025 in pr file , used v140 got different result with dhs report
//replace rural_urban = .n if v025 == 7
lab def rural_urban 1"Urban" 0"Rural"
lab val rural_urban rural_urban
tab rural_urban, m


** (A.2) Socio-Economic Status **
// create dummies var to identify respondent native language
gen native_language		= (v045c == 6) // hv045c
//replace native_language = 0 if v045c == 1
lab def native 0"Myanmar" 1"Other"
lab val native_language native
tab native_language, m

// create dummies var for wealth index
tab v190, m // hv270
tab v190, nolab m // wealth index

gen wealth_index = v190
lab var wealth_index "HH Wealth Index"
lab def wealth_index 1"Poorest" 2"Poorer" 3"Middle" 4"Richer" 5"Richest"
lab val wealth_index wealth_index

forvalues x = 1/5 {
	egen wi_`x' = anymatch(v190), values(`x')
	replace wi_`x' = .n if mi(v190) 
	tab wi_`x', m
}
rename wi_1 index_poorest
rename wi_2 index_poorer
rename wi_3 index_middle
rename wi_4 index_richer
rename wi_5 index_richest


// create dummies var for mother education level
tab v106, m // hc61
tab v106, nolab m

gen mom_edu = v106
//replace mom_edu = .d if v106 == 8
//replace mom_edu = .m if v106 == 9
lab var mom_edu "Mother Education Level - highest attained"
lab def mom_edu 0"No education" 1"Primary" 2"Secondary" 3"Higher"
lab val mom_edu mom_edu
tab mom_edu, m

forvalues x = 0/3 {
	egen mom_edu_`x' = anymatch(mom_edu), values(`x')
	replace mom_edu_`x' = .n if mi(mom_edu) 
	tab mom_edu_`x', m
}
rename mom_edu_0 mom_edu_no
rename mom_edu_1 mom_edu_primary
rename mom_edu_2 mom_edu_secondary
rename mom_edu_3 mom_edu_higher


** (A.3) Child Related Info **
// create dummies var for birth interval 
tab b11, m // hc63
recode b11		(1/23 = 1 "<24")(24/47 = 2 "24-47")(48/226 = 3 "48+") ///
				(999 = .d)(.n = 0 "First Birth"), gen(birth_interval)
tab birth_interval, m

forvalues x = 0/3 {
	egen birth_interval_`x' = anymatch(birth_interval), values(`x')
	replace birth_interval_`x' = .n if mi(birth_interval) 
	tab birth_interval_`x', m
}
rename birth_interval_0 binter_first
rename birth_interval_1 binter_less_24
rename birth_interval_2 binter_24_47
rename birth_interval_3 binter_more47

// create dummies var for child sex 
gen child_sex		= b4 //hc27
replace child_sex	= 0 if b4 == 2
lab def child_sex 1"Male" 0"Female"
lab val child_sex child_sex
tab child_sex,m



** (B) - BREASTFEEDING STATUS INDICATOR CONSTRUCTION ** 
* Breastfeeding status.

gen water = 0
gen liquids = 0
gen milk = 0
gen solids = 0
gen breast = 0

* Water
replace water = 1 if (v409 >= 1 & v409 <= 7) 
               
* Other non-milk liquids
* check for country specific liquids
foreach xvar of varlist v409a v410 v410a v412c v413* {
	replace liquids=1 if `xvar'>=1 & `xvar'<=7
}
                         
* Powdered or tinned milk, formula, fresh milk
foreach xvar of varlist v411 v411a v412 {
	replace milk=1 if `xvar' >= 1 & `xvar' <= 7
}

* Solid food
* check for country specific foods
foreach xvar of varlist v412a v412b v414* {
	replace solids=1 if `xvar' >= 1 & `xvar' <= 7
}

* Still breastfeeding
replace breast=1 if m4 == 95 

tab1 water liquids milk solids breast

* Generate column variable used in table 11.3
gen feeding = 1
replace feeding = 2 if water == 1 
replace feeding = 3 if liquids == 1
replace feeding = 4 if milk == 1
replace feeding = 5 if solids == 1 
replace feeding = 0 if breast == 0 
tab feeding,m

label define feeding ///
  0 "Not breastfeeding" 1 "Exclusive breastfeeding" ///
  2 "+Water" 3 "+Liquids" ///
  4 "+Other Milk" 5 "+Solids"
label val feeding feeding
  
tab child_age feeding [iweight = wgt], row

tab child_age_2 feeding [iweight = wgt], row

tab feeding [iweight = wgt]


** (C) MINIMUM DIETARY DIVERSITY
*-------------------------------------------------------------------------------
** Minimum dietary diversity 6-23 months **
// indicator def - WHO ;
// Proportion of children 6 to 23 months of age who receive foods from 4 or more food groups.
// Children 6 to 23 months of age who received foods from =4 food groups during the previous day

** DHS def;
// +4
// 1 Food groups: a. infant formula, milk other than breast milk, cheese or yogurt or other milk products; 
// b. foods made from grains, roots, and tubers, including porridge and fortified baby food from grains; 
// c. vitamin A-rich fruits and vegetables (and red palm oil); d. other fruits and vegetables; e. eggs; 
// f. meat, poultry, fish, and shellfish (and organ meats); g. legumes and nuts
// weblink - https://userforum.dhsprogram.com/index.php?t=msg&goto=10664&&srch=food+group#msg_10664
*-------------------------------------------------------------------------------

gen	childdiet_rice		= v414e
gen childdiet_potatoes	= v414f
gen	childdiet_foodgram	= v414o
gen childdiet_milk		= v414p	 // v414v
gen childdiet_liver		= v414m	
gen childdiet_beef		= v414h	
gen childdiet_fishfresh	= v414n	
//rename childdiet_fishdried // not specify fish as fresh and dried in dhs data
gen childdiet_eggs		= v414g	
gen childdiet_pumpkin	= v414i	
gen childdiet_veggreen	= v414j	
gen childdiet_orange	= v414k	
//rename childdiet_vegoth // not specify as other veg and other fruit in dhs data
gen childdiet_fruits	= v414l	
gen childdiet_yogurt	= v414v	 // included as separate item in dhs
gen childdiet_formula	= v411a  // include in dds calculation in dhs 
gen childdiet_oth_milk	= v411	 // include in dds calculation in dhs  
//gen childdiet_oth_milk	= v412b  // include in dds calculation in dhs 
gen childdiet_cerelac	= v412a // include in dds calculation in dhs 

foreach var of varlist childdiet_* {
	tab `var', m
	tab `var', m nolab
	replace `var' = 0 if `var' == 8 // in dhs treat don't know as 0 
}

gen childdiet_fg_grains = (childdiet_rice == 1 | childdiet_potatoes == 1 | childdiet_cerelac == 1)
replace childdiet_fg_grains = .n if mi(childdiet_rice) & mi(childdiet_potatoes) & mi(childdiet_cerelac)
lab val childdiet_fg_grains yesno
lab var childdiet_fg_grains "Grain - child diet recall food group"
tab childdiet_fg_grains,m

gen childdiet_fg_pulses=childdiet_foodgram
lab val childdiet_fg_pulses yesno
lab var childdiet_fg_pulses "Pulses & Nut - child diet recall food group"
tab childdiet_fg_pulses,m

gen childdiet_fg_diary = (childdiet_milk == 1 | childdiet_yogurt == 1 | childdiet_formula == 1 | childdiet_oth_milk == 1)
replace childdiet_fg_diary = .n if mi(childdiet_milk) & mi(childdiet_yogurt) & mi(childdiet_formula) & mi(childdiet_oth_milk)
lab val childdiet_fg_diary yesno
lab var childdiet_fg_diary "Diary - child diet recall food group"
tab childdiet_fg_diary,m

gen childdiet_fg_meat = (childdiet_liver == 1 | childdiet_beef == 1 | childdiet_fishfresh == 1 /*| childdiet_fishdried == 1*/)
replace childdiet_fg_meat = .n if mi(childdiet_liver) & mi(childdiet_beef) & mi(childdiet_fishfresh) /*& mi(childdiet_fishdried)*/
lab val childdiet_fg_meat yesno
lab var childdiet_fg_meat "Meat & Fishes - child diet recall food group"
tab childdiet_fg_meat,m

gen childdiet_fg_eggs = childdiet_eggs
lab val childdiet_fg_eggs yesno
lab var childdiet_fg_eggs "Eggs - child diet recall food group"
tab childdiet_fg_eggs,m

gen childdiet_fg_vit_vegfruit = (childdiet_pumpkin == 1 | childdiet_veggreen == 1 | childdiet_orange == 1)
replace childdiet_fg_vit_vegfruit = .n if mi(childdiet_pumpkin) & mi(childdiet_veggreen) & mi(childdiet_orange)
lab val childdiet_fg_vit_vegfruit yesno
lab var childdiet_fg_vit_vegfruit "Vit riched Vegetable & Fruits - child diet recall food group"
tab childdiet_fg_vit_vegfruit,m

gen childdiet_fg_vegfruit_oth = (/*childdiet_vegoth == 1 | */childdiet_fruits == 1)
replace childdiet_fg_vegfruit_oth = .n if /*mi(childdiet_vegoth) &*/ mi(childdiet_fruits)
lab val childdiet_fg_vegfruit_oth yesno
lab var childdiet_fg_vegfruit_oth "Other Vegetable & Fruits - child diet recall food group"
tab childdiet_fg_vegfruit_oth,m

foreach var of varlist childdiet_fg_grains-childdiet_fg_vegfruit_oth {
	replace `var'= .n if child_valid_age < 6 | mi(child_valid_age)
	tab `var',m
}

egen childdiet_dds = rowtotal(childdiet_fg_grains childdiet_fg_pulses childdiet_fg_diary childdiet_fg_meat ///
childdiet_fg_eggs childdiet_fg_vit_vegfruit childdiet_fg_vegfruit_oth), missing
replace childdiet_dds = .n if mi(childdiet_fg_grains) & mi(childdiet_fg_pulses) & mi(childdiet_fg_diary) & ///
mi(childdiet_fg_meat) & mi(childdiet_fg_eggs) & mi(childdiet_fg_vit_vegfruit) & mi(childdiet_fg_vegfruit_oth) // 
replace childdiet_dds = .n if mi(child_valid_age) | child_valid_age < 6
lab var childdiet_dds "Mean Child Dietary Diversity Score - 24 hours diet recall"
tab childdiet_dds,m

gen childdiet_min_ddds = (childdiet_dds >= 4 & childdiet_dds != .n & child_valid_age >= 6 & child_valid_age < 24)
replace childdiet_min_ddds = .n if /*mi(childdiet_dds) |*/ child_valid_age < 6 | mi(child_valid_age)
lab val childdiet_min_ddds yesno
lab var childdiet_min_ddds "% of  children received Minimum Dietary Diversity Score 6-23 months"
tab childdiet_min_ddds,m

tab childdiet_min_ddds [iw = wgt]
tab child_age_3 childdiet_min_ddds [iw = wgt], row
mean childdiet_dds [iw = wgt]

recode m4 (95 = 1 "Currently Breastfeed") (93/94 = 0 "Currently Not Breastfeed"), gen(child_breastfeed_now)
gen childdiet_solidfoodnum = m39
replace childdiet_solidfoodnum = .d if m39 == 8
replace childdiet_solidfoodnum = .m if m39 == 9

tab child_age_3 childdiet_min_ddds [iw = wgt] if child_breastfeed_now == 1, row 
tab child_age_3 childdiet_min_ddds [iw = wgt] if child_breastfeed_now == 0, row 


** (D) MINIMUM MEAL FREQUENCY 
*-------------------------------------------------------------------------------
**Minimum meal frequency**
**indicator def - WHO;
//Proportion of breastfed and non-breastfed children 6-23 months of age 
//who receive solid, semi-solid, or soft foods (but also including milk feeds for non-breastfed children) the minimum number of times or more.
//BF children: Breastfed children 6-23 months of age who received solid, semi-solid or soft foods
//the minimum number of times or more during the previous day
//Non BF children: Non-breastfed children 6-23 months of age who received solid, semi-solid or soft foods or
//milk feeds the minimum number of times or more during the previous day
//Minimum is defined as:
//2 times for breastfed infants 6-8 months
//3 times for breastfed children 9-23 months
//4 times for non-breastfed children 6-23 months

** DHS def 
// For breastfed children, minimum meal frequency is receiving solid or semisolid food
// at least twice a day for infants age 6-8 months and at least three times a day for children age 9-23 months.
// For nonbreastfed children age 6-23 months, minimum meal frequency is receiving solid or semisolid food or milk feeds at least four times a day
// weblink - https://userforum.dhsprogram.com/index.php?t=msg&th=5913&start=0&
*-------------------------------------------------------------------------------

tab child_age_3 child_breastfeed_now [iw = wgt], row

** Breastfeeding Children **
gen childdiet_68_bf_minfreq=(child_valid_age>=6 & child_valid_age<9 & child_breastfeed_now==1 & childdiet_solidfoodnum>=2 & !mi(childdiet_solidfoodnum))
//replace childdiet_68_bf_minfreq=.d if child_breastfeed_now==.d | childdiet_solidfoodnum==.d 
replace childdiet_68_bf_minfreq=.n if child_breastfeed_now!=1 | /*mi(childdiet_solidfoodnum) |*/ mi(child_breastfeed_now) | mi(child_valid_age)  // in dhs treat don't knwo and missing case as "0" meal frequency time
replace childdiet_68_bf_minfreq=.n if child_valid_age<6 | child_valid_age>=9
lab val childdiet_68_bf_minfreq yesno
lab var childdiet_68_bf_minfreq "% of  breastfeeding children received Minimum Meal Frequency 6-8 months"
tab childdiet_68_bf_minfreq,m
 
gen childdiet_923_bf_minfreq=(child_valid_age>=9 & child_valid_age<24 & child_breastfeed_now==1 & childdiet_solidfoodnum>=3 & !mi(childdiet_solidfoodnum))
//replace childdiet_923_bf_minfreq=.d if child_breastfeed_now==.d | childdiet_solidfoodnum==.d 
replace childdiet_923_bf_minfreq=.n if child_breastfeed_now!=1 | /*mi(childdiet_solidfoodnum) |*/ mi(child_breastfeed_now) | mi(child_valid_age) // in dhs treat don't knwo and missing case as "0" meal frequency time
replace childdiet_923_bf_minfreq=.n if child_valid_age<9 | child_valid_age>=24
lab val childdiet_923_bf_minfreq yesno
lab var childdiet_923_bf_minfreq "% of  breastfeeding children received Minimum Meal Frequency 9-23 months"
tab childdiet_923_bf_minfreq,m
 
gen childdiet_bf_minfreq=(childdiet_68_bf_minfreq==1 | childdiet_923_bf_minfreq==1)
replace childdiet_bf_minfreq=.n if mi(childdiet_68_bf_minfreq) & mi(childdiet_923_bf_minfreq)
replace childdiet_bf_minfreq=.d if childdiet_68_bf_minfreq==.n & childdiet_923_bf_minfreq==.d
lab val childdiet_bf_minfreq yesno
lab var childdiet_bf_minfreq "% of  breastfeeding children received Minimum Meal Frequency 6-23 months"
tab childdiet_bf_minfreq,m

tab child_age_3 childdiet_bf_minfreq [iw = wgt], row


** Non-Breastfeeding Children **
tab child_age_3 child_breastfeed_now [iw = wgt], row

tab v469e, m  
tab v469f, m
tab v469x, m

gen child_liquid_powdmilk_num 	= v469e 
gen child_liquid_formula_num	= v469f 
gen child_liquid_yogurt_num		= v469x

foreach var of varlist child_liquid_powdmilk_num child_liquid_formula_num child_liquid_yogurt_num {
	//replace `var' = .d if `var' == 8
	replace `var' = .m if `var' == 9

}

egen childdiet_formulacf_num=rowtotal(childdiet_solidfoodnum child_liquid_powdmilk_num child_liquid_formula_num /*child_liquid_freshmilk_num*/ ///
/*child_liquid_goatmilk_num child_liquid_evapmilk_num child_liquid_condmilk_num child_liquid_milktea_num*/ child_liquid_yogurt_num), missing
//replace childdiet_formulacf_num=.n if /*mi(childdiet_solidfoodnum) &*/ mi(child_liquid_powdmilk_num) & mi(child_liquid_formula_num) & ///
/*mi(child_liquid_freshmilk_num) & mi(child_liquid_goatmilk_num) & mi(child_liquid_evapmilk_num) & mi(child_liquid_condmilk_num) & mi(child_liquid_milktea_num)*/ ///
//mi(child_liquid_yogurt_num)
lab var childdiet_formulacf_num "% of  formula feeding and complementary feeding times for non-breastfeeding children"
tab childdiet_formulacf_num,m

gen childdiet_nobf_minfreq=(child_valid_age>=6 & child_valid_age<24 & child_breastfeed_now==0 & childdiet_formulacf_num>=4 & !mi(childdiet_formulacf_num))
replace childdiet_nobf_minfreq=.n if /*mi(childdiet_formulacf_num) |*/ child_breastfeed_now!=0 | child_valid_age<6 | child_valid_age>=24
lab val childdiet_nobf_minfreq yesno
lab var childdiet_nobf_minfreq "% of  non-breastfeeding children received Minimum Meal Frequency 6-23 months"
tab childdiet_nobf_minfreq,m

tab child_age_3 childdiet_nobf_minfreq [iw = wgt], row

** For Both Breastfeed or non-breastfeed Children 6 - 23 months **
gen childdiet_min_mealfreq=(childdiet_bf_minfreq==1 | childdiet_nobf_minfreq==1)
replace childdiet_min_mealfreq=.n if mi(childdiet_bf_minfreq) & mi(childdiet_nobf_minfreq)
lab val childdiet_min_mealfreq yesno
lab var childdiet_min_mealfreq "% of  children received Minimum Meal Frequency 6-23 months"
tab childdiet_min_mealfreq,m

tab child_age_3 childdiet_min_mealfreq [iw = wgt], row

*-------------------------------------------------------------------------------
**Minimum acceptable diet**
**Indicator def - WHO;
//Proportion of children 6 to 23 months of age who receive a minimum acceptable diet (apart from breast milk).
//Breastfed children 6 to 23 months of age who had at least the minimum dietary diversity
//and the minimum meal frequency during the previous day
//Non-breastfed children 6 to 23 months of age who received at least 2 milk feedings and 
//had at least the minimum dietary diversity not including milk feeds and the minimum meal frequency during the previous day
//It is recommended that the indicator be further disaggregated and reported for the following age groups: 
//6-1 months, 1-ֱ7 months and 1-ֲ3 months of age, if sample size permits.

** DHS def:
// Nonbreastfed children age 6-23 months are considered to be fed with a minimum standard of three infant and young child feeding practices 
// if they receive other milk or milk products at least twice a day, receive the minimum meal frequency, and 
// receive solid or semisolid foods from at least four food groups not including the milk or milk products food group.
*-------------------------------------------------------------------------------

** Breastfeeding Children **
gen childdiet_bf_min_acceptdiet=(/*child_breastfeed_now==1 & */childdiet_min_ddds==1 & childdiet_min_mealfreq==1)
replace childdiet_bf_min_acceptdiet=.n if child_breastfeed_now!=1 //| mi(childdiet_min_ddds) | mi(childdiet_bf_minfreq) 
lab val childdiet_bf_min_acceptdiet yesno
lab var childdiet_bf_min_acceptdiet "% of  breastfeeding children received Minimum Acceptable Diet 6-23 months"
tab childdiet_bf_min_acceptdiet,m

tab child_age_3 childdiet_bf_min_acceptdiet [iw = wgt], row

** Non-Breastfeeding Children **
egen childdiet_nomilk_dds=rowtotal(childdiet_fg_grains childdiet_fg_pulses childdiet_fg_meat ///
childdiet_fg_eggs childdiet_fg_vit_vegfruit childdiet_fg_vegfruit_oth),missing
replace childdiet_nomilk_dds=.n if mi(childdiet_fg_grains) & mi(childdiet_fg_pulses) & mi(childdiet_fg_diary) & ///
mi(childdiet_fg_meat) & mi(childdiet_fg_eggs) & mi(childdiet_fg_vit_vegfruit) & mi(childdiet_fg_vegfruit_oth)
lab var childdiet_nomilk_dds "Child Non-milk Dietary Diversity Score - 24 hours diet recall"
tab childdiet_nomilk_dds,m


egen childdiet_formula_num=rowtotal(childdiet_milk child_liquid_powdmilk_num child_liquid_formula_num child_liquid_yogurt_num), missing
//replace childdiet_formula_num=.n if mi(child_liquid_powdmilk_num) & mi(child_liquid_formula_num) & mi(child_liquid_freshmilk_num) & ///
//mi(child_liquid_goatmilk_num) & mi(child_liquid_evapmilk_num) & mi(child_liquid_condmilk_num) & mi(child_liquid_milktea_num)
lab var childdiet_formula_num "% of  formula or milk only feeding times for non-breastfeeding children"
tab childdiet_formula_num,m

gen childdiet_nobf_min_acceptdiet=(/*child_breastfeed_now==0 & */childdiet_formulacf_num>=4 & childdiet_formula_num>=2 & childdiet_nomilk_dds>=4)
replace childdiet_nobf_min_acceptdiet=.n if child_breastfeed_now!=0 //| mi(childdiet_formulacf_num) | mi(childdiet_formula_num) | mi(childdiet_nomilk_dds)
lab val childdiet_nobf_min_acceptdiet yesno
lab var childdiet_nobf_min_acceptdiet "% of  non-breastfeeding children received Minimum Acceptable Diet 6-23 months"
tab childdiet_nobf_min_acceptdiet,m

tab child_age_3 childdiet_nobf_min_acceptdiet [iw = wgt], row

** For Both Breastfeed or non-breastfeed Children 6 - 23 months **
gen childdiet_min_acceptdiet=(childdiet_bf_min_acceptdiet==1 | childdiet_nobf_min_acceptdiet==1)
replace childdiet_min_acceptdiet=.n if mi(childdiet_bf_min_acceptdiet) & mi(childdiet_nobf_min_acceptdiet)
lab val childdiet_min_acceptdiet yesno
lab var childdiet_min_acceptdiet "% of  children received Minimum Acceptable Diet 6-23 months"
tab childdiet_min_acceptdiet,m

tab child_age_3 childdiet_min_acceptdiet [iw = wgt], row

*-------------------------------------------------------------------------------
**Consumption of iron-rich or iron-fortified foods**
**indicator def;
//Proportion of children 6-23 months of age who receive an iron-rich food or 
//iron-fortified food that is specially designed for infants and young children, or that is fortified in the home.
//Children 6-23 months of age who received an iron-rich food
//or a food that was specially designed for infants and young children and was fortified with iron,
//or a food that was fortified in the home with a product that included iron during the previous day
*-------------------------------------------------------------------------------

gen childdiet_iron_consum=(childdiet_fg_meat==1 | childdiet_fg_eggs == 1 /*child_valid_age>=6 & child_valid_age<24*/)
replace childdiet_iron_consum=.n if /*mi(childdiet_fg_meat) | */child_valid_age<6 | child_valid_age>=24
lab val childdiet_iron_consum yesno
lab var childdiet_iron_consum "% of  children received iron riched food 6-23 months"
tab childdiet_iron_consum,m

tab child_age_3 childdiet_iron_consum [iw = wgt], row

** (E) INFANT & CHILD FEEDING INDEX (ICFI) 
*-------------------------------------------------------------------------------
/*
                                Age-group (months)
                 ----------------------------------
                   6 - 8      9 - 11     12 - 23   
                 -------   ---------   ---------     
    Breastfed    Yes  +2   Yes    +2   Yes    +1   
                 -------   ---------   ---------    
    Food groups  == 1 +1   1 or 2 +1   2 or 3 +1   
                 >= 2 +2   >= 3   +2   >=4    +2   
                 -------   ---------   ---------     
    Meals        == 1 +1   1 or 2 +1   ==2    +1   
                 >= 2 +2   >=3    +2   ==3    +2   
                                       >=4    +3   
                 -------   ---------   ---------     
using 7 food groups score for "Food Groups" indicators	

Table 2 : The principal IYCF indicator
Classification
Age				Good					Not good
< 6 months		Exclusively breastfed	Not exclusively breastfed
Older children	ICFI = 6				ICFI < 6
*/			 
// weblink - https://www.ennonline.net//fex/47/iycf
*-------------------------------------------------------------------------------

// create dummies var for exclusive BF 0-5 months.
tab feeding, m
gen child_ebf 		= (feeding == 1 & child_valid_age <6 )
replace child_ebf 	= .n if mi(feeding) | child_valid_age > 5
lab var child_ebf "Exclusively Breastfeed Children 0-5 months"
tab child_ebf, m

tab child_ebf [iw = wgt]


// create score var for currently breastfeeding.
gen child_bfeed_6to8		= 2 if child_valid_age < 9 & child_valid_age > 5 & child_breastfeed_now == 1
replace child_bfeed_6to8 	= 0 if child_valid_age < 9 & child_valid_age > 5 & child_breastfeed_now != 1 
replace child_bfeed_6to8 	= .n if child_valid_age < 6 | child_valid_age >8
replace child_bfeed_6to8 	= .n if mi(child_breastfeed_now) 

gen child_bfeed_9to11		= 2 if child_valid_age < 12 & child_valid_age > 8 & child_breastfeed_now == 1
replace child_bfeed_9to11 	= 0 if child_valid_age < 12 & child_valid_age > 8 & child_breastfeed_now != 1 
replace child_bfeed_9to11 	= .n if child_valid_age < 9 | child_valid_age > 11
replace child_bfeed_9to11 	= .n if mi(child_breastfeed_now)

gen child_bfeed_12to23		= (child_valid_age < 24 & child_valid_age > 11 & child_breastfeed_now == 1)
replace child_bfeed_12to23 	= .n if mi(child_breastfeed_now) | child_valid_age < 12

egen child_bfeed_6to23		= rowtotal(child_bfeed_6to8 child_bfeed_9to11 child_bfeed_12to23), missing
replace child_bfeed_6to23	= .n if mi(child_bfeed_6to8) & mi(child_bfeed_9to11) & mi(child_bfeed_12to23)
tab child_bfeed_6to23, m

// create score var for food groups score.
foreach var of varlist	childdiet_fg_grains childdiet_fg_pulses childdiet_fg_diary ///
						childdiet_fg_meat childdiet_fg_eggs childdiet_fg_vit_vegfruit ///
						childdiet_fg_vegfruit_oth {
tab `var', m
}
tab childdiet_dds, m

gen child_fg_6to8		= (child_valid_age < 9 & child_valid_age > 5 & childdiet_dds == 1)
replace child_fg_6to8 	= 2 if child_valid_age < 9 & child_valid_age > 5 & childdiet_dds > 1 & !mi(childdiet_dds)
replace child_fg_6to8 	= .n if mi(childdiet_dds)

gen child_fg_9to11		= (child_valid_age < 12 & child_valid_age > 8 & childdiet_dds < 3 & childdiet_dds != 0)
replace child_fg_9to11 	= 2 if child_valid_age < 12 & child_valid_age > 8 & childdiet_dds > 2 & !mi(childdiet_dds)
replace child_fg_9to11 	= .n if mi(childdiet_dds)

gen child_fg_12to23		= (child_valid_age < 24 & child_valid_age > 11 & childdiet_dds < 4 & childdiet_dds > 1)
replace child_fg_12to23 = 2 if child_valid_age < 24 & child_valid_age > 11 & childdiet_dds > 3 & !mi(childdiet_dds)
replace child_fg_12to23 = .n if mi(childdiet_dds)

egen child_fg_6to23		= rowtotal(child_fg_6to8 child_fg_9to11 child_fg_12to23), missing
replace child_fg_6to23	= .n if mi(child_fg_6to8) & mi(child_fg_9to11) & mi(child_fg_12to23)
tab child_fg_6to23, m


// create score var for meals frequency.
tab childdiet_solidfoodnum, m

gen child_freq_6to8			= (child_valid_age < 9 & child_valid_age > 5 & childdiet_solidfoodnum == 1)
replace child_freq_6to8 	= 2 if child_valid_age < 9 & child_valid_age > 5 & childdiet_solidfoodnum > 1 & !mi(childdiet_solidfoodnum)
replace child_freq_6to8 	= .n if mi(childdiet_solidfoodnum)

gen child_freq_9to11		= (child_valid_age < 12 & child_valid_age > 8 & childdiet_solidfoodnum < 3 & childdiet_solidfoodnum != 0)
replace child_freq_9to11 	= 2 if child_valid_age < 12 & child_valid_age > 8 & childdiet_solidfoodnum > 2 & !mi(childdiet_solidfoodnum)
replace child_freq_9to11 	= .n if mi(childdiet_solidfoodnum)

gen child_freq_12to23		= (child_valid_age < 24 & child_valid_age > 11 & childdiet_solidfoodnum == 2)
replace child_freq_12to23 	= 2 if child_valid_age < 24 & child_valid_age > 11 & childdiet_solidfoodnum == 3
replace child_freq_12to23 	= 3 if child_valid_age < 24 & child_valid_age > 11 & childdiet_solidfoodnum > 3 & !mi(childdiet_solidfoodnum)
replace child_freq_12to23 	= .n if mi(childdiet_solidfoodnum)

egen child_freq_6to23		= rowtotal(child_freq_6to8 child_freq_9to11 child_freq_12to23), missing
replace child_freq_6to23	= .n if mi(child_freq_6to8) & mi(child_freq_9to11) & mi(child_freq_12to23)
tab child_freq_6to23, m


// create icfi score for  6-23 months children
egen icfi_score = rowtotal(child_bfeed_6to23 child_fg_6to23 child_freq_6to23), missing
replace icfi_score = .n if mi(child_bfeed_6to23) & mi(child_fg_6to23) & mi(child_freq_6to23)
replace icfi_score = .n if child_valid_age < 6
tab icfi_score, m


// create dummies var for good iycf indicator
gen iycf_good = (child_ebf == 1 | icfi_score == 6)
replace iycf_good = .n if mi(child_ebf) & mi(icfi_score)
tab iycf_good, m

tab child_age iycf_good [iw = wgt], row

