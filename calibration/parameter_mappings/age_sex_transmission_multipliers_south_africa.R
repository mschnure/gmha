# South African HIV Behavioural, Sero-status and Media (SABSSM) surveys
# 2012: South Africa National HIV Prevalence, Incidence and Behaviour Survey: 
# https://hsrc.ac.za/uploads/pageContent/4565/SABSSM%20IV%20LEO%20final.pdf
# multiple sexual partners, figure 3.8
# condom use last sex, figure 3.10

## HI Allison!


## Hi Melissa! 

# @Allison: Just to orient yourself to what you are building below, run this line:
data = data.frame(
    ages = c("15-24","25-49","50+"),
    prop.multiple.partners=c(.230,.115,.075), # values come from the bar graph in figure 3.8, year 2002, male
    condom.use.last.sex = c(.571,.267,.082)) # values come from the bar graph in figure 3.10, year 2002, male

# then look at the "data" dataframe: 
data

# now, fill in the below function with all the dataframes we need 
get.transmission.multipliers.SABSSM  = function(sex,
                                                year){
    if(sex=="male"){
        if(year==2002){
            data = data.frame(
                ages = c("15-24","25-49","50+"),
                prop.multiple.partners=c(.230,.115,.075), 
                condom.use.last.sex = c(.571,.267,.082)) 
        } else if(year==2005){
            # fill in a similar data = data.frame... section for year 2005, male
            
            
        } else if(year==2008){
            # same for 2008, male
            
        } else if(year==2012){
            # same for 2012, male
            
        } else stop("data for given year not available") # this statement stops the function if we try to access a year for which data are not available 
    } else if(sex=="female"){
        if(year==2002){
            
            # fill in the same as what you did above, but now for female 
            
        } else if(year==2005){
            
        } else if(year==2008){
            
        } else if(year==2012){
            
        } else stop("data for given year not available")
    } else stop ("sex must be male or female")
    
    data$prop.at.risk = data$prop.multiple.partners*(1-data$condom.use.last.sex)
    data$multiplier = data$prop.at.risk/(data$prop.at.risk[data$ages=="25-49"])
    
    # "rv" stands for "return value" - what we want the function to return when we call it 
    rv = array(data$multiplier,
               dim = length(data$ages),
               dimnames = list(data$ages))
    
    # a function in R will always return the last line of code - so here, the function will return a call to the rv array we made
    rv 
}

# Once you've filled out the above sections, run the line above where you define the function (line 17), and then try calling the function yourself and see what you get
# this will call the function and return the values for male, 2002 (which I filled in), but you can try out other sexes/years
get.transmission.multipliers.SABSSM(sex = "male",
                                    year = "2002")

