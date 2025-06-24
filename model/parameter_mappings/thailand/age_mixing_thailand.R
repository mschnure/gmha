source("helpers/pairing_helpers.R") # Todd wrote these for JHEEM 


# Smith, 2023; Hong Kong age mixing study:
# https://journals.lww.com/stdjournal/fulltext/2023/01000/age_based_mixing_and_condom_use_patterns_in.5.aspx

# male to female is from the female's point of view (female ages; partner ages)
get.male.to.female.age.model.thailand = function(){
    # Step 1: Define age group bounds
    age_bounds <- c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, Inf)
    
    # Step 2: Compute male and female age distributions from normal curve
    male_probs <- diff(pnorm(age_bounds, mean = 37.3, sd = 10.7))
    female_probs <- diff(pnorm(age_bounds, mean = 36.7, sd = 9.4))
    
    # Step 3: Compute expected matrix under independence
    expected_matrix <- outer(male_probs, female_probs)  # 9x9 matrix
    
    # Step 4: Relative matrix (same as before)
    relative_matrix <- matrix(c(
        5.95, 1.08, 0.71, 0.40, 0.24, 0.00, 0.00, 0.00, 0.00,
        2.90, 4.86, 0.77, 0.46, 0.53, 0.53, 0.00, 0.00, 0.00,
        0.00, 2.22, 2.94, 1.00, 0.61, 0.77, 0.00, 0.00, 0.00,
        0.54, 1.21, 2.52, 4.43, 0.59, 0.82, 0.00, 0.00, 0.00,
        0.00, 0.16, 0.76, 3.96, 4.74, 0.65, 0.00, 0.00, 0.00,
        0.00, 0.24, 0.20, 0.14, 3.05, 4.46, 1.43, 0.00, 0.00,
        0.53, 0.13, 0.14, 0.85, 1.71, 1.99, 0.00, 0.00, 0.00,
        0.00, 0.12, 0.00, 0.23, 1.16, 4.74, 4.26, 0.00, 0.00,
        0.00, 0.19, 0.12, 0.17, 0.31, 0.00, 0.70, 2.40, 3.88
    ), nrow = 9, byrow = TRUE)
    
    # Step 5: Multiply relative × expected to get estimated observed
    observed_matrix <- relative_matrix * expected_matrix
    
    # Step 6: Normalize to get proportions
    pairing_proportions <- observed_matrix / sum(observed_matrix)
    
    # Step 7: Add labels
    rownames(pairing_proportions) <- c("<25", "25–29", "30–34", "35–39", "40–44", "45–49", "50–54", "55–59", "60+")
    colnames(pairing_proportions) <- c("<25", "25–29", "30–34", "35–39", "40–44", "45–49", "50–54", "55–59", "60+")
    
    # Step 8: View matrix in % terms
    round(100 * pairing_proportions, 2)
    pairing_proportions = t(pairing_proportions) # female age is the row, partner age is the column 
    pairing_proportions.per.age = pairing_proportions/rowSums(pairing_proportions)
    
    age.counts = rowSums(pairing_proportions*1000)
    age.cutoffs = c(15,25,30,35,40,45,50,55,60,75)
    
    expanded.data = suppressWarnings(matrix.to.scatter(t(pairing_proportions.per.age*age.counts),
                                      age.cutoffs = age.cutoffs) )
    
    age.model = fit.age.model(age.of.reference = expanded.data$age.to, # to is always the reference
                              age.of.partner = expanded.data$age.from) 
    
    rv = list(mean.age.diff.intercept = age.model["mean.intercept"],
              mean.age.diff.slope = age.model["mean.slope"],
              sd.age.diff.intercept = age.model["sd.intercept"],
              sd.age.diff.slope = age.model["sd.slope"])
    
    rv
    
}


get.female.to.male.age.model.thailand = function(){
    # Step 1: Define age group bounds
    age_bounds <- c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, Inf)
    
    # Step 2: Compute male and female age distributions from normal curve
    male_probs <- diff(pnorm(age_bounds, mean = 37.3, sd = 10.7))
    female_probs <- diff(pnorm(age_bounds, mean = 36.7, sd = 9.4))
    
    # Step 3: Compute expected matrix under independence
    expected_matrix <- outer(male_probs, female_probs)  # 9x9 matrix
    
    # Step 4: Relative matrix (same as before)
    relative_matrix <- matrix(c(
        9.47, 3.58, 2.78, 0.46, 0.43, 0.00, 1.88, 1.29, 0.00,
        0.57, 5.39, 4.53, 1.97, 1.12, 0.85, 0.00, 0.96, 0.82,
        0.00, 0.50, 2.03, 3.05, 1.42, 1.65, 0.36, 1.56, 0.00,
        0.00, 0.60, 0.40, 2.83, 3.77, 0.00, 0.00, 0.00, 0.82,
        0.00, 0.00, 0.00, 0.51, 1.14, 2.65, 1.81, 1.03, 1.95,
        0.00, 0.00, 0.00, 0.00, 0.24, 2.74, 3.51, 0.00, 1.74,
        0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.40, 3.31, 2.42,
        0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.89, 1.34,
        0.16, 0.12, 0.10, 0.35, 0.84, 0.85, 0.49, 0.00, 0.61
    ), nrow = 9, byrow = TRUE)
    
    # Step 5: Multiply relative × expected to get estimated observed
    observed_matrix <- relative_matrix * expected_matrix
    
    # Step 6: Normalize to get proportions
    pairing_proportions <- observed_matrix / sum(observed_matrix)
    
    # Step 7: Add labels
    rownames(pairing_proportions) <- c("<25", "25–29", "30–34", "35–39", "40–44", "45–49", "50–54", "55–59", "60+")
    colnames(pairing_proportions) <- c("<25", "25–29", "30–34", "35–39", "40–44", "45–49", "50–54", "55–59", "60+")
    
    # Step 8: View matrix in % terms
    #round(100 * pairing_proportions, 2)
    
    pairing_proportions = t(pairing_proportions) # female age is the row, partner age is the column 
    pairing_proportions.per.age = pairing_proportions/rowSums(pairing_proportions)
    
    age.counts = rowSums(pairing_proportions*1000)
    age.cutoffs = c(15,25,30,35,40,45,50,55,60,75)
    
    expanded.data = suppressWarnings(matrix.to.scatter(t(pairing_proportions.per.age*age.counts),
                                                       age.cutoffs = age.cutoffs) )
    
    age.model = fit.age.model(age.of.reference = expanded.data$age.to, # to is always the reference
                              age.of.partner = expanded.data$age.from) 
    
    rv = list(mean.age.diff.intercept = age.model["mean.intercept"],
              mean.age.diff.slope = age.model["mean.slope"],
              sd.age.diff.intercept = age.model["sd.intercept"],
              sd.age.diff.slope = age.model["sd.slope"])
    
    rv
}
