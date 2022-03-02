# UML-Gerrymandering

1. expanding test suite

    a. Work through nan issues in computation of different metrics
    
    - [x] Analyze maps that yield nan values
    - [x] Reimplement anything in the base redist package? (not neeeded it appears)
        
    b. Confidence intervals for kl divergence metrics, kde density estimation
    
    - [ ] Succeed in implementing a discrete KL approximation (completely unstable for now)
    - [ ] Or: compute confidence interval from KDE density estimation
        
2. getting more technical in the discussion of our algorithm and thinking about how we can beef that up, 

    a. Improve python code via any optimizations possible
    
    - [ ] Optimize dissimilarity matrix computation (vectorization or in-place modification)
    - [ ] Find a way to remove the deepcopies at each step
    
    b. Write in algorithmic package and provide algorithm runtime
    
    - [ ] Add comments
    
    c. Practical and theoretical (if any) results
    
    - [ ] State space being reachable?
    - [ ] reworking the intro to get rid of some fluff, 
    
3. expanding related literature section, 

4. Ask about publishing and what we need to improve… other stuff

5. EAAMO Specifics

    - [ ] Read through formatting and citation guidelines and implement them
    
    - [ ] Read through past years papers to guide relevant editing and problem motivation
    
