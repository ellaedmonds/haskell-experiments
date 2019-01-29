module PTfunctions where
quadraticVertex a b c = -b/(2*a)
quadraticDiscriminant a b c = (b**2) - 4*a*c
quadraticRealSolutionQty a b c = if (quadraticDiscriminant a b c) >= 0
    then if (quadraticDiscriminant a b c) == 0
            then print("There is one Solution")
        else print("There are two Solutions")
    else print("There are no Solutions")
convertFtoC f = (f - 32)/1.8
convertCtoF c = (1.8*c) + 32
convertTemp t s = if s == "c"
    then convertCtoF t
else convertFtoC t