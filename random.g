# Copyright: 2018 Andres Goens
#

###############################
# function RandomMapping
# Input:
#   n  - total number of pes
#   k  - number of pes to be occupied
#   src - random source
#
# Output:
#   list - a random mapping
#        (subset of size k of n) 
###############################
RandomMapping := function(src,n,k)
    local elems,res,nprime,rand,i;
    
    nprime := n;
    elems := [1..n];
    res := [];
    for  i in [1..k] do
        rand := Random(src,1,nprime);
        Add(res,elems[rand]);
        Remove(elems,rand);
        nprime := nprime - 1;
    od;
    return res;
end;

###############################
# function TestRandomMappings
# Input:
#   r  - total number of mappings
#   G  - AutomorphismGroup of graph
#   u  - utilization percentage
#
# Output:
#  none 
###############################
TestRandomMappings := function(r,G,u,brute)
    local n,s,k,m,x,points,i,usertime,
    resHeuristic, resBrute, resNaive,
    tetrisFail, naiveFail, bruteFail,
    tetrisTime, naiveTime, bruteTime,
    seed, randomSource, randomTime;
    
    n :=  LargestMovedPoint(G);
    s := Int(n * u);
    k := Minimum(3,n-s);
    points := [1..n];
    naiveFail := 0;
    bruteFail := 0;
    tetrisFail := 0;

    ClearProfile();
    Print("\n\n --- Test with Group ");
    Print(G);
    Print(", with utilization: ");
    Print(u);
    Print("--- \n");
    usertime := Runtimes().user_time;

    seed := State(GlobalMersenneTwister);
    randomSource := RandomSource(IsMersenneTwister);
    Reset(randomSource, seed);
    for i in [1..r] do
        x := RandomMapping(randomSource,n,s);
        m := RandomMapping(randomSource,n,k);
        #Print("\n");
        #Print(m);
        resHeuristic := PlayTetris(G,x,m,rec());
        if resHeuristic = [] then
            tetrisFail := tetrisFail + 1;
        fi;
        
    od;
    tetrisTime := Runtimes().user_time - usertime; 
    usertime := Runtimes().user_time;
    
    Reset(randomSource, seed);
    for i in [1..r] do
        x := RandomMapping(randomSource,n,s);
        m := RandomMapping(randomSource,n,k);
        #Print("\n");
        #Print(m);
        resNaive := NaiveTetris(G,x,m);
        if resNaive = [] then
            naiveFail := naiveFail + 1;
        fi;
        
    od;
    naiveTime := Runtimes().user_time - usertime; 
    usertime := Runtimes().user_time;

    if brute = true then
        Reset(randomSource, seed);
        for i in [1..r] do
            x := RandomMapping(randomSource,n,s);
            m := RandomMapping(randomSource,n,k);
            #Print("\n");
            #Print(m);
            resBrute := BruteForceTetris(G,x,m);
            if resBrute = [] then
                bruteFail := bruteFail + 1;
            fi;
            
        od;
        bruteTime := Runtimes().user_time - usertime; 
        usertime := Runtimes().user_time;
    fi;
    

    Reset(randomSource, seed);
    for i in [1..r] do
        x := RandomMapping(randomSource,n,s);
        m := RandomMapping(randomSource,n,k);
    od;
    randomTime := Runtimes().user_time - usertime; 
    
    tetrisTime := tetrisTime - randomTime;
    naiveTime := naiveTime - randomTime;
    if brute = true then
        bruteTime := bruteTime - randomTime;
    fi;
    
    Print("\n--- TETRiS --- \n");
    Print("Time: ");
    Print(tetrisTime);
    Print(", Failed: ");
    Print(tetrisFail);
    Print("\n--- Naive --- \n");
    Print("Time: ");
    Print(naiveTime);
    Print(", Failed: ");
    Print(naiveFail);
    if brute = true then
        Print("\n--- Brute --- \n");
        Print("Time: ");
        Print(bruteTime);
        Print(", Failed: ");
        Print(bruteFail);
    fi;
end;

    
