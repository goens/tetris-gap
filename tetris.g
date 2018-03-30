# Copyright: 2017 Andres Goens
#

###############################
# function Overlap
# Input:
#   s1  - set 1 
#   s2  - set 2
#
# Output:
#   int - the overlap between 
#         both sets
###############################
Overlap := function(s1, s2)
    local inters;
    inters := Intersection(s1,s2);
    return Length(inters);
end;
                     
###############################
# function trueWithProb
# Input:
#   p  -  probability 
#
# Output:
#   bool - true with
#          probability p
###############################
TrueWithProb := function(p)
    local num, rand;
    rand := Random(1,10^4);
    num := Int(p * 10^4);
    if rand <= num then
        return true;
    else
        return false;
    fi;
end;

###############################
# function PlayTetris
# Input:
#   G Group G acting on M
#   X ocupied subset of M
#   m point in M
#   options - dict with options,
#            *maxNumIter: max
#             no. of iterations
#            *probToAdd: probability
#             to add a new elem
#             if overlap is bigger
#
# Output:
#   gm with overlap(gm,X) = 0
#   or empty if none found
#
#------------------------------
# The basic idea as follows:
# 1. take an element m in G-set
# 2. apply all gens s_i to m
# 3. if the overlap is better,
#    add s_i m to workset.
# 4. otherwise, only add it 
#    with a given probability.
# 5. continue until max number 
#    of iterations, or found 
#    one with overlap 0, or 
#    worklist is empty.
###############################
PlayTetris := function(G,X,m, options)
    local workSet, ms, gens, lastOver,
          over, iterCount, maxIter, w,
          ws, gen, probAdd;
    
    gens := GeneratorsOfGroup(G);
    workSet := [ m ];
    if IsBound( options.maxNumIter ) then
        maxIter := options.maxNumIter;
    else
        maxIter := 100;
    fi;
    if IsBound( options.probToAdd ) then
        probAdd := options.probToAdd;
    else
        probAdd := 0.3;
    fi;
    iterCount := 0;
    
    #main loop
    for w in workSet do
        iterCount := iterCount + 1;
        lastOver := Overlap(X,w);

        for gen in gens do
            ws := OnTuples(w , gen);
            over := Overlap(X,ws);
            if Overlap(X,ws) = 0 then
                return ws;
            fi;
            if Overlap(X,ws) <= lastOver then
                Add(workSet,ws);
            else
                if TrueWithProb(probAdd) then
                    Add(workSet,ws);
                fi;
            fi;
            
                
       
        od;
        if iterCount >= maxIter then
            Print("Warning, maximum num.  of iterations reached  on playTetris");      
            return;
        fi;
        
    od;
end;

###############################
# function NaiveTetris
# Input:
#   G Group G acting on M
#   X ocupied subset of M
#   m point in M
#
# Output:
#   gm with overlap(gm,X) = 0
#   or empty if none found
#
#------------------------------
# Naive version: calculates the
# whole orbit and returns one
# without overlap, if it exists.
###############################
NaiveTetris := function(G,x,m)
    local orb, overlaps, elem;
    orb := Orbit(G,m,OnTuples);
    for elem in orb do
        if Overlap(x,elem) = 0 then
            return elem;
        fi;
    od;
    return;
end;

###############################
# function BruteForceTetris
# Input:
#   G Group G acting on M
#   X ocupied subset of M
#   m point in M
#
# Output:
#   gm with overlap(gm,X) = 0
#   or empty if none found
#
#------------------------------
# Brute-force version: calculates
# the elements one by one and
# checks if they are in the orbit
# if they have no overlap
###############################
BruteForceTetris := function(G,x,m)
    local S,orb, overlaps, elem;
    S := SymmetricGroup( LargestMovedPoint(G));
    orb := Orbit(G,m,OnTuples);
    for elem in orb do
        if Overlap(x,elem) = 0 then
            return elem;
        fi;
    od;
    return;
end;
        
    
#example:
points := [1..15];
# G := SymmetricGroup(15);
G := Group( (1,2,3,4),(1,4)(2,3),(5,6,7,8),(5,8)(6,7),(9,10,11,12),(9,12)(10,11),(13,14,15,16),(13,16)(14,15),(1,9)(2,10)(4,12)(3,11)(5,13)(8,16)(6,14)(7,15));
x := [3,5,7,10,12,16];
m := [2,4,5,6];
Print(Runtimes());
resHeuristic := PlayTetris(G,x,m,rec());
Print(Runtimes());
resNaive := NaiveTetris(G,x,m);
Print(Runtimes());
resBrute := BruteForceTetris(G,x,m);
Print(Runtimes());

#example 2:



points := [1..15];
# G := SymmetricGroup(15);
ArchKeystone := DirectProduct( SymmetricGroup(8), SymmetricGroup(4));
gens := GeneratorsOfGroup(DirectProduct( ArchKeystone, ArchKeystone));
elem :=(1,13)(2,14)(3,15)(4,16)(5,17)(6,18)(7,19)(8,20)(9,21)(10,22)(11,23)(12,24);
gens := UnionSet(gens,[elem] );
TwoKeystones := Group(gens );




x := [3,5,7,10,12,16];
m := [2,4,5,6];
Print(Runtimes());
Print("\n--- TETRiS --- \n");
resHeuristic := PlayTetris(G,x,m,rec());
Print(Runtimes());
Print("\n--- Naive --- \n");
resNaive := NaiveTetris(G,x,m);
Print(Runtimes());
