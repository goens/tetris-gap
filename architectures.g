#Imports:
LoadPackage("grape");

##############################
# function GenerateMesh
# Input:
#   n size of mesh
# Output: 
#   Dihederal Group on n^2 points
#
#    1       2       3  ...  n
#   n+1     n+2     n+3 ... 2n
#   ...
#  n(n-1)+1 n(n-1)+2    ... n^2
#
##############################
 
# n - m should be even
GenerateMeshRotation := function(n,m)
    local rot, d,off;
    d := (n-m)/2;
    rot := [];
    rot := Concatenation( rot, ([d*n+d+1, d*n+d+2 .. d*n+d+m-1]));
    rot := Concatenation( rot, ([d*n+d+m,(d+1)*n+d+m..(d+m-2)*n+d+m]));
    rot := Concatenation( rot, ([(d+m-1)*n+d+m,(d+m-1)*n+d+m-1 .. (d+m-1)*n+d+2]));
    rot := Concatenation( rot,([(d+m-1)*n+d+1,(d+m-2)*n+d+1..(d+1)*n+d+1]));
    #Print(rot);
    return rot;
end;
    
GenerateMesh := function(n)
    local rot, ref, nhalf, i,j;
    rot := [];
    if IsOddInt(n) then
        nhalf := Int(Floor(Float(n/2)));
        for i in [1..nhalf] do
            #Print(i);
            #Print(": ");
            Append(rot, [GenerateMeshRotation(n,2*i+1)]);
            #Print("\n");
        od;
    else
        nhalf := n/2;
        for i in [1..nhalf] do
            Append(rot, [GenerateMeshRotation(n,2*i)]);
        od;
    fi;
    return rot;
end;

MeshCheckRelation := function(n)
    local f;
    f := function(i,j)
        local xi,yi,xj,yj;
        xi := ((i - 1) mod n) + 1;
        xj := ((j - 1) mod n) + 1;
        yi := (i - xi)/n;
        yj := (j - xj)/n;
        
        if (AbsInt(xi - xj) = 1) and (yi = yj) then
            return true;
        fi;
        if (AbsInt(yi - yj) = 1) and (xi = xj) then
            return true;
        fi;
        return false;
    end;
    return f;
end;


GenerateMeshGraph := function(n)
    local f,L,G;
    G := Group([()]);
    L := [1..n*n];
    f := MeshCheckRelation(n);
    return Graph(G,L,OnPoints,f);
end;

InvertNames := function(names)
    local res,i;
    res := [1..Length(names)];
    for i in [1..Length(names)] do
        res[names[i]] := i;
    od;
    return res;
end;


HAECPlane := function()
    local G, periodic, temp, res, invnames, i,j, edges, n;
    periodic=true;
    n := 4;
    temp := GenerateMeshGraph(n);
    edges := [];
    G := AutGroupGraph(temp);
    for i in [1..Length(temp.representatives)] do
        for j in [1..Length(temp.adjacencies[i])] do
            Append(edges, [[temp.representatives[i],temp.adjacencies[i][j]]]);
        od;
    od;
    
    #perodic boundary conditions
    if periodic = true then
        for i in [1..n] do
            Append(edges, [[i,n*(n-1)+i]]);
            Append(edges, [[n*(i-1)+1,n*(i-1)+n]]);
        od;
    fi;

    res:= EdgeOrbitsGraph(G,edges,n*n);
    return res;
end;

HAECBox := function()

    
                   
    
# Test: GenerateMesh(4):
#
#  1  2  3  4
#  5  6  7  8
#  9  10 11 12
#  13 14 15 16
#
# (1,2,3,4,8,12,16,15,14,13,9,5)(6,7,11,10)

      
# Test: GenerateMesh(5):
#
#  1  2  3  4  5
#  6  7  8  9  10
#  11 12 13 14 15
#  16 17 18 19 20
#  21 22 23 24 25
#
# (1,2,3,4,5,10,15,20,25,24,23,22,21,16,11,6)(7,8,9,14,19,18,17,12)

