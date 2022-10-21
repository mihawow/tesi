# tesi codice cplex
range range range range float float
c=1..9;
Sol =1..500; Treni =1..4000; Fermate =1..10;
Pa[Treni][Fermate]=...; Ar[Treni][Fermate]=...;
float Ce[Fermate][Fermate]=...; float Cb[Fermate][Fermate]=...; float Cg[Fermate][Fermate]=...; int random[i in Sol]=rand(11); int t P=...;
int P=...;
int t A=...;
int A=...;
int s=1;
float tratta[Sol][c];
string percorso[Sol ][1..2]; int Visited [ Fermate ] ;
int reach [ Fermate ] [ Fermate ] ; int dong=0;
dvar float x[i in 1..4]; execute{
for(var i in Sol){ tratta [ i ][1]=10000000;
tratta [ i ][2]=10000000; tratta [ i ][3]=10000000; tratta [ i ][4]=10000000; tratta [ i ][5]=10000000; tratta [ i ][6]=10000000; tratta [ i ][7]=10000000;
tratta [ i ][8]=10000000;
}
for (var f in Fermate) Visited[f]=0; for (var f1 in Fermate) {
for (var f2 in Fermate) {
reach [ f1 ] [ f2 ]=0;
}
}
function pathfind(i , j){ Visited [ i ]=1;
for (var t in Treni) {
if (Pa[t][i]>=tP&&Ar[t][j]>Pa[t][i]&&Ar[t][j]<=tA){
t r a t t a [ s ] [ 1 ] = Pa [ t ] [ i ] ;
t r a t t a [ s ] [ 2 ] = Ar [ t ] [ j ] ;
t r a t t a [ s ] [ 3 ] = Ce [ i ] [ j ] ;
t r a t t a [ s ] [ 4 ] = Cb [ i ] [ j ] ;
t r a t t a [ s ] [ 5 ] = Cg [ i ] [ j ] ;
t r a t t a [ s ] [ 6 ] = Ar [ t ] [ j ]−Pa [ t ] [ i ] ;
 
tratta[s][7]=Pa[t][i]−t P; tratta[s][8]=0;
percorso [ s ][1]= i+”−>”+j ;
percorso [ s ][2]= t ; reach [ i ] [ j ]=Pa [ t ] [ i ] ; if (i==P&& j==A) { s=s+1;
}
}
for (var f in Fermate) {
if (f!=j && Pa[t][i] >=t P && Ar[t][f]>=Pa[t][i] && Ar[t][f]<=t A
&& Visited [ f]==0){
pathfind(f , j);
if (reach[f][j]>0 && Ar[t][f]<=reach[f][j]){ dong =1;
t r a t t a [ s ] [ 1 ] = Pa [ t ] [ i ] ;
t r a t t a [ s ] [ 3 ] = t r a t t a [ s ] [ 3 ] + Ce [ i ] [ f ] ;
t r a t t a [ s ] [ 4 ] = t r a t t a [ s ] [ 4 ] + Cb [ i ] [ f ] ;
t r a t t a [ s ] [ 5 ] = t r a t t a [ s ] [ 5 ] + Cg [ i ] [ f ] ;
t r a t t a [ s ] [ 6 ] = t r a t t a [ s ] [ 6 ] + Ar [ t ] [ f ]−Pa [ t ] [ i ] ; tratta[s][7]=tratta[s][7]+Pa[t][i]−t P;
tratta[s][8]=tratta[s][8]+1;
percorso [ s ][1]= i+”−>”+percorso [ s ] [ 1 ] ; percorso[s][2]=t+”, ”+percorso[s][2]; reach [ i ] [ j ]=Pa [ t ] [ i ] ;
if (i==P&& j==A) {
s=s+1;
} } } } }
}
pathfind(P, A); s=s−1
for(i in Sol){
i f ( i <=s ) {
tratta [ i ][9]=random[ i ]
} else{
tratta [ i ][9]=0 }
}
}
minimize sum(i in 1..s ) abs(x[1]∗tratta[i][3] + x[2]∗tratta[i ][6]∗15/60 + x[3]∗ tratta [ i ][7]∗4/60 + 12∗x[4]∗ t
