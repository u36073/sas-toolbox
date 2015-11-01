%Macro quicksort (
 Arr = 							/* Parallel array name list */
,By = % QScan(&Arr,1,%Str( )) 	/* Key array name */
,Seq = A 						/* Seq=D for descending */
,LB = Lbound(&By) 				/* Lower bound to sort */
,HB = Hbound(&By) 				/* Upper bound to sort */
,M = 9 							/* Tuning range: (1:15) */
);
%Local _ H I J L N P Q S T W;

%Macro Sw (I,J);
%Local W;
Do;
%Do W = 1 %To &N;
&T&W = &&A&W(&I);
&&A&W(&I) = &&A&W(&J);
&&A&W(&J) = &T&W ;
%End;
End;
%Mend Sw;

%If %Upcase(&Seq) = %Upcase(A) %Then %Let Q = G;
%Else %Let Q = L;

%Do %Until (&&A&N EQ );
%Let N = %Eval(&N+1);
%Local A&N;
%Let A&N = %Scan(&Arr,&N,%Str( ));
%End;
%Let N = %Eval(&N-1);

%Let _ = %Substr(%Sysfunc(Ranuni(0)),3,
%Eval(7-%Length(&N)+5*(%Substr(&Sysver,1,1) GT 6)));

%Let H = H&_; %Let I = I&_; %Let J = J&_; %Let L = L&_;
%Let P = P&_; %Let S = S&_; %Let T = T&_; %Let Z = Z&_;

Array &Z (0:1, 0:50) _Temporary_;
&L = &LB; &H = &HB;

If &H-&L GT &M Then Do &S=1 By 0 While (&S);
&J = (&H-&L)/3; &I = &L+&J; &J = &I+&J;
If &By(&L) &Q.T &By(&I) Then %Sw(&L,&I);
If &By(&I) &Q.T &By(&J) Then %Sw(&I,&J);
If &By(&J) &Q.T &By(&H) Then %Sw(&J,&H);
If &By(&L) &Q.T &By(&I) Then %Sw(&L,&I);
If &By(&I) &Q.T &By(&J) Then %Sw(&I,&J);
If &By(&L) &Q.T &By(&I) Then %Sw(&L,&I);

%If &M LE 3 %Then %Do;
If &H-&L LE 3 Then Do;
&L = &Z(0,&S); &H = &Z(1,&S); &S+-1;
Continue;
End;
%End;

%Sw(&L,&I); &P = &By(&L); &I = &L;
Do &J=&H+1 By 0;
Do &I=&I+1 By +1 Until(&By(&I) &Q.E &P); End;
Do &J=&J-1 By -1 Until(&P &Q.E &By(&J)); End;
If &I GE &J Then Leave;
%Sw(&I,&J);
End;
%Sw(&L,&J);

If &H-&J GE &J-&L GT &M Then Do &S = &S+1;
&Z(0,&S) = &J+1; &Z(1,&S) = &H; &H = &J-1;
End;
Else If &J-&L GE &H-&J GT &M Then Do &S = &S+1;
&Z(0,&S) = &L; &Z(1,&S) = &J-1; &L = &J+1;
End;
Else If &J-&L GT &M GE &H-&J Then &H = &J-1;
Else If &H-&J GT &M GE &J-&L Then &L = &J+1;
Else Do;
&L = &Z(0,&S); &H = &Z(1,&S); &S+-1;
End;
End;
%mend; 
