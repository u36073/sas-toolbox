/******************************************************************************
 Macro: tm_sasname

 Parameters: Positional
   x		SAS Variable Name

 This macro determines if the supplied entry is a valid sas variable name.
 It returns a value of 0 or 1

 0		Invalid SAS Variable name
 1		Valid SAS Variable Name
 
 Example:
 
 %let myvar=bad#sas_variable_name;
 
 %if %tm_sasname(&myvar)=1 %then %do;
    %end;
******************************************************************************/

%macro tm_sasname(x);
	%local null rv x i ValidFirstChar ValidNonFirstChar;
	%let null=;
	%let rv=1;
	%let x=%upcase(trim(left(x)));
	%if x=&null %then %do;
		%let rv=0;
		%end;
	%else %do;
		%do i=1 %to %length(x);
			%let ValidFirstChar=0;
			%let ValidNonFirstChar=0;
			%if %qsubstr(&x,&i,1)=%quote(_) or
             %qsubstr(&x,&I,1)=%quote(A) or
             %qsubstr(&x,&I,1)=%quote(B) or
             %qsubstr(&x,&I,1)=%quote(C) or
             %qsubstr(&x,&I,1)=%quote(D) or
             %qsubstr(&x,&I,1)=%quote(E) or
             %qsubstr(&x,&I,1)=%quote(F) or
             %qsubstr(&x,&I,1)=%quote(G) or
             %qsubstr(&x,&I,1)=%quote(H) or
             %qsubstr(&x,&I,1)=%quote(I) or
             %qsubstr(&x,&I,1)=%quote(J) or
             %qsubstr(&x,&I,1)=%quote(K) or
             %qsubstr(&x,&I,1)=%quote(L) or
             %qsubstr(&x,&I,1)=%quote(M) or
             %qsubstr(&x,&I,1)=%quote(N) or
             %qsubstr(&x,&I,1)=%quote(O) or
             %qsubstr(&x,&I,1)=%quote(P) or
             %qsubstr(&x,&I,1)=%quote(Q) or
             %qsubstr(&x,&I,1)=%quote(R) or
             %qsubstr(&x,&I,1)=%quote(S) or
             %qsubstr(&x,&I,1)=%quote(T) or
             %qsubstr(&x,&I,1)=%quote(U) or
             %qsubstr(&x,&I,1)=%quote(V) or
             %qsubstr(&x,&I,1)=%quote(W) or
             %qsubstr(&x,&I,1)=%quote(X) or
             %qsubstr(&x,&I,1)=%quote(Y) or
             %qsubstr(&x,&I,1)=%quote(Z)
         %then %let ValidFirstChar=1;
         %if &ValidFirstChar=1 or (
             %qsubstr(&x,&I,1)=%quote(0) or
             %qsubstr(&x,&I,1)=%quote(1) or
             %qsubstr(&x,&I,1)=%quote(2) or
             %qsubstr(&x,&I,1)=%quote(3) or
             %qsubstr(&x,&I,1)=%quote(4) or
             %qsubstr(&x,&I,1)=%quote(5) or
             %qsubstr(&x,&I,1)=%quote(6) or
             %qsubstr(&x,&I,1)=%quote(7) or
             %qsubstr(&x,&I,1)=%quote(8) or
             %qsubstr(&x,&I,1)=%quote(9))
         %then %let ValidNonFirstChar=1;
         %if &i=1 and &ValidFirstChar=0 %then %let rv=0;
         %if &i>1 and &ValidNonFirstChar=0 %then %let rv=0;
			%end;
		%end;
&rv
%mend;