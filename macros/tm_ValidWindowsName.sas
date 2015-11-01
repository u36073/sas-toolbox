/******************************************************************************
 Macro: tm_ValidWindowsName

 Parameters: Positional
   x		Windows Filename

 This macro determines if the supplied entry is a valid windows file name without the path infomration.
 It returns a value of 0 or 1

 0		Invalid Windows Filename
 1		Valid Windows Filename
******************************************************************************/

%macro tm_ValidWindowsName(x);
	%local null rv x;
	%let null=;
	%let rv=1;
	%let x=%upcase(trim(left(&x)));
	%if %quote(&x) = %quote(&null) %then %do;
		%let rv=0;
		%end;
	%else %do;
		%do i=1 %to %length(&x);
			%if %bquote(%substr(&x,&i,1))=%bquote(\) or
			    %bquote(%substr(&x,&i,1))=%bquote(/) or
			    %bquote(%substr(&x,&i,1))=%bquote(:) or
			    %bquote(%substr(&x,&i,1))=%bquote(*) or
			    %bquote(%substr(&x,&i,1))=%bquote(?) or
			    %bquote(%substr(&x,&i,1))=%bquote(") or
			    %bquote(%substr(&x,&i,1))=%bquote(") or
			    %bquote(%substr(&x,&i,1))=%bquote(<) or
			    %bquote(%substr(&x,&i,1))=%bquote(>) or
			    %bquote(%substr(&x,&i,1))=%bquote(|)
			%then %let rv=0;
			%end;
		%end;
	%str(&rv)
%mend;