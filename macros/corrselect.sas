
%macro corrselect(data=,
								 by=,
	               filename=,
	               depvar=,
	               vars=,
	               NumToSelect=200
	               );

%let null=;

%if %qupcase(&filename)=%quote(&null) %then %let filename=log;   

%let by=%ListDedupe(&by);
%let nbv=%ParseList(bv,&by);      

proc contents data=&data(keep=&vars) noprint out=_contents;
run;

data _null_;
	retain nvc 0;
	set _contents end=last;
	if type=1 then do;
		nvc=nvc+1;
	  call symputx('var'||trim(left(nvc)),name);
	  end;
	if last then do;
		call symputx('nvars',put(nvc,best10.));
	end;
run;

%if &by ne &null %then %do;
	proc sort data=&data;
	by &by;
  run;
	%end;
	
proc corr data=&data pearson noprint outp=_corr;
%if &by ne &null %then %do;
	by &by;
	%end;
var %do i=1 %to &nvars; &&var&i %end;;
with &depvar;
run;

data _corr2;
	set _corr(where=(_type_='CORR'));
	_name_='CORR';
run;

%if &by ne &null %then %do;
	proc sort data=_corr2;
	by &by;
  run;
	%end;

proc transpose data=_corr2 out=_corr3;
%if &by ne &null %then %do;
	by &by;
	%end;
run;

data _corr4;	
	set _corr3;
	corr_abs=abs(corr);
run;

proc sort data=_corr4;
%if &by ne &null %then %do;
	by &by descending corr_abs;
	%end;
%else %do;
  by corr_abs;
  %end;
run;

data _corr5(keep=_name_);
set _corr4;
retain _vr;
drop _vr;
%if &by ne &null %then %do;
	by %do i=1 %to &nbv; &&bv&i %end;;  
	if first.&&bv&nbv then do;
		 _vr=0;
		end;
	%end;
_vr=_vr+1;

if _vr<=&NumToSelect then output;
else delete;
run;

proc sort data=_corr5 nodupkey;
by _name_;
run;

data _null_;
	file &filename;
	set _corr5 end=last;
	if _n_=1 then do;
		put '%let SelectedVars=';
	end;
	put '   ' _name_;
	if last then do;
		put '   ;';
	end;
run;

%mend;


      