%macro tm_csv_import(file=,
                     out=,
                     delimiter=%bquote(,),
                     max_length=256,
                     header=y                     
                    );

filename _tmci "&file.";

data _null_;
	infile _tmci;
	input @1 x $1.;
	
	length word $ 64;
	i=0; 
	do until (word='');
		i+1;
		word=scan(_infile_,i,',');
		if word ne '' then do;
			call symput(cats('var',i),strip(compress(word,'"')));
			end;
		end;
		
	call symput('nvars',i-1);
	stop;
	run;
	
data _tmci1(compress=yes);
	infile _tmci dsd delimiter=',' missover firstobs=2;
	
	%do i=1 %to &nvars;
		length &&var&i $ &max_length.;
		input &&var&i $ @;
		%if &i.=1 %then %do;
			if strip(upcase(&&var&i..)) ne strip(upcase("&&var&i.."));
			%end;
		%end;
	run;
	
proc sql ;
create table _max as 
select %do i=1 %to &nvars;
			max(length(&&var&i..)) as length&i,
			%end;
		count(*) as nobs
from _tmci1
;quit;

data _null_;
	set _max;
	%do i=1 %to &nvars;
		call symput("length&i.",strip(length&i.));
		%end;
	run;
	
proc sql;
create table _char as
select %do i=1 %to &nvars;
			%if &&length&i..<=32 %then %do;
				max(case when input(&&var&i..,? &&length&i...)=. and &&var&i.. not in ('.','') then 1 else 0 end) as charvar&i,
				%end;
			%else %do;
				max(1) as charvar&i.,
				%end;
			%end;
		count(*) as nobs
from _tmci1
;quit;

data _null_;
	set _char;
	%do i=1 %to &nvars;
		if charvar&i.=1 then call symput("type&i.",'2');
		else call symput("type&i.",'1');
		%end;
	run;
	
data &out.(compress=yes);
	infile _tmci dsd delimiter=',' missover firstobs=2;
	
	%do i=1 %to &nvars;
		%if &&type&i.=1 %then %do;
			length &&var&i.. 8;
			input &&var&i.. @;
			%end;
		%else %do;
			length &&var&i.. $ &&length&i..;
			input &&var&i.. $ @;
			%end;
		%end;
	run;
	
%mend;
	


                    
                    
                                     
                     