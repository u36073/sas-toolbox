%macro tm_bin_nominal(data=,
                      depvar=,
                      binvar=,
                      min_bin_size=.01,                                            
                      );

%let null=;

proc contents data=&data out=_contents noprint;
run;

data _null_;
   set _contents; 
   if strip(upcase(name))=strip(upcase(binvar)) then do;
      call symput('bin_var_type',strip(type));
      end;      
   run;
   
proc sql;
create _tmbn_vw1 as
select &binvar,
       count(*) as n,
       sum(&binvar.) as sum,
       mean(&binvar.) as mean,
from &data
group by 1




                      
%mend;                     