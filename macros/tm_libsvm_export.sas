%macro tm_libsvm_export(data=,
                        out_spec_data=,
                        depvar=,
                        keyvars=,
                        out_dir=,
                        out_prefix=
                        );
   
%let null=;

libname __d "&out_dir.";
filename __f "&out_dir.\&out_prefix..libvsm.txt" lrecl=32767;
			 
proc contents data=&data out=_contents noprint;
run;

data _contents;
   set _contents;
   if strip(upcase(name)) ne strip(upcase("&depvar."));
   if type=1; 
   if indexw(strip(upcase("&keyvars.")),strip(upcase(name)))=0;   
   name=strip(upcase(name));
   run;
   
proc sort data=_contents;
by name;
run;
   
data __d.&out_prefix.libsvm_cmap(keep=name libsvm_column_index);
	set _contents end=last;  	
	by name;
	call symput(cats('var',_n_),strip(lowcase(name)));
	libsvm_column_index=_n_;
	if last then do;
		call symput('nvars',_n_);
		end;
	run;
	
%if %qupcase(&out_spec_data.) ne %quote(&null) %then %do;
   data &out_spec_data.;
      length role $ 32;
      set __d.&out_prefix.libsvm_cmap(rename=(name=variable) end=last;
      role = "FEATURE";
      output;
      if last then do;
         role = "TARGET";
         variable = lowcase("&depvar.");
         libsvm_column_index=.;
         output;
         end;
      run;      
   %end;

%if %quote(keyvars.) ne %quote(&null.) %then %do;	
   data __d.&out_prefix.libsvm_kmap(keep=&keyvars. libsvm_row_number);
      set &data;
      libsvm_row_number=_n_;
      run;
   %end;

data _null_;
   set &data;   
   file __f;
   
   length pv $ 32;
   
   pv=strip(put(&depvar.,best32.9));
   put pv @;
   
   %do i=1 %to &nvars;
      if &&var&i not in (.,0) then do;
         pv=strip(put(&&var&i..,best32.9));
         put +(-1) " &i.:" pv @;
         end;
      %end;
   put;
   run; 
   
libname __d clear;
filename __f clear;                       
%mend;