%macro include_macros(incmacdir);
%local null; 
%let null=;

%if %quote(&incmacdir)=%quote(&null) %then %do;
	%let incmacdir=X:\AppData\sas-toolbox\macros;
	%end;
	
%let workdir=%sysfunc(getoption(work));

data _null_;
	length rn $ 7 fn $ 1024;
	flag='N';
	do until(flag='Y');
		rn=put(int(9999999*ranuni(-1))+1,z7.);
		fn=cats("&workdir.\incmac",rn,".sas");
		if fileexist(fn)=0 then flag='Y';
		end;
	call symput('incmac_code_file',strip(fn));
	run;
	
data _incmacfiles;
	keep filename;	
	length fref $8 filename $512 _fn $512;
 	rc = filename(fref, "&incmacdir.");
 	if rc = 0 then do;
 		did = dopen(fref);
 		rc = filename(fref);
 		end;
 	else do;
 		length msg $200.;
 		msg = sysmsg();
 		put msg=;
 		did = .;
 		end;
 	if did <= 0  then do; 
 		putlog "ERROR:  Unable to open include macros directory  ";
 		putlog "        &incmacdir.";
 		file "&incmac_code_file" lrecl=1024;
 		put "/*********************************************************";
 		put;
 		put "ERROR:  Unable to open include macros directory.";
 		put;
 		put "        &incmac_code_file";
 		put;
 		put "*********************************************************/";
 		end;
 	else do;
 		file "&incmac_code_file" lrecl=1024;
 		put;
	 	dnum = dnum(did);
	 	do i = 1 to dnum;
	 		filename = dread(did, i);
	 		/* If this entry is a file, then output. */
	 		fid = mopen(did, filename);	 	
	 		if fid > 0 then do;	 					 			 		
	 			if strip(upcase(filename)) ne 'INCLUDE_MACROS.SAS' and length(filename)>4 then do;
	 				_fn=strip(upcase(filename));
	 				if substr(_fn,length(_fn)-2,3)="SAS" then do;
	 					put '%include "' "&incmacdir.\" filename +(-1) '";';	 			
	 					end;
	 				end;
	 			end;
	 		end;
	 	rc = dclose(did);
	 	end;
 run;	
 
 %include "&incmac_code_file.";
	
%mend;