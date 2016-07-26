%macro include_dir_files(incdir,extension);
%local null; 
%let null=;

%if %quote(&incdir)=%quote(&null) %then %do;
	%let incdir=X:\AppData\sas-toolbox\macros;
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
 	rc = filename(fref, "&incdir.");
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
 		putlog "        &incdir.";
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
	 			if strip(upcase(filename)) ne 'INCLUDE_DIR_FILES.SAS' and length(filename)>4 then do;
	 				_fn=strip(upcase(filename));
	 				if substr(_fn,length(_fn)-2,3)="%qupcase(&extension)" then do;
	 					put '%include "' "&incdir.\" filename +(-1) '";';	 			
	 					end;
	 				end;
	 			end;
	 		end;
	 	rc = dclose(did);
	 	end;
 run;	
 
 %include "&incmac_code_file.";
 
 filename __icf "&incmac_code_file.";
 %let rc=%sysfunc(fdelete(__icf));
	
%mend;