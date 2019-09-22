%macro tm_import_delimited_file(
       file=,
       out=,
       delimiter=%quote(,),
       cr_lf_in_fields=y,
       file_max_record_length=65000,
       guessingrows=1000,
       tempdir=     
       );

%local i null;
%let null=;

%if %quote(&tempdir.)=%quote(&null) %then %let tempdir=%sysfunc(pathname(work));

%if %qupcase(&cr_lf_in_fields)=%quote(Y) %then %do;

    filename _tidftmp "&tempdir.\tm_import_delimite_file-macro-temp-file.txt" recfm=n;
      
    data _null_;
    	
        infile "&file" recfm=n eof=end_of_file;
                
        length current_char next_char repA repD $ 1;
        
        retain first_character 1 open 0 current_char prev1_char prev2_char prev3_char repA repD '' 
        char_count last_non_quote last_open 0;
        
        input next_char $char1.;
        
        char_count=char_count+1;
                
        if first_character=1 then do;
           current_char=next_char;
           prev1_char='';
           prev2_char='';
           prev3_char='';
           first_character=0;
           repA=byte(174);
           repD=byte(175);
           last_non_quote=-1;
           return;
           end;
		
		file _tidftmp recfm=n;
		last_open=last_open+1;
							
		%let close_cond=(next_char="%unquote(&delimiter)" or next_char='0D'x or next_char='0A'x );                
        if open=0 and current_char='"' then do;
            open=1;
            last_open=0;
            last_non_quote=-1;
            end;
        else if open=1 then do;
            if current_char ne '"' then last_non_quote=0;
            else last_non_quote=last_non_quote+1;
            
            if (prev1_char ne '"' and current_char='"' and &close_cond.) or
               (prev2_char="," and last_open=2 and prev1_char='"' and current_char='"' and &close_cond.) or
               (last_non_quote>=3 and last_non_quote/2 ne int(last_non_quote/2) and prev1_char='"' and current_char='"' and &close_cond.) 
            then do;
                open=0;
                last_non_quote=-1;
                end;
            end;
            
        if open=1 and current_char='0D'x then put repD +(-1);
        else if open=1 and current_char='0A'x then put repA +(-1);
        else put current_char +(-1);
        
        prev3_char=prev2_char;
        prev2_char=prev1_char;
        prev1_char=current_char;           
        current_char = next_char;
        
        
        %*if char_count>1000 then stop;
        
        return;
        
        end_of_file: put current_char +(-1);        
        run;
    
    filename _tidftmp clear;
    
    filename _tidftmp "&tempdir.\tm_import_delimite_file-macro-temp-file.txt" recfm=v lrecl=&file_max_record_length;
    
    proc import datafile=_tidftmp
                out=&out
                dbms=dlm
                replace;
    delimiter="%unquote(&delimiter)";
    getnames=yes;
    guessingrows=&guessingrows;
    datarow=2;
    run;
    
    filename _tidftmp clear;
    
/*	data _null_;*/
/*    	fname="_tidftmp";*/
/*  		rc=filename(fname,"./tm_import_delimite_file-macro-temp-file");*/
/*  		if rc = 0 and fexist(fname) then rc=fdelete(fname);*/
/*  		rc=filename(fname);*/
/*	run;*/
	
	proc contents data=&out out=contents noprint;
	run;
	
	data _null_;
		set contents end=last;
		retain n 0;
		if type=2 then do;
			n=n+1;
			call symput('cvar'||trim(left(n)),trim(left(name)));
			end;
		if last then do;
			call symput('ncvars',trim(left(n)));
			end;
		run;
		
	data &out;
		set &out;
		__schars=byte(174)||byte(175);
		__rchars='0A'x||'0D'x;
		drop __schars __rchars;
		%do i=1 %to &ncvars;
			&&cvar&i=translate(&&cvar&i,__rchars,__schars);
			%end;
		run;
		
    %end;
%mend;