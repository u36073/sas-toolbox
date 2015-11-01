%macro tm_gcode_princomp(
   score=,
   code=,
   by=,
   by_rename_to=,
   language=sas,
   uselabels=n,
   append=n   
);

%local i k flag null;
%let null=;
    
proc contents data=&score(drop=_type_ _name_ &by) noprint out=contents;
run;

data contents;
   set contents;
   if trim(left(label))='' then label=name;
   run;
   
data _null_;
    set contents end=last;
    call symput('var'||trim(left(_n_)),name);
    call symput('lab'||trim(left(_n_)),trim(left(label)));
    if last then do;
        call symput('nvar',trim(left(_n_)));
        end;
    run;
    
%if %qupcase(&uselabels)=%quote(Y) %then %let rtarget=label;
%else %let rtarget=name;
          
data _null_;
    set contents end=last;
    call symput('rvar'||trim(left(_n_)),&rtarget);
run;

%if %quote(&by)=%quote(&null) %then %do;
   data _null_;
      %if %quote(&code) ne %quote(&null) %then %do;
          %if %qupcase(&append)=%quote(N) %then %do;
              file &code;
              %end;
          %else %do;
              file &code mod;
              %end;
          %end;
      set &score(where=(_type_='SCORE')) end=last;
      put _name_ " = 0";
      %do i=1 %to &nvar;
          if &&var&i ne . then 
             put "          + %trim(%left(%lowcase(&&rvar&i))) * " &&var&i;
          %end;
      put "           ;";
    run;                
    %end;
%else %do;
    %let flag=0;
    %let k=0;
    %do %until(&flag=1);
        %let k=%eval(&k+1);
        %let word=%scan(&by,&k);
        %if %quote(&word) ne %quote(&null) %then %do;
           %let by&k=&word;
           %end;
        %else %do;
             %let nbv=%eval(&k-1);
             %let flag=1;
           %end;
        %end;
    
    %if %quote(&by_rename_to) ne %quote(&null) %then %do;
       %let flag=0;
       %let k=0;
       %do %until(&flag=1);
           %let k=%eval(&k+1);
           %let word=%scan(&by_rename_to,&k);
           %if %quote(&word) ne %quote(&null) %then %do;
              %let byrt&k=&word;
              %end;
           %else %do;
                %let nbrtv=%eval(&k-1);
                %let flag=1;
              %end;
           %end;                           
       %end;
    %else %do;
       %let nbrtv=0;
       %end;
       
    %if &nbrtv<&nbv %then %do;
       %do i=%eval(&nbrtv+1) %to &nbv;
            %let byrt&i=&&by&i;
            %end;
       %end;     
    
    data _null_;
       length &rtarget $ 32;
       %do i=1 %to &nbv;
          &rtarget=trim(left(upcase("&&byrt&i")));
          call symput("rby&i",trim(left(&rtarget)));
          %end;
       run;
       
    proc sort data=&score out=__byscore;
    where _type_='SCORE';
    by &by _name_;
    run;
    
    data _null_;
      %if %quote(&code) ne %quote(&null) %then %do;
          %if %qupcase(&append)=%quote(N) %then %do;
              file &code;
              %end;
          %else %do;
              file &code mod;
              %end;
          %end;
      set __byscore(drop=_type_) end=last;
      by &by _name_;
      
      if first.&&by&nbv then do;
         %do i=1 %to &nbv;
            if _n_=1 then do;
                   put "if " @;
                   end;
            else do;
               %if %qupcase(&language)=%quote(SAS) %then %do;
                  put "else if " @;
                  %end;
               %else %if %qupcase(&language)=%quote(C#) %then %do;
                  put "else if " @;
                  %end;
               end;
            %if %qupcase(&language)=%quote(SAS) %then %do;
               %if &i=1 %then %do;                
                     put "trim(left(%lowcase(&&rby&i)))='" &&by&i +(-1) "' " @;
                     %end;
               %else %do;
                    put "and trim(left(%lowcase(&&rby&i)))='" &&by&i +(-1) "' " @;
                    %end;
               %end;
            %else %if %qupcase(&language)=%quote(C#) %then %do;
              %if &i=1 %then %do;                 
                     put "(%lowcase(&&rby&i).ToString()==" '"' &&by&i +(-1) '"' @;
                     %end;
                %else %do;
                    put "%nrstr(&&) %lowcase(&&rby&i).ToString()==" '"' &&by&i +(-1) '"' @;
                    %end;
               %end;
            %end;
         %if %qupcase(&language)=%quote(SAS) %then %do;   
            put "then do;";
            %end;
         %else %if %qupcase(&language)=%quote(C#) %then %do;
            put ") {";
            %end;
         end;
                             
      put "   " _name_ " = 0";
      %do i=1 %to &nvar;
          if &&var&i ne . then 
             put "          + %trim(%left(%lowcase(&&rvar&i))) * " &&var&i;
          %end;
      put "           ;";
      
      if last.&&by&nbv then do;
         %if %qupcase(&language)=%quote(SAS) %then %do;
            put "        end;";
            %end;
         %else %if %qupcase(&language)=%quote(C#) %then %do;
            put "           }";
            %end;
         end; 
    run;            
    %end;
%mend;