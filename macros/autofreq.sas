%macro autofreq(data=,                                                                                                      
            maxsamp=50000,
            sigdigs=1,                                                                                
            samppct=10,                                                                                                 
            highpctl=95,                                                                                                
            lowpctl=5,                                                                                                  
            groups=10,                                                                                                  
            frqgroup=100,                                                                                               
            work=work,                                                                                                  
            fmtcat=work.formats,                                                                                        
            fmtdata=work.fmts,                                                                                          
            codefile=_temp_,                                                                                            
            level_th=100,                                                                                                
            seqobs=500000,                                                                                              
            seq=no,                                                                                                     
            depvar=,                                                                                                    
            dvformat=,                                                                                                  
            options=,
            chisqrnk=no, 
            chisqrnk_dataset=_temp_,
            bargraph=no,
            linegraph=no
            );                                                                                                          

 
%let null=;  
%let workdir=%sysfunc(getoption(work));                                                                                                           

%if %qupcase(&chisqrnk)=%quote(YES) or %qupcase(&chisqrnk)=%quote(Y) %then %do;
   %let options=&options chisq;
   %end;
 
%if %qupcase(&sysscpl)=%quote(MVS)                          /*  Here the operating system which this macro is     */    
   %then %let os=mvs;                                       /*  being run under is determined and the OS type is  */    
%else %if %qupcase(&sysscpl)=%quote(HP-UX)                  /*  stored in the macro variable &OS.                 */  
       or %qupcase(&sysscpl)=%quote(OSF1)  
   %then %let os=unix;                                                                                                  
%else %if %qupcase(%substr(&sysscpl,1,3))=%quote(WIN)                                                                                
   %then %let os=windows; 
%else %let os=windows; 
                                                                                                                        
                                                                                                                        
                                                                                                                        
data _null_;                                                /*  This datastep determines a random id to use in the  */   
   x=int(ranuni(-1)*10000);                                 /*  temporary filenames, so that more than one instance */   
   call symput('tid',trim(left(x)));                        /*  of this macro can run in the same directory. The id */   
   run;                                                     /*  is stored in the macro variable &TID.               */   
                                                                                                                        
%let tid=%trim(%left(&tid));                                                                                            
                                                                                                                        
%if %quote(&codefile)=%quote(_temp_) or 
    %quote(&codefile)=%quote(&null.) %then %do;             /*  Here the temporary filename is allocated depending */   
   %if &os=mvs %then %do;                                   /*  on the value of &OS.  The file is only allocated   */   
      %let unum=%substr(&sysjobid,1,6);                     /*  if &codefile is equal to _temp_                    */   
      filename codefile "&unum..tg&tid..freq.app"                                                                       
         disp=(new,delete,delete) space=(cyl,(5,1))                                                                     
         blksize=22000 lrecl=110 recfm=fb dsorg=ps;                                                                     
      %end;                                                                                                             
   %else %do;                                                                                                           
      filename codefile "&workdir.\tg&tid..freq.app";                                                                             
      %end;                                                                                                             
   %let codefile=codefile;                                                                                              
   %end;                                                                                                                
                                                                                                                        
%if %qupcase(&chisqrnk_dataset)=%quote(_TEMP_) or %qupcase(&chisqrnk_dataset)=%qupcase(&null) %then %do;        
   %let chisqrnk_dataset=chisqrnk;                          /* This is the default dataset to store chi-square   */                                                                 
   %end;                                                    /* statistic output in.                              */                                                            
                                                                                                                                       
                                                                                                                        
                                                                                                                        
data _null_;                                                /*  Initializes the code file                         */    
   file &codefile;                                                                                                      
   put ' ';                                                                                                             
   return;                                                                                                              
   run;                                                                                                                 
                                                                                                                        
                                                                                                                      
                                                                                                                        
                                                                                                                        
%if %qupcase(&seq) ne %quote(YES) %then %do;                /*  Determines the number of observations in the       */   
   proc sql; reset noprint;                                 /*  dataset.  If the library is sequential, (&seq=YES) */   
   select count(*) into :dsize from &data;                  /*  , then this is supplied by the user in the &seqobs */   
   %end;                                                    /*  parameter.  The value is then stored in the &dsize */   
%else %do;                                                  /*  macro variable.                                    */   
   %let dsize=&seqobs;                                                                                                  
   %end;                                                                                                                
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
data &work..sample;                                         /*  Creates a sample of the original dataset.          */   
   retain  __p 0;                                           /*  This sample will not be larger than &maxsamp       */   
   drop __p sampsize;                                       /*  observations.                                      */   
   set &data;                                                                                                           
                                                                                                                        
   __p=__p+1;                                                                                                           
                                                                                                                        
   if (&samppct/100)*&dsize >= &maxsamp then                                                                            
      sampsize=&maxsamp;                                                                                                
   else sampsize=&maxsamp;                                                                                              
                                                                                                                        
   if ranuni(-1)<=(sampsize/&dsize) then output;                                                                        
   return;                                                                                                              
   run;                                                                                                                 
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
proc contents data=&work..sample noprint out=contents;      /*  Assign variable names and types (1=num, 2=char)   */    
run;                                                        /*  to macro variables.  &&var&i will contain the     */    
                                                            /*  variable name and &&typ&i will contain the type   */    
proc sort data=contents;                                    /*  code.  Also assign the number of variables to the */    
by name;                                                    /*  variable &varn, and the # of variable groups to   */    
run;                                                        /*  the variable &vgrps.  Variable groups define the  */    
                                                            /*  maximum number of variables that will be used in  */    
data _null_;                                                /*  one procedure or calculation at the same time.    */    
   set contents end=last;                                   /*  This can be set w/ the &frqgroup parameter.  It   */    
   by name;                                                 /*  defaults to 200.                                  */    
   call symput('var'||trim(left(_n_)),trim(left(name)));                                                                
   call symput('typ'||trim(left(_n_)),trim(left(type)));
   call symput('fmt'||trim(left(_n_)),''); 
   if last then do;                                                                                                     
      call symput('varn',trim(left(_n_)));                                                                              
      vgrps=int(_n_/&frqgroup)+1;                                                                                       
      lgrp=_n_-vgrps;                                                                                                   
      call symput('vgrps',trim(left(vgrps)));                                                                           
      call symput('lgrp',trim(left(lgrp)));                                                                             
      end;                                                                                                              
   return;                                                                                                              
   run;                                                                                                                 
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
%detfmt;                                                    /*  This calls the %detfmt macro.  This contains the  */    
                                                            /*  the code to determine a format for each variable  */    
                                                            /*  based on the sample dataset.  The results all of  */    
                                                            /*  the variables are returned in the &fmtdata data-  */    
                                                            /*  set.                                              */    
                                                                                                                        
                                                                                                                        
                                                                                                                        
proc sort data=&fmtdata;                              /*B*/ /*  This creates the proc freq statements and appends */    
   by name;                                                 /*  them to the code file after the proc format       */    
   run;                                                     /*  statements.                                       */    
                                                                                                                        
proc sql;                                                                                                               
   reset noprint;                                                                                                       
   select count(*) into :nvars from &fmtdata;                                                                           
                                                                                                                        
data _null_;                                                                                                            
   file &codefile mod;                                                                                                  
   length odsname $ 64;                                                                                                                     
   do i=1 to &vgrps;                                        /*  Cycle through the variables in groups of &frqgroup*/    
      begin=&frqgroup*(i-1)+1;                                                                                          
      if i ne &vgrps then end=i*&frqgroup;                                                                              
      else end=&nvars;                                                                                                  
      
      %if (%qupcase(&chisqrnk)=%quote(YES) or %qupcase(&chisqrnk)=%quote(Y)) and 
           %qupcase(&depvar) ne %quote(&null) %then %do;
         put "ODS OUTPUT ChiSq=csqrtmp" i +(-1) ";";
         %end;
      
      %if (%qupcase(&bargraph)=%quote(YES) or %qupcase(&bargraph)=%quote(Y)) or
          ((%qupcase(&linegraph)=%quote(YES) or %qupcase(&linegraph)=%quote(Y)) and 
            %qupcase(&depvar) ne %quote(&null))
      %then %do;
         count=0;
         do k=begin to end;
            set &fmtdata point=k;
            if upcase(format) ne 'NOFREQ' then do;
               count=count+1;
               odsname=trim(left(name))||"_by_&depvar";
               odsname=trim(left(odsname));
               
               if length(odsname)>32 then odsname=substr(odsname,1,32);
               
               if count>1 then do;
                  put "ODS OUTPUT Freq." odsname +(-1) 
                      ".CrossTabFreqs" count +(-1) "=ctds" k +(-1) ";";
                  end;
               else do;
                  put "ODS OUTPUT Freq." odsname +(-1) 
                      ".CrossTabFreqs=ctds" k +(-1) ";";
                  end;
               end;
            end;
         %end;
         
      put "proc freq data=&data;";                                                                                      
      put "tables";                                                                                                                                                                                                                             
      flag=0;                                                                                                           
      do k=begin to end;                                    /*  Cycle through for the tables statement and then   */    
         set &fmtdata point=k;                              /*  again for the format statement.  This is done     */    
                                                            /*  using the point= option of the set statement and  */    
         %if %quote(&depvar) ne %quote(&null) %then %do;    /*  random access capabilities of sas datasets.       */    
            if upcase(format) ne 'NOFREQ' then                                                                          
               put @3 name +(-1) "*&depvar";                                                                                  
            %end;                                                                                                       
         %else %do;                                                                                                     
            if upcase(format) ne 'NOFREQ' then put @3 name;                                                             
            %end;                                                                                                       
                                                                                                                        
         if upcase(format) not in ('NOFREQ','NOFMT')                                                                    
            then flag=1;                                                                                                
         end;     
 
      %if %quote(&options) ne %quote(&null) %then %do;                                                               
         put "/ contents='Distribution Table' &options ;";                                                                                        
         %end;                                                                                                      
 
      if flag=1 then do;                                    /*  Only create a format statement if there are formats */  
                                                            /*  for this variable group.                            */                                                                                                                        
         put ";";                                                                                                       
         put "format";                                                                                                  
                                                                                                                        
         do k=begin to end;                                                                                             
            set &fmtdata point=k;                                                                                       
            if upcase(format) not in ('NOFREQ','NOFMT') then put @3 name @60 format;                          
            end;                                                                                                        
         end;                                                                                                           
                                                                                                                        
      %if %quote(&depvar) ne %quote(&null) and                                                                          
          %quote(&dvformat) ne %quote(&null) %then %do;                                                                 
          put "format &depvar &dvformat;";                                                                              
          %end;                                                                                                         
                                                                                                                        
      put @3 ";" / "run;";                                                                                              
      end;                                                                                                              
                                                                                                                        
   stop;                                                                                                                
   return;                                                                                                              
   run;                                            /*E*/                                                                
                                                                                                                        
%include &codefile / source2;                            /*  This executes the proc formats and proc freqs        */    
        
%if %quote(&codefile)=%quote(codefile) and               /*  This deletes the temporary codefile on unix or       */    
    %quote(&os)=%quote(unix) %then %do;                  /*  windows based operating systems.  This is not needed */    
   x "delete tg&tid..freq.app";                              /*  for MVS.                                             */    
   %end;                                                                                                                

%if (%qupcase(&chisqrnk)=%quote(YES) or %qupcase(&chisqrnk)=%quote(Y)) and 
     %qupcase(&depvar) ne %quote(&null) %then %do;

   %let dvlen=%length(&depvar);

   data &chisqrnk_dataset;
      length variable $ 50;
      set %do i=1 %to &vgrps;
            csqrtmp&i(where=(Statistic='Chi-Square'))
            %end;
            ;
            
      tlen=index(table,'_by_');
      vlen=tlen-1;
      variable=substr(table,1,vlen);
      drop tlen vlen table;
      output;
      run;

   proc sort data=&chisqrnk_dataset;
   by descending value;
   run;
    
   title 'Chi-Square Ranking of Independant Variables'; 
   
   proc print data=&chisqrnk_dataset noobs 
              contents='Chi-Square Ranking of Independant Variables';
              
   var variable value;
   run;
   %end;

%if ((%qupcase(&bargraph)=%quote(YES) or %qupcase(&bargraph)=%quote(Y))and 
     %qupcase(&depvar) ne %quote(&null))
         or
    ((%qupcase(&linegraph)=%quote(YES) or %qupcase(&linegraph)=%quote(Y)) and
      %qupcase(&depvar) ne %quote(&null)) %then %do;
      
      %do i=1 %to &vgrps;  
            %let begin=%eval(&frqgroup*(&i-1)+1); 
            %if &i ne &vgrps %then %let end=%eval(&i*&frqgroup);                                                                              
            %else %let end=&nvars;   
         
         %do k=&begin %to &end;           
            %if %qupcase(&&fmt&k) ne %quote(NOFREQ) %then %do;
               proc sort data=ctds&k(where=(_type_='11')) out=charttmp;
               by &&var&k &depvar;
               run;

               data charttmp;
                  set charttmp;
                  by &&var&k &depvar;
                  length group $ 8;
                  retain count 0;
                  if first.&&var&k then count=count+1;
                  group='G'||trim(left(put(count,best4.)));
                  output;
                  run;  
               
               proc sort data=ctds&k(where=(_type_='10')) out=chrttmp2;
               by &&var&k &depvar;
               run;

               proc sql;
               reset noprint;
               select distinct count(*) into :levels
               from chrttmp2
               ;quit;

               data _null_;
                  set chrttmp2;
                  by &&var&k &depvar;
                  %if &&typ&k=1 and %qupcase(&&fmt&k) ne %quote(NOFMT) %then %do;
                     call symput('fg'||trim(left(put(_n_,best4.))),put(&&var&k,&&fmt&k)); 
                     %end;
                  %else %if &&typ&k=1 %then %do;
                     call symput('fg'||trim(left(put(_n_,best4.))),put(&&var&k,best12.)); 
                     %end;
                  %else %do;
                     call symput('fg'||trim(left(put(_n_,best4.))),&&var&k);
                     %end;
                  call symput('pc'||trim(left(put(_n_,best4.))),put(percent,4.2));
                  return;
                  run;
               
               axis1 value=(%do z=1 %to &levels;
                           "G&z"
                           %end;
                           )
                    order=(%do z=1 %to &levels;
                           "G&z"
                           %end;
                        )
                    label=("&&var&k")
                    ;

               %let numfnotes=%sysevalf(&levels/3,ceil);
               %do z=1 %to &numfnotes;
                  %let fnt1=&z;
                  %let fnt2=%eval(&numfnotes + &z);
                  %let fnt3=%eval(2*&numfnotes + &z);

                  %if &fnt3 <= &levels %then %do;
                     footnote&z justify=left   "G&fnt1.=&&fg&fnt1 (%trim(%left(&&pc&fnt1))%nrquote(%))"   
                              justify=center "G&fnt2.=&&fg&fnt2 (%trim(%left(&&pc&fnt2))%nrquote(%))"   
                              justify=right  "G&fnt3.=&&fg&fnt3 (%trim(%left(&&pc&fnt3))%nrquote(%))";
                     %end;
                  %else %if &fnt2 <= &levels %then %do;
                     footnote&z justify=left   "G&fnt1.=&&fg&fnt1 (%trim(%left(&&pc&fnt1))%nrquote(%))"   
                              justify=center "G&fnt2.=&&fg&fnt2 (%trim(%left(&&pc&fnt2))%nrquote(%))";
                     %end;
                  %else %if &fnt1 <= &levels %then %do;
                     footnote&z justify=left   "G&fnt1.=&&fg&fnt1 (%trim(%left(&&pc&fnt1))%nrquote(%))";
                     %end;
                  %end;

                  
               %if (%qupcase(&bargraph)=%quote(YES) or %qupcase(&bargraph)=%quote(Y))
               %then %do;
                  
                  proc gchart data=charttmp;
                  title "Distribution of &depvar by values of &&var&k";
                  vbar &depvar / freq=Frequency type=percent group=group discrete g100 gaxis=axis1 percent;
                  %if %quote(&dvformat) ne %quote(&null) %then %do;
                     format &depvar &dvformat;
                     %end;
                  run;
                  quit;
                  %end;

               %if (%qupcase(&linegraph)=%quote(YES) or %qupcase(&linegraph)=%quote(Y))
               %then %do;
                  
                  symbol1 color=black interpol=join value=dot height=2; 
                  axis2 label=("Mean of &depvar");
                  title "MEAN(&depvar) By &&var&k";
                  proc gplot data=charttmp(where=(&depvar=1)); 
                     plot RowPercent*group=1 / haxis=axis1 vaxis=axis2 frame;  
                  run;
                  quit;
                  %end;

               %do z=1 %to 10;
                  footnote&z;
                  title;
                  %end;
               %end;
            %end;
         %end;
      %end; 
%mend;                                                                                                              
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
