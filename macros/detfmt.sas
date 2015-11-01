%macro detfmt;                                                                                                          
                                                                                                                        
proc sql;                                                      /*  Create the &fmtdata and fmtdb datasets.           */ 
   reset noprint;                                              /*  &fmtdata is the table of variable and there corr- */ 
   create table &fmtdata                                       /*  esponding format.  fmtdb is a table of defined    */ 
     (name        char(32),                                     /*  formats.  This is needed so that I can use exist- */ 
      format      char(7));                                    /*  ing formats for other variables.                  */ 
                                                                                                                        
   create table fmtdb                                                                                                   
     (begin       num,                                                                                                  
      end         num,                                                                                                  
      groups      num,                                                                                                  
      levels      num,                                                                                                  
      ntype      char(5),
      format      char(7));                                                                                             
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
%do i=1 %to &vgrps;                                            /*  Begin looping through the variabls by variable    */ 
   %if &i ne &vgrps %then %do;                                 /*  groups.                                           */ 
      %let begin=%eval(((&i-1)*&frqgroup)+1);                                                                           
      %let end=%eval(&i*&frqgroup);                                                                                     
      %end;                                                                                                             
   %else %do;                                                                                                           
      %let begin=%eval(((&i-1)*&frqgroup)+1);                                                                           
      %let end=&varn;                                                                                                   
      %end;                                                                                                             
                                                                                                                        
                                                                                                                        
   %let npflag=0;                                              /*  &npflag is a flag.  1 means there is a numeric    */ 
   %do k=&begin %to &end;                                      /*  variable in the variable group and 0 means there  */ 
      %if &&typ&k=1 %then %let npflag=1;                       /*  is not.                                           */ 
      %end;                                                                                                             
                                                                                                                    
                                                                                                                        
   %if &npflag=1 %then %do;                                    /*  If there is a numeric variable in the group then  */ 
      proc univariate data=&work..sample noprint;              /*  compute the min, max, &highpctl percentile, and   */ 
      var                                                      /*  &lowpctl percentile.  This is done using proc     */ 
      %do k=&begin %to &end;                                   /*  univariate.  This done on an entire variable group*/ 
         %if &&typ&k=1 %then %do;                              /*  at a time and stored in the dataset &work..sample */ 
            &&var&k                                                                                                     
            %end;                                                                                                       
         %end;                                                                                                          
         ;                                                                                                              
      output out=&work..univ&i                                                                                          
             pctlpts=&highpctl &lowpctl                                                                                 
             sum= %do k=&begin %to &end;                                                                                
                     %if &&typ&k=1 %then %do;                                                                           
                        sum&k                                                                                           
                        %end;                                                                                           
                     %end;                                                                                              
             min= %do k=&begin %to &end;                                                                                
                     %if &&typ&k=1 %then %do;                                                                           
                        min&k                                                                                           
                        %end;                                                                                           
                     %end;                                                                                              
             max= %do k=&begin %to &end;                                                                                
                     %if &&typ&k=1 %then %do;                                                                           
                        max&k                                                                                           
                        %end;                                                                                           
                     %end;                                                                                              
             pctlpre= %do k=&begin %to &end;                                                                            
                        %if &&typ&k=1 %then %do;                                                                        
                           v&k.p                                                                                        
                           %end;                                                                                        
                        %end;                                                                                           
             ;                                                                                                          
      run;                                                                                                              
   %end;                                                                                                                
                                                                                                                        
   proc freq data=&work..sample;                                                                                        
   %do k=&begin %to &end;                                                                                               
      tables &&var&k / noprint out=fd&k;                                                                                
      %end;                                                                                                             
   run;                                                                                                                 
                                                                                                                        
                                                                                                                        
   %do k=&begin %to &end;                                      /*  Begin cycling through all of the variables in the */ 
                                                               /*  current variable group.                           */ 
                                                                                                                        
                                                                                                                        
                                                                                                                        
      proc sql;                                                /*  Determine the number of unique values for the     */ 
      reset noprint;                                           /*  current variable.  I used proc freq instead of an */ 
      select count(*) into :levels from work.fd&k              /*  sql statement because I was running into a bug    */ 
      ;quit;                                                   /*  proc sql on MVS.  I was getting a stack overflow  */ 
                                                               /*  error after many iterations.                      */ 
                                                                                                                        
                                                                                                                        
      %if &&typ&k=2 %then %do;                                 /*  Start processing for character variables.         */ 
                                                                                                                        
         %let lenflag=0;                                                                                                
                                                                                                                        
         data _null_;                                                                                                   
           set fd&k;                                                                                                    
           if length(&&var&k)>3 then do;                                                                                
              call symput('lenflag','1');                                                                               
              stop;                                                                                                     
              end;                                                                                                      
           return;                                                                                                      
           run;                                                                                                         
                                                                                                                        
         data temp(keep=name format);                          /*  If there are more than 100 categories for the     */ 
            length name $ 32 format $ 7;                        /*  character variable then do not produce a frequency*/ 
            name="&&var&k";                                    /*  table for it, otherwise set the variable to use   */ 
            if &levels<=100 and &lenflag=0 then format="NOFMT";/*  no format.  This is stored in a temp dataset and  */ 
            else format="NOFREQ";                              /*  is then appended to the &fmtdata dataset.         */
         call symput("fmt&k",format); 
            output;                                                                                                     
            run;                                                                                                        
                                                                                                                        
         proc append base=&fmtdata data=temp;                  /*  Here the above dataset is appended to the        */  
         run;                                                  /*  &fmtdata dataset.                                */  
                                                                                                                        
         %end;                                                 /*  This ends the processing for character variables */  
                                                                                                                        
                                                                                                                        
                                                                                                                        
      %else %if &&typ&k=1 %then %do;                           /*  Start processing for numeric variables           */  
         %let flag=0;                                                                                                   
                                                                                                                        
         proc sql;                                             /*  Store the summary statistic values into macro    */  
         reset noprint;                                        /*  variables &sum, &min, &max, &high, &low.         */  
         select sum&k into :sum from &work..univ&i;            /*  &high corresponds to the &highpctl percentile    */  
         select min&k into :min from &work..univ&i;            /*  and &low to the &lowpctl percentile.             */  
         select max&k into :max from &work..univ&i;                                                                     
         select v&k.p&highpctl into :high from &work..univ&i;                                                           
         select v&k.p&lowpctl into :low from &work..univ&i;                                                             
         quit;                                                                                                               
                                                                                                                        
                                                                                                                        
                                                                                                                        
         %let nfflag=0;                                        /*  &nfflag is a binary variable.  1 = no format for  */ 
                                                               /*  variable.   0 = variable has a format.            */ 
                                                                                                                        
                                                                                                                        
         data _null_;                                          /*  This step cleans up the statistics if the var-    */ 
            low=&low;                                          /*  iable has null data or approximately null data.   */ 
            high=&high;                                        /*  Conditions that occur are:  min=max or min=. or   */ 
                                                               /*  max=. or high percentile=low percentile or high   */ 
            if low=high then do;                               /*  percentile=. or low percentile=.                  */ 
               call symput('low',"&min");                      /*  In the event of the situations no format will be  */ 
               call symput('high',"&max");                     /*  produced but a frequecy table will be produced.   */ 
               low=&min; high=&max;                            /*                                                    */ 
               end;                                            /*  The variable &nfflag is set to 1 when this one    */ 
                                                               /*  of these conditions occur.                        */ 
            if low=. or high=. or low=high then do;                                                                     
               call symput('nfflag','1');                                                                               
               end;                                                                                                     
                                                                                                                        
            sum=&sum;                                                                                                   
            levels=&levels;                                                                                             
                                                                                                                        
            if sum ne int(sum) or levels>&level_th or          /*  Here each numeric variable is determined to be    */ 
               low>20 or high>100 then do;                     /*  treated as a count variable or a floating point   */ 
               call symput('ntype','FLOAT');                   /*  variable.  A variable is to be treated as floating*/ 
               end;                                            /*  point if the sum(var) ne int(sum(var))     or     */ 
            else do;
               if levels<=10 then do;
                  call symput('nfflag','1'); 
                  end;
               else do;
                                                                /*  the number of distinct values > &level_th  or     */ 
                  call symput('ntype','COUNT');                 /*  low percentile > 20 or high percentile > 100      */
                  end;
               end;                                                                                                     
            return;                                            /*  &ncount contains the value which determines how   */ 
            run;                                               /*  each numeric variable is to be treated.  The      */ 
                                                               /*  values are FLOAT and COUNT                        */ 
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
         data tempdb(keep=begin end groups format levels ntype); /*  A dataset is created for the current variable and */ 
            length format $ 7 ntype $ 5;                         /*  contains information defining the format that will*/ 
                                                                 /*  produced for it.  The fields are listed in the    */ 
            tv=&high; sum=&sum;                                  /*  keep statement.  Most are self explanatory, except*/ 
                                                                 /*  groups which is just the value of &groups.  I     */ 
            if tv=0 or tv=. then begin=10;                       /*  included this for possible use in the future.     */ 
            else begin=round(tv,10**int(log10(abs(tv))));        /*  The algorithm could be changed to optimize the #  */ 
                                                                 /*  groups based on the sample or something like that.*/ 
            tv=&low;                                             /*  Currently it is constant for every variable and is*/ 
                                                                 /*  specified with the &group parameter.  The format  */ 
            if tv=0 or tv=. then end=10;                         /*  name is in the format variable and is named f&k.z */ 
            else end=round(tv,10**int(log10(abs(tv))));                                                                      
                                                                                                                        
            groups=&groups;                                                                                             
            format="f&k.z";                                                                                             
            levels=&levels;                                                                                             
            ntype="&ntype";
                                                                                                                        
            if levels<=&level_th and &low<=20 and               /* If the variable is a COUNT variable then set the   */ 
            &high<=100 and sum=int(sum) then do;                /* values for a COUNT variable.  Currently there is   */ 
               begin=1;                                         /* only one type of COUNT variable and it is defined  */ 
               end=&groups;                                     /* with a format that goes from 1 to &groups.  This   */ 
               end;                                             /* could change to variable range count variables in  */ 
                                                                /* the future.                                        */ 
                                                                                                                        
                                                                                                                        
            if &nfflag=1 or &low=&high then do;                 /*  If the &nfflag is set then we are not going to    */ 
               call symput('nfflag','1');
               delete;
               end;
            else output;                                        /*  create a format for it and we will not need to    */ 
            run;                                                /*  store any information about the format.  Also     */ 
                                                                /*  some of the information is null or missing.       */ 
                                                                                                                        
                                                                                                                        
                                                                                                                        
         proc sort data=fmtdb;                                  /*  Sort the datasets for merging                     */ 
         by begin end groups ntype;                                                                                           
         run;                                                                                                           
         
         proc sort data=tempdb;
         by begin end groups ntype;
         run;                                                                                          
                                                                                                                        
                                                                                                                        
         %let dbflag=0;                                        /*  &dbflag is a binary variable.                     */ 
                                                               /*    0 = no previous format can be used for the      */ 
                                                               /*        current variable.                           */ 
                                                               /*    1 = there is a previous format that can be      */ 
                                                               /*        used for the current variable.              */ 
                                                                                                                        
                                                                                                                        
         %if &nfflag=0 %then %do;         
         data _null_;                                          /*  The two datasets are merged and if a match on     */ 
            merge fmtdb(in=in1) tempdb(drop=format in=in2);    /*  the current format with a previous format is      */ 
            by begin end groups ntype;                         /*  found then &dbflag is set to 1 and the previous   */ 
            if in1 and in2 then do;                            /*  format name is stored in the variable &prevfmt    */ 
               call symput('dbflag','1');                                                                               
               call symput('prevfmt',format);                                                                           
               end;                                                                                                     
            return;                                                                                                     
            run;                                                                                                        
         %end;                                                                                                               
         %else %do;
            %let dbflag=1;
            %end;


         %if &dbflag=1 %then %do;                              /*  If &dbflag=1 then the previous format is assoc-   */ 
            data temp;                                         /*  iated with it and stored in a temp dataset which  */ 
               length name $ 32 format $ 7;                     /*  is then appended to the &fmtdata dataset.         */ 
               if &nfflag=1 then format="NOFMT";                                                                        
               else format=trim(left("&prevfmt"))||"."; 
               put "SUGI:  " format;                                                                                 
               name="&&var&k"; 
               call symput("fmt&k",format); 
               output;                                                                                                  
               run;                                                                                                     
                                                                                                                        
            proc append base=&fmtdata data=temp;                                                                        
            run;                                                                                                        
            %end;                                                                                                       
                                                                                                                        
                                                                                                                        
                                                                                                                        
         %else %if &dbflag=0 %then %do;                        /*  If &dbflag=0 then processing to define the format */ 
            proc append base=fmtdb data=tempdb;                /*  continues.  The new format definition is added to */ 
            run;                                               /*  the fmtdb dataset.                                */ 
                                                                                                                        
                                                                                                                        
            data temp;                                         /*  The variable-format association information is    */ 
               length name $ 32 format $ 7;                     /*  created in a temp dataset which is then appended  */ 
               if &nfflag=1 then format="NOFMT";               /*  to the &fmtdata dataset.  If &nfflag is set then  */ 
               else format="f&k.z.";                           /*  the format is set to NOFMT, which means no format */ 
               name="&&var&k";                                 /*  is to be applied to the current variable.         */ 
            call symput("fmt&k",format);
            output;                                                                                                  
               run;                                                                                                     
                                                                                                                        
            proc append base=&fmtdata data=temp;                                                                        
            run;                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
                                                                                                                        
            %if &ntype=FLOAT %then %do;                        /*  Proccessing begins for floating point variables.  */ 
                                                                                                                        
                                                                                                                        
               data format(keep=begin end value);              /*  A dataset called format is created that will      */ 
                  length value $ 60;                           /*  hold each range definition of the format.         */ 
                  flag=0;                                      /*  Begin is the beginning value.  End is the ending  */ 
                  counter=1;                                   /*  value.  Value is the translated value for the     */ 
                  last=.;                                      /*  range.                                            */ 
                                                                                                                        
                  
              tv=(&high-&low)/&groups;                     /*  UNIT contains the range length.                   */ 
                  if tv=0 or tv=. then tv=10;
                  
              rndval=int(log10(abs(tv))) - &sigdigs + 1;        /*  RNDVAL is the rounding threshold for the current  */ 
                                                               /*  variable.  This is set to the # of present digits */ 
                                                               /*  and/or decimal places in the unit variable.       */ 
                                                                                                                        
              if tv<0 then tv=tv-((10**rndval)/2);
              else tv=tv+((10**rndval)/2);

              unit=round(tv,10**rndval);
                  
              put "high &high";
              put "low &low";
              put "groups &groups";
              put "tv " tv;
              put "rndval " rndval;
              put "unit " unit;

                                                                                                                        
                  if &min<0 then do;                           /*  If the minimum is below zero then compute the     */ 
                     begin=-72E15;                              /*  first range entry for the current variable.       */ 
                                                                                                                        
                     tv=&low;                                                                                           
                     end=round(tv,10**rndval);                                                                              
                                                                                                                        
                     if end=0 then xend='ZERO';                                                                         
                     else xend=trim(left(end));                                                                         
                     value="LT " || trim(left(end));                                                             
                     output;                                                                                            
                                                                                                                        
                     if end=0 then do;                                                                                  
                        begin=0; end=0; value='ZERO';                                                                   
                        output;                                                                                         
                        end;                                                                                            
                     end;                                                                                               
                  else do;                                     /*  Else compute the first range entry for variables  */ 
                     begin=-72E15;                              /*  with a positive minimum.                          */ 
                     end=0;                                                                                             
                     value="LT ZERO";                                                                            
                     output;                                                                                            
                     begin=0;end=0;value="ZERO";                                                                        
                     output;                                                                                            
                     end;                                                                                               
                                                                                                                        
                                                                                                                        
                  if end=0 and &low>0 and &min>=0 then do;     /*  If &low is greater than zero then compute the     */ 
                     begin=0;                                  /*  second range entry for the current variable.      */ 
                     tv=&low;                                  /*  Also set the variable a to the end of the range.  */ 
                     a=round(tv,10**rndval);                   /*  This will be used in the loop below to specify    */ 
                     if a=0 then a=10**rndval;                 /*  the rest of the ranges.                           */ 
                                                                                                                        
                     end=a;                                                                                             
                     value="0 <-< " || trim(left(end));                                                                 
                     output;                                                                                            
                     end;                                                                                               
                  else a=end;                                  /*  If &low is not greater than zero then set the     */ 
                                                               /*  variable a to the end value of the last range.    */ 
                                                                                                                        
                                                                                                                        
                  tv=&high;                                    /*  Set the variable b to the high end of the range   */ 
                  b=round(tv,10**rndval);                      /*  defenitions, &highpctl percentile rounded.        */ 
                                                                                                                        
                                                                                                                        
                  last=a;                                                                                               
                  if a ne b then do;                           /*  Compute the rest of the range definitions for the */ 
                     do j=(a+unit) to b by unit;               /*  current variable.                                 */ 
                        begin=last;                                                                                     
                        end=j;                                                                                          
                        if begin=0 then value=(trim(left(begin)) || " <-< " || trim(left(end)));                        
                        else value=(trim(left(begin)) || " -< " || trim(left(end)));                                    
                        output;                                                                                         
                        last=end;                                                                                       
                        end;                                                                                            
                     end;                                                                                               
                                                                                                                        
                  begin=last;                                  /*  Compute the last range definition for the current */ 
                  end=72E15;                                    /*  current variable.                                 */ 
                  value="GTE " || trim(left(last));                                                
                  output;                                                                                               
                  run;                                                                                                  
               %end;                                                                                                    
            %else %do;                                         /*  If &ntype ne FLOAT then compute a format for a    */ 
               data format;                                    /*  count variable.  This should only execute once    */ 
                  length value $ 60;                           /*  with the current algorithm.                       */ 
                  begin=-72E15;                                                                                          
                  end=0;                                                                                                
                  value="LT ZERO";                                                                               
                  output;                                                                                               
                  do j=0 to &groups;                                                                                    
                     begin=j;                                                                                           
                     end=j;                                                                                             
                     value=trim(left(j));                                                                               
                     output;                                                                                            
                     end;                                                                                               
                  begin=&groups+1;                                                                                      
                  end=72E15;                                                                                             
                  value="GT " || trim(left("&groups"));                                                       
                  output;                                                                                               
                  run;                                                                                                  
               %end;                                                                                                    
                                                                                                                        
            %if &nfflag=0 %then %do;                           /*  If &nfflag is not set then create the proc format */ 
               proc sort data=format;                          /*  statement and append it to the code file.         */ 
               by begin;                                                                                                
               run;                                                                                                     
                                                                                                                        
               data _null_;                                                                                             
                  file &codefile mod;                                                                                   
                  set format end=last;                                                                                  
                                                                                                                        
                  length xbegin xend $ 30;                                                                              
                                                                                                                        
                  if _n_=1 then do;                                                                                     
                     put / / "proc format library=&fmtcat;";                                                            
                     put "value f&k.z";                                                                                 
                     end;                                                                                               
                                                                                                                        
                  if begin=end then do;                                                                                 
                     put @30 begin @60 "= '" value +(-1) "'";                                                           
                     end;                                                                                               
                  else do;                                                                                              
                     if begin=-72E15 then xbegin='LOW';                                                                  
                     else xbegin=trim(left(begin));                                                                     
                                                                                                                        
                     if end=72E15 then xend='HIGH';                                                                      
                     else xend=trim(left(end));                                                                         
                                                                                                                        
                     xend=trim(left(xend));                                                                             
                     xbegin=trim(left(xbegin));                                                                         
                                                                                                                        
                     if begin=0 then connect=" <-< ";                                                                   
                     else connect="  -< ";                                                                              
                                                                                                                        
                     if xend ne 'HIGH' then do;                                                                         
                        put @3 xbegin @30 connect xend @60 "= '" value +(-1) "'";                                       
                        end;                                                                                            
                     else do;                                                                                           
                        put @3 xbegin @30 "  -  " xend @60 "= '" value +(-1) "'";                                       
                        end;                                                                                            
                     end;                                                                                               
                                                                                                                        
                  if last then put ";run;";                                                                             
                                                                                                                        
                  return;                                                                                               
                  run;                                                                                                  
               %end;                                                                                                    
            %end;                                                                                                       
         %end;                                                                                                          
                                                                                                                        
         proc datasets library=work nodetails nolist;                                                                                    
         delete fd&k;                                                                                                   
         run;                                                                                                           
                                                                                                                        
      %end;                                                                                                             
   %end;                                                                                                                
                                                                                                                        
%mend;                                                                                                                  
