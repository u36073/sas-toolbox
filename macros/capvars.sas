/******************************************************************************
 Macro: VarType
 
 Parameters: Positional
 	x			   Variable Name
  data     SAS data set
   
 This macro resolves to the type associated with variable &x
 	N= numerical
 	C= character
 
 Example:  %VarLabel(varname,dataset)
******************************************************************************/ 
%macro VarType(x,data);
%local dsid i nvars name rc rv;
%let dsid = %sysfunc(open(&data,i));                                                                                                                                                
%let nvars = %sysfunc(attrn(&dsid,NVARS));                                                                                                                                          
%let rv=;                                                                                                                                                                          
%do i=1 %to &nvars;                                                                                                                                                                 
  %let name=%sysfunc(varname(&dsid,&i));                                                                                                                                            
  %if %qupcase(&name)=%qupcase(&x) %then %do;                                                                                                                                 
  	  %let rv=%sysfunc(vartype(&dsid,&i));
     %end;
  %end;                                                                                                                                                                             
%let rc=%sysfunc(close(&dsid));                                                                                                                                                     
&rv                                                                                                                                                                          
%mend;

/****************************************************************************** 
 Macro: ListDedupe                                                                   
                                                                                
 Parameters: Positional
 
 	list 		list of values separated by a space
               
 Removes any duplicate entries in a list of values.  The values can't contain
 spaces.  The macro resolves to a space separated list excluding duplicate values.
 
 Ex.
 
 %let varlist=x1 x2 score1 score2 x1 y1 z3 mailing_seq_number;
 %let varlist=%ListDedupe(&varlist);       
******************************************************************************/ 

%macro ListDedupe(list);
	%local null nitems x i j rv;
	
	%let null=;
	%let rv=&null;
	
	%if &list ne &null %then %do;
		%let nitems=0;
	   %let x=%scan(&list,1,%str( ));
	   %do %until(&x=&null);
	   	%let nitems=%eval(&nitems+1);
	   	%let x=%scan(&list,&nitems,%str( ));
	   	%end;
	   %let nitems=%eval(&nitems-1);
	   
	   %do i=1 %to &nitems;
	   	%local ldv&i;
	   	%let ldv&i=%trim(%left(%upcase(%scan(&list,&i,%str( )))));
	   	%end;
	   
	   %if &nitems>1 %then %do;	
	   	%do i=1 %to %eval(&nitems-1);
	   		%do j=%eval(&i+1) %to &nitems;
	   			%if &&ldv&i=&&ldv&j %then %let ldv&j=&null;
	   			%end;
	   		%end;
	   	
	   	%let rv=&ldv1;
	   	%do i=2 %to &nitems;
	   		%if &&ldv&i ne &null %then %let rv=&rv &&ldv&i;
	   		%end;
	   	%end;
	   %else %do;
	   	%let rv=&ldv1;
	   	%end;
	   %end;
	&rv	
%mend;

/******************************************************************************
Macro:  ParseList

Parameters:  positional

	prefix	Prefix to use for macro variables containing the name of each
	         member in the list
	         
	list		List of values separated by spaces
	
Parses a list of text or numeric values separated by a space and assigns each
value to a macro variable with the prefix specified in the [prefix] parameter 
and an integer suffix corresponding to its position in the list.

The macro itself resolves to the number of values in the list. An additional macro
variable is also created in the global environment of the form &prefix._csv.
It contains the list of values separated by a comma.

Example: 
	
	%let varlist=x y a1 model_score utilization debt_to_income;
	%let num_vars=%ParseList(var,&varlist);
	
	data .........;
		%do i=1 %to &num_vars;
			&&var&i=round(&&var&i,.00001);
			%end;
		run;
*******************************************************************************/

%macro ParseList(prefix,list);
	%local n word flag null;
	%let null=;
	%let list=%trim(%left(&list));
	
	%if %quote(&list) ne %quote(&null) %then %do;	
	   %let flag=0; %let n=1; %let word=; 
      %global &prefix.1;
      %let &prefix.1=%scan(&list,1,%str( ));
	   %do %while (&flag=0);
	   	%let n=%eval(&n+1);
	   	%let word=%scan(&list,&n,%str( ));
	   	%if &word=&null %then %do;
	   		%let nwords=%eval(&n-1);
	   		%let flag=1;
	   		%end;
	   	%else %do;
	   		%global &prefix.&n;
	   		%let &prefix.&n=&word;
	   		%end;
	   	%end;  
	   %end;
	%else %do;
		%let nwords=0;
	 	%end;
	 	
	 %if &nwords>0 %then %do;
	 	%global &prefix._csv;
 		%let &prefix._csv=&prefix.1;
 		%if &nwords>1 %then %do;
 			%do i=2 %to &nwords;
 				%let &prefix._csv=&&&prefix._csv,&&&prefix.&i;
 				%end;
 			%end;
	 	%end;
	&nwords
%mend;


%macro capvars(data=,
							 filename=,
							 vars=,
							 by=,
							 PercentileHigh=99,
							 PercentileLow=,
							 DropExistingVars=Y,
							 VariableSuffix=
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

%let pctlpts=75;
%let pctlname=p75;

%if &PercentileLow ne &null %then %do;
   %let pctlpts=&pctlpts &PercentileLow;
   %let pctlname=&pctlname p&PercentileLow;
   %end;
%if &PercentileHigh ne &null %then %do;
	 %let pctlpts=&pctlpts &PercentileHigh;
	 %let pctlname=&pctlname p&PercentileHigh;
	 %end;
		
proc univariate data=&data noprint;
%if &by ne &null %then %do;
	class &by;
	%end;
var	%do i=1 %to &nvars; &&var&i %end;;
output out=_univ
			 
       pctlpts=&pctlpts
       pctlpre=%do i=1 %to &nvars; var&i._ %end;
       pctlname=&pctlname
       ;
run;

%if &by ne &null %then %do;
	proc sort data=_univ;
	by %do i=1 %to &nbv; &&bv&i %end;;  
	run;
	%end;
	

data _null_;
	set _univ;
	file &filename;

	%if &by ne &null %then %do;
	   by %do i=1 %to &nbv; &&bv&i %end;;  
	   if first.&&bv&nbv then do;
	      put "if " @;
	      %do i=1 %to &nbv;
	      	 %if &i>1 %then %do;
	      	    put " and " @;
	      	    %end;
	         %if %qupcase(%VarType(&&bv&i))=%quote(C) %then %do;
	            put "&&bv&i='" &&bv&i "'" @;
	            %end;
	         %else %do;
	         	  put "&&bv&i=" &&bv&i @;
	            %end;
	         %end;
	      put " then do;";
	      end;
	   %end;
	  
	%do k=1 %to &nvars;
		 %if &by ne &null %then %do;
	      put "   " @;
	      %end;
	   %if &PercentileLow ne &null and &PercentileHigh ne &null %then %do;
	      if var&k._p&PercentileHigh>var&k._p75 then 
	         put "&&var&k..&VariableSuffix.=max(" var&k._p&PercentileLow +(-1) ",min(" var&k._p&PercentileHigh ",&&var&k));";
	      else put "&&var&k..&VariableSuffix.=max(" var&k._p&PercentileLow +(-1) ",&&var&k);";
	      %end;
	   %else %if &PercentileLow=&null and &PercentileHigh ne &null %then %do;
	      if var&k._p&PercentileHigh>var&k._p75 then 
	         put "&&var&k..&VariableSuffix.=min(" var&k._p&PercentileHigh +(-1) ",&&var&k);";
	      else put +(-3) @;
	      %end;
	   %else %do;
	      put "&&var&k..&VariableSuffix.=max(" var&k._p&PercentileLow +(-1) ",&&var&k);";
	      %end;
	   %end;
	   
	%if &by ne &null %then %do;
	   if last.&&bv&nbv then do;
	   	  put "   end;";
	      end;
	   %end;   

	run;
		
%mend;							
							 