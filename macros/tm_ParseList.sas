/******************************************************************************
Macro:  tm_ParseList

Parameters:  positional

    prefix  Prefix to use for macro variables containing the name of each
             member in the list

    list    List of values separated by spaces

Parses a list of text or numeric values separated by a space and assigns each
value to a macro variable with the prefix specified in the [prefix] parameter
and an integer suffix corresponding to its position in the list.

The macro itself resolves to the number of values in the list. An additional macro
variable is also created in the global environment of the form &prefix._csv.
It contains the list of values separated by a comma.

Example:

    %let varlist=x y a1 model_score utilization debt_to_income;
    %let num_vars=%tm_ParseList(var,&varlist);

    data .........;
        %do i=1 %to &num_vars;
            &&var&i=round(&&var&i,.00001);
            %end;
        run;
*******************************************************************************/

%macro tm_ParseList(prefix,list);
    %local n word flag null i nwords;
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
