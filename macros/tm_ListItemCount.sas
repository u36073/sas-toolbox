/******************************************************************************
Macro:  tm_ListItemCount

Parameters:  positional

    list        List of values separated by spaces

Returns the number of items in a space separated list.

Example:

    %let varlist=x y a1 model_score utilization debt_to_income;
    %let num_vars=%tm_ListItemCount(&varlist);

*******************************************************************************/

%macro tm_ListItemCount(list);
    %local n word flag null;
    %let null=;
    %let list=%trim(%left(&list));

    %if %quote(&list) ne %quote(&null) %then %do;
      %let flag=0; %let n=1; %let word=;
      %local item1;
      %let item1=%scan(&list,1,%str( ));
       %do %while (&flag=0);
        %let n=%eval(&n+1);
        %let word=%scan(&list,&n,%str( ));
        %if &word=&null %then %do;
            %let nwords=%eval(&n-1);
            %let flag=1;
            %end;
        %else %do;
            %local item&n;
            %let item&n=&word;
            %end;
        %end;
       %end;
    %else %do;
        %let nwords=0;
        %end;
&nwords
%mend;
