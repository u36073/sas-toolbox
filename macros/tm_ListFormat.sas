/******************************************************************************
 Macro: tm_ListFormat

 Parameters: Positional

    List      = space separated list of values.  Values can't contain single or double
                quotes or embedded spaces.
    type      = separator to use in the list generated and stored in the macro Variable
                SPACE    space delimited list
                COMMA    comma delimited list
                QSPACE   space delimited list with double quotes around each value
                QCOMMA   comma delimited list with double quotes around each value

 Reformats a space separated list into another type of formatted list.  The macro
 resolves to the newly formatted list.
******************************************************************************/
%macro tm_ListFormat(list,type);
    %local i x nitems flist null;

    %let null=;

    %if %qupcase(&type) ne %quote(COMMA) and
            %qupcase(&type) ne %quote(QCOMMA) and
            %qupcase(&type) ne %quote(SPACE) and
            %qupcase(&type) ne %quote(QSPACE)
  %then %let type=COMMA;

    %if &list ne &null %then %do;
        %let nitems=0;
        %let x=%scan(&list,1,%str( ));
        %do %until(&x=&null);
            %let nitems=%eval(&nitems+1);
            %let x=%scan(&list,&nitems,%str( ));
            %end;
        %let nitems=%eval(&nitems-1);

        %do i=1 %to &nitems;
            %local lv&i;
            %let lv&i=%trim(%left(%upcase(%scan(&list,&i,%str( )))));
            %end;


        %if %qupcase(&type)=%quote(QCOMMA) or %qupcase(&type)=%quote(QSPACE) %then %let flist="&lv1";
        %else %let flist=&lv1;

        %if &nitems>1 %then %do;
            %do i=2 %to &nitems;
                %if %qupcase(&type)=%quote(COMMA) %then %let flist=&flist,&&lv&i;
                %if %qupcase(&type)=%quote(QCOMMA) %then %let flist=&flist,"&&lv&i";
                %if %qupcase(&type)=%quote(SPACE) %then %let flist=&flist &&lv&i;
                %if %qupcase(&type)=%quote(QSPACE) %then %let flist=&flist "&&lv&i";
                %end;
            %end;
        %end;
    %else %do;
        %let flist=&null;
        %end;
&flist
%mend;