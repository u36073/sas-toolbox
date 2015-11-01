/****************************************************************************************/
/*   Macro: tm_ListDedupe                                                               */
/*                                                                                      */
/*   Parameters: Positional                                                             */
/*                                                                                      */
/*      list        list of values separated by a space                                 */
/*                                                                                      */
/*   Removes any duplicate entries in a list of values.  The values can't contain       */
/*   spaces.  The macro resolves to a space separated list excluding duplicate values.  */
/*                                                                                      */
/*   Ex.                                                                                */
/*                                                                                      */
/*   %let varlist=x1 x2 score1 score2 x1 y1 z3 mailing_seq_number;                      */
/*   %let varlist=%ListDedupe(&varlist);                                                */
/****************************************************************************************/


%macro tm_ListDedupe(list);
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