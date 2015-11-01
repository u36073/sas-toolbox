%macro tm_gcodelin(score=,
                code=,
                by=,
			    by_rename_to=,
                var=,
                append=n,
                uselabels=n,
                language=sas,   /* SAS or C# */
                model_type=OLS, /* OLS = Ordinary Least Squares, IPC = Incomplete Principal Component */                                                                
                output=m,       /* m=model v=variable list */
                replace1=,
                with1=,
                replace2=,
                with2=,
                replace3=,
                with3=
                );
    %local i;
    %local k;
    %local drop_vars;
    %let null=;
        
    %if %qupcase(&model_type)=%quote(IPC) %then %do;
       proc sort data=&score(where=(_type_='IPC')) out=__score;
       by &by _type_ _pcomit_;
       run;
       
       data __score;
          set __score;
          by &by _type_ _pcomit_;
          if first._type_;
          run;
       %end;   
    %else %do;
       data __score;
          set &score;
          if _type_='PARMS';
          run;
       %end; 
       
    %let drop_vars=;
    %if %tm_VarInDataset(_rmse_,__score)=1 %then %let drop_vars=&drop_vars _rmse_;
    %if %tm_VarInDataset(_type_,__score)=1 %then %let drop_vars=&drop_vars _type_;
    %if %tm_VarInDataset(_depvar_,__score)=1 %then %let drop_vars=&drop_vars _depvar_;
    %if %tm_VarInDataset(_model_,__score)=1 %then %let drop_vars=&drop_vars _model_;
    %if %tm_VarInDataset(_pcomit_,__score)=1 %then %let drop_vars=&drop_vars _pcomit_;
    %if %tm_VarInDataset(_ridge_,__score)=1 %then %let drop_vars=&drop_vars _ridge_;
    
        
    proc contents data=__score(drop=&drop_vars intercept &by) noprint out=contents;
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
        
  %let __vlist=;
  %if %qupcase(&output)=%quote(V) %then %do;  
     
     proc sql;
     create table __vlist1 as 
     select 
     count(*) as ___nobs____
     %do i=1 %to &nvar;
        ,sum(case when &&var&i not in (.,-1) then 1 else 0 end) as &&var&i
        %end;
     from __score
     ;quit;
     
     data _null_;
        set __vlist1;
        length vlist $ 10000;
        vlist='';
        %do i=1 %to &nvar;
          if &&var&i>0 then vlist=trim(left(vlist))||" %trim(%left(%lowcase(&&var&i)))";
          %end;
        call symput("&var",trim(left(vlist)));
        run;
     %goto Exit;
     %end;          
 
  %if %qupcase(&uselabels)=%quote(Y) %then %let rtarget=label;
  %else %let rtarget=name;
            
    data contents;
            set contents;
            %if %quote(&replace1) ne %quote(&null) %then %do;
               &rtarget=trim(left(upcase(&rtarget)));
               _replace1=trim(left(upcase("&replace1")));
               _with1=trim(left(upcase("&with1")));
               _pos=-1;
               do until(_pos=0);
                _pos=index(&rtarget,_replace1);
                if _pos>0 then substr(&rtarget,_pos,length(_replace1))=_with1;
                end;
               %end;
            
            %if %quote(&replace1) ne %quote(&null) %then %do;
               _replace2=trim(left(upcase("&replace2")));
               _with2=trim(left(upcase("&with2")));
               _pos=-1;
               do until(_pos=0);
                _pos=index(&rtarget,_replace2);
                if _pos>0 then substr(&rtarget,_pos,length(_replace2))=_with2;
                 end;
               %end;
               
            %if %quote(&replace1) ne %quote(&null) %then %do;
               _replace3=trim(left(upcase("&replace3")));
               _with3=trim(left(upcase("&with3")));
               _pos=-1;
               do until(_pos=0);
                _pos=index(&rtarget,_replace2);
                if _pos>0 then substr(&rtarget,_pos,length(_replace2))=_with2;
                 end;   
               %end;        
            run;

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
            set __score(drop=&drop_vars) end=last;
            %if %quote(&var) ne %quote(&null) %then %do;
                put "&var = " intercept;
                %end;
            %else %do;
                put 'p_' _depvar_ +(-1) " = " intercept;
                %end;
            %do i=1 %to &nvar;
                if &&var&i not in (.,-1) then put "          + %trim(%left(%lowcase(&&rvar&i))) * " &&var&i;
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
                       		    
        proc contents data=__score(drop=&drop_vars intercept %do i=1 %to &nbv; &&by&i %end;) noprint out=contents;
        run;
        
        data contents;
    		set contents;
    		if trim(left(label))='' then label=name;
    		run;
        
        data _null_;
            set contents end=last;
            call symput('var'||trim(left(_n_)),name);
            if last then do;
                call symput('nvar',trim(left(_n_)));
                end;
            run;
            
        data contents;
            set contents;
            %if %quote(&replace1) ne %quote(&null) %then %do;
               &rtarget=trim(left(upcase(&rtarget)));
               _replace1=trim(left(upcase("&replace1")));
               _with1=trim(left(upcase("&with1")));
               _pos=-1;
               do until(_pos=0);
                _pos=index(&rtarget,_replace1);
                if _pos>0 then substr(&rtarget,_pos,length(_replace1))=_with1;
                end;
               %end;
            
            %if %quote(&replace2) ne %quote(&null) %then %do;
               _replace2=trim(left(upcase("&replace2")));
               _with2=trim(left(upcase("&with2")));
               _pos=-1;
               do until(_pos=0);
                _pos=index(&rtarget,_replace2);
                if _pos>0 then substr(&rtarget,_pos,length(_replace2))=_with2;
                 end;
               %end;
               
            %if %quote(&replace3) ne %quote(&null) %then %do;
               _replace3=trim(left(upcase("&replace3")));
               _with3=trim(left(upcase("&with3")));
               _pos=-1;
               do until(_pos=0);
                _pos=index(&rtarget,_replace3);
                if _pos>0 then substr(&rtarget,_pos,length(_replace3))=_with3;
                 end;   
               %end;        
            run;    
                
        data _null_;
        set contents end=last;
        call symput('rvar'||trim(left(_n_)),&rtarget);
        run;  
        
        data _byvars;
        	length &rtarget $ 32;
            %do i=1 %to &nbv;           
               &rtarget=trim(left(upcase("&&byrt&i")));
               %if %quote(&replace1) ne %quote(&null) and %quote(&by_rename_to)=%quote(&null) %then %do;
                   &rtarget=trim(left(upcase(&rtarget)));
                   _replace1=trim(left(upcase("&replace1")));
                   _with1=trim(left(upcase("&with1")));
                   _pos=-1;
                   do until(_pos=0);
                       _pos=index(&rtarget,_replace1);
                       if _pos>0 then substr(&rtarget,_pos,length(_replace1))=_with1;
                       end;
                   %end;
                
              %if %quote(&replace2) ne %quote(&null) and %quote(&by_rename_to)=%quote(&null) %then %do;
                  _replace2=trim(left(upcase("&replace2")));
                  _with2=trim(left(upcase("&with2")));
                  _pos=-1;
                  do until(_pos=0);
                      _pos=index(&rtarget,_replace2);
                      if _pos>0 then substr(&rtarget,_pos,length(_replace2))=_with2;
                      end;
                 %end;
                 
              %if %quote(&replace3) ne %quote(&null) and %quote(&by_rename_to)=%quote(&null) %then %do;
                 _replace3=trim(left(upcase("&replace3")));
                 _with3=trim(left(upcase("&with3")));
                 _pos=-1;
                 do until(_pos=0);
                  _pos=index(&rtarget,_replace3);
                  if _pos>0 then substr(&rtarget,_pos,length(_replace3))=_with3;
                   end;   
                 %end;    
               output;
              %end;   
          run;        

     data _null_;
        set _byvars end=last;
        call symput('rby'||trim(left(_n_)),trim(left(&rtarget)));
        run;            
     
     proc sort data=__score;
     by &by;
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
            set __score(drop=&drop_vars) end=last;
            if _n_=1 then do;
                put "     if " @;
                end;
            else do;
            	%if %qupcase(&language)=%quote(SAS) %then %do;
                	put "     else if " @;
                	%end;
                %else %if %qupcase(&language)=%quote(C#) %then %do;
                    put "     else if " @;
                    %end;
                end;
            %do i=1 %to &nbv;
                %if %qupcase(&language)=%quote(SAS) %then %do;
                   %if &i=1 %then %do;            		
                   	    put "%lowcase(&&rby&i)='" &&by&i +(-1) "' " @;
                   	    %end;
                   %else %do;
                        put "and %lowcase(&&rby&i)='" &&by&i +(-1) "' " @;
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
            
            %if %quote(&var) ne %quote(&null) %then %do;
                put "        &var = " intercept;
                %end;
            %else %do;
                put "        p_" _depvar_ +(-1)" = " intercept;
                %end;           
            
            %do i=1 %to &nvar;
                if &&var&i not in (.,-1) then put "                 + %trim(%left(%lowcase(&&rvar&i))) * " &&var&i;
                %end;
 			%if %qupcase(&language)=%quote(SAS) %then %do;   
            	put "           ;";
              	put "        end;";
                %end;
            %else %if %qupcase(&language)=%quote(C#) %then %do;
                put "           ;";
              	put "           }";
                %end;              
            
            run;
        %end;       
%Exit:
%mend;



