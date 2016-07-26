%macro tm_DatasetVariableReport(data=,    
                                by=,                                                    
                                output_pdf_file=
                                );

proc contents data=&data out=_contents noprint;
run;

data _null_;
    set _contents end=last;
    
    call symput(cats('var',_n_),strip(name));
    call symput(cats('type',_n_),strip(type));
    call symput(cats('format',_n_),strip(format));
    call symput(cats('label',_n_),strip(label));

    _datevar = format in ("YYQ", "YYQC", "YYQD", "YYQN", "YYQP", "YYQS",
                          "YYQR", "YYQRC", "YYQRD", "YYQRN", "YYQRP", "YYQRS",
                          "MMYY", "MMYYC", "MMYYD", "MMYYN", "MMYYP", "MMYYS",
                          "YYMM", "YYMMC", "YYMMD", "YYMMN", "YYMMP", "YYMMS",
                          "DDMMYY", "DDMMYYB", "DDMMYYC", "DDMMYYD", "DDMMYYN", "DDMMYYP", "DDMMYYS",
                          "MMDDYY", "MMDDYYB", "MMDDYYC", "MMDDYYD", "MMDDYYN", "MMDDYYP", "MMDDYYS",
                          "YYMMDD", "YYMMDDB", "YYMMDDC", "YYMMDDD", "YYMMDDN", "YYMMDDP", "YYMMDDS",
                          "DATE", "HEBDATE", "HDATE", "MINGUO", "YYMON", "NENGO", "DAY",
                          "DOWNAME", "EURDFDN", "EURDFDWN", "EURDFMN", "EURDFWDX", "EURDFWKX", "MONNAME",
                          "MONTH", "NLDATEMN", "NLDATEW", "NLDATWN", "QTR", "QTRR", "WEEKDATE", "WEEKDATX",
                          "WEEKDAY", "WEEKU", "WEEKV", "WEEKW", "WORDDATE", "WORDDATX", "YEAR",
                          "EURDFDE", "EURDFDD", "EURDFMY", "NLDATE", "MONYY", "JULDAY", "JULIAN");
    
    _timevar = format in ("HHMM", "HOUR", "NLTIMAP", "NLTIME", "TIME", "TIMEAMPM",
                          "MMSS", "MMSSC", "MMSSD", "MMSSN", "MMSSP", "MMSSS", "TOD");
                          
    _dtvar = format in ("DATETIME", "EURDFDT", "DTYYQC", "DATEAMPM", "DTDATE",
                        "DTYYQC", "NLDATMW", "NLDATM", "NLDATMAP", "NLDATMTM",
                        "NLTIMAP", "DTMONYY", "DTWKDATX", "DTYEAR");
                              
    call symput(cats('datevar',_n_),strip(_datevar));
    call symput(cats('timevar',_n_),strip(_timevar));
    call symput(cats('dtvar',_n_),strip(_dtvar));    
    
    if last then do;
    	call symput('nvars',strip(_n_));
    	end;                                                
    run;
                                
%mend;                                
                                