     ‚*---------------------------------------------------------------*
     ‚* CHKSRC-VARIABLEN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  24.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  CHKSRC-VARIABLEN                             *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       01  COBOL-CMD-TAB.
        05 TAB-REC                       OCCURS 1000 TIMES.
         07 COMMAND                      PIC X(30).
         07 INAKTIV                      PIC 9(1).
         07 LAENGE                       PIC 9(4).
         07 HEXWRK                       PIC X(1).

       01  COBOL-SYS-TAB.
        05 TAB-REC                       OCCURS 1000 TIMES.
         07 COMMAND                      PIC X(30).
         07 INAKTIV                      PIC 9(1).
         07 LAENGE                       PIC 9(4).
         07 HEXWRK                       PIC X(1).

       01  COBOL-EUP-TAB.
        05 TAB-REC                       OCCURS 1000 TIMES.
         07 COMMAND                      PIC X(30).
         07 INAKTIV                      PIC 9(1).
         07 LAENGE                       PIC 9(4).
         07 HEXWRK                       PIC X(1).

       01  COBOL-OTH-TAB.
        05 TAB-REC                       OCCURS 1000 TIMES.
         07 COMMAND                      PIC X(30).
         07 INAKTIV                      PIC 9(1).
         07 LAENGE                       PIC 9(4).
         07 HEXWRK                       PIC X(1).
     ‚*€
