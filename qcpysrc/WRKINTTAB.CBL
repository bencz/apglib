     ‚*---------------------------------------------------------------*
     ‚* TYPENDEFINITION VON INTERNEN TABELLEN                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.02.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  INTERNE TABELLEN                             *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       01  PTAGLIB-COPY.
            COPY DDS-ALL-FORMATS OF PTAGLIB.
       01  PKDPLAN-COPY.
            COPY DDS-ALL-FORMATS OF PKDPLAN.
     ‚*€

       01  SELTAG-TYPE IS TYPEDEF.
        05 TAB-POS                       PIC 9(4).
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC OCCURS 500 TIMES.
         07 PTTAG         LIKE PTTAG  OF PTAGLIB-COPY VALUE SPACES.
         07 TAG-KZ                       PIC X(1).

     ‚* KÜCHENDIENST
       01  KDPLAN-TYPE IS TYPEDEF.
        05 TAB-POS                       PIC 9(4).
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC OCCURS 500 TIMES.
         07 KDTYP         LIKE KDTYP  OF PKDPLAN-COPY VALUE SPACES.
         07 MONTAG        LIKE KDUSER OF PKDPLAN-COPY VALUE SPACES.
         07 DIENSTAG      LIKE KDUSER OF PKDPLAN-COPY VALUE SPACES.
         07 MITTWOCH      LIKE KDUSER OF PKDPLAN-COPY VALUE SPACES.
         07 DONNERST      LIKE KDUSER OF PKDPLAN-COPY VALUE SPACES.
         07 FREITAG       LIKE KDUSER OF PKDPLAN-COPY VALUE SPACES.

       01  SEUCMD-TYPE IS TYPEDEF.
        05 TAB-POS                       PIC 9(4).
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC OCCURS 500 TIMES.
         07 SRCSEQ                       PIC 9(4)V9(2).
         07 SRCCMD                       PIC X(7).
         07 IS-DIRECT                    PIC 9(1).
         07 IS-BLOCK                     PIC 9(1).
         07 IS-OPEN                      PIC 9(1).
         07 IS-TARGET                    PIC 9(1).
         07 NEED-TARGET                  PIC 9(1).
         07 TARGET-SEQ                   PIC 9(4)V9(2).
     ‚*€

       01  TMPL-TAB-TYPE IS TYPEDEF.
        05 TAB-POS                       PIC 9(4).
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC OCCURS 500 TIMES.
         07 SRCSEQ                       PIC 9(4)V9(2).
         07 TMPCOMMAND                   PIC X(35).
         07 TMPVALUE                     PIC X(50).
     ‚*€

     ‚* OFFENE COPYMODULE
       01  SEU-COPY-TAB-TYPE IS TYPEDEF.
        05 COPY-REC OCCURS 100 TIMES.
         06 COPY-MOD-LIB                 PIC X(10).
         06 COPY-MOD-RCN                 PIC X(10).
         06 COPY-MOD-MBR                 PIC X(10).

     ‚* TEXT AUFTEILEN
       01  HLP0021-TAB IS TYPEDEF.
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC OCCURS 100 TIMES.
         06 ZEILE                        PIC X(100).

     ‚* KALENDERWOCHENTABELLE
       01  AZP0100-TAB IS TYPEDEF.
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC OCCURS 15 TIMES.
         06 KW                           PIC 9(2).

