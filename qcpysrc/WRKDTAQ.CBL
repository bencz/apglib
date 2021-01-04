     ‚* WRKFORMAT COPIED FROM PRODATV2/PDTAQ
       01  DTAQ-REF IS TYPEDEF.
         05 DTAQ-STRUCT.
     ‚* LAUFENDE NUMMER. EINDEUTIG PRO DTAQ EINTRAG.
     ‚* NUMMER WIRD IM LFSSNDDDTAQ ERMITTELT UND IN DEN DTAQ
     ‚* GESCHRIEBEN.
           10 D-LFDN                      PIC S9(9).
     ‚* ORIGINAL DTAQNAME/GRUPPEN-ID
           10 D-GRPID                     PIC X(10).
     ‚* UNTERGRUPPEN-ID
           10 D-UGRPID                    PIC X(10).
     ‚* EXTERNAL TYP (TCP/DTQ)
           10 D-ETYPE                     PIC X(3).
           10 D-DTAID                     PIC X(10).
           10 D-ACODE                     PIC X(10).
     ‚* EXTERNAL VERSION
           10 D-EVERS                     PIC X(9).
           10 D-OUTQ                      PIC X(10).
           10 D-PARM1                     PIC X(10).
           10 D-PARM2                     PIC X(10).
           10 D-PARM3                     PIC X(13).
           10 D-KONZNR                    PIC X(3).
           10 D-FIRMNR                    PIC X(3).
           10 D-ANLK                      PIC X(4).
           10 D-MFCN                      PIC X(3).
           10 D-MFCB                      PIC X(4).
           10 D-SYCC                      PIC X(4).
           10 D-SPRACHE                   PIC X(3).
           10 D-PRT-TYPE                  PIC X(1).
           10 D-ZUNR                      PIC S9(9).
           10 D-NBR                       PIC S9(4).
           10 D-FGRP                      PIC X(10).
     ‚* UHRZEIT/DATUM WANN DTAQ EINTRAG GESENDET WURDE
           10 D-TIME                      PIC S9(6).
           10 D-DATE                      PIC S9(8).
     ‚* PGM/JOB/USER/JOBNR VON DEM DER DTAQ EINTRAG GESENDET WURDE
           10 D-PGM                       PIC X(10).
           10 D-TERM                      PIC X(10).
           10 D-USER                      PIC X(10).
           10 D-JBNR                      PIC S9(6).
     ‚* KENNZEICHEN ZUR SEQUENTIELLEN ABARBEITUNG
           10 D-SQID                      PIC X(10).
           10 D-SQKEY                     PIC X(50).
     ‚* WIRD IM PGM LFSSNDDTAQ GESETZT. DIESER EINTRAG WIRD NICHT
     ‚* VERARBEITET, ABER IN DER DATEI PDTAQ PROTOKOLLIERT
           10 D-IAKZ                      PIC X(1).
           10 D-EMAIL                     PIC X(55).
           10 D-PARM4                     PIC X(182).

       01  DTAQ-REC TYPE DTAQ-REF.
