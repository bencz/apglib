       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0030.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* METADATEN HINZUFÜGEN                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  01.09.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  METADATEN JE SOURCETYP HINZUFÜGEN            *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0030".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.
       01  SRCSEQ-WRK                    LIKE SRCSEQ OF SRC-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-SRCTYP                      LIKE SRCTYPE OF RTVMBRD-REC.
       01  X-SOURCE-PTR                  USAGE POINTER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-SRCTYP
                                X-SOURCE-PTR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0030"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* DATUM ERMITTELN
           PERFORM  COPY-GET-TIME.

           EVALUATE X-SRCTYP
                    WHEN "CBL"
                    WHEN "CBLLE"
                    WHEN "SQLCBLLE"
                    WHEN "DSPF"
                    WHEN "PRTF"
                         PERFORM ADD-META-AS400
                    WHEN "C"
                    WHEN "CPP"
                         PERFORM ADD-META-C
           END-EVALUATE.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* META-DATEN FÜR ALLE SOURCEN MIT * AN DER 7 STELLE ALS KOMMENT.
     ‚*--------------------------------------------------------------
       ADD-META-AS400 SECTION.
       AMT-AS4-00.

     ‚* EINLEITENDER TAG
           INITIALIZE SRC-WRK, SRCSEQ-WRK.
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "      *M*"                   DELIMITED BY SIZE
                    "<ISEU-META>"                 DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* USERNAME
           INITIALIZE SRC-WRK.
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "      *M*"                   DELIMITED BY SIZE
                    " <USER>"                     DELIMITED BY SIZE
                    T-USER                        DELIMITED BY SIZE
                    "</USER>"                     DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* DATUM
           INITIALIZE SRC-WRK.
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "      *M*"                   DELIMITED BY SIZE
                    " <DATE>"                     DELIMITED BY SIZE
                    DATE-8                        DELIMITED BY SIZE
                    "</DATE>"                     DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* ZEIT
           INITIALIZE SRC-WRK.
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "      *M*"                   DELIMITED BY SIZE
                    " <TIME>"                     DELIMITED BY SIZE
                    TIME-6                        DELIMITED BY SIZE
                    "</TIME>"                     DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* ABSCHLUSS-TAG
           INITIALIZE SRC-WRK.
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "      *M*"                   DELIMITED BY SIZE
                    "</ISEU-META>"                DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

       AMT-AS4-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* META-DATEN FÜR ALLE SOURCEN MIT * AN DER 7 STELLE ALS KOMMENT.
     ‚*--------------------------------------------------------------
       ADD-META-C SECTION.
       AME-TAC-00.

     ‚* EINLEITENDER TAG
           INITIALIZE SRC-WRK, SRCSEQ-WRK.
           ADD      0,01                          TO SRCSEQ-WRK.
           MOVE     "     /*<ISEU-META>*/"        TO SRCDTA OF SRC-WRK.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* USERNAME
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "     /*"                     DELIMITED BY SIZE
                    " <USER>"                     DELIMITED BY SIZE
                    T-USER                        DELIMITED BY SIZE
                    "</USER>*/"                   DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* DATUM
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "     /*"                     DELIMITED BY SIZE
                    " <DATE>"                     DELIMITED BY SIZE
                    DATE-8                        DELIMITED BY SIZE
                    "</DATE>*/"                   DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* ZEIT
           ADD      0,01                          TO SRCSEQ-WRK.
           STRING   "     /*"                     DELIMITED BY SIZE
                    " <TIME>"                     DELIMITED BY SIZE
                    TIME-6                        DELIMITED BY SIZE
                    "</TIME>*/"                   DELIMITED BY SIZE
               INTO SRCDTA OF SRC-WRK
           END-STRING.
           PERFORM  SCHREIBE-MAPDTA.

     ‚* ABSCHLUSS-TAG
           INITIALIZE SRC-WRK.
           ADD      0,01                          TO SRCSEQ-WRK.
           MOVE     "     /*</ISEU-META>*/"       TO SRCDTA OF SRC-WRK.
           PERFORM  SCHREIBE-MAPDTA.

       AME-TAC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAPDTA SCHREIBEN
     ‚*--------------------------------------------------------------
       SCHREIBE-MAPDTA SECTION.
       SCR-MAP-00.

     ‚* ZEILENNUMMER ÜBERNEHMEN
           MOVE     SRCSEQ-WRK         TO SRCSEQ OF SRC-WRK.
           MOVE     DATE-8(3:)         TO SRCDAT OF SRC-WRK.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
     ‚* DAMIT DIESE ZEILEN IN DER SORTIERUNG GANZ HINTEN ERSCHEINEN
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     SRC-WRK            TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR.
           PERFORM  COPY-CALL-MAPDTA.

       SCR-MAP-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
