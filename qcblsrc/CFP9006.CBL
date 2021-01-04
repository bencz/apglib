       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LOGGEN EINES PROGRAMMAUFRUFS                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  14.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PROGRAMMAUFRUF LOGGEN (PPGMLOG)              *
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

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI KONFIGURATION
     ‚*-------------------------------------------------------------
           SELECT PPGMLOG-DP
                  ASSIGN       TO  DATABASE-PPGMLOG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEXTE
       FD  PPGMLOG-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLOG-P.
           COPY DDS-ALL-FORMATS OF PPGMLOG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9006".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-LGPGM                       LIKE LGPGM  OF PPGMLOG-P.
       01  X-LGUSER                      LIKE LGUSER OF PPGMLOG-P.
       01  X-LGTYP                       PIC X(10).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-LGPGM
                                X-LGUSER
                                X-LGTYP.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP9006" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* NUR EINMAL ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     I-O   PPGMLOG-DP
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-LGPGM  = SPACES
            OR      X-LGUSER = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERMITTELN DER LAUFENDE NUMMER
           PERFORM  ADD-LOG.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN BZW. ANLEGEN DES LFDN-SATZES S
     ‚*--------------------------------------------------------------
       ADD-LOG SECTION.
       ADD-LOG-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

           INITIALIZE PPGMLOGF OF PPGMLOG-P.
           MOVE     X-LGPGM         TO LGPGM  OF PPGMLOG-P.
           MOVE     X-LGUSER        TO LGUSER OF PPGMLOG-P.
           READ     PPGMLOG-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PPGMLOGF OF PPGMLOG-P
                    MOVE   X-LGPGM  TO LGPGM  OF PPGMLOG-P
                    MOVE   X-LGUSER TO LGUSER OF PPGMLOG-P
                    IF   X-LGTYP = "EXIT"
                         MOVE   1        TO LGEFDN OF PPGMLOG-P
                    ELSE
                         MOVE   1        TO LGLFDN OF PPGMLOG-P
                    END-IF
     ‚*             USER/TERMINAL
                    MOVE   PGM-WRK  TO LGAPGM OF PPGMLOG-P
                    MOVE   T-USER   TO LGAUSR OF PPGMLOG-P
                    MOVE   T-TERM   TO LGABS  OF PPGMLOG-P
     ‚*             DATUM/UHRZEIT
                    MOVE   DATE-8   TO LGADTA OF PPGMLOG-P
                    MOVE   TIME-6   TO LGATIA OF PPGMLOG-P
                    WRITE  PPGMLOG-P
           ELSE
                    IF   X-LGTYP = "EXIT"
                         ADD    1        TO LGEFDN OF PPGMLOG-P
                    ELSE
                         ADD    1        TO LGLFDN OF PPGMLOG-P
                    END-IF
     ‚*             USER/TERMINAL
                    MOVE   PGM-WRK  TO LGMPGM OF PPGMLOG-P
                    MOVE   T-USER   TO LGMSER OF PPGMLOG-P
                    MOVE   T-TERM   TO LGMBS  OF PPGMLOG-P
     ‚*             DATUM/UHRZEIT
                    MOVE   DATE-8   TO LGMDTA OF PPGMLOG-P
                    MOVE   TIME-6   TO LGMTIA OF PPGMLOG-P
                    REWRITE  PPGMLOG-P
           END-IF.

       ADD-LOG-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
