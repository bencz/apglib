       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0021.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERMITTELN EINER LAUFENDEN NUMMER                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERMITTELN EINER LAUFENDEN NUMMER MIT ID      *
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
           SELECT PCFGSEQ-DP
                  ASSIGN       TO  DATABASE-PCFGSEQ
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEXTE
       FD  PCFGSEQ-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGSEQ-P.
           COPY DDS-ALL-FORMATS OF PCFGSEQ.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0021".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CFID                        LIKE CFID   OF PCFGSEQ-P.
       01  X-CFKEY                       LIKE CFKEY  OF PCFGSEQ-P.
       01  X-CFLFDN                      LIKE CFLFDN OF PCFGSEQ-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CFID
                                X-CFKEY
                                X-CFLFDN.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0021" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCFGSEQ-DP.

           INITIALIZE X-CFLFDN.
     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-CFID   = SPACES
            OR      X-CFKEY  = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERMITTELN DER LAUFENDE NUMMER
           PERFORM  ERMITTELN-LFDN.
           IF       X-CFLFDN = ZEROES
                    MOVE 1        TO X-CFLFDN
           END-IF.

       ENDE.
           CLOSE                 PCFGSEQ-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN BZW. ANLEGEN DES LFDN-SATZES S
     ‚*--------------------------------------------------------------
       ERMITTELN-LFDN SECTION.
       ERM-LFD-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

           INITIALIZE PCFGSEQF OF PCFGSEQ-P.
           MOVE     X-CFID          TO CFID   OF PCFGSEQ-P.
           MOVE     X-CFKEY         TO CFKEY  OF PCFGSEQ-P.
           READ     PCFGSEQ-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGSEQF OF PCFGSEQ-P
                    MOVE   X-CFID   TO CFID   OF PCFGSEQ-P
                    MOVE   X-CFKEY  TO CFKEY  OF PCFGSEQ-P
                    MOVE   1        TO CFLFDN OF PCFGSEQ-P
     ‚*             USER/TERMINAL
                    MOVE   PGM-WRK  TO CFAPGM OF PCFGSEQ-P
                    MOVE   T-USER   TO CFAUSR OF PCFGSEQ-P
                    MOVE   T-TERM   TO CFABS  OF PCFGSEQ-P
     ‚*             DATUM/UHRZEIT
                    MOVE   DATE-8   TO CFADTA OF PCFGSEQ-P
                    MOVE   TIME-6   TO CFATIA OF PCFGSEQ-P
                    WRITE  PCFGSEQ-P
           ELSE
                    ADD      1      TO CFLFDN OF PCFGSEQ-P
     ‚*             USER/TERMINAL
                    MOVE   PGM-WRK  TO CFMPGM OF PCFGSEQ-P
                    MOVE   T-USER   TO CFMSER OF PCFGSEQ-P
                    MOVE   T-TERM   TO CFMBS  OF PCFGSEQ-P
     ‚*             DATUM/UHRZEIT
                    MOVE   DATE-8   TO CFMDTA OF PCFGSEQ-P
                    MOVE   TIME-6   TO CFMTIA OF PCFGSEQ-P
                    REWRITE  PCFGSEQ-P
           END-IF.

     ‚* ÜBERNEHMEN DES TEXTES
           MOVE     CFLFDN OF PCFGSEQ-P TO X-CFLFDN.

       ERM-LFD-90.
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
