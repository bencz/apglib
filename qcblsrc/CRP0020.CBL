       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0020.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERMITTELN SRC-TYP FÜR SYNTAX-HIGHLIGHTING                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LESEN PTYPHGH                                *
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
     ‚* PHYSISCHE DATEI PTYPHGH
     ‚*-------------------------------------------------------------
           SELECT PTYPHGH-DP
                  ASSIGN       TO  DATABASE-PTYPHGH
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- KONFIGURATION
       FD  PTYPHGH-DP
           LABEL RECORDS ARE STANDARD.
       01  PTYPHGH-P.
           COPY DDS-ALL-FORMATS OF PTYPHGH.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0020".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE TYUSER OF PTYPHGH-P.
       01  X-SRC-FROM                    LIKE TYTYP  OF PTYPHGH-P.
       01  X-SRC-TO                      LIKE TYOVR  OF PTYPHGH-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-SRC-FROM
                                X-SRC-TO.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0020" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           OPEN     INPUT PTYPHGH-DP.

           INITIALIZE PTYPHGHF OF PTYPHGH-P.
           MOVE     X-USER        TO TYUSER OF PTYPHGH-P.
           MOVE     X-SRC-FROM    TO TYTYP  OF PTYPHGH-P.
           READ     PTYPHGH-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE TYOVR  OF PTYPHGH-P TO X-SRC-TO
           ELSE
                    MOVE X-SRC-FROM          TO X-SRC-TO
           END-IF.

       ENDE.
           CLOSE    PTYPHGH-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
