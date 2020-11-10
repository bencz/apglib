       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0213.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB BESTELLUNG VORHANDEN IST                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN AUF OFFENE BESTELLUNG                 *
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
     ‚* PKDBST1L2: KBTYP, KBBST1
     ‚*-------------------------------------------------------------
           SELECT PKDBST1-DL
                  ASSIGN       TO  DATABASE-PKDBST1L2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PKDBST1L2: KBTYP, KDBST1
       FD  PKDBST1-DL
           LABEL RECORDS ARE STANDARD.
       01  PKDBST1-L.
           COPY DDS-ALL-FORMATS OF PKDBST1L2.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0213".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-RETCODE                     LIKE RET-CODE.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0213" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     INPUT PKDBST1-DL.

           INITIALIZE X-RETCODE.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* LESEN DER PKDBST1
           INITIALIZE PKDBST1F OF PKDBST1-L.
           MOVE     "KD"        TO KBTYP  OF PKDBST1-L.
           MOVE     SPACES      TO KBBST1 OF PKDBST1-L.
           START    PKDBST1-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

       ANF020.
           READ     PKDBST1-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           IF       KBTYP  OF PKDBST1-L NOT = "KD"
                    GO TO ENDE
           END-IF.

     ‚* DATENSATZ VORHANDEN
           MOVE     "1" TO X-RETCODE.

       ENDE.
           CLOSE                 PKDBST1-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
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
