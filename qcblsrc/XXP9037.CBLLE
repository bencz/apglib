       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9037.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  31.01.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
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
     ‚* EHRHARDT/ULANDICL2
     ‚*-------------------------------------------------------------
           SELECT ULANDIC-DL
                  ASSIGN       TO  DATABASE-ULANDICL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- EHRHARDT/ULANDICL2
       FD  ULANDIC-DL
           LABEL RECORDS ARE STANDARD.
       01  ULANDIC-L.
           COPY DDS-ALL-FORMATS OF EHRHARDT-ULANDICL2.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9037".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9037"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     INPUT ULANDIC-DL.

           INITIALIZE PLANDICF OF ULANDIC-L.
           MOVE     "LFS"       TO LDVERK OF ULANDIC-L.
           MOVE     SPACES      TO LDVERS OF ULANDIC-L.
           MOVE     "DEU"       TO LDLANG OF ULANDIC-L.
           MOVE     "artikel"   TO LDTRAN OF ULANDIC-L.
           START    ULANDIC-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           READ     ULANDIC-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

       ENDE.
           CLOSE    ULANDIC-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
