       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0041.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LÖSCHEN EINES BERICHTSHEFTSATZES                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LÖSCHEN EINES PAZUBER-SATZES                 *
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
     ‚* PAZUBER: ABJAHR, ABKW, ABUSER
     ‚*-------------------------------------------------------------
           SELECT PAZUBER-DP
                  ASSIGN       TO  DATABASE-PAZUBER
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*- PAZUBER: ABJAHR, ABKW, ABUSER
       FD  PAZUBER-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUBER-P.
           COPY DDS-ALL-FORMATS OF PAZUBER.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0041".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-ABJAHR                      LIKE ABJAHR OF PAZUBER-P.
       01  X-ABKW                        LIKE ABKW   OF PAZUBER-P.
       01  X-ABUSER                      LIKE ABUSER OF PAZUBER-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-ABJAHR
                                X-ABKW
                                X-ABUSER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0041" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PAZUBER-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-ABJAHR = ZEROES
            OR      X-ABKW   = ZEROES
            OR      X-ABUSER = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERST LÖSCHEN DES PROGRAMM, DANN DIE TAGS
           PERFORM  DELETE-PAZUBER.

       ENDE.
           CLOSE                 PAZUBER-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DES RECORDS
     ‚*--------------------------------------------------------------
       DELETE-PAZUBER SECTION.
       DEL-AZU-00.

     ‚* DATENSATZ LÖSCHEN
           INITIALIZE PAZUBERF OF PAZUBER-P.
           MOVE     X-ABJAHR            TO ABJAHR OF PAZUBER-P.
           MOVE     X-ABKW              TO ABKW   OF PAZUBER-P.
           MOVE     X-ABUSER            TO ABUSER OF PAZUBER-P.
           READ     PAZUBER-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE   PAZUBER-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE     CONST-LIB   TO LIB-WRK
                    MOVE     "PAZUBER"   TO FILE-WRK
                    MOVE     PAZUBER-P   TO RECORD-WRK
                    CALL     "CFP8100" USING LIB-WRK
                                             FILE-WRK
                                             PGM-WRK
                                             RECORD-WRK
                    END-CALL
           END-IF.

       DEL-AZU-90.
           EXIT.
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
