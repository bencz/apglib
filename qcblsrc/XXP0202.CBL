       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0202.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LÖSCHEN EINES KD-SATZES                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LÖSCHEN EINES KÜCHENDIENSTSATZES             *
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
     ‚* PHYSISCHE DATEI KÜCHENDIENST
     ‚*-------------------------------------------------------------
           SELECT PKDPLAN-DP
                  ASSIGN       TO  DATABASE-PKDPLAN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- KÜCHENDIENST
       FD  PKDPLAN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-P.
           COPY DDS-ALL-FORMATS OF PKDPLAN.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0202".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-KDDATE                      LIKE KDDATE OF PKDPLAN-P.
       01  X-KDTYP                       LIKE KDTYP  OF PKDPLAN-P.
       01  X-KDUSER                      LIKE KDUSER OF PKDPLAN-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-KDDATE
                                X-KDTYP
                                X-KDUSER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0202" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PKDPLAN-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-KDDATE = ZEROES
            OR      X-KDTYP  = SPACES
            OR      X-KDUSER = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERST LÖSCHEN DES PROGRAMM, DANN DIE TAGS
           PERFORM  DELETE-PKDPLAN.

       ENDE.
           CLOSE                 PKDPLAN-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TAGS
     ‚*--------------------------------------------------------------
       DELETE-PKDPLAN SECTION.
       DEL-KDP-00.

     ‚* DATENSATZ LÖSCHEN
           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     X-KDDATE            TO KDDATE OF PKDPLAN-P.
           MOVE     X-KDTYP             TO KDTYP  OF PKDPLAN-P.
           MOVE     X-KDUSER            TO KDUSER OF PKDPLAN-P.
           READ     PKDPLAN-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE   PKDPLAN-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE     CONST-LIB   TO LIB-WRK
                    MOVE     "PKDPLAN"   TO FILE-WRK
                    MOVE     PKDPLAN-P   TO RECORD-WRK
                    CALL     "CFP8100" USING LIB-WRK
                                             FILE-WRK
                                             PGM-WRK
                                             RECORD-WRK
                    END-CALL
           END-IF.

       DEL-KDP-90.
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
