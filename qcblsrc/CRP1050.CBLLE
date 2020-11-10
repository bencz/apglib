       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CRP1050.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN DES AUFRUFPROGRAMMS                                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.11.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN DES CHKSRC-PROGRAMMS                  *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP1050".
       01  PGM-TYP                       PIC X(8)  VALUE "CHKSRC".

       01  PCHKSRC-WRK.
           COPY DDS-ALL-FORMATS OF PCHKSRC.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-CRLFDN                      LIKE CRLFDN OF PCHKSRC-WRK.
       01  X-CRLIB                       LIKE CRLIB  OF PCHKSRC-WRK.
       01  X-CRFILE                      LIKE CRFILE OF PCHKSRC-WRK.
       01  X-CRMBR                       LIKE CRMBR  OF PCHKSRC-WRK.
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-CRLFDN
                                X-CRLIB
                                X-CRFILE
                                X-CRMBR.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP1050" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           IF       T-USER = "APG"
            AND     1 = 2
                    CALL "CRP1061" USING X-CRLFDN
                                         X-CRLIB
                                         X-CRFILE
                                         X-CRMBR
                    END-CALL
           ELSE
                    CALL "CRP1051" USING X-CRLFDN
                                         X-CRLIB
                                         X-CRFILE
                                         X-CRMBR
                    END-CALL
           END-IF.

       ENDE.
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
