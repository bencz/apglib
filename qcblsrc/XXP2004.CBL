       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP2004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF EINER API "EXECUTE COMMAND (QCMDEXC) API"              *
     ‚* HTTP://PUBLIB.BOULDER.IBM.COM/ISERIES/V5R2/IC2928/INFO/APIS/  *
     ‚* QCMDEXC.HTM                                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BEFEHL AUSFÜHREN                             *
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
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP2004".
       01  PGM-TYP                       PIC X(8)  VALUE "API".

     ‚* VARIABLEN FÜR AUFRUF API QEZSNDMG
       01  EXECUTE-WRK                  PIC X(200).
       01  EXECUTE-LEN                  PIC 9(10)V9(5) COMP-3 VALUE 52.

      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-COMMAND                     PIC X(200).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-COMMAND.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP2004" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* STANDARDWERTE SETZEN
           INITIALIZE EXECUTE-WRK.
           INITIALIZE EXECUTE-LEN.

     ‚* PARAMETER ÜBERNEHMEN
           MOVE     X-COMMAND        TO EXECUTE-WRK.
           INSPECT  EXECUTE-WRK TALLYING EXECUTE-LEN FOR CHARACTERS.

     ‚* API AUFRUFEN
           CALL     "QCMDEXC" USING EXECUTE-WRK
                                    EXECUTE-LEN
           END-CALL.

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
