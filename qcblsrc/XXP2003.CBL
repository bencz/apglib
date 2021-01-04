       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP2003.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF EINER API "SEND MESSAGE (QEZSNDMG) API"                *
     ‚* HTTP://PUBLIB.BOULDER.IBM.COM/ISERIES/V5R2/IC2928/INFO/APIS/  *
     ‚* QEZSNDMG.HTM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SENDEN EINER NACHRICHT AN EINEN USER         *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP2003".
       01  PGM-TYP                       PIC X(8)  VALUE "API".

       01  ERROR-REC.
        05 BYTESPROV                     PIC 9(8) BINARY.
        05 BYTESAVAIL                    PIC 9(8) BINARY.
        05 EXCEPTID                      PIC X(7).
        05 FILLER1                       PIC X(1).
        05 FILLER2                       PIC X(16).

     ‚* VARIABLEN FÜR AUFRUF API QEZSNDMG
       01  MESSAGE-TYPE                  PIC X(10).
       01  DELIVERY-MODE                 PIC X(10).
       01  MESSAGE-TEXT                  PIC X(256).
       01  MESSAGE-LEN                   PIC S9(9) BINARY.
       01  USER-LIST                     PIC X(10).
       01  USER-LEN                      PIC S9(9) BINARY.
       01  MESSAGE-SENT-INDIC            PIC S9(9) BINARY.
       01  FUNCTION-REQUEST              PIC S9(9) BINARY.
       01  SHOW-DISPLAY                  PIC X(1).
       01  QUEUE-NAME                    PIC X(20).
       01  NAME-TYPE-INDIC               PIC X(4).

      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        PIC X(10).
       01  X-MESSAGE                     PIC X(256).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-MESSAGE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP2003" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* STANDARDWERTE SETZEN
           MOVE     "*INFO"          TO MESSAGE-TYPE.
           MOVE     "*BREAK"         TO DELIVERY-MODE.
           MOVE     256              TO MESSAGE-LEN.
           MOVE     1                TO USER-LEN.
           MOVE     ZEROES           TO MESSAGE-SENT-INDIC.
           MOVE     ZEROES           TO FUNCTION-REQUEST.
           MOVE     "N"              TO SHOW-DISPLAY.
           MOVE     SPACES           TO QUEUE-NAME.
           MOVE     "*USR"           TO NAME-TYPE-INDIC.

     ‚* PARAMETER ÜBERNEHMEN
           MOVE     X-MESSAGE        TO MESSAGE-TEXT.
           MOVE     X-USER           TO USER-LIST.

     ‚* API AUFRUFEN
           CALL     "QEZSNDMG" USING MESSAGE-TYPE
                                     DELIVERY-MODE
                                     MESSAGE-TEXT
                                     MESSAGE-LEN
                                     USER-LIST
                                     USER-LEN
                                     MESSAGE-SENT-INDIC
                                     FUNCTION-REQUEST
                                     ERROR-REC
                                     SHOW-DISPLAY
                                     QUEUE-NAME
                                     NAME-TYPE-INDIC
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
