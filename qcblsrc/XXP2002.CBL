       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP2002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF EINER API "RETRIEVE CALL STACK (QWVRCSTK) API"         *
     ‚* HTTP://PUBLIB.BOULDER.IBM.COM/ISERIES/V5R2/IC2928/INFO/APIS/  *
     ‚* QWVRCSTK.HTM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  17.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSFÜHREN DER API UND ERMITTELN STACK        *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP2002".
       01  PGM-TYP                       PIC X(8)  VALUE "API".

     ‚* VARIABLEN FÜR AUFRUF QWVRCSTK
       01  RETVAR                        PIC X(4000).
       01  FMTNAME                       PIC X(8) VALUE "CSTK0100".
       01  BIN-4STELLIG                  PIC 9(8) BINARY VALUE 1000.
       01  BIN-4ALPHA REDEFINES BIN-4STELLIG PIC X(4).
       01  OFFSET                        PIC S9(9) BINARY.
       01  LEN                           PIC S9(9) BINARY.

       01  JOB-FORMAT                    PIC X(8) VALUE "JIDF0100".

       01  ERROR-REC.
        05 BYTESPROV                     PIC 9(8) BINARY.
        05 BYTESAVAIL                    PIC 9(8) BINARY.
        05 EXCEPTID                      PIC X(7).
        05 FILLER1                       PIC X(1).
        05 FILLER2                       PIC X(16).

     ‚* KOPIERT AUS QSYSINC/QCBLLESRC/QWVRCSTK
       01  QWV-CSTK0100.
        05 BYTES-RETURNED                PIC S9(9) BINARY.
        05 BYTES-AVAILABLE               PIC S9(9) BINARY.
        05 ENTRY-AVAILABLE               PIC S9(9) BINARY.
        05 ENTRY-OFFSET                  PIC S9(9) BINARY.
        05 ENTRY-RETURNED                PIC S9(9) BINARY.
        05 RETURNED-THREAD-ID            PIC X(8).
        05 INFORMATION-STATUS            PIC X(1).

     ‚* KOPIERT AUS QSYSINC/QCBLLESRC/QWVRCSTK
       01  QWV-RCSTK-ENTRY.
        05 ENTRY-LENGTH                  PIC S9(9) BINARY.
        05 STMT-DISPLACEMENT             PIC S9(9) BINARY.
        05 STMT-RETURNED                 PIC S9(9) BINARY.
        05 PROC-DISPLACEMENT             PIC S9(9) BINARY.
        05 PROC-LENGTH                   PIC S9(9) BINARY.
        05 REQUEST-LEVEL                 PIC S9(9) BINARY.
        05 PROGRAM-NAME                  PIC X(10).
        05 PROGRAM-LIBRARY               PIC X(10).
        05 INSTRUCTION                   PIC S9(9) BINARY.
        05 MODULE-NAME                   PIC X(10).
        05 MODULE-LIBRARY                PIC X(10).
        05 CONTROL-BDY                   PIC X(1).
        05 RESERVED                      PIC X(3).
        05 ACT-GROUP-NUMBER              PIC 9(9) BINARY.
        05 ACT-GROUP-NAME                PIC X(10).
        05 RESERVED-2                    PIC X(2).
        05 PROGRAM-ASP-NAME              PIC X(10).
        05 PROGRAM-LIBRARY-ASP-NAME      PIC X(10).
        05 PROGRAM-ASP-NUMBER            PIC S9(9) BINARY.
        05 PROGRAM-LIBRARY-ASP-NUMBER    PIC S9(9) BINARY.
        05 ACT-GROUP-NUMBER-LONG         PIC 9(18) BINARY.

     ‚* KOPIERT AUS QSYSINC/QCBLLESRC/QWCATTR
       01  QWC-JIDF0100.
        05 JOB-NAME                      PIC X(10).
        05 USER-NAME                     PIC X(10).
        05 JOB-NUMBER                    PIC X(6).
        05 INT-JOB-ID                    PIC X(16).
        05 RESERVED                      PIC X(2).
        05 THREAD-INDICATOR              PIC S9(9) BINARY.
        05 THREAD-ID                     PIC X(8).

      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP2002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JOB-INFO WERTE MÜSSEN ANGEGEBEN WERDEN
           MOVE     LOW-VALUES     TO QWC-JIDF0100.
           MOVE     "*"            TO JOB-NAME         OF QWC-JIDF0100.
           MOVE     SPACES         TO USER-NAME        OF QWC-JIDF0100.
           MOVE     SPACES         TO JOB-NUMBER       OF QWC-JIDF0100.
           MOVE     SPACES         TO INT-JOB-ID       OF QWC-JIDF0100.
           MOVE     1              TO THREAD-INDICATOR OF QWC-JIDF0100.

     ‚* API AUFRUFEN
           CALL     "QWVRCSTK" USING RETVAR
                                     BIN-4STELLIG
                                     FMTNAME
                                     QWC-JIDF0100
                                     JOB-FORMAT
                                     ERROR-REC
           END-CALL.

     ‚* HEADER-WERTE SICHERN, HIER STEHT WO DER EIGENTLICHE STACK
     ‚* BEGINNT
           MOVE     RETVAR(1:LENGTH OF QWV-CSTK0100) TO QWV-CSTK0100.
           MOVE     ENTRY-OFFSET OF QWV-CSTK0100 TO OFFSET.
           MOVE     LENGTH OF QWV-RCSTK-ENTRY    TO LEN.

     ‚* JETZT DEN ERSTEN PASSENDEN WERT AUSLESEN, DAMIT KRIEGEN WIR
     ‚* IMMER DEN AUFRUF DES XXP2002 (ALSO DER API SELBST)
           IF       ENTRY-AVAILABLE OF QWV-CSTK0100 > 2
                    MOVE     RETVAR(1 + OFFSET:LEN) TO QWV-RCSTK-ENTRY
                    ADD      ENTRY-LENGTH OF QWV-RCSTK-ENTRY TO OFFSET
     ‚*             ... DAS GANZE MUSS MAN 2MAL MACHEN
                    MOVE     RETVAR(1 + OFFSET:LEN) TO QWV-RCSTK-ENTRY
                    ADD      ENTRY-LENGTH OF QWV-RCSTK-ENTRY TO OFFSET
           END-IF.

     ‚* JETZT DEN ERSTEN PASSENDEN WERT AUSLESEN
           IF       ENTRY-AVAILABLE OF QWV-CSTK0100 > 4
                    MOVE     RETVAR(1 + OFFSET:LEN) TO QWV-RCSTK-ENTRY
                    ADD      ENTRY-LENGTH OF QWV-RCSTK-ENTRY TO OFFSET
     ‚*             ... DAS GANZE MUSS MAN 2MAL MACHEN
                    MOVE     RETVAR(1 + OFFSET:LEN) TO QWV-RCSTK-ENTRY
                    ADD      ENTRY-LENGTH OF QWV-RCSTK-ENTRY TO OFFSET
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
