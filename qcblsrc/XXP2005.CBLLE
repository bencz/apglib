       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP2005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF EINER API "RETRIEVE MEMBER DESCRIPTION (QUSRMBRD) API" *
     ‚* HTTP://PUBLIB.BOULDER.IBM.COM/ISERIES/V5R2/IC2928/INFO/APIS/  *
     ‚* QUSRMBRD.HTM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  RTVMBRD ALS API                              *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP2005".
       01  PGM-TYP                       PIC X(8)  VALUE "API".

     ‚* PARAMETER FÜR API
       01  LIB-WRK                       PIC X(10).
       01  FILE-WRK                      PIC X(10).
       01  MBR-WRK                       PIC X(10).
       01  LIB-FILE                      PIC X(20).

       01  RCVLEN                        PIC 9(8) BINARY.
       01  RCVFMT                        PIC X(8) VALUE "MBRD0100".
       01  OVRPRO                        PIC X(1) VALUE "0".

       01  QDB-MBRD0100.
           05  BYTES-RETURNED            PIC S9(00009) BINARY          .
           05  BYTES-AVAILABLE           PIC S9(00009) BINARY          .
           05  DB-FILE-NAME              PIC X(00010).
           05  DB-FILE-LIB               PIC X(00010).
           05  MEMBER-NAME               PIC X(00010).
           05  FILE-ATTR                 PIC X(00010).
           05  SRC-TYPE                  PIC X(00010).
           05  CRT-DATE                  PIC X(00013).
           05  CHANGE-DATE               PIC X(00013).
           05  TEXT-DESC                 PIC X(00050).
           05  SRC-FILE                  PIC X(00001).

     ‚* FEHLER STRUKTUR
       01  ERROR-REC.
           05 BYTESPROV                  PIC 9(8) BINARY.
           05 BYTESAVAIL                 PIC 9(8) BINARY.
           05 EXCEPTID                   PIC X(7).
           05 FILLER1                    PIC X(1).
           05 FILLER2                    PIC X(16).

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-RTVMBRD                     LIKE RTVMBRD-REC.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-RTVMBRD
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-RETCODE.
     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP2005" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÜBERHAUPT WAS VORHANDEN?
           IF       X-RTVMBRD = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÜBERNEHMEN IN RTVMBRD-REC.
           INITIALIZE RTVMBRD-REC.
           MOVE     X-RTVMBRD TO RTVMBRD-REC.

     ‚* DATEN ZUM AUFRUF DES API FÜLLEN.
           MOVE     LENGTH   OF QDB-MBRD0100 TO RCVLEN.
           MOVE     OBJLIB   OF RTVMBRD-REC  TO LIB-WRK.
           MOVE     FILENAME OF RTVMBRD-REC  TO FILE-WRK.
           MOVE     MBRNAME  OF RTVMBRD-REC  TO MBR-WRK.
           MOVE     16     TO BYTESPROV OF ERROR-REC.
           MOVE     SPACES TO EXCEPTID  OF ERROR-REC.

     ‚* QUALIFIZIERTEN NAMEN AUFBAUEN
           STRING   FILE-WRK   DELIMITED BY SIZE
                    LIB-WRK    DELIMITED BY SIZE
           INTO     LIB-FILE.

     ‚* API AUFRUFEN
           CALL     "QUSRMBRD" USING QDB-MBRD0100
                                     RCVLEN
                                     RCVFMT
                                     LIB-FILE
                                     MBR-WRK
                                     OVRPRO
                                     ERROR-REC
           END-CALL.
           IF       EXCEPTID OF ERROR-REC NOT = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* MAPPEN AUF RTVMBRD-REC.
           INITIALIZE RTVMBRD-REC, X-RTVMBRD.
           MOVE     DB-FILE-NAME OF QDB-MBRD0100 TO
                    FILENAME     OF RTVMBRD-REC.
           MOVE     DB-FILE-LIB  OF QDB-MBRD0100 TO
                    OBJLIB       OF RTVMBRD-REC.
           MOVE     MEMBER-NAME  OF QDB-MBRD0100 TO
                    MBRNAME      OF RTVMBRD-REC.
           MOVE     SRC-TYPE     OF QDB-MBRD0100 TO
                    SRCTYPE      OF RTVMBRD-REC.
           MOVE     CRT-DATE     OF QDB-MBRD0100 TO
                    CRTDATE      OF RTVMBRD-REC.
           MOVE     CHANGE-DATE  OF QDB-MBRD0100 TO
                    CHGDATE      OF RTVMBRD-REC.
           MOVE     TEXT-DESC    OF QDB-MBRD0100 TO
                    MBRTEXT      OF RTVMBRD-REC.

     ‚* MAPPEN AUF RTVMBRD-REC.
           MOVE     RTVMBRD-REC TO X-RTVMBRD.

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
