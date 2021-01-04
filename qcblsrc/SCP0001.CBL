       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              SCP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTEN VON SCHULUNGSAUFGABEN                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  01.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TESTEN VON SCHULUNGSAUSGABEN                 *
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
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "SCP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "SCHULUNG".

       01  LEN                           PIC 9(4).
       01  STRING-WRK                    PIC X(50).

       01  STRING-FNC1                   PIC X(10).
       01  STRING-FNC2                   PIC X(10).
       01  STRING-FNC3                   PIC X(10).
       01  STRING-FUNC                   PIC X(50).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PARM                        PIC X(1).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PARM.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "SCP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* WAS IST ZU TUN?
           EVALUATE X-PARM
                    WHEN "1"
                         PERFORM INSPECT-ALL-CHAR
                    WHEN "2"
                         PERFORM REVERSE-STRING
                    WHEN "3"
                         PERFORM STRING-FUNCT
           END-EVALUATE.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN WIE OFT DAS "E" IN DEM STRING VORKOMMT
     ‚*--------------------------------------------------------------
       INSPECT-ALL-CHAR SECTION.
       INP-ALC-00.

           MOVE     SPACES TO STRING-WRK.
           MOVE     ZEROES TO LEN.
     ‚* STRING FÜLLEN
           MOVE     "DIES IST EIN TEST FÜR DEN INSPECT-BEFEHL" TO
                    STRING-WRK.
           DISPLAY  STRING-WRK.

     ‚* STRING PRÜFEN
           INSPECT  STRING-WRK TALLYING LEN FOR ALL "E".
           DISPLAY  LEN.

       INP-ALC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DEN STRING UMDREHEN
     ‚*--------------------------------------------------------------
       REVERSE-STRING SECTION.
       REV-STR-00.

           MOVE     SPACES TO STRING-WRK.
     ‚* STRING ANZEIGEN
           MOVE     "COBOL" TO STRING-WRK.
           DISPLAY  STRING-WRK.

     ‚* STRING UMDREHEN UND ANZEIGEN
           MOVE     FUNCTION REVERSE(STRING-WRK) TO STRING-WRK.
           DISPLAY  STRING-WRK.

       REV-STR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BENUTZEN DES STRING
     ‚*--------------------------------------------------------------
       STRING-FUNCT SECTION.
       STR-FNC-00.

           MOVE     SPACES TO STRING-FUNC.
           MOVE     "COBOL"      TO STRING-FNC1.
           MOVE     "HALLO WELT" TO STRING-FNC2.
           MOVE     "!"          TO STRING-FNC3.
           STRING   STRING-FNC1 DELIMITED BY SIZE
                    STRING-FNC2 DELIMITED BY SPACES
                    STRING-FNC3 DELIMITED BY SPACES
           INTO     STRING-FUNC.
           DISPLAY  STRING-FUNC.

       STR-FNC-90.
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
