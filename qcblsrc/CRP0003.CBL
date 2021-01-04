       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0003.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERSTELLEN CR-VORGANG                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERSTELLEN EINES CR-VORGANGS                  *
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
     ‚* PHYSISCHE DATEI SYNTAXHIGHLIGHTING
     ‚*-------------------------------------------------------------
           SELECT PCHKHGH-DP
                  ASSIGN       TO  DATABASE-PCHKHGH
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- CODEREVIEW
       FD  PCHKHGH-DP
           LABEL RECORDS ARE STANDARD.
       01  PCHKHGH-P.
           COPY DDS-ALL-FORMATS OF PCHKHGH.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CODE-REVIEW VARIABLEN
           COPY WRKCRCPY   OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0003".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* FARBATTRIBUTE
       01  SYCOLR-WRK                    LIKE SYCOLR OF PCHKHGH-P.
       01  SYHEX-WRK                     LIKE SYHEX  OF PCHKHGH-P.
       01  SYNTAX-TYP                    LIKE SYTYP  OF PCHKHGH-P.
       01  COBOL-COMMAND                 LIKE SYCMD  OF PCHKHGH-P.
       01  LEN                           PIC 9(4).

     ‚* LAUFVARIABLEN
       01  TAB-COUNTER                   PIC 9(4).

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0003" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PCHKHGH-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           PERFORM  COPY-FILL-SY-DATA.
           PERFORM  FILL-COBOL.
           PERFORM  FILL-COBOL2.
           PERFORM  FILL-COBOL3.

           PERFORM  FILL-CONST.
           PERFORM  FILL-EUP.

       ENDE.
           CLOSE                 PCHKHGH-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER COBOL-WERTE
     ‚*--------------------------------------------------------------
       FILL-COBOL SECTION.
       FIL-CBL-00.

           INITIALIZE RET-CODE.
     ‚* ERSTMAL PRÜFEN OB IN DER ZEILE EIN COBOL-BEFEHL VORHANDEN
     ‚* IST, WENN JA, DANN PRÜFEN WO DIESER STEHT
           INITIALIZE TAB-COUNTER.
           MOVE     "CBLLE" TO SYNTAX-TYP.
           MOVE     "BLUE"  TO SYCOLR-WRK.

       FIL-CBL-20.
           ADD      1 TO TAB-COUNTER.
           IF       TAB-COUNTER > 1000
            OR      COMMAND OF COBOL-CMD-TAB(TAB-COUNTER) = SPACES
                    GO TO FIL-CBL-90
           END-IF.
           MOVE     COMMAND OF COBOL-CMD-TAB(TAB-COUNTER) TO
                    COBOL-COMMAND.

           PERFORM  SCHREIBE-VORGANG.
           GO TO    FIL-CBL-20.

       FIL-CBL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER COBOL-WERTE
     ‚*--------------------------------------------------------------
       FILL-COBOL2 SECTION.
       FIL-CB2-00.

           INITIALIZE RET-CODE.
     ‚* ERSTMAL PRÜFEN OB IN DER ZEILE EIN COBOL-BEFEHL VORHANDEN
     ‚* IST, WENN JA, DANN PRÜFEN WO DIESER STEHT
           INITIALIZE TAB-COUNTER.
           MOVE     "CBL"   TO SYNTAX-TYP.
           MOVE     "BLUE"  TO SYCOLR-WRK.

       FIL-CB2-20.
           ADD      1 TO TAB-COUNTER.
           IF       TAB-COUNTER > 1000
            OR      COMMAND OF COBOL-CMD-TAB(TAB-COUNTER) = SPACES
                    GO TO FIL-CB2-90
           END-IF.
           MOVE     COMMAND OF COBOL-CMD-TAB(TAB-COUNTER) TO
                    COBOL-COMMAND.

           PERFORM  SCHREIBE-VORGANG.
           GO TO    FIL-CB2-20.

       FIL-CB2-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER COBOL-WERTE
     ‚*--------------------------------------------------------------
       FILL-COBOL3 SECTION.
       FIL-CB3-00.

           INITIALIZE RET-CODE.
     ‚* ERSTMAL PRÜFEN OB IN DER ZEILE EIN COBOL-BEFEHL VORHANDEN
     ‚* IST, WENN JA, DANN PRÜFEN WO DIESER STEHT
           INITIALIZE TAB-COUNTER.
           MOVE     "SQLCBLLE" TO SYNTAX-TYP.
           MOVE     "BLUE"  TO SYCOLR-WRK.

       FIL-CB3-20.
           ADD      1 TO TAB-COUNTER.
           IF       TAB-COUNTER > 1000
            OR      COMMAND OF COBOL-CMD-TAB(TAB-COUNTER) = SPACES
                    GO TO FIL-CB3-90
           END-IF.
           MOVE     COMMAND OF COBOL-CMD-TAB(TAB-COUNTER) TO
                    COBOL-COMMAND.

           PERFORM  SCHREIBE-VORGANG.
           GO TO    FIL-CB3-20.

       FIL-CB3-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER CONST-WERTE
     ‚*--------------------------------------------------------------
       FILL-CONST SECTION.
       FIL-CNS-00.

           INITIALIZE RET-CODE.
     ‚* ERSTMAL PRÜFEN OB IN DER ZEILE EIN COBOL-BEFEHL VORHANDEN
     ‚* IST, WENN JA, DANN PRÜFEN WO DIESER STEHT
           INITIALIZE TAB-COUNTER.
           MOVE     "CONST"   TO SYNTAX-TYP.
           MOVE     "YELLOW"  TO SYCOLR-WRK.

       FIL-CNS-20.
           ADD      1 TO TAB-COUNTER.
           IF       TAB-COUNTER > 1000
            OR      COMMAND OF COBOL-SYS-TAB(TAB-COUNTER) = SPACES
                    GO TO FIL-CNS-90
           END-IF.
           MOVE     COMMAND OF COBOL-SYS-TAB(TAB-COUNTER) TO
                    COBOL-COMMAND.

           PERFORM  SCHREIBE-VORGANG.
           GO TO    FIL-CNS-20.

       FIL-CNS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER EUP-WERTE
     ‚*--------------------------------------------------------------
       FILL-EUP SECTION.
       FIL-EUP-00.

           INITIALIZE RET-CODE.
     ‚* ERSTMAL PRÜFEN OB IN DER ZEILE EIN COBOL-BEFEHL VORHANDEN
     ‚* IST, WENN JA, DANN PRÜFEN WO DIESER STEHT
           INITIALIZE TAB-COUNTER.
           MOVE     "E+P" TO SYNTAX-TYP.
           MOVE     "RED" TO SYCOLR-WRK.

       FIL-EUP-20.
           ADD      1 TO TAB-COUNTER.
           IF       TAB-COUNTER > 1000
            OR      COMMAND OF COBOL-EUP-TAB(TAB-COUNTER) = SPACES
                    GO TO FIL-EUP-90
           END-IF.
           MOVE     COMMAND OF COBOL-EUP-TAB(TAB-COUNTER) TO
                    COBOL-COMMAND.

           PERFORM  SCHREIBE-VORGANG.
           GO TO    FIL-EUP-20.

       FIL-EUP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN KOPFSATZ DES VORGANGS
     ‚*--------------------------------------------------------------
       SCHREIBE-VORGANG SECTION.
       SCR-VOR-00.

           INITIALIZE LEN.
     ‚* ERMITTELN DER LÄNGE
           COMPUTE  LEN = LENGTH OF COBOL-COMMAND.
           PERFORM  UNTIL COBOL-COMMAND(LEN:1) NOT = SPACES
                     OR   LEN                      = ZEROES
                    SUBTRACT 1 FROM LEN
           END-PERFORM.

           INITIALIZE PCHKHGHF OF PCHKHGH-P.
     ‚* SCHLÜSSEL FÜLLEN
           MOVE     T-USER                 TO SYUSER OF PCHKHGH-P.
           MOVE     SYNTAX-TYP             TO SYTYP  OF PCHKHGH-P.
           MOVE     COBOL-COMMAND          TO SYCMD  OF PCHKHGH-P.
           MOVE     LEN                    TO SYLEN  OF PCHKHGH-P.
     ‚* SCHALTER
           MOVE     1                      TO SYSTAN OF PCHKHGH-P.
           MOVE     1                      TO SYAKT  OF PCHKHGH-P.
     ‚* FARBWERT ERMITTELN
           MOVE     SYCOLR-WRK      TO COLOR-WRK.
           MOVE     SPACES          TO INVERS-WRK.
           MOVE     SPACES          TO BLINK-WRK.
           MOVE     SPACES          TO UNDERLINE-WRK.
           MOVE     SPACES          TO SYHEX-WRK.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    SYHEX-WRK
           END-CALL.
           MOVE     SYHEX-WRK              TO SYHEX  OF PCHKHGH-P.
           MOVE     SYCOLR-WRK             TO SYCOLR OF PCHKHGH-P.
     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                 TO SYAUSR OF PCHKHGH-P.
           MOVE     T-TERM                 TO SYABS  OF PCHKHGH-P.
           MOVE     PGM-WRK                TO SYAPGM OF PCHKHGH-P.
           MOVE     DATE-8                 TO SYADTA OF PCHKHGH-P.
           MOVE     TIME-6                 TO SYATIA OF PCHKHGH-P.

           WRITE    PCHKHGH-P.

       SCR-VOR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-FILL-CR-DATA: CODE-REVIEW-VARIABLEN FÜLLEN
     ‚*---------------------------------------------------------------

           COPY     CRCPY      OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
