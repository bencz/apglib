     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CFP0010.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB INFOTEXTE VORHANDEN SIND UND GGF. KOPIEREN          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN UND KOPIEREN INFOTEXTE                *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚* HINWEISTEXTE (LESEN)
     ‚*-------------------------------------------------------------
           SELECT PHINTXT-DP
                  ASSIGN       TO  DATABASE-PHINTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
     ‚*-------------------------------------------------------------
     ‚* HINWEISTEXTE (SCHREIBEN)
     ‚*-------------------------------------------------------------
           SELECT PHINTXT-DP1
                  ASSIGN       TO  DATABASE-PHINTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- HINWEISTEXTE (LESEN)
       FD  PHINTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PHINTXT-P.
           COPY DDS-ALL-FORMATS OF PHINTXT.
      /
     ‚*--- HINWEISTEXTE (SCHREIBEN)
       FD  PHINTXT-DP1
           LABEL RECORDS ARE STANDARD.
       01  PHINTXT-P1.
           COPY DDS-ALL-FORMATS OF PHINTXT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0010".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-PGMWRK                      LIKE PGM-WRK.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-PGMWRK
                                         X-RETCODE.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0010" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
     ‚* NUR EINMAL ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     INPUT PHINTXT-DP
                    OPEN     I-O   PHINTXT-DP1
           END-IF.

           INITIALIZE X-RETCODE.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB TEXTE FÜR DIESES PROGRAMM VORHANDEN SIND
           PERFORM  CHECK-TEXTE.

       ENDE.
     ‚*€   CLOSE                 PHINTXT-DP
     ‚*€                         PHINTXT-DP1.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       CHECK-TEXTE SECTION.
       CHK-TXT-00.

           INITIALIZE DATEN.
     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PHINTXTF OF PHINTXT-P.
           MOVE     X-PGMWRK            TO HWPGM  OF PHINTXT-P.
           MOVE     SPACES              TO HWUSER OF PHINTXT-P.
           MOVE     ZEROES              TO HWLFPO OF PHINTXT-P.
           START    PHINTXT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TXT-90
           END-IF.

       CHK-TXT-20.
           READ     PHINTXT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TXT-90
           END-IF.

     ‚* KEY PRÜFEN
           IF       HWPGM  OF PHINTXT-P NOT = X-PGMWRK
                    GO TO CHK-TXT-90
           END-IF.

     ‚* NUR AKTIVE TEXTE KOPIEREN
           IF       HWFK01 OF PHINTXT-P NOT = ZEROES
                    GO TO CHK-TXT-20
           END-IF.

     ‚* IST DER TEXT AUCH FÜR DEN USER SCHON VORHANDEN?
           PERFORM  CHECK-TEXT-USER.
           IF       RET-CODE = SPACES
                    PERFORM COPY-TEXT-USER
           END-IF.

           GO TO    CHK-TXT-20.

       CHK-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DER TEXT AUCH FÜR DEN USER VORHANDEN IST
     ‚*--------------------------------------------------------------
       CHECK-TEXT-USER SECTION.
       CHK-TXU-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB DER TEXT VORHANDEN IST
           INITIALIZE PHINTXTF OF PHINTXT-P1.
           MOVE     HWPGM  OF PHINTXT-P TO HWPGM  OF PHINTXT-P1.
           MOVE     T-USER              TO HWUSER OF PHINTXT-P1.
           MOVE     HWLFPO OF PHINTXT-P TO HWLFPO OF PHINTXT-P1.
           READ     PHINTXT-DP1 WITH NO LOCK.
           IF       FILE-STATUS = ZEROES
                    MOVE "1" TO RET-CODE
     ‚*             AUCH MELDEN, DASS WIR EINEN INFOTEXT
     ‚*             ANZEIGEN KÖNNEN
                    IF   HWFK01 OF PHINTXT-P1 = ZEROES
                         MOVE "1" TO X-RETCODE
                    END-IF
           END-IF.

       CHK-TXU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEXT FÜR DEN USER KOPIEREN
     ‚*--------------------------------------------------------------
       COPY-TEXT-USER SECTION.
       COP-TXT-00.

           INITIALIZE PHINTXTF OF PHINTXT-P1.
           MOVE     CORR PHINTXTF OF PHINTXT-P TO
                         PHINTXTF OF PHINTXT-P1.
     ‚* KEYS EXPLIZIT FÜLLEN
           MOVE     HWPGM  OF PHINTXT-P  TO HWPGM  OF PHINTXT-P1.
           MOVE     T-USER               TO HWUSER OF PHINTXT-P1.
           MOVE     HWLFPO OF PHINTXT-P  TO HWLFPO OF PHINTXT-P1.
     ‚* IMMER ALS UNGELESEN KOPIEREN
           MOVE     ZEROES               TO HWFK01 OF PHINTXT-P1.
     ‚* USER FÜLLEN/DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER               TO HWAUSR OF PHINTXT-P1.
           MOVE     T-TERM               TO HWABS  OF PHINTXT-P1.
           MOVE     PGM-WRK              TO HWAPGM OF PHINTXT-P1.
           MOVE     DATE-8               TO HWADTA OF PHINTXT-P1.
           MOVE     TIME-6               TO HWATIA OF PHINTXT-P1.
           WRITE    PHINTXT-P1.
           IF       FILE-STATUS = ZEROES
     ‚*             JETZT HABEN WIR AUCH EINEN TEXT
                    MOVE "1" TO X-RETCODE
           END-IF.

       COP-TXT-90.
           EXIT.
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
