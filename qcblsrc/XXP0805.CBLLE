     ˆ* BAUSTELLE
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0805.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCEN EINEM PROJEKT HINZUFÜGEN                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.09.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SOURCEN HINZUFÜGEN                           *
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
     ‚* PPRJMBR: COSLIB, COSRCN, COSMBR
     ‚*-------------------------------------------------------------
           SELECT PPRJMBR-DP
                  ASSIGN       TO  DATABASE-PPRJMBR
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PTMPLST (TEMPRÄRE DATEI)
     ‚*-------------------------------------------------------------
           SELECT PTMPLST-DP
                  ASSIGN       TO  DATABASE-PTMPLST
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PPRJMBR: COSLIB, COSRCN, COSMBR
       FD  PPRJMBR-DP
           LABEL RECORDS ARE STANDARD.
       01  PPRJMBR-P.
           COPY DDS-ALL-FORMATS OF PPRJMBR.
      /
     ‚*--- PTMPLST (TEMPRÄRE DATEI)
       FD  PTMPLST-DP
           LABEL RECORDS ARE STANDARD.
       01  PTMPLST-P.
           COPY DDS-ALL-FORMATS OF PTMPLST.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0805".
       01  PGM-TYP                       PIC X(8)  VALUE "PROJEKT".

     ‚* PARAMETER FÜR MD5FILE
       01  MD5-WRK                       PIC X(32).
       01  PATH-WRK                      PIC X(91).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PROJEKT                     LIKE COPROJ OF PPRJMBR-P.
       01  X-TEILPROJEKT                 LIKE COTPRJ OF PPRJMBR-P.
       01  X-SRCLIB                      LIKE COSLIB OF PPRJMBR-P.
       01  X-SRCRCN                      LIKE COSRCN OF PPRJMBR-P.
       01  X-SRCMBR                      LIKE COSMBR OF PPRJMBR-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PROJEKT
                                X-TEILPROJEKT
                                X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0805" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PPRJMBR-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERSTMAL ALLE SOURCEN MITTELS DSPFD ERMITTELN
           PERFORM  COLLECT-SOURCES.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT DIE TEMPORÄRE DATEI LESEN UND ALLE SOURCEN HINZUFÜGEN
           PERFORM  CREATE-PPRJMBR.

       ENDE.
           CLOSE                 PPRJMBR-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DSPFD AUSFÜHREN UM ALLE SOURCEN ZU ERMITTELN
     ‚*--------------------------------------------------------------
       COLLECT-SOURCES SECTION.
       COL-SRC-00.

           INITIALIZE RET-CODE, DSPFD-REC.
           MOVE     X-SRCLIB            TO LIBNAME   OF DSPFD-REC.
           MOVE     X-SRCRCN            TO FILENAME  OF DSPFD-REC.
           MOVE     "*OUTFILE"          TO OUTPUTTYP OF DSPFD-REC.
           MOVE     "*MBRLIST"          TO TYPNAME   OF DSPFD-REC.
           MOVE     "PTMPLST"           TO OUTFILE   OF DSPFD-REC.
           MOVE     "QTEMP"             TO OUTLIB    OF DSPFD-REC.
           MOVE     "*REPLACE"          TO OUTOPT    OF DSPFD-REC.
           CALL     "CLP0042" USING DSPFD-REC
                                    RET-CODE
           END-CALL.

       COL-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE SOURCEN KOPIEREN
     ‚*--------------------------------------------------------------
       CREATE-PPRJMBR SECTION.
       CRT-PMB-00.

     ‚* DATEI ÖFFNEN
           OPEN     INPUT PTMPLST-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CRT-PMB-90
           END-IF.

           INITIALIZE QWHFDML OF PTMPLST-P.

        CRT-PMB-20.
           READ     PTMPLST-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CRT-PMB-90
           END-IF.

     ‚* BESTIMMTE ZEILEN NICHT BEARBEITEN
     ‚*
     ‚* LEERE DATEIEN
           IF       MLNAME OF PTMPLST-P     = SPACES
     ‚*     PHYSISICHE DER LOGISCHE OBJEKT-DATEIEN
            OR      MLFILE OF PTMPLST-P     = MLNAME OF PTMPLST-P
     ‚*     WAS AUCH IMMER DAS IST, ICH MAGS NICHT
            OR      MLFILE OF PTMPLST-P     = "EVFEVENT"
                    GO TO CRT-PMB-20
           END-IF.

     ‚* SOURCEINFORMATIONEN ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     MLLIB  OF PTMPLST-P TO OBJLIB   OF RTVMBRD-REC.
           MOVE     MLFILE OF PTMPLST-P TO FILENAME OF RTVMBRD-REC.
           MOVE     MLNAME OF PTMPLST-P TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    GO TO CRT-PMB-20
           END-IF.

     ‚* JETZT DEN DATENSATZ IN DER PPRJMBR ANLEGEN
           PERFORM  WRITE-PPRJMBR.

           GO TO    CRT-PMB-20.

       CRT-PMB-90.
           CLOSE    PTMPLST-DP.

           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN DES DATENSATZES
     ‚*--------------------------------------------------------------
       WRITE-PPRJMBR SECTION.
       WRT-PMB-00.

           INITIALIZE PPRJMBRF OF PPRJMBR-P.
           MOVE     MLLIB  OF PTMPLST-P TO COSLIB OF PPRJMBR-P.
           MOVE     MLFILE OF PTMPLST-P TO COSRCN OF PPRJMBR-P.
           MOVE     MLNAME OF PTMPLST-P TO COSMBR OF PPRJMBR-P.
           READ     PPRJMBR-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             DATEN FÜLLEN DIE IMMER KOMMEN
                    PERFORM FILL-DATA
                    REWRITE PPRJMBR-P
           ELSE
     ‚*             KEY FÜLLEN
                    MOVE    MLLIB  OF PTMPLST-P TO COSLIB OF PPRJMBR-P
                    MOVE    MLFILE OF PTMPLST-P TO COSRCN OF PPRJMBR-P
                    MOVE    MLNAME OF PTMPLST-P TO COSMBR OF PPRJMBR-P
     ‚*             DATEN FÜLLEN DIE IMMER KOMMEN
                    PERFORM FILL-DATA
                    WRITE   PPRJMBR-P
           END-IF.

       WRT-PMB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEN FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-DATA SECTION.
       FIL-DTA-00.

     ‚* PROJEKT FÜLLEN
           MOVE     X-PROJEKT                   TO COPROJ OF PPRJMBR-P.
           MOVE     X-TEILPROJEKT               TO COTPRJ OF PPRJMBR-P.

     ‚* LANGER SOURCENAME
           IF       COLMBR OF PPRJMBR-P = SPACES
                    MOVE COSMBR OF PPRJMBR-P    TO COLMBR OF PPRJMBR-P
           END-IF.

     ‚* USER/DATUM/ETC FÜLLEN
           PERFORM  COPY-GET-TIME.
           IF       COAUSR OF PPRJMBR-P = SPACES
                    MOVE     T-USER             TO COAUSR OF PPRJMBR-P
                    MOVE     T-TERM             TO COABS  OF PPRJMBR-P
                    MOVE     PGM-WRK            TO COAPGM OF PPRJMBR-P
                    MOVE     DATE-8             TO COADTA OF PPRJMBR-P
                    MOVE     TIME-6             TO COATIA OF PPRJMBR-P
           ELSE
                    MOVE     T-USER             TO COMSER OF PPRJMBR-P
                    MOVE     T-TERM             TO COMBS  OF PPRJMBR-P
                    MOVE     PGM-WRK            TO COMPGM OF PPRJMBR-P
                    MOVE     DATE-8             TO COMDTA OF PPRJMBR-P
                    MOVE     TIME-6             TO COMTIA OF PPRJMBR-P
           END-IF.

     ‚* INFORMATIONEN AUS RTVMBRD
     ‚*
     ‚* SOURCETYP
           MOVE     SRCTYPE OF RTVMBRD-REC      TO COTYP  OF PPRJMBR-P.
     ‚* BESCHREIBUNG
           MOVE     MBRTEXT  OF RTVMBRD-REC     TO COTEXT OF PPRJMBR-P.
     ‚* ERSTELLUNGSDATUM
           MOVE     CRTDATE OF RTVMBRD-REC(2:6) TO
                    CODTE  OF PPRJMBR-P(3:).
           IF       CODTE  OF PPRJMBR-P NOT = ZEROES
                    IF   CRTDATE OF RTVMBRD-REC(2:2) > 90
                         MOVE 19 TO CODTE  OF PPRJMBR-P(1:2)
                    ELSE
                         MOVE 20 TO CODTE  OF PPRJMBR-P(1:2)
                    END-IF
           END-IF.
     ‚* ÄNDERUNGSDATUM
           MOVE     CHGDATE OF RTVMBRD-REC(2:6) TO
                    CODTM OF PPRJMBR-P(3:).
           IF       CODTM  OF PPRJMBR-P NOT = ZEROES
                    IF   CHGDATE OF RTVMBRD-REC(2:2) > 90
                         MOVE 19 TO CODTM  OF PPRJMBR-P(1:2)
                    ELSE
                         MOVE 20 TO CODTM  OF PPRJMBR-P(1:2)
                    END-IF
           END-IF.
     ‚* ÄNDERUNGSDATUM
           MOVE     SAVDATE OF RTVMBRD-REC(2:6) TO
                    CODTS OF PPRJMBR-P(3:).
           IF       CODTS  OF PPRJMBR-P NOT = ZEROES
                    IF   SAVDATE OF RTVMBRD-REC(2:2) > 90
                         MOVE 19 TO CODTS  OF PPRJMBR-P(1:2)
                    ELSE
                         MOVE 20 TO CODTS  OF PPRJMBR-P(1:2)
                    END-IF
           END-IF.
     ‚* ÄNDERUNGSDATUM
           MOVE     RSTDATE OF RTVMBRD-REC(2:6) TO
                    CODTR OF PPRJMBR-P(3:).
           IF       CODTR  OF PPRJMBR-P NOT = ZEROES
                    IF   RSTDATE OF RTVMBRD-REC(2:2) > 90
                         MOVE 19 TO CODTR  OF PPRJMBR-P(1:2)
                    ELSE
                         MOVE 20 TO CODTR  OF PPRJMBR-P(1:2)
                    END-IF
           END-IF.
     ‚* SOURCEDATUM
           MOVE     CODTM  OF PPRJMBR-P TO CODTSC OF PPRJMBR-P.

     ‚* SYSTEMNAMEN ERMITTELN
           INITIALIZE RTVNETA-REC.
           MOVE     "SYSNAME" TO AKTION OF RTVNETA-REC.
           CALL     "CLP0060" USING RTVNETA-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE SYSNAME OF RTVNETA-REC TO COSYSN OF PPRJMBR-P
           END-IF.

     ‚* MD5-HASHSUMME BERECHNEN. AUFBAU: LIB/FILE(MBR)
           INITIALIZE PATH-WRK.
           INITIALIZE MD5-WRK.
           STRING   MLLIB  OF PTMPLST-P         DELIMITED BY SPACES
                    "/"                         DELIMITED BY SIZE
                    MLFILE OF PTMPLST-P         DELIMITED BY SPACES
                    "("                         DELIMITED BY SIZE
                    MLNAME OF PTMPLST-P         DELIMITED BY SPACES
                    ")"                         DELIMITED BY SIZE
                    X'00'                       DELIMITED BY SIZE
           INTO     PATH-WRK.
           CALL     PROCEDURE "MD5FILE" USING PATH-WRK
                                              MD5-WRK
           END-CALL.
           IF       MD5-WRK NOT = ZEROES
            AND     MD5-WRK NOT = SPACES
                    MOVE MD5-WRK                TO COMD5  OF PPRJMBR-P
           END-IF.

       FIL-DTA-90.
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
