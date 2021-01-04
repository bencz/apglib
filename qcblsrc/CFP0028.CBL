       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0028.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SCHREIBEN EINES OPL-EINTRAGS                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.11.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SCHREIBEN EINES NEUEN OPL-EINTRAGS JE ID     *
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
     ‚* PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PCFGSTS: STLFDN
     ‚*-------------------------------------------------------------
           SELECT PCFGSTS-DP
                  ASSIGN       TO  DATABASE-PCFGSTS
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* POPLLST: OPLFDN
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DP
                  ASSIGN       TO  DATABASE-POPLLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PTXTFIT: FTFILE, FTKEY, FTLFPO
     ‚*-------------------------------------------------------------
           SELECT PTXTFIT-DP
                  ASSIGN       TO  DATABASE-PTXTFIT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* POPLLSTL3: OPTTYP, OPUSER, OPTID
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DL
                  ASSIGN       TO  DATABASE-POPLLSTL3
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
     ‚*--- PCFGSTS: STLFDN
       FD  PCFGSTS-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGSTS-P.
           COPY DDS-ALL-FORMATS OF PCFGSTS.
      /
     ‚*--- POPLLST: OPLFDN
       FD  POPLLST-DP
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-P.
           COPY DDS-ALL-FORMATS OF POPLLST.
      /
     ‚*--- PTXTFIT: FTFILE, FTKEY, FTLFPO
       FD  PTXTFIT-DP
           LABEL RECORDS ARE STANDARD.
       01  PTXTFIT-P.
           COPY DDS-ALL-FORMATS OF PTXTFIT.
      /
     ‚*--- POPLLSTL3: OPTTYP, OPUSER, OPTID
       FD  POPLLST-DL
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-L.
           COPY DDS-ALL-FORMATS OF POPLLSTL3.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH LOG-CPY
           COPY WRKLOGCPY   OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0028".

       01  OPLFDN-WRK                    LIKE OPLFDN OF POPLLST-P.

       01  FTKEY-WRK                     LIKE FTKEY  OF PTXTFIT-P.
       01  FTLFPO-WRK                    LIKE FTLFPO OF PTXTFIT-P.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-STLFDN                      LIKE STLFDN OF PCFGSTS-P.
       01  X-OPL-TYPE                    LIKE OPTTYP OF POPLLST-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-STLFDN
                                X-OPL-TYPE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0028" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN I-O   POPLLST-DP
                               PTXTFIT-DP
                    OPEN INPUT POPLLST-DL
                               PCFGSTS-DP
                               PCONFIG-DP
           END-IF.

           INITIALIZE PCFGSTSF OF PCFGSTS-P.
           MOVE     X-STLFDN TO STLFDN OF PCFGSTS-P.
           READ     PCFGSTS-DP.
           IF       FILE-STATUS = ZEROES
                    PERFORM READ-MESSAGE-USER
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE USER PRÜFEN DIE DIESE FEHLERMELDUNG AKTIV HABEN
     ‚*--------------------------------------------------------------
       READ-MESSAGE-USER SECTION.
       REA-MSG-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H102.1"            TO CFID   OF PCONFIG-P.
           MOVE     STTID  OF PCFGSTS-P TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-MSG-90
           END-IF.

       REA-MSG-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO REA-MSG-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = "H102.1"
            OR      CFKEY  OF PCONFIG-P NOT = STTID  OF PCFGSTS-P
                    GO TO REA-MSG-90
           END-IF.

     ‚* GIBT ES DEN EINTRAG SCHON, WIRD DIESER WIEDER AKTIVIERT,
     ‚* ANSONSTEN NEU ANGELEGT
           PERFORM  CHECK-MESSAGE-ACTIVE.
           IF       OPLFDN-WRK NOT = ZEROES
                    PERFORM UPDATE-POPLLST
           ELSE
                    PERFORM WRITE-POPLLST
           END-IF.

     ‚* ANSCHLIEßEND WIRD EINE DETAILMELDUNG IN DIE PTXTFIT
     ‚* GESCHRIEBEN (AUSWAHL "8" IM TODO-TOOL)
           PERFORM  ADD-PTXTFIT.

           GO TO    REA-MSG-20.

       REA-MSG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VORHANDEN EINTRAG AKTUALISIEREN
     ‚*--------------------------------------------------------------
       UPDATE-POPLLST SECTION.
       UPD-OPL-00.

           INITIALIZE POPLLSTF OF POPLLST-P.
           MOVE     OPLFDN-WRK          TO OPLFDN OF POPLLST-P.
           READ     POPLLST-DP.
           IF       FILE-STATUS = ZEROES
                    PERFORM COPY-GET-TIME
     ‚*             DATENSATZ BEKOMMT NEUSTES DATUM
                    MOVE    DATE-8              TO OPDATE OF POPLLST-P
                    MOVE    ZEROES              TO OPPRZT OF POPLLST-P
                    MOVE    SPACES              TO OPSTAT OF POPLLST-P
     ‚*             USER FÜLLEN
                    MOVE    T-USER              TO OPMSER OF POPLLST-P
                    MOVE    T-TERM              TO OPMBS  OF POPLLST-P
                    MOVE    PGM-WRK             TO OPMPGM OF POPLLST-P
                    MOVE    DATE-8              TO OPMDTA OF POPLLST-P
                    MOVE    TIME-6              TO OPMTIA OF POPLLST-P
                    REWRITE POPLLST-P
                    IF   OPSHRN OF POPLLST-P NOT = ZEROES
                         CALL "XXP0017" USING OPLFDN OF POPLLST-P
                                              OPSHRN OF POPLLST-P
                         END-CALL
                    END-IF
           END-IF.

       UPD-OPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES NEUEN EINTRAGS
     ‚*--------------------------------------------------------------
       WRITE-POPLLST SECTION.
       WRI-OPL-00.

           INITIALIZE POPLLSTF OF POPLLST-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "POPLLST" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK                  TO OPLFDN OF POPLLST-P.

     ‚* NUTZDATEN FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8                    TO OPDATE OF POPLLST-P.
           MOVE     CFKEY2 OF PCONFIG-P       TO OPUSER OF POPLLST-P.
           MOVE     "*DEFAULT"                TO OPTYPE OF POPLLST-P.

           STRING   "You have new "           DELIMITED BY SIZE
                    X-OPL-TYPE                DELIMITED BY SPACES
                    " messages."              DELIMITED BY SIZE
             INTO   OPTEXT OF POPLLST-P
           END-STRING.

           MOVE     ZEROES                    TO OPPRZT OF POPLLST-P.
           MOVE     SPACES                    TO OPSTAT OF POPLLST-P.

     ‚* FEHLER-TYP UND ID EINTRAGEN
           MOVE     X-OPL-TYPE                TO OPTTYP OF POPLLST-P.
           MOVE     STTID  OF PCFGSTS-P       TO OPTID  OF POPLLST-P.

     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                    TO OPAUSR OF POPLLST-P.
           MOVE     T-TERM                    TO OPABS  OF POPLLST-P.
           MOVE     PGM-WRK                   TO OPAPGM OF POPLLST-P.
           MOVE     DATE-8                    TO OPADTA OF POPLLST-P.
           MOVE     TIME-6                    TO OPATIA OF POPLLST-P.

           WRITE    POPLLST-P.

       WRI-OPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DETAILSATZ IN PTXTFIT SCHREIBEN
     ‚*--------------------------------------------------------------
       ADD-PTXTFIT SECTION.
       ADD-TXT-00.

     ‚* BEI SHARE-EINTRÄGEN MÜSSEN AUCH SHARE-TEXTE ERFASST WERDEN
           IF       OPSHRN OF POPLLST-P NOT = ZEROES
                    MOVE "S"                 TO FTKEY-WRK
                    MOVE OPSHRN OF POPLLST-P TO FTKEY-WRK(2:)
           ELSE
                    MOVE OPLFDN OF POPLLST-P TO FTKEY-WRK
           END-IF.

     ‚* ERSTMAL PRÜFEN WIEVIELE TEXTE ES SCHON GIBT
           PERFORM  GET-HIGHEST-LFPO.
           ADD      1 TO FTLFPO-WRK.

           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     "POPLLST"              TO FTFILE OF PTXTFIT-P.
           MOVE     FTKEY-WRK              TO FTKEY  OF PTXTFIT-P.

     ‚* TEXT ZUSAMMENBAUEN
           MOVE     STTID  OF PCFGSTS-P    TO FTTEXT OF PTXTFIT-P.
           MOVE     ":"                    TO FTTEXT OF PTXTFIT-P(11:).
           MOVE     STSTS  OF PCFGSTS-P    TO FTTEXT OF PTXTFIT-P(13:).

     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                 TO FTAUSR OF PTXTFIT-P.
           MOVE     T-TERM                 TO FTABS  OF PTXTFIT-P.
           MOVE     PGM-WRK                TO FTAPGM OF PTXTFIT-P.
           MOVE     DATE-8                 TO FTADTA OF PTXTFIT-P.
           MOVE     TIME-6                 TO FTATIA OF PTXTFIT-P.
           WRITE    PTXTFIT-P.
     ‚* SIND DATEN VORHANDEN, DIESE GGF. AUCH ANZEIGEN
           IF       STDATA OF PCFGSTS-P NOT = SPACES
                    ADD  1                   TO FTLFPO OF PTXTFIT-P
                    MOVE SPACES              TO FTTEXT OF PTXTFIT-P
                    MOVE STDATA OF PCFGSTS-P TO
                         FTTEXT OF PTXTFIT-P(10:)
                    WRITE PTXTFIT-P
           END-IF.

       ADD-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HÖCHSTE LAUFENDE NUMMER ERMITTELN
     ‚*--------------------------------------------------------------
       GET-HIGHEST-LFPO SECTION.
       GET-LFP-00.

           INITIALIZE FTLFPO-WRK.
     ‚* JETZT HÖCHSTE NUMMER ERMITTELN
           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     "POPLLST"           TO FTFILE OF PTXTFIT-P.
           MOVE     FTKEY-WRK           TO FTKEY  OF PTXTFIT-P.
           MOVE     ZEROES              TO FTLFPO OF PTXTFIT-P.
           START    PTXTFIT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-LFP-90
           END-IF.

       GET-LFP-20.
           READ     PTXTFIT-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-LFP-90
           END-IF.

           IF       FTFILE OF PTXTFIT-P NOT = "POPLLST"
            OR      FTKEY  OF PTXTFIT-P NOT = FTKEY-WRK
                    GO TO GET-LFP-90
           END-IF.

           IF       FTLFPO OF PTXTFIT-P > FTLFPO-WRK
                    MOVE FTLFPO OF PTXTFIT-P TO FTLFPO-WRK
           END-IF.

           GO TO    GET-LFP-20.

       GET-LFP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB NACHRICHT MIT ID SCHON EXISTIERT
     ‚*--------------------------------------------------------------
       CHECK-MESSAGE-ACTIVE SECTION.
       CHK-MAC-00.

           INITIALIZE OPLFDN-WRK.
     ‚* IN DER POPLLST SUCHEN
           INITIALIZE POPLLSTF OF POPLLST-L.
           MOVE     X-OPL-TYPE          TO OPTTYP OF POPLLST-L.
           MOVE     CFKEY2 OF PCONFIG-P TO OPUSER OF POPLLST-L.
           MOVE     SPACES              TO OPTID  OF POPLLST-L.
           START    POPLLST-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-MAC-90
           END-IF.

       CHK-MAC-20.
           READ     POPLLST-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-MAC-90
           END-IF.

           IF       OPTTYP OF POPLLST-L NOT = X-OPL-TYPE
            OR      OPUSER OF POPLLST-L NOT = CFKEY2 OF PCONFIG-P
                    GO TO CHK-MAC-90
           END-IF.

           MOVE     OPLFDN OF POPLLST-L TO OPLFDN-WRK.

       CHK-MAC-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
