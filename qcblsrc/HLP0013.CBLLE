       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0013.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* STRING ERSETZEN (LGPGM0926)                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  STRING ERSETZEN                              *
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

     ‚*
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0013 ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* VARIABLEN FÜR DIE LÄNGEN DER PARAMETER.
       01  STRING-ERSETZUNG.
           05  VON-ERSETZ-STRING         PIC X(100) VALUE SPACES.
           05  VON-ERSETZ-POS            PIC 9(3)   VALUE ZEROES.
           05  NACH-ERSETZ-STRING        PIC X(100) VALUE SPACES.
           05  ERSETZ-WRK                PIC X(250) VALUE SPACES.

     ‚* ERSETZEN-STRING SECTION
       01  ERSETZ-WRK2                   PIC X(250) VALUE SPACES.
       01  LEN3                          PIC 9(3)   VALUE ZEROES.
       01  LEN2                          PIC 9(3)   VALUE ZEROES.
       01  LEN4                          PIC 9(3)   VALUE ZEROES.
       01  ANZAHL                        PIC 9(3)   VALUE ZEROES.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-STRING                      PIC X(250).
     ‚*    X-STRING :  STRING, WELCHER BEARBEITET WERDEN SOLL
       01  X-VON-STR                     PIC X(100).
     ‚*    X-VON-STR:  STRING, WELCHER ERSETZT WERDEN SOLL
       01  X-NACH-STR                    PIC X(100).
     ‚*    X-NACH-STR: STRING, DER EINGEFÜGT WERDEN SOLL
       01  X-MODE                        PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION  USING       X-STRING
                                       X-VON-STR
                                       X-NACH-STR
                                       X-MODE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           MOVE     X-STRING   TO ERSETZ-WRK.
           MOVE     X-VON-STR  TO VON-ERSETZ-STRING.
           MOVE     X-NACH-STR TO NACH-ERSETZ-STRING.

           PERFORM  ERSETZEN-STRING.

           MOVE     ERSETZ-WRK TO X-STRING.

       ENDE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ERSETZEN EINES STRINGS IN EINEM ANDEREM DURCH EINEN NEUEN
     ‚* STRING BELIEBIGER LÄNGE
     ‚*---------------------------------------------------------------
       ERSETZEN-STRING SECTION.
       ERS-STR-00.

     ‚* Parameter:                                                   *
     ‚*   ERSETZ-WRK         : Ursprünglicher String                 *
     ‚*   VON-ERSETZ-STRING  : Zu ersetzender Text                   *
     ‚*   NACH-ERSETZ-STRING : Neuer Text                            *

     ‚* ANMERKUNG: NACH-WERT IN HOCHKOMMAS FÜR LÄNGENBESTIMMUNG?

           MOVE     SPACES    TO ERSETZ-WRK2.
           MOVE     ZEROES    TO LEN2.
           MOVE     ZEROES    TO LEN3.
           MOVE     ZEROES    TO LEN4.
           MOVE     ZEROES    TO ANZAHL.

           IF       VON-ERSETZ-POS    > ZEROES
            AND     VON-ERSETZ-STRING > SPACES
                    GO TO ERS-STR-90
           END-IF.

           IF       VON-ERSETZ-POS    = ZEROES
            AND     VON-ERSETZ-STRING = SPACES
                    GO TO ERS-STR-90
           END-IF.

           IF       VON-ERSETZ-STRING NOT = SPACES
                    INSPECT VON-ERSETZ-STRING TALLYING ANZAHL FOR ALL
                            X"7F"
                    IF   ANZAHL = 2
                         MOVE VON-ERSETZ-STRING(2:) TO ERSETZ-WRK2
                         MOVE ERSETZ-WRK2    TO VON-ERSETZ-STRING
                         MOVE SPACES TO ERSETZ-WRK2
                         INSPECT VON-ERSETZ-STRING TALLYING LEN3 FOR
                                 CHARACTERS BEFORE X"7F"
                         MOVE SPACES TO VON-ERSETZ-STRING(LEN3 + 1:)
                         MOVE ZEROES TO ANZAHL
                    END-IF
                    INSPECT NACH-ERSETZ-STRING TALLYING ANZAHL FOR ALL
                            X"7F"
                    IF   ANZAHL = 2
                         MOVE NACH-ERSETZ-STRING(2:) TO ERSETZ-WRK2
                         MOVE ERSETZ-WRK2    TO NACH-ERSETZ-STRING
                         MOVE SPACES TO ERSETZ-WRK2
                         INSPECT NACH-ERSETZ-STRING TALLYING LEN4
                                 FOR CHARACTERS BEFORE X"7F"
                         MOVE SPACE TO NACH-ERSETZ-STRING(LEN4 + 1:)
                    END-IF
                    MOVE ZEROES TO ANZAHL
                    COMPUTE LEN3 = FUNCTION LENGTH(VON-ERSETZ-STRING)
                    END-COMPUTE
                    PERFORM UNTIL LEN3 = ZEROES
                              OR VON-ERSETZ-STRING(LEN3:1) NOT = SPACES
                            SUBTRACT 1 FROM LEN3
                    END-PERFORM
                    IF   LEN3 = ZEROES
                         IF   VON-ERSETZ-POS = ZEROES
                              GO TO ERS-STR-90
                         ELSE
                              MOVE VON-ERSETZ-POS TO LEN3
                         END-IF
                    END-IF
           ELSE
                    MOVE VON-ERSETZ-POS TO LEN3
           END-IF.

           COMPUTE  LEN4  = FUNCTION LENGTH(NACH-ERSETZ-STRING)
           END-COMPUTE.

           PERFORM  UNTIL LEN4 = ZEROES
                          OR NACH-ERSETZ-STRING(LEN4:1) NOT = SPACES
                    SUBTRACT 1 FROM LEN4
           END-PERFORM.

           IF       VON-ERSETZ-STRING NOT = SPACES
                    INSPECT ERSETZ-WRK TALLYING ANZAHL FOR ALL
                            VON-ERSETZ-STRING(1:LEN3)
                    PERFORM ANZAHL TIMES
                            MOVE   ZEROES    TO LEN2
                            MOVE   SPACES    TO ERSETZ-WRK2
                            INSPECT ERSETZ-WRK TALLYING LEN2 FOR
                                    CHARACTERS BEFORE
                                    VON-ERSETZ-STRING(1:LEN3)
                            IF   LEN2 = FUNCTION LENGTH(ERSETZ-WRK)
                                 GO TO ERS-STR-90
                            END-IF
                            IF   LEN3 = LEN4
                                 MOVE   NACH-ERSETZ-STRING(1:LEN4) TO
                                        ERSETZ-WRK(LEN2 + 1:LEN4)
                            ELSE
                                 MOVE ERSETZ-WRK(LEN3 + LEN2 + 1:) TO
                                      ERSETZ-WRK2
                                 IF   LEN4 > ZEROES
                                      MOVE NACH-ERSETZ-STRING(1:LEN4)
                                      TO ERSETZ-WRK(LEN2 + 1:LEN4)
                                 END-IF
                                 MOVE   ERSETZ-WRK2(1:) TO
                                        ERSETZ-WRK(LEN2 + LEN4 + 1:)
     ‚*                          ZUSATZHANDLING?
                                 IF   X-MODE NOT = SPACES
                                      MOVE SPACES      TO
                                           ERSETZ-WRK(LEN2 + LEN4 + 1:)
                                      MOVE ERSETZ-WRK2 TO
                                           ERSETZ-WRK(LEN2 + LEN3 + 1:)
                                 END-IF
                            END-IF
                    END-PERFORM
           ELSE
                    MOVE ERSETZ-WRK(LEN3:) TO ERSETZ-WRK2
                    IF   LEN4 > ZEROES
                         MOVE NACH-ERSETZ-STRING(1:LEN4) TO
                              ERSETZ-WRK(LEN3:LEN4)
                    END-IF
                    MOVE   ERSETZ-WRK2(1:) TO ERSETZ-WRK(LEN3 + LEN4:)
           END-IF.

       ERS-STR-90.
           MOVE     SPACES TO VON-ERSETZ-STRING.
           MOVE     SPACES TO NACH-ERSETZ-STRING.
           MOVE     SPACES TO ERSETZ-WRK2.
           MOVE     ZEROES TO LEN2.
           MOVE     ZEROES TO LEN3.
           MOVE     ZEROES TO LEN4.
           MOVE     ZEROES TO ANZAHL.
           MOVE     ZEROES TO VON-ERSETZ-POS.

            EXIT.
      /
