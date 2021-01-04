       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9007.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* NACHTRÄGLICHES FÜLLEN DER KW                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  25.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  NACHTRÄGLICHES FÜLLEN DER KW                 *
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
     ‚* PHYSISCHE DATEI KÜCHENPLAN
     ‚*-------------------------------------------------------------
           SELECT PKDPLAN-DP
                  ASSIGN       TO  DATABASE-PKDPLAN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI DATUM
     ‚*-------------------------------------------------------------
           SELECT PCFGDAT-DP
                  ASSIGN       TO  DATABASE-PCFGDAT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- KÜCHENPLAN
       FD  PKDPLAN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-P.
           COPY DDS-ALL-FORMATS OF PKDPLAN.
      /
     ‚*--- DATUMS-TABELLE
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9007".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  DATE-WRK                      LIKE DATE-8.
       01  KW-WRK                        PIC 9(2).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           OPEN     I-O   PKDPLAN-DP
                          PCFGDAT-DP.

           PERFORM  CHANGE-PKDPLAN.
           PERFORM  CHANGE-PCFGDAT.

       ENDE.

           CLOSE                 PKDPLAN-DP
                                 PCFGDAT-DP.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER PKDPLAN
     ‚*--------------------------------------------------------------
       CHANGE-PKDPLAN SECTION.
       CHG-PKD-00.

     ‚* PKDPLAN LESEN
           INITIALIZE PKDPLANF OF PKDPLAN-P.
           MOVE     ZEROES              TO KDDATE OF PKDPLAN-P.
           MOVE     SPACES              TO KDTYP  OF PKDPLAN-P.
           MOVE     SPACES              TO KDUSER OF PKDPLAN-P.
           START    PKDPLAN-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-PKD-90
           END-IF.

       CHG-PKD-20.
           READ     PKDPLAN-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-PKD-90
           END-IF.

     ‚* KALENDERWOCHE ERMITTELN
           INITIALIZE KW-WRK.
           MOVE     KDDATE OF PKDPLAN-P TO DATE-WRK.
           CALL     "CFP8007" USING DATE-WRK
                                    KW-WRK
           END-CALL.

     ‚* KALENDERWOCHE ÜBERNEHMEN UND SPEICHERN
           MOVE     KW-WRK  TO KDKW OF PKDPLAN-P.
           REWRITE  PKDPLAN-P.

           GO TO    CHG-PKD-20.

       CHG-PKD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER PCFGDAT
     ‚*--------------------------------------------------------------
       CHANGE-PCFGDAT SECTION.
       CHG-DAT-00.

     ‚* PKDPLAN LESEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     ZEROES              TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-DAT-90
           END-IF.

       CHG-DAT-20.
           READ     PCFGDAT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-DAT-90
           END-IF.

     ‚* KALENDERWOCHE ERMITTELN
           INITIALIZE KW-WRK.
           MOVE     DTLFDN OF PCFGDAT-P TO DATE-WRK.
           CALL     "CFP8007" USING DATE-WRK
                                    KW-WRK
           END-CALL.

     ‚* KALENDERWOCHE ÜBERNEHMEN UND SPEICHERN
           MOVE     KW-WRK  TO DTKW OF PCFGDAT-P.
           REWRITE  PCFGDAT-P.

           GO TO    CHG-DAT-20.

       CHG-DAT-90.
           EXIT.
      /
