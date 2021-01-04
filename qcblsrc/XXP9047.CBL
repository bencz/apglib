       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9047.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  30.08.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
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
     ‚* PBESTTP: TPPALN, TPPALP
     ‚*-------------------------------------------------------------
           SELECT PBESTTP-DP
                  ASSIGN       TO  DATABASE-PBESTTP
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*- PBESTTP: TPPALN, TPPALP
       FD  PBESTTP-DP
           LABEL RECORDS ARE STANDARD.
       01  PBESTTP-P.
           COPY DDS-ALL-FORMATS OF R06DATV7-PBESTTP.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9047".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  PBESTTP-WRK.
           COPY DDS-ALL-FORMATS OF R06DATV7-PBESTTP.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TPPALN                      LIKE TPPALN OF PBESTTP-P.
       01  X-TPPALP                      LIKE TPPALP OF PBESTTP-P.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TPPALN
                                X-TPPALP.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9047"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ÖFFNEN DER DATEI
           OPEN     I-O PBESTTP-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           INITIALIZE PBESTTPF OF PBESTTP-P.
           MOVE     X-TPPALN TO TPPALN OF PBESTTP-P.
           MOVE     X-TPPALP TO TPPALP OF PBESTTP-P.
           READ     PBESTTP-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             ALTE FELDER RETTEN
                    INITIALIZE PBESTTPF OF PBESTTP-WRK
                    MOVE PBESTTP-P TO PBESTTP-WRK
     ‚*             JETZT RECORD ZERSTÖREN
                    MOVE SPACES   TO PBESTTP-P
                    MOVE X-TPPALN TO TPPALN OF PBESTTP-P
                    MOVE X-TPPALP TO TPPALP OF PBESTTP-P
     ‚*             DIESE ALTEN WERTE RETTEN
                    MOVE TPKONZ OF PBESTTP-WRK TO TPKONZ OF PBESTTP-P
                    MOVE TPFIRM OF PBESTTP-WRK TO TPFIRM OF PBESTTP-P
                    MOVE TPGRKO OF PBESTTP-WRK TO TPGRKO OF PBESTTP-P
                    MOVE TPIDEN OF PBESTTP-WRK TO TPIDEN OF PBESTTP-P
     ‚*             SONST GIBTS ZUORDNUNGSFEHLER
                    MOVE ZEROES                TO TPAPN  OF PBESTTP-P
                    MOVE ZEROES                TO TPSATN OF PBESTTP-P
                    MOVE ZEROES                TO TPSATP OF PBESTTP-P
                    MOVE ZEROES                TO TPGTNR OF PBESTTP-P
                    MOVE ZEROES                TO TPNRKS OF PBESTTP-P
                    MOVE ZEROES                TO TPSLNR OF PBESTTP-P
                    MOVE ZEROES                TO TPBENR OF PBESTTP-P
                    MOVE ZEROES                TO TPLENR OF PBESTTP-P
                    MOVE ZEROES                TO TPVREG OF PBESTTP-P
                    MOVE ZEROES                TO TPVHOR OF PBESTTP-P
                    MOVE ZEROES                TO TPVVER OF PBESTTP-P
                    MOVE ZEROES                TO TPVSEG OF PBESTTP-P
                    MOVE ZEROES                TO TPNREG OF PBESTTP-P
                    MOVE ZEROES                TO TPNHOR OF PBESTTP-P
                    MOVE ZEROES                TO TPNVER OF PBESTTP-P
                    MOVE ZEROES                TO TPNSEG OF PBESTTP-P
                    MOVE ZEROES                TO TPMDE3 OF PBESTTP-P
                    MOVE ZEROES                TO TPPALA OF PBESTTP-P
                    MOVE ZEROES                TO TPPAPA OF PBESTTP-P
                    MOVE ZEROES                TO TPAPN1 OF PBESTTP-P
                    MOVE ZEROES                TO TPKOKT OF PBESTTP-P
                    MOVE ZEROES                TO TPLOKT OF PBESTTP-P
                    MOVE ZEROES                TO TPBMEN OF PBESTTP-P
                    MOVE ZEROES                TO TPKMEN OF PBESTTP-P
                    MOVE ZEROES                TO TPTLGN OF PBESTTP-P
                    MOVE ZEROES                TO TPPKNR OF PBESTTP-P
                    MOVE ZEROES                TO TPZUPK OF PBESTTP-P
                    MOVE ZEROES                TO TPZUNR OF PBESTTP-P
                    MOVE ZEROES                TO TPDTDC OF PBESTTP-P
                    REWRITE PBESTTP-P
           END-IF.

     ‚* DATENBANK SCHLIEßEN
           CLOSE    PBESTTP-DP.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
