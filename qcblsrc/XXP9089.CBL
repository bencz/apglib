       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9089.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.05.2017                                   *
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
     ‚* PTSTATR: TEST DATA
     ‚*-------------------------------------------------------------
           SELECT PTSTATR-DP
                  ASSIGN       TO  DATABASE-PTSTATR
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEST DATA
       FD  PTSTATR-DP
           LABEL RECORDS ARE STANDARD.
       01  PTSTATR-P.
           COPY DDS-ALL-FORMATS OF PTSTATR.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP9089".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".

       01  KEY-ARRAY.
        05 RANDOM-KEY-ARRAY               PIC X(10) OCCURS 10 TIMES.
       01  COL-ARRAY.
        05 RANDOM-COL-ARRAY               PIC X(10) OCCURS 5 TIMES.
       01  OWNER-ARRAY.
        05 RANDOM-OWN-ARRAY               PIC X(10) OCCURS 10 TIMES.
       01  WEIGHT-ARRAY.
        05 RANDOM-WGT-ARRAY               PIC X(10) OCCURS 10 TIMES.
       01  ISSUEL-ARRAY.
        05 RANDOM-ISS-ARRAY               PIC X(10) OCCURS 10 TIMES.
       01  IMAGE-ARRAY.
        05 RANDOM-IMG-ARRAY               PIC X(10) OCCURS 10 TIMES.
       01  SOLVED-ARRAY.
        05 RANDOM-SLV-ARRAY               PIC X(10) OCCURS 2 TIMES.
       01  BATCH-ARRAY.
        05 RANDOM-BAT-ARRAY               PIC X(10) OCCURS 5 TIMES.
       01  TYPE-ARRAY.
        05 RANDOM-TYP-ARRAY               PIC X(10) OCCURS 10 TIMES.
       01  ARCHIVE-ARRAY.
        05 RANDOM-ARC-ARRAY               PIC X(10) OCCURS 2 TIMES.
       01  ASSIGNED-ARRAY.
        05 RANDOM-ASS-ARRAY               PIC X(10) OCCURS 10 TIMES.

       01  ATPALN-WRK                LIKE ATPALN OF PTSTATR-P.
       01  ATATRK-WRK                LIKE ATATRK OF PTSTATR-P.
       01  ATATRV-WRK                LIKE ATATRV OF PTSTATR-P.

       01  RAND-KEY                       PIC X(10).
       01  RANDOM-SEED                    PIC 9(9).
       01  RANDOM-NBR                     PIC 9(1)V9(9).
       01  RANDOM-WRK                     PIC 9(3).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚* NUMBER OF RECORDS WHICH SHOULD BE ADDED
       01  X-NUMBER                       PIC 9(9).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-NUMBER.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           PERFORM  COPY-PGM-INIT.

           OPEN     OUTPUT PTSTATR-DP.
           PERFORM  FILL-ARRAY.

           MOVE     17     TO RANDOM-SEED.
           MOVE     ZEROES TO ATPALN-WRK.
           PERFORM  X-NUMBER TIMES
                    ADD     1  TO ATPALN-WRK
                    PERFORM GET-RANDOM-VALUES
           END-PERFORM.

           CLOSE    PTSTATR-DP.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* GET RANDOM VALUES FOR WRITE
     ‚*--------------------------------------------------------------
       GET-RANDOM-VALUES SECTION.
       GET-RNV-00.

     ‚* HOW MANY ATTRIBUTES DO WE ADD?
           MOVE     "*ATTR" TO RAND-KEY.
           PERFORM  GET-RANDOM-NUMBER.

           PERFORM  RANDOM-WRK TIMES
     ‚*             WHICH ATTRIBUTE?
                    MOVE    "*ATTR-SEL" TO RAND-KEY
                    PERFORM GET-RANDOM-NUMBER
                    MOVE    RANDOM-KEY-ARRAY(RANDOM-WRK) TO ATATRK-WRK
     ‚*             ATTRIBUTE VALUE
                    MOVE    ATATRK-WRK TO RAND-KEY
                    PERFORM GET-RANDOM-NUMBER
                    PERFORM GET-ARRAY-VALUE
                    PERFORM WRITE-RECORD
           END-PERFORM.

       GET-RNV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GET RANDOM NUMBER
     ‚*--------------------------------------------------------------
       GET-RANDOM-NUMBER SECTION.
       GET-RND-00.

     ‚* GET RANDOM KEY
           COMPUTE  RANDOM-NBR = FUNCTION RANDOM().

           EVALUATE RAND-KEY
     ‚*        ATTRIBUTES WITH 10 VALUES
               WHEN "*ATTR"
               WHEN "*ATTR-SEL"
               WHEN "OWNER"
               WHEN "WEIGHT"
               WHEN "ISSUE"
               WHEN "IMAGE"
               WHEN "ASSIGNED"
               WHEN "TYPE"
                    COMPUTE RANDOM-WRK = RANDOM-NBR * 10 + 1
                    IF   RANDOM-WRK <= ZEROES
                         MOVE 1  TO RANDOM-WRK
                    END-IF
                    IF   RANDOM-WRK >  10
                         MOVE 10 TO RANDOM-WRK
                    END-IF
     ‚*        ATTRIBUTES WITH 5 VALUES
               WHEN "COLOR"
               WHEN "BATCH"
                    COMPUTE RANDOM-WRK = (RANDOM-NBR * 10 + 1) / 2
                    IF   RANDOM-WRK <= ZEROES
                         MOVE 1  TO RANDOM-WRK
                    END-IF
                    IF   RANDOM-WRK >  5
                         MOVE 5  TO RANDOM-WRK
                    END-IF
     ‚*        ATTRIBUTES WITH 2 VALUES
               WHEN "SOLVED"
               WHEN "ARCHIVE"
                    COMPUTE RANDOM-WRK = RANDOM-NBR * 10 + 1
                    IF   RANDOM-WRK > ZEROES
                     AND RANDOM-WRK < 5
                         MOVE 1  TO RANDOM-WRK
                    ELSE
                         MOVE 2  TO RANDOM-WRK
                    END-IF
           END-EVALUATE.

       GET-RND-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GET VALUE PER TYPE
     ‚*--------------------------------------------------------------
       GET-ARRAY-VALUE SECTION.
       GET-ARR-00.

           EVALUATE ATATRK-WRK
               WHEN "COLOR"
                    MOVE RANDOM-COL-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "OWNER"
                    MOVE RANDOM-OWN-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "WEIGHT"
                    MOVE RANDOM-WGT-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "ISSUE"
                    MOVE RANDOM-ISS-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "IMAGE"
                    MOVE RANDOM-IMG-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "SOLVED"
                    MOVE RANDOM-SLV-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "BATCH"
                    MOVE RANDOM-BAT-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "TYPE"
                    MOVE RANDOM-TYP-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "ARCHIVE"
                    MOVE RANDOM-ARC-ARRAY(RANDOM-WRK) TO ATATRV-WRK
               WHEN "ASSIGNED"
                    MOVE RANDOM-ASS-ARRAY(RANDOM-WRK) TO ATATRV-WRK
           END-EVALUATE.

       GET-ARR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FILL ARRAY
     ‚*--------------------------------------------------------------
       FILL-ARRAY SECTION.
       FIL-ARR-00.

     ‚* KEY VALUES
           MOVE     "COLOR"       TO RANDOM-KEY-ARRAY(1).
           MOVE     "OWNER"       TO RANDOM-KEY-ARRAY(2).
           MOVE     "WEIGHT"      TO RANDOM-KEY-ARRAY(3).
           MOVE     "ISSUE"       TO RANDOM-KEY-ARRAY(4).
           MOVE     "IMAGE"       TO RANDOM-KEY-ARRAY(5).
           MOVE     "SOLVED"      TO RANDOM-KEY-ARRAY(6).
           MOVE     "BATCH"       TO RANDOM-KEY-ARRAY(7).
           MOVE     "TYPE"        TO RANDOM-KEY-ARRAY(8).
           MOVE     "ARCHIVE"     TO RANDOM-KEY-ARRAY(9).
           MOVE     "ASSIGNED"    TO RANDOM-KEY-ARRAY(10).

     ‚* RANDOM COLORS
           MOVE     "BLUE"        TO RANDOM-COL-ARRAY(1).
           MOVE     "RED"         TO RANDOM-COL-ARRAY(2).
           MOVE     "GREEN"       TO RANDOM-COL-ARRAY(3).
           MOVE     "YELLOW"      TO RANDOM-COL-ARRAY(4).
           MOVE     "BLACK"       TO RANDOM-COL-ARRAY(5).

     ‚* RANDOM OWNER
           MOVE     "APG"         TO RANDOM-OWN-ARRAY(1).
           MOVE     "CFR"         TO RANDOM-OWN-ARRAY(2).
           MOVE     "RRU"         TO RANDOM-OWN-ARRAY(3).
           MOVE     "DSM"         TO RANDOM-OWN-ARRAY(4).
           MOVE     "JPF"         TO RANDOM-OWN-ARRAY(5).
           MOVE     "RRE"         TO RANDOM-OWN-ARRAY(6).
           MOVE     "MF"          TO RANDOM-OWN-ARRAY(7).
           MOVE     "BWI"         TO RANDOM-OWN-ARRAY(8).
           MOVE     "BIW"         TO RANDOM-OWN-ARRAY(9).
           MOVE     "SEG"         TO RANDOM-OWN-ARRAY(10).

     ‚* RANDOM WEIGHT
           MOVE     "10.10"       TO RANDOM-WGT-ARRAY(1).
           MOVE     "1233.70"     TO RANDOM-WGT-ARRAY(2).
           MOVE     "40.31"       TO RANDOM-WGT-ARRAY(3).
           MOVE     "11.31"       TO RANDOM-WGT-ARRAY(4).
           MOVE     "500.00"      TO RANDOM-WGT-ARRAY(5).
           MOVE     "307.47"      TO RANDOM-WGT-ARRAY(6).
           MOVE     "100.14"      TO RANDOM-WGT-ARRAY(7).
           MOVE     "103.10"      TO RANDOM-WGT-ARRAY(8).
           MOVE     "3.47"        TO RANDOM-WGT-ARRAY(9).
           MOVE     "40.78"       TO RANDOM-WGT-ARRAY(10).

     ‚* RANDOM ISSUE
           MOVE     "8795646465"  TO RANDOM-ISS-ARRAY(1).
           MOVE     "0540654877"  TO RANDOM-ISS-ARRAY(2).
           MOVE     "0007897989"  TO RANDOM-ISS-ARRAY(3).
           MOVE     "0789731237"  TO RANDOM-ISS-ARRAY(4).
           MOVE     "4567813247"  TO RANDOM-ISS-ARRAY(5).
           MOVE     "7893203079"  TO RANDOM-ISS-ARRAY(6).
           MOVE     "1230879360"  TO RANDOM-ISS-ARRAY(7).
           MOVE     "1260789637"  TO RANDOM-ISS-ARRAY(8).
           MOVE     "7989760452"  TO RANDOM-ISS-ARRAY(9).
           MOVE     "6648976945"  TO RANDOM-ISS-ARRAY(10).

     ‚* RANDOM IMAGE
           MOVE     "FILE.JPG"    TO RANDOM-IMG-ARRAY(1).
           MOVE     "USER.JPG"    TO RANDOM-IMG-ARRAY(2).
           MOVE     "MYPIC.JOG"   TO RANDOM-IMG-ARRAY(3).
           MOVE     "FUNNY.GIF"   TO RANDOM-IMG-ARRAY(4).
           MOVE     "QRCODE.JPG"  TO RANDOM-IMG-ARRAY(5).
           MOVE     "CASE.PNG"    TO RANDOM-IMG-ARRAY(6).
           MOVE     "HOTEL.JPG"   TO RANDOM-IMG-ARRAY(7).
           MOVE     "CARD.GIF"    TO RANDOM-IMG-ARRAY(8).
           MOVE     "FLAG.PNG"    TO RANDOM-IMG-ARRAY(9).
           MOVE     "*NONE"       TO RANDOM-IMG-ARRAY(10).

     ‚* RANDOM SOLVED
           MOVE     "YES"         TO RANDOM-SLV-ARRAY(1).
           MOVE     "NO"          TO RANDOM-SLV-ARRAY(2).

     ‚* RANDOM BATCH
           MOVE     "#213123"     TO RANDOM-BAT-ARRAY(1).
           MOVE     "#794563"     TO RANDOM-BAT-ARRAY(2).
           MOVE     "#3453X7"     TO RANDOM-BAT-ARRAY(3).
           MOVE     "#F87974"     TO RANDOM-BAT-ARRAY(4).
           MOVE     "#DF3355"     TO RANDOM-BAT-ARRAY(5).

     ‚* RANDOM TYPE
           MOVE     "MEDICAL"     TO RANDOM-TYP-ARRAY(1).
           MOVE     "INSURANCE"   TO RANDOM-TYP-ARRAY(2).
           MOVE     "HOUSE"       TO RANDOM-TYP-ARRAY(3).
           MOVE     "USER"        TO RANDOM-TYP-ARRAY(4).
           MOVE     "TICKET"      TO RANDOM-TYP-ARRAY(5).
           MOVE     "CAR"         TO RANDOM-TYP-ARRAY(6).
           MOVE     "WAREHOUSE"   TO RANDOM-TYP-ARRAY(7).
           MOVE     "ACCIDENT"    TO RANDOM-TYP-ARRAY(8).
           MOVE     "COMPUTER"    TO RANDOM-TYP-ARRAY(9).
           MOVE     "MSGW"        TO RANDOM-TYP-ARRAY(10).

     ‚* RANDOM ARCHIVE
           MOVE     "YES"         TO RANDOM-ARC-ARRAY(1).
           MOVE     "NO"          TO RANDOM-ARC-ARRAY(2).

     ‚* RANDOM ASSIGNED
           MOVE     "APG"         TO RANDOM-ASS-ARRAY(1).
           MOVE     "CFR"         TO RANDOM-ASS-ARRAY(2).
           MOVE     "RRU"         TO RANDOM-ASS-ARRAY(3).
           MOVE     "DSM"         TO RANDOM-ASS-ARRAY(4).
           MOVE     "JPF"         TO RANDOM-ASS-ARRAY(5).
           MOVE     "RRE"         TO RANDOM-ASS-ARRAY(6).
           MOVE     "MF"          TO RANDOM-ASS-ARRAY(7).
           MOVE     "BWI"         TO RANDOM-ASS-ARRAY(8).
           MOVE     "BIW"         TO RANDOM-ASS-ARRAY(9).
           MOVE     "SEG"         TO RANDOM-ASS-ARRAY(10).

       FIL-ARR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WRITE PTSTATR RECORD
     ‚*--------------------------------------------------------------
       WRITE-RECORD SECTION.
       WRI-REC-00.

           INITIALIZE PTSTATRF OF PTSTATR-P.
           MOVE     ATPALN-WRK   TO ATPALN OF PTSTATR-P.
           MOVE     1            TO ATPALP OF PTSTATR-P.
           MOVE     ATATRK-WRK   TO ATATRK OF PTSTATR-P.
           MOVE     ATATRV-WRK   TO ATATRV OF PTSTATR-P.
           WRITE    PTSTATR-P.

       WRI-REC-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
