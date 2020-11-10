      *M*<ISEU-META>
      *M* <USER>APG       </USER>
      *M* <DATE>20100901</DATE>
      *M* <TIME>151946</TIME>
      *M*</ISEU-META>
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM FÜR HSEU                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.01.2010                                   *
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
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9004".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  SOURCE-WRK                    PIC X(100).
       01  LEN                           PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           MOVE     "        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA."
                    TO SOURCE-WRK.

           MOVE     ZEROES TO LEN.
           INSPECT  SOURCE-WRK(40:) TALLYING LEN FOR
                    CHARACTERS BEFORE INITIAL " IS ".

     ‚* NORMALER AUFRUF
           PERFORM  TESTROUTINE.

     ‚* AUFRUF ALS SCHLEIFE END-PERFORM
           PERFORM  TESTROUTINE UNTIL RET-CODE = SPACES.

     ‚* AUFRUF ALS SCHLEIFE IN MEHREREN ZEILEN
           PERFORM  TESTROUTINE
                    UNTIL RET-CODE = SPACES.

     ‚* AUFRUF IN MEHREREN ZEILEN
           PERFORM
                    TESTROUTINE
                               .
      * TESTKOMMENTAR (OHNE WEIß)

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* TESTROUTINE
     ‚*--------------------------------------------------------------
       TESTROUTINE SECTION.
       TST-RTR-00.

           INITIALIZE RET-CODE.

       TST-RTR-90.
           EXIT.
      /
