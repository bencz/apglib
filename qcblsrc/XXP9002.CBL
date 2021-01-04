       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TETS-PGM FÜR INSPOECT/SUBSTRING                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.11.2009                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9002".
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

       ENDE.

           GOBACK.
      /
