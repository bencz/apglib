       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9064.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WAREHOUSE MANAGEMENT SYSTEM LFS.7                             *
     ‚*---------------------------------------------------------------*
     ‚* COPYRIGHT BY  :  EHRHARDT + PARTNER GMBH & CO. KG             *
     ‚*                  SOFTWARE-SYSTEME FÜR WAREHOUSE-LOGISTIK      *
     ‚*                  56154 BOPPARD-BUCHHOLZ                       *
     ‚*                  +49 (0) 67 42 / 87 27 0                      *
     ‚*                  INFO@EHRHARDT-PARTNER.COM                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* @AUTHOR       :  XXXXXXXXXX                                   *
     ‚*                                                               *
     ‚* @CREATED      :  XXXXX JJJJ                                   *
     ‚*                                                               *
     ‚* @FUNCTION     :  P3 99999 / LFS 001.00 / ANG 999.99           *
     ‚*                  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     ‚* @COPY FROM    :  XXXXXXXXXX                                   *
     ‚*                                                               *
     ‚* @TAGS         :  WORK.PACK ROUNDING DELETE MAPDTA ARRAY       *
     ‚*                  EXAMPLE PACKING                              *
     ‚*                                                               *
     ‚* @AREA         :  STANDARD PACKING PICKING                     *
     ‚*                                                               *
     ‚* @CHANGES:                                                     *
     ‚* DATE       BY    DESCRIPTION                                  *
     ‚* TT.MM.JJJJ XXX   P3 99999 / LFS 001.00 / ANG 999.99           *
     ‚*                  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
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
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP9063".
      /
     ‚*-------------------------------------------------------------
     ‚* \PARAM X-APN WORK-PACKAGE THAT MUST BE USED
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-APN                          PIC 9(9).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-APN.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
     ‚* \BRIEF MAIN SECTION FOR WORK PACKAGE PROCESSING
     ‚*-------------------------------------------------------------
       MAIN SECTION.
       ANFANG.

           PERFORM  TEST-DOC.
           PERFORM  ANOTHER-TEST.

       ENDE.
           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* \BRIEF CHECK IF WORK PACKAGE IS ALLOWED TO BE PROCESSED
     ‚* THIS IS A REALLY SIMPLE TEST SECTION TO SHOW YOU HOW
     ‚* COBOL-DOC COULD WORK. VALIDATION IS DONE WITH A CHECK ON
     ‚* FILE \SEE PPAKET.
     ‚* \PARAM APN-WRK
     ‚* \PARAM PKNR-WRK
     ‚* \RETURN RET-CODE VALUE SPACES MEANS EVERYTHING IS OK WHILE
     ‚*  "1" IS AN ERROR
     ‚*--------------------------------------------------------------
       TEST-DOC SECTION.
       TST-DOC-00.

           INITIALIZE RET-CODE.

       TST-DOC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* \BRIEF DELETE WORK PACKAGE
     ‚* DELETE WORK PACKAGE IN \SEE PANFKOP AND ALL POSITIONS
     ‚* IN FILE \SEE PANFPOS.
     ‚* \PARAM APN-WRK
     ‚*--------------------------------------------------------------
       ANOTHER-TEST SECTION.
       ANO-TST-00.

           INITIALIZE RET-CODE.

       ANO-TST-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
