             CMD        PROMPT('ESSEN400-MAIL')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('ESSENMAIL') RSTD(*NO) MIN(0)

             PARM       KWD(DATUM) TYPE(*CHAR) LEN(8) RSTD(*NO) +
                          DFT(00000000) MIN(0) CHOICE('YYYYMMDD') +
                          PROMPT('Datum')
