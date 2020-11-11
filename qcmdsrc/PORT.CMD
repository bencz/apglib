             CMD        PROMPT('PORT ANZEIGEN')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('PORT') RSTD(*NO) MIN(0)
             PARM       KWD(PARM) TYPE(*CHAR) LEN(20) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Service')
