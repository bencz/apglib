             CMD        PROMPT('iRoom ausführen')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('IROOM') RSTD(*NO) MIN(0)
             PARM       KWD(CALLPARM) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('User')
