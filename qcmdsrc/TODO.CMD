             CMD        PROMPT('TODO-LISTE')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('TODO') RSTD(*NO) MIN(0)

             PARM       KWD(SELEKTION) TYPE(*CHAR) LEN(30) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Selektion')
             PARM       KWD(KATEGORIE) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Kategorie')
