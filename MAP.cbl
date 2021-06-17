 000100          TITLE 'MAPA MENU CLIENTES COBOL'
 000200 MAPSETA  DFHMSD TYPE=&SYSPARM,MODE=INOUT,CTRL=(FREEKB,FRSET),          *
 000300                LANG=COBOL,TIOAPFX=YES,COLOR=BLUE
 000310
 000400 MAP0233  DFHMDI SIZE=(24,80)
 000500          DFHMDF POS=(3,10),LENGTH=22,INITIAL='MENU CONSULTA CLIENTES'
 000710
 000800          DFHMDF POS=(03,54),LENGTH=12,INITIAL='T233-MAP0233'
 000810
 000900 FECHA    DFHMDF POS=(04,54),LENGTH=10,INITIAL='99-99-9999'
 000910
 000920          DFHMDF POS=(07,13),LENGTH=18,INITIAL='SELECCIONAR OPCION'
 000930
 001000          DFHMDF POS=(09,13),LENGTH=05,INITIAL='-ALTA'
 001210
 001300          DFHMDF POS=(11,13),LENGTH=05,INITIAL='-BAJA'
 001310
 001400          DFHMDF POS=(13,13),LENGTH=13,INITIAL='-MODIFICACION'
 001410
 001420          DFHMDF POS=(15,13),LENGTH=09,INITIAL='-CONSULTA'
 002610
 002700 MSG      DFHMDF POS=(21,04),LENGTH=72,ATTRB=PROT,COLOR=RED,            *
 002710                PICOUT='X(72)',HILIGHT=UNDERLINE
 002720          DFHMDF POS=(21,77),LENGTH=01,ATTRB=(ASKIP,PROT)
 002800          DFHMDF POS=(23,13),LENGTH=08,INITIAL='PF1:ALTA'
 002900          DFHMDF POS=(23,24),LENGTH=08,INITIAL='PF2:BAJA'
 002902          DFHMDF POS=(23,48),LENGTH=12,INITIAL='PF3:MODIFICA'
 002903          DFHMDF POS=(23,63),LENGTH=12,INITIAL='PF4:CONSULTA'
 002930          DFHMDF POS=(24,35),LENGTH=10,INITIAL='PF12:Salir'
 003000          DFHMSD TYPE=FINAL
 003100          END


















 Command ===>                                                  Scroll ===> CSR
  F1=Help      F2=Split     F3=Exit      F5=Rfind     F6=Rchange   F7=Up
  F8=Down      F9=Swap     F10=Left     F11=Right    F12=Cancel
   *EDIT    -EDIT
================================================================================
   File  Edit  Edit_Settings  Menu  Utilities  Compilers  Test  Help
 „Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ„Ÿ
 EDIT       ALU0033.CURSOS.FUENTE(MAP0233) - 01.19          Columns 00001 00072
 ****** **************************** Bottom of Data ****************************
























 Command ===>                                                  Scroll ===> CSR
  F1=Help      F2=Split     F3=Exit      F5=Rfind     F6=Rchange   F7=Up
  F8=Down      F9=Swap     F10=Left     F11=Right    F12=Cancel
   *EDIT    -EDIT
