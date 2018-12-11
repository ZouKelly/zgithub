**&---------------------------------------------------------------------*
**& Report zkz_test
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT zkz_test.

SELECT DISTINCT ernam, matkl FROM mara
  INTO TABLE @DATA(gt_mara)
  WHERE ernam IN ('ALAN', 'AI4879').
IF sy-subrc = 0.
  LOOP AT gt_mara INTO DATA(lwa_mara).
    write: 'test'.
  ENDLOOP.

ENDIF.
*DATA: BEGIN OF gs_mara,
*        matnr TYPE matnr,
*        ersda TYPE mara-ersda,
*        mtart TYPE mara-mtart,
*        matkl TYPE mara-matkl,
*      END OF gs_mara.
*
*SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME.
*SELECT-OPTIONS: so_matnr FOR gs_mara-matnr.
*SELECT-OPTIONS: so_ersda FOR gs_mara-ersda.
*PARAMETERS: pa_mtart RADIOBUTTON GROUP grp DEFAULT 'X' USER-COMMAND com.
*SELECT-OPTIONS: so_mtart FOR gs_mara-mtart.
*PARAMETERS: pa_matkl RADIOBUTTON GROUP grp.
*SELECT-OPTIONS: so_matkl FOR gs_mara-matkl.
*SELECTION-SCREEN END OF BLOCK 0.
*
*INITIALIZATION.
*  so_ersda-low = sy-datum - 1.
*  APPEND so_ersda.
*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM hide_field.
*
*AT SELECTION-SCREEN.
*  PERFORM check_field.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_matnr-low.
*  PERFORM read_material.
*CALL FUNCTION '/1BCDWB/SF00000045'
* EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
*   CONTROL_PARAMETERS         =
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
.
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.


*TABLES: ekko,ekpo.
*DATA gt_ekpo TYPE TABLE OF ekpo.
*DATA gv_sum TYPE ekpo-menge.
*
*
*PARAMETERS: zebeln LIKE ekko-ebeln.
*FREE MEMORY ID 'ZCOPY_ITEMS'.
*
*
*CALL FUNCTION 'OPEN_FORM'
*  EXPORTING
*    form     = 'ZKZ_TEST'
*    language = sy-langu.
*SELECT SINGLE * FROM ekko WHERE ebeln = zebeln.
*
*SELECT * FROM ekpo
*  INTO TABLE gt_ekpo
*  WHERE ebeln = zebeln.
*IF sy-subrc = 0.
*  LOOP AT gt_ekpo INTO ekpo.
*    CALL FUNCTION 'WRITE_FORM'
*      EXPORTING
*        element = 'TEST'
*        type    = 'BODY'
*        window  = 'MAIN'
*      EXCEPTIONS
*        OTHERS  = 01.
*
*    gv_sum = gv_sum + ekpo-menge.
*    PERFORM write_copy.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element = 'TOTAL'
*      window  = 'MAIN'
*    EXCEPTIONS
*      OTHERS  = 01.
*
*
*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element                  = 'TOTAL'
*      function                 = 'APPEND'
*      window                   = 'COPY'
** IMPORTING
**     PENDING_LINES            =
*    EXCEPTIONS
*      element                  = 1
*      function                 = 2
*      type                     = 3
*      unopened                 = 4
*      unstarted                = 5
*      window                   = 6
*      bad_pageformat_for_print = 7
*      spool_error              = 8
*      codepage                 = 9
*      OTHERS                   = 10.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*ENDIF.
*CALL FUNCTION 'CLOSE_FORM'.
*
*
*FORM read_table TABLES in_par STRUCTURE itcsy
*                       out_par STRUCTURE itcsy.
*  BREAK zouk.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form WRITE_COPY
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**& -->  p1        text
**& <--  p2        text
**&---------------------------------------------------------------------*
*FORM write_copy .
*  DATA ls_ekpo TYPE ekpo.
*  DATA lt_ekpo TYPE TABLE OF ekpo.
*  DATA:
*    wa_header TYPE thead, " header for standard text
*    it_line   TYPE STANDARD TABLE OF tline,
*    wa_line   TYPE tline.
*
*
*  CONSTANTS: c_star     TYPE   c LENGTH 2 VALUE 'P1',
*             c_lang     TYPE   char1 VALUE 'ES',
*             c_tab      TYPE   c LENGTH 2 VALUE ',,',
*             c_txt      TYPE   thead-tdobject  VALUE 'TEXT',
*             c_tname_po TYPE   thead-tdname  VALUE 'ZMAIN',
*             c_tid      TYPE   thead-tdid  VALUE 'ST'.
*
*  IMPORT p1 = lt_ekpo FROM MEMORY ID 'ZCOPY_ITEMS'.
*
*  IF lines( lt_ekpo ) = 12.
*    FREE lt_ekpo.
*  ENDIF.
*  ls_ekpo = ekpo.
*
*  APPEND ls_ekpo TO lt_ekpo.
*  EXPORT p1 = lt_ekpo TO MEMORY ID 'ZCOPY_ITEMS'.
*
*  LOOP AT lt_ekpo INTO ls_ekpo.
*    wa_line-tdformat = '*'.
*    wa_line-tdline = ls_ekpo-ebeln && ',,' && ls_ekpo-ebelp.
*    APPEND wa_line TO it_line.
*  ENDLOOP.
*
*  wa_header-tdobject = c_txt.
*  wa_header-tdname   = c_tname_po.
*  wa_header-tdid     = c_tid .
*  wa_header-tdspras  = c_lang.
*
**  CALL FUNCTION 'SAVE_TEXT'
**    EXPORTING
***     CLIENT          = SY-MANDT
**      header          = wa_header
***     INSERT          = ' '
**      savemode_direct = 'X'
***     OWNER_SPECIFIED = ' '
***     LOCAL_CAT       = ' '
***   IMPORTING
***     FUNCTION        =
***     NEWHEADER       =
**    TABLES
**      lines           = it_line
**    EXCEPTIONS
**      id              = 1
**      language        = 2
**      name            = 3
**      object          = 4
**      OTHERS          = 5.
**  IF sy-subrc = 0.
**
**  ENDIF.
*
*  CALL FUNCTION 'WRITE_FORM_LINES'
*    EXPORTING
**     FUNCTION                       = 'SET'
*      header = wa_header
**     TYPE   = 'BODY'
*      window = 'COPY'
** IMPORTING
**     FROMPAGE                       =
**     PENDING_LINES                  =
*    TABLES
*      lines  = it_line
* EXCEPTIONS
*     FUNCTION                       = 1
*     TYPE   = 2
*     UNOPENED                       = 3
*     UNSTARTED                      = 4
*     WINDOW = 5
*     BAD_PAGEFORMAT_FOR_PRINT       = 6
*     SPOOL_ERROR                    = 7
*     CODEPAGE                       = 8
*     OTHERS = 9
*    .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
**  CALL FUNCTION 'WRITE_FORM'
**    EXPORTING
**      element                  = '525'
**      function                 = 'SET'
**      window                   = 'COPY'
**    EXCEPTIONS
**      element                  = 1
**      function                 = 2
**      type                     = 3
**      unopened                 = 4
**      unstarted                = 5
**      window                   = 6
**      bad_pageformat_for_print = 7
**      spool_error              = 8
**      codepage                 = 9
**      OTHERS                   = 10.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIDE_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM hide_field .
*  LOOP AT SCREEN.
*    IF pa_mtart IS NOT INITIAL.
*      IF screen-name CS 'SO_MATKL'.
*        screen-active = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*      IF screen-name CS 'SO_MTART'.
*        screen-active = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM check_field .
*  IF pa_mtart IS NOT INITIAL.
*    IF so_mtart[] IS NOT INITIAL.
*      SELECT SINGLE mtart FROM t134
*        INTO @DATA(lv_mtart)
*        WHERE mtart IN @so_mtart.
*      IF sy-subrc <> 0.
*        MESSAGE 'Material type entered is not valid' TYPE 'I'.
*      ENDIF.
*    ENDIF.
*  ELSE.
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_MATERIAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
**&---------------------------------------------------------------------*
*FORM read_material .
*  SELECT matnr, ersda, ernam FROM mara
*    INTO TABLE @DATA(lt_mara)
*    WHERE mtart IN @so_mtart.
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'MATNR'
*      dynpprog        = sy-repid
*      dynpnr          = '1000'
*      dynprofield     = 'SO_MATNR-LOW'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_mara
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*ENDFORM.
