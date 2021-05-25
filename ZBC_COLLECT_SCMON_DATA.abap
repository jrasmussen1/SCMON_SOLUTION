*----------------------------------------------------------------------
* Title           :   ZBC_COLLECT_SCMON_DATA
* Reference       :   Grundfos
* Developer       :   Baranidharan Padur
* Funct. Analyst  :   Santosh Ashok Patil
* Create Date     :   2019.07.04
* Description     :   This program collects custom objects from the Usage data
*                     collected by SAP Transaction Code SCMON
*                     Below 3 custom tables are updated
*                     ZBSCMON_PROG - ABAP Call Monitor: Program Table
*                     ZBSCMON_PROC - ABAP Call Monitor: Procedure Table
*                     ZBSCMON_SUB - ABAP Call Monitor: Subkey Table
*                     Developed as part of S/4 Hana project
*-------------------------MODIFICATION LOG-----------------------------
* Date        Developer       Funct. Analyst Correction  Description
* 2019 July   Jacob Rasmussen Majken Rod J.  CHG0031011  After update of Z tables the standard SCMON tables are cleared.
*----------------------------------------------------------------------
REPORT zbc_collect_scmon_data.

CLASS lcl_main DEFINITION DEFERRED.
*--------------------------------------------------------------------*
* Class global data declaration
*--------------------------------------------------------------------*
DATA: go_main      TYPE REF TO lcl_main ##NEEDED,
      gv_proc_type TYPE scmon_proc-proctype,
      gv_proc_name TYPE scmon_proc-procname,
      gv_rootname  TYPE scmon_sub-rootname,
      gv_obj_name  TYPE scmon_prog-obj_name.
*--------------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------------*
SELECT-OPTIONS:s_root FOR gv_rootname
                NO INTERVALS,
               s_name FOR gv_obj_name
                NO INTERVALS,
               s_procnm  FOR gv_proc_name
                NO INTERVALS,
               s_procty FOR gv_proc_type
                NO INTERVALS.
*--------------------------------------------------------------------*
* Class declaration
*--------------------------------------------------------------------*
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_selscreen_data.
    METHODS:
      get_data,
      put_data,
      delete_data.
  PRIVATE SECTION.
    CONSTANTS gc_i TYPE c LENGTH 1 VALUE 'I'.
    CONSTANTS gc_e TYPE c LENGTH 1 VALUE 'E'.

    DATA: gt_sub  TYPE STANDARD TABLE OF zbscmon_sub,
          gt_prog TYPE STANDARD TABLE OF zbscmon_prog,
          gt_proc TYPE STANDARD TABLE OF zbscmon_proc.
ENDCLASS.
*--------------------------------------------------------------------*
* Class Implementation
*--------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD set_selscreen_data.
*-----------------------------------------------------------------
*This method fills initial values into the selection screen fields
*-----------------------------------------------------------------
    CONSTANTS:lc_cp    TYPE c LENGTH 2 VALUE 'CP',
              lc_eq    TYPE c LENGTH 2 VALUE 'EQ',
              lc_z     TYPE c LENGTH 2 VALUE 'Z*',
              lc_y     TYPE c LENGTH 2 VALUE 'Y*',
              lc_sapmz TYPE sobj_name VALUE 'SAPMZ*',
              lc_sapmy TYPE sobj_name VALUE 'SAPMY*',
              lc_func  TYPE scmon_proc-proctype VALUE 'FUNC',
              lc_meth  TYPE scmon_proc-proctype VALUE 'METH'.

    DATA: ls_root   LIKE s_root,
          ls_name   LIKE s_name,
          ls_procty LIKE s_procty,
          ls_procnm LIKE s_procnm.
*Root Names
    ls_root-sign = gc_i.
    ls_root-option = lc_cp.
    ls_root-low = lc_z.
    APPEND ls_root TO s_root.
    ls_root-low = lc_y.
    APPEND ls_root TO s_root.
    ls_root-low = lc_sapmz.
    APPEND ls_root TO s_root.
    ls_root-low = lc_sapmy.
    APPEND ls_root TO s_root.

*Program Names
    ls_name-sign = gc_i.
    ls_name-option = lc_cp.
    ls_name-low = lc_z.
    APPEND ls_name TO s_name.
    ls_name-low = lc_y.
    APPEND ls_name TO s_name.
    ls_name-low = lc_sapmy.
    APPEND ls_name TO s_name.
    ls_name-low = lc_sapmz.
    APPEND ls_name TO s_name.
*Procedure types
    ls_procty-sign = gc_i.
    ls_procty-option = lc_eq.
    ls_procty-low = lc_func. "Function Modules
    APPEND ls_procty TO s_procty.
    ls_procty-low = lc_meth. "Methods
    APPEND ls_procty TO s_procty.
*Procedure Names
    ls_procnm-sign = gc_i.
    ls_procnm-option = lc_cp.
    ls_procnm-low = lc_z.
    APPEND ls_procnm TO s_procnm.
    ls_procnm-low = lc_y.
    APPEND ls_procnm TO s_procnm.


  ENDMETHOD.

  METHOD get_data.
*-----------------------------------------------------------------
*This method selects Custom Objects from the Standard SCMON Tables
*-----------------------------------------------------------------
*--------------------------------------------------------------------*
* Local constant declatrations
*--------------------------------------------------------------------*
    CONSTANTS : lc_sf_fm   TYPE char11 VALUE  '/1BCDWB/SF%',
                lc_ad_fm   TYPE char11 VALUE  '/1BCDWB/SM%',
                lc_sf_type TYPE char4 VALUE  'SSFO',
                lc_ad_type TYPE char4 VALUE  'SFPF',
                lc_z       TYPE c LENGTH 1 VALUE 'Z',
                lc_y       TYPE c LENGTH 1 VALUE 'Y'.
* Fetch data from SCMON_PROG table
    SELECT @sy-mandt AS mandt,                          "#EC CI_NOFIELD
           progname,
           object,
           obj_name,
           progid
      FROM scmon_prog
      INTO TABLE @gt_prog
      WHERE obj_name IN @s_name.
    IF sy-subrc <> 0.
      CLEAR gt_prog[].
    ENDIF.
* Fetch data from SCMON_PROC table
    SELECT @sy-mandt AS mandt,                          "#EC CI_NOFIELD
           proctype,
           procname,
           classname,
           trigid,
           progid
    FROM scmon_proc
    WHERE   proctype IN @s_procty AND
          ( procname IN @s_procnm OR
           classname IN @s_procnm )
    UNION ALL
* Get smartform names for FM in SCMON_PROC
    SELECT @sy-mandt AS mandt,                          "#EC CI_NOFIELD
           @lc_sf_type AS proctype,
           b~formname AS procname,
           a~classname,
           a~trigid,
           a~progid
    FROM scmon_proc AS a
    LEFT OUTER JOIN stxfadmi AS b
    ON substring( a~procname, 11 , 8 ) = b~fmnumb
    WHERE a~procname LIKE @lc_sf_fm AND
          substring( b~formname, 1 , 1 ) IN ( @lc_z, @lc_y )
    UNION ALL
* Get adobe from names for FM in SCMON_PROC
    SELECT @sy-mandt AS mandt,                          "#EC CI_NOFIELD
           @lc_ad_type AS proctype,
           b~name AS procname,
           a~classname,
           a~trigid,
           a~progid
    FROM scmon_proc AS a
    LEFT OUTER JOIN fpcontexti AS b
    ON substring( a~procname, 11 , 8 ) = b~fmnumber
    WHERE a~procname LIKE @lc_ad_fm AND
          substring( b~name, 1 , 1 )  IN ( @lc_z, @lc_y )
    INTO TABLE @gt_proc.
    IF sy-subrc <> 0.
      CLEAR gt_proc[].
    ENDIF.

* Fetch data from SCMON_SUB table
    SELECT @sy-mandt AS mandt,                          "#EC CI_NOFIRST
           roottype,
           rootname,
           subid
      FROM scmon_sub
      INTO TABLE @gt_sub
      WHERE rootname IN @s_root.
    IF sy-subrc <> 0.
      CLEAR gt_sub[].
    ENDIF.
  ENDMETHOD.

  METHOD put_data.
*-----------------------------------------------------------------
*This method stores selected Custom Objects into Custom SCMON Tables
*-----------------------------------------------------------------
    DATA: lv_msg TYPE c LENGTH 132.

*Message out if no data is selected
    IF gt_prog[] IS INITIAL
    AND gt_proc[] IS INITIAL
    AND gt_sub[] IS INITIAL.
      MESSAGE TEXT-001 TYPE gc_i.
      LEAVE PROGRAM.
    ENDIF.
* Insert data into table ZBSCMON_PROG
    IF gt_prog[] IS NOT INITIAL.
      MODIFY zbscmon_prog FROM TABLE gt_prog.
      IF sy-subrc = 0.
        COMMIT WORK.
*output the no of records
        lv_msg = |{ TEXT-002 } { sy-dbcnt }|.
        IF sy-batch = abap_true.
          MESSAGE lv_msg TYPE gc_i.
        ELSE.
          WRITE:/1 lv_msg.
        ENDIF.
      ELSE.
        MESSAGE TEXT-e01 TYPE gc_e.
      ENDIF.
    ENDIF.
* Insert data into table ZBSCMON_PROC
    IF gt_proc[] IS NOT INITIAL.
      MODIFY zbscmon_proc FROM TABLE gt_proc.
      IF sy-subrc = 0.
        COMMIT WORK.
*output the no of records
        lv_msg = |{ TEXT-003 } { sy-dbcnt }|.
        IF sy-batch = abap_true.
          MESSAGE lv_msg TYPE gc_i.
        ELSE.
          WRITE:/1 lv_msg.
        ENDIF.
      ELSE.
        MESSAGE TEXT-e02 TYPE gc_e.
      ENDIF.
    ENDIF.
* Insert data into table ZBSCMON_SUB
    IF gt_sub[] IS NOT INITIAL.
      MODIFY zbscmon_sub FROM TABLE gt_sub.
      IF sy-subrc = 0.
        COMMIT WORK.
*output the no of records
        lv_msg = |{ TEXT-004 } { sy-dbcnt }|.
        IF sy-batch = abap_true.
          MESSAGE lv_msg TYPE gc_i.
        ELSE.
          WRITE:/1 lv_msg.
        ENDIF.
      ELSE.
        MESSAGE TEXT-e03 TYPE gc_e.
      ENDIF.
    ENDIF.
    FREE: gt_proc,
          gt_proc,
          gt_sub.
  ENDMETHOD.

  METHOD delete_data.
*-----------------------------------------------------------------
*This method delete all records from standard SCMON Tables
*-----------------------------------------------------------------
    CALL FUNCTION 'SCMON_DELETE_DATA'
      EXCEPTIONS
        data_access_error = 1
        OTHERS            = 2.

    IF sy-subrc = 0.
      IF sy-batch = abap_true.
        MESSAGE TEXT-005 TYPE gc_i.
      ELSE.
        WRITE:/1 TEXT-005.
      ENDIF.
    ELSE.
      MESSAGE TEXT-e03 TYPE gc_e.
    ENDIF.

  ENDMETHOD.



ENDCLASS.

INITIALIZATION.
*Set initial values for selection screen fields
  lcl_main=>set_selscreen_data( ).

START-OF-SELECTION.
  CREATE OBJECT go_main.
*Get Custom Objects from Standard SCMON Tables
  CALL METHOD go_main->get_data( ).

END-OF-SELECTION.
*Put Custom Objects from Standard SCMON Tables to Custom SCMON Tables
  CALL METHOD go_main->put_data( ).

* Delete all data collected in SAP standard SCMON tables
  CALL METHOD go_main->delete_data( ).
  FREE go_main.
