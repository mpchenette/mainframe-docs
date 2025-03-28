*&---------------------------------------------------------------------*
* ABAP Code Examples
* Note: These examples are for reference only and must be run in an SAP system
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Example 1: Basic SELECT statement
*----------------------------------------------------------------------*
DATA: lt_users TYPE TABLE OF usr02.
SELECT * FROM usr02 
  INTO TABLE @lt_users
  WHERE bname LIKE 'TEST%'.

*----------------------------------------------------------------------*
* Example 2: Internal Table handling
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_employee,
         id         TYPE n LENGTH 8,
         first_name TYPE c LENGTH 20,
         last_name  TYPE c LENGTH 20,
       END OF ty_employee.

DATA: lt_employees TYPE TABLE OF ty_employee,
      ls_employee  TYPE ty_employee.

ls_employee-id = '10000001'.
ls_employee-first_name = 'John'.
ls_employee-last_name = 'Doe'.
APPEND ls_employee TO lt_employees.

*----------------------------------------------------------------------*
* Example 3: ALV Grid Display
*----------------------------------------------------------------------*
DATA: lo_alv TYPE REF TO cl_salv_table.

TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = lt_employees ).
    
    lo_alv->display( ).
  CATCH cx_salv_msg.
    MESSAGE 'Error creating ALV' TYPE 'E'.
ENDTRY.

*----------------------------------------------------------------------*
* Example 4: Function Module Call
*----------------------------------------------------------------------*
DATA: lv_partner TYPE bu_partner.

CALL FUNCTION 'BAPI_CUSTOMER_GETDETAIL'
  EXPORTING
    customerid       = '1000000'
  IMPORTING
    customerno      = lv_partner
  EXCEPTIONS
    not_found       = 1
    OTHERS          = 2.

IF sy-subrc <> 0.
  MESSAGE 'Customer not found' TYPE 'E'.
ENDIF.

*----------------------------------------------------------------------*
* Example 5: Class Definition and Implementation
*----------------------------------------------------------------------*
CLASS lcl_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING
          iv_num1        TYPE i
          iv_num2        TYPE i
        RETURNING
          VALUE(rv_sum)  TYPE i.
ENDCLASS.

CLASS lcl_calculator IMPLEMENTATION.
  METHOD add.
    rv_sum = iv_num1 + iv_num2.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Example 6: Data Dictionary Table Creation (SE11 equivalent)
*----------------------------------------------------------------------*
* Note: This would typically be done in SE11, but here's the ABAP code
@EndUserText.label : 'Customer Master Data'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zcustomer_master {
  key client        : abap.clnt not null;
  key customer_id   : abap.char(10) not null;
  customer_name     : abap.char(40);
  street           : abap.char(60);
  city             : abap.char(40);
  country          : abap.char(3);
  created_by       : syuname;
  created_date     : sydatum;
}

*----------------------------------------------------------------------*
* Example 7: Web Dynpro ABAP Component Controller
*----------------------------------------------------------------------*
CLASS lcl_web_dynpro_controller DEFINITION.
  PUBLIC SECTION.
    METHODS:
      wddoinit,
      handle_button_click
        FOR EVENT button_click OF cl_web_dynpro_button
        IMPORTING sender.
ENDCLASS.

CLASS lcl_web_dynpro_controller IMPLEMENTATION.
  METHOD wddoinit.
    * Initialize component data
  ENDMETHOD.

  METHOD handle_button_click.
    * Handle button click event
  ENDMETHOD.
ENDCLASS.