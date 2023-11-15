class zcl_cmd_customer definition
* SUPPORTS <7.4 *
  public
  create public .

  public section.

    data customer type kna1-kunnr read-only .
    data mode type zcl_cmd_util=>t_mode .
    data central type ref to zcl_cmd_central .
    data address type ref to zcl_cmd_address .
    data vat type ref to zcl_cmd_vat .
    data texts type ref to zcl_cmd_texts .
    data: extension_id type guid_32 read-only.
    data ZLOG type ref to ZCL_API_LOG .
    data tax_indicator type ref to zcl_cmd_tax_ind .

    methods constructor
      importing
        value(i_customer) type kna1-kunnr optional
        value(i_extension_classes) type zcl_cmd_extensions=>tt_extension_classes optional
        value(I_LOGID) type CLIKE optional
      raising
        zcx_cmd_customer .
    "! Gets all customer data from DB and store it in CMD_EI_API structure
    methods get_data
      importing
        !i_reread_db         type abap_bool default abap_true
      returning
        value(r_master_data) type cmds_ei_extern
      raising
        zcx_cmd_customer .
    methods save
      importing
        !i_test_run          type abap_bool default abap_false
        !i_wait_after_commit type abap_bool default abap_false
      exporting
        !e_customer          type kna1-kunnr
        !r_master_data       type cmds_ei_extern
      raising
        zcx_cmd_customer .
    "! If you'd like to fill all data by yourself.
    methods set_master_data
      importing
        !i_master_data type cmds_ei_extern .
    methods add_new_contact
      returning
        value(r_contact) type ref to zcl_cmd_contact .
    methods change_contact
      importing
        value(i_contact) type knvp-parnr
      returning
        value(r_contact) type ref to zcl_cmd_contact
      raising
        zcx_cmd_customer .
    methods get_contact
      importing
        value(i_contact) type knvp-parnr
      returning
        value(r_contact) type ref to zcl_cmd_contact
      raising
        zcx_cmd_customer .
    methods get_contact_by_name
      importing
        value(i_firstname) type bapiad3vl-firstname
        value(i_lastname)  type bapiad3vl-lastname
      returning
        value(r_contact)   type ref to zcl_cmd_contact
      raising
        zcx_cmd_customer .
    "! @parameter i_contact |
    "! @parameter i_raise_error | Default set to False, as if contact does not exists then it's no problem that it cannot be deleted
    "! @parameter r_contact |
    "! @raising zcx_cmd_customer |
    methods delete_contact
      importing
        value(i_contact)     type knvp-parnr
        value(i_raise_error) type abap_bool default abap_false
      returning
        value(r_contact)     type ref to zcl_cmd_contact
      raising
        zcx_cmd_customer .
    methods change_sales_org
      importing
        value(i_sales_org)     type vkorg
        value(i_distr_channel) type vtweg
        value(i_division)      type spart
      returning
        value(r_sales)         type ref to zcl_cmd_sales
      raising
        zcx_cmd_customer .
    methods create_sales_org
      importing
        value(i_sales_org)     type vkorg
        value(i_distr_channel) type vtweg
        value(i_division)      type spart
      returning
        value(r_sales)         type ref to zcl_cmd_sales
      raising
        zcx_cmd_customer .
    methods delete_sales_org
      importing
        value(i_sales_org)     type vkorg
        value(i_distr_channel) type vtweg
        value(i_division)      type spart
      returning
        value(r_sales)         type ref to zcl_cmd_sales
      raising
        zcx_cmd_customer .
    methods get_sales_org
      importing
        value(i_sales_org)     type vkorg
        value(i_distr_channel) type vtweg
        value(i_division)      type spart
      returning
        value(r_sales)         type ref to zcl_cmd_sales
      raising
        zcx_cmd_customer .
    methods change_company
      importing
        value(i_company_code) type bukrs
      returning
        value(r_company)      type ref to zcl_cmd_company
      raising
        zcx_cmd_customer .
    methods create_company
      importing
        value(i_company_code) type bukrs
      returning
        value(r_company)      type ref to zcl_cmd_company
      raising
        zcx_cmd_customer .
    methods delete_company
      importing
        value(i_company_code) type bukrs
      returning
        value(r_company)      type ref to zcl_cmd_company
      raising
        zcx_cmd_customer .
    methods get_company
      importing
        value(i_company_code) type bukrs
      returning
        value(r_company)      type ref to zcl_cmd_company
      raising
        zcx_cmd_customer .
  protected section.
    data: master_data type cmds_ei_extern.
    methods: lock raising zcx_cmd_customer.
    methods: unlock raising zcx_cmd_customer.
    methods RAISE_MESSAGE
    importing
      value(MSGID) type SCX_T100KEY-MSGID default 'Z_CMD_CUSTOMERS'
      value(MSGNO) type SCX_T100KEY-MSGNO default '999'
      value(ATTR1) type CLIKE optional
      value(ATTR2) type CLIKE optional
      value(ATTR3) type CLIKE optional
      value(ATTR4) type CLIKE optional
      value(T100KEY) type SCX_T100KEY optional
    raising
      ZCX_CMD_CUSTOMER .
    data: "! Kept only when get_data is used to be able to compare later new and old data
          original_data type cmds_ei_extern.



PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CMD_CUSTOMER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->ADD_NEW_CONTACT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_CONTACT                      TYPE REF TO ZCL_CMD_CONTACT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_new_contact.
    DATA: lr_contact_data TYPE REF TO CMDS_EI_CONTACTS. "vmds_ei_extern-central_data-contact-contacts .
    TRY.
        r_contact ?= zcl_cmd_contact=>create_instance( i_extension_id = extension_id
                                                       i_customer = customer ).
        INSERT INITIAL LINE INTO TABLE master_data-central_data-contact-contacts REFERENCE INTO lr_contact_data.
*        INSERT INITIAL LINE INTO TABLE master_data-central_data-contact-contacts REFERENCE INTO DATA(cont).
        IF sy-subrc EQ 0.
          r_contact->set_data( lr_contact_data ).
        ENDIF.
      CATCH zcx_cmd_customer.
        "In case of new contact there should not be any error here
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->CHANGE_COMPANY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_COMPANY_CODE                 TYPE        BUKRS
* | [<-()] R_COMPANY                      TYPE REF TO ZCL_CMD_COMPANY
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method change_company.
    FIELD-SYMBOLS: <company> LIKE LINE OF master_data-company_data-company.
    READ TABLE master_data-company_data-company[] WITH KEY data_key-bukrs = i_company_code ASSIGNING <company>.
*    assign master_data-company_data-company[ data_key-bukrs = i_company_code
*                                                                      ] to field-symbol(<company>).
    IF sy-subrc EQ 0.
      DATA: lr_company TYPE REF TO cmds_ei_company.
      GET REFERENCE OF <company> INTO lr_company.
      r_company ?= zcl_cmd_company=>create_instance( i_extension_id = extension_id ).
      r_company->set_data( lr_company ). "REF #( <company> ) ).
      r_company->set_task( zcl_cmd_util=>mode-change ).
    ELSE.
      raise_message( msgno = '013' attr1 = i_company_code ).
    ENDIF.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->CHANGE_CONTACT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CONTACT                      TYPE        KNVP-PARNR
* | [<-()] R_CONTACT                      TYPE REF TO ZCL_CMD_CONTACT
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method change_contact.
    DATA: lo_exc TYPE REF TO zcx_cmd_customer.
    FIELD-SYMBOLS: <cont> LIKE LINE OF master_data-central_data-contact-contacts.
    CHECK i_contact IS NOT INITIAL.
    TRY.
        r_contact ?= zcl_cmd_contact=>create_instance( i_extension_id = extension_id
                                                       i_customer = customer
                                                       i_contact  = i_contact ).
        READ TABLE master_data-central_data-contact-contacts WITH KEY data_key-parnr = i_contact ASSIGNING <cont>.
*        ASSIGN master_data-central_data-contact-contacts[ data_key-parnr = i_contact ] TO FIELD-SYMBOL(<cont>).
        IF sy-subrc EQ 0.
          DATA: lr_contact TYPE REF TO cmds_ei_contacts.
          GET REFERENCE OF <cont> INTO lr_contact.
          r_contact->set_data( lr_contact )."REF #( <cont> ) ).
          r_contact->set_mode( zcl_cmd_util=>mode-change ).
        ENDIF.

      CATCH zcx_cmd_customer INTO lo_exc.
        me->zlog->add_message( iv_textmsg1 = 'Contact doesn''t exists' iv_textmsgty = 'W' ).
        raise_message( t100key = lo_exc->if_t100_message~t100key ).
    ENDTRY.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->CHANGE_SALES_ORG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SALES_ORG                    TYPE        VKORG
* | [--->] I_DISTR_CHANNEL                TYPE        VTWEG
* | [--->] I_DIVISION                     TYPE        SPART
* | [<-()] R_SALES                        TYPE REF TO ZCL_CMD_SALES
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method change_sales_org.
    FIELD-SYMBOLS: <sales> LIKE LINE OF master_data-sales_data-sales.
    READ TABLE master_data-sales_data-sales WITH KEY data_key-vkorg = i_sales_org
                                                     data_key-vtweg = i_distr_channel
                                                     data_key-spart = i_division ASSIGNING <sales>.
    if sy-subrc eq 0.
      DATA: lr_sales_md TYPE REF TO cmds_ei_sales.
      GET REFERENCE OF <sales> INTO lr_sales_md.
      r_sales ?= zcl_cmd_sales=>create_instance( i_extension_id = extension_id ).
      r_sales->set_data( lr_sales_md ). " ref #( <sales> ) ).
      r_sales->set_task( zcl_cmd_util=>mode-change ).
    else.
      raise_message( msgno = '005' attr1 = customer
                                   attr2 = i_sales_org
                                   attr3 = i_distr_channel
                                   attr4 = i_division ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CUSTOMER                     TYPE        KNA1-KUNNR(optional)
* | [--->] I_EXTENSION_CLASSES            TYPE        ZCL_CMD_EXTENSIONS=>TT_EXTENSION_CLASSES(optional)
* | [--->] I_LOGID                        TYPE        CLIKE(optional)
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
    me->zlog = zcl_api_Log=>init_popup_log( iv_object = 'ZCL_CMD_CUSTOMER' iv_subobject = i_logid ).

*    customer = i_customer.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = i_customer
      IMPORTING
        output        = customer.

    extension_id = zcl_cmd_extensions=>set_extensions( extensions = i_extension_classes ).
    if customer is initial or customer co '0'.
      mode = zcl_cmd_util=>mode-create.
      master_data-header-object_task = zcl_cmd_util=>mode-create.
    else.
      select single kunnr into customer
          from kna1
          where kunnr eq customer.
      if sy-subrc ne 0.
        raise_message( msgno = '001' attr1 = customer ).
*        raise exception type zcx_cmd_customer exporting no = 001 v1 = conv #( customer ).
      endif.
      get_data( ).
    endif.

    DATA: lr_central  TYPE REF TO cmds_ei_extern-central_data-central-data,
          lr_centralx TYPE REF TO cmds_ei_extern-central_data-central-datax,
          lr_address  TYPE REF TO cmds_ei_extern-central_data-address,
          lr_text     TYPE REF TO cmds_ei_extern-central_data-text,
          lr_vat      TYPE REF TO cmds_ei_extern-central_data-vat_number,
          lr_tax      TYPE REF TO cmds_ei_extern-central_data-tax_ind.

    GET REFERENCE OF: master_data-central_data-central-data  INTO lr_central,
                      master_data-central_data-central-datax INTO lr_centralx,
                      master_data-central_data-address       INTO lr_address,
                      master_data-central_data-text          INTO lr_text,
                      master_data-central_data-vat_number    INTO lr_vat,
                      master_data-central_data-tax_ind       INTO lr_tax.

    central ?= zcl_cmd_central=>create_instance( i_extension_id = extension_id i_data  = lr_central
                                                                               i_datax = lr_centralx ).
    address ?= zcl_cmd_address=>create_instance_c( i_extension_id = extension_id ).
    address->set_data( lr_address ).
    texts ?= zcl_cmd_texts=>create_instance( i_extension_id = extension_id i_texts = lr_text  ).
    vat   ?= zcl_cmd_vat=>create_instance(   i_extension_id = extension_id i_vat   = lr_vat ).
    tax_indicator ?= zcl_cmd_tax_ind=>create_instance( i_extension_id = extension_id i_tax_ind = lr_tax ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->CREATE_COMPANY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_COMPANY_CODE                 TYPE        BUKRS
* | [<-()] R_COMPANY                      TYPE REF TO ZCL_CMD_COMPANY
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_company.
    FIELD-SYMBOLS: <company> LIKE LINE OF master_data-company_data-company.
    READ TABLE master_data-company_data-company WITH KEY data_key-bukrs = i_company_code ASSIGNING <company>.

*    assign master_data-company_data-company[ data_key-bukrs = i_company_code
*                                                                      ] to field-symbol(<company>).
    if sy-subrc ne 0.
      APPEND INITIAL LINE TO master_data-company_data-company ASSIGNING <company>.
      <company>-task = zcl_cmd_util=>mode-create.
      <company>-data_key-bukrs = i_company_code.
*      insert value #( task = zcl_cmd_util=>mode-create
*                      data_key-bukrs = i_company_code
*                    ) into table master_data-company_data-company reference into data(company).
      DATA: lr_company TYPE REF TO cmds_ei_company.
      GET REFERENCE OF <company> INTO lr_company.
      if sy-subrc eq 0.
        r_company ?= zcl_cmd_company=>create_instance( i_extension_id = extension_id ).
        r_company->set_data( lr_company ).
      endif.
    else.
      raise_message( msgno = '014' attr1 = i_company_code ).
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->CREATE_SALES_ORG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SALES_ORG                    TYPE        VKORG
* | [--->] I_DISTR_CHANNEL                TYPE        VTWEG
* | [--->] I_DIVISION                     TYPE        SPART
* | [<-()] R_SALES                        TYPE REF TO ZCL_CMD_SALES
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_sales_org.
    FIELD-SYMBOLS: <sales> LIKE LINE OF master_data-sales_data-sales.
    READ TABLE master_data-sales_data-sales WITH KEY data_key-vkorg = i_sales_org
                                                     data_key-vtweg = i_distr_channel
                                                     data_key-spart = i_division ASSIGNING <sales>.

*    assign master_data-sales_data-sales[ data_key-vkorg = i_sales_org
*                                         data_key-vtweg = i_distr_channel
*                                         data_key-spart = i_division
*                                                                      ] to field-symbol(<sales>).
    if sy-subrc ne 0.
      DATA: lr_sales_md TYPE REF TO cmds_ei_sales.
*      GET REFERENCE OF <sales> INTO lr_sales_md.

      r_sales ?= zcl_cmd_sales=>create_instance( i_extension_id = extension_id ).
      APPEND INITIAL LINE TO master_data-sales_data-sales REFERENCE INTO lr_sales_md.
      lr_sales_md->data_key-vkorg = i_sales_org.
      lr_sales_md->data_key-vtweg = i_distr_channel.
      lr_sales_md->data_key-spart = i_division.
*      insert value #( data_key-vkorg = i_sales_org
*                      data_key-vtweg = i_distr_channel
*                      data_key-spart = i_division
*                     ) into table master_data-sales_data-sales reference into data(sales).
*      if sy-subrc eq 0.
      r_sales->set_data( lr_sales_md ).
      r_sales->set_task( zcl_cmd_util=>mode-create ).
*      endif.
    else.
      raise_message( msgno = '006' attr1 = customer
                                   attr2 = i_sales_org
                                   attr3 = i_distr_channel
                                   attr4 = i_division ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->DELETE_COMPANY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_COMPANY_CODE                 TYPE        BUKRS
* | [<-()] R_COMPANY                      TYPE REF TO ZCL_CMD_COMPANY
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method delete_company.
    FIELD-SYMBOLS: <company> LIKE LINE OF master_data-company_data-company.
    READ TABLE master_data-company_data-company WITH KEY data_key-bukrs = i_company_code ASSIGNING <company>.
*    assign master_data-company_data-company[ data_key-bukrs = i_company_code
*                                                                      ] to field-symbol(<company>).
    if sy-subrc eq 0.
      DATA: lr_company TYPE REF TO cmds_ei_company.
      GET REFERENCE OF <company> INTO lr_company.
      r_company ?= zcl_cmd_company=>create_instance( i_extension_id = extension_id ).
      r_company->set_data( lr_company ). "ref #( <company> ) ).
      r_company->set_loevm( abap_true ).
      r_company->set_task( zcl_cmd_util=>mode-change ).
    else.
      raise_message( msgno = '013' attr1 = i_company_code ).
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->DELETE_CONTACT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CONTACT                      TYPE        KNVP-PARNR
* | [--->] I_RAISE_ERROR                  TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<-()] R_CONTACT                      TYPE REF TO ZCL_CMD_CONTACT
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method delete_contact.
    FIELD-SYMBOLS: <contact> LIKE LINE OF master_data-central_data-contact-contacts,
                   <cc> LIKE LINE OF master_data-central_data-contact-contacts.
    DATA: lr_contact TYPE REF TO cmds_ei_contacts,
          lo_exc TYPE REF TO zcx_cmd_customer.
    check i_contact is not initial.
    try.
        r_contact ?= zcl_cmd_contact=>create_instance( i_extension_id = extension_id
                                                       i_customer = customer
                                                       i_contact  = i_contact ).
        READ TABLE master_data-central_data-contact-contacts WITH KEY data_key-parnr = i_contact ASSIGNING <contact>.
*        assign master_data-central_data-contact-contacts[ data_key-parnr = i_contact ] to field-symbol(<cont>).
        if sy-subrc eq 0.
          " This is needed as in other case contact data are deleted in internal table
          " and there is an error that name is initial. So me must clear current state and
          " set all contacts as to be updated.
          clear master_data-central_data-contact-current_state.
          loop at master_data-central_data-contact-contacts ASSIGNING <cc> where task is initial.
            <cc>-task = zcl_cmd_util=>mode-change.
          endloop.
          GET REFERENCE OF <contact> INTO lr_contact.
          r_contact->set_data( lr_contact ). "REF #( <cont> ) ).
          r_contact->set_mode( zcl_cmd_util=>mode-delete ).
        endif.

      catch zcx_cmd_customer into lo_exc.
        "Contact doesn't exists
        me->zlog->add_message( iv_textmsg1 = 'Contact doesn''t exists' iv_textmsgty = 'W').
        IF i_raise_error EQ abap_true.
          raise_message( t100key = lo_exc->if_t100_message~t100key ).
        ENDIF.
    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->DELETE_SALES_ORG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SALES_ORG                    TYPE        VKORG
* | [--->] I_DISTR_CHANNEL                TYPE        VTWEG
* | [--->] I_DIVISION                     TYPE        SPART
* | [<-()] R_SALES                        TYPE REF TO ZCL_CMD_SALES
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method delete_sales_org.
    FIELD-SYMBOLS: <sales> LIKE LINE OF master_data-sales_data-sales.
    READ TABLE master_data-sales_data-sales WITH KEY data_key-vkorg = i_sales_org
                                                     data_key-vtweg = i_distr_channel
                                                     data_key-spart = i_division ASSIGNING <sales>.

*    assign master_data-sales_data-sales[ data_key-vkorg = i_sales_org
*                                         data_key-vtweg = i_distr_channel
*                                         data_key-spart = i_division
*                                                                      ] to field-symbol(<sales>).
    if sy-subrc eq 0.
      DATA: lr_sales_md TYPE REF TO cmds_ei_sales.
      GET REFERENCE OF <sales> INTO lr_sales_md.
      r_sales ?= zcl_cmd_sales=>create_instance( i_extension_id = extension_id ).
      r_sales->set_data( lr_sales_md ). " ref #( <sales> ) ).
      r_sales->set_loevm( abap_true ).
      r_sales->set_task( zcl_cmd_util=>mode-change ).
    else.
      raise_message( msgno = '005' attr1 = customer
                                   attr2 = i_sales_org
                                   attr3 = i_distr_channel
                                   attr4 = i_division ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->GET_COMPANY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_COMPANY_CODE                 TYPE        BUKRS
* | [<-()] R_COMPANY                      TYPE REF TO ZCL_CMD_COMPANY
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_company.
    FIELD-SYMBOLS: <company> LIKE LINE OF master_data-company_data-company.
    READ TABLE master_data-company_data-company WITH KEY data_key-bukrs = i_company_code ASSIGNING <company>.

*    assign master_data-company_data-company[ data_key-bukrs = i_company_code
*                                                                         ] to field-symbol(<company>).
    IF sy-subrc EQ 0.
      DATA: lr_company TYPE REF TO cmds_ei_company.
      GET REFERENCE OF <company> INTO lr_company.

      r_company ?= zcl_cmd_company=>create_instance( i_extension_id = extension_id ).
      r_company->set_data( lr_company )."REF #( <company> ) ).
    ELSE.
      raise_message( msgno = '013' attr1 = i_company_code ).
    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->GET_CONTACT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CONTACT                      TYPE        KNVP-PARNR
* | [<-()] R_CONTACT                      TYPE REF TO ZCL_CMD_CONTACT
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_contact.
    FIELD-SYMBOLS: <contact> LIKE LINE OF master_data-central_data-contact-contacts.
    DATA: lr_contact TYPE REF TO cmds_ei_contacts,
          lo_exc TYPE REF TO zcx_cmd_customer.

    try.
        r_contact ?= zcl_cmd_contact=>create_instance( i_extension_id = extension_id
                                                       i_customer = customer
                                                       i_contact  = i_contact ).
        READ TABLE master_data-central_data-contact-contacts WITH KEY data_key-parnr = i_contact ASSIGNING <contact>.
*        assign master_data-central_data-contact-contacts[ data_key-parnr = i_contact ] to field-symbol(<cont>).
        if sy-subrc eq 0.
          GET REFERENCE OF <contact> INTO lr_contact.
          r_contact->set_data( lr_contact ). "REF #( <cont> ) ).
          r_contact->set_mode( zcl_cmd_util=>mode-change ).
        endif.
      CATCH zcx_cmd_customer INTO lo_exc.
        me->zlog->add_message( iv_textmsg1 = 'Contact doesn''t exists' iv_textmsgty = 'W').
        raise_message( t100key = lo_exc->if_t100_message~t100key ).
    endtry.



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->GET_CONTACT_BY_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_FIRSTNAME                    TYPE        BAPIAD3VL-FIRSTNAME
* | [--->] I_LASTNAME                     TYPE        BAPIAD3VL-LASTNAME
* | [<-()] R_CONTACT                      TYPE REF TO ZCL_CMD_CONTACT
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_contact_by_name.
    FIELD-SYMBOLS: <contact> LIKE LINE OF master_data-central_data-contact-contacts.
    DATA: lr_contact TYPE REF TO cmds_ei_contacts,
          lo_exc TYPE REF TO zcx_cmd_customer.

    check i_lastname is not initial.
    READ TABLE master_data-central_data-contact-contacts WITH KEY address_type_3-postal-data-firstname = i_firstname
                                                                  address_type_3-postal-data-lastname = i_lastname ASSIGNING <contact>.

*    assign master_data-central_data-contact-contacts[  address_type_3-postal-data-firstname  = i_firstname
*                                                       address_type_3-postal-data-lastname   = i_lastname
*                                                         ] to field-symbol(<cont>).
    if sy-subrc eq 0.
      try.
        GET REFERENCE OF <contact> INTO lr_contact.
        r_contact ?= zcl_cmd_contact=>create_instance( i_extension_id = extension_id
                                                       i_customer = customer
                                                       i_contact  = <contact>-data_key-parnr ).
          r_contact->set_data( lr_contact ). "REF #( <cont> ) ).
          r_contact->set_mode( zcl_cmd_util=>mode-change ).
         CATCH zcx_cmd_customer INTO lo_exc.
          "Contact doesn't exists
          me->zlog->add_message( iv_textmsg1 = 'Contact doesn''t exists' iv_textmsgty = 'W').
          raise_message( t100key = lo_exc->if_t100_message~t100key ).
      endtry.
    endif.



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_REREAD_DB                    TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R_MASTER_DATA                  TYPE        CMDS_EI_EXTERN
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_data.
     DATA: ls_master_in TYPE cMDS_EI_MAIN,
           ls_master_out TYPE cMDS_EI_MAIN,
           ls_error TYPE CVIS_MESSAGE.
     FIELD-SYMBOLS: <ls_head> TYPE cMDS_EI_EXTERN,
                    <customer> TYPE cmds_ei_extern.
    check customer is not initial.

    if i_reread_db eq abap_true.

      APPEND INITIAL LINE TO ls_master_in-customers ASSIGNING <ls_head>.
      <ls_head>-header-object_task = zcl_cmd_util=>mode-modify .
      <ls_head>-header-object_instance = customer .

      cmd_ei_api_extract=>get_data(
        exporting
          is_master_data = ls_master_in
*          value #( customers = value #( ( header = value #( object_task = zcl_cmd_util=>mode-modify object_instance = customer ) ) ) )
        importing
          es_master_data = ls_master_out
          es_error       = ls_error
      ).
      if ls_error-is_error eq abap_false.
        READ TABLE ls_master_out-customers ASSIGNING <customer> INDEX 1.
*        assign es_master_data-customers[ 1 ] to field-symbol(<customer>).
        if sy-subrc eq 0.
          r_master_data = <customer>.
          master_data = <customer>.
          original_data = <customer>.

        endif.
      else.
        raise_message( ).
*        raise exception type zcx_cmd_customer exporting messages = e_error-messages.
      endif.
    else.
      r_master_data = master_data.
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->GET_SALES_ORG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SALES_ORG                    TYPE        VKORG
* | [--->] I_DISTR_CHANNEL                TYPE        VTWEG
* | [--->] I_DIVISION                     TYPE        SPART
* | [<-()] R_SALES                        TYPE REF TO ZCL_CMD_SALES
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_sales_org.
    FIELD-SYMBOLS: <sales> LIKE LINE OF master_data-sales_data-sales.
    READ TABLE master_data-sales_data-sales WITH KEY data_key-vkorg = i_sales_org
                                                     data_key-vtweg = i_distr_channel
                                                     data_key-spart = i_division ASSIGNING <sales>.

*    assign master_data-sales_data-sales[ data_key-vkorg = i_sales_org
*                                         data_key-vtweg = i_distr_channel
*                                         data_key-spart = i_division
*                                                                      ] to field-symbol(<sales>).
    if sy-subrc eq 0.
      DATA: lr_sales_md TYPE REF TO cmds_ei_sales.
      GET REFERENCE OF <sales> INTO lr_sales_md.
      r_sales ?= zcl_cmd_sales=>create_instance( i_extension_id = extension_id ).
      r_sales->set_data( lr_sales_md ). " ref #( <sales> ) ).
    else.
      raise_message( msgno = '005' attr1 = customer
                                   attr2 = i_sales_org
                                   attr3 = i_distr_channel
                                   attr4 = i_division ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CMD_CUSTOMER->LOCK
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method lock.
    check customer is not initial.
    call function 'ENQUEUE_EXKNA1'
      exporting
        mode_kna1      = 'E'
        mandt          = sy-mandt
        kunnr          = customer
*       x_kunnr        = x_kunnr    " Fill argument 02 with initial value?
*       _scope         = _scope
*       _wait          = _wait
*       _collect       = _collect    " Initially only collect lock
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.
    if sy-subrc <> 0.
      raise_message( msgid = sy-msgid attr1 = sy-msgv1 attr3 = sy-msgv3
                     msgno = sy-msgno attr2 = sy-msgv2 attr4 = sy-msgv4 ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CMD_CUSTOMER->RAISE_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] MSGID                          TYPE        SCX_T100KEY-MSGID (default ='Z_CMD_CUSTOMERS')
* | [--->] MSGNO                          TYPE        SCX_T100KEY-MSGNO (default ='999')
* | [--->] ATTR1                          TYPE        CLIKE(optional)
* | [--->] ATTR2                          TYPE        CLIKE(optional)
* | [--->] ATTR3                          TYPE        CLIKE(optional)
* | [--->] ATTR4                          TYPE        CLIKE(optional)
* | [--->] T100KEY                        TYPE        SCX_T100KEY(optional)
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
   METHOD raise_message.
    DATA: ls_t100key TYPE scx_t100key.
    IF t100key IS NOT INITIAL.
      ls_t100key = t100key.
    ELSE.
      ls_t100key-msgid = msgid.
      ls_t100key-msgno = msgno.
      ls_t100key-attr1 = attr1.
      ls_t100key-attr2 = attr2.
      ls_t100key-attr3 = attr3.
      ls_t100key-attr4 = attr4.
    ENDIF.
    me->zlog->add_message( is_t100key = ls_t100key iv_textmsgty = 'E' ).
    me->zlog->save( iv_save_info = space ).
    RAISE EXCEPTION TYPE zcx_cmd_customer
        EXPORTING
          textid = ls_t100key.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TEST_RUN                     TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] I_WAIT_AFTER_COMMIT            TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<---] E_CUSTOMER                     TYPE        KNA1-KUNNR
* | [<---] R_MASTER_DATA                  TYPE        CMDS_EI_EXTERN
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save.


    IF master_data IS INITIAL.
      raise_message( msgno = '002' ).
*      raise exception type zcx_cmd_customer exporting no = 002.
    ENDIF.


    lock( ).

    "Delete Not changed company code data
    IF mode NE zcl_cmd_util=>mode-create.
      DELETE master_data-company_data-company WHERE task IS INITIAL OR task EQ zcl_cmd_util=>mode-current_state.
    ENDIF.

    "Delete other sales orgs from the list to not check the authorizations.
    DELETE master_data-sales_data-sales WHERE task IS INITIAL OR task EQ zcl_cmd_util=>mode-current_state.

    DATA: ls_master_data TYPE cmds_ei_main.
    APPEND master_data TO ls_master_data-customers[].
*    data(bapi_structure) = value cmds_ei_main( customers = value #( ( master_data ) ) ).

    DATA:
     correct_data  TYPE cmds_ei_main,
     correct_messages  TYPE cvis_message,
     error_messages  TYPE cvis_message.

    cmd_ei_api=>maintain_bapi(
      EXPORTING
         iv_test_run              = i_test_run
         iv_collect_messages      = abap_true
         is_master_data           = ls_master_data
*         is_master_data           = value #( customers = value #( ( master_data ) ) )
      IMPORTING
        es_master_data_correct   = correct_data
        es_message_correct       = correct_messages
        es_message_defective     = error_messages
        ).

    IF error_messages-is_error EQ abap_false AND i_test_run EQ abap_false.

      IF error_messages-is_error EQ abap_false.
        zcl_cmd_util=>commit_work( i_wait = i_wait_after_commit ).
        unlock( ).
        DATA: ls_customer LIKE LINE OF correct_data-customers.
        READ TABLE correct_data-customers INTO ls_customer INDEX 1.
        IF sy-subrc = 0.
          customer = ls_customer-header-object_instance-kunnr.
          e_customer = customer.

          IF mode = zcl_cmd_util=>mode-create.
            me->zlog->add_message( iv_textmsg1 = 'Customer created' iv_textmsg2 = customer ).
          ELSE.
            me->zlog->add_message( iv_textmsg1 = 'Customer updated' iv_textmsg2 = customer ).
          ENDIF.

          mode = zcl_cmd_util=>mode-change.

        ENDIF.
*        try.
*            customer = correct_data-customers[ 1 ]-header-object_instance-kunnr.
*            mode = zcl_cmd_util=>mode-change.
*            e_customer = customer.
*          catch cx_sy_itab_line_not_found.
*
*        endtry.
      ELSE.
        zcl_cmd_util=>rollback_work( ).
        unlock( ).
        me->zlog->add_message( it_bapiret = error_messages-messages[] ).
        raise_message( msgno = '003' ).
*        raise exception type zcx_cmd_customer exporting no = 003 messages = error_messages-messages.
      ENDIF.
    ELSEIF error_messages-is_error EQ abap_true.
      IF i_test_run EQ abap_false.
        zcl_cmd_util=>rollback_work( ).
      ENDIF.
      unlock( ).
      me->zlog->add_message( it_bapiret = error_messages-messages[] ).
      raise_message( msgno = '003' ).
*      raise exception type zcx_cmd_customer exporting no = 003 messages = error_messages-messages.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CMD_CUSTOMER->SET_MASTER_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MASTER_DATA                  TYPE        CMDS_EI_EXTERN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_master_data.
    master_data = i_master_data.

    IF i_master_data-header-object_task = cvi_ei_api_map_structure=>gc_update.

      zcl_vmd_vendor=>update_master_data( EXPORTING is_old = original_data
                                          CHANGING  cs_new = master_data   ).

    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CMD_CUSTOMER->UNLOCK
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_CMD_CUSTOMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method unlock.
    call function 'DEQUEUE_EXKNA1'
      exporting
        mode_kna1 = 'E'
        mandt     = sy-mandt    " 01th enqueue argument
        kunnr     = customer
*       x_kunnr   = x_kunnr    " Fill argument 02 with initial value?
*       _scope    = _scope
*       _synchron = _synchron    " Synchonous unlock
*       _collect  = _collect    " Initially only collect lock
      .
  endmethod.
ENDCLASS.
