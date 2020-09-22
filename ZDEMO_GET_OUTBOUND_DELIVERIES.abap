*&---------------------------------------------------------------------*
*& Report ZDEMO_GET_INBOUND_DELIVERIES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_get_outbound_deliveries.
TYPES:
  BEGIN OF ty_final,
    docno        TYPE /scdl/dl_docno_int, "EWM Document
    doccat       TYPE /scdl/dl_doccat,    "EWM Doc Category
    doctype      TYPE /scdl/dl_doctype,   " EWM Doc Type
    inco1        TYPE /scdl/dl_inco1,     " Inco Terms
    inco2        TYPE /scdl/dl_inco2,     " Inco Terms
    partyno      TYPE /scdl/dl_partyno,   " Partner
    erpdel       TYPE /scdl/dl_refdocno,  " ERP Inbound Del
    erpdelitemno TYPE /scdl/dl_refitemno, " ERP Inbound Del Item
    itemno       TYPE /scdl/dl_itemno,    " EWM Doc Item
    productno    TYPE /scdl/dl_productno, " Product
    batchno      TYPE /scdl/dl_batchno,   " Batch
    product_text TYPE /scdl/dl_text,      " Item Text
    qty          TYPE /scdl/dl_quantity,  " Quantity
    uom          TYPE /scdl/dl_uom,       " UoM
    gmstatus     TYPE char30,             " Goods Mvt Status
    pickstatus   TYPE char30,             " Picking Status
    packstatus   TYPE char30,             " Packing Status
  END OF ty_final,
  ty_final_tab TYPE TABLE OF ty_final WITH KEY docno itemno.

DATA it_selection TYPE /scwm/dlv_selection_tab. " Selection
DATA it_final     TYPE ty_final_tab.
DATA s_final      TYPE ty_final.
DATA s_read       TYPE /scwm/dlv_query_contr_str.
DATA s_incl       TYPE /scwm/dlv_query_incl_str_prd.
DATA g_obd        TYPE vbeln. " ERP Outbound Delivery
DATA g_so         TYPE vbeln. " ERP Sales Order
DATA g_ewmdel     TYPE /scdl/dl_docno_int.


PARAMETERS p_lgnum TYPE /scwm/lgnum   " Warehouse
            OBLIGATORY.
PARAMETERS p_vbeln TYPE vbeln OBLIGATORY.
SELECT-OPTIONS s_obd    FOR g_obd.    " ERP Outbound Delivery
SELECT-OPTIONS s_so     FOR g_so.     " Sales Order
SELECT-OPTIONS s_ewmdel FOR g_ewmdel. " EWM Delivery


START-OF-SELECTION.
  " ERP Outbound Delivery
  it_selection = VALUE /scwm/dlv_selection_tab(
                  FOR ls_obd IN s_obd
                       ( fieldname  = /scdl/if_dl_logfname_c=>sc_refdocno_erp_i
                         sign       = ls_obd-sign
                         option     = ls_obd-option
                         low        = ls_obd-low
                         high       = ls_obd-high )   ).
  " ERP Sales Order
  it_selection = VALUE /scwm/dlv_selection_tab( BASE it_selection
                  FOR ls_so IN s_so
                        ( fieldname = /scdl/if_dl_logfname_c=>sc_refdocno_so_i
                          sign      = ls_so-sign
                          option    = ls_so-option
                          low       = ls_so-low
                          high      = ls_so-high )   ).

  " EWM Outbound Delivery
  it_selection = VALUE /scwm/dlv_selection_tab(
                  BASE it_selection FOR ls_ewmdel IN s_ewmdel
                        ( fieldname = /scdl/if_dl_logfname_c=>sc_docno_h
                          sign      = ls_ewmdel-sign
                          option    = ls_ewmdel-option
                          low       = ls_ewmdel-low
                          high      = ls_ewmdel-high )   ).

  " Ship To Party
  it_selection = VALUE /scwm/dlv_selection_tab( 
						BASE it_selection  
						( fieldname = /scdl/if_dl_logfname_c=>SC_LOCATIONNO_STPRT_H  
						  sign      = ls_ewmdel-sign
                          option    = ls_ewmdel-option
                          low       = ls_ewmdel-low
                          high      = ls_ewmdel-high ) ).

  s_read = VALUE #( data_retrival_only  = 'X' ).
  s_incl = VALUE #(  head_partyloc      = 'X'
                     head_refdoc        = 'X'
                     head_status        = 'X'
                     item_refdoc        = 'X'
                     item_addmeas       = 'X'
                     item_status        = 'X'
                     item_product_ext   = 'X'
				     item_refdoc        = 'X' ).

  " Get Deliveries
  DATA(o_dlv) = NEW /scwm/cl_dlv_management_prd( ).
  o_dlv->query(
    EXPORTING
      iv_whno         = p_lgnum
      iv_doccat       = 'PDO'  " Outbound Delivery Document Category
      it_selection    = it_selection
      is_read_options = s_read
      is_include_data = s_incl
    IMPORTING
      et_headers = DATA(lt_header)
      et_items = DATA(lt_items) ).


  LOOP AT lt_items INTO DATA(ls_items).
    TRY .
        DATA(ls_header) = lt_header[ docno = ls_items-docno ].
        " Header
        s_final-docno = ls_header-docno.
        s_final-doccat = ls_header-doccat.
        s_final-doctype = ls_header-doctype.
        " Header - Inco Terms
        s_final-inco1 = ls_header-incoterms-inco1.
        s_final-inco2 = ls_header-incoterms-inco2.
        " Header - Party Role
        TRY.
            s_final-partyno = ls_header-partyloc[ party_role = 'SFPRT' ]-partyno.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        " Header - Reference Document
        TRY .
            DATA(ls_refdoc) = ls_header-refdoc[ refdoccat = 'ERP' ].
            s_final-erpdel = ls_refdoc-refdocno.
            s_final-erpdelitemno = ls_refdoc-refitemno.
          CATCH cx_sy_itab_line_not_found .

        ENDTRY.

        " Item - Product
        s_final-itemno        = ls_items-itemno.
        s_final-productno     = ls_items-product-productno.
        s_final-batchno       = ls_items-product-batchno.
        s_final-product_text  = ls_items-product-product_text.
        " Item - Qty
        s_final-qty = ls_items-qty-qty.
        s_final-uom = ls_items-qty-uom.
        " Item - Status
        TRY.
            s_final-gmstatus = COND #( LET lw_value = ls_items-status[ status_type = 'DGI' ]-status_value
                                         IN WHEN lw_value = 0 THEN 'Not Relavant'
                                            WHEN lw_value = 1 THEN 'Not Started'
                                            WHEN lw_value = 2 THEN 'Partially Completed'
                                            WHEN lw_value = 9 THEN 'Completed' ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        TRY.
            s_final-pickstatus = COND #( LET lw_value = ls_items-status[ status_type = 'DPI' ]-status_value
                                         IN WHEN lw_value = 0 THEN 'Not Relavant'
                                            WHEN lw_value = 1 THEN 'Not Started'
                                            WHEN lw_value = 2 THEN 'Partially Completed'
                                            WHEN lw_value = 9 THEN 'Completed' ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        TRY.
            s_final-packstatus = COND #( LET lw_value = ls_items-status[ status_type = 'DPC' ]-status_value
                                         IN WHEN lw_value = 0 THEN 'Not Relavant'
                                            WHEN lw_value = 1 THEN 'Not Started'
                                            WHEN lw_value = 2 THEN 'Partially Completed'
                                            WHEN lw_value = 9 THEN 'Completed' ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        APPEND s_final TO it_final.
        CLEAR s_final.
        CLEAR ls_refdoc.
        CLEAR ls_header.
      CATCH cx_sy_itab_line_not_found .
    ENDTRY.
  ENDLOOP.

END-OF-SELECTION.

  cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                          CHANGING t_table = it_final ).
  IF lo_alv IS BOUND.
    " Column Optimize
    lo_alv->get_columns( )->set_optimize( 'X' ).
    " Column Headings
    lo_alv->get_columns( )->get_column( 'DOCNO' )->set_medium_text( 'EWM Delivery' ).
    lo_alv->get_columns( )->get_column( 'ERPDEL' )->set_medium_text( 'ERP Delivery' ).
    lo_alv->get_columns( )->get_column( 'ERPDELITEMNO' )->set_medium_text( 'ERP Del Item' ).
    lo_alv->get_columns( )->get_column( 'ITEMNO' )->set_medium_text( 'EWM Item No' ).
    lo_alv->get_columns( )->get_column( 'PARTYNO' )->set_medium_text( 'Ship From' ).
    lo_alv->get_columns( )->get_column( 'PRODUCT_TEXT' )->set_medium_text( 'Product Description' ).
    lo_alv->get_columns( )->get_column( 'GMSTATUS' )->set_medium_text( 'PGI Status' ).
    lo_alv->get_columns( )->get_column( 'PICKSTATUS' )->set_medium_text( 'Pick Status' ).
    lo_alv->get_columns( )->get_column( 'PACKSTATUS' )->set_medium_text( 'Pack Status' ).
    lo_alv->display( ).
  ENDIF.
  " Display