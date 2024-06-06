class ZCL_ZOV_DPC_EXT definition
  public
  inheriting from ZCL_ZOV_DPC
  create public .

public section.
protected section.

  methods MENSAGEMSET_CREATE_ENTITY
    redefinition .
  methods MENSAGEMSET_DELETE_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITYSET
    redefinition .
  methods MENSAGEMSET_UPDATE_ENTITY
    redefinition .
  methods OVCABSET_CREATE_ENTITY
    redefinition .
  methods OVCABSET_DELETE_ENTITY
    redefinition .
  methods OVCABSET_GET_ENTITY
    redefinition .
  methods OVCABSET_GET_ENTITYSET
    redefinition .
  methods OVCABSET_UPDATE_ENTITY
    redefinition .
  methods OVITEMSET_CREATE_ENTITY
    redefinition .
  methods OVITEMSET_DELETE_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITYSET
    redefinition .
  methods OVITEMSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC_EXT IMPLEMENTATION.


  method MENSAGEMSET_CREATE_ENTITY.
  endmethod.


  method MENSAGEMSET_DELETE_ENTITY.
**TRY.
*CALL METHOD SUPER->MENSAGEMSET_DELETE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  method MENSAGEMSET_GET_ENTITY.
  endmethod.


  method MENSAGEMSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->MENSAGEMSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  method MENSAGEMSET_UPDATE_ENTITY.
  endmethod.


  METHOD ovcabset_create_entity.
    DATA: ld_lastid TYPE int4.
    DATA: ls_cab    TYPE zovcab.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_cab.

    ls_cab-criacao_data    = sy-datum.
    ls_cab-criacao_hora    = sy-uzeit.
    ls_cab-criacao_usuario = sy-uname.

    SELECT SINGLE MAX( ordemid )
      INTO ld_lastid
      FROM zovcab.

    ls_cab-ordemid = ld_lastid + 1.
    INSERT zovcab FROM ls_cab.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir ordem'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " atualizando
    MOVE-CORRESPONDING ls_cab TO er_entity.

    CONVERT
      DATE ls_cab-criacao_data
      TIME ls_cab-criacao_hora
      INTO TIME STAMP er_entity-datacriacao
      TIME ZONE 'UTC'. "sy-zonlo.
  ENDMETHOD.


  method OVCABSET_DELETE_ENTITY.
  endmethod.


  METHOD ovcabset_get_entity.
    DATA: ld_ordemid TYPE zovcab-ordemid.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_cab     TYPE zovcab.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não informado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
    ld_ordemid = ls_key_tab-value.

    SELECT SINGLE *
      INTO ls_cab
      FROM zovcab
     WHERE ordemid = ld_ordemid.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_cab TO er_entity.

      er_entity-criadopor = ls_cab-criacao_usuario.

      CONVERT DATE ls_cab-criacao_data
              TIME ls_cab-criacao_hora
         INTO TIME STAMP er_entity-datacriacao
         TIME ZONE 'UTC'. "sy-zonlo.
    ELSE.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não encontrado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ovcabset_get_entityset.
    DATA: lt_cab       TYPE STANDARD TABLE OF zovcab.
    DATA: ls_cab       TYPE zovcab.
    DATA: ls_entityset LIKE LINE OF et_entityset.

    DATA: lt_orderby   TYPE STANDARD TABLE OF string.
    DATA: ld_orderby   TYPE string.

    " montando orderby dinâmico
    LOOP AT it_order INTO DATA(ls_order).
      TRANSLATE ls_order-property TO UPPER CASE.
      TRANSLATE ls_order-order TO UPPER CASE.
      IF ls_order-order = 'DESC'.
        ls_order-order = 'DESCENDING'.
      ELSE.
        ls_order-order = 'ASCENDING'.
      ENDIF.
      APPEND |{ ls_order-property } { ls_order-order }|
          TO lt_orderby.
    ENDLOOP.
    CONCATENATE LINES OF lt_orderby INTO ld_orderby SEPARATED BY ', '. " Ajuste 05/06/2024

    " ordenação obrigatória caso nenhuma seja definida
    IF ld_orderby = '' .
      ld_orderby = 'OrdemId ASCENDING'.
    ENDIF.

    SELECT *
      FROM zovcab
     WHERE (iv_filter_string)
  ORDER BY (ld_orderby)
      INTO TABLE @lt_cab
     UP TO @is_paging-top ROWS
    OFFSET @is_paging-skip.

*    SELECT *
*      INTO TABLE lt_cab
*      FROM zovcab
*      WHERE (iv_filter_string)
*      ORDER BY (ld_orderby).


    LOOP AT lt_cab INTO ls_cab.
      CLEAR ls_entityset.
      MOVE-CORRESPONDING ls_cab TO ls_entityset.

      ls_entityset-criadopor = ls_cab-criacao_usuario.

      CONVERT DATE ls_cab-criacao_data
              TIME ls_cab-criacao_hora
         INTO TIME STAMP ls_entityset-datacriacao
         TIME ZONE 'UTC'. "sy-zonlo.

      APPEND ls_entityset TO et_entityset.
    ENDLOOP.
  ENDMETHOD.


  METHOD ovcabset_update_entity.
    DATA: ld_error TYPE flag.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    er_entity-ordemid = it_key_tab[ name = 'OrdemId' ]-value.

    " validações
    IF er_entity-clienteid = 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Cliente vazio'
      ).
    ENDIF.

    IF er_entity-totalordem < 10.
      ld_error = 'X'.
      lo_msg->add_message(
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZOV'
          iv_msg_number = 1
          iv_msg_v1     = 'R$ 10,00'
          iv_msg_v2     = |{ er_entity-ordemid }|
      ).
    ENDIF.

    IF ld_error = 'X'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg
          http_status_code  = 500.
    ENDIF.

    UPDATE zovcab
       SET clienteid  = er_entity-clienteid
           totalitens = er_entity-totalitens
           totalfrete = er_entity-totalfrete
           totalordem = er_entity-totalordem
           status     = er_entity-status
     WHERE ordemid    = er_entity-ordemid.

    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao atualizar ordem'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ovitemset_create_entity.
    DATA: ls_item TYPE zovitem.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_item.

    IF er_entity-itemid = 0.
      SELECT SINGLE MAX( itemid )
        INTO er_entity-itemid
        FROM zovitem
       WHERE ordemid = er_entity-ordemid.

      er_entity-itemid = er_entity-itemid + 1.
    ENDIF.

    INSERT zovitem FROM ls_item.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir item'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  method OVITEMSET_DELETE_ENTITY.
  endmethod.


  METHOD ovitemset_get_entity.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_item    TYPE zovitem.
    DATA: ld_error   TYPE flag.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
    IF sy-subrc <> 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não informado'
      ).
    ENDIF.
    ls_item-ordemid = ls_key_tab-value.

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ItemId'.
    IF sy-subrc <> 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id do item não informado'
      ).
    ENDIF.
    ls_item-itemid = ls_key_tab-value.

    IF ld_error = 'X'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    SELECT SINGLE *
      INTO ls_item
      FROM zovitem
     WHERE ordemid = ls_item-ordemid
       AND itemid  = ls_item-itemid.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_item TO er_entity.
    ELSE.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Item não encontrado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ovitemset_get_entityset.
    DATA: ld_ordemid       TYPE int4,
          lt_ordemid_range TYPE RANGE OF int4,
          ls_ordemid_range LIKE LINE OF lt_ordemid_range,
          ls_key_tab       LIKE   LINE OF it_key_tab.

    "input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemId'.
    IF sy-subrc EQ 0.
      ld_ordemid = ls_key_tab-value.

      CLEAR ls_ordemid_range.
      ls_ordemid_range-sign = 'I'.
      ls_ordemid_range-option = 'EQ'.
      ls_ordemid_range-low = ld_ordemid.
      APPEND ls_ordemid_range TO lt_ordemid_range.
    ENDIF.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      FROM zovitem
      WHERE ordemid IN lt_ordemid_range.
  ENDMETHOD.


  method OVITEMSET_UPDATE_ENTITY.
  endmethod.
ENDCLASS.
