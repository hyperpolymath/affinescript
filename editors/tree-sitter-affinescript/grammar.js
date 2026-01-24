// SPDX-License-Identifier: MIT
// Tree-sitter grammar for AffineScript

module.exports = grammar({
  name: 'affinescript',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  conflicts: $ => [
    [$.type_expr, $.expr],
  ],

  rules: {
    source_file: $ => repeat($.decl),

    // Comments
    comment: $ => choice(
      seq('//', /.*/),
      seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')
    ),

    // Identifiers
    identifier: $ => /[a-z_][a-zA-Z0-9_]*/,
    type_identifier: $ => /[A-Z][a-zA-Z0-9_]*/,

    // Literals
    literal: $ => choice(
      $.integer,
      $.float,
      $.boolean,
      $.string,
      $.char,
      $.unit
    ),

    integer: $ => choice(
      /[0-9][0-9_]*/,
      /0x[0-9a-fA-F][0-9a-fA-F_]*/,
      /0b[01][01_]*/,
      /0o[0-7][0-7_]*/
    ),

    float: $ => /[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?/,

    boolean: $ => choice('true', 'false'),

    string: $ => seq(
      '"',
      repeat(choice(
        /[^"\\]+/,
        $.escape_sequence
      )),
      '"'
    ),

    char: $ => seq(
      "'",
      choice(/[^'\\]/, $.escape_sequence),
      "'"
    ),

    escape_sequence: $ => /\\(n|r|t|\\|"|'|x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\})/,

    unit: $ => '()',

    // Declarations
    decl: $ => choice(
      $.fun_decl,
      $.let_decl,
      $.type_decl,
      $.effect_decl,
      $.trait_decl,
      $.impl_decl,
      $.mod_decl,
      $.use_decl,
      $.extern_decl
    ),

    fun_decl: $ => seq(
      optional($.visibility),
      'fn',
      field('name', $.identifier),
      optional($.type_params),
      field('params', $.param_list),
      optional(seq('->', field('return_type', $.type_expr))),
      optional(seq('/', field('effect', $.effect_expr))),
      choice(
        seq('=', field('body', $.expr)),
        field('body', $.block)
      )
    ),

    visibility: $ => 'pub',

    type_params: $ => seq('<', sep1($.type_identifier, ','), '>'),

    param_list: $ => seq('(', optional(sep1($.param, ',')), ')'),

    param: $ => seq(
      field('pattern', $.pattern),
      optional(seq(':', field('type', $.type_expr)))
    ),

    let_decl: $ => seq(
      'let',
      field('pattern', $.pattern),
      optional(seq(':', field('type', $.type_expr))),
      '=',
      field('value', $.expr),
      ';'
    ),

    type_decl: $ => seq(
      'type',
      field('name', $.type_identifier),
      optional($.type_params),
      '=',
      field('definition', $.type_expr),
      ';'
    ),

    effect_decl: $ => seq(
      'effect',
      field('name', $.type_identifier),
      optional(seq(
        '{',
        repeat($.effect_operation),
        '}'
      )),
      ';'
    ),

    effect_operation: $ => seq(
      'fn',
      $.identifier,
      '(',
      optional(sep1($.type_expr, ',')),
      ')',
      optional(seq('->', $.type_expr)),
      ';'
    ),

    trait_decl: $ => seq(
      'trait',
      field('name', $.type_identifier),
      optional($.type_params),
      '{',
      repeat($.trait_item),
      '}'
    ),

    trait_item: $ => seq(
      'fn',
      $.identifier,
      ':',
      $.type_expr,
      ';'
    ),

    impl_decl: $ => seq(
      'impl',
      optional($.type_params),
      optional(seq(field('trait', $.type_expr), 'for')),
      field('type', $.type_expr),
      '{',
      repeat($.impl_item),
      '}'
    ),

    impl_item: $ => $.fun_decl,

    mod_decl: $ => seq(
      'mod',
      field('name', $.identifier),
      '{',
      repeat($.decl),
      '}'
    ),

    use_decl: $ => seq(
      'use',
      sep1($.identifier, '::'),
      ';'
    ),

    extern_decl: $ => seq(
      'extern',
      'fn',
      $.identifier,
      ':',
      $.type_expr,
      ';'
    ),

    // Patterns
    pattern: $ => choice(
      '_',
      $.identifier,
      $.literal,
      $.tuple_pattern,
      $.record_pattern,
      $.constructor_pattern,
      seq($.pattern, '|', $.pattern),
      seq($.pattern, 'as', $.identifier)
    ),

    tuple_pattern: $ => seq('(', sep1($.pattern, ','), ')'),

    record_pattern: $ => seq(
      '{',
      sep1($.record_pattern_field, ','),
      optional('..'),
      '}'
    ),

    record_pattern_field: $ => choice(
      $.identifier,
      seq($.identifier, ':', $.pattern)
    ),

    constructor_pattern: $ => seq(
      $.type_identifier,
      optional(seq('(', sep1($.pattern, ','), ')'))
    ),

    // Expressions
    expr: $ => choice(
      $.literal,
      $.identifier,
      $.binary_expr,
      $.unary_expr,
      $.call_expr,
      $.if_expr,
      $.match_expr,
      $.let_expr,
      $.block,
      $.tuple_expr,
      $.record_expr,
      $.field_expr,
      $.array_expr,
      $.index_expr,
      $.lambda_expr,
      $.handle_expr,
      $.perform_expr,
      $.resume_expr,
      $.annotated_expr,
      $.move_expr,
      seq('(', $.expr, ')')
    ),

    binary_expr: $ => choice(
      prec.left(10, seq($.expr, choice('*', '/', '%'), $.expr)),
      prec.left(9, seq($.expr, choice('+', '-'), $.expr)),
      prec.left(8, seq($.expr, choice('<', '>', '<=', '>='), $.expr)),
      prec.left(7, seq($.expr, choice('==', '!='), $.expr)),
      prec.left(6, seq($.expr, '&&', $.expr)),
      prec.left(5, seq($.expr, '||', $.expr))
    ),

    unary_expr: $ => choice(
      prec(12, seq('-', $.expr)),
      prec(12, seq('!', $.expr)),
      prec(12, seq('*', $.expr)),
      prec(12, seq('&', $.expr))
    ),

    call_expr: $ => prec(15, seq(
      field('function', $.expr),
      field('arguments', $.arg_list)
    )),

    arg_list: $ => seq('(', optional(sep1($.expr, ',')), ')'),

    if_expr: $ => prec.right(seq(
      'if',
      field('condition', $.expr),
      field('then', $.block),
      optional(seq('else', field('else', choice($.block, $.if_expr))))
    )),

    match_expr: $ => seq(
      'match',
      field('scrutinee', $.expr),
      '{',
      repeat($.match_arm),
      '}'
    ),

    match_arm: $ => seq(
      field('pattern', $.pattern),
      '=>',
      field('body', $.expr),
      ','
    ),

    let_expr: $ => seq(
      'let',
      field('pattern', $.pattern),
      '=',
      field('value', $.expr),
      ';',
      field('body', $.expr)
    ),

    block: $ => seq('{', repeat($.stmt), optional($.expr), '}'),

    tuple_expr: $ => seq('(', sep2($.expr, ','), ')'),

    record_expr: $ => seq(
      '{',
      sep1($.record_field, ','),
      optional(seq('..', $.expr)),
      '}'
    ),

    record_field: $ => seq($.identifier, ':', $.expr),

    field_expr: $ => prec(20, seq($.expr, '.', $.identifier)),

    array_expr: $ => seq('[', optional(sep1($.expr, ',')), ']'),

    index_expr: $ => prec(20, seq($.expr, '[', $.expr, ']')),

    lambda_expr: $ => seq(
      '|',
      optional(sep1($.param, ',')),
      '|',
      optional(seq('->', $.type_expr)),
      $.expr
    ),

    handle_expr: $ => seq(
      'handle',
      '{',
      $.expr,
      '}',
      'with',
      '{',
      repeat($.handler),
      '}'
    ),

    handler: $ => seq(
      field('pattern', $.pattern),
      '=>',
      field('body', $.expr),
      ','
    ),

    perform_expr: $ => seq('perform', $.identifier, $.arg_list),

    resume_expr: $ => seq('resume', $.expr),

    annotated_expr: $ => seq('(', $.expr, ':', $.type_expr, ')'),

    move_expr: $ => seq('move', $.expr),

    // Statements
    stmt: $ => choice(
      $.expr_stmt,
      $.let_stmt,
      $.assign_stmt,
      $.while_stmt,
      $.for_stmt,
      $.return_stmt,
      $.break_stmt,
      $.continue_stmt
    ),

    expr_stmt: $ => seq($.expr, ';'),

    let_stmt: $ => seq(
      'let',
      field('pattern', $.pattern),
      optional(seq(':', $.type_expr)),
      '=',
      field('value', $.expr),
      ';'
    ),

    assign_stmt: $ => seq($.expr, '=', $.expr, ';'),

    while_stmt: $ => seq('while', $.expr, $.block),

    for_stmt: $ => seq('for', $.pattern, 'in', $.expr, $.block),

    return_stmt: $ => seq('return', optional($.expr), ';'),

    break_stmt: $ => seq('break', ';'),

    continue_stmt: $ => seq('continue', ';'),

    // Type expressions
    type_expr: $ => choice(
      $.type_identifier,
      $.identifier,
      $.type_app,
      $.arrow_type,
      $.tuple_type,
      $.record_type,
      $.forall_type,
      $.exists_type
    ),

    type_app: $ => seq(
      $.type_expr,
      '<',
      sep1($.type_expr, ','),
      '>'
    ),

    arrow_type: $ => prec.right(seq(
      $.type_expr,
      '->',
      $.type_expr,
      optional(seq('/', $.effect_expr))
    )),

    tuple_type: $ => seq('(', sep2($.type_expr, ','), ')'),

    record_type: $ => seq(
      '{',
      sep1($.record_type_field, ','),
      '}'
    ),

    record_type_field: $ => seq($.identifier, ':', $.type_expr),

    forall_type: $ => seq('forall', repeat1($.identifier), '.', $.type_expr),

    exists_type: $ => seq('exists', repeat1($.identifier), '.', $.type_expr),

    // Effect expressions
    effect_expr: $ => choice(
      $.type_identifier,
      $.identifier,
      seq($.effect_expr, '|', $.effect_expr)
    ),
  }
});

// Helper functions
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

function sep2(rule, separator) {
  return seq(rule, separator, rule, repeat(seq(separator, rule)));
}
