const multiplicative_operators = ["*", "/", "%", "<<", ">>", ">>>", "&", "^"],
  additive_operators = ["+", "-", "|"],
  comparison_operators = [">", "<", "<=", ">=", "==", "!="],
  assignment_operators = multiplicative_operators
    .concat(additive_operators)
    .map((operator) => operator + "=")
    .concat("="),
  builtin_types = [
    "type_expr",
    "rawptr",
    "str",
    "cstr",
    "i8",
    "i16",
    "i32",
    "i64",
    "u8",
    "u16",
    "u32",
    "u64",
    "f32",
    "f64",
    "bool",
    "void",
  ],
  op_prec = {
    control_flow: 11,
    r_unary: 10,
    l_unary: 9,
    multiplicative: 8,
    membership: 7,
    additive: 6,
    comparison: 5,
    range: 4,
    and: 3,
    or: 2,
    ternary: 1,
  },
  unicodeLetter = /\p{L}/,
  unicodeDigit = /[0-9]/,
  unicodeChar = /./,
  unicodeValue = unicodeChar,
  letter = choice(unicodeLetter, "_"),
  newline = "\n",
  terminator = choice(newline, ";"),
  partial_terminator = choice(newline, ","),
  hexDigit = /[0-9a-fA-F]/,
  octalDigit = /[0-7]/,
  decimalDigit = /[0-9]/,
  binaryDigit = /[01]/,
  hexDigits = seq(hexDigit, repeat(seq(optional("_"), hexDigit))),
  octalDigits = seq(octalDigit, repeat(seq(optional("_"), octalDigit))),
  decimalDigits = seq(decimalDigit, repeat(seq(optional("_"), decimalDigit))),
  binaryDigits = seq(binaryDigit, repeat(seq(optional("_"), binaryDigit))),
  hexLiteral = seq("0", choice("x", "X"), optional("_"), hexDigits),
  octalLiteral = seq("0", choice("o", "O"), optional("_"), octalDigits),
  decimalLiteral = choice(
    "0",
    seq(/[1-9]/, optional(seq(optional("_"), decimalDigits))),
  ),
  binaryLiteral = seq("0", choice("b", "B"), optional("_"), binaryDigits),
  intLiteral = choice(binaryLiteral, decimalLiteral, octalLiteral, hexLiteral),
  decimalExponent = seq(
    choice("e", "E"),
    optional(choice("+", "-")),
    decimalDigits,
  ),
  decimalFloatLiteral = seq(
    choice(
      seq(
        decimalDigits,
        ".",
        optional(decimalDigits),
        optional(decimalExponent),
      ),
      seq(decimalDigits, decimalExponent),
      seq(".", decimalDigits, optional(decimalExponent)),
    ),
    optional("f"),
  ),
  // hexExponent = seq(
  //   choice("p", "P"),
  //   optional(choice("+", "-")),
  //   decimalDigits,
  // ),
  // hexMantissa = choice(
  //   seq(optional("_"), hexDigits, ".", optional(hexDigits)),
  //   seq(optional("_"), hexDigits),
  //   seq(".", hexDigits),
  // ),
  // hexFloatLiteral = seq("0", choice("x", "X"), hexMantissa, hexExponent),
  floatLiteral = decimalFloatLiteral; // choice(decimalFloatLiteral, hexFloatLiteral),

const list = (sep, rule) => optional(seq(rule, repeat(seq(sep, rule))));
const list1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)));

module.exports = grammar({
  name: "onyx",
  extras: ($) => [$.comment, /\s/],
  inline: ($) => [$._type, $.package_clause, $.string_literal],
  word: ($) => $.identifier,
  conflicts: ($) => [[$.parameter, $.quick_function_definition]],

  rules: {
    source_file: ($) =>
      seq(optional($.package_clause), list(terminator, optional($._top))),

    package_clause: ($) =>
      seq(
        optional(field("docs", $.doc_comment)),
        alias("package", $.keyword),
        field("name", $._identifier_list),
      ),

    _top: ($) =>
      choice($.use_stmt, $._top_level_directive, $._top_level_declaration),

    // Top-level statements
    use_stmt: ($) =>
      seq(
        alias("use", $.keyword),
        $._identifier_list,
        optional($._use_stmt_body),
      ),

    _use_stmt_body: ($) =>
      seq("{", repeat(choice(partial_terminator, $.identifier)), "}"),

    _top_level_directive: ($) =>
      choice(
        $.load_directive,
        $.error_directive,
        $.export_directive,
        $.init_directive,
        $.js_directive,
      ),

    load_directive: ($) =>
      seq(
        "#",
        choice(
          "load",
          "load_all",
          "load_all_recursive",
          "load_path",
          "library_path",
        ),
        $._expression,
      ),

    error_directive: ($) => seq("#", "error", $._expression),
    export_directive: ($) =>
      seq(
        "#",
        "export",
        field("name", $._expression),
        field("value", $._expression),
      ),
    init_directive: ($) => seq("#", "init", $._expression),
    js_directive: ($) =>
      seq(
        "#",
        "js",
        optional(seq("#", "order", $._expression)),
        choice(
          optional(seq("#", "file", field("file", $._expression))),
          field("code", $._expression),
        ),
      ),

    _top_level_declaration: ($) =>
      choice($.global_declaration, $.binding_declaration),

    global_declaration: ($) =>
      seq(
        repeat($.tag),
        optional(seq("#", "thread_local")),
        prec.right(
          1,
          seq(
            field("name", alias($.identifier, $.const_identifier)),
            alias(":", $.operator),
            choice(
              field("type", $._type),
              seq(
                optional(field("type", $._type)),
                alias("=", $.operator),
                field("value", $._expression),
              ),
            ),
          ),
        ),
      ),

    binding_declaration: ($) =>
      seq(
        repeat($.tag),
        prec.right(
          1,
          seq(
            field("name", alias($.identifier, $.const_identifier)),
            alias("::", $.operator),
            field("value", $._expression),
          ),
        ),
      ),

    _type: ($) => choice(alias($._proc_type, $.proc_type), $._non_proc_type),

    _proc_type: ($) => "PROC_TYPE",

    _non_proc_type: ($) =>
      prec.right(
        choice(
          $._selector_expression,
          alias(choice(...builtin_types), $.type_identifier),
          alias(seq("&", $._type), $.pointer_type),
          alias(seq("[]", $._type), $.slice_type),
          alias(seq("[&]", $._type), $.multi_pointer_type),
          alias(seq("[..]", $._type), $.dynamic_array_type),
          alias(seq("?", $._type), $.optional_type),
          alias(
            seq("[", field("count", $._expression), "]", $._type),
            $.array_type,
          ),
          seq(
            "$",
            alias($.identifier, $.polymorphic_variable),
            optional(seq("/", $._expression)),
          ),
          alias(seq("typeof", field("expr", $._expression)), $.typeof_type),
          alias("#Self", $.self_type),
          $.struct_type,
          $.union_type,
          $.enum_type,
        ),
      ),

    struct_type: ($) =>
      seq(
        alias("struct", $.keyword),
        optional(field("parameters", $.parameter_list)),
        repeat(alias($._struct_directives, $.compiler_directive)),
        optional("\n"),
        "{",
        optional("\n"),
        list(
          terminator,
          choice($.binding_declaration, $.struct_field_declaration),
        ),
        optional(terminator),
        "}",
      ),

    _struct_directives: ($) =>
      choice(
        "#pack",
        "#union",
        seq("#size", $._expression),
        seq("#align", $._expression),
      ),

    struct_field_declaration: ($) =>
      seq(
        optional("use"),
        list1(",", field("name", $.identifier)),
        alias(":", $.operator),
        choice(
          field("type", $._type),
          seq(
            optional(field("type", $._type)),
            alias("=", $.operator),
            field("value", $._expression),
          ),
        ),
      ),

    union_type: ($) =>
      seq(
        alias("union", $.keyword),
        optional(field("parameters", $.parameter_list)),
        optional("\n"),
        "{",
        optional("\n"),
        list(
          terminator,
          choice($.binding_declaration, $.union_field_declaration),
        ),
        optional(terminator),
        "}",
      ),

    union_field_declaration: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq("as", field("encoding", $.int_literal))),
        alias(":", $.operator),
        field("type", $._type),
      ),

    enum_type: ($) =>
      seq(
        alias("enum", $.keyword),
        repeat(alias($._enum_directives, $.compiler_directive)),
        optional(seq("(", field("backing_type", $.identifier), ")")),
        optional("\n"),
        "{",
        optional("\n"),
        list(terminator, $.enum_value_declaration),
        optional(terminator),
        "}",
      ),

    _enum_directives: ($) => choice("#flag"),

    enum_value_declaration: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq("::", field("value", $._expression))),
      ),

    parameter_list: ($) =>
      seq("(", list(",", field("parameter", $.parameter)), ")"),

    parameter: ($) =>
      seq(
        field("is_used", optional("use")),
        optional(field("baked", "$")),
        list1(",", field("name", $.identifier)),
        ":",
        choice(
          field("type", $._type),
          seq(
            optional(field("type", $._type)),
            alias("=", $.operator),
            field("value", $._expression),
          ),
        ),
      ),

    function_header: ($) =>
      prec.left(
        seq(
          $.parameter_list,
          optional(seq("->", alias($._type, $.return_type))),
        ),
      ),

    function_definition: ($) =>
      prec(
        2,
        seq(
          $.function_header,
          optional("\n"),
          alias($.imperative_scope, $.function_body),
        ),
      ),

    quick_function_definition: ($) =>
      prec(
        1,
        choice(
          seq(
            field("parameter", $.identifier),
            "=>",
            optional("\n"),
            alias($.imperative_scope, $.function_body),
          ),
          seq(
            "(",
            list(",", field("parameter", $.identifier)),
            ")",
            "=>",
            optional("\n"),
            alias($.imperative_scope, $.function_body),
          ),
        ),
      ),

    imperative_scope: ($) =>
      choice(
        seq(
          "{",
          optional("\n"),
          list(terminator, $._statement),
          optional(terminator),
          "}",
        ),
        seq("do", $._statement),
        "---",
      ),

    _statement: ($) => choice($._expression, $.imperative_scope, $.declaration),

    declaration: ($) =>
      prec.left(
        seq(
          list1(",", $.identifier),
          ":",
          choice(
            field("type", $._type),
            seq(
              optional(field("type", $._type)),
              alias("=", $.operator),
              field("value", $._expression),
            ),
          ),
        ),
      ),

    _expression: ($) =>
      prec.right(
        1,
        choice(
          $.string_literal,
          $.bool_literal,
          $.int_literal,
          $.float_literal,
          $.identifier,
          $._type,
          $.binary_expression,
          $.quick_function_definition,
          $.function_definition,
          seq("(", $._expression, ")"),
        ),
      ),

    _selector_expression: ($) => list1(".", $.identifier),

    binary_expression: ($) => {
      const table = [
        [op_prec.multiplicative, choice(...multiplicative_operators)],
        [op_prec.additive, choice(...additive_operators)],
        [op_prec.comparison, choice(...comparison_operators)],
        [op_prec.range, choice("..", "..=")],
        [op_prec.and, "&&"],
        [op_prec.or, "||"],
      ];
      return choice(
        ...table.map(([p, o]) =>
          prec.left(
            p,
            seq(
              field("left", $._expression),
              field("operator", alias(o, $.operator)),
              field("right", $._expression),
            ),
          ),
        ),
      );
    },

    // Useful helpers
    identifier: ($) => token(seq(letter, repeat(choice(letter, unicodeDigit)))),
    _identifier_list: ($) => seq($.identifier, repeat(seq(".", $.identifier))),

    tag: ($) =>
      seq(choice("@", seq("#", "tag")), $._expression, optional("\n")),

    // Literals
    _true: ($) => "true",
    _false: ($) => "false",
    bool_literal: ($) => choice($._true, $._false),
    float_literal: ($) => token(floatLiteral),
    int_literal: ($) => token(intLiteral),

    string_literal: ($) =>
      seq(
        '"',
        repeat(
          choice(token.immediate(prec(1, /[^"\n\\]+/)), $.escape_sequence),
        ),
        '"',
      ),

    escape_sequence: ($) =>
      token.immediate(
        seq(
          "\\",
          choice(
            /[^xuU]/,
            /\d{2,3}/,
            /x[0-9a-fA-F]{2}/,
            /u[0-9a-fA-F]{4}/,
            /U[0-9a-fA-F]{8}/,
          ),
        ),
      ),

    doc_comment: ($) => seq("///", /.*/),
    comment: ($) =>
      token(
        choice(
          seq(/\/\/[^/]/, /.*/),
          seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/"),
        ),
      ),
  },
});
