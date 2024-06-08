const special_operators = ["|>", "??"],
  multiplicative_operators = ["*", "/", "%", "<<", ">>", ">>>", "&", "^"],
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
  extras: ($) => [$.comment, /[ \t]/],
  inline: ($) => [$._type],
  word: ($) => $.identifier,
  conflicts: ($) => [
    [$.parameter, $.quick_function_definition],
    [$.source_file],
  ],

  rules: {
    source_file: ($) =>
      seq(
        optional(seq(repeat("\n"), $.package_clause)),
        list(terminator, optional($._top)),
      ),

    package_clause: ($) =>
      seq(
        optional(field("docs", $.doc_comment)),
        alias("package", $.keyword),
        field("name", $.identifier_list),
      ),

    _top: ($) =>
      choice($.use_statement, $._top_level_directive, $._top_level_declaration),

    // Top-level statements
    use_statement: ($) =>
      prec.right(
        seq(
          alias("use", $.keyword),
          field("package_name", $.identifier_list),
          optional($._use_stmt_body),
        ),
      ),

    identifier_list: ($) => $._identifier_list,

    _use_stmt_body: ($) =>
      seq(
        "{",
        choice(
          repeat(
            choice(
              partial_terminator,
              seq(
                $._use_stmt_body_term,
                optional(seq(alias("::", $.operator), $._use_stmt_body_term)),
              ),
            ),
          ),
          alias("*", $.wildcard),
        ),
        "}",
      ),

    _use_stmt_body_term: ($) =>
      choice($.identifier, alias("package", $.keyword)),

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
      choice(
        $.global_declaration,
        alias($._top_level_binding_declaration, $.binding_declaration),
      ),

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

    _top_level_binding_declaration: ($) =>
      seq(
        repeat($.tag),
        prec.right(
          1,
          seq(
            field("name", alias($._non_proc_type, $.const_identifier)),
            alias("::", $.operator),
            field("value", $._expression),
          ),
        ),
      ),

    binding_declaration: ($) =>
      seq(
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

    _proc_type: ($) =>
      seq(
        "(",
        list(
          partial_terminator,
          seq(
            optional(seq(field("parameter_name", $.identifier), ":")),
            field("parameter_type", $._type),
          ),
        ),
        ")",
        "->",
        field("return_type", $._type),
      ),

    _type_in_expression: ($) =>
      prec.right(
        1,
        choice(
          seq(alias("#type", $.compiler_directive), $._type),
          alias(choice(...builtin_types), $.type_identifier),
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
            optional(seq("/", $._factor)),
          ),
          alias(seq("typeof", field("expr", $._expression)), $.typeof_type),
          alias("#Self", $.compiler_directive),
          $.struct_type,
          $.union_type,
          $.enum_type,
        ),
      ),

    _non_proc_type: ($) =>
      prec.right(
        1,
        choice(
          seq(
            $.identifier,
            prec.right(
              12,
              repeat(
                choice(
                  alias($.argument_list, $.poly_call_type),
                  alias(seq(".", $.identifier), $.selector),
                ),
              ),
            ),
          ),
          $._type_in_expression,
          alias(seq("&", $._type), $.pointer_type),
        ),
      ),

    struct_type: ($) =>
      seq(
        alias("struct", $.keyword),
        optional(field("parameters", $.parameter_list)),
        repeat(alias($._struct_directives, $.compiler_directive)),
        optional("\n"),
        "{",
        list(
          terminator,
          optional(choice($.binding_declaration, $.struct_field_declaration)),
        ),
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
        optional(alias("use", $.keyword)),
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
        list(
          terminator,
          optional(choice($.binding_declaration, $.union_field_declaration)),
        ),
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
        list(terminator, optional($.enum_value_declaration)),
        "}",
      ),

    _enum_directives: ($) => choice("#flags"),

    enum_value_declaration: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq("::", field("value", $._expression))),
      ),

    parameter_list: ($) =>
      prec.left(seq("(", list(",", field("parameter", $.parameter)), ")")),

    parameter: ($) =>
      prec.left(
        seq(
          field("is_used", optional(alias("use", $.keyword))),
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
        seq($.function_header, optional("\n"), alias($.block, $.function_body)),
      ),

    quick_function_definition: ($) =>
      prec(
        1,
        choice(
          seq(
            field("parameter", $.identifier),
            "=>",
            optional("\n"),
            alias($.block, $.function_body),
          ),
          seq(
            "(",
            list(",", field("parameter", $.identifier)),
            ")",
            "=>",
            optional("\n"),
            alias($.block, $.function_body),
          ),
        ),
      ),

    _curly_block: ($) =>
      seq("{", list(terminator, optional($._statement)), "}"),

    block: ($) =>
      choice(alias($._curly_block, $.block), seq("do", $._statement), "---"),

    _statement: ($) =>
      choice(
        $.block,
        $.declaration,
        $.if_statement,
        $.while_statement,
        $.break_statement,
        $.continue_statement,
        $.fallthrough_statement,
        $.return_statement,
        $.defer_statement,
        $.use_statement,
        $.binding_declaration,
        $._expression,
      ),

    declaration: ($) =>
      prec.left(
        1,
        seq(
          optional(alias("use", $.keyword)),
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

    return_statement: ($) =>
      prec.left(
        seq(
          repeat1(alias("return", $.keyword)),
          optional(alias("#from_proc", $.compiler_directive)),
          optional($._expression_list),
        ),
      ),

    break_statement: ($) => repeat1(alias("break", $.keyword)),
    continue_statement: ($) => repeat1(alias("continue", $.keyword)),
    fallthrough_statement: ($) => repeat1(alias("fallthrough", $.keyword)),

    if_statement: ($) =>
      prec.right(
        seq(
          alias("if", $.keyword),
          optional(seq(field("initializer", $._statement), ";")),
          field("condition", $._expression),
          optional("\n"),
          field("if_true", $.block),
          repeat(
            seq(
              optional("\n"),
              alias("elseif", $.keyword),
              field("condition", $._expression),
              optional("\n"),
              $.block,
            ),
          ),
          optional("\n"),
          optional(
            seq(
              alias("else", $.keyword),
              optional("\n"),
              field("if_false", $.block),
            ),
          ),
        ),
      ),

    while_statement: ($) =>
      prec.right(
        seq(
          alias("while", $.keyword),
          optional(seq(field("initializer", $._statement))),
          optional(alias("defer", $.keyword)),
          field("condition", $._expression),
          field("body", $.block),
        ),
      ),

    defer_statement: ($) =>
      prec.right(seq(alias("defer", $.keyword), $._statement)),

    _expression_list: ($) => prec.left(list1(",", $._expression)),

    _expression: ($) => prec.right(1, choice($._factor, $.binary_expression)),

    _factor: ($) =>
      prec.right(
        2,
        seq(
          repeat(
            prec.right(
              12,
              choice(
                ...["-", "!", "~", "~~", "*", "&"].map((x) =>
                  alias(x, $.operator),
                ),
              ),
            ),
          ),
          choice(
            $.string_literal,
            $.bool_literal,
            $.int_literal,
            $.float_literal,
            $.char_literal,
            $.identifier,
            $.quick_function_definition,
            $.function_definition,
            $.code_block,
            seq(alias("#type", $.compiler_directive), $._type),
            $._type_in_expression,
            $.sizeof_expression,
            $.alignof_expression,
            $.cast_expression,
            $._directive_expression,
            alias(seq("$", $.identifier), $.polymorphic_variable),
            seq("(", $._expression, ")"),
          ),
          prec.right(
            12,
            repeat(
              choice(
                alias("?", $.operator),
                alias(seq(".", $.identifier), $.selector),
                alias(
                  seq("->", field("name", $.identifier), $.argument_list),
                  $.method_call,
                ),
                alias(
                  seq(/\n\s*->/, field("name", $.identifier), $.argument_list),
                  $.method_call,
                ),
                alias(seq(".*"), $.operator),
                alias(
                  seq("[", field("subscript", $._expression), "]"),
                  $.subscript,
                ),
                alias($.argument_list, $.proc_call),
                alias(/!\{[^}]*\}/, $.proc_macro_body),
              ),
            ),
          ),
        ),
      ),

    _directive_expression: ($) =>
      prec.right(
        choice(
          alias("#file", $.compiler_directive),
          alias("#line", $.compiler_directive),
          alias("#column", $.compiler_directive),
          alias("#first", $.compiler_directive),
          alias("#this_package", $.compiler_directive),
          seq(alias("#export_name", $.compiler_directive), $._factor),
          seq(alias("#cstr", $.compiler_directive), $.string_literal),
          seq(alias("#file_contents", $.compiler_directive), $._expression),
          seq(alias("#defined", $.compiler_directive), "(", $._expression, ")"),
          seq(
            alias("#unquote", $.compiler_directive),
            field("code", $._expression),
            optional($.argument_list),
          ),
          seq(
            alias("#solidify", $.compiler_directive),
            field("base", $._factor),
            optional("\n"),
            "{",
            optional("\n"),
            list(partial_terminator, seq($.identifier, "=", $._expression)),
            optional("\n"),
            "}",
          ),
        ),
      ),

    sizeof_expression: ($) =>
      seq(alias("sizeof", $.keyword), field("type", $._type)),

    alignof_expression: ($) =>
      seq(alias("alignof", $.keyword), field("type", $._type)),

    cast_expression: ($) =>
      seq(
        alias("cast", $.keyword),
        "(",
        field("type", $._type),
        choice(
          seq(",", field("expr", $._expression), ")"),
          seq(")", field("expr", $._factor)),
        ),
      ),

    argument: ($) => seq(optional(seq($.identifier, "=")), $._expression),

    argument_list: ($) =>
      seq("(", list(partial_terminator, optional($.argument)), ")"),

    code_block: ($) =>
      prec.right(
        6,
        seq(
          choice(
            seq("[", list(partial_terminator, optional($.identifier)), "]"),
          ),
          optional("\n"),
          choice(
            seq(alias($._curly_block, $.block)),
            seq("(", optional("\n"), $._expression, ")"),
          ),
        ),
      ),

    binary_expression: ($) => {
      const table = [
        [op_prec.control_flow, choice(...special_operators)],
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
    _identifier_list: ($) =>
      prec.right(seq($.identifier, repeat(seq(".", $.identifier)))),

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
    char_literal: ($) =>
      seq(
        "'",
        choice(token.immediate(prec(1, /[^'\n\\]+/)), $.escape_sequence),
        "'",
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
