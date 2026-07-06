/**
 * @file a simple systems PL targeting RISC-V and xv6
 * @author Vincent Lafouasse
 * @license WTFPL
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: "nanoc",

  rules: {
    source_file: $ => repeat($.top_level_item),

    top_level_item: $ => choice(
      $.var_def,
      $.fn_def,
      $.struct_def,
    ),

    var_def: $ => seq(
      "var",
      $.identifier,
      ":",
      $.type,
      "=",
      $.var_initializer,
      ";"
    ),

    var_initializer: $ => choice($.expression, "undefined", "zeroed"),

    struct_def: $ => seq(
      "struct",
      $.identifier,
      "{",
      repeat(seq($.identifier, ":", $.type, ",")),
      "}",
      ";",
    ),

    fn_def: $ => seq("fn", $.todo),

    // TODO: what even is a statement?
    statement: $ => "TODO: STATEMENTS",

    // TODO: what even is a type?
    type: $ => "TODO: TYPES",

    // TODO: what even is an expression?
    expression: $ => "TODO: EXPRESSIONS",

    // TODO: consider escape sequences
    string_literal: $ => token(seq(
      '"',
      repeat(choice(/[^"\\]/, /\\./)),
      '"',
    )),
    char_literal: $ => token(seq(
      "'",
      choice(/[^'\\]/, /\\./),
      "'",
    )),

    int_literal: $ => token(/\d+/),
    bool_literal: $ => choice('true', 'false'),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    todo: $ => "todo",
  }
});
