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

    var_def: $ => seq("var", $.todo),
    fn_def: $ => seq("fn", $.todo),
    struct_def: $ => seq("struct", $.todo),

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
