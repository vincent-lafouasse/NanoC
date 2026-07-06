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

    todo: $ => "todo",
  }
});
