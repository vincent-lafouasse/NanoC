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
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
