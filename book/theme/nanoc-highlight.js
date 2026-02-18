(function() {
  'use strict';

  function nanoc(hljs) {
    const KEYWORDS = {
      keyword: 'var const return if else while syscall undefined zeroed goto fn struct inline u8 i32 u32 ptr',
      literal: 'true false'
    };

    const NUMBER = {
      className: 'number',
      variants: [
        { begin: '\\b0b[01]+u?\\b' },           // Binary: 0b1010
        { begin: '\\b0x[0-9a-fA-F]+u?\\b' },    // Hex: 0xFF
        { begin: '\\b\\d+u?\\b' }               // Decimal: 42
      ]
    };

    const ESCAPE = {
      className: 'char-escape',
      begin: /\\[nrt0\\'"]/,
      relevance: 0
    };

    const HEX_ESCAPE = {
      className: 'char-escape',
      begin: /\\x[0-9a-fA-F]{2}/,
      relevance: 0
    };

    const STRING = {
      className: 'string',
      variants: [
        {
          begin: '"',
          end: '"',
          illegal: '\\n',
          contains: [ESCAPE, HEX_ESCAPE]
        },
        {
          begin: "'",
          end: "'",
          illegal: '\\n',
          contains: [ESCAPE, HEX_ESCAPE]
        }
      ]
    };

    const FUNCTION_CALL = {
      className: 'title function',
      begin: /\b[a-zA-Z_][a-zA-Z0-9_]*(?=\s*\()/,
      relevance: 10
    };

    // `label:` at the start of a line.
    // Can't use ^/m: hljs v9 strips flags when compiling patterns.
    // Use (?<=\n) lookbehind instead — matches after a newline.
    const LABEL = {
      className: 'symbol',
      // Include the \n in the match rather than using a lookbehind — hljs v9
      // may drop lookbehind assertions when building its combined regex.
      // A newline inside a color span is invisible, so this is safe.
      begin: /\n[a-zA-Z_][a-zA-Z0-9_]*\s*:/,
      relevance: 10
    };

    // `: Type` — type annotation after a colon.
    // [ \t]* (not \s*) in the lookbehind: prevents matching across newlines,
    // which would wrongly color the first identifier on a line after `label:\n`.
    // Negative lookahead excludes primitive keywords so they fall through to
    // keyword matching and get mauve instead of type yellow.
    const TYPE_ANNOTATION = {
      className: 'type',
      begin: /(?<=:[ \t]*)(?!u8\b|i32\b|u32\b|ptr\b)[a-zA-Z_][a-zA-Z0-9_]*/,
      relevance: 0
    };

    // `struct Name { ... }` — color the name after `struct` as a type.
    // Uses beginKeywords (a first-class hljs v9 feature) instead of a
    // variable-length lookbehind, which hljs may mishandle in its combined regex.
    const STRUCT_DEF = {
      beginKeywords: 'struct',
      end: /\{/,
      contains: [{
        className: 'type',
        begin: /[a-zA-Z_][a-zA-Z0-9_]*/
      }]
    };

    return {
      name: 'NanoC',
      aliases: ['nanoc', 'nc'],
      keywords: KEYWORDS,
      contains: [
        hljs.C_LINE_COMMENT_MODE,
        hljs.C_BLOCK_COMMENT_MODE,
        STRING,
        NUMBER,
        STRUCT_DEF,
        LABEL,
        TYPE_ANNOTATION,
        FUNCTION_CALL,
        {
          className: 'operator',
          begin: /[+\-*\/%&|^~!<>=]+|->|&&|\|\||<<|>>|<=|>=|==|!=/
        },
        {
          className: 'punctuation',
          begin: /[(){}\[\];,]/
        }
      ]
    };
  }

  // Register the language
  if (typeof hljs !== 'undefined') {
    hljs.registerLanguage('nanoc', nanoc);
    console.log('NanoC language registered. Available languages:', hljs.listLanguages ? hljs.listLanguages() : 'listLanguages not available');

    // Re-highlight NanoC code blocks (force re-highlight even if already processed)
    function highlightNanoC() {
      document.querySelectorAll('code.language-nanoc, code.language-nc').forEach(function(block) {
        // Get original source code
        var code = block.textContent;

        try {
          // Manually highlight using the nanoc language
          // Try both old (v9) and new (v10+) API
          var result;
          if (hljs.highlight.length >= 2) {
            // Old API: hljs.highlight(lang, code)
            result = hljs.highlight('nanoc', code, true);
          } else {
            // New API: hljs.highlight(code, {language: lang})
            result = hljs.highlight(code, {language: 'nanoc'});
          }

          // Replace the block's content with highlighted HTML
          block.innerHTML = result.value;

          // Add hljs class if not present
          if (!block.classList.contains('hljs')) {
            block.classList.add('hljs');
          }

          console.log('Successfully highlighted NanoC code block');
        } catch (e) {
          console.error('Failed to highlight NanoC code:', e);
        }
      });
    }

    // Run immediately if DOM is ready, otherwise wait
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', highlightNanoC);
    } else {
      highlightNanoC();
    }

    // Also try again after a small delay to catch late-loading elements
    setTimeout(highlightNanoC, 100);
  }
})();
