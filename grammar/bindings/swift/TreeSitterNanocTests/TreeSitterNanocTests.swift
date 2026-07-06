import XCTest
import SwiftTreeSitter
import TreeSitterNanoc

final class TreeSitterNanocTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_nanoc())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading NanoC grammar")
    }
}
