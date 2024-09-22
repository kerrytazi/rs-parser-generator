extern crate rs_parser_generator;
use rs_parser_generator::rules;

rules!(
	@mod parser;
	@debug;
	letter_lower: "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z";
	letter_upper: "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z";
	letter: letter_lower | letter_upper;
	word: letter+;
	punct: "." | "!" | "?";
	sentence: word (" " word)* punct;
);

fn main() {
	println!("{:#?}", parser::parse_sentence("Hello world!"));
}

/*
Result:

Some(
    ParseResult {
        tok: Token {
            ttype: Sentence,
            tree: [
                Token {
                    ttype: Word,
                    tree: [
                        Token {
                            ttype: Letter,
                            tree: [
                                Token {
                                    ttype: LetterUpper,
                                    tree: [
                                        Token {
                                            ttype: _Literal,
                                            tree: [],
                                            source: "H",
                                        },
                                    ],
                                    source: "H",
                                },
                            ],
                            source: "H",
                        },
                        Token {
                            ttype: Letter,
                            tree: [
                                Token {
                                    ttype: LetterLower,
                                    tree: [
                                        Token {
                                            ttype: _Literal,
                                            tree: [],
                                            source: "e",
                                        },
                                    ],
                                    source: "e",
                                },
                            ],
                            source: "e",
                        },
                        Token {
                            ttype: Letter,
                            tree: [
                                Token {
                                    ttype: LetterLower,
                                    tree: [
                                        Token {
                                            ttype: _Literal,
                                            tree: [],
                                            source: "l",
                                        },
                                    ],
                                    source: "l",
                                },
                            ],
                            source: "l",
                        },
                        Token {
                            ttype: Letter,
                            tree: [
                                Token {
                                    ttype: LetterLower,
                                    tree: [
                                        Token {
                                            ttype: _Literal,
                                            tree: [],
                                            source: "l",
                                        },
                                    ],
                                    source: "l",
                                },
                            ],
                            source: "l",
                        },
                        Token {
                            ttype: Letter,
                            tree: [
                                Token {
                                    ttype: LetterLower,
                                    tree: [
                                        Token {
                                            ttype: _Literal,
                                            tree: [],
                                            source: "o",
                                        },
                                    ],
                                    source: "o",
                                },
                            ],
                            source: "o",
                        },
                    ],
                    source: "Hello",
                },
                Token {
                    ttype: SentenceG0,
                    tree: [
                        Token {
                            ttype: _Literal,
                            tree: [],
                            source: " ",
                        },
                        Token {
                            ttype: Word,
                            tree: [
                                Token {
                                    ttype: Letter,
                                    tree: [
                                        Token {
                                            ttype: LetterLower,
                                            tree: [
                                                Token {
                                                    ttype: _Literal,
                                                    tree: [],
                                                    source: "w",
                                                },
                                            ],
                                            source: "w",
                                        },
                                    ],
                                    source: "w",
                                },
                                Token {
                                    ttype: Letter,
                                    tree: [
                                        Token {
                                            ttype: LetterLower,
                                            tree: [
                                                Token {
                                                    ttype: _Literal,
                                                    tree: [],
                                                    source: "o",
                                                },
                                            ],
                                            source: "o",
                                        },
                                    ],
                                    source: "o",
                                },
                                Token {
                                    ttype: Letter,
                                    tree: [
                                        Token {
                                            ttype: LetterLower,
                                            tree: [
                                                Token {
                                                    ttype: _Literal,
                                                    tree: [],
                                                    source: "r",
                                                },
                                            ],
                                            source: "r",
                                        },
                                    ],
                                    source: "r",
                                },
                                Token {
                                    ttype: Letter,
                                    tree: [
                                        Token {
                                            ttype: LetterLower,
                                            tree: [
                                                Token {
                                                    ttype: _Literal,
                                                    tree: [],
                                                    source: "l",
                                                },
                                            ],
                                            source: "l",
                                        },
                                    ],
                                    source: "l",
                                },
                                Token {
                                    ttype: Letter,
                                    tree: [
                                        Token {
                                            ttype: LetterLower,
                                            tree: [
                                                Token {
                                                    ttype: _Literal,
                                                    tree: [],
                                                    source: "d",
                                                },
                                            ],
                                            source: "d",
                                        },
                                    ],
                                    source: "d",
                                },
                            ],
                            source: "world",
                        },
                    ],
                    source: " world",
                },
                Token {
                    ttype: Punct,
                    tree: [
                        Token {
                            ttype: _Literal,
                            tree: [],
                            source: "!",
                        },
                    ],
                    source: "!",
                },
            ],
            source: "Hello world!",
        },
        rest: "",
    },
)

*/
