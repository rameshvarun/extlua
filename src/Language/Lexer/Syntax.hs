module Language.Lexer.Syntax where

  data SourceLocation = SourceLocation {
    start  :: Int,
    length :: Int
  } deriving (Show, Eq)

  data TokenData =
    -- Default Lua Keywords
    Do | End | While | Repeat | Until | If | Then | Else | For | In | Function |
    Local | Return | Break | And | Or | Not | Nil | True | False |

    -- Default lua tokens with data attached
    Name String |
    NumberLiteral String |
    StringLiteral String |

    -- Lambda extension
    Arrow |

    -- Special Tokens
    Skip | EOF |

    -- Asignment operator extension
    PlusEquals | MinusEquals | ConcatEquals | MultEquals | DivEquals |
    ModEquals | ExpEquals |

    -- Self sugar syntax
    At |

    -- Function bind syntax
    DoubleColon |

    -- Module extension
    Export | Import | From |

    -- OOP Extension
    Class | Extends | Static |

    -- Doc-comment extension
    DocComment

    deriving (Show, Eq)

  data Token = Token TokenData SourceLocation
    deriving (Show, Eq)

  getTokenSourceLocation :: Token -> SourceLocation
  getTokenSourceLocation (Token _ loc) = loc

  getTokenLength :: Token -> Int
  getTokenLength (Token _ loc) = Language.Lexer.Syntax.length loc
