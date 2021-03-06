# Scheme Practice

------

** Extra comments: Project contains the following **
 
- File **fns.rkt** with scheme functions for expression evaluation using Scheme.

- Recursive-descent parser implemented in Scheme.

- JAVA Solution provided for the project 1 was used for the implementation. The referenced Java grammar functions are

--------

```
/** Return standard syntax regexp corresponding to ugly-regexp read from _scanner.*/
  private String uglyRegexp() {
    String t = term();
    return regexpRest(t);
  }

  private String regexpRest(String t) {
    if (check(Token.Kind.CHAR, ".")) {
      match(Token.Kind.CHAR, ".");
      String t1 = term();
      return regexpRest("(" + t + t1 + ")");
    }
    else {
      return t;
    }
  }

  private String term() {
    String f = factor();
    return termRest(f);
  }

  private String termRest(String f) {
    if (check(Token.Kind.CHAR, "+")) {
      match(Token.Kind.CHAR, "+");
      String f1 = factor();
      return termRest("(" + f + "|" + f1 + ")");
    }
    else {
      return f;
    }
  }

  private String factor() {
    if (check(Token.Kind.CHAR, "(")) {
      match(Token.Kind.CHAR, "(");
      String e = uglyRegexp();
      match(Token.Kind.CHAR, ")");
      return "(" + e + ")";
    }
    else if (check(Token.Kind.CHAR, "*")) {
      match(Token.Kind.CHAR, "*");
      String f = factor();
      return f + "*";
    }
    else {
      match(Token.Kind.CHARS);
      match(Token.Kind.CHAR, "(");
      String s = _lookahead.lexeme;
      match(Token.Kind.CHAR);
      String chars = chars(quote(s));
      match(Token.Kind.CHAR, ")");
      return "[" + chars + "]";
    }
  }

  private String chars(String s) {
    if (check(Token.Kind.CHAR, ",")) {
      match(Token.Kind.CHAR, ",");
      String s1 = _lookahead.lexeme;
      match(Token.Kind.CHAR);
      return chars(s + quote(s1));
    }
    else {
      return s;
    }
  }
```

			
