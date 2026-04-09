// Copyright (C) 2013 - 2026  Metrum Research Group
//
// This file is part of mrgsolve.
//
// mrgsolve is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// mrgsolve is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.

// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp14)]]
#include <Rcpp.h>

#include <boost/spirit/include/qi.hpp>
#include <boost/phoenix.hpp>
#include <boost/variant.hpp>
#include <boost/variant/recursive_wrapper.hpp>

#include <string>
#include <vector>
#include <sstream>
#include <stdexcept>

// ---------------------------------------------------------------------------
// 1.  AST
// ---------------------------------------------------------------------------

namespace ast {

struct Nil {};

// Numeric literal stored as its original source text so we never reformat it.
struct Number {
  std::string text;
  Number() {}
  explicit Number(std::string t) : text(std::move(t)) {}
  template <typename Iter>
  explicit Number(const boost::iterator_range<Iter>& rng)
    : text(rng.begin(), rng.end()) {}
};

struct UnaryExpr;
struct BinaryExpr;
struct FuncCall;

using Expr = boost::variant<
  Nil,
  Number,
  std::string,                              // identifier
  boost::recursive_wrapper<UnaryExpr>,
  boost::recursive_wrapper<BinaryExpr>,
  boost::recursive_wrapper<FuncCall>
>;

struct UnaryExpr {
  char op;
  Expr operand;
  UnaryExpr() : op(0) {}
  UnaryExpr(char op_, Expr operand_) : op(op_), operand(std::move(operand_)) {}
};

struct BinaryExpr {
  Expr lhs;
  std::string op;  // arithmetic: '+' '-' '*' '/' '^' (** -> ^)
                   // comparison: '==' '!=' '<' '<=' '>' '>='
                   // logical:    '&&' '||'
  Expr rhs;
  BinaryExpr() {}
  BinaryExpr(Expr lhs_, std::string op_, Expr rhs_)
    : lhs(std::move(lhs_)), op(std::move(op_)), rhs(std::move(rhs_)) {}
};

struct FuncCall {
  std::string        name;
  std::vector<Expr>  args;
  FuncCall() {}
  FuncCall(std::string name_, std::vector<Expr> args_)
    : name(std::move(name_)), args(std::move(args_)) {}
};

} // namespace ast

// ---------------------------------------------------------------------------
// 2.  Emitter  (AST -> C++ string)
// ---------------------------------------------------------------------------

static int precedence(const std::string& op) {
  if (op == "||") return 1;
  if (op == "&&") return 2;
  if (op == "==" || op == "!=" ||
      op == "<"  || op == "<=" ||
      op == ">"  || op == ">=") return 3;
  if (op == "+" || op == "-") return 4;
  if (op == "*" || op == "/") return 5;
  return 0;  // "^" / unknown — not used for parens decisions (pow is a function)
}

static const ast::BinaryExpr* as_binop(const ast::Expr& e) {
  return boost::get<ast::BinaryExpr>(&e);
}

static bool needs_parens_left(const ast::Expr& child, const std::string& parent_op) {
  const ast::BinaryExpr* b = as_binop(child);
  if (!b || b->op == "^") return false;
  return precedence(b->op) < precedence(parent_op);
}

static bool needs_parens_right(const ast::Expr& child, const std::string& parent_op) {
  const ast::BinaryExpr* b = as_binop(child);
  if (!b || b->op == "^") return false;
  int cp = precedence(b->op);
  int pp = precedence(parent_op);
  return cp < pp || (cp == pp && (parent_op == "-" || parent_op == "/"));
}

struct Emitter : boost::static_visitor<std::string> {

  std::string operator()(ast::Nil) const {
    return "";
  }

  std::string operator()(const ast::Number& n) const {
    return n.text;
  }

  std::string operator()(const std::string& id) const {
    return id;
  }

  std::string operator()(const ast::UnaryExpr& u) const {
    return std::string(1, u.op) + boost::apply_visitor(*this, u.operand);
  }

  std::string operator()(const ast::BinaryExpr& b) const {
    std::string l = boost::apply_visitor(*this, b.lhs);
    std::string r = boost::apply_visitor(*this, b.rhs);
    if (b.op == "^") {
      return "pow(" + l + ", " + r + ")";
    }
    if (needs_parens_left(b.lhs, b.op)) l = "(" + l + ")";
    if (needs_parens_right(b.rhs, b.op)) r = "(" + r + ")";
    return l + b.op + r;
  }

  std::string operator()(const ast::FuncCall& f) const {
    std::string out = f.name + "(";
    for (std::size_t i = 0; i < f.args.size(); ++i) {
      if (i) out += ", ";
      out += boost::apply_visitor(*this, f.args[i]);
    }
    out += ")";
    return out;
  }
};

inline std::string emit(const ast::Expr& e) {
  return boost::apply_visitor(Emitter{}, e);
}

// ---------------------------------------------------------------------------
// 3.  Grammar
// ---------------------------------------------------------------------------

namespace qi  = boost::spirit::qi;
namespace phx = boost::phoenix;
namespace asc = boost::spirit::ascii;

template <typename Iterator>
struct ExprGrammar
  : qi::grammar<Iterator, ast::Expr(), asc::space_type>
{
  ExprGrammar() : ExprGrammar::base_type(expr)
  {
    using qi::char_;
    using qi::alpha;
    using qi::alnum;
    using qi::lit;
    using qi::_val;
    using qi::_1;
    using qi::_2;
    using phx::construct;
    using phx::push_back;
    using phx::ref;
    using phx::val;

    // identifier: starts with alpha or '_', then alnum or '_'
    identifier %= qi::lexeme[qi::raw[
      (alpha | char_('_')) >> *(alnum | char_('_'))
    ]];

    // numeric literal: capture raw source text via qi::raw so we
    // preserve exactly what the user wrote (2, 0.75, 1.5e-3, etc.)
    number = qi::lexeme[qi::raw[qi::double_]][
      _val = construct<ast::Number>(_1)
    ];

    // argument list for function calls
    arglist = expr[push_back(_val, _1)]
              >> *( lit(',') >> expr[push_back(_val, _1)] );

    // function call: identifier followed immediately by '('
    funccall = (identifier >> lit('(') >> arglist >> lit(')'))[
      _val = construct<ast::FuncCall>(_1, _2)
    ];

    // primary: number | function call | identifier | parenthesised expr
    primary =
        number                           [_val = _1]
      | funccall                         [_val = _1]
      | identifier                       [_val = _1]
      | ( lit('(') >> expr >> lit(')') ) [_val = _1]
      ;

    // power: left-associative; base is primary so unary minus binds looser
    // Fortran '**' stored as '^' in the AST; exponent may be unary (a**-2)
    power =
      primary[_val = _1]
      >> *(
        lit("**") >> unary[
          _val = construct<ast::BinaryExpr>(_val, val(std::string("^")), _1)
        ]
      );

    // unary: optional leading +/-; wraps power so -a**2 == -(a**2)
    unary =
        ( char_('+') >> unary )[_val = construct<ast::UnaryExpr>(_1, _2)]
      | ( char_('-') >> unary )[_val = construct<ast::UnaryExpr>(_1, _2)]
      | power                  [_val = _1]
      ;

    // multiplicative: * and /
    mulop = qi::string("*") | qi::string("/");
    multiplicative =
      unary[_val = _1]
      >> *(
        (mulop >> unary)[
          _val = construct<ast::BinaryExpr>(_val, _1, _2)
        ]
      );

    // additive: + and -
    addop = qi::string("+") | qi::string("-");
    additive =
      multiplicative[_val = _1]
      >> *(
        (addop >> multiplicative)[
          _val = construct<ast::BinaryExpr>(_val, _1, _2)
        ]
      );

    // comparison: ==  !=  <=  >=  <  >
    // Multi-char tokens must appear before their single-char prefixes.
    cmpop = qi::string("==") | qi::string("!=")
          | qi::string("<=") | qi::string(">=")
          | qi::string("<")  | qi::string(">");
    comparison =
      additive[_val = _1]
      >> *(
        (cmpop >> additive)[
          _val = construct<ast::BinaryExpr>(_val, _1, _2)
        ]
      );

    // logical AND  (higher precedence than ||)
    logical_and =
      comparison[_val = _1]
      >> *(
        lit("&&") >> comparison[
          _val = construct<ast::BinaryExpr>(_val, val(std::string("&&")), _1)
        ]
      );

    // logical OR
    logical_or =
      logical_and[_val = _1]
      >> *(
        lit("||") >> logical_and[
          _val = construct<ast::BinaryExpr>(_val, val(std::string("||")), _1)
        ]
      );

    expr = logical_or;
  }

  qi::rule<Iterator, ast::Expr(),              asc::space_type> expr;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> logical_or;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> logical_and;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> comparison;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> additive;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> multiplicative;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> power;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> unary;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> primary;
  qi::rule<Iterator, ast::Number(),            asc::space_type> number;
  qi::rule<Iterator, ast::FuncCall(),          asc::space_type> funccall;
  qi::rule<Iterator, std::vector<ast::Expr>(), asc::space_type> arglist;
  qi::rule<Iterator, std::string(),            asc::space_type> identifier;
  qi::rule<Iterator, std::string(),            asc::space_type> addop;
  qi::rule<Iterator, std::string(),            asc::space_type> mulop;
  qi::rule<Iterator, std::string(),            asc::space_type> cmpop;
};

// ---------------------------------------------------------------------------
// Integer-division detector
// ---------------------------------------------------------------------------

// Returns true if the Number text represents a plain integer (digits only,
// no decimal point, no exponent suffix).
static bool is_integer_literal(const ast::Number& n) {
  const std::string& t = n.text;
  return !t.empty() &&
    t.find_first_not_of("0123456789") == std::string::npos;
}

struct IntDivInstance {
  std::string num;
  std::string den;
};

// Walks the AST and collects every BinaryExpr where op=="/" and both operands
// are integer literals (e.g. 3/4, 1/2).
struct IntDivFinder : boost::static_visitor<void> {
  std::vector<IntDivInstance>& found;
  explicit IntDivFinder(std::vector<IntDivInstance>& f) : found(f) {}

  void operator()(ast::Nil) const {}
  void operator()(const ast::Number&) const {}
  void operator()(const std::string&) const {}

  void operator()(const ast::UnaryExpr& u) const {
    boost::apply_visitor(*this, u.operand);
  }

  void operator()(const ast::BinaryExpr& b) const {
    if (b.op == "/") {
      const auto* lnum = boost::get<ast::Number>(&b.lhs);
      const auto* rnum = boost::get<ast::Number>(&b.rhs);
      if (lnum && rnum && is_integer_literal(*lnum) && is_integer_literal(*rnum)) {
        found.push_back({lnum->text, rnum->text});
      }
    }
    boost::apply_visitor(*this, b.lhs);
    boost::apply_visitor(*this, b.rhs);
  }

  void operator()(const ast::FuncCall& f) const {
    for (const auto& arg : f.args) {
      boost::apply_visitor(*this, arg);
    }
  }
};

static std::vector<IntDivInstance> find_integer_divisions(const ast::Expr& e) {
  std::vector<IntDivInstance> result;
  IntDivFinder finder(result);
  boost::apply_visitor(finder, e);
  return result;
}

// ---------------------------------------------------------------------------
// Per-line processor
// ---------------------------------------------------------------------------

// Find the first '=' that is a plain assignment — not part of ==, >=, <=, !=.
static size_t find_assign_eq(const std::string& s) {
  for (size_t i = 0; i < s.size(); ++i) {
    if (s[i] != '=') continue;
    if (i > 0 && (s[i-1] == '=' || s[i-1] == '>' ||
                  s[i-1] == '<' || s[i-1] == '!')) continue;
    if (i + 1 < s.size() && s[i+1] == '=') continue;
    return i;
  }
  return std::string::npos;
}

// Scan backwards from the assignment '=' to find where the LHS token begins,
// handling both simple identifiers (d, CL) and subscripted ones (A(1), THETA(2)).
// Returns the index of the first character of the LHS token.
static size_t find_lhs_start(const std::string& s, size_t eq) {
  if (eq == 0) return 0;
  size_t i = eq;
  while (i > 0 && s[i-1] == ' ') --i;  // skip spaces before '='
  if (i == 0) return 0;
  --i;  // last non-space character before '='
  if (s[i] == ')') {
    // LHS ends with ')' e.g. A(1) — scan back over balanced parens
    int depth = 1;
    while (i > 0 && depth > 0) {
      --i;
      if (s[i] == ')') ++depth;
      else if (s[i] == '(') --depth;
    }
    // scan back over the identifier preceding '('
    while (i > 0 && (isalnum((unsigned char)s[i-1]) || s[i-1] == '_')) --i;
  } else {
    // simple identifier — scan back while alnum or '_'
    while (i > 0 && (isalnum((unsigned char)s[i-1]) || s[i-1] == '_')) --i;
  }
  return i;
}

static std::string strip_line_comment(const std::string& s) {
  auto pos = s.find("//");
  return pos == std::string::npos ? s : s.substr(0, pos);
}

static std::string convert_line(const std::string& line) {
  if (line.find("**") == std::string::npos) return line;

  using It = std::string::const_iterator;
  ExprGrammar<It> grammar;

  auto eq = find_assign_eq(line);
  std::string prefix, rhs_str;

  std::string stripped_line = strip_line_comment(line);

  if (eq != std::string::npos) {
    size_t lhs_start    = find_lhs_start(line, eq);
    std::string cond_part   = line.substr(0, lhs_start);               // e.g. "if(x**2>5) "
    std::string assign_part = line.substr(lhs_start, eq + 1 - lhs_start); // e.g. "d ="
    rhs_str = stripped_line.substr(std::min(eq + 1, stripped_line.size()));

    // If the condition part (e.g. "if(x**2 > 5) ") contains **, try to convert it.
    if (cond_part.find("**") != std::string::npos) {
      std::string trimmed = cond_part;
      while (!trimmed.empty() && trimmed.back() == ' ') trimmed.pop_back();
      It cfirst = trimmed.cbegin(), clast = trimmed.cend();
      ast::Expr cond_result;
      bool cond_ok = qi::phrase_parse(cfirst, clast, grammar, asc::space, cond_result);
      if (cond_ok && cfirst == clast) {
        // re-attach any trailing whitespace that was trimmed off
        cond_part = emit(cond_result) + cond_part.substr(trimmed.size());
      }
    }

    prefix = cond_part + assign_part;
  } else {
    prefix  = "";
    rhs_str = stripped_line;
  }

  It first = rhs_str.cbegin();
  It last  = rhs_str.cend();
  ast::Expr result;

  // Optional trailing ';' consumed at call site; reattached to output.
  bool has_semi = false;
  bool ok = qi::phrase_parse(
    first, last,
    grammar >> -qi::lit(';')[phx::ref(has_semi) = true],
    asc::space,
    result
  );

  if (!ok || first != last) {
    return line;
  }

  std::string sep = prefix.empty() ? "" : " ";
  return prefix + sep + emit(result) + (has_semi ? ";" : "");
}

static std::vector<IntDivInstance> check_line_integer_division(const std::string& line) {
  using It = std::string::const_iterator;
  ExprGrammar<It> grammar;
  std::vector<IntDivInstance> all_found;

  std::string stripped = strip_line_comment(line);
  auto eq = find_assign_eq(stripped);
  std::string rhs_str;

  if (eq != std::string::npos) {
    size_t lhs_start = find_lhs_start(stripped, eq);
    std::string cond_part = stripped.substr(0, lhs_start);
    rhs_str = stripped.substr(eq + 1);

    if (!cond_part.empty()) {
      std::string trimmed = cond_part;
      while (!trimmed.empty() && trimmed.back() == ' ') trimmed.pop_back();
      It cfirst = trimmed.cbegin(), clast = trimmed.cend();
      ast::Expr cond_result;
      bool ok = qi::phrase_parse(cfirst, clast, grammar, asc::space, cond_result);
      if (ok && cfirst == clast) {
        auto found = find_integer_divisions(cond_result);
        all_found.insert(all_found.end(), found.begin(), found.end());
      }
    }
  } else {
    rhs_str = stripped;
  }

  It first = rhs_str.cbegin(), last = rhs_str.cend();
  ast::Expr result;
  bool ok = qi::phrase_parse(
    first, last,
    grammar >> -qi::lit(';'),
    asc::space,
    result
  );
  if (ok && first == last) {
    auto found = find_integer_divisions(result);
    all_found.insert(all_found.end(), found.begin(), found.end());
  }

  return all_found;
}

// ---------------------------------------------------------------------------
// Fortran IF/THEN/ELSE/ENDIF converter
// ---------------------------------------------------------------------------

// Case-insensitive prefix check.
static bool starts_with_ci(const std::string& s, const char* prefix) {
  for (size_t i = 0; prefix[i] != '\0'; ++i) {
    if (i >= s.size()) return false;
    if (std::toupper((unsigned char)s[i]) != std::toupper((unsigned char)prefix[i]))
      return false;
  }
  return true;
}

// Replace all case-insensitive occurrences of `from` with `to` in `s`.
static std::string replace_all_ci(const std::string& s,
                                   const std::string& from,
                                   const std::string& to) {
  if (from.empty() || s.size() < from.size()) return s;
  std::string result;
  result.reserve(s.size());
  size_t pos = 0;
  while (pos + from.size() <= s.size()) {
    bool match = true;
    for (size_t i = 0; i < from.size() && match; ++i) {
      match = (std::toupper((unsigned char)s[pos + i]) ==
               std::toupper((unsigned char)from[i]));
    }
    if (match) {
      result += to;
      pos += from.size();
    } else {
      result += s[pos++];
    }
  }
  result += s.substr(pos);
  return result;
}

// Convert Fortran relational/logical operators to C++ equivalents.
static std::string convert_fortran_ops(const std::string& s) {
  std::string r = s;
  r = replace_all_ci(r, ".GE.", ">=");
  r = replace_all_ci(r, ".LE.", "<=");
  r = replace_all_ci(r, ".GT.", ">");
  r = replace_all_ci(r, ".LT.", "<");
  r = replace_all_ci(r, ".EQ.", "==");
  r = replace_all_ci(r, ".NE.", "!=");
  r = replace_all_ci(r, ".AND.", "&&");
  r = replace_all_ci(r, ".OR.", "||");
  return r;
}

// Find the closing ')' matching the '(' at open_pos.  Returns npos if
// the parens are unbalanced.
static size_t find_close_paren(const std::string& s, size_t open_pos) {
  int depth = 1;
  for (size_t i = open_pos + 1; i < s.size(); ++i) {
    if      (s[i] == '(') ++depth;
    else if (s[i] == ')') { if (--depth == 0) return i; }
  }
  return std::string::npos;
}

// Trim leading and trailing spaces/tabs.
static std::string trim_ws(const std::string& s) {
  size_t b = s.find_first_not_of(" \t");
  if (b == std::string::npos) return "";
  size_t e = s.find_last_not_of(" \t");
  return s.substr(b, e - b + 1);
}

static std::string convert_fortran_if_line(const std::string& line) {
  // Preserve leading whitespace (indentation).
  size_t iend = 0;
  while (iend < line.size() &&
         (line[iend] == ' ' || line[iend] == '\t')) ++iend;
  const std::string indent = line.substr(0, iend);
  const std::string body   = line.substr(iend);

  if (body.empty()) return line;

  // Skip C-style comments unchanged.
  if (body.size() >= 2 && body[0] == '/' && body[1] == '/') return line;

  // Check for bare ENDIF / END IF and ELSE.
  std::string bu = trim_ws(body);
  {
    std::string u = bu;
    std::transform(u.begin(), u.end(), u.begin(),
                   [](unsigned char c){ return std::toupper(c); });
    if (u == "ENDIF" || u == "END IF") return indent + "}";
    if (u == "ELSE")                   return indent + "} else {";
  }

  // Detect IF / ELSEIF / ELSE IF keyword.
  bool is_elseif = false;
  size_t kw_end  = 0;

  if (starts_with_ci(body, "ELSEIF")) {
    is_elseif = true;
    kw_end = 6;
  } else if (starts_with_ci(body, "ELSE")) {
    // Could be "ELSE IF" (with one or more spaces).
    size_t p = 4;
    while (p < body.size() && body[p] == ' ') ++p;
    if (starts_with_ci(body.substr(p), "IF")) {
      is_elseif = true;
      kw_end = p + 2;
    } else {
      // ELSE with a non-empty body that is not IF — unexpected; convert ops.
      return indent + convert_fortran_ops(body);
    }
  } else if (starts_with_ci(body, "IF")) {
    kw_end = 2;
  } else {
    // Not an IF keyword line: convert Fortran operators and return.
    return indent + convert_fortran_ops(body);
  }

  // Skip optional space(s) between keyword and '('.
  size_t paren = kw_end;
  while (paren < body.size() && body[paren] == ' ') ++paren;

  if (paren >= body.size() || body[paren] != '(') {
    // Keyword not followed by '(' (e.g. a variable named "IF1").
    return indent + convert_fortran_ops(body);
  }

  // Find matching ')'.
  size_t close = find_close_paren(body, paren);
  if (close == std::string::npos) return indent + convert_fortran_ops(body);

  std::string cond  = convert_fortran_ops(
                        body.substr(paren + 1, close - paren - 1));
  std::string after = trim_ws(body.substr(close + 1));

  // Check whether the remainder is THEN.
  {
    std::string au = after;
    std::transform(au.begin(), au.end(), au.begin(),
                   [](unsigned char c){ return std::toupper(c); });
    if (au == "THEN") {
      return is_elseif
        ? indent + "} else if(" + cond + ") {"
        : indent + "if(" + cond + ") {";
    }
  }

  // Single-line form: IF(cond) statement.
  std::string stmt = convert_fortran_ops(after);
  return is_elseif
    ? indent + "} else if(" + cond + ") " + stmt
    : indent + "if(" + cond + ") " + stmt;
}

// ---------------------------------------------------------------------------
// Semicolon inserter
// ---------------------------------------------------------------------------

// Return the position of the first occurrence of ch in s at paren depth 0,
// or npos if none.
static size_t find_at_depth0(const std::string& s, char ch) {
  int depth = 0;
  for (size_t i = 0; i < s.size(); ++i) {
    if      (s[i] == '(') ++depth;
    else if (s[i] == ')') --depth;
    else if (s[i] == ch && depth == 0) return i;
  }
  return std::string::npos;
}

static size_t find_semi_depth0(const std::string& s) {
  return find_at_depth0(s, ';');
}

static bool contains_at_depth0(const std::string& s, char ch) {
  return find_at_depth0(s, ch) != std::string::npos;
}

static std::string convert_semicolon_line(const std::string& line) {
  std::string t = trim_ws(line);

  if (t.empty())                               return line;  // blank
  if (t.back() == '{' || t.back() == '}')      return line;  // brace line
  if (t[0] == '#')                             return line;  // preprocessor
  if (t.size() >= 2 && t[0] == '/' &&
      (t[1] == '/' || t[1] == '*'))            return line;  // comment

  // C++ inline comment mid-line: insert semicolon before '//'.
  size_t cmt = line.find("//");
  if (cmt != std::string::npos) {
    std::string code_part = line.substr(0, cmt);
    std::string code_trim = trim_ws(code_part);
    static const std::string cc = "+-*/&,=(;";
    if (!code_trim.empty() && cc.find(code_trim.back()) == std::string::npos) {
      size_t last_code = code_part.find_last_not_of(" \t");
      return code_part.substr(0, last_code + 1) + "; //" + line.substr(cmt + 2);
    }
    return line;
  }

  // Bare control-flow condition header: starts with a keyword, and the ')'
  // that closes the keyword's own '(' is the last character of the trimmed
  // line.  The body will be on the next line, so no semicolon belongs here.
  if (t.back() == ')') {
    static const char* ctrl[] = {"if", "else if", "for", "while", nullptr};
    for (int k = 0; ctrl[k]; ++k) {
      size_t kn = std::strlen(ctrl[k]);
      if (t.size() >= kn && t.substr(0, kn) == ctrl[k] &&
          (t.size() == kn || t[kn] == '(' || t[kn] == ' ')) {
        // Find the '(' that opens the keyword's condition and check that
        // its matching ')' is the very last character of t.
        size_t open = t.find('(', kn);
        if (open != std::string::npos) {
          size_t close = find_close_paren(t, open);
          if (close == t.size() - 1) return line;
        }
        break;
      }
    }
  }

  // Only consider ';' at paren depth 0 — keeps for(;;) intact.
  size_t semi = find_semi_depth0(line);

  if (semi == std::string::npos) {
    // No depth-0 ';': append one after the last non-whitespace character,
    // unless the line ends with a continuation operator.
    size_t last = line.find_last_not_of(" \t");
    static const std::string cont_chars = "+-*/&,=(";
    if (cont_chars.find(t.back()) != std::string::npos) return line;
    return line.substr(0, last + 1) + ";";
  }

  // Depth-0 ';' found — inspect what follows it.
  std::string after = trim_ws(line.substr(semi + 1));

  if (after.empty()) return line;  // ';' already at end

  // Already a C++ comment after ';' — leave it alone.
  if (after.size() >= 2 && after[0] == '/' && after[1] == '/') return line;

  // Another depth-0 ';' or '=' after this one suggests C++ statements
  // rather than a Fortran-style comment — leave it alone.
  if (contains_at_depth0(after, ';') || contains_at_depth0(after, '='))
    return line;

  // Fortran-style inline comment: drop text after ';'
  return line.substr(0, semi + 1);
}

// ---------------------------------------------------------------------------
// R-facing entry point
// ---------------------------------------------------------------------------

static void warn_no_call(const std::string& msg) {
  Rcpp::Function warning_r("warning");
  warning_r(msg, Rcpp::Named("call.") = false);
}

//' Convert Fortran-style exponentiation to C++ pow()
//'
//' Translates \code{base**exponent} to \code{pow(base, exponent)} in each
//' element of \code{code}, handling arbitrarily nested expressions.
//' Numeric literals are preserved exactly as written. A trailing semicolon
//' is preserved if present.
//'
//' @param code Character vector of source lines.
//' @param block Name of the model block, included in the warning message.
//' @return Character vector with \code{**} replaced by \code{pow()}.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::CharacterVector convert_pow_impl(Rcpp::CharacterVector code,
                                        std::string block) {
  Rcpp::CharacterVector out(code.size());
  for (int i = 0; i < code.size(); ++i) {
    try {
      std::string line = Rcpp::as<std::string>(code[i]);
      std::string converted = convert_line(line);
      size_t ts = line.find_first_not_of(" \t");
      bool is_comment = ts != std::string::npos &&
        ts + 1 < line.size() && line[ts] == '/' &&
        (line[ts + 1] == '/' || line[ts + 1] == '*');
      if (!is_comment && converted == line && line.find("**") != std::string::npos) {
        std::string prefix = block.empty()
          ? "Could not convert **"
          : "Could not convert ** in $" + block + " block";
        warn_no_call(prefix + ": '" + line + "'");
      }
      out[i] = converted;
    } catch (const std::exception& e) {
      Rcpp::warning("Line %d: %s", i + 1, e.what());
      out[i] = code[i];
    }
  }
  return out;
}

//' Warn about literal integer division in model code
//'
//' Scans each element of \code{code} and issues an R warning for every
//' instance of literal integer division found (e.g. \code{3/4}, \code{1/2}).
//' Integer division in C++ truncates toward zero, so \code{3/4} evaluates to
//' \code{0} and \code{7/3} evaluates to \code{2}, which is rarely intended.
//'
//' @param code Character vector of source lines.
//' @param block Name of the model block, included in the warning message.
//' @return \code{code} unchanged (called for its side-effect warnings).
//' @keywords internal
// [[Rcpp::export]]
Rcpp::CharacterVector warn_int_div_impl(Rcpp::CharacterVector code,
                                         std::string block) {
  for (int i = 0; i < code.size(); ++i) {
    try {
      std::string line = Rcpp::as<std::string>(code[i]);
      auto instances = check_line_integer_division(line);
      for (const auto& inst : instances) {
        std::string prefix = block.empty()
          ? "Integer division"
          : "Integer division in $" + block + " block";
        std::string msg =
          prefix + ": '" +
          inst.num + "/" + inst.den + "' truncates toward zero; " +
          "use " + inst.num + ".0/" + inst.den + ".0 for real division";
        warn_no_call(msg);
      }
    } catch (...) {}
  }
  return code;
}

//' Convert Fortran-style IF/THEN/ELSE/ENDIF to C++
//'
//' Translates Fortran block-form and single-line IF constructs to C++ in each
//' element of \code{code}.  Fortran relational and logical operators
//' (\code{.GE.}, \code{.LE.}, \code{.GT.}, \code{.LT.}, \code{.EQ.},
//' \code{.NE.}, \code{.AND.}, \code{.OR.}) are converted everywhere they
//' appear. Matching is
//' case-insensitive.
//'
//' @param code Character vector of source lines.
//' @return Character vector with Fortran IF constructs replaced by C++.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::CharacterVector convert_fort_if_impl(Rcpp::CharacterVector code) {
  Rcpp::CharacterVector out(code.size());
  for (int i = 0; i < code.size(); ++i) {
    try {
      out[i] = convert_fortran_if_line(Rcpp::as<std::string>(code[i]));
    } catch (const std::exception& e) {
      Rcpp::warning("Line %d: %s", i + 1, e.what());
      out[i] = code[i];
    }
  }
  return out;
}

//' Insert semicolons at the end of C++ statements
//'
//' Appends a semicolon to each element of \code{code} that looks like a
//' statement but does not already have one.  Lines that are left unchanged:
//' blank lines, lines already ending with \code{;}, lines ending with
//' \code{\{} or \code{\}}, C/C++ comments (\code{//} or \code{/*}), and
//' preprocessor directives (\code{#}).
//'
//' @param code Character vector of source lines.
//' @return Character vector with semicolons inserted where needed.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::CharacterVector convert_semicolons_impl(Rcpp::CharacterVector code) {
  Rcpp::CharacterVector out(code.size());
  for (int i = 0; i < code.size(); ++i) {
    out[i] = convert_semicolon_line(Rcpp::as<std::string>(code[i]));
  }
  return out;
}
