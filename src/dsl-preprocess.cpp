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
// 4.  Per-line processor
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

static std::string convert_line(const std::string& line) {
  if (line.find("**") == std::string::npos) return line;

  using It = std::string::const_iterator;
  ExprGrammar<It> grammar;

  auto eq = find_assign_eq(line);
  std::string prefix, rhs_str;

  if (eq != std::string::npos) {
    size_t lhs_start    = find_lhs_start(line, eq);
    std::string cond_part   = line.substr(0, lhs_start);               // e.g. "if(x**2>5) "
    std::string assign_part = line.substr(lhs_start, eq + 1 - lhs_start); // e.g. "d ="
    rhs_str = line.substr(eq + 1);

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
    rhs_str = line;
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

// ---------------------------------------------------------------------------
// 5.  R-facing entry point
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
