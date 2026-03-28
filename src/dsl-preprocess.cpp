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
  char op;   // '+' '-' '*' '/'  '^' (we map ** -> ^)
  Expr rhs;
  BinaryExpr() : op(0) {}
  BinaryExpr(Expr lhs_, char op_, Expr rhs_)
    : lhs(std::move(lhs_)), op(op_), rhs(std::move(rhs_)) {}
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

static int precedence(char op) {
  switch (op) {
    case '+': case '-': return 1;
    case '*': case '/': return 2;
    default: return 0;
  }
}

static const ast::BinaryExpr* as_binop(const ast::Expr& e) {
  return boost::get<ast::BinaryExpr>(&e);
}

static bool needs_parens_left(const ast::Expr& child, char parent_op) {
  const ast::BinaryExpr* b = as_binop(child);
  if (!b || b->op == '^') return false;
  return precedence(b->op) < precedence(parent_op);
}

static bool needs_parens_right(const ast::Expr& child, char parent_op) {
  const ast::BinaryExpr* b = as_binop(child);
  if (!b || b->op == '^') return false;
  int cp = precedence(b->op);
  int pp = precedence(parent_op);
  return cp < pp || (cp == pp && (parent_op == '-' || parent_op == '/'));
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
    if (b.op == '^') {
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
          _val = construct<ast::BinaryExpr>(_val, '^', _1)
        ]
      );

    // unary: optional leading +/-; wraps power so -a**2 == -(a**2)
    unary =
        ( char_('+') >> unary )[_val = construct<ast::UnaryExpr>(_1, _2)]
      | ( char_('-') >> unary )[_val = construct<ast::UnaryExpr>(_1, _2)]
      | power                  [_val = _1]
      ;

    // multiplicative: * and /
    mulop = char_('*') | char_('/');
    multiplicative =
      unary[_val = _1]
      >> *(
        (mulop >> unary)[
          _val = construct<ast::BinaryExpr>(_val, _1, _2)
        ]
      );

    // additive: + and -
    addop = char_('+') | char_('-');
    additive =
      multiplicative[_val = _1]
      >> *(
        (addop >> multiplicative)[
          _val = construct<ast::BinaryExpr>(_val, _1, _2)
        ]
      );

    expr = additive;
  }

  qi::rule<Iterator, ast::Expr(),              asc::space_type> expr;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> additive;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> multiplicative;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> power;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> unary;
  qi::rule<Iterator, ast::Expr(),              asc::space_type> primary;
  qi::rule<Iterator, ast::Number(),            asc::space_type> number;
  qi::rule<Iterator, ast::FuncCall(),          asc::space_type> funccall;
  qi::rule<Iterator, std::vector<ast::Expr>(), asc::space_type> arglist;
  qi::rule<Iterator, std::string(),            asc::space_type> identifier;
  qi::rule<Iterator, char(),                   asc::space_type> addop;
  qi::rule<Iterator, char(),                   asc::space_type> mulop;
};

// ---------------------------------------------------------------------------
// 4.  Per-line processor
// ---------------------------------------------------------------------------

static std::string convert_line(const std::string& line) {
  if (line.find("**") == std::string::npos) return line;

  using It = std::string::const_iterator;
  ExprGrammar<It> grammar;

  auto eq = line.find('=');
  std::string prefix, rhs_str;
  if (eq != std::string::npos) {
    prefix  = line.substr(0, eq + 1);
    rhs_str = line.substr(eq + 1);
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

//' Convert Fortran-style exponentiation to C++ pow()
//'
//' Translates \code{base**exponent} to \code{pow(base, exponent)} in each
//' element of \code{code}, handling arbitrarily nested expressions.
//' Numeric literals are preserved exactly as written. A trailing semicolon
//' is preserved if present.
//'
//' @param code Character vector of source lines.
//' @return Character vector with \code{**} replaced by \code{pow()}.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::CharacterVector convert_pow_impl(Rcpp::CharacterVector code) {
  Rcpp::CharacterVector out(code.size());
  for (int i = 0; i < code.size(); ++i) {
    try {
      out[i] = convert_line(Rcpp::as<std::string>(code[i]));
    } catch (const std::exception& e) {
      Rcpp::warning("Line %d: %s", i + 1, e.what());
      out[i] = code[i];
    }
  }
  return out;
}
