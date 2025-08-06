const SPACE = /[ \r\n\t\f\v\p{Zs}\p{Zl}\p{Zp}]/

const SIGN = /[+-]/

const DIGIT = /[0-9]/

const SYMBOL = token(repeat1(/[^ \r\n\t\f\v\p{Zs}\p{Zl}\p{Zp}#:;"'`,(){}\[\]\\|]/))

const PACKAGE_QUALIFIER = /:{1,2}/

module.exports = grammar({

  name: "coalton",

  rules: {
    program: $ => repeat($._token),
    _token: $ => choice($._space, $._value),
    _space: $ => choice(token(repeat1(SPACE)), $.comment),
    comment: $ => /;.*/,
    _value: $ => choice($.string,
                        $.number,
                        $.symbol,
                        $.list),
    number: _ => token(num()),
    string: $ => seq('"', /[^"\\]*/, '"'),

    _symbol_qualified: $ =>
    prec(1, seq(field("package", alias(SYMBOL, $.symbol_package)),
                field("qualifier", alias(PACKAGE_QUALIFIER, $.symbol_qualifier)),
                field("name", alias(SYMBOL, $.symbol_name)))),

    _symbol: $ =>
    field('name', alias(choice(token("/"), SYMBOL), $.symbol_name)),

    symbol: $ =>
        choice($._symbol_qualified, $._symbol),

    list: $ => seq(field("open", "("),
                   repeat($._token),
                   field("close", ")"))
  },
  
})

function num() {

  const int = repeat1(DIGIT)
  const exp = optional(seq(/[eE]/,
                           optional(SIGN),
                           repeat1(DIGIT)))
  const dec = choice(seq(int, exp),
                     seq(".",
                         repeat1(DIGIT),
                         exp),
                     seq(repeat1(DIGIT),
                         ".",
                         repeat(DIGIT),
                         exp))
  const real = choice(int, dec)

  return seq(optional(SIGN), real)
}
