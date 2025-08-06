#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 32
#define LARGE_STATE_COUNT 9
#define SYMBOL_COUNT 21
#define ALIAS_COUNT 1
#define TOKEN_COUNT 11
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 5
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 7

enum ts_symbol_identifiers {
  aux_sym__space_token1 = 1,
  sym_comment = 2,
  sym_number = 3,
  anon_sym_DQUOTE = 4,
  aux_sym_string_token1 = 5,
  aux_sym__symbol_qualified_token1 = 6,
  aux_sym__symbol_qualified_token2 = 7,
  anon_sym_SLASH = 8,
  anon_sym_LPAREN = 9,
  anon_sym_RPAREN = 10,
  sym_program = 11,
  sym__token = 12,
  sym__space = 13,
  sym__value = 14,
  sym_string = 15,
  sym__symbol_qualified = 16,
  sym__symbol = 17,
  sym_symbol = 18,
  sym_list = 19,
  aux_sym_program_repeat1 = 20,
  alias_sym_symbol_package = 21,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [aux_sym__space_token1] = "_space_token1",
  [sym_comment] = "comment",
  [sym_number] = "number",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_token1] = "string_token1",
  [aux_sym__symbol_qualified_token1] = "symbol_name",
  [aux_sym__symbol_qualified_token2] = "symbol_qualifier",
  [anon_sym_SLASH] = "symbol_name",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [sym_program] = "program",
  [sym__token] = "_token",
  [sym__space] = "_space",
  [sym__value] = "_value",
  [sym_string] = "string",
  [sym__symbol_qualified] = "_symbol_qualified",
  [sym__symbol] = "_symbol",
  [sym_symbol] = "symbol",
  [sym_list] = "list",
  [aux_sym_program_repeat1] = "program_repeat1",
  [alias_sym_symbol_package] = "symbol_package",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [aux_sym__space_token1] = aux_sym__space_token1,
  [sym_comment] = sym_comment,
  [sym_number] = sym_number,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_token1] = aux_sym_string_token1,
  [aux_sym__symbol_qualified_token1] = aux_sym__symbol_qualified_token1,
  [aux_sym__symbol_qualified_token2] = aux_sym__symbol_qualified_token2,
  [anon_sym_SLASH] = aux_sym__symbol_qualified_token1,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [sym_program] = sym_program,
  [sym__token] = sym__token,
  [sym__space] = sym__space,
  [sym__value] = sym__value,
  [sym_string] = sym_string,
  [sym__symbol_qualified] = sym__symbol_qualified,
  [sym__symbol] = sym__symbol,
  [sym_symbol] = sym_symbol,
  [sym_list] = sym_list,
  [aux_sym_program_repeat1] = aux_sym_program_repeat1,
  [alias_sym_symbol_package] = alias_sym_symbol_package,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [aux_sym__space_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_number] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_string_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__symbol_qualified_token1] = {
    .visible = true,
    .named = true,
  },
  [aux_sym__symbol_qualified_token2] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [sym_program] = {
    .visible = true,
    .named = true,
  },
  [sym__token] = {
    .visible = false,
    .named = true,
  },
  [sym__space] = {
    .visible = false,
    .named = true,
  },
  [sym__value] = {
    .visible = false,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [sym__symbol_qualified] = {
    .visible = false,
    .named = true,
  },
  [sym__symbol] = {
    .visible = false,
    .named = true,
  },
  [sym_symbol] = {
    .visible = true,
    .named = true,
  },
  [sym_list] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_program_repeat1] = {
    .visible = false,
    .named = false,
  },
  [alias_sym_symbol_package] = {
    .visible = true,
    .named = true,
  },
};

enum ts_field_identifiers {
  field_close = 1,
  field_name = 2,
  field_open = 3,
  field_package = 4,
  field_qualifier = 5,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_close] = "close",
  [field_name] = "name",
  [field_open] = "open",
  [field_package] = "package",
  [field_qualifier] = "qualifier",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 3},
  [3] = {.index = 4, .length = 1},
  [4] = {.index = 5, .length = 2},
  [5] = {.index = 7, .length = 3},
  [6] = {.index = 10, .length = 2},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_name, 0, .inherited = true},
    {field_package, 0, .inherited = true},
    {field_qualifier, 0, .inherited = true},
  [4] =
    {field_name, 0, .inherited = true},
  [5] =
    {field_close, 1},
    {field_open, 0},
  [7] =
    {field_name, 2},
    {field_package, 0},
    {field_qualifier, 1},
  [10] =
    {field_close, 2},
    {field_open, 0},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [5] = {
    [0] = alias_sym_symbol_package,
  },
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 2,
  [4] = 4,
  [5] = 4,
  [6] = 6,
  [7] = 7,
  [8] = 6,
  [9] = 9,
  [10] = 9,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 15,
  [19] = 12,
  [20] = 17,
  [21] = 13,
  [22] = 11,
  [23] = 14,
  [24] = 16,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 25,
  [30] = 26,
  [31] = 27,
};

static TSCharacterRange aux_sym__space_token1_character_set_1[] = {
  {'\t', '\r'}, {' ', ' '}, {0xa0, 0xa0}, {0x1680, 0x1680}, {0x2000, 0x200a}, {0x2028, 0x2029}, {0x202f, 0x202f}, {0x205f, 0x205f},
  {0x3000, 0x3000},
};

static TSCharacterRange aux_sym__symbol_qualified_token1_character_set_1[] = {
  {0, 0x08}, {0x0e, 0x1f}, {'!', '!'}, {'$', '&'}, {'*', '+'}, {'-', '9'}, {'<', 'Z'}, {'^', '_'},
  {'a', 'z'}, {'~', 0x9f}, {0xa1, 0x167f}, {0x1681, 0x1fff}, {0x200b, 0x2027}, {0x202a, 0x202e}, {0x2030, 0x205e}, {0x2060, 0x2fff},
  {0x3001, 0x10ffff},
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(2);
      ADVANCE_MAP(
        '"', 9,
        '(', 20,
        ')', 21,
        '.', 14,
        '/', 19,
        ':', 18,
        ';', 5,
        '+', 12,
        '-', 12,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(3);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(6);
      if (lookahead == 0xa0 ||
          lookahead == 0x1680 ||
          (0x2000 <= lookahead && lookahead <= 0x200a) ||
          lookahead == 0x2028 ||
          lookahead == 0x2029 ||
          lookahead == 0x202f ||
          lookahead == 0x205f ||
          lookahead == 0x3000) ADVANCE(4);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '#' &&
          (lookahead < '\'' || ')' < lookahead) &&
          (lookahead < '+' || ';' < lookahead) &&
          (lookahead < '[' || ']' < lookahead) &&
          lookahead != '`' &&
          (lookahead < '{' || '}' < lookahead)) ADVANCE(16);
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(9);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(1);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 2:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 3:
      ACCEPT_TOKEN(aux_sym__space_token1);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead == 0xa0 ||
          lookahead == 0x1680 ||
          (0x2000 <= lookahead && lookahead <= 0x200a) ||
          lookahead == 0x2028 ||
          lookahead == 0x2029 ||
          lookahead == 0x202f ||
          lookahead == 0x205f ||
          lookahead == 0x3000) ADVANCE(4);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(aux_sym__space_token1);
      if (set_contains(aux_sym__space_token1_character_set_1, 9, lookahead)) ADVANCE(4);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(5);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(7);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(13);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(6);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(13);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(7);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(8);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(10);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(11);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(11);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(aux_sym__symbol_qualified_token1);
      if (lookahead == '.') ADVANCE(14);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(6);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(aux_sym__symbol_qualified_token1);
      if (lookahead == '+' ||
          lookahead == '-') ADVANCE(15);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(8);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(aux_sym__symbol_qualified_token1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(7);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(aux_sym__symbol_qualified_token1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(8);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(aux_sym__symbol_qualified_token1);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(aux_sym__symbol_qualified_token2);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(aux_sym__symbol_qualified_token2);
      if (lookahead == ':') ADVANCE(17);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_SLASH);
      if ((!eof && set_contains(aux_sym__symbol_qualified_token1_character_set_1, 17, lookahead))) ADVANCE(16);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 1},
  [27] = {.lex_state = 10},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 1},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 10},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [aux_sym__space_token1] = ACTIONS(1),
    [sym_comment] = ACTIONS(1),
    [sym_number] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [aux_sym__symbol_qualified_token1] = ACTIONS(1),
    [aux_sym__symbol_qualified_token2] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
  },
  [1] = {
    [sym_program] = STATE(28),
    [sym__token] = STATE(7),
    [sym__space] = STATE(7),
    [sym__value] = STATE(7),
    [sym_string] = STATE(7),
    [sym__symbol_qualified] = STATE(20),
    [sym__symbol] = STATE(15),
    [sym_symbol] = STATE(7),
    [sym_list] = STATE(7),
    [aux_sym_program_repeat1] = STATE(7),
    [ts_builtin_sym_end] = ACTIONS(3),
    [aux_sym__space_token1] = ACTIONS(5),
    [sym_comment] = ACTIONS(7),
    [sym_number] = ACTIONS(7),
    [anon_sym_DQUOTE] = ACTIONS(9),
    [aux_sym__symbol_qualified_token1] = ACTIONS(11),
    [anon_sym_SLASH] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
  [2] = {
    [sym__token] = STATE(6),
    [sym__space] = STATE(6),
    [sym__value] = STATE(6),
    [sym_string] = STATE(6),
    [sym__symbol_qualified] = STATE(17),
    [sym__symbol] = STATE(18),
    [sym_symbol] = STATE(6),
    [sym_list] = STATE(6),
    [aux_sym_program_repeat1] = STATE(6),
    [aux_sym__space_token1] = ACTIONS(17),
    [sym_comment] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [anon_sym_DQUOTE] = ACTIONS(21),
    [aux_sym__symbol_qualified_token1] = ACTIONS(23),
    [anon_sym_SLASH] = ACTIONS(25),
    [anon_sym_LPAREN] = ACTIONS(27),
    [anon_sym_RPAREN] = ACTIONS(29),
  },
  [3] = {
    [sym__token] = STATE(6),
    [sym__space] = STATE(6),
    [sym__value] = STATE(6),
    [sym_string] = STATE(6),
    [sym__symbol_qualified] = STATE(17),
    [sym__symbol] = STATE(18),
    [sym_symbol] = STATE(6),
    [sym_list] = STATE(6),
    [aux_sym_program_repeat1] = STATE(6),
    [aux_sym__space_token1] = ACTIONS(17),
    [sym_comment] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [anon_sym_DQUOTE] = ACTIONS(21),
    [aux_sym__symbol_qualified_token1] = ACTIONS(23),
    [anon_sym_SLASH] = ACTIONS(25),
    [anon_sym_LPAREN] = ACTIONS(27),
    [anon_sym_RPAREN] = ACTIONS(31),
  },
  [4] = {
    [sym__token] = STATE(3),
    [sym__space] = STATE(3),
    [sym__value] = STATE(3),
    [sym_string] = STATE(3),
    [sym__symbol_qualified] = STATE(17),
    [sym__symbol] = STATE(18),
    [sym_symbol] = STATE(3),
    [sym_list] = STATE(3),
    [aux_sym_program_repeat1] = STATE(3),
    [aux_sym__space_token1] = ACTIONS(33),
    [sym_comment] = ACTIONS(35),
    [sym_number] = ACTIONS(35),
    [anon_sym_DQUOTE] = ACTIONS(21),
    [aux_sym__symbol_qualified_token1] = ACTIONS(23),
    [anon_sym_SLASH] = ACTIONS(25),
    [anon_sym_LPAREN] = ACTIONS(27),
    [anon_sym_RPAREN] = ACTIONS(37),
  },
  [5] = {
    [sym__token] = STATE(2),
    [sym__space] = STATE(2),
    [sym__value] = STATE(2),
    [sym_string] = STATE(2),
    [sym__symbol_qualified] = STATE(17),
    [sym__symbol] = STATE(18),
    [sym_symbol] = STATE(2),
    [sym_list] = STATE(2),
    [aux_sym_program_repeat1] = STATE(2),
    [aux_sym__space_token1] = ACTIONS(39),
    [sym_comment] = ACTIONS(41),
    [sym_number] = ACTIONS(41),
    [anon_sym_DQUOTE] = ACTIONS(21),
    [aux_sym__symbol_qualified_token1] = ACTIONS(23),
    [anon_sym_SLASH] = ACTIONS(25),
    [anon_sym_LPAREN] = ACTIONS(27),
    [anon_sym_RPAREN] = ACTIONS(43),
  },
  [6] = {
    [sym__token] = STATE(6),
    [sym__space] = STATE(6),
    [sym__value] = STATE(6),
    [sym_string] = STATE(6),
    [sym__symbol_qualified] = STATE(17),
    [sym__symbol] = STATE(18),
    [sym_symbol] = STATE(6),
    [sym_list] = STATE(6),
    [aux_sym_program_repeat1] = STATE(6),
    [aux_sym__space_token1] = ACTIONS(45),
    [sym_comment] = ACTIONS(48),
    [sym_number] = ACTIONS(48),
    [anon_sym_DQUOTE] = ACTIONS(51),
    [aux_sym__symbol_qualified_token1] = ACTIONS(54),
    [anon_sym_SLASH] = ACTIONS(57),
    [anon_sym_LPAREN] = ACTIONS(60),
    [anon_sym_RPAREN] = ACTIONS(63),
  },
  [7] = {
    [sym__token] = STATE(8),
    [sym__space] = STATE(8),
    [sym__value] = STATE(8),
    [sym_string] = STATE(8),
    [sym__symbol_qualified] = STATE(20),
    [sym__symbol] = STATE(15),
    [sym_symbol] = STATE(8),
    [sym_list] = STATE(8),
    [aux_sym_program_repeat1] = STATE(8),
    [ts_builtin_sym_end] = ACTIONS(65),
    [aux_sym__space_token1] = ACTIONS(67),
    [sym_comment] = ACTIONS(69),
    [sym_number] = ACTIONS(69),
    [anon_sym_DQUOTE] = ACTIONS(9),
    [aux_sym__symbol_qualified_token1] = ACTIONS(11),
    [anon_sym_SLASH] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
  [8] = {
    [sym__token] = STATE(8),
    [sym__space] = STATE(8),
    [sym__value] = STATE(8),
    [sym_string] = STATE(8),
    [sym__symbol_qualified] = STATE(20),
    [sym__symbol] = STATE(15),
    [sym_symbol] = STATE(8),
    [sym_list] = STATE(8),
    [aux_sym_program_repeat1] = STATE(8),
    [ts_builtin_sym_end] = ACTIONS(71),
    [aux_sym__space_token1] = ACTIONS(73),
    [sym_comment] = ACTIONS(76),
    [sym_number] = ACTIONS(76),
    [anon_sym_DQUOTE] = ACTIONS(79),
    [aux_sym__symbol_qualified_token1] = ACTIONS(82),
    [anon_sym_SLASH] = ACTIONS(85),
    [anon_sym_LPAREN] = ACTIONS(88),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 3,
    ACTIONS(95), 1,
      aux_sym__symbol_qualified_token2,
    ACTIONS(91), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(93), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [16] = 3,
    ACTIONS(91), 1,
      aux_sym__space_token1,
    ACTIONS(97), 1,
      aux_sym__symbol_qualified_token2,
    ACTIONS(93), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [32] = 2,
    ACTIONS(99), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(101), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [45] = 2,
    ACTIONS(103), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(105), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [58] = 2,
    ACTIONS(107), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(109), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [71] = 2,
    ACTIONS(111), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(113), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [84] = 2,
    ACTIONS(115), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(117), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [97] = 2,
    ACTIONS(91), 1,
      aux_sym__space_token1,
    ACTIONS(93), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [110] = 2,
    ACTIONS(119), 1,
      aux_sym__space_token1,
    ACTIONS(121), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [123] = 2,
    ACTIONS(115), 1,
      aux_sym__space_token1,
    ACTIONS(117), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [136] = 2,
    ACTIONS(103), 1,
      aux_sym__space_token1,
    ACTIONS(105), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [149] = 2,
    ACTIONS(119), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(121), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [162] = 2,
    ACTIONS(107), 1,
      aux_sym__space_token1,
    ACTIONS(109), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [175] = 2,
    ACTIONS(99), 1,
      aux_sym__space_token1,
    ACTIONS(101), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [188] = 2,
    ACTIONS(111), 1,
      aux_sym__space_token1,
    ACTIONS(113), 7,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [201] = 2,
    ACTIONS(91), 2,
      ts_builtin_sym_end,
      aux_sym__space_token1,
    ACTIONS(93), 6,
      sym_comment,
      sym_number,
      anon_sym_DQUOTE,
      aux_sym__symbol_qualified_token1,
      anon_sym_SLASH,
      anon_sym_LPAREN,
  [214] = 1,
    ACTIONS(123), 1,
      anon_sym_DQUOTE,
  [218] = 1,
    ACTIONS(125), 1,
      aux_sym__symbol_qualified_token1,
  [222] = 1,
    ACTIONS(127), 1,
      aux_sym_string_token1,
  [226] = 1,
    ACTIONS(129), 1,
      ts_builtin_sym_end,
  [230] = 1,
    ACTIONS(131), 1,
      anon_sym_DQUOTE,
  [234] = 1,
    ACTIONS(133), 1,
      aux_sym__symbol_qualified_token1,
  [238] = 1,
    ACTIONS(135), 1,
      aux_sym_string_token1,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(9)] = 0,
  [SMALL_STATE(10)] = 16,
  [SMALL_STATE(11)] = 32,
  [SMALL_STATE(12)] = 45,
  [SMALL_STATE(13)] = 58,
  [SMALL_STATE(14)] = 71,
  [SMALL_STATE(15)] = 84,
  [SMALL_STATE(16)] = 97,
  [SMALL_STATE(17)] = 110,
  [SMALL_STATE(18)] = 123,
  [SMALL_STATE(19)] = 136,
  [SMALL_STATE(20)] = 149,
  [SMALL_STATE(21)] = 162,
  [SMALL_STATE(22)] = 175,
  [SMALL_STATE(23)] = 188,
  [SMALL_STATE(24)] = 201,
  [SMALL_STATE(25)] = 214,
  [SMALL_STATE(26)] = 218,
  [SMALL_STATE(27)] = 222,
  [SMALL_STATE(28)] = 226,
  [SMALL_STATE(29)] = 230,
  [SMALL_STATE(30)] = 234,
  [SMALL_STATE(31)] = 238,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_program, 0, 0, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(9),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(4),
  [29] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(3),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [41] = {.entry = {.count = 1, .reusable = false}}, SHIFT(2),
  [43] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [45] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(6),
  [48] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(6),
  [51] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(31),
  [54] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(10),
  [57] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(16),
  [60] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(4),
  [63] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_program, 1, 0, 0),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [69] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(8),
  [76] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(8),
  [79] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(27),
  [82] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(9),
  [85] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(24),
  [88] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_program_repeat1, 2, 0, 0), SHIFT_REPEAT(5),
  [91] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__symbol, 1, 0, 1),
  [93] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__symbol, 1, 0, 1),
  [95] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [97] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [99] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__symbol_qualified, 3, 0, 5),
  [101] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__symbol_qualified, 3, 0, 5),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 2, 0, 4),
  [105] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list, 2, 0, 4),
  [107] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3, 0, 0),
  [109] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_string, 3, 0, 0),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 3, 0, 6),
  [113] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list, 3, 0, 6),
  [115] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_symbol, 1, 0, 3),
  [117] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_symbol, 1, 0, 3),
  [119] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_symbol, 1, 0, 2),
  [121] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_symbol, 1, 0, 2),
  [123] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [125] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [127] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [129] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_coalton(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
