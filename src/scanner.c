#include <tree_sitter/parser.h>

enum TokenType
{
    multi_line_string
};

void * tree_sitter_onyx_external_scanner_create() {
    return NULL;
}

void tree_sitter_onyx_external_scanner_destroy(void *payload) {
}

unsigned tree_sitter_onyx_external_scanner_serialize(void *payload, char *buffer)  {
    return 0;
}

void tree_sitter_onyx_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
}

static int is_whitespace(int32_t character) {
    return character == ' ' || character ==  '\t' || character == '\r' || character == '\n';
}

static void skip_whitespace(TSLexer *lexer) {
    while (is_whitespace(lexer->lookahead)) {
        lexer->advance(lexer, 1);
    }
}

bool tree_sitter_onyx_external_scanner_scan(void* payload, TSLexer * lexer, const bool* valid_symbols) {
    int matched = 0;

    skip_whitespace(lexer);
    while (matched < 3) {
        if (lexer->lookahead == '"') {
            lexer->advance(lexer, 0);
            matched++;
        } else {
            return 0;
        }
    }

    while (!lexer->eof(lexer))
    {
        if (lexer->lookahead == '"')
        {
            matched++;
        }
        else
        {
            matched = 0;
        }

        if (matched == 3)
        {
            lexer->advance(lexer, false);
            lexer->mark_end(lexer);
            lexer->result_symbol = multi_line_string;
            return 1;
        }

        lexer->advance(lexer, false);
    }

    return 0;
}
