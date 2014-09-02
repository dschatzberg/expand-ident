#![crate_name = "expand_ident"]
#![experimental]
#![feature(plugin_registrar)]

extern crate rustc;
extern crate syntax;

use rustc::plugin::registry::Registry;
use syntax::ast;
use syntax::codemap;
use syntax::ext::base::DummyResult;
use syntax::parse;

use syntax::codemap::Span;
use syntax::ext::base::{ExtCtxt, MacResult};
use syntax::fold::Folder;
use syntax::parse::parser::Parser;
use syntax::parse::attr::ParserAttr;
use syntax::parse::token::{SEMI, EOF};
use syntax::parse::token;
use syntax::util::small_vector::SmallVector;

use std::cell::RefCell;

use std::gc::Gc;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("expand_string_to_expr", expand_string_to_expr);
}

struct ParserAnyMacro<'a> {
    parser: RefCell<Parser<'a>>,
}

impl<'a> ParserAnyMacro<'a> {
    /// Make sure we don't have any tokens left to parse, so we don't
    /// silently drop anything. `allow_semi` is so that "optional"
    /// semicolons at the end of normal expressions aren't complained
    /// about e.g. the semicolon in `macro_rules! kapow( () => {
    /// fail!(); } )` doesn't get picked up by .parse_expr(), but it's
    /// allowed to be there.
    fn ensure_complete_parse(&self, allow_semi: bool) {
        let mut parser = self.parser.borrow_mut();
        if allow_semi && parser.token == SEMI {
            parser.bump()
        }
        if parser.token != EOF {
            let token_str = parser.this_token_to_string();
            let msg = format!("macro expansion ignores token `{}` and any \
                               following",
                              token_str);
            let span = parser.span;
            parser.span_err(span, msg.as_slice());
        }
    }
}

impl<'a> MacResult for ParserAnyMacro<'a> {
    fn make_expr(&self) -> Option<Gc<ast::Expr>> {
        let ret = self.parser.borrow_mut().parse_expr();
        self.ensure_complete_parse(true);
        Some(ret)
    }
    fn make_pat(&self) -> Option<Gc<ast::Pat>> {
        let ret = self.parser.borrow_mut().parse_pat();
        self.ensure_complete_parse(false);
        Some(ret)
    }
    fn make_items(&self) -> Option<SmallVector<Gc<ast::Item>>> {
        let mut ret = SmallVector::zero();
        loop {
            let mut parser = self.parser.borrow_mut();
            // so... do outer attributes attached to the macro invocation
            // just disappear? This question applies to make_methods, as
            // well.
            match parser.parse_item_with_outer_attributes() {
                Some(item) => ret.push(item),
                None => break
            }
        }
        self.ensure_complete_parse(false);
        Some(ret)
    }

    fn make_methods(&self) -> Option<SmallVector<Gc<ast::Method>>> {
        let mut ret = SmallVector::zero();
        loop {
            let mut parser = self.parser.borrow_mut();
            match parser.token {
                EOF => break,
                _ => ret.push(parser.parse_method(None))
            }
        }
        self.ensure_complete_parse(false);
        Some(ret)
    }

    fn make_stmt(&self) -> Option<Gc<ast::Stmt>> {
        let attrs = self.parser.borrow_mut().parse_outer_attributes();
        let ret = self.parser.borrow_mut().parse_stmt(attrs);
        self.ensure_complete_parse(true);
        Some(ret)
    }
}

fn expand_string_to_expr<'a>(cx: &'a mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult + 'a> {
    use syntax::print::pprust;

    let mut parser = parse::new_parser_from_tts(cx.parse_sess(), cx.cfg(), Vec::from_slice(tts));
    let arg = cx.expander().fold_expr(parser.parse_expr());
    let expr_string = match arg.node {
        ast::ExprLit(spanned) => {
            match spanned.node {
                ast::LitStr(ref s, _) => s.to_string(),
                _ => {
                    cx.span_err(sp, format!(
                            "expected string literal but got `{}`",
                            pprust::lit_to_string(&*spanned)).as_slice());
                    return DummyResult::expr(sp)
                }
            }
        }
        _ => {
            cx.span_err(sp, format!(
                    "expected string literal but got `{}`",
                    pprust::expr_to_string(&*arg)).as_slice());
            return DummyResult::expr(sp)
        }
    };
    if !parser.eat(&token::EOF) {
        cx.span_err(parser.span, "only one string literal allowed");
        return DummyResult::expr(sp);
    }

    let p = parse::new_parser_from_source_str(cx.parse_sess(), cx.cfg(), "string_expr".to_string(), expr_string);
    return box ParserAnyMacro{
        parser: std::cell::RefCell::new(p),
    } as Box<MacResult>
}
