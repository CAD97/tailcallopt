extern crate proc_macro;

use matches::matches;
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse, parse_quote};
use syn::{Block, Expr, ExprBlock, FnArg, FnDecl, Ident, ItemFn, ReturnType, Stmt, Visibility};

struct TCO {
    fns: Vec<ItemFn>,
}

impl parse::Parse for TCO {
    fn parse(input: parse::ParseStream) -> parse::Result<Self> {
        let mut fns = vec![];

        while let Ok(fun) = input.parse() {
            fns.push(fun);
        }

        if !input.is_empty() {
            // Create error looking for ItemFn
            let _: ItemFn = input.parse()?;
        }

        Ok(TCO { fns })
    }
}

#[proc_macro]
pub fn tco(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    tco2(input.into()).unwrap_or_else(|x| x).into()
}

fn tco2(input: TokenStream) -> Result<TokenStream, TokenStream> {
    let TCO { fns } = ::syn::parse2(input).map_err(|err| err.to_compile_error())?;

    if let Some(lint) = fns.iter().find_map(lint_fn) {
        Err(lint)
    } else if fns.is_empty() {
        Err(quote! {
            compile_error!("At least one function definition is required for TCO");
        })
    } else if matches!(&fns[0].vis, Visibility::Inherited) || fns
        .iter()
        .skip(1)
        .any(|fun| !matches!(&fun.vis, Visibility::Inherited))
    {
        Err(quote! {
            compile_error!("The first and only the first fn in tco!{} must be non-private");
        })
    } else {
        let trampoline = Ident::new("__trampoline", Span::call_site());
        let trampoline = &trampoline;

        let next_enum = Ident::new("__NextCall", Span::call_site());
        let next_enum = &next_enum;
        let next_enum_iter = ::std::iter::repeat(next_enum);

        let output = match &fns[0].decl.output {
            ReturnType::Default => quote!(()),
            ReturnType::Type(_, typ) => quote!(#typ),
        };
        let output = &output;

        let entry = &fns[0].clone();

        let tco_fns: Vec<Ident> = fns.iter().map(|fun| fun.ident.clone()).collect();
        let tco_fns = &tco_fns;
        let tco_fns1 = tco_fns;

        let tco_variants: Vec<TokenStream> = fns.iter().map(|fun| {
            let name = &fun.ident;
            let inputs: Vec<_> = fun.decl.inputs.iter().map(|arg| match arg {
                arg @ FnArg::SelfRef(_) | arg @ FnArg::SelfValue(_) => Err(quote_spanned! {arg.span()=>
                    compile_error!("`self` argument not allowed here");
                }),
                arg @ FnArg::Inferred(_) | arg @ FnArg::Ignored(_) => Err(quote_spanned! {arg.span()=>
                    compile_error!("This kind of argument is only allowed in a closure (right?)");
                }),
                FnArg::Captured(arg) => Ok(arg.ty.clone()),
            }).collect::<Result<_, _>>()?;
            Ok(quote!(#name(#(#inputs),*)))
        }).collect::<Result<_, TokenStream>>()?;

        let trampoline_args: Vec<_> = fns
            .iter()
            .map(|fun| fun.decl.inputs.clone().into_iter()
                .map(|arg| match arg {
                    FnArg::SelfRef(_)
                    | FnArg::SelfValue(_)
                    | FnArg::Inferred(_)
                    | FnArg::Ignored(_) => unreachable!("should be caught earlier"),
                    FnArg::Captured(arg) => arg.pat,
                }).collect::<Vec<_>>())
            .collect();
        let trampoline_args1 = trampoline_args.iter();
        let trampoline_args2 = trampoline_args.iter();

        let trampolined_fn = fns
            .into_iter()
            .map(|fun| trampoline_fn(fun, next_enum, &tco_fns));

        let ItemFn {
            vis,
            constness,
            unsafety,
            asyncness,
            ident,
            decl,
            ..
        } = entry;
        let FnDecl { inputs, .. } = &**decl;

        let entry_inputs_passthrough = &trampoline_args[0];

        Ok(quote! {
            #vis #constness #unsafety #asyncness fn #ident ( #inputs ) -> #output {
                #trampoline ( #next_enum::#ident ( #(#entry_inputs_passthrough),* ) )
            }

            enum #next_enum {
                #(#tco_variants,)*
                __DONE ( #output ),
            }

            fn #trampoline ( mut next: #next_enum ) -> #output {
                #(#trampolined_fn)*

                loop {
                    next = match next {
                        #(#next_enum_iter::#tco_fns(#(#trampoline_args1),*) =>
                            #tco_fns1(#(#trampoline_args2),*),)*
                        #next_enum::__DONE ( res ) => return res,
                    }
                }
            }
        })
    }
}

fn lint_fn(fun: &ItemFn) -> Option<TokenStream> {
    if !fun.decl.generics.params.is_empty() {
        let span = fun.decl.generics.params.span();
        Some(quote_spanned! {span=>
            compile_error!("Generics are not supported for tail call optimization");
        })
    } else if let Some(where_clause) = &fun.decl.generics.where_clause {
        let span = where_clause.span();
        Some(quote_spanned! {span=>
            compile_error!("Where clauses are not supported for tail call optimization");
        })
    } else if let Some(variadic) = &fun.decl.variadic {
        let span = variadic.span();
        Some(quote_spanned! {span=>
            compile_error!("Variadic fn are not supported for tail call optimization");
        })
    } else if let Some(abi) = &fun.abi {
        let span = abi.span();
        Some(quote_spanned! {span=>
            compile_error!("Explicit ABIs are not supported for tail call optimization");
        })
    } else {
        None
    }
}

/// Transform a function item like
///
/// ```rust,no_run
/// fn foo(cond: bool) -> Bar {
///     if cond {
///         bar()
///     } else {
///         Bar::default()
///     }
/// }
/// ```
///
/// to a function item like
///
/// ```rust,no_run
/// fn foo(cond: bool) -> Next {
///     if cond {
///         Next::bar()
///     } else {
///         Next::DONE(Bar::default())
///     }
/// }
/// ```
fn trampoline_fn(fun: ItemFn, next_enum: &Ident, tco_fns: &[Ident]) -> TokenStream {
    let ItemFn {
        vis,
        constness,
        unsafety,
        asyncness,
        ident,
        decl,
        block,
        ..
    } = fun;

    let FnDecl { inputs, .. } = *decl;
    let block = tail_call_optimize_block(*block, next_enum, tco_fns);

    quote!(#vis #constness #unsafety #asyncness fn #ident ( #inputs ) -> #next_enum #block)
}

fn tail_call_optimize_block(block: Block, next_enum: &Ident, tco_fns: &[Ident]) -> Block {
    let Block {
        mut stmts,
        brace_token,
    } = block;

    match stmts.pop() {
        Some(Stmt::Expr(expr)) => {
            stmts.push(Stmt::Expr(tail_call_optimize(&expr, next_enum, tco_fns)))
        }
        Some(stmt) => stmts.push(stmt),
        None => {}
    }

    Block { stmts, brace_token }
}

fn tail_call_optimize(expr: &Expr, next_enum: &Ident, tco_fns: &[Ident]) -> Expr {
    match expr {
        Expr::Call(expr) => {
            if let Expr::Path(func) = &*expr.func {
                if func.qself.is_none()
                    && func.path.leading_colon.is_none()
                    && func.path.segments.len() == 1
                    && tco_fns.contains(&func.path.segments.first().unwrap().value().ident)
                {
                    let ident = &func.path.segments.first().unwrap().value().ident;
                    let args = &expr.args;
                    return parse_quote!(#next_enum :: #ident ( #args ));
                }
            }
        }
        Expr::If(expr) => {
            let cond = &expr.cond;
            let then_branch =
                tail_call_optimize_block(expr.then_branch.clone(), next_enum, tco_fns);
            let else_branch = expr
                .else_branch
                .as_ref()
                .map(|(_, expr)| tail_call_optimize(expr, next_enum, tco_fns));
            return parse_quote!(if #cond #then_branch else #else_branch);
        }
        Expr::Block(expr) => {
            return Expr::Block(ExprBlock {
                block: tail_call_optimize_block(expr.block.clone(), next_enum, tco_fns),
                attrs: expr.attrs.clone(),
                label: expr.label.clone(),
            });
        }
        _ => {} // fallthrough
                // TODO: Specify every Expr variant manually to fully determine which should fallthrough
    }

    parse_quote!(#next_enum :: __DONE ( #expr ))
}
