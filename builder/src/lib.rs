use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{punctuated::Punctuated, *};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder_macro(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let mut builder_tokens = proc_macro2::TokenStream::new();
    let mut build_tokens = proc_macro2::TokenStream::new(); // for builder's build method

    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        let (builder, build_struct_fields) = derive_builder_implementation(named);

        if !builder.is_empty() {
            builder_tokens = quote! {
                #(#builder)*
            };
        };

        if !build_struct_fields.is_empty() {
            build_tokens = quote! {
                #(#build_struct_fields)*
            };
        };
    }

    let generics = input.generics;
    let ident = input.ident;
    let builder_type_name = ident.to_string() + "Builder";
    let builder_type_name_ident = Ident::new(&builder_type_name, Span::call_site());
    let tokens = quote! {
        use std::error::Error;

        pub struct #builder_type_name_ident {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl #generics #ident {
            pub fn builder() -> #builder_type_name_ident {
                #builder_type_name_ident {
                    executable: None,
                    args: Some(Vec::new()),
                    env: Some(Vec::new()),
                    current_dir: None,
                }
            }
        }

        impl #builder_type_name_ident {
            #builder_tokens

            pub fn build(&mut self) -> Result<#ident, Box<dyn Error>> {
                Ok(#ident {
                    #build_tokens
                })
            }
        }
    };

    TokenStream::from(tokens)
}

fn derive_builder_implementation(
    named: &Punctuated<Field, Token![,]>,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    let mut builder = Vec::new();
    let mut build_fields = Vec::new();

    for field in named {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        let mut generic_inner_type = None;
        if let Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) = ty
        {
            if let Some(segment) = segments.into_iter().next() {
                // segment.ident could be Option<>, Vec<>
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = &segment.arguments
                {
                    generic_inner_type = Some((segment.ident.to_string(), args.clone()));
                }
            }
        }

        let literal = if let Some(attribute) = field.attrs.first() {
            extract_attribute_literal(attribute)
        } else {
            None
        };

        let builder_method = generate_builder_method(ident, &generic_inner_type, ty, &literal);
        // fs::write(format!("/tmp/builder_method_{}", ident), builder_method.to_string());
        builder.push(builder_method);
        let build_field = generate_build_struct_field(ident, &generic_inner_type);
        // fs::write(format!("/tmp/build_field_{}", ident), build_field.to_string());
        build_fields.push(build_field);
    }

    (builder, build_fields)
}

fn extract_attribute_literal(attribute: &Attribute) -> Option<String> {
    if let syn::Meta::List(MetaList { tokens, .. }) = &attribute.meta {
        match parse2::<MetaNameValue>(tokens.clone()) {
            Ok(pair) => {
                if let Some(ident) = pair.path.get_ident() {
                    if ident != "each" {
                        eprintln!("Unknown attribute {}", ident);
                    }
                } else {
                    eprintln!("Failed to get ident");
                }

                match pair.value {
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(ls), ..
                    }) => {
                        let value = ls.value();
                        return Some(value);
                    }
                    _ => {
                        eprintln!("Unknown value");
                    }
                }
            }
            Err(e) => {
                eprintln!("Failed to parse MetaNameValue: {}", e);
            }
        }
    }

    None
}

fn generate_builder_method(
    ident: &Ident,
    generic_inner_type: &Option<(String, Punctuated<GenericArgument, token::Comma>)>,
    ty: &Type,
    each_literal: &Option<String>,
) -> proc_macro2::TokenStream {
    match generic_inner_type {
        Some((generic, args)) if generic == "Option" => {
            quote! {
                fn #ident(&mut self, #ident: #args) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
        Some((generic, args)) if generic == "Vec" => {
            if let Some(arg_literal) = each_literal {
                let item_ident = Ident::new(arg_literal, Span::call_site());

                quote! {
                    fn #item_ident(&mut self, #item_ident: #args) -> &mut Self {
                        self.#ident.as_mut().unwrap().push(#item_ident);
                        self
                    }
                }
            } else {
                quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                }
            }
        }
        Some((generic, _args)) => {
            eprintln!("Unknown generic type {}", generic);
            quote! {}
        }
        None => {
            quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    }
}

fn generate_build_struct_field(
    ident: &Ident,
    generic_inner_type: &Option<(String, Punctuated<GenericArgument, token::Comma>)>,
) -> proc_macro2::TokenStream {
    match generic_inner_type {
        Some((generic, _args)) if generic == "Option" => {
            quote! {
                #ident: self.#ident.clone(),
            }
        }
        _ => {
            let identifier = ident.to_string();
            quote! {
                #ident: self.#ident.take().ok_or_else(|| Box::<dyn std::error::Error>::from(format!("{} is not set.", #identifier)))?,
            }
        }
    }
}
