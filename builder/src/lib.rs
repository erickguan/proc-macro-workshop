use proc_macro::TokenStream;
use proc_macro2::Span
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Lit, AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, Expr, ExprLit, Field, Fields, FieldsNamed, GenericArgument, Ident, MetaNameValue, Path, PathArguments, Token, Type, TypePath
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder_macro(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let mut builder_tokens = proc_macro2::TokenStream::new();
    let mut build_tokens = proc_macro2::TokenStream::new(); // for builder's build method

    let attributes = input.attrs;
    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        let (builder, build_struct_fields) = derive_builder_implementation(named, &attributes);

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
                    args: None,
                    env: None,
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
    attributes: &Vec<Attribute>,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    let mut builder = Vec::new();
    let mut build_fields = Vec::new();

    for field in named {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        let mut option_inner_type = None;
        if let Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) = ty
        {
            let option = Ident::new("Option", Span::call_site());
            if let Some(segment) = segments.into_iter().next() {
                if segment.ident == option {
                    if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args,
                        ..
                    }) = &segment.arguments
                    {
                        if let Some(arg) = args.into_iter().next() {
                            option_inner_type = Some(arg.clone());
                        }
                    }
                }
            }
        }

        // TODO: validate the each="" decorates a Vec<T>. and we also need to extract the T out of Vec.
        let literal =
            if let Some(attribute) = field.attrs.iter().next() {
                if let syn::Meta::NameValue(MetaNameValue { path, value: Expr::Lit(lit), ..}) = &attribute.meta {
                    if let Some(ident) = path.get_ident() {
                        if ident != "each" {
                            panic!("We don't this #[builder] attribute: {}", ident);
                        }

                        Some(lit.lit.clone())
                    } else {
                        // don't want to deal with bare attributes.
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };

        builder.push(generate_build_method(ident, &option_inner_type, ty, &literal));
        build_fields.push(generate_build_struct_field(ident, &option_inner_type));
    }

    (builder, build_fields)
}

fn generate_build_method(
    ident: &Ident,
    option_inner_type: &Option<GenericArgument>,
    ty: &Type,
    each_literal: &Option<Lit>
) -> proc_macro2::TokenStream {
    if let Some(option) = option_inner_type {
        quote! {
            fn #ident(&mut self, #ident: #option) -> &mut Self {
                self.#ident = Some(#ident);
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

fn generate_build_struct_field(
    ident: &Ident,
    option_inner_type: &Option<GenericArgument>,
) -> proc_macro2::TokenStream {
    if let Some(_option) = option_inner_type {
        quote! {
            #ident: self.#ident.clone(),
        }
    } else {
        quote! {
            #ident: self.#ident.take().ok_or_else(|| Box::<dyn std::error::Error>::from("ident is not set.".to_string()))?,
        }
    }
}
