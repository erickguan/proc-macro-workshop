use proc_macro2::Span;
use syn::{parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Fields, FieldsNamed, GenericArgument, Ident, Path, PathArguments, Type, TypePath};
use quote::quote;
use proc_macro::TokenStream;

#[proc_macro_derive(Builder)]
pub fn derive_builder_macro(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let mut builder = Vec::new();
    let mut fields_list = Vec::new();

    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        for field in named {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;

            let mut option_inner_type: Option<GenericArgument> = None;
            if let Type::Path(TypePath {
                path: Path { 
                    segments,
                    ..
                },
                ..
            }) = ty {
                let option = Ident::new("Option", Span::call_site());
                for segment in segments {
                    if segment.ident == option {
                        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { 
                            args,
                            ..
                        })= &segment.arguments {
                            for arg in args {
                                option_inner_type = Some(arg.clone());
                                break;
                            }
                        }
                    }
                    break; // First level is enough
                }
            }

           let expanded = 
            if let Some(ref option) = option_inner_type {
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
            };
            builder.push(expanded);

            let expanded = if let Some(ref _option) = option_inner_type {
                quote! {
                    #ident: self.#ident.clone(),
                }
            } else {
                quote! {
                    #ident: self.#ident.take().ok_or_else(|| Box::<dyn std::error::Error>::from("ident is not set.".to_string()))?,
                }
            };
            fields_list.push(expanded);
        }
    }

    let builder_impl = if !builder.is_empty() {
        quote! {
            #(#builder)*
        }
    } else {
        quote! {}
    };

    let build_impl = if !fields_list.is_empty() {
        quote! {
            #(#fields_list)*
        }
    } else {
        quote! {}
    };

    // println!("{}", build_impl);

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
            #builder_impl

            pub fn build(&mut self) -> Result<#ident, Box<dyn Error>> {
                Ok(#ident {
                    #build_impl
                })
            }
        }
    };    

    TokenStream::from(tokens)
}
