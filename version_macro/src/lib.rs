use std::{collections::{HashMap, LinkedList}};

use proc_macro::TokenStream;
use quote::{ToTokens, quote};

mod version {
    use std::{fmt::{Display, Debug}, iter::repeat};

    const VERSION_PAD: &str = "000000000";

    #[derive(PartialEq, Eq, Clone, Copy, Hash)]
    pub struct Version {
        major: u64,
        minor: u64
    }

    impl From<&str> for Version {
        fn from(value: &str) -> Self {
            value.to_owned().into()
        }
    }

    impl From<String> for Version {
        fn from(value: String) -> Self {
            let (major, minor) = match value.split_once('.') {
                Some(v) => v,
                None => panic!("Invalid version string")
            };

            // Force minor version to be 10 digits so that leading zeroes are dealt with
            let mut minor = minor.to_owned() + VERSION_PAD;
            minor.truncate(10);

            let major_int: u64 = major.parse().expect("Invalid version string");
            let minor_int: u64 = minor.parse().expect("Invalid version string");

            Version { major: major_int, minor: minor_int }
        }
    }

    impl PartialOrd for Version {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for Version {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            let major_order = self.major.cmp(&other.major);

            if major_order.is_eq() {
                return self.minor.cmp(&other.minor);
            }

            major_order
        }
    }

    impl Display for Version {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s = self.minor.to_string();
            let zeros = 10 - s.len();
            let mut zeros_to_remove = 0;
            while s.chars().nth(s.len() - 1 - zeros_to_remove) == Some('0') {
                zeros_to_remove += 1;
            }

            let mut minor = repeat('0').take(zeros).collect::<String>() + s.as_str();
            minor.truncate(minor.len() - zeros_to_remove);

            write!(f, "{}.{}", self.major, minor)
        }
    }

    impl Debug for Version {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}.{}", self.major, self.minor)
        }
    }
}

use version::Version;

#[derive(Clone)]
struct StructInfo {
    v_mod: String,
    version: Version,
    converts_to: Vec<Version>
}

fn get_version<'si>(v_mod: &'si syn::ItemMod, struct_ident: &str) -> Option<StructInfo> {
    let structs: Vec<&syn::ItemStruct> = v_mod.content.iter().flat_map(|x| &x.1)
        .filter_map(|x| match x { syn::Item::Struct(s) => Some(s), _ => None}).collect();

    for v_struct in structs {
        if v_struct.ident.to_string() == struct_ident {
            let mut converts = Vec::new();
            let mut version = None;
            for attr in &v_struct.attrs {
                let last = attr.path.segments.last();
                match last {
                    Some(p) => {
                        match p.ident.to_string().as_str() {
                            "converts_to" => {
                                let arg: syn::LitStr = attr.parse_args().expect("Argument must be a string literal that indicates a version.");
                                converts.push(arg.value().into());
                            },
                            "version" => {
                                if version.is_some() {
                                    panic!("Cannot have more than one version!")
                                }

                                let arg: syn::LitStr = attr.parse_args().expect("Argument must be a string literal that indicates a version.");
                                version = Some(arg.value());
                            },
                            _ => {}
                        }
                    },
                    None => {}
                }
            }

            match version {
                Some(v) => {
                    return Some(StructInfo { v_mod: v_mod.ident.to_string(), version: v.into(), converts_to: converts })
                },
                None => {}
            }
        }
    }

    None
}

fn find_upgrade_path(from: Version, to: Version, info: &HashMap<Version, StructInfo>) -> Box<[Version]> {
    let mut q = LinkedList::new();

    // bfs
    q.push_back(vec![from]);
    while q.len() > 0 {
        let path = q.pop_front().unwrap();

        let inf = &info[path.iter().last().unwrap()];
        for cnv in &inf.converts_to {
            let mut new_path = path.clone();
            new_path.push(*cnv);

            if *cnv == to {
                return new_path.into_boxed_slice();
            }

            q.push_back(new_path);
        }
    }

    Box::new([])
}

fn generate_upgrade_code(path: &Box<[Version]>, ident: &str, info: &HashMap<Version, StructInfo>) -> TokenStream {
    let ident_t: proc_macro2::TokenStream = ident.parse().unwrap();

    let end_type: proc_macro2::TokenStream = ("super::".to_owned() + info[path.iter().last().unwrap()].v_mod.as_str() + "::" + ident).parse().unwrap();
    let mut stream = quote!{};

    // let start = &info[&path[0]];
    for node in path[1..].iter() {
        let next_type: proc_macro2::TokenStream = ("super::".to_owned() + info[node].v_mod.as_str() + "::" + ident).parse().unwrap();
        let code = quote!{
            let u: #next_type = u.convert_to();
        };
        stream.extend(code);
    }
    
    quote! {
        impl #ident_t {
            pub fn upgrade(self) -> #end_type {
                let u = self;

                #stream

                u
            }
        }
    }.into()
}

#[proc_macro_attribute]
pub fn version_mod(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast: syn::Item = syn::parse(item).unwrap();
    let mut m = match ast {
        syn::Item::Mod(m) => m,
        _ => {panic!("Must be used on a module!")}
    };

    let struct_ident = attr.to_string();

    let mut mods: HashMap<String, &mut syn::ItemMod> = m.content.iter_mut().flat_map(|x| &mut x.1)
        .filter_map(|y| match y {
            syn::Item::Mod(m) => Some(m),
            _ => None
        })
        .map(|x| (x.ident.to_string(), x)).collect();

    let structs: HashMap<Version, StructInfo> = mods.iter().filter_map(|(_, m)| {
            match get_version(m, &struct_ident) {
                Some(v) => Some((v.version, v)),
                None => None
            }
        })
        .collect();

    let newest = structs.values().max_by(|a, b| a.version.cmp(&b.version)).unwrap();

    let versions: Vec<Version> = structs.iter().map(|x| *x.0).collect();
    for v in versions {
        if v != newest.version {
            let path = find_upgrade_path(v, newest.version, &structs);

            let code: syn::Item = syn::parse(generate_upgrade_code(&path, struct_ident.as_str(), &structs)).unwrap();

            let info = &structs[&v];
            let m = mods.get_mut(&info.v_mod).unwrap();

            m.content.as_mut().unwrap().1.push(code);
        }
    }

    let code: syn::Item = syn::parse2(quote!{
        pub fn hello() {
            println!("Hello World!");
        }
    }).unwrap();

    m.content.as_mut().unwrap().1.push(code);

    m.to_token_stream().into()
}

#[proc_macro_attribute]
pub fn version(_: TokenStream, item: TokenStream) -> TokenStream {
    item
}

#[proc_macro_attribute]
pub fn converts_to(_: TokenStream, item: TokenStream) -> TokenStream {
    item
}