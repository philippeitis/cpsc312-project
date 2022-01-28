#![feature(get_mut_unchecked)]
#![feature(array_windows)]

use swipl::prelude::*;

use std::cmp::Ordering;
use std::io::{self, Write};
use std::sync::{Arc, RwLock};

const LEV_SCALE_U32: u32 = 10;
const LEV_SCALE: f32 = 10.;
const INSERT_COST: u32 = LEV_SCALE_U32;
const DEL_COST: u32 = LEV_SCALE_U32;

#[arc_blob("permutation_state")]
#[derive(Clone)]
struct PermutationState {
    start: usize,
    swap_index: usize,
    items: Arc<RwLock<Vec<String>>>,
    parent: Option<Arc<PermutationState>>,
}

impl ArcBlobImpl for PermutationState {
    fn compare(&self, _: &Self) -> Ordering {
        unimplemented!()
    }

    fn write(&self, stream: &mut PrologStream) -> io::Result<()> {
        write!(stream, "<permutation_state {:?}>", self.items)
    }
}

impl PermutationState {
    fn new(items: Vec<String>) -> Self {
        Self {
            start: 0,
            swap_index: 0,
            items: Arc::new(RwLock::new(items)),
            parent: None,
        }
    }

    fn next(&mut self) -> (String, PermutationState) {
        println!("n {}, {}, {:?}", self.start, self.swap_index, self.items);
        assert!(self.has_next());

        // First swap does nothing.
        let item = {
            let mut items = self.items.write().unwrap();
            items.swap(self.start, self.start + self.swap_index);
            items[self.start].clone()
        };
        println!("n       {:?}", self.items);

        self.swap_index += 1;

        (
            item,
            PermutationState {
                start: self.start + 1,
                swap_index: 0,
                items: self.items.clone(),
                parent: None,
            },
        )
    }

    fn prev(&mut self) -> bool {
        println!("p {}, {}, {:?}", self.start, self.swap_index, self.items);
        // First swap does nothing.
        if self.swap_index == 0 {
            return false;
        }
        self.swap_index -= 1;
        {
            let mut items = self.items.write().unwrap();
            items.swap(self.start, self.start + self.swap_index);
        }
        println!("p       {:?}", self.items);
        true
    }

    fn has_next(&self) -> bool {
        let len = self.items.read().unwrap().len();
        self.start + self.swap_index < len
    }
}

impl Drop for PermutationState {
    fn drop(&mut self) {
        println!(" Drop ({})", self.start);
        while self.prev() {
            continue;
        }
        self.parent = None;
    }
}

#[inline(always)]
fn char_cost(a: char, b: char) -> u32 {
    if a == b {
        0
    } else if a.to_ascii_lowercase() == b.to_ascii_lowercase() {
        4
    } else {
        10
    }
}

fn levenshtein_core<I: IntoIterator<Item = u32>>(
    start_cost: I,
    mut row0: Vec<u32>,
    long: &str,
    short: &str,
) -> Vec<u32> {
    let mut row1 = vec![0; row0.len()];
    for (sc, init) in short.chars().zip(start_cost) {
        row1[0] = init;
        for (ins, (lc, [sub, del])) in long.chars().zip(row0.array_windows::<2>()).enumerate() {
            row1[ins + 1] = (sub + char_cost(sc, lc))
                .min(del + DEL_COST)
                .min(row1[ins] + INSERT_COST);
        }
        std::mem::swap(&mut row0, &mut row1);
    }

    row0
}

fn levenshtein_fuzzy(a: &str, b: &str) -> f32 {
    let alen = a.chars().count();
    let blen = b.chars().count();
    let (long, short, row) = if alen > blen {
        (a, b, vec![0; alen + 1])
    } else {
        (b, a, vec![0; blen + 1])
    };

    (levenshtein_core(
        (1..short.len() + 1).map(|x| x as u32 * LEV_SCALE_U32),
        row,
        long,
        short,
    )
    .into_iter()
    .skip(1)
    .min()
    .unwrap() as f32)
        / LEV_SCALE
}

fn levenshtein_total(a: &str, b: &str) -> f32 {
    let alen = a.chars().count();
    let blen = b.chars().count();
    let (long, short, row) = if alen > blen {
        (
            a,
            b,
            (0..alen + 1).map(|x| x as u32 * LEV_SCALE_U32).collect(),
        )
    } else {
        (
            b,
            a,
            (0..blen + 1).map(|x| x as u32 * LEV_SCALE_U32).collect(),
        )
    };

    (*levenshtein_core(
        (1..short.len() + 1).map(|x| x as u32 * LEV_SCALE_U32),
        row,
        long,
        short,
    )
    .last()
    .unwrap() as f32)
        / LEV_SCALE
}

predicates! {
    /// list_in: List to permute (either list, or blob)
    /// item: permutation head
    /// list_out: Permutation tail (always blob)
    nondet fn next_item<Arc<PermutationState>>(context, list_in, item, list_out) {
        setup => {
            if let Ok(functor_list) = list_in.get::<Vec<String>>() {
                Ok(Some(Arc::new(PermutationState::new(functor_list))))
            } else if let Ok(state) = list_in.get::<Arc<PermutationState>>() {
                Ok(Some(state))
            } else {
                Ok(None)
            }
        },
        call(v) => {
            if !v.has_next() {
                list_out.unify(&Nil)?;
                Ok(false)
            } else {
                // Safety - not multithreaded
                let (functor, mut next) = unsafe { Arc::get_mut_unchecked(v).next() };
                println!("Count ({}): {}", v.start, Arc::strong_count(&v));
                next.parent = Some(v.clone());
                println!("Count ({}): {}", v.start, Arc::strong_count(&v));
                println!(" Next ({})", next.start);
                item.unify(&functor)?;
                list_out.unify(&Arc::new(next))?;
                Ok(true)
            }
        }
    }

    semidet fn fuzzy_substr(_context, a, b, cost) {
        let a: String = a.get()?;
        let b: String = b.get()?;
        cost.unify(&(levenshtein_fuzzy(&a, &b) as f64))
    }

    semidet fn levenshtein_distance(_context, a, b, cost) {
        let a: String = a.get()?;
        let b: String = b.get()?;
        cost.unify(&(levenshtein_total(&a, &b) as f64))
    }

    semidet fn split_left(_context, target, sep, num, substrings) {
        let target: String = target.get()?;
        let sep: String = sep.get()?;
        let mut num = num.get::<u64>()? as usize;

        let mut items = Vec::new();
        let mut front: &str = &target;
        while num > 0 && !front.is_empty() {
            if let Some((head, rest)) = front.split_once(&sep) {
                let head = head.trim_start_matches(|c| sep.contains(c));
                front = rest.trim_start_matches(|c| sep.contains(c));
                if head.is_empty() {
                    continue;
                } else {
                    num -= 1;
                    items.push(head);
                }
            } else {
                break;
            }
        }

        if !front.is_empty() {
            items.push(front);
        }
        substrings.unify(items.as_slice())
    }
}

#[no_mangle]
pub extern "C" fn install() {
    register_next_item();
    register_fuzzy_substr();
    register_levenshtein_distance();
    register_split_left();
}