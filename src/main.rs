/* snesrc - SNES Recompiler
 *
 * Mar 23, 2010: addition by spinout to actually fix CRC if it is incorrect
 * Feb 16, 2025: rewrite in Rust by Jhynjhiruu for my purposes
 *
 * Copyright notice for this file:
 *  Copyright (C) 2005 Parasyte
 *
 * Based on uCON64's N64 checksum algorithm by Andreas Sterbenz
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

use std::fmt::Display;
use std::fs::{read, write};
use std::io::Cursor;
use std::iter::repeat;
use std::num::{ParseIntError, Wrapping};
use std::path::PathBuf;
use std::str::FromStr;

use anyhow::Result;
use binrw::{binrw, BinWrite};
use clap::Parser;
use clap_num::maybe_hex;
use strum::EnumString;
use thiserror::Error;

#[cfg(feature = "7105")]
const CIC7105: &[u8] = include_bytes!("../cic7105.bin");

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input GameShark ROM file
    infile: PathBuf,

    /// Output GameShark ROM file
    outfile: PathBuf,

    /// Offset in file to write keycodes
    #[arg(short, long, value_parser = maybe_hex::<usize>)]
    offset: Option<usize>,

    /// List of key codes to generate
    key_codes: Vec<KeyCode>,

    /// Entrypoint override
    #[arg(short, long, value_parser = maybe_hex::<u32>)]
    entrypoint: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString, strum::Display)]
enum Cic {
    #[strum(
        serialize = "7101",
        serialize = "7102",
        serialize = "6101",
        serialize = "6102"
    )]
    Cic7101,
    #[strum(serialize = "7103", serialize = "6103")]
    Cic7103,
    #[cfg(feature = "7105")]
    #[strum(serialize = "7105", serialize = "6105")]
    Cic7105,
    #[strum(serialize = "7106", serialize = "6106")]
    Cic7106,
}

impl Cic {
    fn seed(&self) -> u32 {
        match self {
            Cic::Cic7101 => 0xF8CA4DDC,
            Cic::Cic7103 => 0xA3886759,
            #[cfg(feature = "7105")]
            Cic::Cic7105 => 0xDF26F436,
            Cic::Cic7106 => 0x1FEA617A,
        }
    }
}

#[derive(Debug, Clone)]
struct KeyCode {
    cic: Cic,
    name: String,
    entrypoint: u32,
}

impl Display for KeyCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{:08X}", self.cic, self.name, self.entrypoint)
    }
}

#[derive(Debug, Error)]
pub enum ParseKeyCodeError {
    #[error("patch string was not correctly separated")]
    BadSeparator,

    #[error(transparent)]
    IntError(#[from] ParseIntError),

    #[error(transparent)]
    StrumError(#[from] strum::ParseError),
}

impl FromStr for KeyCode {
    type Err = ParseKeyCodeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split = s.split(':').collect::<Vec<_>>();

        if split.len() != 3 {
            return Err(ParseKeyCodeError::BadSeparator);
        }

        let cic = Cic::from_str(split[0])?;

        let entrypoint = u32::from_str_radix(split[2], 16)?;

        Ok(Self {
            cic,
            name: split[1].to_string(),
            entrypoint,
        })
    }
}

#[binrw]
#[derive(Debug)]
struct ROMKeyCode {
    crc: [u32; 2],
    entrypoint: u32,
    checksum: u8,
    #[br(temp)]
    #[bw(try_calc(name.bytes().chain(repeat(0)).take(0x1F).collect::<Vec<u8>>().try_into().map_err(|_| binrw::Error::AssertFail { pos: __binrw_generated_position_temp, message: format!("key code name is too long ({} bytes, max 31)", name.len()) })))]
    raw_name: [u8; 0x1F],
    #[br(try_calc(String::from_utf8(raw_name.to_vec()).map(|n| n.trim().to_string())))]
    #[bw(ignore)]
    name: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut infile = read(args.infile)?;

    let data = infile
        .iter()
        .chain(repeat(&0))
        .skip(0x1000)
        .take(0x20000 - 0x1000)
        .chain(repeat(&0))
        .take(0x100000)
        .cloned()
        .collect::<Vec<_>>();

    let mut key_codes = vec![];

    let header_sum = infile[0x30..0x40]
        .chunks_exact(size_of::<u32>())
        .map(|c| u32::from_be_bytes(c.try_into().unwrap()))
        .sum::<u32>();

    for key in args.key_codes {
        let seed = Wrapping(key.cic.seed());

        let (mut t1, mut t2, mut t3, mut t4, mut t5, mut t6) = (seed, seed, seed, seed, seed, seed);

        #[cfg_attr(not(feature = "7105"), allow(unused_variables))]
        for (index, i) in data.chunks_exact(size_of::<u32>()).enumerate() {
            let d = Wrapping(u32::from_be_bytes(i.try_into().unwrap()));

            if (t6 + d) < t6 {
                t4 += 1;
            }
            t6 += d;
            t3 ^= d;

            let r = Wrapping(d.0.rotate_left(d.0 & 0x1F));
            t5 += r;
            if t2 > d {
                t2 ^= r;
            } else {
                t2 ^= t6 ^ d;
            }

            #[cfg(feature = "7105")]
            if key.cic == Cic::Cic7105 {
                let from_cic = Wrapping(u32::from_be_bytes(
                    CIC7105[0x710 + ((index * 4) & 0xFF)
                        ..0x710 + ((index * 4) & 0xFF) + size_of::<u32>()]
                        .try_into()
                        .unwrap(),
                ));

                t1 += from_cic ^ d;
            } else {
                t1 += t5 ^ d;
            }

            #[cfg(not(feature = "7105"))]
            {
                t1 += t5 ^ d;
            }
        }

        let crc = match key.cic {
            Cic::Cic7101 => [(t6 ^ t4 ^ t3).0, (t5 ^ t2 ^ t1).0],
            Cic::Cic7103 => [((t6 ^ t4) + t3).0, ((t5 ^ t2) + t1).0],
            #[cfg(feature = "7105")]
            Cic::Cic7105 => [(t6 ^ t4 ^ t3).0, (t5 ^ t2 ^ t1).0],
            Cic::Cic7106 => [((t6 * t4) + t3).0, ((t5 * t2) + t1).0],
        };

        let checksum = crc[0]
            .wrapping_add(crc[1])
            .wrapping_add(key.entrypoint)
            .wrapping_add(header_sum) as u8;

        key_codes.push(ROMKeyCode {
            crc,
            entrypoint: key.entrypoint,
            checksum,
            name: key.name,
        })
    }

    if let Some(offset) = args.offset {
        let mut out = vec![];
        let mut cursor = Cursor::new(&mut out);
        key_codes.write_be(&mut cursor)?;

        out.resize(0x400, 0x00);

        infile[offset..offset + 0x400].copy_from_slice(&out);
    }

    if let Some(c) = key_codes.first() {
        infile[0x10..0x14].copy_from_slice(&c.crc[0].to_be_bytes());
        infile[0x14..0x18].copy_from_slice(&c.crc[1].to_be_bytes());
        infile[0x08..0x0C].copy_from_slice(&args.entrypoint.unwrap_or(c.entrypoint).to_be_bytes());
    }

    write(args.outfile, infile)?;

    Ok(())
}
