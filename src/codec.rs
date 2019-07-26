use bytes::BytesMut;

use super::AutocompRequest;
use super::Error;

#[derive(Default, Clone, Debug, Hash)]
pub struct ArgvCodec {
    read: usize,         // The number of bytes read
    argc: usize,         // Arg count
    word: Option<usize>, // What word to autocomplete
    argv: Vec<String>,   // The result
}

impl ArgvCodec {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Drop for ArgvCodec {
    fn drop(&mut self) {
        if self.word.is_none() || self.argc != self.argv.len() {
            eprintln!(
                "Request dropped in an incoherent state: argc = {}, argv (partial) = {:?}",
                self.argc, self.argv
            );
        }
    }
}

impl tokio_codec::Decoder for ArgvCodec {
    type Item = AutocompRequest;
    type Error = Error;
    fn decode(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if self.argc == 0 {
            if buf.len() < 2 {
                return Ok(None);
            } else {
                self.argc = u16::from_be_bytes([buf[0], buf[1]]) as usize;
                buf.advance(2);
                self.argv.reserve(self.argc);
            }
        }
        if self.word.is_none() {
            if buf.len() < 2 {
                return Ok(None);
            } else {
                let word = u16::from_be_bytes([buf[0], buf[1]]) as usize;
                if word >= self.argc {
                    return Err(Error::WordOutOfRange(self.argc, word));
                }
                self.word = Some(word);
                buf.advance(2);
            }
        }

        while let Some(mut offset) = buf[self.read..].iter().position(|&b| b == b'\0') {
            // The index of the '\n' is at the sum of the start position + the offset found.
            offset += self.read;
            let line = buf.split_to(offset + 1);
            // Drop the nul terminator
            let line = &line[..line.len() - 1];
            // Convert the bytes to a string and panic if the bytes are not valid utf-8.
            let line = std::str::from_utf8(line)?.to_string();

            self.argv.push(line);
            self.read = 0;
            if self.argv.len() == self.argc {
                self.argc = 0;
                // Set the search start index back to 0.
                return Ok(Some(AutocompRequest {
                    argv: std::mem::replace(&mut self.argv, Vec::new()),
                    word: self.word.unwrap(), // If the word is none, it is impossible that it compes here
                }));
            }
        }

        // Ok(None) signifies that more data is needed to produce a full frame.
        Ok(None)
    }
}
