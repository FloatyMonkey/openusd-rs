use std::ops::Range;

pub struct Input<'a, C> {
	pub input: &'a str,
	pub pos: usize,
	pub ctx: &'a mut C,
}

impl<'a, C> Input<'a, C> {
	pub fn new(input: &'a str, ctx: &'a mut C) -> Self {
		Input { input, pos: 0, ctx }
	}

	pub fn char_at_byte(&self, byte_pos: usize) -> Option<char> {
		self.input[byte_pos..].chars().next()
	}

	pub fn current_char(&self) -> Option<char> {
		self.char_at_byte(self.pos)
	}

	pub fn advance(&mut self) {
		if let Some(c) = self.current_char() {
			self.pos += c.len_utf8();
		}
	}

	pub fn slice(&self, range: Range<usize>) -> &'a str {
		&self.input[range.start..range.end]
	}

	pub fn get_error_context(&self) -> ErrorContext<'a> {
		ErrorContext::new(self.input, self.pos)
	}

	pub fn format_error(&self, error: &str) -> String {
		self.get_error_context().format_error_display(error)
	}

	pub fn remaining(&self) -> &'a str {
		&self.input[self.pos..]
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
	line: usize,
	column: usize,
}

pub struct ErrorContext<'a> {
	input: &'a str,
	position: usize,
	location: Location,
}

impl<'a> ErrorContext<'a> {
	pub fn new(input: &'a str, position: usize) -> Self {
		let location = ErrorContext::get_location(input, position);
		Self {
			input,
			position,
			location,
		}
	}

	pub fn get_location(input: &str, position: usize) -> Location {
		let position = std::cmp::min(position, input.len());
		let mut line = 1;
		let mut column = 1;

		for (_, c) in input.char_indices().take_while(|(i, _)| *i < position) {
			if c == '\n' {
				line += 1;
				column = 1;
			} else {
				column += 1;
			}
		}

		Location { line, column }
	}

	pub fn format_error_display(&self, error: &str) -> String {
		let line_start = self.input[..self.position]
			.rfind('\n')
			.map_or(0, |pos| pos + 1);

		let line_end = self.input[self.position..]
			.find('\n')
			.map_or(self.input.len(), |pos| self.position + pos);

		let line_content = &self.input[line_start..line_end];
		let caret_position = self.position - line_start;

		let mut result = format!(
			"Error at line {}, column {}: {}\n",
			self.location.line, self.location.column, error
		);

		result.push_str(&format!("{}\n", line_content));
		result.push_str(&format!("{}^\n", " ".repeat(caret_position)));

		result
	}
}

#[derive(Debug, Clone)]
pub struct Error {
	global: bool,
	message: String,
}

impl Error {
	pub fn from_msg(msg: &str) -> Self {
		Error {
			global: false,
			message: msg.to_string(),
		}
	}

	pub fn fatal(mut self) -> Self {
		self.global = true;
		self
	}
}

fn err() -> Error {
	Error {
		global: false,
		message: String::new(),
	}
}

pub fn one<'a, C>(i: &mut Input<'a, C>, expected: char) -> Result<(), Error> {
	match i.current_char() {
		Some(c) if c == expected => {
			i.advance();
			Ok(())
		}
		Some(_) | None => Err(err()),
	}
}

// Recognizes a specific string tag
pub fn tag<'a, C>(i: &mut Input<'a, C>, expected_tag: &'static str) -> Result<&'a str, Error> {
	let start_pos = i.pos;
	for expected_char in expected_tag.chars() {
		match i.current_char() {
			Some(c) if c == expected_char => {
				i.advance();
			}
			_ => {
				i.pos = start_pos; // Backtrack on mismatch
				return Err(err());
			}
		}
	}
	Ok(i.slice(start_pos..i.pos))
}

// Makes a parser optional
pub fn opt<'a, T, P, C>(i: &mut Input<'a, C>, parser: P) -> Result<Option<T>, Error>
where
	P: FnOnce(&mut Input<'a, C>) -> Result<T, Error>,
{
	match parser(i) {
		Ok(result) => Ok(Some(result)),
		Err(_) => Ok(None),
	}
}

// Runs parser P and consumes output of parser S afterwards
pub fn terminated<'a, T, P, S, C>(
	i: &mut Input<'a, C>,
	mut parser: P,
	mut terminator: S,
) -> Result<T, Error>
where
	P: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
	S: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	let result = parser(i)?;
	terminator(i)?;
	Ok(result)
}

// Runs parser S, then parser P, returns result of P
pub fn preceded<'a, T, P, S, C>(
	i: &mut Input<'a, C>,
	mut prefix: S,
	mut parser: P,
) -> Result<T, Error>
where
	P: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
	S: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	prefix(i)?;
	parser(i)
}

// Runs parser O, then P, then C. Returns result of P.
pub fn delimited<'a, T, O, P, CL, C>(
	i: &mut Input<'a, C>,
	mut opener: O,
	mut parser: P,
	mut closer: CL,
) -> Result<T, Error>
where
	O: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
	P: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
	CL: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	opener(i)?;
	let result = parser(i)?;
	closer(i)?;
	Ok(result)
}

/// Matches a non-empty list of P separated by S.
/// Equivalent to seq(P, zero_or_more(S, P)).
pub fn list<'a, T, P, S, C>(
	i: &mut Input<'a, C>,
	mut parser: P,
	mut separator: S,
) -> Result<&'a str, Error>
where
	P: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
	S: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	let start_pos = i.pos;
	parser(i)?;
	zero_or_more(i, |i| {
		separator(i)?;
		parser(i)?;
		Ok(())
	})?;

	Ok(i.slice(start_pos..i.pos))
}

/// Matches a non-empty list of P separated by S and returns the results in a Vec.
/// Equivalent to seq(P, zero_or_more(S, P)).
pub fn list_collect<'a, T, P, S, C>(
	i: &mut Input<'a, C>,
	mut parser: P,
	mut separator: S,
) -> Result<Vec<T>, Error>
where
	P: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
	S: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	let mut results = Vec::new();

	results.push(parser(i)?);
	zero_or_more(i, |i| {
		separator(i)?;
		results.push(parser(i)?);
		Ok(())
	})?;

	Ok(results)
}

pub fn must<'a, T, P, C>(i: &mut Input<'a, C>, mut parser: P) -> Result<T, Error>
where
	P: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
{
	match parser(i) {
		Ok(result) => Ok(result),
		Err(e) => Err(e.fatal()),
	}
}

pub fn zero_or_more<'a, P, C>(i: &mut Input<'a, C>, mut parser: P) -> Result<(), Error>
where
	P: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	loop {
		let start_pos = i.pos;
		match parser(i) {
			Ok(_) => {
				if i.pos == start_pos {
					// Zero-width loop detected
					// TODO: Is it correct to return Ok here?
					return Ok(());
				}
			}
			Err(e) if e.global => {
				return Err(e);
			}
			Err(_) => {
				i.pos = start_pos; // Backtrack on non-fatal error
				return Ok(());
			}
		}
	}
}

pub fn one_or_more<'a, P, C>(i: &mut Input<'a, C>, mut parser: P) -> Result<(), Error>
where
	P: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	let initial_pos = i.pos;
	parser(i)?;

	if i.pos == initial_pos {
		// First parse succeeded but consumed nothing. This is tricky.
		// If zero_or_more is called next, it needs to handle this.
		// Let's rely on zero_or_more's ZeroWidthLoop check.
	}

	zero_or_more(i, parser)?;

	Ok(())
}

pub fn blank<'a, C>(i: &mut Input<'a, C>) -> Result<(), Error> {
	loop {
		let start_pos = i.pos;
		match i.current_char() {
			Some(c) if c.is_whitespace() => {
				i.advance();
				if i.pos == start_pos {
					return Err(err());
				}
			}
			_ => break,
		}
	}
	Ok(())
}

/// Matches an R that can be padded by arbitrary many P on the left and on the right.
/// Equivalent to seq(zero_or_more(P), R, zero_or_more(P)).
pub fn pad<'a, R, P, T, C>(i: &mut Input<'a, C>, mut parser: R, mut padding: P) -> Result<T, Error>
where
	R: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
	P: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	zero_or_more(i, |i| padding(i))?;
	let result = parser(i)?;
	zero_or_more(i, |i| padding(i))?;
	Ok(result)
}

/// Matches an optional R that can be padded by arbitrary many P or matches just arbitrary many P.
/// Equivalent to seq(zero_or_more(P), opt(R, zero_or_more(P))).
pub fn pad_opt<'a, R, P, T, C>(
	i: &mut Input<'a, C>,
	mut parser: R,
	mut padding: P,
) -> Result<Option<T>, Error>
where
	R: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
	P: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	zero_or_more(i, |i| padding(i))?;
	opt(i, |i| {
		terminated(i, |i| parser(i), |i| zero_or_more(i, |i| padding(i)))
	})
}

// Parses content until a delimiter is reached, allowing escapes
// Returns the parsed content exluding the delimiter and consumes the input past the delimiter
pub fn until<'a, D, E, C>(
	i: &mut Input<'a, C>,
	mut delimiter: D,
	mut element: E,
) -> Result<&'a str, Error>
where
	D: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
	E: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
{
	let start_pos = i.pos;

	loop {
		let delimiter_start = i.pos;

		if delimiter(i).is_ok() {
			return Ok(i.slice(start_pos..delimiter_start));
		}

		i.pos = delimiter_start;

		match element(i) {
			Ok(_) => {
				if i.pos == delimiter_start {
					return Err(Error::from_msg("Zero-width match in until parser"));
				}
			}
			Err(e) if e.global => return Err(e),
			Err(_) => {
				if i.current_char().is_none() {
					return Err(Error::from_msg("Unterminated sequence in until parser"));
				}
				i.advance();
			}
		}
	}
}

pub fn if_must<'a, F, S, C, T>(i: &mut Input<'a, C>, mut first: F, second: S) -> Result<T, Error>
where
	F: FnMut(&mut Input<'a, C>) -> Result<(), Error>,
	S: FnMut(&mut Input<'a, C>) -> Result<T, Error>,
{
	first(i)?;
	must(i, second)
}

/// Succeeds at "end-of-input", i.e. when the input is empty or all input has been consumed.
pub fn eoi<'a, C>(i: &mut Input<'a, C>) -> Result<(), Error> {
	if i.current_char().is_none() {
		Ok(())
	} else {
		Err(Error::from_msg("Expected end of input"))
	}
}

macro_rules! sor {
	// Base case with single parser
	($input:expr, $parser:expr $(,)?) => {
		$parser($input)
	};

	// Recursive case with two or more parsers
	($input:expr, $first:expr, $($rest:expr),+ $(,)?) => {{
		let start_pos = $input.pos;
		match $first($input) {
			Ok(result) => Ok(result),
			Err(_) => {
				$input.pos = start_pos;
				match sor!($input, $($rest),+) {
					Ok(result) => Ok(result),
					Err(e) => {
						$input.pos = start_pos;
						Err(e)
					}
				}
			}
		}
	}};
}

pub(crate) use sor;
