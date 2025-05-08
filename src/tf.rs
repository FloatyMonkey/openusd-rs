/// Token for efficient comparison, assignment, and hashing of known strings.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Token {
	data: String,
}

impl Token {
	pub fn new(name: impl ToString) -> Self {
		Token {
			data: name.to_string(),
		}
	}

	pub fn is_empty(&self) -> bool {
		self.data.is_empty()
	}

	pub fn as_str(&self) -> &str {
		&self.data
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.data)
	}
}

#[macro_export]
macro_rules! declare_public_tokens {
	($struct:ident, $static:ident, [$($name:ident: $value:expr),*]) => {
		pub struct $struct {
			$(pub $name: tf::Token,)*
		}

		pub static $static: std::sync::LazyLock<$struct> = std::sync::LazyLock::new(|| {
			$struct {
				$($name: tf::Token::new($value),)*
			}
		});
	};
}
